use std::mem;
use std::ops;
use std::str;
use std::fmt;
use std::result;
use std::cell::RefCell;
use std::rc::Rc;
use std::borrow::Cow;
use std::collections::{hash_map, HashMap, HashSet};
use vec_map::{self, VecMap};
use atomic::Atomic;
use atomic::Ordering::Relaxed;

use kailua_env::{self, Span, Spanned, WithLoc, ScopedId, ScopeMap, SpanMap};
use kailua_diag::{Result, Kind, Report, Reporter, Locale, Localize};
use kailua_syntax::{Name, NameRef};
use diag::{unquotable_name, Origin, TypeReport, TypeResult, TypeReportHint, TypeReportMore};
use ty::{Displayed, Display};
use ty::{Ty, TySeq, Nil, T, Slot, F, TVar, RVar, Lattice, Union, Tag};
use ty::{TypeContext, TypeResolver, ClassId, Class, Tables, Functions, Function, Key};
use ty::flags::*;
use defs::get_defs;
use options::Options;
use check::Checker;
use message as m;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Id {
    Local(usize, ScopedId),
    Global(Name),
}

impl Id {
    pub fn from(map_index: usize, nameref: NameRef) -> Id {
        match nameref {
            NameRef::Local(scoped_id) => Id::Local(map_index, scoped_id),
            NameRef::Global(name) => Id::Global(name),
        }
    }

    pub fn name<'a, R: Report>(&'a self, ctx: &'a Context<R>) -> &'a Name {
        match *self {
            Id::Local(map_index, ref scoped_id) => scoped_id.name(&ctx.scope_maps[map_index]),
            Id::Global(ref name) => name,
        }
    }

    pub fn scope<R: Report>(&self, ctx: &Context<R>) -> Option<kailua_env::Scope> {
        match *self {
            Id::Local(map_index, ref scoped_id) =>
                Some(scoped_id.scope(&ctx.scope_maps[map_index])),
            Id::Global(_) => None,
        }
    }

    pub fn is_global(&self) -> bool {
        match *self {
            Id::Local(..) => false,
            Id::Global(..) => true,
        }
    }

    pub fn display<'a, R: Report>(&'a self, ctx: &'a Context<R>) -> IdDisplay<'a, R> {
        IdDisplay { id: self, ctx: ctx }
    }
}

#[must_use]
pub struct IdDisplay<'a, R: 'a> {
    id: &'a Id,
    ctx: &'a Context<R>,
}

impl<'a, R: Report> fmt::Display for IdDisplay<'a, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.id {
            Id::Local(map_index, ref scoped_id) => {
                let (name, scope) = self.ctx.scope_maps[map_index].find_id(scoped_id);
                write!(f, "{:?}$", name)?;
                { // bijective numeral: a b c d .. z aa ab .. az ba .. zz aaa ..
                    let mut index = map_index;
                    let mut mult = 26;
                    let mut len = 1;
                    while index >= mult {
                        index -= mult;
                        mult *= 26;
                        len += 1;
                    }
                    for _ in 0..len {
                        mult /= 26;
                        write!(f, "{}", (b'a' + (index / mult) as u8) as char)?;
                        index %= mult;
                    }
                }
                write!(f, "{}", scope.to_usize())
            }
            Id::Global(ref name) => {
                write!(f, "{:?}_", name)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Returns<T> {
    None, // the function hasn't returned and has no rettype spec
    Never, // the function has a diverging rettype spec
    Implicit(T), // the function returns but has no rettype spec, returns will be unioned
    Explicit(T), // the function has a rettype spec, cannot be updated
}

impl<T> Returns<T> {
    pub fn as_ref(&self) -> Returns<&T> {
        match *self {
            Returns::None => Returns::None,
            Returns::Never => Returns::Never,
            Returns::Implicit(ref v) => Returns::Implicit(v),
            Returns::Explicit(ref v) => Returns::Explicit(v),
        }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Returns<U> {
        match self {
            Returns::None => Returns::None,
            Returns::Never => Returns::Never,
            Returns::Implicit(v) => Returns::Implicit(f(v)),
            Returns::Explicit(v) => Returns::Explicit(f(v)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Frame {
    pub vararg: Option<TySeq>,
    pub returns: Returns<TySeq>,
}

#[derive(Clone, Debug)]
pub struct NameDef {
    pub span: Span,
    pub slot: NameSlot,
}

#[derive(Clone, Debug)]
pub enum NameSlot {
    None, // not initialized, no type specified
    Unset(Slot),
    Set(Slot),
}

impl NameSlot {
    pub fn set(&self) -> bool {
        match *self {
            NameSlot::None | NameSlot::Unset(_) => false,
            NameSlot::Set(_) => true,
        }
    }

    pub fn slot(&self) -> Option<&Slot> {
        match *self {
            NameSlot::None => None,
            NameSlot::Unset(ref slot) | NameSlot::Set(ref slot) => Some(slot),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeDef {
    pub span: Span,
    pub ty: Ty,
}

#[derive(Clone, Debug)]
pub struct ClassDef {
    pub span: Span,
    pub name: Option<Spanned<Name>>,
    pub parent: Option<ClassId>,
    pub class_ty: HashMap<Key, Slot>,
    pub instance_ty: HashMap<Key, Slot>,
}

#[derive(Clone, Debug)]
pub struct Scope {
    frame: Option<Frame>,
    types: HashMap<Name, TypeDef>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope { frame: None, types: HashMap::new() }
    }

    pub fn new_function(frame: Frame) -> Scope {
        Scope { frame: Some(frame), types: HashMap::new() }
    }

    pub fn get_frame<'a>(&'a self) -> Option<&'a Frame> {
        self.frame.as_ref()
    }

    pub fn get_frame_mut<'a>(&'a mut self) -> Option<&'a mut Frame> {
        self.frame.as_mut()
    }

    // the span indicates the position of the initial definition
    pub fn get_type<'a>(&'a self, name: &Name) -> Option<&'a TypeDef> {
        self.types.get(name)
    }

    // the caller should check for the outermost types first
    pub fn put_type(&mut self, name: Spanned<Name>, ty: Ty) -> bool {
        self.types.insert(name.base, TypeDef { span: name.span, ty: ty }).is_none()
    }
}

trait Partition {
    fn create(parent: usize, rank: usize) -> Self;
    fn read(&self) -> (usize /*parent*/, usize /*rank*/);
    fn write_parent(&self, parent: usize);
    fn increment_rank(&mut self);
}

#[derive(Debug)]
struct Partitions<T> {
    map: VecMap<T>,
}

impl<T: Partition> Partitions<T> {
    fn new() -> Partitions<T> {
        Partitions { map: VecMap::new() }
    }

    fn find(&self, i: usize) -> usize {
        if let Some(u) = self.map.get(&i) {
            let (mut parent, _) = u.read();
            if parent != i { // path compression
                while let Some(v) = self.map.get(&parent) {
                    let (newparent, _) = v.read();
                    if newparent == parent { break; }
                    parent = newparent;
                }
                u.write_parent(parent);
            }
            parent
        } else {
            i
        }
    }

    fn union(&mut self, lhs: usize, rhs: usize) -> usize {
        use std::cmp::Ordering;

        let lhs = self.find(lhs);
        let rhs = self.find(rhs);
        if lhs == rhs { return rhs; }

        let (_, lrank) = self.map.entry(lhs).or_insert_with(|| Partition::create(lhs, 0)).read();
        let (_, rrank) = self.map.entry(rhs).or_insert_with(|| Partition::create(rhs, 0)).read();
        match lrank.cmp(&rrank) {
            Ordering::Less => {
                self.map.get_mut(&lhs).unwrap().write_parent(rhs);
                rhs
            }
            Ordering::Greater => {
                self.map.get_mut(&rhs).unwrap().write_parent(lhs);
                lhs
            }
            Ordering::Equal => {
                self.map.get_mut(&rhs).unwrap().write_parent(lhs);
                self.map.get_mut(&lhs).unwrap().increment_rank();
                lhs
            }
        }
    }
}

impl<T> ops::Deref for Partitions<T> {
    type Target = VecMap<T>;
    fn deref(&self) -> &VecMap<T> { &self.map }
}

impl<T> ops::DerefMut for Partitions<T> {
    fn deref_mut(&mut self) -> &mut VecMap<T> { &mut self.map }
}

#[derive(Debug)]
struct Bound {
    parent: Atomic<u32>,
    rank: u8,
    bound: Option<Ty>,
}

// a set of constraints that can be organized as a tree
#[derive(Debug)]
struct Constraints {
    op: &'static str,
    bounds: Partitions<Box<Bound>>,
}

// is this bound trivial so that one can always overwrite?
fn is_bound_trivial(t: &Option<Ty>) -> bool {
    // TODO special casing ? is not enough, should resolve b.bound's inner ?s as well
    if let Some(ref t) = *t {
        match **t { T::None | T::Dynamic(_) => true, _ => false }
    } else {
        true
    }
}

impl Partition for Box<Bound> {
    fn create(parent: usize, rank: usize) -> Box<Bound> {
        Box::new(Bound { parent: Atomic::new(parent as u32), rank: rank as u8, bound: None })
    }

    fn read(&self) -> (usize /*parent*/, usize /*rank*/) {
        (self.parent.load(Relaxed) as usize, self.rank as usize)
    }

    fn write_parent(&self, parent: usize) {
        self.parent.store(parent as u32, Relaxed);
    }

    fn increment_rank(&mut self) {
        self.rank += 1;
    }
}

impl Constraints {
    fn new(op: &'static str) -> Constraints {
        Constraints { op: op, bounds: Partitions::new() }
    }

    fn is(&self, lhs: TVar, rhs: TVar) -> bool {
        lhs == rhs || self.bounds.find(lhs.0 as usize) == self.bounds.find(rhs.0 as usize)
    }

    fn get_bound<'a>(&'a self, lhs: TVar) -> Option<&'a Bound> {
        let lhs = self.bounds.find(lhs.0 as usize);
        self.bounds.get(&lhs).map(|b| &**b)
    }

    // Some(bound) indicates that the bound already exists and is not consistent to rhs
    fn add_bound<'a>(&'a mut self, lhs: TVar, rhs: &Ty) -> Option<&'a Ty> {
        let lhs_ = self.bounds.find(lhs.0 as usize);
        let b = self.bounds.entry(lhs_).or_insert_with(|| Partition::create(lhs_, 0));
        if is_bound_trivial(&b.bound) {
            b.bound = Some(rhs.clone());
        } else {
            let lhsbound = b.bound.as_ref().unwrap();
            if lhsbound != rhs { return Some(lhsbound); }
        }
        None
    }

    fn add_relation(&mut self, lhs: TVar, rhs: TVar) -> bool {
        if lhs == rhs { return true; }

        let lhs_ = self.bounds.find(lhs.0 as usize);
        let rhs_ = self.bounds.find(rhs.0 as usize);
        if lhs_ == rhs_ { return true; }

        fn take_bound(bounds: &mut VecMap<Box<Bound>>, i: usize) -> Option<Ty> {
            if let Some(b) = bounds.get_mut(&i) {
                mem::replace(&mut b.bound, None)
            } else {
                None
            }
        }

        // take the bounds from each representative variable.
        let lhsbound = take_bound(&mut self.bounds, lhs_);
        let rhsbound = take_bound(&mut self.bounds, rhs_);

        let bound = match (is_bound_trivial(&lhsbound), is_bound_trivial(&rhsbound)) {
            (false, _) => lhsbound,
            (true, false) => rhsbound,
            (true, true) if lhsbound == rhsbound => lhsbound,
            (true, true) => {
                info!("variables {:?}/{:?} cannot have multiple bounds \
                       (left {} {:?}, right {} {:?})",
                      lhs, rhs, self.op, lhsbound, self.op, rhsbound);
                return false;
            },
        };

        // update the shared bound to the merged representative
        let new = self.bounds.union(lhs_, rhs_);
        if !is_bound_trivial(&bound) {
            // the merged entry should have non-zero rank, so unwrap() is fine
            self.bounds.get_mut(&new).unwrap().bound = bound;
        }

        true
    }
}

#[derive(Debug)]
struct RowInfo {
    // the hashmap being None indicates that it is currently recursing;
    // the value can be Some(slot) for "positive" fields, which the row variable contains that key,
    // or None for "negative" fields, which the variable _cannot_ contain that key
    // and it's an error for the unification to introduce it.
    fields: Option<HashMap<Key, Option<Slot>>>,

    // when the next variable is None, it is not instantiated yet and
    // implicitly thought to have negative (absent) fields for each key in fields
    next: Option<RVar>,
}

impl RowInfo {
    fn new() -> RowInfo {
        RowInfo { fields: Some(HashMap::new()), next: None }
    }
}

#[derive(Clone, Debug)]
pub struct Module {
    pub returns: Option<Slot>, // None if the module never returns
    pub exported_types: HashMap<Name, TypeDef>,
}

impl Module {
    pub fn dummy() -> Module {
        Module { returns: Some(Slot::dummy()), exported_types: HashMap::new() }
    }
}

enum LoadStatus {
    Done(Module),
    Ongoing(Span), // span for who to blame
}

// global context (also acts as a type context).
// anything has to be retained across multiple files should be here
pub struct Context<R> {
    report: R,
    output: Output,
}

// a "reportless" version of Context, used for analysis
pub struct Output {
    // name, scope and span information
    ids: HashMap<Id, NameDef>,
    scope_maps: Vec<ScopeMap<Name>>,
    spanned_slots: SpanMap<Slot>,

    // TODO this might be eventually found useless
    global_scope: Scope,

    // type variable information
    next_tvar: TVar,
    tvar_sub: Constraints, // upper bound
    tvar_sup: Constraints, // lower bound
    tvar_eq: Constraints, // tight bound

    // row variable information
    next_rvar: RVar,
    row_infos: VecMap<Box<RowInfo>>,

    // module information
    opened: HashSet<String>,
    loaded: HashMap<Vec<u8>, LoadStatus>, // corresponds to `package.loaded`

    // runtime information
    string_meta: Option<Spanned<Slot>>,

    // classes defined
    classes: Vec<ClassDef>,
}

impl<R: Report> Context<R> {
    pub fn new(report: R) -> Context<R> {
        let mut ctx = Context {
            report: report,
            output: Output {
                ids: HashMap::new(),
                scope_maps: Vec::new(),
                spanned_slots: SpanMap::new(),
                global_scope: Scope::new(),
                next_tvar: TVar(1), // TVar(0) for the top-level return
                tvar_sub: Constraints::new("<:"),
                tvar_sup: Constraints::new(":>"),
                tvar_eq: Constraints::new("="),
                next_rvar: RVar::new(1), // RVar::new(0) == RVar::empty()
                row_infos: VecMap::new(),
                opened: HashSet::new(),
                loaded: HashMap::new(),
                string_meta: None,
                classes: Vec::new(),
            }
        };

        // it is fine to return from the top-level, so we treat it as like a function frame
        let global_frame = Frame { vararg: None, returns: Returns::None };
        ctx.global_scope.frame = Some(global_frame);
        ctx
    }

    pub fn report(&self) -> &R {
        &self.report
    }

    pub fn open_library(&mut self, name: &Spanned<Name>, opts: Rc<RefCell<Options>>) -> Result<()> {
        if let Some(defs) = str::from_utf8(&name.base).ok().and_then(get_defs) {
            // one library may consist of multiple files, so we defer duplicate check
            for def in defs {
                if self.opened.insert(def.name.to_owned()) {
                    // the built-in code is parsed independently and has no usable span
                    let chunk = def.to_chunk();
                    let mut env = Env::new(self, opts.clone(), chunk.map);
                    let mut checker = Checker::new(&mut env);
                    checker.visit(&chunk.block)?
                }
            }
        } else {
            self.error(name, m::CannotOpenLibrary {}).done()?;
        }
        Ok(())
    }

    pub fn get_loaded_module(&self, name: &[u8], span: Span) -> Result<Option<Module>> {
        match self.loaded.get(name) {
            Some(&LoadStatus::Done(ref module)) => Ok(Some(module.clone())),
            None => Ok(None),

            // this is allowed in Lua 5.2 and later, but will result in a loop anyway.
            Some(&LoadStatus::Ongoing(oldspan)) => {
                self.error(span, m::RecursiveRequire {})
                    .note(oldspan, m::PreviousRequire {})
                    .done()?;
                Ok(Some(Module::dummy()))
            }
        }
    }

    pub fn mark_module_as_loading(&mut self, name: &[u8], span: Span) {
        self.loaded.entry(name.to_owned()).or_insert(LoadStatus::Ongoing(span));
    }

    pub fn make_class(&mut self, parent: Option<ClassId>, span: Span) -> ClassId {
        assert!(parent.map_or(true, |cid| (cid.0 as usize) < self.classes.len()));

        let cid = ClassId(self.classes.len() as u32);
        self.classes.push(ClassDef {
            span: span,
            name: None,
            parent: parent,
            class_ty: HashMap::new(),
            instance_ty: HashMap::new(),
        });
        cid
    }

    pub fn name_class(&mut self, cid: ClassId, name: Spanned<Name>) -> Result<()> {
        let cls = &mut self.output.classes[cid.0 as usize];
        if let Some(ref prevname) = cls.name {
            self.report.warn(name, m::RedefinedClassName {})
                       .note(prevname, m::PreviousClassName {})
                       .done()?;
        } else {
            info!("named {:?} as {:?}", cid, name);
            cls.name = Some(name);
        }
        Ok(())
    }

    fn assert_rvar_rel(&mut self, lhs: RVar, rhs: RVar, is_sub: bool) -> TypeResult<()> {
        trace!("{:?} should be {} {:?}", lhs, if is_sub { "<:" } else { "=" }, rhs);

        // we want to avoid modifying shared row variables (which we cannot distinguish,
        // therefore we can only modify a row variable which next var is not yet instantiated),
        // while making sure that all fields known to be present or absent in both operands
        // are common to each other as well. we therefore have to go through a chain of
        // row variables for each operand, collect positive and negative fields respectively,
        // set the bounds from mismatching fields to the last row variable in the chain
        // _and_ finally set the next row variables for both to the same row variable.
        //
        // this is going to be tough.

        fn collect_fields<R: Report>(mut r: RVar, ctx: &mut Context<R>)
                -> TypeResult<(HashMap<Key, Slot>, RVar)> {
            let mut present = HashMap::new();
            let mut absent = HashSet::new(); // only used for filling the last row if none

            let err;
            'err: loop {
                if r == RVar::empty() {
                    return Ok((present, r));
                }

                match ctx.row_infos.entry(r.to_usize()) {
                    vec_map::Entry::Occupied(row) => {
                        let row = row.get();

                        let fields = if let Some(ref fields) = row.fields {
                            fields
                        } else {
                            err = format!("recursive record");
                            break 'err;
                        };

                        for (k, v) in fields {
                            if let Some(ref v) = *v {
                                // positive field should be unique
                                if present.insert(k.clone(), v.clone()).is_some() {
                                    err = format!("internally duplicate field {:?}", k);
                                    break 'err;
                                }
                            } else {
                                // negative field will *not* be unique
                                absent.insert(k.clone());
                            }
                        }

                        if let Some(ref next) = row.next {
                            r = next.clone();
                        } else {
                            return Ok((present, r));
                        }
                    }

                    vec_map::Entry::Vacant(e) => {
                        // instantiate the row and implicitly fill negative fields
                        let mut fields = HashMap::new();
                        fields.extend(present.iter().map(|(k, _)| (k.clone(), None)));
                        fields.extend(absent.into_iter().map(|k| (k, None)));
                        e.insert(Box::new(RowInfo { fields: Some(fields), next: None }));
                        return Ok((present, r));
                    }
                }
            }

            Err(ctx.gen_report().put(Origin::RVar, err))
        };

        if lhs == rhs {
            return Ok(());
        }

        let (lfields, lnext) = collect_fields(lhs.clone(), self)?;
        let (rfields, rnext) = collect_fields(rhs.clone(), self)?;

        let mut matching = Vec::new();
        let mut lmissing = Vec::new();
        let mut rmissing = Vec::new();

        // collect missing fields and matching fields
        // (we cannot immediately check for them due to borrowing)
        for (k, lv) in lfields.iter() {
            if let Some(rv) = rfields.get(k) {
                matching.push((k.clone(), lv.clone(), rv.clone()));
            } else {
                rmissing.push((k.clone(), lv.clone()));
            }
        }
        for (k, rv) in rfields.iter() {
            if !lfields.contains_key(k) {
                lmissing.push((k.clone(), rv.clone()));
            }
        }

        // to make tests reproducible :-)
        matching.sort_by(|a, b| a.0.cmp(&b.0));
        lmissing.sort_by(|a, b| a.0.cmp(&b.0));
        rmissing.sort_by(|a, b| a.0.cmp(&b.0));

        // check for the matching fields
        if is_sub {
            for (_k, lv, rv) in matching {
                lv.assert_sub(&rv, self)?;
            }
        } else {
            for (_k, lv, rv) in matching {
                lv.assert_eq(&rv, self)?;
            }
        }

        // remaining fields should be in the relevant updatable row variable
        if !lmissing.is_empty() {
            self.assert_rvar_includes_(lnext.clone(), &lmissing, false)?;
        }
        if !rmissing.is_empty() {
            self.assert_rvar_includes_(rnext.clone(), &rmissing, is_sub)?;
        }

        // finally two row variables should be linked by setting the common last row variable
        let last = self.gen_rvar();
        if lnext != RVar::empty() {
            let mut row =
                self.row_infos.entry(lnext.to_usize()).or_insert_with(|| Box::new(RowInfo::new()));
            assert_eq!(row.next, None);
            row.next = Some(last.clone());
        }
        if rnext != RVar::empty() && rnext != lnext {
            // avoid setting the next twice, which breaks the assertion
            let mut row =
                self.row_infos.entry(rnext.to_usize()).or_insert_with(|| Box::new(RowInfo::new()));
            assert_eq!(row.next, None);
            row.next = Some(last);
        }

        Ok(())
    }

    fn assert_rvar_includes_(&mut self, lhs: RVar, includes: &[(Key, Slot)],
                             nilable: bool) -> TypeResult<()> {
        trace!("{:?} should include {:?} ({} nil)",
               lhs, includes, if nilable { "allows" } else { "disallows" });

        // optimize a no-op just in case
        if includes.is_empty() {
            return Ok(());
        }

        if lhs == RVar::empty() {
            return Err(self.gen_report().put(Origin::RVar,
                                             format!("the record is not extensible")));
        }

        let lhs_ = lhs.to_usize();

        // take fields out, so that we can detect an infinite recursion
        let fields_and_next = {
            let row = self.row_infos.entry(lhs_).or_insert_with(|| Box::new(RowInfo::new()));
            row.fields.take().map(|fields| (fields, row.next.clone()))
        };
        let (mut fields, next) = if let Some(fields_and_next) = fields_and_next {
            fields_and_next
        } else {
            return Err(self.gen_report().put(Origin::RVar, format!("recursive record")));
        };
        trace!("{:?} already had {:?} and {:?}", lhs, fields, next);

        let e = inner(self, lhs, includes, nilable, &mut fields, next);
        self.row_infos.get_mut(&lhs_).unwrap().fields = Some(fields);
        return e;

        fn inner<R: Report>(ctx: &mut Context<R>, lhs: RVar, includes: &[(Key, Slot)],
                            nilable: bool, fields: &mut HashMap<Key, Option<Slot>>,
                            next: Option<RVar>) -> TypeResult<()> {
            // collect missing fields, whether positive or negative, and
            // check if other matching fields are compatible
            let mut missing = Vec::new();
            for &(ref k, ref rv) in includes {
                match fields.get(k) {
                    Some(&Some(ref lv)) => {
                        // the existing fields should be compatible
                        rv.assert_sub(lv, ctx)?;
                    }
                    Some(&None) => {
                        // the field is excluded, immediately fail
                        return Err(ctx.gen_report().put(Origin::RVar,
                                                        format!("record {:?} cannot have \
                                                                 a field {:?}", lhs, k)));
                    }
                    None => {
                        // the field should be added to the next row variable (if any)
                        missing.push((k.clone(), rv.clone()));
                    }
                }
            }

            // if we have missing fields they should be in the next row variable if any;
            // we can avoid instantiation when it has not yet been instantiated though
            if let Some(next) = next {
                // we need to put fields back, so this cannot be a tail recursion
                ctx.assert_rvar_includes_(next, &missing, nilable)?;
            } else {
                for (k, v) in missing.into_iter() {
                    // when the record is extended due to the (in)equality relation,
                    // the type that is not explicitly nilable is disallowed
                    if nilable || v.unlift().can_omit() {
                        fields.insert(k, Some(v));
                    } else {
                        return Err(ctx.gen_report().put(Origin::RVar,
                                                        format!("record {:?} is being extended \
                                                                 with non-nilable {:?}", lhs, v)));
                    }
                }
            }

            Ok(())
        }
    }

    pub fn into_output(self) -> Output {
        self.output
    }
}

impl Output {
    pub fn spanned_slots(&self) -> &SpanMap<Slot> {
        &self.spanned_slots
    }

    pub fn spanned_slots_mut(&mut self) -> &mut SpanMap<Slot> {
        &mut self.spanned_slots
    }

    pub fn global_scope(&self) -> &Scope {
        &self.global_scope
    }

    pub fn global_scope_mut(&mut self) -> &mut Scope {
        &mut self.global_scope
    }

    pub fn get<'a>(&'a self, id: &Id) -> Option<&'a NameDef> {
        self.ids.get(id)
    }

    pub fn get_mut<'a>(&'a mut self, id: &Id) -> Option<&'a mut NameDef> {
        self.ids.get_mut(id)
    }

    pub fn all<'a>(&'a self) -> hash_map::Iter<'a, Id, NameDef> {
        self.ids.iter()
    }

    pub fn get_class<'a>(&'a self, cid: ClassId) -> Option<&'a ClassDef> {
        self.classes.get(cid.0 as usize)
    }

    pub fn get_class_mut<'a>(&'a mut self, cid: ClassId) -> Option<&'a mut ClassDef> {
        self.classes.get_mut(cid.0 as usize)
    }

    // returns a pair of type flags that is an exact lower and upper bound for that type
    // used as an approximate type bound testing like arithmetics;
    // better be replaced with a non-instantiating assertion though.
    pub fn get_type_bounds(&self, ty: &Ty) -> (/*lb*/ Flags, /*ub*/ Flags) {
        let flags = ty.flags();
        let (lb, ub) = ty.get_tvar().map_or((T_NONE, T_NONE), |v| self.get_tvar_bounds(v));
        (flags | lb, flags | ub)
    }

    // exactly resolves the type variable inside `ty` if possible
    // this is a requirement for table indexing and function calls
    pub fn resolve_exact_type<'a>(&self, ty: &Ty) -> Option<Ty> {
        if let T::TVar(tv) = **ty {
            if let Some(ty2) = self.get_tvar_exact_type(tv) {
                Some(ty2.union_nil(ty.nil()).with_tag(ty.tag()))
            } else {
                None
            }
        } else {
            Some(ty.clone())
        }
    }

    pub fn get_string_meta(&self) -> Option<Spanned<Slot>> {
        self.string_meta.clone()
    }

    pub fn last_tvar(&self) -> Option<TVar> {
        let tvar = self.next_tvar;
        if tvar == TVar(0) { None } else { Some(TVar(tvar.0 - 1)) }
    }

    pub fn get_tvar_bounds(&self, tvar: TVar) -> (Flags /*lb*/, Flags /*ub*/) {
        if let Some(b) = self.tvar_eq.get_bound(tvar).and_then(|b| b.bound.as_ref()) {
            let flags = b.flags();
            (flags, flags)
        } else {
            let lb = self.tvar_sup.get_bound(tvar).and_then(|b| b.bound.as_ref())
                                                  .map_or(T_NONE, |b| b.flags());
            let ub = self.tvar_sub.get_bound(tvar).and_then(|b| b.bound.as_ref())
                                                  .map_or(!T_NONE, |b| b.flags());
            assert_eq!(lb & !ub, T_NONE);
            (lb, ub)
        }
    }

    pub fn get_tvar_exact_type(&self, tvar: TVar) -> Option<Ty> {
        self.tvar_eq.get_bound(tvar).and_then(|b| b.bound.as_ref()).cloned()
    }

    // differs from the trait version because we cannot use generics in trait objects
    pub fn list_rvar_fields<E, F>(&self, mut rvar: RVar, mut f: F) -> result::Result<RVar, E>
        where F: FnMut(&Key, &Slot) -> result::Result<(), E>
    {
        loop {
            if let Some(info) = self.row_infos.get(&rvar.to_usize()) {
                if let Some(ref fields) = info.fields {
                    trace!("{:?} contains {:?}", rvar, fields);
                    for (k, v) in fields.iter() {
                        if let Some(ref v) = *v { // skip negative fields
                            f(k, v)?; // handle user requested break
                        }
                    }
                }
                if let Some(ref next) = info.next {
                    rvar = next.clone();
                } else {
                    return Ok(RVar::any());
                }
            } else {
                // return immediately if the row variable is special or not yet instantiated
                return Ok(rvar);
            }
        }
    }

    pub fn get_available_fields<'a>(&'a self, ty: &Ty) -> Option<HashMap<Key, Slot>> {
        if let Some(mut ty) = self.resolve_exact_type(ty) {
            // if the type includes an explicit nil no direct field access is safe
            if ty.nil() == Nil::Noisy {
                return None;
            }

            // nominal types
            match *ty {
                T::Class(Class::Prototype(cid)) => {
                    let mut fields = HashMap::new();
                    let mut cid = Some(cid);
                    while let Some(def) = cid.and_then(|cid| self.get_class(cid)) {
                        // do not update the existing (children's) fields
                        for (k, v) in def.class_ty.iter() {
                            fields.entry(k.clone()).or_insert(v.clone());
                        }
                        cid = def.parent;
                    }
                    return Some(fields);
                }

                T::Class(Class::Instance(cid)) => {
                    // instance fields take preference over class fields, which get overwritten
                    let mut instfields = HashMap::new();
                    let mut fields = HashMap::new();
                    let mut cid = Some(cid);
                    while let Some(def) = cid.and_then(|cid| self.get_class(cid)) {
                        // do not update the existing (children's) fields
                        for (k, v) in def.instance_ty.iter() {
                            instfields.entry(k.clone()).or_insert(v.clone());
                        }
                        for (k, v) in def.class_ty.iter() {
                            fields.entry(k.clone()).or_insert(v.clone());
                        }
                        cid = def.parent;
                    }
                    fields.extend(instfields.into_iter());
                    return Some(fields);
                }

                _ => {}
            }

            // string types (use the current metatable instead)
            if ty.flags() == T_STRING {
                if let Some(metaslot) = self.get_string_meta() {
                    if let Some(metaty) = self.resolve_exact_type(&metaslot.unlift()) {
                        ty = metaty;
                    }
                }
            }

            // otherwise it should be a record
            if let Some(&Tables::Fields(ref rvar)) = ty.get_tables() {
                let mut fields = HashMap::new();
                self.list_rvar_fields(rvar.clone(), |k, v| -> result::Result<(), ()> {
                    fields.insert(k.clone(), v.clone());
                    Ok(())
                }).expect("list_rvar_fields exited early while we haven't break");
                return Some(fields);
            }
        }

        None
    }

    pub fn fmt_class(&self, cls: Class, f: &mut fmt::Formatter) -> fmt::Result {
        fn class_name(classes: &[ClassDef],
                      cid: ClassId) -> Option<(&Spanned<Name>, &'static str)> {
            let cid = cid.0 as usize;
            if cid < classes.len() {
                if let Some(ref name) = classes[cid].name {
                    let q = if unquotable_name(&name) { "" } else { "`" };
                    return Some((name, q));
                }
            }
            None
        }

        match cls {
            Class::Prototype(cid) => {
                if let Some((name, q)) = class_name(&self.classes, cid) {
                    write!(f, "<prototype for {}{:-?}{}>", q, name, q)
                } else {
                    write!(f, "<prototype for unnamed class #{}>", cid.0)
                }
            }
            Class::Instance(cid) => {
                if let Some((name, q)) = class_name(&self.classes, cid) {
                    write!(f, "{}{:-?}{}", q, name, q)
                } else {
                    write!(f, "<unnamed class #{}>", cid.0)
                }
            }
        }
    }

    pub fn is_subclass_of(&self, mut lhs: ClassId, rhs: ClassId) -> bool {
        if lhs == rhs { return true; }

        while let Some(parent) = self.classes[lhs.0 as usize].parent {
            assert!(parent < lhs);
            lhs = parent;
            if lhs == rhs { return true; }
        }

        false
    }
}

impl<R: Report> ops::Deref for Context<R> {
    type Target = Output;
    fn deref(&self) -> &Output { &self.output }
}

impl<R: Report> ops::DerefMut for Context<R> {
    fn deref_mut(&mut self) -> &mut Output { &mut self.output }
}

impl<R: Report> Report for Context<R> {
    fn message_locale(&self) -> Locale {
        self.report.message_locale()
    }

    fn add_span(&self, k: Kind, s: Span, m: &Localize) -> Result<()> {
        self.report.add_span(k, s, m)
    }
}

impl<R: Report> TypeContext for Context<R> {
    fn gen_report(&self) -> TypeReport {
        TypeReport::new(self.report.message_locale())
    }

    fn last_tvar(&self) -> Option<TVar> {
        self.output.last_tvar()
    }

    fn gen_tvar(&mut self) -> TVar {
        self.next_tvar.0 += 1;
        self.next_tvar
    }

    fn copy_tvar(&mut self, tvar: TVar) -> TVar {
        if self.tvar_eq.get_bound(tvar).map_or(false, |b| b.bound.is_some()) {
            // we have an equal bound, so tvar has no chance to be extended
            trace!("copying {:?} is a no-op", tvar);
            tvar
        } else {
            let tvar_ = self.gen_tvar();
            trace!("copied {:?} to {:?}", tvar, tvar_);
            if let Some(ub) = self.tvar_sub.get_bound(tvar).and_then(|b| b.bound.clone()) {
                let oldub = self.tvar_sub.add_bound(tvar_, &ub);
                assert!(oldub.is_none(), "bounding fresh tvar should not fail");
            }
            if let Some(lb) = self.tvar_sup.get_bound(tvar).and_then(|b| b.bound.clone()) {
                let oldlb = self.tvar_sup.add_bound(tvar_, &lb);
                assert!(oldlb.is_none(), "bounding fresh tvar should not fail");
            }
            tvar_
        }
    }

    fn assert_tvar_sub(&mut self, lhs: TVar, rhs0: &Ty) -> TypeResult<()> {
        let rhs = rhs0.clone().coerce();
        debug!("adding a constraint {:?} <: {:?} (coerced to {:?})", lhs, rhs0, rhs);
        if let Some(eb) = self.tvar_eq.get_bound(lhs).and_then(|b| b.bound.clone()) {
            eb.assert_sub(&rhs, self)?;
        } else {
            if let Some(ub) = self.tvar_sub.add_bound(lhs, &rhs).map(|b| b.clone()) {
                // the original bound is not consistent, bound <: rhs still has to hold
                if let Err(e) = ub.assert_sub(&rhs, self) {
                    info!("variable {:?} cannot have multiple possibly disjoint \
                           bounds (original <: {:?}, later <: {:?}): {:?}", lhs, ub, rhs, e);
                    return Err(e);
                }
            }
            if let Some(lb) = self.tvar_sup.get_bound(lhs).and_then(|b| b.bound.clone()) {
                lb.assert_sub(&rhs, self)?;
            }
        }
        Ok(())
    }

    fn assert_tvar_sup(&mut self, lhs: TVar, rhs: &Ty) -> TypeResult<()> {
        // no coercion here, as type coercion will always expand the type
        debug!("adding a constraint {:?} :> {:?}", lhs, rhs);
        if let Some(eb) = self.tvar_eq.get_bound(lhs).and_then(|b| b.bound.clone()) {
            rhs.assert_sub(&eb, self)?;
        } else {
            if let Some(lb) = self.tvar_sup.add_bound(lhs, rhs).map(|b| b.clone()) {
                // the original bound is not consistent, bound :> rhs still has to hold
                if let Err(e) = rhs.assert_sub(&lb, self) {
                    info!("variable {:?} cannot have multiple possibly disjoint \
                           bounds (original :> {:?}, later :> {:?}): {:?}", lhs, lb, rhs, e);
                    return Err(e);
                }
            }
            if let Some(ub) = self.tvar_sub.get_bound(lhs).and_then(|b| b.bound.clone()) {
                rhs.assert_sub(&ub, self)?;
            }
        }
        Ok(())
    }

    fn assert_tvar_eq(&mut self, lhs: TVar, rhs0: &Ty) -> TypeResult<()> {
        let rhs = rhs0.clone().coerce();
        debug!("adding a constraint {:?} = {:?} (coerced to {:?})", lhs, rhs0, rhs);
        if let Some(eb) = self.tvar_eq.add_bound(lhs, &rhs).map(|b| b.clone()) {
            // the original bound is not consistent, bound = rhs still has to hold
            if let Err(e) = eb.assert_eq(&rhs, self) {
                info!("variable {:?} cannot have multiple possibly disjoint \
                       bounds (original = {:?}, later = {:?}): {:?}", lhs, eb, rhs, e);
                return Err(e);
            }
        } else {
            if let Some(ub) = self.tvar_sub.get_bound(lhs).and_then(|b| b.bound.clone()) {
                rhs.assert_sub(&ub, self)?;
            }
            if let Some(lb) = self.tvar_sup.get_bound(lhs).and_then(|b| b.bound.clone()) {
                lb.assert_sub(&rhs, self)?;
            }
        }
        Ok(())
    }

    fn assert_tvar_sub_tvar(&mut self, lhs: TVar, rhs: TVar) -> TypeResult<()> {
        debug!("adding a constraint {:?} <: {:?}", lhs, rhs);
        if !self.tvar_eq.is(lhs, rhs) {
            if !self.tvar_sub.add_relation(lhs, rhs) {
                // TODO
                return Err(self.gen_report().not_sub(Origin::TVar, "<tvar>", "<tvar>", self));
            }
            if !self.tvar_sup.add_relation(rhs, lhs) {
                // TODO
                return Err(self.gen_report().not_sub(Origin::TVar, "<tvar>", "<tvar>", self));
            }
        }
        Ok(())
    }

    fn assert_tvar_eq_tvar(&mut self, lhs: TVar, rhs: TVar) -> TypeResult<()> {
        debug!("adding a constraint {:?} = {:?}", lhs, rhs);
        // do not update tvar_sub & tvar_sup, tvar_eq will be consulted first
        if !self.tvar_eq.add_relation(lhs, rhs) {
            // TODO
            return Err(self.gen_report().not_eq(Origin::TVar, "<tvar>", "<tvar>", self));
        }
        Ok(())
    }

    fn get_tvar_bounds(&self, tvar: TVar) -> (Flags /*lb*/, Flags /*ub*/) {
        self.output.get_tvar_bounds(tvar)
    }

    fn get_tvar_exact_type(&self, tvar: TVar) -> Option<Ty> {
        self.output.get_tvar_exact_type(tvar)
    }

    fn gen_rvar(&mut self) -> RVar {
        let rvar = self.next_rvar.clone();
        self.next_rvar = RVar::new(rvar.to_usize() + 1);
        rvar
    }

    fn copy_rvar(&mut self, rvar0: RVar) -> RVar {
        let mut fields = HashMap::new();
        let mut rvar = rvar0.clone();

        loop {
            if rvar == RVar::empty() {
                // the original rvar has no chance to be extended, so we can safely return that
                return rvar0;
            }

            if let Some(info) = self.row_infos.get(&rvar.to_usize()) {
                for (k, v) in info.fields.as_ref().unwrap().iter() {
                    if let Some(ref v) = *v {
                        // positive fields can overwrite negative fields
                        let prev = fields.insert(k.clone(), Some(v.clone()));
                        assert!(prev.map_or(true, |t| t.is_none()),
                                "duplicate field {:?} in the chain {:?}..{:?}", k, rvar0, rvar);
                    } else {
                        // negative fields should not overwrite positive fields
                        fields.entry(k.clone()).or_insert(None);
                    }
                }
                if let Some(ref next) = info.next {
                    rvar = next.clone();
                } else {
                    break;
                }
            } else {
                // next row variable has been instantiated but not set any fields, stop here
                break;
            }

            // TODO is recursion detection required here?
        }

        // we know that rvar0 contains `fields` plus an unspecified non-empty row variable,
        // which should be replaced with an (uninstantiated) fresh row variable.
        let rvar = self.gen_rvar();
        self.row_infos.insert(rvar.to_usize(),
                              Box::new(RowInfo { fields: Some(fields), next: None }));
        rvar
    }

    fn assert_rvar_sub(&mut self, lhs: RVar, rhs: RVar) -> TypeResult<()> {
        // TODO
        self.assert_rvar_rel(lhs.clone(), rhs.clone(), true).map_err(|r| {
            r.not_sub(Origin::RVar, "<rvar>", "<rvar>", self)
        })
    }

    fn assert_rvar_eq(&mut self, lhs: RVar, rhs: RVar) -> TypeResult<()> {
        // TODO
        self.assert_rvar_rel(lhs.clone(), rhs.clone(), false).map_err(|r| {
            r.not_eq(Origin::RVar, "<rvar>", "<rvar>", self)
        })
    }

    fn assert_rvar_includes(&mut self, lhs: RVar, rhs: &[(Key, Slot)]) -> TypeResult<()> {
        self.assert_rvar_includes_(lhs.clone(), rhs, true).map_err(|r| {
            r.put(Origin::RVar, format!("the record should include {:?} but didn't", rhs))
        })
    }

    fn assert_rvar_closed(&mut self, mut rvar: RVar) -> TypeResult<()> {
        trace!("{:?} should not be extensible", rvar);

        // detect a cycle by advancing slowrvar 1/2x slower than rvar;
        // if rvar == slowrvar is true after the initial loop, it's a cycle
        let mut slowrvar = rvar.clone();
        let mut slowtick = true;

        loop {
            // a row variable has been already closed
            if rvar == RVar::empty() {
                return Ok(());
            }

            {
                let rvar_ = rvar.to_usize();
                let info = self.row_infos.entry(rvar_).or_insert_with(|| Box::new(RowInfo::new()));
                if let Some(ref next) = info.next {
                    rvar = next.clone();
                } else {
                    info.next = Some(RVar::empty());
                    return Ok(());
                }
            }

            // advance slowrvar on the 2nd, 4th, 6th, ... iterations and compare with rvar
            // slowrvar is guaranteed not to be special, as it has once been rvar previously
            if slowtick {
                slowtick = false;
            } else {
                slowrvar = self.row_infos.get(&slowrvar.to_usize()).unwrap().next.clone().unwrap();
                if slowrvar == rvar {
                    return Err(self.gen_report().put(Origin::RVar,
                                                     "recursive record detected \
                                                      while closing the record".into()));
                }
                slowtick = true;
            }
        }
    }

    fn list_rvar_fields(
        &self, rvar: RVar, f: &mut FnMut(&Key, &Slot) -> result::Result<(), ()>
    ) -> result::Result<RVar, ()> {
        self.output.list_rvar_fields(rvar, f)
    }

    fn fmt_class(&self, cls: Class, f: &mut fmt::Formatter) -> fmt::Result {
        self.output.fmt_class(cls, f)
    }

    fn is_subclass_of(&self, lhs: ClassId, rhs: ClassId) -> bool {
        self.output.is_subclass_of(lhs, rhs)
    }
}

// per-file environment
pub struct Env<'ctx, R: 'ctx> {
    context: &'ctx mut Context<R>,
    opts: Rc<RefCell<Options>>,
    map_index: usize,
    scopes: Vec<Scope>,
    // separate from scoped types, `--# type` will set both
    exported_types: HashMap<Name, TypeDef>,
}

impl<'ctx, R: Report> Env<'ctx, R> {
    pub fn new(context: &'ctx mut Context<R>, opts: Rc<RefCell<Options>>,
               map: ScopeMap<Name>) -> Env<'ctx, R> {
        let map_index = context.scope_maps.len();
        context.scope_maps.push(map);
        let global_frame = Frame { vararg: None, returns: Returns::None };
        Env {
            context: context,
            opts: opts,
            map_index: map_index,
            // we have local variables even at the global position, so we need at least one Scope
            scopes: vec![Scope::new_function(global_frame)],
            exported_types: HashMap::new(),
        }
    }

    // not to be called internally; it intentionally reduces the lifetime
    pub fn context(&mut self) -> &mut Context<R> {
        self.context
    }

    pub fn opts(&self) -> &Rc<RefCell<Options>> {
        &self.opts
    }

    pub fn scope_map(&self) -> &ScopeMap<Name> {
        &self.context.scope_maps[self.map_index]
    }

    // convenience function to avoid mutable references
    pub fn display<'a, 'c, T: Display>(&'c self, x: &'a T) -> Displayed<'a, T, &'c TypeContext> {
        x.display(self.context)
    }

    pub fn id_from_nameref(&self, nameref: &Spanned<NameRef>) -> Spanned<Id> {
        Id::from(self.map_index, nameref.base.clone()).with_loc(nameref)
    }

    pub fn enter(&mut self, scope: Scope) {
        debug!("entering to a scope {:#?}", scope);
        self.scopes.push(scope);
    }

    pub fn leave(&mut self) {
        assert!(self.scopes.len() > 1);
        let scope = self.scopes.pop().unwrap();
        debug!("leaving from a scope {:#?}", scope);
    }

    // returns a pair of type flags that is an exact lower and upper bound for that type
    // used as an approximate type bound testing like arithmetics;
    // better be replaced with a non-instantiating assertion though.
    pub fn get_type_bounds(&self, ty: &Ty) -> (/*lb*/ Flags, /*ub*/ Flags) {
        self.context.get_type_bounds(ty)
    }

    // exactly resolves the type variable inside `ty` if possible
    // this is a requirement for table indexing and function calls
    pub fn resolve_exact_type<'a>(&self, ty: &Ty) -> Option<Ty> {
        self.context.resolve_exact_type(ty)
    }

    pub fn return_from_module(mut self, modname: &[u8], diverging: bool,
                              span: Span) -> Result<Option<Module>> {
        // note that this scope is distinct from the global scope
        let top_scope = self.scopes.drain(..).next().unwrap();
        let returns = match top_scope.frame.unwrap().returns {
            Returns::Implicit(returns) | Returns::Explicit(returns) => Some(returns.into_first()),
            // chunk implicitly returns nil at the end (unless it's diverging)
            Returns::None if !diverging => Some(Ty::noisy_nil()),
            Returns::None | Returns::Never => None,
        };

        let modty = if let Some(returns) = returns {
            if let Some(ty) = self.resolve_exact_type(&returns) {
                let flags = ty.flags();

                // prepare for the worse
                if !flags.is_dynamic() && flags.contains(T_FALSE) {
                    self.error(span, m::ModCannotReturnFalse {}).done()?;
                    return Ok(None);
                }

                // simulate `require` behavior, i.e. nil translates to true
                let ty = if ty.nil() == Nil::Noisy {
                    let tywithoutnil = ty.without_nil().with_loc(span);
                    tywithoutnil.union(&T::True.without_loc(), false, self.context).expect(
                        "failed to union the module return type with True, this should be \
                         always possible because we know the return type doesn't have a tvar!"
                    )
                } else {
                    ty
                };

                Some(ty)
            } else {
                // TODO ideally we would want to resolve type variables in this type
                self.error(span, m::ModCannotReturnInexactType { returns: self.display(&returns) })
                    .done()?;
                return Ok(None);
            }
        } else {
            None
        };

        // this has to be Var since the module is shared across the entire program
        let module = Module {
            returns: modty.map(|ty| Slot::new(F::Var, ty)),
            exported_types: self.exported_types,
        };
        self.context.loaded.insert(modname.to_owned(), LoadStatus::Done(module.clone()));
        Ok(Some(module))
    }

    // not to be called internally; it intentionally reduces the lifetime
    pub fn global_scope(&self) -> &Scope {
        self.context.global_scope()
    }

    // not to be called internally; it intentionally reduces the lifetime
    pub fn global_scope_mut(&mut self) -> &mut Scope {
        self.context.global_scope_mut()
    }

    pub fn current_scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn get_name<'a>(&'a self, nameref: &'a NameRef) -> &'a Name {
        match *nameref {
            NameRef::Local(ref scoped_id) =>
                scoped_id.name(&self.context.scope_maps[self.map_index]),
            NameRef::Global(ref name) => name,
        }
    }

    pub fn get_var<'a>(&'a self, nameref: &NameRef) -> Option<&'a NameDef> {
        self.context.ids.get(&Id::from(self.map_index, nameref.clone()))
    }

    pub fn get_var_mut<'a>(&'a mut self, nameref: &NameRef) -> Option<&'a mut NameDef> {
        self.context.ids.get_mut(&Id::from(self.map_index, nameref.clone()))
    }

    pub fn get_frame<'a>(&'a self) -> &'a Frame {
        for scope in self.scopes.iter().rev() {
            if let Some(frame) = scope.get_frame() { return frame; }
        }
        self.context.global_scope().get_frame().expect("global scope lacks a frame")
    }

    pub fn get_frame_mut<'a>(&'a mut self) -> &'a mut Frame {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(frame) = scope.get_frame_mut() { return frame; }
        }
        self.context.global_scope_mut().get_frame_mut().expect("global scope lacks a frame")
    }

    pub fn get_vararg<'a>(&'a self) -> Option<&'a TySeq> {
        self.get_frame().vararg.as_ref()
    }

    pub fn get_string_meta(&self) -> Option<Spanned<Slot>> {
        self.context.get_string_meta()
    }

    // why do we need this special method?
    // conceptually, assigning to the placeholder `<prototype>.init` should give
    // the exact type for the `init` method, thus also a type for automatically created `new`.
    // in reality we instead get a type bound which cannot be exactly resolved;
    // we instead put [constructor] tag to the placeholder and
    // catch the exact type being assigned (method definitions guarantee the resolvability).
    fn create_new_method_from_init(&mut self, init: &Spanned<Slot>) -> Result<()> {
        // ensure that the type can be resolved...
        let ty = if let Some(ty) = self.resolve_exact_type(&init.unlift()) {
            ty
        } else {
            self.error(init, m::InexactInitMethod { init: self.display(init) }).done()?;
            return Ok(());
        };

        // ...and is a function...
        let mut func = match *ty {
            T::Functions(ref func) => match **func {
                Functions::Simple(ref f) => f.to_owned(),
                _ => {
                    self.error(init, m::OverloadedFuncInitMethod { init: self.display(init) })
                        .done()?;
                    return Ok(());
                }
            },
            _ => {
                self.error(init, m::NonFuncInitMethod { init: self.display(init) }).done()?;
                return Ok(());
            },
        };

        // ...and has the first argument readily known as a [constructible] class instance type.
        let mut cid = None;
        if !func.args.head.is_empty() {
            let selfarg = func.args.head.remove(0);
            if let Some(selfarg) = self.resolve_exact_type(&selfarg) {
                if selfarg.tag() == Some(Tag::Constructible) {
                    if let T::Class(Class::Instance(cid_)) = *selfarg {
                        cid = Some(cid_);
                    }
                }
            }
        }
        if let Some(cid) = cid {
            if !func.argnames.is_empty() {
                func.argnames.remove(0);
            }

            // now `init` is: function(/* removed: [constructible] <%cid> */, ...) -> any
            // fix the return type to make a signature for the `new` method
            let returns = T::Class(Class::Instance(cid));
            let ctor = Function { args: func.args, argnames: func.argnames,
                                  returns: Some(TySeq::from(returns)) };
            let ctor = Slot::new(F::Const, Ty::new(T::func(ctor)));

            debug!("implicitly setting the `new` method of {:?} as {:?}", cid, ctor);
            let cdef = self.context.get_class_mut(cid).expect("invalid ClassId");
            let key = Key::Str(b"new"[..].into());
            let prevctor = cdef.class_ty.insert(key, ctor);
            assert_eq!(prevctor, None); // should have been prevented
        } else {
            self.error(init, m::BadSelfInInitMethod { init: self.display(init) }).done()?;
            return Ok(());
        }

        Ok(())
    }

    // returns false if the assignment is failed and constraints should not be added
    fn assign_special(&mut self, lhs: &Spanned<Slot>, rhs: &Spanned<Slot>) -> Result<bool> {
        match lhs.tag() {
            Some(b @ Tag::PackagePath) |
            Some(b @ Tag::PackageCpath) => {
                if let Some(s) = self.resolve_exact_type(&rhs.unlift())
                                     .and_then(|t| t.as_string().map(|s| s.to_owned())) {
                    let ret = if b == Tag::PackagePath {
                        self.opts.borrow_mut().set_package_path(&s)
                    } else {
                        self.opts.borrow_mut().set_package_cpath(&s)
                    };

                    // the implementation may have reported by its own
                    match ret {
                        Ok(()) => {}
                        Err(None) => {
                            self.warn(rhs, m::CannotAssignToPackagePath { name: b.name() }).done()?;
                            return Ok(false);
                        }
                        Err(Some(stop)) => {
                            return Err(stop);
                        }
                    }
                } else {
                    self.warn(rhs, m::UnknownAssignToPackagePath { name: b.name() }).done()?;
                }
            }

            Some(Tag::Constructible) => {
                self.error(lhs, m::SelfCannotBeAssignedInCtor {}).done()?;
                return Ok(false);
            }

            Some(Tag::Constructor) => {
                self.create_new_method_from_init(rhs)?;
            }

            _ => {}
        }

        Ok(true)
    }

    fn assign_(&mut self, lhs: &Spanned<Slot>, rhs: &Spanned<Slot>, init: bool) -> Result<()> {
        if self.assign_special(lhs, rhs)? {
            if lhs.accept(rhs, self.context, init).is_err() {
                self.error(lhs, m::CannotAssign { lhs: self.display(lhs), rhs: self.display(rhs) })
                    .note_if(rhs, m::OtherTypeOrigin {})
                    .done()?;
            }
        }
        Ok(())
    }

    // same to Slot::accept but also able to handle the built-in semantics;
    // should be used for any kind of non-internal assignments.
    pub fn assign(&mut self, lhs: &Spanned<Slot>, rhs: &Spanned<Slot>) -> Result<()> {
        trace!("assigning {:?} to an existing slot {:?}", rhs, lhs);
        self.assign_(lhs, rhs, false)
    }

    // same to `assign` but the slot is assumed to be newly created (out of field)
    // and the strict equality instead of subtyping is applied.
    // this is required because the slot itself is generated before doing any assignment;
    // the usual notion of accepting by subtyping does not work well here.
    // this is technically two assignments, of which the latter is done via the strict equality.
    pub fn assign_new(&mut self, lhs: &Spanned<Slot>, initrhs: &Spanned<Slot>,
                      specrhs: Option<&Spanned<Slot>>) -> Result<()> {
        trace!("assigning {:?} to a new slot {:?} with type {:?}", initrhs, lhs, specrhs);

        // first assignment of initrhs to specrhs, if any
        let specrhs = if let Some(specrhs) = specrhs {
            if !self.assign_special(specrhs, initrhs)? { return Ok(()); }
            if let Err(r) = specrhs.accept(initrhs, self.context, true) {
                self.error(specrhs, m::CannotAssign { lhs: self.display(specrhs),
                                                      rhs: self.display(initrhs) })
                    .note_if(initrhs, m::OtherTypeOrigin {})
                    .report_types(r, TypeReportHint::None)
                    .done()?;
            }
            Cow::Borrowed(specrhs)
        } else {
            Cow::Owned(initrhs.clone().map(|s| s.coerce()))
        };

        // second assignment of specrhs (or initrhs) to lhs
        specrhs.adapt(lhs.flex(), self.context);
        if !self.assign_special(lhs, &specrhs)? { return Ok(()); }
        if let Err(r) = lhs.assert_eq(&*specrhs, self.context) {
            self.error(lhs, m::CannotAssign { lhs: self.display(lhs),
                                              rhs: self.display(&specrhs) })
                .note_if(&*specrhs, m::OtherTypeOrigin {})
                .report_types(r, TypeReportHint::None)
                .done()?;
        }
        Ok(())
    }

    pub fn ensure_var(&mut self, nameref: &Spanned<NameRef>) -> Result<Slot> {
        trace!("ensuring {:?} has been initialized", nameref);
        let id = self.id_from_nameref(nameref);

        let defslot;
        {
            let def = self.context.ids.get_mut(&id);
            let mut def = def.expect("Env::ensure_var with an undefined var");
            match def.slot {
                NameSlot::None => {
                    // do not try to accept the slot again
                    let nil = Slot::just(Ty::noisy_nil());
                    def.slot = NameSlot::Set(nil.clone());
                    return Ok(nil);
                }
                NameSlot::Unset(ref slot) => {
                    // needs to be updated, but we hold the context right now.
                    // clone the slot and continue from the outside.
                    defslot = slot.clone().with_loc(def.span);
                }
                NameSlot::Set(ref slot) => {
                    return Ok(slot.clone());
                }
            }
        }

        // not yet set but typed (e.g. `local x --: string`), the type should accept `nil`
        let nil = Slot::just(Ty::noisy_nil()).without_loc();
        if self.assign_special(&defslot, &nil)? {
            if defslot.accept(&nil, self.context, true).is_ok() { // this IS still initialization
                self.context.ids.get_mut(&id).unwrap().slot = NameSlot::Set(defslot.base.clone());
            } else {
                // won't alter the set flag, so subsequent uses are still errors
                self.error(&id, m::UseOfUnassignedVar {})
                    .note_if(&defslot, m::UnassignedVarOrigin { var: self.display(&defslot) })
                    .done()?;
            }
        }

        Ok(defslot.base)
    }

    fn name_class_if_any(&mut self, id: &Spanned<Id>, info: &Spanned<Slot>) -> Result<()> {
        match **info.unlift() {
            T::Class(Class::Prototype(cid)) => {
                let name = id.name(&self.context).clone().with_loc(id);

                // check if the name conflicts in the type namespace earlier
                // note that even when the type is defined in the global scope
                // we check both the local and global scope for the type name
                if let Some(def) = self.get_named_type(&name) {
                    self.error(&name, m::CannotRedefineTypeAsClass { name: &name.base })
                        .note(def.span, m::AlreadyDefinedType {})
                        .done()?;
                    return Ok(());
                }

                self.context.name_class(cid, name.clone())?;

                let scope = match id.base {
                    Id::Local(..) => self.current_scope_mut(),
                    Id::Global(..) => self.global_scope_mut(),
                };
                let ret = scope.put_type(name, Ty::new(T::Class(Class::Instance(cid))));
                assert!(ret, "failed to insert the type");
            }

            T::Union(ref u) => {
                // should raise an error if a prototype is used within a union
                let is_prototype = |&cls| if let Class::Prototype(_) = cls { true } else { false };
                if u.classes.iter().any(is_prototype) {
                    self.error(info, m::CannotNameUnknownClass { cls: self.display(info) })
                        .done()?;
                }
            }

            _ => {}
        }

        Ok(())
    }

    // adds a local variable with the explicit type `specinfo` and the implicit type `initinfo`.
    pub fn add_var(&mut self, nameref: &Spanned<NameRef>,
                   specinfo: Option<Spanned<Slot>>,
                   initinfo: Option<Spanned<Slot>>) -> Result<()> {
        let id = self.id_from_nameref(nameref);
        debug!("adding a variable {} with {:?} (specified) and {:?} (initialized)",
               id.display(&self.context), specinfo, initinfo);

        // raise an error in any case that the nameref is defined with a spec multiple times.
        // practically this means that the global var has been redefined.
        // (local variables are created uniquely and unlikely to trigger this with a correct AST)
        if self.context.ids.get(&id).is_some() {
            let name = id.name(&self.context);
            self.error(nameref, m::CannotRedefineVar { name: name }).done()?;
            return Ok(());
        }

        let slot = if let Some(initinfo) = initinfo {
            let specinfo = specinfo.unwrap_or_else(|| initinfo.clone().map(|s| s.coerce()));
            self.assign_(&specinfo, &initinfo, true)?;

            // name the class if it is currently unnamed
            self.name_class_if_any(&id, &initinfo)?;

            NameSlot::Set(specinfo.base)
        } else if let Some(specinfo) = specinfo {
            NameSlot::Unset(specinfo.base)
        } else {
            NameSlot::None
        };

        self.context.ids.insert(id.base, NameDef { span: id.span, slot: slot });
        Ok(())
    }

    pub fn add_local_var_already_set(&mut self, scoped_id: &Spanned<ScopedId>,
                                     info: Spanned<Slot>) -> Result<()> {
        let id = Id::Local(self.map_index, scoped_id.base.clone()).with_loc(scoped_id);
        debug!("adding a local variable {} already set to {:?}", id.display(&self.context), info);

        // we cannot blindly `accept` the `initinfo`, since it will discard the flexibility
        // (e.g. if the callee requests `F::Var`, we need to keep that).
        // therefore we just remap `F::Just` to `F::Var`.
        info.adapt(F::Var, self.context);

        self.name_class_if_any(&id, &info)?;

        self.context.ids.insert(id.base, NameDef { span: id.span, slot: NameSlot::Set(info.base) });
        Ok(())
    }

    // assigns to a global or local variable with a right-hand-side type of `info`.
    // it may create a new global variable if there is no variable with that name.
    pub fn assign_to_var(&mut self, nameref: &Spanned<NameRef>, info: Spanned<Slot>) -> Result<()> {
        let id = self.id_from_nameref(nameref);

        let (previnfo, prevset, needslotassign) = if self.context.ids.contains_key(&id.base) {
            let mut def = self.context.ids.get_mut(&id.base).unwrap();
            let (previnfo, prevset, needslotassign) = match def.slot {
                NameSlot::None => (info.base.clone(), false, false),
                NameSlot::Unset(ref slot) => (slot.clone(), false, true),
                NameSlot::Set(ref slot) => (slot.clone(), true, true),
            };
            def.slot = NameSlot::Set(previnfo.clone());
            (previnfo, prevset, needslotassign)
        } else {
            self.context.ids.insert(id.base.clone(),
                                    NameDef { span: id.span,
                                              slot: NameSlot::Set(info.base.clone()) });
            (info.base.clone(), true, true)
        };
        debug!("assigning {:?} to a variable {} with type {:?}",
               info, id.display(&self.context), previnfo);

        if needslotassign {
            self.assign_(&previnfo.with_loc(&id), &info, !prevset)?;
        }
        self.name_class_if_any(&id, &info)?;
        Ok(())
    }

    fn assume_special(&mut self, info: &Spanned<Slot>) -> Result<()> {
        match info.tag() {
            Some(Tag::StringMeta) => {
                if let Some(ref prevmeta) = self.context.string_meta {
                    // while it is possible to alter the string metatable from C,
                    // we don't think that it is useful after the initialization.
                    self.error(info, m::CannotRedefineStringMeta {})
                        .note(prevmeta, m::PreviousStringMeta {})
                        .done()?;
                }
                self.context.string_meta = Some(info.clone());
            }
            _ => {}
        }

        Ok(())
    }

    pub fn assume_var(&mut self, name: &Spanned<NameRef>, info: Spanned<Slot>) -> Result<()> {
        let id = Id::from(self.map_index, name.base.clone());
        debug!("(force) adding a variable {} as {:?}", id.display(&self.context), info);

        self.assume_special(&info)?;

        let mut def = self.context.ids.entry(id).or_insert_with(|| {
            NameDef { span: name.span, slot: NameSlot::None }
        });
        def.slot = NameSlot::Set(info.base);

        Ok(())
    }

    pub fn get_tvar_bounds(&self, tvar: TVar) -> (Flags /*lb*/, Flags /*ub*/) {
        self.context.get_tvar_bounds(tvar)
    }

    pub fn get_tvar_exact_type(&self, tvar: TVar) -> Option<Ty> {
        self.context.get_tvar_exact_type(tvar)
    }

    pub fn get_named_global_type<'a>(&'a self, name: &Name) -> Option<&'a TypeDef> {
        self.context.global_scope().get_type(name)
    }

    pub fn get_named_local_type<'a>(&'a self, name: &Name) -> Option<&'a TypeDef> {
        for scope in self.scopes.iter().rev() {
            if let Some(def) = scope.get_type(name) { return Some(def); }
        }
        None
    }

    pub fn get_named_type<'a>(&'a self, name: &Name) -> Option<&'a TypeDef> {
        self.get_named_local_type(name).or_else(|| self.get_named_global_type(name))
    }

    pub fn define_local_type(&mut self, name: &Spanned<Name>, ty: Ty) -> Result<()> {
        if let Some(def) = self.get_named_local_type(name) {
            self.error(name, m::CannotRedefineLocalType { name: &name.base })
                .note(def.span, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        } else if let Some(def) = self.get_named_global_type(name) {
            self.error(name, m::CannotRedefineGlobalType { name: &name.base })
                .note(def.span, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        }

        let ret = self.current_scope_mut().put_type(name.clone(), ty);
        assert!(ret, "failed to insert the type");
        Ok(())
    }

    pub fn define_global_type(&mut self, name: &Spanned<Name>, ty: Ty) -> Result<()> {
        if let Some(def) = self.get_named_local_type(name) {
            self.error(name, m::CannotRedefineLocalTypeAsGlobal { name: &name.base })
                .note(def.span, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        } else if let Some(def) = self.get_named_global_type(name) {
            self.error(name, m::CannotRedefineGlobalType { name: &name.base })
                .note(def.span, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        }

        let ret = self.global_scope_mut().put_type(name.clone(), ty);
        assert!(ret, "failed to insert the type");
        Ok(())
    }

    pub fn define_and_export_type(&mut self, name: &Spanned<Name>, ty: Ty) -> Result<()> {
        if let Some(def) = self.get_named_type(name) {
            self.error(name, m::CannotRedefineAndReexportType { name: &name.base })
                .note(def.span, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        }

        // insert to the exported types (distinct from scoped types)
        let defspan = match self.exported_types.entry(name.base.clone()) {
            hash_map::Entry::Vacant(e) => {
                e.insert(TypeDef { ty: ty.clone(), span: name.span });
                None
            },
            hash_map::Entry::Occupied(e) => Some(e.get().span),
        };
        if let Some(defspan) = defspan {
            // if the parser has been working correctly this should be impossible,
            // because a set of exported types should be a subset of top-level local types.
            // therefore this error can only occur when the AST is not well-formed.
            self.error(name, m::CannotRedefineAndReexportType { name: &name.base })
                .note(defspan, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        }

        // insert a locally scoped type
        let ret = self.current_scope_mut().put_type(name.clone(), ty);
        assert!(ret, "failed to insert the type");
        Ok(())
    }

    pub fn redefine_global_type(&mut self, name: &Spanned<Name>, tyspan: Span) -> Result<()> {
        let ty = if let Some(def) = self.get_named_local_type(name) {
            def.ty.clone()
        } else if let Some(def) = self.get_named_global_type(name) {
            self.error(name, m::CannotRedefineGlobalType { name: &name.base })
                .note(def.span, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        } else {
            self.error(tyspan, m::NoType { name: &name.base }).done()?;
            Ty::dummy()
        };

        let ret = self.global_scope_mut().put_type(name.clone(), ty);
        assert!(ret, "failed to insert the type");
        Ok(())
    }

    pub fn reexport_local_type(&mut self, name: &Spanned<Name>, tyspan: Span) -> Result<()> {
        let ty = if let Some(def) = self.get_named_local_type(name) {
            def.ty.clone()
        } else if let Some(def) = self.get_named_global_type(name) {
            self.error(name, m::CannotRedefineGlobalType { name: &name.base })
                .note(def.span, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        } else {
            self.error(tyspan, m::NoType { name: &name.base }).done()?;
            Ty::dummy()
        };

        // insert to the exported types
        let defspan = match self.exported_types.entry(name.base.clone()) {
            hash_map::Entry::Vacant(e) => {
                e.insert(TypeDef { ty: ty.clone(), span: name.span });
                None
            },
            hash_map::Entry::Occupied(e) => Some(e.get().span),
        };
        if let Some(defspan) = defspan {
            // see `Env::define_and_export_type` for the possibility of this case
            self.error(name, m::CannotReexportType { name: &name.base })
                .note(defspan, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        }

        Ok(())
    }

    pub fn import_types(&mut self, typedefs: Spanned<HashMap<Name, TypeDef>>) -> Result<()> {
        for (name, def) in typedefs.base.into_iter() {
            if let Some(prevdef) = self.get_named_type(&name) {
                self.error(typedefs.span, m::CannotImportAlreadyDefinedType { name: &name })
                    .note(prevdef.span, m::AlreadyDefinedType {})
                    .done()?;
                return Ok(());
            }

            let ret = self.current_scope_mut().put_type(name.with_loc(def.span), def.ty);
            assert!(ret, "failed to insert the type");
        }

        Ok(())
    }
}

impl<'ctx, R: Report> Report for Env<'ctx, R> {
    fn message_locale(&self) -> Locale {
        self.context.report.message_locale()
    }

    fn add_span(&self, k: Kind, s: Span, m: &Localize) -> Result<()> {
        self.context.report.add_span(k, s, m)
    }
}

impl<'ctx, R: Report> TypeResolver for Env<'ctx, R> {
    fn context(&self) -> &TypeContext {
        self.context
    }

    fn context_mut(&mut self) -> &mut TypeContext {
        self.context
    }

    fn ty_from_name(&self, name: &Spanned<Name>) -> Result<Ty> {
        if let Some(def) = self.get_named_type(name) {
            Ok(def.ty.clone())
        } else {
            self.error(name, m::NoType { name: &name.base }).done()?;
            Ok(Ty::dummy())
        }
    }
}

#[test]
fn test_context_is_send_and_sync() {
    use kailua_diag::NoReport;

    fn _assert_send<T: Send>(_x: T) {}
    fn _assert_sync<T: Sync>(_x: T) {}

    _assert_send(Context::new(NoReport));
    _assert_sync(Context::new(NoReport));
}

#[test]
fn test_context_tvar() {
    use kailua_diag::NoReport;

    let mut ctx = Context::new(NoReport);

    { // idempotency of bounds
        let v1 = ctx.gen_tvar();
        assert!(ctx.assert_tvar_sub(v1, &Ty::new(T::Integer)).is_ok());
        assert!(ctx.assert_tvar_sub(v1, &Ty::new(T::Integer)).is_ok());
        assert!(ctx.assert_tvar_sub(v1, &Ty::new(T::String)).is_err());
    }

    { // empty bounds (lb & ub = bottom)
        let v1 = ctx.gen_tvar();
        assert!(ctx.assert_tvar_sub(v1, &Ty::new(T::Integer)).is_ok());
        assert!(ctx.assert_tvar_sup(v1, &Ty::new(T::String)).is_err());

        let v2 = ctx.gen_tvar();
        assert!(ctx.assert_tvar_sup(v2, &Ty::new(T::Integer)).is_ok());
        assert!(ctx.assert_tvar_sub(v2, &Ty::new(T::String)).is_err());
    }

    { // empty bounds (lb & ub != bottom)
        let v1 = ctx.gen_tvar();
        assert!(ctx.assert_tvar_sub(v1, &Ty::new(T::ints(vec![3, 4, 5]))).is_ok());
        assert!(ctx.assert_tvar_sup(v1, &Ty::new(T::ints(vec![1, 2, 3]))).is_err());

        let v2 = ctx.gen_tvar();
        assert!(ctx.assert_tvar_sup(v2, &Ty::new(T::ints(vec![3, 4, 5]))).is_ok());
        assert!(ctx.assert_tvar_sub(v2, &Ty::new(T::ints(vec![1, 2, 3]))).is_err());
    }

    { // implicitly disjoint bounds
        let v1 = ctx.gen_tvar();
        let v2 = ctx.gen_tvar();
        assert!(ctx.assert_tvar_sub_tvar(v1, v2).is_ok());
        assert!(ctx.assert_tvar_sub(v2, &Ty::new(T::String)).is_ok());
        assert!(ctx.assert_tvar_sub(v1, &Ty::new(T::Integer)).is_err());

        let v3 = ctx.gen_tvar();
        let v4 = ctx.gen_tvar();
        assert!(ctx.assert_tvar_sub_tvar(v3, v4).is_ok());
        assert!(ctx.assert_tvar_sup(v3, &Ty::new(T::String)).is_ok());
        assert!(ctx.assert_tvar_sup(v4, &Ty::new(T::Integer)).is_err());
    }

    { // equality propagation
        let v1 = ctx.gen_tvar();
        assert!(ctx.assert_tvar_eq(v1, &Ty::new(T::Integer)).is_ok());
        assert!(ctx.assert_tvar_sub(v1, &Ty::new(T::Number)).is_ok());
        assert!(ctx.assert_tvar_sup(v1, &Ty::new(T::String)).is_err());

        let v2 = ctx.gen_tvar();
        assert!(ctx.assert_tvar_sub(v2, &Ty::new(T::Number)).is_ok());
        assert!(ctx.assert_tvar_eq(v2, &Ty::new(T::Integer)).is_ok());
        assert!(ctx.assert_tvar_sup(v2, &Ty::new(T::String)).is_err());

        let v3 = ctx.gen_tvar();
        assert!(ctx.assert_tvar_sub(v3, &Ty::new(T::Number)).is_ok());
        assert!(ctx.assert_tvar_eq(v3, &Ty::new(T::String)).is_err());
    }
}

