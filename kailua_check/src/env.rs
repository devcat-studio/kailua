use std::mem;
use std::ops;
use std::str;
use std::fmt;
use std::cell::RefCell;
use std::rc::Rc;
use std::collections::{hash_map, HashMap, HashSet};
use vec_map::VecMap;
use atomic::Atomic;
use atomic::Ordering::Relaxed;

use kailua_env::{self, Span, Spanned, WithLoc, ScopedId, ScopeMap, SpanMap};
use kailua_diag::{self, Kind, Report, Reporter, Localize};
use kailua_syntax::{Name, NameRef};
use diag::{CheckResult, unquotable_name};
use ty::{Ty, TySeq, Nil, T, Slot, F, TVar, RVar, Lattice, Union, Tag, Displayed, Display};
use ty::{TypeContext, TypeResolver, ClassId, Class, Functions, Function, Key};
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
pub struct Frame {
    pub vararg: Option<TySeq>,
    pub returns: Option<TySeq>, // None represents the bottom (TySeq does not have it)
    pub returns_exact: bool, // if false, returns can be updated
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

    fn add_relation(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()> {
        if lhs == rhs { return Ok(()); }

        let lhs_ = self.bounds.find(lhs.0 as usize);
        let rhs_ = self.bounds.find(rhs.0 as usize);
        if lhs_ == rhs_ { return Ok(()); }

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
            (true, true) =>
                if lhsbound == rhsbound {
                    lhsbound
                } else {
                    return Err(format!("variables {:?}/{:?} cannot have multiple bounds \
                                        (left {} {:?}, right {} {:?})",
                                       lhs, rhs, self.op, lhsbound, self.op, rhsbound));
                },
        };

        // update the shared bound to the merged representative
        let new = self.bounds.union(lhs_, rhs_);
        if !is_bound_trivial(&bound) {
            // the merged entry should have non-zero rank, so unwrap() is fine
            self.bounds.get_mut(&new).unwrap().bound = bound;
        }

        Ok(())
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

enum LoadStatus {
    Done(Slot),
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
        let global_frame = Frame { vararg: None, returns: None, returns_exact: false };
        ctx.global_scope.frame = Some(global_frame);
        ctx
    }

    pub fn report(&self) -> &R {
        &self.report
    }

    pub fn open_library(&mut self, name: &Spanned<Name>,
                        opts: Rc<RefCell<Options>>) -> CheckResult<()> {
        let name_ = str::from_utf8(&name.base).map_err(|e| e.to_string())?;
        if let Some(defs) = get_defs(name_) {
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
            Ok(())
        } else {
            Err(format!("cannot open an unknown library {:?}", name.base))
        }
    }

    pub fn get_loaded_module(&self, name: &[u8], span: Span) -> CheckResult<Option<Slot>> {
        match self.loaded.get(name) {
            Some(&LoadStatus::Done(ref slot)) => Ok(Some(slot.clone())),
            None => Ok(None),

            // this is allowed in Lua 5.2 and later, but will result in a loop anyway.
            Some(&LoadStatus::Ongoing(oldspan)) => {
                self.error(span, m::RecursiveRequire {})
                    .note(oldspan, m::PreviousRequire {})
                    .done()?;
                Ok(Some(Slot::dummy()))
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

    pub fn name_class(&mut self, cid: ClassId, name: Spanned<Name>) -> CheckResult<()> {
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

    fn assert_rvar_rel(&mut self, lhs: RVar, rhs: RVar, is_sub: bool) -> CheckResult<()> {
        // simulate a tail recursion
        let mut lhs = lhs;
        let mut rhs = rhs;

        loop {
            trace!("{:?} should be {} {:?}", lhs, if is_sub { "<:" } else { "=" }, rhs);

            if lhs == rhs {
                return Ok(());
            }

            // an empty row is special and does not equal to any other "normal" rows
            match (lhs == RVar::empty(), rhs == RVar::empty()) {
                (false, false) | (true, true) => {}
                (false, true) => { return Err(format!("{:?} is not empty", lhs)); }
                (true, false) => { return Err(format!("{:?} is not empty", rhs)); }
            }

            let lhs_ = lhs.to_usize();
            let rhs_ = rhs.to_usize();

            // populate the row infos if needed
            self.row_infos.entry(lhs_).or_insert_with(|| Box::new(RowInfo::new()));
            self.row_infos.entry(rhs_).or_insert_with(|| Box::new(RowInfo::new()));

            let mut matching = Vec::new();
            let mut lmissing = Vec::new(); // positive fields missing in lfields
            let mut rmissing = Vec::new(); // positive fields missing in rfields
            let mut labsent = Vec::new(); // negative fields missing in lfields
            let mut rabsent = Vec::new(); // negative fields missing in rfields

            let (lnext, rnext) = {
                // get the row infos, and bail out if recursion is detected
                let lrow = self.row_infos.get(&lhs_).unwrap();
                let rrow = self.row_infos.get(&rhs_).unwrap();
                let lfields = if let Some(ref fields) = lrow.fields {
                    fields
                } else {
                    return Err("recursive row instantiation".to_string());
                };
                let rfields = if let Some(ref fields) = rrow.fields {
                    fields
                } else {
                    return Err("recursive row instantiation".to_string());
                };

                // collect missing fields from rhs and also remaining matching fields
                // (we cannot immediately check for them due to borrowing)
                for (k, lv) in lfields.iter() {
                    match (lv, rfields.get(k)) {
                        // both lhs and rhs has the same positive field, check the relation
                        (&Some(ref lv), Some(&Some(ref rv))) => {
                            matching.push((k.clone(), lv.clone(), rv.clone()));
                        }

                        // lhs has a field and rhs does not, rhs' next row variable should have it
                        (&Some(ref lv), None) => {
                            rmissing.push((k.clone(), lv.clone()));
                        }

                        // lhs has a negative field which should be also in rhs
                        // if the field is missing in rhs it should be in rhs' next row variable
                        (&None, Some(&None)) => {}
                        (&None, None) => {
                            rabsent.push(k.clone());
                        }

                        // otherwise unification fails
                        (&None, Some(&Some(_))) => {
                            return Err(format!("{:?} cannot have a field {:?}", lhs, k));
                        }
                        (&Some(_), Some(&None)) => {
                            return Err(format!("{:?} cannot have a field {:?}", rhs, k));
                        }
                    }
                }

                // collect missing fields from lhs (matches have been already checked)
                for (k, rv) in rfields.iter() {
                    match (lfields.get(k), rv) {
                        (Some(&Some(_)), &Some(_)) => {}
                        (None, &Some(ref rv)) => {
                            lmissing.push((k.clone(), rv.clone()));
                        }
                        (Some(&None), &None) => {}
                        (None, &None) => {
                            labsent.push(k.clone());
                        }
                        (Some(&None), &Some(_)) => {
                            return Err(format!("{:?} cannot have a field {:?}", lhs, k));
                        }
                        (Some(&Some(_)), &None) => {
                            return Err(format!("{:?} cannot have a field {:?}", rhs, k));
                        }
                    }
                }

                (lrow.next.clone(), rrow.next.clone())
            };

            // now check for the matching fields
            if is_sub {
                for (_k, lv, rv) in matching {
                    lv.assert_sub(&rv, self)?;
                }
            } else {
                for (_k, lv, rv) in matching {
                    lv.assert_eq(&rv, self)?;
                }
            }

            // ensure that two next row variables are identical (w.r.t. given relation).
            // this is primarily done by instantiating them as needed and recursing later.
            let (lnext, rnext) = match (lnext, rnext) {
                // if both have been already instantiated we can simply unify them,
                // i.e. do the tail recursion with next variables.
                (Some(lnext), Some(rnext)) => (lnext, rnext),

                // if only one of them has been instantiated, another should be newly instantiated.
                // it will have an exclusion set of all keys in corresponding fields.
                // then do the tail recursion with instantiated variables.
                (Some(lnext), None) => {
                    let rnext = self.gen_rvar();

                    let mut row = Box::new(RowInfo::new());
                    {
                        let rfields = self.row_infos.get(&rhs_).unwrap().fields.as_ref().unwrap();
                        let mut fields = row.fields.as_mut().unwrap();
                        fields.extend(rfields.keys().map(|k| (k.clone(), None)));
                    }
                    self.row_infos.insert(rnext.to_usize(), row);

                    self.row_infos.get_mut(&rhs_).unwrap().next = Some(rnext.clone());
                    (lnext, rnext)
                },

                (None, Some(rnext)) => {
                    let lnext = self.gen_rvar();

                    let mut row = Box::new(RowInfo::new());
                    {
                        let lfields = self.row_infos.get(&lhs_).unwrap().fields.as_ref().unwrap();
                        let mut fields = row.fields.as_mut().unwrap();
                        fields.extend(lfields.keys().map(|k| (k.clone(), None)));
                    }
                    self.row_infos.insert(lnext.to_usize(), row);

                    self.row_infos.get_mut(&lhs_).unwrap().next = Some(lnext.clone());
                    (lnext, rnext)
                },

                // if none has been instantiated, only one row variable gets newly instantiated,
                // and it will have an exclusion set of all keys _in both fields_.
                //
                // this is same to instantiating two variables, setting exclusion sets and
                // immediately unifying them, but avoids an infinite loop because
                // unifying one variable with itself trivially ends the recursion.
                (None, None) => {
                    let next = self.gen_rvar();

                    let mut row = Box::new(RowInfo::new());
                    {
                        let lfields = self.row_infos.get(&lhs_).unwrap().fields.as_ref().unwrap();
                        let rfields = self.row_infos.get(&rhs_).unwrap().fields.as_ref().unwrap();
                        let mut fields = row.fields.as_mut().unwrap();
                        fields.extend(lfields.keys().map(|k| (k.clone(), None)));
                        fields.extend(rfields.keys().map(|k| (k.clone(), None)));
                    }
                    self.row_infos.insert(next.to_usize(), row);

                    self.row_infos.get_mut(&lhs_).unwrap().next = Some(next.clone());
                    self.row_infos.get_mut(&rhs_).unwrap().next = Some(next.clone());
                    (next.clone(), next)
                },
            };

            // finally, fill any positive or negative fields to the *next* row variable.
            // at this point we have instantiated both next variables so no check is needed.
            //
            // it might be possible that any of them is empty,
            // or has been already included or excluded when lnext = rnext;
            // assert_rvar_includes_and_excludes should handle such cases correctly.
            self.assert_rvar_includes_and_excludes(lnext.clone(), &lmissing, &labsent)?;
            self.assert_rvar_includes_and_excludes(rnext.clone(), &rmissing, &rabsent)?;

            lhs = lnext;
            rhs = rnext;
        }
    }

    fn assert_rvar_includes_and_excludes(&mut self, lhs: RVar, includes: &[(Key, Slot)],
                                         excludes: &[Key]) -> CheckResult<()> {
        trace!("{:?} should include {:?} and exclude {:?}", lhs, includes, excludes);

        // optimize a no-op just in case
        if includes.is_empty() && excludes.is_empty() {
            return Ok(());
        }

        let lhs_ = lhs.to_usize();

        // take fields out, so that we can detect an infinite recursion
        let (mut fields, next) = {
            let row = self.row_infos.entry(lhs_).or_insert_with(|| Box::new(RowInfo::new()));
            if let Some(fields) = row.fields.take() {
                (fields, row.next.clone())
            } else {
                return Err("recursive row instantiation".to_string());
            }
        };

        let e = inner(self, lhs, includes, excludes, &mut fields, next);
        self.row_infos.get_mut(&lhs_).unwrap().fields = Some(fields);
        return e;

        fn inner<R: Report>(ctx: &mut Context<R>, lhs: RVar,
                            includes: &[(Key, Slot)], excludes: &[Key],
                            fields: &mut HashMap<Key, Option<Slot>>,
                            next: Option<RVar>) -> CheckResult<()> {
            // collect missing fields, whether positive or negative, and
            // check if other matching fields are compatible
            let mut missing = Vec::new();
            let mut absent = Vec::new();
            for &(ref k, ref rv) in includes {
                match fields.get(k) {
                    Some(&Some(ref lv)) => {
                        // the existing fields should be compatible
                        rv.assert_sub(lv, ctx)?;
                    }
                    Some(&None) => {
                        // the field is excluded, immediately fail
                        return Err(format!("{:?} cannot have a field {:?}", lhs, k));
                    }
                    None => {
                        // the field should be added to the next row variable (if any)
                        missing.push((k.clone(), rv.clone()));
                    }
                }
            }
            for k in excludes {
                match fields.get(k) {
                    Some(&Some(ref lv)) => {
                        return Err(format!("{:?} should not have a field {:?} \
                                            but already had {:?}", lhs, k, lv));
                    }
                    Some(&None) => {}
                    None => {
                        absent.push(k.clone());
                    }
                }
            }

            // if we have missing fields they should be in the next row variable if any;
            // we can avoid instantiation when it has not yet been instantiated though
            if let Some(next) = next {
                // we need to put fields back, so this cannot be a tail recursion
                ctx.assert_rvar_includes_and_excludes(next, &missing, &absent)?;
            } else {
                fields.extend(missing.into_iter().map(|(k, v)| (k, Some(v))));
                fields.extend(absent.into_iter().map(|k| (k, None)));
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
    fn add_span(&self, k: Kind, s: Span, m: &Localize) -> kailua_diag::Result<()> {
        self.report.add_span(k, s, m)
    }
}

impl<R: Report> TypeContext for Context<R> {
    fn last_tvar(&self) -> Option<TVar> {
        self.output.last_tvar()
    }

    fn gen_tvar(&mut self) -> TVar {
        self.next_tvar.0 += 1;
        self.next_tvar
    }

    fn assert_tvar_sub(&mut self, lhs: TVar, rhs: &Ty) -> CheckResult<()> {
        debug!("adding a constraint {:?} <: {:?}", lhs, *rhs);
        if let Some(eb) = self.tvar_eq.get_bound(lhs).and_then(|b| b.bound.clone()) {
            eb.assert_sub(rhs, self)?;
        } else {
            if let Some(ub) = self.tvar_sub.add_bound(lhs, rhs).map(|b| b.clone()) {
                // the original bound is not consistent, bound <: rhs still has to hold
                if let Err(e) = ub.assert_sub(rhs, self) {
                    info!("variable {:?} cannot have multiple possibly disjoint \
                           bounds (original <: {:?}, later <: {:?}): {}", lhs, ub, *rhs, e);
                    return Err(e);
                }
            }
            if let Some(lb) = self.tvar_sup.get_bound(lhs).and_then(|b| b.bound.clone()) {
                lb.assert_sub(rhs, self)?;
            }
        }
        Ok(())
    }

    fn assert_tvar_sup(&mut self, lhs: TVar, rhs: &Ty) -> CheckResult<()> {
        debug!("adding a constraint {:?} :> {:?}", lhs, *rhs);
        if let Some(eb) = self.tvar_eq.get_bound(lhs).and_then(|b| b.bound.clone()) {
            rhs.assert_sub(&eb, self)?;
        } else {
            if let Some(lb) = self.tvar_sup.add_bound(lhs, rhs).map(|b| b.clone()) {
                // the original bound is not consistent, bound :> rhs still has to hold
                if let Err(e) = rhs.assert_sub(&lb, self) {
                    info!("variable {:?} cannot have multiple possibly disjoint \
                           bounds (original :> {:?}, later :> {:?}): {}", lhs, lb, *rhs, e);
                    return Err(e);
                }
            }
            if let Some(ub) = self.tvar_sub.get_bound(lhs).and_then(|b| b.bound.clone()) {
                rhs.assert_sub(&ub, self)?;
            }
        }
        Ok(())
    }

    fn assert_tvar_eq(&mut self, lhs: TVar, rhs: &Ty) -> CheckResult<()> {
        debug!("adding a constraint {:?} = {:?}", lhs, *rhs);
        if let Some(eb) = self.tvar_eq.add_bound(lhs, rhs).map(|b| b.clone()) {
            // the original bound is not consistent, bound = rhs still has to hold
            if let Err(e) = eb.assert_eq(rhs, self) {
                info!("variable {:?} cannot have multiple possibly disjoint \
                       bounds (original = {:?}, later = {:?}): {}", lhs, eb, *rhs, e);
                return Err(e);
            }
        } else {
            if let Some(ub) = self.tvar_sub.get_bound(lhs).and_then(|b| b.bound.clone()) {
                rhs.assert_sub(&ub, self)?;
            }
            if let Some(lb) = self.tvar_sup.get_bound(lhs).and_then(|b| b.bound.clone()) {
                lb.assert_sub(rhs, self)?;
            }
        }
        Ok(())
    }

    fn assert_tvar_sub_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()> {
        debug!("adding a constraint {:?} <: {:?}", lhs, rhs);
        if !self.tvar_eq.is(lhs, rhs) {
            self.tvar_sub.add_relation(lhs, rhs)?;
            self.tvar_sup.add_relation(rhs, lhs)?;
        }
        Ok(())
    }

    fn assert_tvar_eq_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()> {
        debug!("adding a constraint {:?} = {:?}", lhs, rhs);
        // do not update tvar_sub & tvar_sup, tvar_eq will be consulted first
        self.tvar_eq.add_relation(lhs, rhs)
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

    fn assert_rvar_sub(&mut self, lhs: RVar, rhs: RVar) -> CheckResult<()> {
        self.assert_rvar_rel(lhs, rhs, true)
    }

    fn assert_rvar_eq(&mut self, lhs: RVar, rhs: RVar) -> CheckResult<()> {
        self.assert_rvar_rel(lhs, rhs, false)
    }

    fn assert_rvar_includes(&mut self, lhs: RVar, rhs: &[(Key, Slot)]) -> CheckResult<()> {
        self.assert_rvar_includes_and_excludes(lhs, rhs, &[])
    }

    fn assert_rvar_closed(&mut self, mut rvar: RVar) -> CheckResult<()> {
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
                    return Err("recursive row instantiation".to_string());
                }
                slowtick = true;
            }
        }
    }

    fn list_rvar_fields(&self, mut rvar: RVar,
                        f: &mut FnMut(&Key, &Slot) -> CheckResult<bool>) -> CheckResult<RVar> {
        loop {
            if let Some(info) = self.row_infos.get(&rvar.to_usize()) {
                if let Some(ref fields) = info.fields {
                    trace!("{:?} contains {:?}", rvar, fields);
                    for (k, v) in fields.iter() {
                        if let Some(ref v) = *v { // skip negative fields
                            if !f(k, v)? { // user requested break
                                return Ok(rvar);
                            }
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
}

impl<'ctx, R: Report> Env<'ctx, R> {
    pub fn new(context: &'ctx mut Context<R>, opts: Rc<RefCell<Options>>,
               map: ScopeMap<Name>) -> Env<'ctx, R> {
        let map_index = context.scope_maps.len();
        context.scope_maps.push(map);
        let global_frame = Frame { vararg: None, returns: None, returns_exact: false };
        Env {
            context: context,
            opts: opts,
            map_index: map_index,
            // we have local variables even at the global position, so we need at least one Scope
            scopes: vec![Scope::new_function(global_frame)],
        }
    }

    // not to be called internally; it intentionally reduces the lifetime
    pub fn context(&mut self) -> &mut Context<R> {
        self.context
    }

    pub fn opts(&self) -> &Rc<RefCell<Options>> {
        &self.opts
    }

    // convenience function to avoid mutable references
    pub fn display<'a, 'c, T: Display>(&'c self, x: &'a T) -> Displayed<'a, 'c, T>
            where Displayed<'a, 'c, T>: fmt::Display {
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

    pub fn return_from_module(mut self, modname: &[u8], span: Span) -> CheckResult<Slot> {
        // note that this scope is distinct from the global scope
        let top_scope = self.scopes.drain(..).next().unwrap();
        let returns = if let Some(returns) = top_scope.frame.unwrap().returns {
            returns.into_first()
        } else {
            Ty::noisy_nil()
        };

        if let Some(ty) = self.resolve_exact_type(&returns) {
            let flags = ty.flags();

            // prepare for the worse
            if !flags.is_dynamic() && flags.contains(T_FALSE) {
                self.error(span, m::ModCannotReturnFalse {}).done()?;
                return Ok(Slot::dummy());
            }

            // simulate `require` behavior, i.e. nil translates to true
            let ty = if ty.nil() == Nil::Noisy {
                ty.without_nil().with_loc(span).union(&T::True.without_loc(), false, self.context)?
            } else {
                ty
            };

            // this has to be Var since the module is shared across the entire program
            let slot = Slot::new(F::Var, ty);
            self.context.loaded.insert(modname.to_owned(), LoadStatus::Done(slot.clone()));
            Ok(slot)
        } else {
            // TODO ideally we would want to resolve type variables in this type
            self.error(span, m::ModCannotReturnInexactType { returns: self.display(&returns) })
                .done()?;
            Ok(Slot::dummy())
        }
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
    fn create_new_method_from_init(&mut self, init: &Spanned<Slot>) -> CheckResult<()> {
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
            // now `init` is: function(/* removed: [constructible] <%cid> */, ...) -> any
            // fix the return type to make a signature for the `new` method
            let returns = T::Class(Class::Instance(cid));
            let ctor = Function { args: func.args, returns: TySeq::from(returns) };
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
    fn assign_special(&mut self, lhs: &Spanned<Slot>, rhs: &Spanned<Slot>) -> CheckResult<bool> {
        match lhs.tag() {
            Some(b @ Tag::PackagePath) |
            Some(b @ Tag::PackageCpath) => {
                if let Some(s) = self.resolve_exact_type(&rhs.unlift())
                                     .and_then(|t| t.as_string().map(|s| s.to_owned())) {
                    if b == Tag::PackagePath {
                        self.opts.borrow_mut().set_package_path(&s)?;
                    } else {
                        self.opts.borrow_mut().set_package_cpath(&s)?;
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

    fn assign_(&mut self, lhs: &Spanned<Slot>, rhs: &Spanned<Slot>, init: bool) -> CheckResult<()> {
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
    pub fn assign(&mut self, lhs: &Spanned<Slot>, rhs: &Spanned<Slot>) -> CheckResult<()> {
        trace!("assigning {:?} to an existing slot {:?}", rhs, lhs);
        self.assign_(lhs, rhs, false)
    }

    // same to `assign` but the slot is assumed to be newly created (out of field)
    // and the strict equality instead of subtyping is applied.
    // this is required because the slot itself is generated before doing any assignment;
    // the usual notion of accepting by subtyping does not work well here.
    // this is technically two assignments, of which the latter is done via the strict equality.
    pub fn assign_new(&mut self, lhs: &Spanned<Slot>, initrhs: &Spanned<Slot>,
                      specrhs: Option<&Spanned<Slot>>) -> CheckResult<()> {
        trace!("assigning {:?} to a new slot {:?} with type {:?}", initrhs, lhs, specrhs);

        // first assignment of initrhs to specrhs, if any
        let specrhs = if let Some(specrhs) = specrhs {
            if !self.assign_special(initrhs, specrhs)? { return Ok(()); }
            specrhs.accept(initrhs, self.context, true)?;
            specrhs
        } else {
            initrhs
        };

        // second assignment of specrhs (or initrhs) to lhs
        specrhs.adapt(lhs.flex(), self.context);
        if !self.assign_special(lhs, specrhs)? { return Ok(()); }
        lhs.assert_eq(specrhs, self.context)
    }

    pub fn ensure_var(&mut self, nameref: &Spanned<NameRef>) -> CheckResult<Slot> {
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

    fn name_class_if_any(&mut self, id: &Spanned<Id>, info: &Spanned<Slot>) -> CheckResult<()> {
        match **info.unlift() {
            T::Class(Class::Prototype(cid)) => {
                let name = id.name(&self.context).clone().with_loc(id);

                // check if the name conflicts in the type namespace earlier
                // note that even when the type is defined in the global scope
                // we check both the local and global scope for the type name
                if let Some(def) = self.get_named_type(&name) {
                    self.error(&name, m::CannotRedefineType { name: &name.base })
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
                   initinfo: Option<Spanned<Slot>>) -> CheckResult<()> {
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
            let specinfo = specinfo.unwrap_or_else(|| initinfo.clone());
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
                                     info: Spanned<Slot>) -> CheckResult<()> {
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
    pub fn assign_to_var(&mut self, nameref: &Spanned<NameRef>,
                         info: Spanned<Slot>) -> CheckResult<()> {
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

    fn assume_special(&mut self, info: &Spanned<Slot>) -> CheckResult<()> {
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

    pub fn assume_var(&mut self, name: &Spanned<NameRef>, info: Spanned<Slot>) -> CheckResult<()> {
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

    pub fn get_named_type<'a>(&'a self, name: &Name) -> Option<&'a TypeDef> {
        for scope in self.scopes.iter().rev() {
            if let Some(def) = scope.get_type(name) { return Some(def); }
        }
        if let Some(def) = self.context.global_scope().get_type(name) { return Some(def); }
        None
    }

    pub fn define_type(&mut self, name: &Spanned<Name>, ty: Ty) -> CheckResult<()> {
        if let Some(def) = self.get_named_type(name) {
            self.error(name, m::CannotRedefineType { name: &name.base })
                .note(def.span, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        }

        let ret = self.current_scope_mut().put_type(name.clone(), ty);
        assert!(ret, "failed to insert the type");
        Ok(())
    }
}

impl<'ctx, R: Report> Report for Env<'ctx, R> {
    fn add_span(&self, k: Kind, s: Span, m: &Localize) -> kailua_diag::Result<()> {
        self.context.report.add_span(k, s, m)
    }
}

impl<'ctx, R: Report> TypeResolver for Env<'ctx, R> {
    fn context(&mut self) -> &mut TypeContext {
        self.context
    }

    fn ty_from_name(&self, name: &Spanned<Name>) -> CheckResult<Ty> {
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
        assert_eq!(ctx.assert_tvar_sub(v1, &Ty::new(T::Integer)), Ok(()));
        assert_eq!(ctx.assert_tvar_sub(v1, &Ty::new(T::Integer)), Ok(()));
        assert!(ctx.assert_tvar_sub(v1, &Ty::new(T::String)).is_err());
    }

    { // empty bounds (lb & ub = bottom)
        let v1 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub(v1, &Ty::new(T::Integer)), Ok(()));
        assert!(ctx.assert_tvar_sup(v1, &Ty::new(T::String)).is_err());

        let v2 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sup(v2, &Ty::new(T::Integer)), Ok(()));
        assert!(ctx.assert_tvar_sub(v2, &Ty::new(T::String)).is_err());
    }

    { // empty bounds (lb & ub != bottom)
        let v1 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub(v1, &Ty::new(T::ints(vec![3, 4, 5]))), Ok(()));
        assert!(ctx.assert_tvar_sup(v1, &Ty::new(T::ints(vec![1, 2, 3]))).is_err());

        let v2 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sup(v2, &Ty::new(T::ints(vec![3, 4, 5]))), Ok(()));
        assert!(ctx.assert_tvar_sub(v2, &Ty::new(T::ints(vec![1, 2, 3]))).is_err());
    }

    { // implicitly disjoint bounds
        let v1 = ctx.gen_tvar();
        let v2 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub_tvar(v1, v2), Ok(()));
        assert_eq!(ctx.assert_tvar_sub(v2, &Ty::new(T::String)), Ok(()));
        assert!(ctx.assert_tvar_sub(v1, &Ty::new(T::Integer)).is_err());

        let v3 = ctx.gen_tvar();
        let v4 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub_tvar(v3, v4), Ok(()));
        assert_eq!(ctx.assert_tvar_sup(v3, &Ty::new(T::String)), Ok(()));
        assert!(ctx.assert_tvar_sup(v4, &Ty::new(T::Integer)).is_err());
    }

    { // equality propagation
        let v1 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_eq(v1, &Ty::new(T::Integer)), Ok(()));
        assert_eq!(ctx.assert_tvar_sub(v1, &Ty::new(T::Number)), Ok(()));
        assert!(ctx.assert_tvar_sup(v1, &Ty::new(T::String)).is_err());

        let v2 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub(v2, &Ty::new(T::Number)), Ok(()));
        assert_eq!(ctx.assert_tvar_eq(v2, &Ty::new(T::Integer)), Ok(()));
        assert!(ctx.assert_tvar_sup(v2, &Ty::new(T::String)).is_err());

        let v3 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub(v3, &Ty::new(T::Number)), Ok(()));
        assert!(ctx.assert_tvar_eq(v3, &Ty::new(T::String)).is_err());
    }
}

