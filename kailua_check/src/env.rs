use std::mem;
use std::ops;
use std::str;
use std::fmt;
use std::cell::{Cell, RefCell};
use std::rc::Rc;
use std::collections::{hash_map, HashMap, HashSet};
use vec_map::VecMap;

use kailua_env::{self, Span, Spanned, WithLoc, ScopedId, ScopeMap, SpanMap};
use kailua_diag::{self, Kind, Report, Reporter, Localize};
use kailua_syntax::{Name, NameRef};
use diag::{CheckResult, unquotable_name};
use ty::{Ty, TySeq, T, Slot, F, TVar, Mark, Lattice, Builtin, Displayed, Display};
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

    pub fn name<'a>(&'a self, ctx: &'a Context) -> &'a Name {
        match *self {
            Id::Local(map_index, ref scoped_id) => scoped_id.name(&ctx.scope_maps[map_index]),
            Id::Global(ref name) => name,
        }
    }

    pub fn scope(&self, ctx: &Context) -> Option<kailua_env::Scope> {
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

    pub fn display<'a>(&'a self, ctx: &'a Context) -> IdDisplay<'a> {
        IdDisplay { id: self, ctx: ctx }
    }
}

#[must_use]
pub struct IdDisplay<'a> {
    id: &'a Id,
    ctx: &'a Context,
}

impl<'a> fmt::Display for IdDisplay<'a> {
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
    pub slot: Slot,
    pub set: bool,
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
    parent: Cell<u32>,
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
        Box::new(Bound { parent: Cell::new(parent as u32), rank: rank as u8, bound: None })
    }

    fn read(&self) -> (usize /*parent*/, usize /*rank*/) {
        (self.parent.get() as usize, self.rank as usize)
    }

    fn write_parent(&self, parent: usize) {
        self.parent.set(parent as u32);
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
    fn add_bound<'a>(&'a mut self, lhs: TVar, rhs: &Ty) -> Option<&'a T> {
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

#[derive(Copy, Clone, Debug)]
enum Rel { Eq, Sup }

#[derive(Debug)]
struct MarkDeps {
    // this mark implies the following mark
    follows: Option<Mark>,
    // the preceding mark implies this mark
    precedes: Option<Mark>,
    // constraints over types for this mark to be true
    // the first type is considered the base type and should be associated to this mark forever
    constraints: Option<(Ty, Vec<(Rel, Ty)>)>,
}

impl MarkDeps {
    fn new() -> MarkDeps {
        MarkDeps { follows: None, precedes: None, constraints: None }
    }

    fn assert_true(self, ctx: &mut TypeContext) -> CheckResult<()> {
        if let Some((ref base, ref others)) = self.constraints {
            for &(rel, ref other) in others {
                match rel {
                    Rel::Eq => base.assert_eq(other, ctx)?,
                    Rel::Sup => other.assert_sub(base, ctx)?,
                }
            }
        }
        if let Some(follows) = self.follows {
            ctx.assert_mark_true(follows)?;
        }
        Ok(())
    }

    fn assert_false(self, ctx: &mut TypeContext) -> CheckResult<()> {
        if let Some(precedes) = self.precedes {
            ctx.assert_mark_false(precedes)?;
        }
        Ok(())
    }

    fn merge(self, other: MarkDeps, ctx: &mut TypeContext) -> CheckResult<MarkDeps> {
        // while technically possible, the base type should be equal for the simplicity.
        let constraints = match (self.constraints, other.constraints) {
            (None, None) => None,
            (None, Some(r)) => Some(r),
            (Some(l), None) => Some(l),
            (Some((lb, mut lt)), Some((rb, rt))) => {
                lb.assert_eq(&rb, ctx)?;
                lt.extend(rt.into_iter());
                Some((lb, lt))
            }
        };

        let merge_marks = |l: Option<Mark>, r: Option<Mark>| match (l, r) {
            (None, None) => None,
            (None, Some(r)) => Some(r),
            (Some(l), None) => Some(l),
            (Some(_), Some(_)) => panic!("non-linear deps detected"),
        };

        let follows = merge_marks(self.follows, other.follows);
        let precedes = merge_marks(self.precedes, other.precedes);
        Ok(MarkDeps { follows: follows, precedes: precedes, constraints: constraints })
    }
}

#[derive(Debug)]
enum MarkValue {
    Invalid,
    True,
    False,
    Unknown(Option<Box<MarkDeps>>),
}

impl MarkValue {
    fn assert_true(self, mark: Mark, ctx: &mut TypeContext) -> CheckResult<()> {
        match self {
            MarkValue::Invalid => panic!("self-recursive mark resolution"),
            MarkValue::True => Ok(()),
            MarkValue::False => Err(format!("mark {:?} cannot be true", mark)),
            MarkValue::Unknown(None) => Ok(()),
            MarkValue::Unknown(Some(deps)) => deps.assert_true(ctx),
        }
    }

    fn assert_false(self, mark: Mark, ctx: &mut TypeContext) -> CheckResult<()> {
        match self {
            MarkValue::Invalid => panic!("self-recursive mark resolution"),
            MarkValue::True => Err(format!("mark {:?} cannot be false", mark)),
            MarkValue::False => Ok(()),
            MarkValue::Unknown(None) => Ok(()),
            MarkValue::Unknown(Some(deps)) => deps.assert_false(ctx),
        }
    }
}

#[derive(Debug)]
struct MarkInfo {
    parent: Cell<u32>,
    rank: u8,
    value: MarkValue,
}

impl Partition for Box<MarkInfo> {
    fn create(parent: usize, rank: usize) -> Box<MarkInfo> {
        Box::new(MarkInfo { parent: Cell::new(parent as u32), rank: rank as u8,
                            value: MarkValue::Unknown(None) })
    }

    fn read(&self) -> (usize /*parent*/, usize /*rank*/) {
        (self.parent.get() as usize, self.rank as usize)
    }

    fn write_parent(&self, parent: usize) {
        self.parent.set(parent as u32);
    }

    fn increment_rank(&mut self) {
        self.rank += 1;
    }
}

enum LoadStatus {
    Done(Slot),
    Ongoing(Span), // span for who to blame
}

// global context (also acts as a type context).
// anything has to be retained across multiple files should be here
pub struct Context {
    report: Rc<Report>,

    // name, scope and span information
    ids: HashMap<Id, NameDef>,
    scope_maps: Vec<ScopeMap<Name>>,
    spanned_slots: SpanMap<Slot>,

    // TODO this might be eventually found useless
    global_scope: Scope,

    // type variable information
    next_tvar: Cell<TVar>,
    tvar_sub: Constraints, // upper bound
    tvar_sup: Constraints, // lower bound
    tvar_eq: Constraints, // tight bound

    // mark information
    next_mark: Cell<Mark>,
    mark_infos: Partitions<Box<MarkInfo>>,

    // module information
    opened: HashSet<String>,
    loaded: HashMap<Vec<u8>, LoadStatus>, // corresponds to `package.loaded`

    // runtime information
    string_meta: Option<Spanned<Slot>>,

    // classes defined
    classes: Vec<ClassDef>,
}

impl Context {
    pub fn new(report: Rc<Report>) -> Context {
        let mut ctx = Context {
            report: report,
            ids: HashMap::new(),
            scope_maps: Vec::new(),
            spanned_slots: SpanMap::new(),
            global_scope: Scope::new(),
            next_tvar: Cell::new(TVar(1)), // TVar(0) for the top-level return
            tvar_sub: Constraints::new("<:"),
            tvar_sup: Constraints::new(":>"),
            tvar_eq: Constraints::new("="),
            next_mark: Cell::new(Mark(0)),
            mark_infos: Partitions::new(),
            opened: HashSet::new(),
            loaded: HashMap::new(),
            string_meta: None,
            classes: Vec::new(),
        };

        // it is fine to return from the top-level, so we treat it as like a function frame
        let global_frame = Frame { vararg: None, returns: None, returns_exact: false };
        ctx.global_scope.frame = Some(global_frame);
        ctx
    }

    pub fn report(&self) -> &Report {
        &*self.report
    }

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

    pub fn get_class_mut<'a>(&'a mut self, cid: ClassId) -> Option<&'a mut ClassDef> {
        self.classes.get_mut(cid.0 as usize)
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
        let cls = &mut self.classes[cid.0 as usize];
        if let Some(ref prevname) = cls.name {
            self.report.warn(name, m::RedefinedClassName {})
                       .note(prevname, m::PreviousClassName {})
                       .done()?;
        } else {
            cls.name = Some(name);
        }
        Ok(())
    }

    // returns a pair of type flags that is an exact lower and upper bound for that type
    // used as an approximate type bound testing like arithmetics;
    // better be replaced with a non-instantiating assertion though.
    pub fn get_type_bounds(&self, ty: &T) -> (/*lb*/ Flags, /*ub*/ Flags) {
        let flags = ty.flags();
        let (lb, ub) = ty.get_tvar().map_or((T_NONE, T_NONE), |v| self.get_tvar_bounds(v));
        (flags | lb, flags | ub)
    }

    // exactly resolves the type variable inside `ty` if possible
    // this is a requirement for table indexing and function calls
    pub fn resolve_exact_type<'a>(&mut self, ty: &Ty) -> Option<Ty> {
        match ty.split_tvar() {
            (None, None) => unreachable!(),
            (None, Some(t)) => Some(t.clone()),
            (Some(tv), None) => self.get_tvar_exact_type(tv),
            (Some(tv), Some(t)) => {
                if let Some(t_) = self.get_tvar_exact_type(tv) {
                    Some(t.union(&t_, self))
                } else {
                    None
                }
            }
        }
    }

    pub fn get_string_meta(&self) -> Option<Spanned<Slot>> {
        self.string_meta.clone()
    }

    // used by assert_mark_require_{eq,sup}
    fn assert_mark_require(&mut self, mark: Mark, base: &Ty, rel: Rel, ty: &Ty) -> CheckResult<()> {
        let mark_ = self.mark_infos.find(mark.0 as usize);

        let mut value = {
            let info = self.mark_infos.entry(mark_).or_insert_with(|| Partition::create(mark_, 0));
            mem::replace(&mut info.value, MarkValue::Invalid)
        };

        let ret = (|value: &mut MarkValue| {
            match *value {
                MarkValue::Invalid => panic!("self-recursive mark resolution"),
                MarkValue::True => match rel {
                    Rel::Eq => base.assert_eq(ty, self),
                    Rel::Sup => ty.assert_sub(base, self),
                },
                MarkValue::False => Ok(()),
                MarkValue::Unknown(ref mut deps) => {
                    if deps.is_none() { *deps = Some(Box::new(MarkDeps::new())); }
                    let deps = deps.as_mut().unwrap();

                    // XXX probably we can test if `base (rel) ty` this early with a wrapped context
                    if let Some(ref mut constraints) = deps.constraints {
                        base.assert_eq(&mut constraints.0, self)?;
                        constraints.1.push((rel, ty.clone()));
                    } else {
                        deps.constraints = Some((base.clone(), vec![(rel, ty.clone())]));
                    }
                    Ok(())
                }
            }
        })(&mut value);

        self.mark_infos.get_mut(&mark_).unwrap().value = value;
        ret
    }
}

impl Report for Context {
    fn add_span(&self, k: Kind, s: Span, m: &Localize) -> kailua_diag::Result<()> {
        self.report.add_span(k, s, m)
    }
}

impl TypeContext for Context {
    fn last_tvar(&self) -> Option<TVar> {
        let tvar = self.next_tvar.get();
        if tvar == TVar(0) { None } else { Some(TVar(tvar.0 - 1)) }
    }

    fn gen_tvar(&mut self) -> TVar {
        let tvar = self.next_tvar.get();
        self.next_tvar.set(TVar(tvar.0 + 1));
        tvar
    }

    fn assert_tvar_sub(&mut self, lhs: TVar, rhs: &Ty) -> CheckResult<()> {
        debug!("adding a constraint {:?} <: {:?}", lhs, *rhs);
        if let Some(eb) = self.tvar_eq.get_bound(lhs).and_then(|b| b.bound.clone()) {
            (*eb).assert_sub(rhs, self)?;
        } else {
            if let Some(ub) = self.tvar_sub.add_bound(lhs, rhs).map(|b| b.clone().into_send()) {
                // the original bound is not consistent, bound <: rhs still has to hold
                if let Err(e) = ub.assert_sub(rhs, self) {
                    info!("variable {:?} cannot have multiple possibly disjoint \
                           bounds (original <: {:?}, later <: {:?}): {}", lhs, ub, *rhs, e);
                    return Err(e);
                }
            }
            if let Some(lb) = self.tvar_sup.get_bound(lhs).and_then(|b| b.bound.clone()) {
                (*lb).assert_sub(rhs, self)?;
            }
        }
        Ok(())
    }

    fn assert_tvar_sup(&mut self, lhs: TVar, rhs: &Ty) -> CheckResult<()> {
        debug!("adding a constraint {:?} :> {:?}", lhs, *rhs);
        if let Some(eb) = self.tvar_eq.get_bound(lhs).and_then(|b| b.bound.clone()) {
            rhs.assert_sub(&*eb, self)?;
        } else {
            if let Some(lb) = self.tvar_sup.add_bound(lhs, rhs).map(|b| b.clone().into_send()) {
                // the original bound is not consistent, bound :> rhs still has to hold
                if let Err(e) = rhs.assert_sub(&lb, self) {
                    info!("variable {:?} cannot have multiple possibly disjoint \
                           bounds (original :> {:?}, later :> {:?}): {}", lhs, lb, *rhs, e);
                    return Err(e);
                }
            }
            if let Some(ub) = self.tvar_sub.get_bound(lhs).and_then(|b| b.bound.clone()) {
                rhs.assert_sub(&*ub, self)?;
            }
        }
        Ok(())
    }

    fn assert_tvar_eq(&mut self, lhs: TVar, rhs: &Ty) -> CheckResult<()> {
        debug!("adding a constraint {:?} = {:?}", lhs, *rhs);
        if let Some(eb) = self.tvar_eq.add_bound(lhs, rhs).map(|b| b.clone().into_send()) {
            // the original bound is not consistent, bound = rhs still has to hold
            if let Err(e) = eb.assert_eq(rhs, self) {
                info!("variable {:?} cannot have multiple possibly disjoint \
                       bounds (original = {:?}, later = {:?}): {}", lhs, eb, *rhs, e);
                return Err(e);
            }
        } else {
            if let Some(ub) = self.tvar_sub.get_bound(lhs).and_then(|b| b.bound.clone()) {
                rhs.assert_sub(&*ub, self)?;
            }
            if let Some(lb) = self.tvar_sup.get_bound(lhs).and_then(|b| b.bound.clone()) {
                (*lb).assert_sub(rhs, self)?;
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

    fn get_tvar_exact_type(&self, tvar: TVar) -> Option<Ty> {
        self.tvar_eq.get_bound(tvar).and_then(|b| b.bound.as_ref()).cloned()
    }

    fn gen_mark(&mut self) -> Mark {
        let mark = self.next_mark.get();
        self.next_mark.set(Mark(mark.0 + 1));
        mark
    }

    fn assert_mark_true(&mut self, mark: Mark) -> CheckResult<()> {
        debug!("asserting {:?} is true", mark);
        let mark_ = self.mark_infos.find(mark.0 as usize);
        let value = {
            // take the value out of the mapping. even if the mark is somehow recursively consulted
            // (which is normally an error), it should assume that it's true by now.
            let info = self.mark_infos.entry(mark_).or_insert_with(|| Partition::create(mark_, 0));
            mem::replace(&mut info.value, MarkValue::True)
        };
        value.assert_true(mark, self)
    }

    fn assert_mark_false(&mut self, mark: Mark) -> CheckResult<()> {
        debug!("asserting {:?} is false", mark);
        let mark_ = self.mark_infos.find(mark.0 as usize);
        let value = {
            // same as above, but it's false instead
            let info = self.mark_infos.entry(mark_).or_insert_with(|| Partition::create(mark_, 0));
            mem::replace(&mut info.value, MarkValue::False)
        };
        value.assert_false(mark, self)
    }

    fn assert_mark_eq(&mut self, lhs: Mark, rhs: Mark) -> CheckResult<()> {
        debug!("asserting {:?} and {:?} are same", lhs, rhs);

        if lhs == rhs { return Ok(()); }

        let lhs_ = self.mark_infos.find(lhs.0 as usize);
        let rhs_ = self.mark_infos.find(rhs.0 as usize);
        if lhs_ == rhs_ { return Ok(()); }

        { // early error checks
            let lvalue = self.mark_infos.get(&lhs_).map(|info| &info.value);
            let rvalue = self.mark_infos.get(&rhs_).map(|info| &info.value);
            match (lvalue, rvalue) {
                (Some(&MarkValue::Invalid), _) | (_, Some(&MarkValue::Invalid)) =>
                    panic!("self-recursive mark resolution"),
                (Some(&MarkValue::True), Some(&MarkValue::True)) => return Ok(()),
                (Some(&MarkValue::True), Some(&MarkValue::False)) =>
                    return Err(format!("{:?} (known to be true) and {:?} (known to be false) \
                                        cannot never be same", lhs, rhs)),
                (Some(&MarkValue::False), Some(&MarkValue::True)) =>
                    return Err(format!("{:?} (known to be false) and {:?} (known to be true) \
                                        cannot never be same", lhs, rhs)),
                (Some(&MarkValue::False), Some(&MarkValue::False)) => return Ok(()),
                (_, _) => {}
            }
        }

        fn take_value(mark_infos: &mut VecMap<Box<MarkInfo>>, i: usize) -> MarkValue {
            if let Some(info) = mark_infos.get_mut(&i) {
                mem::replace(&mut info.value, MarkValue::Invalid)
            } else {
                MarkValue::Unknown(None)
            }
        }

        let lvalue = take_value(&mut self.mark_infos, lhs_);
        let rvalue = take_value(&mut self.mark_infos, rhs_);

        let new = self.mark_infos.union(lhs_, rhs_);

        let newvalue = match (lvalue, rvalue) {
            (MarkValue::Invalid, _) | (_, MarkValue::Invalid) |
            (MarkValue::True, MarkValue::True) |
            (MarkValue::True, MarkValue::False) |
            (MarkValue::False, MarkValue::True) |
            (MarkValue::False, MarkValue::False) => unreachable!(),

            (MarkValue::True, MarkValue::Unknown(deps)) |
            (MarkValue::Unknown(deps), MarkValue::True) => {
                if let Some(deps) = deps { deps.assert_true(self)?; }
                MarkValue::True
            }

            (MarkValue::False, MarkValue::Unknown(deps)) |
            (MarkValue::Unknown(deps), MarkValue::False) => {
                if let Some(deps) = deps { deps.assert_false(self)?; }
                MarkValue::False
            }

            (MarkValue::Unknown(None), MarkValue::Unknown(None)) =>
                MarkValue::Unknown(None),
            (MarkValue::Unknown(None), MarkValue::Unknown(Some(deps))) |
            (MarkValue::Unknown(Some(deps)), MarkValue::Unknown(None)) =>
                MarkValue::Unknown(Some(deps)),

            // the only case that we need the true merger of dependencies
            (MarkValue::Unknown(Some(mut ldeps)), MarkValue::Unknown(Some(mut rdeps))) => {
                // implication may refer to each other; they should be eliminated first
                if ldeps.follows  == Some(Mark(rhs_ as u32)) { ldeps.follows  = None; }
                if ldeps.precedes == Some(Mark(rhs_ as u32)) { ldeps.precedes = None; }
                if rdeps.follows  == Some(Mark(lhs_ as u32)) { rdeps.follows  = None; }
                if rdeps.precedes == Some(Mark(lhs_ as u32)) { rdeps.precedes = None; }

                let deps = ldeps.merge(*rdeps, self)?;

                // update dependencies for *other* marks depending on this mark
                if let Some(m) = deps.follows {
                    let info = self.mark_infos.get_mut(&(m.0 as usize)).unwrap();
                    if let MarkValue::Unknown(Some(ref mut deps)) = info.value {
                        assert!(deps.precedes == Some(Mark(lhs_ as u32)) ||
                                deps.precedes == Some(Mark(rhs_ as u32)));
                        deps.precedes = Some(Mark(new as u32));
                    } else {
                        panic!("desynchronized dependency implication \
                                from {:?} or {:?} to {:?}", lhs, rhs, m);
                    }
                }
                if let Some(m) = deps.precedes {
                    let info = self.mark_infos.get_mut(&(m.0 as usize)).unwrap();
                    if let MarkValue::Unknown(Some(ref mut deps)) = info.value {
                        assert!(deps.follows == Some(Mark(lhs_ as u32)) ||
                                deps.follows == Some(Mark(rhs_ as u32)));
                        deps.follows = Some(Mark(new as u32));
                    } else {
                        panic!("desynchronized dependency implication \
                                from {:?} to {:?} or {:?}", m, lhs, rhs);
                    }
                }

                MarkValue::Unknown(Some(Box::new(deps)))
            }
        };

        self.mark_infos.get_mut(&new).unwrap().value = newvalue;
        Ok(())
    }

    fn assert_mark_imply(&mut self, lhs: Mark, rhs: Mark) -> CheckResult<()> {
        debug!("asserting {:?} implies {:?}", lhs, rhs);

        if lhs == rhs { return Ok(()); }

        let lhs_ = self.mark_infos.find(lhs.0 as usize);
        let rhs_ = self.mark_infos.find(rhs.0 as usize);
        if lhs_ == rhs_ { return Ok(()); }

        enum Next { AddDeps, AssertRhsTrue, AssertLhsFalse }
        let next = { // early error checks
            let lvalue = self.mark_infos.get(&lhs_).map(|info| &info.value);
            let rvalue = self.mark_infos.get(&rhs_).map(|info| &info.value);
            match (lvalue, rvalue) {
                (Some(&MarkValue::Invalid), _) | (_, Some(&MarkValue::Invalid)) =>
                    panic!("self-recursive mark resolution"),
                (Some(&MarkValue::True), Some(&MarkValue::True)) => return Ok(()),
                (Some(&MarkValue::True), Some(&MarkValue::False)) =>
                    return Err(format!("{:?} (known to be true) cannot imply \
                                        {:?} (known to be false)", lhs, rhs)),
                (Some(&MarkValue::True), _) => Next::AssertRhsTrue,
                (Some(&MarkValue::False), _) => return Ok(()),
                (_, Some(&MarkValue::True)) => return Ok(()),
                (_, Some(&MarkValue::False)) => Next::AssertLhsFalse,
                (_, _) => Next::AddDeps,
            }
        };

        fn get_deps_mut(mark_infos: &mut VecMap<Box<MarkInfo>>, i: usize) -> &mut MarkDeps {
            let info = mark_infos.entry(i).or_insert_with(|| Partition::create(i, 0));
            if let MarkValue::Unknown(ref mut deps) = info.value {
                if deps.is_none() { *deps = Some(Box::new(MarkDeps::new())); }
                deps.as_mut().unwrap()
            } else {
                unreachable!()
            }
        }

        fn take_deps(mark_infos: &mut VecMap<Box<MarkInfo>>, i: usize,
                     repl: MarkValue) -> Option<Box<MarkDeps>> {
            let info = mark_infos.entry(i).or_insert_with(|| Partition::create(i, 0));
            if let MarkValue::Unknown(deps) = mem::replace(&mut info.value, repl) {
                deps
            } else {
                unreachable!()
            }
        }

        match next {
            Next::AddDeps => { // unknown implies unknown
                {
                    let deps = get_deps_mut(&mut self.mark_infos, lhs_);
                    let follows = Mark(rhs_ as u32);
                    if deps.follows == None {
                        deps.follows = Some(follows);
                    } else if deps.follows != Some(follows) {
                        panic!("non-linear deps detected");
                    }
                }

                {
                    let deps = get_deps_mut(&mut self.mark_infos, rhs_);
                    let precedes = Mark(lhs_ as u32);
                    if deps.precedes == None {
                        deps.precedes = Some(precedes);
                    } else if deps.precedes != Some(precedes) {
                        panic!("non-linear deps detected");
                    }
                }
            }

            Next::AssertRhsTrue => { // true implies unknown
                if let Some(deps) = take_deps(&mut self.mark_infos, rhs_, MarkValue::True) {
                    deps.assert_true(self)?;
                }
            }

            Next::AssertLhsFalse => { // unknown implies false
                if let Some(deps) = take_deps(&mut self.mark_infos, lhs_, MarkValue::False) {
                    deps.assert_false(self)?;
                }
            }
        }

        Ok(())
    }

    fn assert_mark_require_eq(&mut self, mark: Mark, base: &Ty, ty: &Ty) -> CheckResult<()> {
        debug!("asserting {:?} requires {:?} = {:?}", mark, base, ty);
        self.assert_mark_require(mark, base, Rel::Eq, ty)
    }

    fn assert_mark_require_sup(&mut self, mark: Mark, base: &Ty, ty: &Ty) -> CheckResult<()> {
        debug!("asserting {:?} requires {:?} :> {:?}", mark, base, ty);
        self.assert_mark_require(mark, base, Rel::Sup, ty)
    }

    fn get_mark_exact(&self, mark: Mark) -> Option<bool> {
        let m = self.mark_infos.find(mark.0 as usize);
        if let Some(info) = self.mark_infos.map.get(&m) {
            match info.value {
                MarkValue::True => return Some(true),
                MarkValue::False => return Some(false),
                _ => {}
            }
        }
        None
    }

    fn fmt_class(&self, cls: Class, f: &mut fmt::Formatter) -> fmt::Result {
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

    fn is_subclass_of(&self, mut lhs: ClassId, rhs: ClassId) -> bool {
        if lhs == rhs { return true; }

        while let Some(parent) = self.classes[lhs.0 as usize].parent {
            assert!(parent < lhs);
            lhs = parent;
            if lhs == rhs { return true; }
        }

        false
    }
}

// per-file environment
pub struct Env<'ctx> {
    context: &'ctx mut Context,
    opts: Rc<RefCell<Options>>,
    map_index: usize,
    scopes: Vec<Scope>,
}

impl<'ctx> Env<'ctx> {
    pub fn new(context: &'ctx mut Context, opts: Rc<RefCell<Options>>,
               map: ScopeMap<Name>) -> Env<'ctx> {
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
    pub fn context(&mut self) -> &mut Context {
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
    pub fn get_type_bounds(&self, ty: &T) -> (/*lb*/ Flags, /*ub*/ Flags) {
        self.context.get_type_bounds(ty)
    }

    // exactly resolves the type variable inside `ty` if possible
    // this is a requirement for table indexing and function calls
    pub fn resolve_exact_type<'a>(&mut self, ty: &Ty) -> Option<Ty> {
        self.context.resolve_exact_type(ty)
    }

    pub fn return_from_module(mut self, modname: &[u8], span: Span) -> CheckResult<Slot> {
        // note that this scope is distinct from the global scope
        let top_scope = self.scopes.drain(..).next().unwrap();
        let returns = if let Some(returns) = top_scope.frame.unwrap().returns {
            returns.into_first()
        } else {
            Ty::new(T::Nil)
        };

        if let Some(ty) = self.resolve_exact_type(&returns) {
            let flags = ty.flags();

            // prepare for the worse
            if !flags.is_dynamic() && flags.contains(T_FALSE) {
                self.error(span, m::ModCannotReturnFalse {}).done()?;
                return Ok(Slot::dummy());
            }

            // simulate `require` behavior, i.e. nil translates to true
            let ty = if flags.contains(T_NIL) {
                ty.without_nil() | Ty::new(T::True)
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
                if let T::Builtin(Builtin::Constructible, ref t) = *selfarg {
                    if let T::Class(Class::Instance(cid_)) = **t {
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

    fn assign_special(&mut self, lhs: &Spanned<Slot>, rhs: &Spanned<Slot>) -> CheckResult<()> {
        match lhs.builtin() {
            Some(b @ Builtin::PackagePath) |
            Some(b @ Builtin::PackageCpath) => {
                if let Some(s) = self.resolve_exact_type(&rhs.unlift())
                                     .and_then(|t| t.as_string().map(|s| s.to_owned())) {
                    if b == Builtin::PackagePath {
                        self.opts.borrow_mut().set_package_path(&s)?;
                    } else {
                        self.opts.borrow_mut().set_package_cpath(&s)?;
                    }
                } else {
                    self.warn(rhs, m::UnknownAssignToPackagePath { name: b.name() }).done()?;
                }
            }

            Some(Builtin::Constructible) => {
                self.error(lhs, m::SelfCannotBeAssignedInCtor {}).done()?;
            }

            Some(Builtin::Constructor) => {
                self.create_new_method_from_init(rhs)?;
            }

            _ => {}
        }

        Ok(())
    }

    fn assign_(&mut self, lhs: &Spanned<Slot>, rhs: &Spanned<Slot>, init: bool) -> CheckResult<()> {
        self.assign_special(lhs, rhs)?;
        if lhs.accept(rhs, self.context, init).is_err() {
            self.error(lhs, m::CannotAssign { lhs: self.display(lhs), rhs: self.display(rhs) })
                .note_if(rhs, m::OtherTypeOrigin {})
                .done()?;
        }
        Ok(())
    }

    // same to Slot::accept but also able to handle the built-in semantics
    // should be used for any kind of non-internal assignments
    pub fn assign(&mut self, lhs: &Spanned<Slot>, rhs: &Spanned<Slot>) -> CheckResult<()> {
        self.assign_(lhs, rhs, false)
    }

    pub fn ensure_var(&mut self, nameref: &Spanned<NameRef>) -> CheckResult<()> {
        let id = self.id_from_nameref(nameref);

        let defslot = {
            let def = self.context.ids.get(&id).expect("Env::ensure_var with an undefined var");
            if def.set { return Ok(()); }
            def.slot.clone().with_loc(def.span)
        };

        // if the variable is not yet set, we may still try to assign nil
        // to allow `local x --: string|nil` (which is fine even when "uninitialized").
        let nil = Slot::just(Ty::new(T::Nil)).without_loc();
        self.assign_special(&defslot, &nil)?;
        if defslot.accept(&nil, self.context, true).is_ok() { // this IS still initialization
            self.context.ids.get_mut(&id).unwrap().set = true;
        } else {
            // won't alter the set flag, so subsequent uses are still errors
            self.error(&id, m::UseOfUnassignedVar {})
                     .note_if(&defslot, m::UnassignedVarOrigin { var: self.display(&defslot) })
                     .done()?;
        }

        Ok(())
    }

    fn name_class_if_any(&mut self, id: &Spanned<Id>, info: &Spanned<Slot>) -> CheckResult<()> {
        match *info.unlift().as_base() {
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

        let assigned = initinfo.is_some();
        let specinfo = specinfo.unwrap_or_else(|| {
            Slot::var(Ty::new(T::Nil), self.context).without_loc()
        });
        if let Some(initinfo) = initinfo {
            self.assign_(&specinfo, &initinfo, true)?;

            // name the class if it is currently unnamed
            self.name_class_if_any(&id, &initinfo)?;
        }

        self.context.ids.insert(id.base, NameDef { span: id.span, slot: specinfo.base,
                                                   set: assigned });
        Ok(())
    }

    pub fn add_local_var_already_set(&mut self, scoped_id: &Spanned<ScopedId>,
                                     info: Spanned<Slot>) -> CheckResult<()> {
        let id = Id::Local(self.map_index, scoped_id.base.clone()).with_loc(scoped_id);
        debug!("adding a local variable {} already set to {:?}", id.display(&self.context), info);

        // we cannot blindly `accept` the `initinfo`, since it will discard the flexibility
        // (e.g. if the callee requests `F::Var`, we need to keep that).
        // therefore we just remap `F::Just` to `F::VarOrCurrently`.
        info.adapt(F::Currently, self.context);

        self.name_class_if_any(&id, &info)?;

        self.context.ids.insert(id.base, NameDef { span: id.span, slot: info.base, set: true });
        Ok(())
    }

    // assigns to a global or local variable with a right-hand-side type of `info`.
    // it may create a new global variable if there is no variable with that name.
    pub fn assign_to_var(&mut self, nameref: &Spanned<NameRef>,
                         info: Spanned<Slot>) -> CheckResult<()> {
        let id = self.id_from_nameref(nameref);
        let (previnfo, prevset) = if self.context.ids.contains_key(&id.base) {
            let def = self.context.ids.get_mut(&id.base).unwrap();
            let prevset = def.set;
            def.set = true;
            (def.slot.clone(), prevset)
        } else {
            let slot = Slot::var(Ty::new(T::Nil), self.context);
            self.context.ids.insert(id.base.clone(),
                                    NameDef { span: id.span, slot: slot.clone(), set: true });
            (slot, true)
        };
        debug!("assigning {:?} to a variable {} with type {:?}",
               info, id.display(&self.context), previnfo);

        self.assign_(&previnfo.with_loc(&id), &info, !prevset)?;
        self.name_class_if_any(&id, &info)?;
        Ok(())
    }

    fn assume_special(&mut self, info: &Spanned<Slot>) -> CheckResult<()> {
        match info.builtin() {
            Some(Builtin::StringMeta) => {
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
        match self.context.ids.entry(id) {
            hash_map::Entry::Vacant(e) => {
                e.insert(NameDef { span: name.span, slot: info.base, set: true });
            }
            hash_map::Entry::Occupied(mut e) => {
                let def = e.get_mut();
                def.slot = info.base;
                def.set = true;
            }
        }

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

impl<'ctx> Report for Env<'ctx> {
    fn add_span(&self, k: Kind, s: Span, m: &Localize) -> kailua_diag::Result<()> {
        self.context.report.add_span(k, s, m)
    }
}

impl<'ctx> TypeResolver for Env<'ctx> {
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
fn test_context_tvar() {
    use kailua_diag::NoReport;

    let mut ctx = Context::new(Rc::new(NoReport));

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

