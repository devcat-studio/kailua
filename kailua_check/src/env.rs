use std::mem;
use std::ops;
use std::str;
use std::fmt;
use std::cell::{Cell, RefCell};
use std::rc::Rc;
use std::collections::{HashMap, HashSet};
use vec_map::VecMap;

use kailua_diag::{self, SourceFile, Kind, Span, Spanned, Report, Reporter, WithLoc, Localize};
use kailua_syntax::{Name, parse_chunk};
use diag::{CheckResult, unquotable_name};
use ty::{Ty, TySeq, T, Slot, F, TVar, Mark, Lattice, Builtin, Displayed, Display};
use ty::{TypeContext, TypeResolver, ClassId, Class, Functions, Function, Key};
use ty::flags::*;
use defs::get_defs;
use options::Options;
use check::Checker;
use message as m;

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
    names: HashMap<Name, NameDef>,
    frame: Option<Frame>,
    types: HashMap<Name, TypeDef>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope { names: HashMap::new(), frame: None, types: HashMap::new() }
    }

    pub fn new_function(frame: Frame) -> Scope {
        Scope { names: HashMap::new(), frame: Some(frame), types: HashMap::new() }
    }

    pub fn get<'a>(&'a self, name: &Name) -> Option<&'a NameDef> {
        self.names.get(name)
    }

    pub fn get_mut<'a>(&'a mut self, name: &Name) -> Option<&'a mut NameDef> {
        self.names.get_mut(name)
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

    pub fn put(&mut self, name: Spanned<Name>, info: Slot, set: bool) {
        self.names.insert(name.base, NameDef { span: name.span, slot: info, set: set });
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
        match **t { T::None | T::Dynamic => true, _ => false }
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
    fn add_bound<'a>(&'a mut self, lhs: TVar, rhs: &T) -> Option<&'a T> {
        let lhs_ = self.bounds.find(lhs.0 as usize);
        let b = self.bounds.entry(lhs_).or_insert_with(|| Partition::create(lhs_, 0));
        if is_bound_trivial(&b.bound) {
            b.bound = Some(Box::new(rhs.clone().into_send()));
        } else {
            let lhsbound = &**b.bound.as_ref().unwrap();
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
    constraints: Option<(T<'static>, Vec<(Rel, T<'static>)>)>,
}

impl MarkDeps {
    fn new() -> MarkDeps {
        MarkDeps { follows: None, precedes: None, constraints: None }
    }

    fn assert_true(self, ctx: &mut TypeContext) -> CheckResult<()> {
        if let Some((ref base, ref others)) = self.constraints {
            for &(rel, ref other) in others {
                match rel {
                    Rel::Eq => try!(base.assert_eq(other, ctx)),
                    Rel::Sup => try!(other.assert_sub(base, ctx)),
                }
            }
        }
        if let Some(follows) = self.follows {
            try!(ctx.assert_mark_true(follows));
        }
        Ok(())
    }

    fn assert_false(self, ctx: &mut TypeContext) -> CheckResult<()> {
        if let Some(precedes) = self.precedes {
            try!(ctx.assert_mark_false(precedes));
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
                try!(lb.assert_eq(&rb, ctx));
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
    global_scope: Scope,
    next_tvar: Cell<TVar>,
    tvar_sub: Constraints, // upper bound
    tvar_sup: Constraints, // lower bound
    tvar_eq: Constraints, // tight bound
    next_mark: Cell<Mark>,
    mark_infos: Partitions<Box<MarkInfo>>,
    opened: HashSet<String>,
    loaded: HashMap<Vec<u8>, LoadStatus>, // corresponds to `package.loaded`
    string_meta: Option<Spanned<Slot>>,
    classes: Vec<ClassDef>,
}

impl Context {
    pub fn new(report: Rc<Report>) -> Context {
        let mut ctx = Context {
            report: report,
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

    pub fn global_scope(&self) -> &Scope {
        &self.global_scope
    }

    pub fn global_scope_mut(&mut self) -> &mut Scope {
        &mut self.global_scope
    }

    pub fn open_library(&mut self, name: &Spanned<Name>,
                        opts: Rc<RefCell<Options>>) -> CheckResult<()> {
        let name_ = try!(str::from_utf8(&name.base).map_err(|e| e.to_string()));
        if let Some(defs) = get_defs(name_) {
            // one library may consist of multiple files, so we defer duplicate check
            for def in defs {
                if self.opened.insert(def.name.to_owned()) {
                    let name = format!("<internal: {}>", def.name);
                    let chunk = {
                        let opts = opts.borrow();
                        let file = SourceFile::from_u8(name, def.code.to_owned());
                        let span = opts.source().borrow_mut().add(file);
                        let chunk = parse_chunk(&opts.source().borrow(), span, &*self.report);
                        try!(chunk.map_err(|_| format!("parse error")))
                    };
                    let mut env = Env::new(self, opts.clone());
                    let mut checker = Checker::new(&mut env);
                    try!(checker.visit(&chunk))
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
                try!(self.error(span, m::RecursiveRequire {})
                         .note(oldspan, m::PreviousRequire {})
                         .done());
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

    pub fn name_class(&mut self, cid: ClassId, name: &Spanned<Name>) -> CheckResult<()> {
        let cls = &mut self.classes[cid.0 as usize];
        if let Some(ref prevname) = cls.name {
            try!(self.report.warn(name, m::RedefinedClassName {})
                            .note(prevname, m::PreviousClassName {})
                            .done());
        } else {
            cls.name = Some(name.clone());
        }
        Ok(())
    }

    // used by assert_mark_require_{eq,sup}
    fn assert_mark_require(&mut self, mark: Mark, base: &T, rel: Rel, ty: &T) -> CheckResult<()> {
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
                        try!(base.assert_eq(&mut constraints.0, self));
                        constraints.1.push((rel, ty.clone().into_send()));
                    } else {
                        deps.constraints = Some((base.clone().into_send(),
                                                 vec![(rel, ty.clone().into_send())]));
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
    fn can_continue(&self) -> bool { self.report.can_continue() }
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

    fn assert_tvar_sub(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()> {
        debug!("adding a constraint {:?} <: {:?}", lhs, *rhs);
        if let Some(eb) = self.tvar_eq.get_bound(lhs).and_then(|b| b.bound.clone()) {
            try!((*eb).assert_sub(rhs, self));
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
                try!((*lb).assert_sub(rhs, self));
            }
        }
        Ok(())
    }

    fn assert_tvar_sup(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()> {
        debug!("adding a constraint {:?} :> {:?}", lhs, *rhs);
        if let Some(eb) = self.tvar_eq.get_bound(lhs).and_then(|b| b.bound.clone()) {
            try!(rhs.assert_sub(&*eb, self));
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
                try!(rhs.assert_sub(&*ub, self));
            }
        }
        Ok(())
    }

    fn assert_tvar_eq(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()> {
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
                try!(rhs.assert_sub(&*ub, self));
            }
            if let Some(lb) = self.tvar_sup.get_bound(lhs).and_then(|b| b.bound.clone()) {
                try!((*lb).assert_sub(rhs, self));
            }
        }
        Ok(())
    }

    fn assert_tvar_sub_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()> {
        debug!("adding a constraint {:?} <: {:?}", lhs, rhs);
        if !self.tvar_eq.is(lhs, rhs) {
            try!(self.tvar_sub.add_relation(lhs, rhs));
            try!(self.tvar_sup.add_relation(rhs, lhs));
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

    fn get_tvar_exact_type(&self, tvar: TVar) -> Option<T<'static>> {
        self.tvar_eq.get_bound(tvar).and_then(|b| b.bound.as_ref()).map(|t| t.clone().into_send())
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
                if let Some(deps) = deps { try!(deps.assert_true(self)); }
                MarkValue::True
            }

            (MarkValue::False, MarkValue::Unknown(deps)) |
            (MarkValue::Unknown(deps), MarkValue::False) => {
                if let Some(deps) = deps { try!(deps.assert_false(self)); }
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

                let deps = try!(ldeps.merge(*rdeps, self));

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
                    try!(deps.assert_true(self));
                }
            }

            Next::AssertLhsFalse => { // unknown implies false
                if let Some(deps) = take_deps(&mut self.mark_infos, lhs_, MarkValue::False) {
                    try!(deps.assert_false(self));
                }
            }
        }

        Ok(())
    }

    fn assert_mark_require_eq(&mut self, mark: Mark, base: &T, ty: &T) -> CheckResult<()> {
        debug!("asserting {:?} requires {:?} = {:?}", mark, *base, *ty);
        self.assert_mark_require(mark, base, Rel::Eq, ty)
    }

    fn assert_mark_require_sup(&mut self, mark: Mark, base: &T, ty: &T) -> CheckResult<()> {
        debug!("asserting {:?} requires {:?} :> {:?}", mark, *base, *ty);
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
    scopes: Vec<Scope>,
}

impl<'ctx> Env<'ctx> {
    pub fn new(context: &'ctx mut Context, opts: Rc<RefCell<Options>>) -> Env<'ctx> {
        let global_frame = Frame { vararg: None, returns: None, returns_exact: false };
        Env {
            context: context,
            opts: opts,
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
        let flags = ty.flags();
        let (lb, ub) = ty.get_tvar().map_or((T_NONE, T_NONE), |v| self.get_tvar_bounds(v));
        (flags | lb, flags | ub)
    }

    // exactly resolves the type variable inside `ty` if possible
    // this is a requirement for table indexing and function calls
    pub fn resolve_exact_type<'a>(&mut self, ty: &T<'a>) -> Option<T<'a>> {
        match ty.split_tvar() {
            (None, None) => unreachable!(),
            (None, Some(t)) => Some(t),
            (Some(tv), None) => self.get_tvar_exact_type(tv),
            (Some(tv), Some(t)) => {
                if let Some(t_) = self.get_tvar_exact_type(tv) {
                    Some(t.union(&t_, self.context))
                } else {
                    None
                }
            }
        }
    }

    pub fn return_from_module(mut self, modname: &[u8], span: Span) -> CheckResult<Slot> {
        // note that this scope is distinct from the global scope
        let top_scope = self.scopes.drain(..).next().unwrap();
        let returns = if let Some(returns) = top_scope.frame.unwrap().returns {
            *returns.into_first()
        } else {
            T::Nil
        };

        if let Some(ty) = self.resolve_exact_type(&returns) {
            let flags = ty.flags();

            // prepare for the worse
            if !flags.is_dynamic() && flags.contains(T_FALSE) {
                try!(self.error(span, m::ModCannotReturnFalse {}).done());
                return Ok(Slot::dummy());
            }

            // simulate `require` behavior, i.e. nil translates to true
            let ty = if flags.contains(T_NIL) {
                ty.without_nil() | T::True
            } else {
                ty
            };

            // this has to be Var since the module is shared across the entire program
            let slot = Slot::new(F::Var, ty.into_send());
            self.context.loaded.insert(modname.to_owned(), LoadStatus::Done(slot.clone()));
            Ok(slot)
        } else {
            // TODO ideally we would want to resolve type variables in this type
            try!(self.error(span,
                            m::ModCannotReturnInexactType { returns: self.display(&returns) })
                     .done());
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

    pub fn get_var<'a>(&'a self, name: &Name) -> Option<&'a NameDef> {
        for scope in self.scopes.iter().rev() {
            if let Some(def) = scope.get(name) { return Some(def); }
        }
        self.context.global_scope().get(name)
    }

    pub fn get_var_mut<'a>(&'a mut self, name: &Name) -> Option<&'a mut NameDef> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(def) = scope.get_mut(name) { return Some(def); }
        }
        self.context.global_scope_mut().get_mut(name)
    }

    pub fn get_local_var<'a>(&'a self, name: &Name) -> Option<&'a NameDef> {
        for scope in self.scopes.iter().rev() {
            if let Some(def) = scope.get(name) { return Some(def); }
        }
        None
    }

    pub fn get_local_var_mut<'a>(&'a mut self, name: &Name) -> Option<&'a mut NameDef> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(def) = scope.get_mut(name) { return Some(def); }
        }
        None
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
        self.context.string_meta.clone()
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
            try!(self.error(init, m::InexactInitMethod { init: self.display(init) }).done());
            return Ok(());
        };

        // ...and is a function...
        let mut func = match ty {
            T::Functions(ref func) => match **func {
                Functions::Simple(ref f) => f.to_owned(),
                _ => {
                    try!(self.error(init, m::OverloadedFuncInitMethod { init: self.display(init) })
                             .done());
                    return Ok(());
                }
            },
            _ => {
                try!(self.error(init, m::NonFuncInitMethod { init: self.display(init) }).done());
                return Ok(());
            },
        };

        // ...and has the first argument readily known as a [constructible] class instance type.
        let mut cid = None;
        if !func.args.head.is_empty() {
            let selfarg = func.args.head.remove(0);
            if let Some(selfarg) = self.resolve_exact_type(&selfarg) {
                if let T::Builtin(Builtin::Constructible, ref t) = selfarg {
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
            let ctor = Slot::new(F::Const, T::func(ctor));

            debug!("implicitly setting the `new` method of {:?} as {:?}", cid, ctor);
            let cdef = self.context.get_class_mut(cid).expect("invalid ClassId");
            let key = Key::Str(b"new"[..].into());
            let prevctor = cdef.class_ty.insert(key, ctor);
            assert_eq!(prevctor, None); // should have been prevented
        } else {
            try!(self.error(init, m::BadSelfInInitMethod { init: self.display(init) }).done());
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
                        try!(self.opts.borrow_mut().set_package_path(&s));
                    } else {
                        try!(self.opts.borrow_mut().set_package_cpath(&s));
                    }
                } else {
                    try!(self.warn(rhs, m::UnknownAssignToPackagePath { name: b.name() }).done());
                }
            }

            Some(Builtin::Constructible) => {
                try!(self.error(lhs, m::SelfCannotBeAssignedInCtor {}).done());
            }

            Some(Builtin::Constructor) => {
                try!(self.create_new_method_from_init(rhs));
            }

            _ => {}
        }

        Ok(())
    }

    fn assign_(&mut self, lhs: &Spanned<Slot>, rhs: &Spanned<Slot>, init: bool) -> CheckResult<()> {
        try!(self.assign_special(lhs, rhs));
        if lhs.accept(rhs, self.context, init).is_err() {
            try!(self.error(lhs, m::CannotAssign { lhs: self.display(lhs),
                                                   rhs: self.display(rhs) })
                     .note_if(rhs, m::OtherTypeOrigin {})
                     .done());
        }
        Ok(())
    }

    // same to Slot::accept but also able to handle the built-in semantics
    // should be used for any kind of non-internal assignments
    pub fn assign(&mut self, lhs: &Spanned<Slot>, rhs: &Spanned<Slot>) -> CheckResult<()> {
        self.assign_(lhs, rhs, false)
    }

    pub fn ensure_var(&mut self, name: &Spanned<Name>) -> CheckResult<()> {
        let defslot = {
            let def = self.get_var(name).expect("Env::ensure_var with an undefined var");
            if def.set { return Ok(()); }
            def.slot.clone().with_loc(def.span)
        };

        // if the variable is not yet set, we may still try to assign nil
        // to allow `local x --: string|nil` (which is fine even when "uninitialized").
        let nil = Slot::just(T::Nil).without_loc();
        try!(self.assign_special(&defslot, &nil));
        if defslot.accept(&nil, self.context, true).is_ok() { // this IS still initialization
            self.get_var_mut(name).unwrap().set = true;
        } else {
            // won't alter the set flag, so subsequent uses are still errors
            try!(self.error(name, m::UseOfUnassignedVar {})
                     .note_if(&defslot, m::UnassignedVarOrigin { var: self.display(&defslot) })
                     .done());
        }

        Ok(())
    }

    fn name_class_if_any(&mut self, name: &Spanned<Name>, info: &Spanned<Slot>,
                         global: bool) -> CheckResult<()> {
        match *info.unlift().as_base() {
            T::Class(Class::Prototype(cid)) => {
                // check if the name conflicts in the type namespace earlier
                // note that even when the type is defined in the global scope
                // we check both the local and global scope for the type name
                if let Some(def) = self.get_named_type(name) {
                    try!(self.error(name, m::CannotRedefineType { name: &name.base })
                             .note(def.span, m::AlreadyDefinedType {})
                             .done());
                    return Ok(());
                }

                try!(self.context.name_class(cid, name));

                let scope = if global { self.global_scope_mut() } else { self.current_scope_mut() };
                let ret = scope.put_type(name.clone(), Box::new(T::Class(Class::Instance(cid))));
                assert!(ret, "failed to insert the type");
            }

            T::Union(ref u) => {
                // should raise an error if a prototype is used within a union
                let is_prototype = |&cls| if let Class::Prototype(_) = cls { true } else { false };
                if u.classes.iter().any(is_prototype) {
                    try!(self.error(info, m::CannotNameUnknownClass { cls: self.display(info) })
                             .done());
                }
            }

            _ => {}
        }

        Ok(())
    }

    // adds a local variable with the explicit type `specinfo` and the implicit type `initinfo`.
    pub fn add_local_var(&mut self, name: &Spanned<Name>,
                         specinfo: Option<Spanned<Slot>>,
                         initinfo: Option<Spanned<Slot>>) -> CheckResult<()> {
        debug!("adding a local variable {:?} with {:?} (specified) and {:?} (initialized)",
               *name, specinfo, initinfo);

        let assigned = initinfo.is_some();
        let specinfo = specinfo.unwrap_or_else(|| Slot::var(T::Nil, self.context).without_loc());
        if let Some(initinfo) = initinfo {
            try!(self.assign_(&specinfo, &initinfo, true));

            // name the class if it is currently unnamed
            try!(self.name_class_if_any(name, &initinfo, false));
        }

        self.current_scope_mut().put(name.to_owned(), specinfo.base, assigned);
        Ok(())
    }

    pub fn add_local_var_already_set(&mut self, name: &Spanned<Name>,
                                     info: Spanned<Slot>) -> CheckResult<()> {
        debug!("adding a local variable {:?} already set to {:?}", *name, info);

        // we cannot blindly `accept` the `initinfo`, since it will discard the flexibility
        // (e.g. if the callee requests `F::Var`, we need to keep that).
        // therefore we just remap `F::Just` to `F::VarOrCurrently`.
        info.adapt(F::Currently, self.context);

        try!(self.name_class_if_any(name, &info, false));

        self.current_scope_mut().put(name.to_owned(), info.base, true);
        Ok(())
    }

    // adds a global variable with the explicit type `specinfo` and the implicit type `initinfo`.
    pub fn add_global_var(&mut self, name: &Spanned<Name>,
                          specinfo: Option<Spanned<Slot>>,
                          initinfo: Spanned<Slot>) -> CheckResult<()> {
        debug!("adding a global variable {:?} with {:?} (specified) and {:?} (initialized)",
               *name, specinfo, initinfo);

        // it might collide with other local or global variables
        if self.get_var(name).is_some() {
            try!(self.error(name, m::CannotRedefineGlobalVar { name: &name.base }).done());
            return Ok(());
        }

        let specinfo =
            specinfo.unwrap_or_else(|| Slot::var(T::Nil, self.context).without_loc());
        try!(self.assign_(&specinfo, &initinfo, true));
        try!(self.name_class_if_any(name, &initinfo, true));

        self.global_scope_mut().put(name.to_owned(), specinfo.base, true);
        Ok(())
    }

    // assigns to a global or local variable with a right-hand-side type of `info`.
    // it may create a new global variable if there is no variable with that name.
    pub fn assign_to_var(&mut self, name: &Spanned<Name>, info: Spanned<Slot>) -> CheckResult<()> {
        let def = if let Some(def) = self.get_local_var(name) {
            Some((def.set, def.slot.clone(), false))
        } else if let Some(def) = self.context.global_scope().get(name) {
            Some((def.set, def.slot.clone(), true))
        } else {
            None
        };
        if let Some((prevset, previnfo, global)) = def {
            debug!("assigning {:?} to a variable {:?} with type {:?}", info, *name, previnfo);
            try!(self.assign_(&previnfo.with_loc(name), &info, !prevset));
            try!(self.name_class_if_any(name, &info, global));
            self.get_var_mut(name).unwrap().set = true;
            return Ok(());
        }

        debug!("adding a global variable {:?} as {:?} (initialized)", *name, info);
        let newinfo = Slot::var(T::Nil, self.context).with_loc(name);
        try!(self.assign_(&newinfo, &info, true));
        try!(self.name_class_if_any(name, &info, true));

        self.global_scope_mut().put(name.to_owned(), newinfo.base, true);
        Ok(())
    }

    fn assume_special(&mut self, info: &Spanned<Slot>) -> CheckResult<()> {
        match info.builtin() {
            Some(Builtin::StringMeta) => {
                if let Some(ref prevmeta) = self.context.string_meta {
                    // while it is possible to alter the string metatable from C,
                    // we don't think that it is useful after the initialization.
                    try!(self.error(info, m::CannotRedefineStringMeta {})
                             .note(prevmeta, m::PreviousStringMeta {})
                             .done());
                }
                self.context.string_meta = Some(info.clone());
            }
            _ => {}
        }

        Ok(())
    }

    pub fn assume_var(&mut self, name: &Spanned<Name>, info: Spanned<Slot>) -> CheckResult<()> {
        try!(self.assume_special(&info));
        if let Some(def) = self.get_local_var_mut(name) {
            debug!("(force) adding a local variable {:?} as {:?}", *name, info);
            def.set = true;
            def.slot = info.base;
            return Ok(());
        }

        // assuming is inherently an implicit assignment
        self.context.global_scope_mut().put(name.to_owned(), info.base, true);
        Ok(())
    }

    pub fn assume_global_var(&mut self, name: &Spanned<Name>,
                             info: Spanned<Slot>) -> CheckResult<()> {
        debug!("(force) adding a global variable {:?} as {:?}", *name, info);
        try!(self.assume_special(&info));

        // assuming is inherently an implicit assignment
        self.context.global_scope_mut().put(name.to_owned(), info.base, true);
        Ok(())
    }

    pub fn get_tvar_bounds(&self, tvar: TVar) -> (Flags /*lb*/, Flags /*ub*/) {
        self.context.get_tvar_bounds(tvar)
    }

    pub fn get_tvar_exact_type(&self, tvar: TVar) -> Option<T<'static>> {
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
            try!(self.error(name, m::CannotRedefineType { name: &name.base })
                     .note(def.span, m::AlreadyDefinedType {})
                     .done());
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
    fn can_continue(&self) -> bool {
        self.context.report.can_continue()
    }
}

impl<'ctx> TypeResolver for Env<'ctx> {
    fn ty_from_name(&self, name: &Spanned<Name>) -> CheckResult<T<'static>> {
        if let Some(def) = self.get_named_type(name) {
            Ok(def.ty.clone().into_send())
        } else {
            try!(self.error(name, m::NoType { name: &name.base }).done());
            Ok(T::dummy())
        }
    }
}

#[test]
fn test_context_tvar() {
    use kailua_diag::NoReport;

    let mut ctx = Context::new(Rc::new(NoReport));

    { // idempotency of bounds
        let v1 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub(v1, &T::integer()), Ok(()));
        assert_eq!(ctx.assert_tvar_sub(v1, &T::integer()), Ok(()));
        assert!(ctx.assert_tvar_sub(v1, &T::string()).is_err());
    }

    { // empty bounds (lb & ub = bottom)
        let v1 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub(v1, &T::integer()), Ok(()));
        assert!(ctx.assert_tvar_sup(v1, &T::string()).is_err());

        let v2 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sup(v2, &T::integer()), Ok(()));
        assert!(ctx.assert_tvar_sub(v2, &T::string()).is_err());
    }

    { // empty bounds (lb & ub != bottom)
        let v1 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub(v1, &T::ints(vec![3, 4, 5])), Ok(()));
        assert!(ctx.assert_tvar_sup(v1, &T::ints(vec![1, 2, 3])).is_err());

        let v2 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sup(v2, &T::ints(vec![3, 4, 5])), Ok(()));
        assert!(ctx.assert_tvar_sub(v2, &T::ints(vec![1, 2, 3])).is_err());
    }

    { // implicitly disjoint bounds
        let v1 = ctx.gen_tvar();
        let v2 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub_tvar(v1, v2), Ok(()));
        assert_eq!(ctx.assert_tvar_sub(v2, &T::string()), Ok(()));
        assert!(ctx.assert_tvar_sub(v1, &T::integer()).is_err());

        let v3 = ctx.gen_tvar();
        let v4 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub_tvar(v3, v4), Ok(()));
        assert_eq!(ctx.assert_tvar_sup(v3, &T::string()), Ok(()));
        assert!(ctx.assert_tvar_sup(v4, &T::integer()).is_err());
    }

    { // equality propagation
        let v1 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_eq(v1, &T::integer()), Ok(()));
        assert_eq!(ctx.assert_tvar_sub(v1, &T::number()), Ok(()));
        assert!(ctx.assert_tvar_sup(v1, &T::string()).is_err());

        let v2 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub(v2, &T::number()), Ok(()));
        assert_eq!(ctx.assert_tvar_eq(v2, &T::integer()), Ok(()));
        assert!(ctx.assert_tvar_sup(v2, &T::string()).is_err());

        let v3 = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub(v3, &T::number()), Ok(()));
        assert!(ctx.assert_tvar_eq(v3, &T::string()).is_err());
    }
}

