use std::fmt;
use std::mem;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use vec_map::VecMap;

use kailua_syntax::Name;
use diag::CheckResult;
use ty::{Ty, T, Union, TVar, Seq, Lattice, TVarContext};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Builtin {
    Require,        // (fixed string) -> table & sideeffect
}

#[derive(Clone, PartialEq)]
pub struct TyInfo {
    pub ty: T<'static>,
    pub builtin: Option<Builtin>,
}

impl TyInfo {
    pub fn from<'a>(ty: T<'a>) -> TyInfo {
        TyInfo { ty: ty.into_send(), builtin: None }
    }
}

impl fmt::Debug for TyInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{:?}", self.ty));
        if let Some(ref builtin) = self.builtin {
            try!(write!(f, " (= {:?})", *builtin));
        }
        Ok(())
    }
}

pub struct Frame {
    pub vararg: Option<TyInfo>,
    pub returns: Ty,
}

pub struct Scope {
    names: HashMap<Name, TyInfo>,
    frame: Option<Frame>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope { names: HashMap::new(), frame: None }
    }

    pub fn new_function(frame: Frame) -> Scope {
        Scope { names: HashMap::new(), frame: Some(frame) }
    }

    pub fn get<'a>(&'a self, name: &Name) -> Option<&'a TyInfo> {
        self.names.get(name)
    }

    pub fn get_mut<'a>(&'a mut self, name: &Name) -> Option<&'a mut TyInfo> {
        self.names.get_mut(name)
    }

    pub fn get_frame<'a>(&'a self) -> Option<&'a Frame> {
        self.frame.as_ref()
    }

    pub fn get_frame_mut<'a>(&'a mut self) -> Option<&'a mut Frame> {
        self.frame.as_mut()
    }

    pub fn put(&mut self, name: Name, info: TyInfo) {
        self.names.insert(name, info);
    }
}

struct Bound {
    parent: Cell<u32>,
    rank: u8,
    bound: T<'static>, // should be T::None when idx != parent
}

impl Bound {
    fn new(parent: u32) -> Bound {
        Bound { parent: Cell::new(parent), rank: 0, bound: T::None }
    }
}

// a set of constraints that can be organized as a tree
struct Constraints {
    op: &'static str,
    bounds: VecMap<Box<Bound>>,
}

// is this bound trivial so that one can always overwrite?
fn is_bound_trivial(t: &T) -> bool {
    // TODO special casing ? is not enough, should resolve b.bound's inner ?s as well
    match *t { T::None | T::Dynamic => true, _ => false }
}

impl Constraints {
    fn new(op: &'static str) -> Constraints {
        Constraints { op: op, bounds: VecMap::new() }
    }

    fn find(&self, tvar: TVar) -> TVar {
        if let Some(b) = self.bounds.get(&(tvar.0 as usize)) {
            let mut parent = b.parent.get();
            if parent != tvar.0 { // path compression
                parent = self.find(TVar(parent)).0;
                b.parent.set(parent);
            }
            TVar(parent)
        } else {
            tvar
        }
    }

    fn union(&mut self, lhs: TVar, rhs: TVar) -> TVar {
        use std::cmp::Ordering;

        let lhs = self.find(lhs).0;
        let rhs = self.find(rhs).0;
        if lhs == rhs { return TVar(rhs); }

        let lrank = self.bounds.get(&(lhs as usize)).map(|b| b.rank);
        let rrank = self.bounds.get(&(rhs as usize)).map(|b| b.rank);
        if lrank.is_none() && rrank.is_none() {
            // special casing, in order to reduce memory allocation
            self.bounds.insert(rhs as usize,
                               Box::new(Bound { rank: 1, ..Bound::new(lhs) }));
            return TVar(lhs);
        }

        if lrank.is_none() {
            self.bounds.insert(lhs as usize, Box::new(Bound::new(lhs)));
        }
        if rrank.is_none() {
            self.bounds.insert(rhs as usize, Box::new(Bound::new(rhs)));
        }

        match lrank.unwrap_or(0).cmp(&rrank.unwrap_or(0)) {
            Ordering::Less => {
                let lb = self.bounds.get_mut(&(lhs as usize)).unwrap();
                lb.parent.set(rhs);
                TVar(rhs)
            }
            Ordering::Greater => {
                let rb = self.bounds.get_mut(&(rhs as usize)).unwrap();
                rb.parent.set(lhs);
                TVar(lhs)
            }
            Ordering::Equal => {
                let rb = self.bounds.get_mut(&(rhs as usize)).unwrap();
                rb.parent.set(lhs);
                rb.rank += 1;
                TVar(lhs)
            }
        }
    }

    fn is(&self, lhs: TVar, rhs: TVar) -> bool {
        lhs == rhs || self.find(lhs) == self.find(rhs)
    }

    fn get_bound<'a>(&'a self, lhs: TVar) -> Option<&'a Bound> {
        let lhs = self.find(lhs);
        self.bounds.get(&(lhs.0 as usize)).map(|b| &**b)
    }

    fn add_bound(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()> {
        let lhs_ = self.find(lhs);
        let b = self.bounds.entry(lhs_.0 as usize).or_insert_with(|| Box::new(Bound::new(lhs_.0)));
        if is_bound_trivial(&b.bound) {
            b.bound = rhs.clone().into_send();
        } else if b.bound != *rhs {
            // TODO check if this restriction has a real world implication
            return Err(format!("variable {:?} cannot have multiple bounds \
                                (original {} {:?}, later {} {:?})",
                               lhs, self.op, b.bound, self.op, *rhs));
        }
        Ok(())
    }

    fn add_relation(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()> {
        if lhs == rhs { return Ok(()); }

        let lhs_ = self.find(lhs);
        let rhs_ = self.find(rhs);
        if lhs_ == rhs_ { return Ok(()); }

        let take_bound = |bounds: &mut VecMap<Box<Bound>>, tvar: TVar| {
            if let Some(b) = bounds.get_mut(&(tvar.0 as usize)) {
                mem::replace(&mut b.bound, T::None)
            } else {
                T::None
            }
        };

        // take the bounds from each representative variable.
        let lhsbound = take_bound(&mut self.bounds, lhs_);
        let rhsbound = take_bound(&mut self.bounds, rhs_);

        let bound = match (is_bound_trivial(&lhsbound), is_bound_trivial(&rhsbound)) {
            (false, _) => rhsbound,
            (true, false) => lhsbound,
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
        let new = self.union(lhs_, rhs_);
        if !is_bound_trivial(&bound) {
            // the merged entry should have non-zero rank, so unwrap() is fine
            self.bounds.get_mut(&(new.0 as usize)).unwrap().bound = bound;
        }

        Ok(())
    }
}

pub struct Context {
    global_scope: Scope,
    next_tvar: Cell<TVar>,
    tvar_sub: Constraints, // upper bound
    tvar_sup: Constraints, // lower bound
    tvar_eq: Constraints, // tight bound
}

impl Context {
    pub fn new() -> Context {
        Context {
            global_scope: Scope::new(),
            next_tvar: Cell::new(TVar(0)),
            tvar_sub: Constraints::new("<:"),
            tvar_sup: Constraints::new(":>"),
            tvar_eq: Constraints::new("="),
        }
    }

    pub fn global_scope(&self) -> &Scope {
        &self.global_scope
    }

    pub fn global_scope_mut(&mut self) -> &mut Scope {
        &mut self.global_scope
    }
}

impl TVarContext for Context {
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
        println!("adding a constraint {:?} <: {:?}", lhs, *rhs);
        try!(self.tvar_sub.add_bound(lhs, rhs));
        if let Some(eb) = self.tvar_eq.get_bound(lhs).map(|b| b.bound.clone()) {
            try!(eb.assert_eq(rhs, self));
        }
        Ok(())
    }

    fn assert_tvar_sup(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()> {
        println!("adding a constraint {:?} :> {:?}", lhs, *rhs);
        try!(self.tvar_sup.add_bound(lhs, rhs));
        if let Some(eb) = self.tvar_eq.get_bound(lhs).map(|b| b.bound.clone()) {
            try!(rhs.assert_eq(&eb, self));
        }
        Ok(())
    }

    fn assert_tvar_eq(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()> {
        println!("adding a constraint {:?} = {:?}", lhs, *rhs);
        try!(self.tvar_eq.add_bound(lhs, rhs));
        if let Some(ub) = self.tvar_sub.get_bound(lhs).map(|b| b.bound.clone()) {
            try!(rhs.assert_sub(&ub, self));
        }
        if let Some(lb) = self.tvar_sup.get_bound(lhs).map(|b| b.bound.clone()) {
            try!(lb.assert_sub(rhs, self));
        }
        Ok(())
    }

    fn assert_tvar_sub_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()> {
        println!("adding a constraint {:?} <: {:?}", lhs, rhs);
        if !self.tvar_eq.is(lhs, rhs) {
            try!(self.tvar_sub.add_relation(lhs, rhs));
            try!(self.tvar_sup.add_relation(rhs, lhs));
        }
        Ok(())
    }

    fn assert_tvar_eq_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()> {
        println!("adding a constraint {:?} = {:?}", lhs, rhs);
        // do not update tvar_sub & tvar_sup, 
        self.tvar_eq.add_relation(lhs, rhs)
    }
}

pub struct Env<'ctx> {
    context: &'ctx mut Context,
    scopes: Vec<Scope>,
}

impl<'ctx> Env<'ctx> {
    pub fn new(context: &'ctx mut Context) -> Env<'ctx> {
        // we have local variables even at the global position, so we need at least one Scope
        Env { context: context, scopes: vec![Scope::new()] }
    }

    // not to be called internally; it intentionally reduces the lifetime
    pub fn context(&mut self) -> &mut Context {
        self.context
    }

    pub fn enter(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }

    pub fn leave(&mut self) {
        assert!(self.scopes.len() > 1);
        self.scopes.pop();
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

    pub fn get_var<'a>(&'a self, name: &Name) -> Option<&'a TyInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) { return Some(info); }
        }
        self.context.global_scope().get(name)
    }

    pub fn get_var_mut<'a>(&'a mut self, name: &Name) -> Option<&'a mut TyInfo> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(info) = scope.get_mut(name) { return Some(info); }
        }
        self.context.global_scope_mut().get_mut(name)
    }

    pub fn get_local_var<'a>(&'a self, name: &Name) -> Option<&'a TyInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) { return Some(info); }
        }
        None
    }

    pub fn get_local_var_mut<'a>(&'a mut self, name: &Name) -> Option<&'a mut TyInfo> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(info) = scope.get_mut(name) { return Some(info); }
        }
        None
    }

    pub fn get_frame<'a>(&'a self) -> Option<&'a Frame> {
        for scope in self.scopes.iter().rev() {
            if let Some(frame) = scope.get_frame() { return Some(frame); }
        }
        self.context.global_scope().get_frame()
    }

    pub fn get_frame_mut<'a>(&'a mut self) -> Option<&'a mut Frame> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(frame) = scope.get_frame_mut() { return Some(frame); }
        }
        self.context.global_scope_mut().get_frame_mut()
    }

    pub fn get_vararg<'a>(&'a self) -> Option<&'a TyInfo> {
        self.get_frame().and_then(|f| f.vararg.as_ref())
    }

    pub fn get_vararg_mut<'a>(&'a mut self) -> Option<&'a mut TyInfo> {
        self.get_frame_mut().and_then(|f| f.vararg.as_mut())
    }

    pub fn add_local_var(&mut self, name: &Name, info: TyInfo) {
        println!("adding a local variable {:?} as {:?}", *name, info);
        self.current_scope_mut().put(name.to_owned(), info);
    }

    pub fn assign_to_var(&mut self, name: &Name, info: TyInfo) -> CheckResult<()> {
        if let Some(previnfo) = self.get_local_var_mut(name) {
            if !previnfo.ty.accept(&info.ty) {
                return Err(format!("cannot assign {:?} to the variable {:?} with type {:?}",
                                   info.ty, name, previnfo.ty));
            } else {
                println!("assigning {:?} to a local variable {:?} with type {:?}",
                         info, *name, *previnfo);
                return Ok(());
            }
        }

        println!("adding a global variable {:?} as {:?}", *name, info);
        self.context.global_scope_mut().put(name.to_owned(), info);
        Ok(())
    }

    pub fn assume_var(&mut self, name: &Name, info: TyInfo) -> CheckResult<()> {
        if let Some(previnfo) = self.get_local_var_mut(name) {
            println!("(force) adding a local variable {:?} as {:?}", *name, info);
            *previnfo = info;
            return Ok(());
        }

        println!("(force) adding a global variable {:?} as {:?}", *name, info);
        self.context.global_scope_mut().put(name.to_owned(), info);
        Ok(())
    }
}

