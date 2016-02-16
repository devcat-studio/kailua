use std::fmt;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use vec_map::VecMap;

use kailua_syntax::Name;
use ty::{Builtin, Ty, T, Union, TVar, Seq};

pub type Error = String;

pub type CheckResult<T> = Result<T, Error>;

#[derive(Clone, PartialEq)]
pub struct TyInfo {
    pub ty: Union,
    pub builtin: Option<Builtin>,
}

impl TyInfo {
    pub fn new(ty: Union) -> TyInfo {
        TyInfo { ty: ty, builtin: None }
    }

    pub fn from<'a>(ty: T<'a>) -> TyInfo {
        TyInfo::new(Union::from(ty))
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

pub struct Scope {
    names: HashMap<Name, TyInfo>,
    vararg: Option<Option<TyInfo>>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope { names: HashMap::new(), vararg: None }
    }

    pub fn new_function(vararg: Option<TyInfo>) -> Scope {
        Scope { names: HashMap::new(), vararg: Some(vararg) }
    }

    pub fn get<'a>(&'a self, name: &Name) -> Option<&'a TyInfo> {
        self.names.get(name)
    }

    pub fn get_mut<'a>(&'a mut self, name: &Name) -> Option<&'a mut TyInfo> {
        self.names.get_mut(name)
    }

    pub fn get_vararg<'a>(&'a self) -> Option<Option<&'a TyInfo>> {
        self.vararg.as_ref().map(|va| va.as_ref())
    }

    pub fn get_vararg_mut<'a>(&'a mut self) -> Option<Option<&'a mut TyInfo>> {
        self.vararg.as_mut().map(|va| va.as_mut())
    }

    pub fn put(&mut self, name: Name, info: TyInfo) {
        self.names.insert(name, info);
    }
}

#[derive(Clone, PartialEq)]
pub struct TVarBounds {
    pub min: Option<Union>,
    pub max: Option<Union>,
}

impl TVarBounds {
    pub fn new() -> TVarBounds {
        TVarBounds { min: None, max: None }
    }
}

pub struct Context {
    global_scope: Scope,
    next_tvar: Cell<TVar>,
    tvar_bounds: VecMap<RefCell<TVarBounds>>,
}

impl Context {
    pub fn new() -> Context {
        Context { global_scope: Scope::new(), next_tvar: Cell::new(TVar(0)),
                  tvar_bounds: VecMap::new() }
    }

    pub fn global_scope(&self) -> &Scope {
        &self.global_scope
    }

    pub fn global_scope_mut(&mut self) -> &mut Scope {
        &mut self.global_scope
    }

    pub fn gen_tvar(&self) -> TVar {
        let tvar = self.next_tvar.get();
        self.next_tvar.set(TVar(tvar.0 + 1));
        tvar
    }

    /*
    pub fn assert_tvar_sub(&mut self, lhs: TVar, rhs: TVar) -> bool {
        if lhs == rhs { return true; }

        let ltv = lhs.0 as usize;
        let rtv = rhs.0 as usize;

        if !self.tvar_bounds.has_key(ltv) {
            self.tvar_bounds.insert(ltv, TVarBounds::new());
        }
        if !self.tvar_bounds.has_key(rtv) {
            self.tvar_bounds.insert(rtv, TVarBounds::new());
        }

        let lbounds = self.tvar_bounds.get(ltv).unwrap();
        let rbounds = self.tvar_bounds.get(rtv).unwrap();

        // lbounds.min <: lhs <: lbounds.max
        // rbounds.min <: rhs <: rbounds.max

        //       rbounds.max
        //            |
        //       rbounds.min
        //         /
        // lbounds.max
        //     |
        // lbounds.min

        lbounds.borrow_mut().min
    }

    pub fn assert_tvar_eq(&mut self, lhs: TVar, rhs: TVar) -> bool {
    }
    */
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

    pub fn get_vararg<'a>(&'a self) -> Option<&'a TyInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(vainfo) = scope.get_vararg() { return vainfo; }
        }
        if let Some(vainfo) = self.context.global_scope().get_vararg() { return vainfo; }
        None
    }

    pub fn get_vararg_mut<'a>(&'a mut self) -> Option<&'a mut TyInfo> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(vainfo) = scope.get_vararg_mut() { return vainfo; }
        }
        if let Some(vainfo) = self.context.global_scope_mut().get_vararg_mut() { return vainfo; }
        None
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

