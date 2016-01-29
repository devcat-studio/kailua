use std::fmt;
use std::collections::HashMap;

use kailua_syntax::Name;
use ty::{Builtin, Ty, T, Union};

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

pub struct Env<'a> {
    globals: &'a mut HashMap<Name, TyInfo>,
    names: HashMap<Name, TyInfo>,
}

impl<'a> Env<'a> {
    pub fn new(globals: &'a mut HashMap<Name, TyInfo>) -> Env<'a> {
        Env { globals: globals, names: HashMap::new() }
    }

    // since `Clone` does not preserve the lifetime of `Env` itself
    pub fn make_subenv<'x>(&'x mut self) -> Env<'x> {
        // XXX cloning names is very expensive
        Env { globals: self.globals, names: self.names.clone() }
    }

    pub fn make_module<'x>(&'x mut self) -> Env<'x> {
        Env { globals: self.globals, names: HashMap::new() }
    }

    pub fn get_var(&'a self, name: &Name) -> Option<&'a TyInfo> {
        self.names.get(name).or_else(|| self.globals.get(name))
    }

    pub fn get_local_var(&'a self, name: &Name) -> Option<&'a TyInfo> {
        self.names.get(name)
    }

    pub fn add_local_var(&mut self, name: &Name, info: TyInfo) {
        println!("adding a local variable {:?} as {:?}", *name, info);
        self.names.insert(name.to_owned(), info);
    }

    // should be used very rarely, e.g. `...` is local only to the function defined that
    pub fn remove_local_var(&mut self, name: &Name) {
        let oldinfo = self.names.remove(name);
        assert!(oldinfo.is_some(), "tried to remove non-existant name {:?}", name);
    }

    pub fn assign_to_var(&mut self, name: &Name, info: TyInfo) -> CheckResult<()> {
        if let Some(previnfo) = self.names.get_mut(name) {
            if !previnfo.ty.accept(&info.ty) {
                Err(format!("cannot assign {:?} to the variable {:?} with type {:?}",
                            info.ty, name, previnfo.ty))
            } else {
                println!("assigning {:?} to a local variable {:?} with type {:?}",
                         info, *name, *previnfo);
                Ok(())
            }
        } else {
            println!("adding a global variable {:?} as {:?}", *name, info);
            self.globals.insert(name.to_owned(), info);
            Ok(())
        }
    }

    pub fn assume_var(&mut self, name: &Name, info: TyInfo) -> CheckResult<()> {
        if self.names.contains_key(name) {
            println!("(force) adding a global variable {:?} as {:?}", *name, info);
            self.names.insert(name.to_owned(), info);
        } else {
            println!("(force) adding a global variable {:?} as {:?}", *name, info);
            self.globals.insert(name.to_owned(), info);
        }
        Ok(())
    }
}

