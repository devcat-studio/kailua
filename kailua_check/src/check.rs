use std::collections::HashMap;

use kailua_syntax::{Name, Str, Var, Params, E, Exp, UnOp, BinOp, FuncScope, SelfParam, S, Stmt, Block};
use ty::{Builtin, Ty, T};

pub type Error = String;

pub type CheckResult<T> = Result<T, Error>;

#[derive(Clone, Debug, PartialEq)]
pub struct TyInfo {
    pub ty: Ty,
    pub builtin: Option<Builtin>,
}

impl TyInfo {
    pub fn new(ty: T) -> TyInfo {
        TyInfo { ty: Box::new(ty), builtin: None }
    }
}

pub trait Options {
    fn require_block(&mut self, path: &[u8]) -> CheckResult<Block> {
        Err("not implemented".into())
    }
}

pub struct Env<'a> {
    globals: &'a mut HashMap<Name, TyInfo>,
    names: HashMap<Name, TyInfo>,
    opts: &'a mut Options,
}

impl<'a> Env<'a> {
    pub fn new(globals: &'a mut HashMap<Name, TyInfo>, opts: &'a mut Options) -> Env<'a> {
        Env { globals: globals, names: HashMap::new(), opts: opts }
    }

    // inject the base libraries to globals
    pub fn open_libs(&mut self) {
        self.globals.insert(Name::from(&b"require"[..]),
                            TyInfo { ty: Box::new(T::Dynamic), builtin: Some(Builtin::Require) });
        self.globals.insert(Name::from(&b"package"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"assert"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"type"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"tonumber"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"tostring"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"pairs"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"ipairs"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"pcall"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"xpcall"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"error"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"getmetatable"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"setmetatable"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"rawget"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"rawset"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"select"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"print"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"loadstring"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"pack"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"unpack"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"next"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"_G"[..]), TyInfo::new(T::Dynamic));

        self.globals.insert(Name::from(&b"string"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"math"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"table"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"io"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"os"[..]), TyInfo::new(T::Dynamic));
        self.globals.insert(Name::from(&b"debug"[..]), TyInfo::new(T::Dynamic));
    }

    // since `Clone` does not preserve the lifetime of `Env` itself
    fn make_subenv<'x>(&'x mut self) -> Env<'x> {
        // XXX cloning names is very expensive
        Env { globals: self.globals, names: self.names.clone(), opts: self.opts }
    }

    fn get_var<'x>(&'x self, name: &Name) -> Option<&'x TyInfo> {
        self.names.get(name).or_else(|| self.globals.get(name))
    }

    fn add_local_var(&mut self, name: &Name, info: TyInfo) {
        self.names.insert(name.to_owned(), info);
    }

    // should be used very rarely, e.g. `...` is local only to the function defined that
    fn remove_local_var(&mut self, name: &Name) {
        let oldinfo = self.names.remove(name);
        assert!(oldinfo.is_some(), "tried to remove non-existant name {:?}", name);
    }

    fn assign_to_var(&mut self, name: &Name, info: TyInfo) -> CheckResult<()> {
        if let Some(previnfo) = self.names.get_mut(name) {
            if previnfo.ty != info.ty {
                Err("type error".into())
            } else {
                *previnfo = info;
                Ok(())
            }
        } else {
            println!("adding a global variable {:?}", *name);
            self.globals.insert(name.to_owned(), info);
            Ok(())
        }
    }

    pub fn visit(&mut self, chunk: &[Stmt]) -> CheckResult<()> {
        self.visit_block(chunk)
    }

    fn visit_block(&mut self, block: &[Stmt]) -> CheckResult<()> {
        let mut env = self.make_subenv();
        for stmt in block {
            try!(env.visit_stmt(stmt));
        }
        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &S) -> CheckResult<()> {
        match *stmt {
            S::Void(ref exp) => {
                try!(self.visit_exp(exp));
            }
            S::Assign(ref vars, ref exps) => {
                for var in vars {
                    try!(self.visit_var(var));
                }
                for (i, exp) in exps.iter().enumerate() {
                    let info = try!(self.visit_exp(exp));
                    if i < vars.len() {
                        if let &Var::Name(ref name) = &vars[i] {
                            // XXX last exp should unpack
                            try!(self.assign_to_var(name, info));
                        }
                    }
                }
                if vars.len() > exps.len() {
                    for var in &vars[exps.len()..] {
                        if let &Var::Name(ref name) = var {
                            let info = TyInfo::new(T::Dynamic);
                            // XXX last exp should unpack
                            try!(self.assign_to_var(name, info));
                        }
                    }
                }
            }
            S::Do(ref block) => {
                try!(self.visit_block(block));
            }
            S::While(ref cond, ref block) => {
                try!(self.visit_exp(cond));
                try!(self.visit_block(block));
            }
            S::Repeat(ref block, ref cond) => {
                try!(self.visit_block(block));
                try!(self.visit_exp(cond));
            }
            S::If(ref conds, ref lastblock) => {
                for &(ref cond, ref block) in conds {
                    try!(self.visit_exp(cond));
                    try!(self.visit_block(block));
                }
                if let &Some(ref block) = lastblock {
                    try!(self.visit_block(block));
                }
            }
            S::For(ref name, ref start, ref end, ref step, ref block) => {
                try!(self.visit_exp(start));
                try!(self.visit_exp(end));
                if let &Some(ref step) = step {
                    try!(self.visit_exp(step));
                }
                let mut env = self.make_subenv();
                env.add_local_var(name, TyInfo::new(T::Dynamic));
                try!(env.visit_block(block));
            }
            S::ForIn(ref names, ref exps, ref block) => {
                for exp in exps {
                    try!(self.visit_exp(exp));
                }
                let mut env = self.make_subenv();
                for name in names {
                    env.add_local_var(name, TyInfo::new(T::Dynamic));
                }
                try!(env.visit_block(block));
            }
            S::FuncDecl(scope, ref name, ref params, ref block) => {
                let info = TyInfo::new(T::Dynamic);
                match scope {
                    FuncScope::Local => self.add_local_var(name, info),
                    FuncScope::Global => try!(self.assign_to_var(name, info)),
                }
                // `name` itself is available to the inner scope
                try!(self.visit_func_body(None, params, block));
            }
            S::MethodDecl(ref names, selfparam, ref params, ref block) => {
                // TODO verify names
                let selfinfo = match selfparam {
                    SelfParam::Yes => Some(TyInfo::new(T::Dynamic)),
                    SelfParam::No => None,
                };
                try!(self.visit_func_body(selfinfo, params, block));
            }
            S::Local(ref names, ref exps) => {
                for (i, exp) in exps.iter().enumerate() {
                    let info = try!(self.visit_exp(exp));
                    if i < names.len() {
                        // XXX last exp should unpack
                        self.add_local_var(&names[i], info);
                    }
                }
                if names.len() > exps.len() {
                    for name in &names[exps.len()..] {
                        let info = TyInfo::new(T::Dynamic);
                        // XXX last exp should unpack
                        self.add_local_var(name, info);
                    }
                }
            }
            S::Return(ref exps) => {
                // XXX should unify with the current function
                for exp in exps {
                    try!(self.visit_exp(exp));
                }
            }
            S::Break => {}
        }
        Ok(())
    }

    fn visit_func_body(&mut self, selfinfo: Option<TyInfo>, params: &Params,
                       block: &[Stmt]) -> CheckResult<()> {
        let mut env = self.make_subenv();
        if let Some(selfinfo) = selfinfo {
            env.add_local_var(&Name::from(&b"self"[..]), selfinfo);
        }
        for param in &params.0 {
            env.add_local_var(param, TyInfo::new(T::Dynamic));
        }
        let vararg = Name::from(&b"..."[..]);
        if params.1 {
            env.add_local_var(&vararg, TyInfo::new(T::Dynamic));
        } else {
            // function a(...)
            //   return function b() return ... end -- this is an error
            // end
            if env.names.contains_key(&vararg) { env.remove_local_var(&vararg); }
        }
        env.visit_block(block)
    }

    fn visit_var(&mut self, var: &Var) -> CheckResult<Option<TyInfo>> {
        match *var {
            Var::Name(ref name) => {
                // may refer to the global variable yet to be defined!
                if let Some(info) = self.get_var(name) {
                    Ok(Some(info.to_owned()))
                } else {
                    Ok(None)
                }
            },
            Var::Index(ref e, ref key) => {
                try!(self.visit_exp(e));
                try!(self.visit_exp(key));
                Ok(Some(TyInfo::new(T::Dynamic))) // XXX
            },
        }
    }

    fn visit_exp(&mut self, exp: &E) -> CheckResult<TyInfo> {
        match *exp {
            E::Nil => Ok(TyInfo::new(T::Dynamic)),
            E::False => Ok(TyInfo::new(T::Dynamic)),
            E::True => Ok(TyInfo::new(T::Dynamic)),
            E::Num(_) => Ok(TyInfo::new(T::Dynamic)),
            E::Str(_) => Ok(TyInfo::new(T::Dynamic)),

            E::Varargs => {
                if let Some(info) = self.get_var(&Name::from(&b"..."[..])) {
                    Ok(info.to_owned())
                } else {
                    Err("vararg not declared in the innermost func".into())
                }
            },
            E::Var(ref name) => {
                if let Some(info) = self.get_var(name) {
                    Ok(info.to_owned())
                } else {
                    Err(format!("global or local variable {:?} not defined", *name))
                }
            },

            E::Func(ref params, ref block) => {
                try!(self.visit_func_body(None, params, block));
                Ok(TyInfo::new(T::Dynamic))
            },
            E::Table(ref fields) => {
                for &(ref key, ref value) in fields {
                    if let Some(ref key) = *key {
                        try!(self.visit_exp(key));
                    }
                    try!(self.visit_exp(value));
                }
                Ok(TyInfo::new(T::Dynamic))
            },

            E::FuncCall(ref func, ref args) => {
                let funcinfo = try!(self.visit_exp(func));
                for arg in args {
                    try!(self.visit_exp(arg));
                }

                match funcinfo.builtin {
                    // require("foo")
                    Some(Builtin::Require) if args.len() >= 1 => {
                        if let E::Str(ref path) = *args[0] {
                            let block = match self.opts.require_block(path) {
                                Ok(block) => block,
                                Err(e) => return Err(format!("failed to require {:?}: {}",
                                                             *path, e)),
                            };
                            let mut env = Env::new(self.globals, self.opts);
                            try!(env.visit_block(&block));
                        }
                        Ok(TyInfo::new(T::Dynamic))
                    },

                    _ => Ok(TyInfo::new(T::Dynamic)),
                }
            },
            E::MethodCall(ref func, ref _method, ref args) => {
                try!(self.visit_exp(func));
                for arg in args {
                    try!(self.visit_exp(arg));
                }
                Ok(TyInfo::new(T::Dynamic))
            },

            E::Index(ref e, ref key) => {
                try!(self.visit_exp(e));
                try!(self.visit_exp(key));
                Ok(TyInfo::new(T::Dynamic))
            },
            E::Un(_op, ref e) => {
                try!(self.visit_exp(e));
                Ok(TyInfo::new(T::Dynamic))
            },
            E::Bin(ref l, _op, ref r) => {
                try!(self.visit_exp(l));
                try!(self.visit_exp(r));
                Ok(TyInfo::new(T::Dynamic))
            },
        }
    }
}

