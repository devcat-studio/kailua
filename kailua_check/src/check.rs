use std::i32;
use std::ops::{Deref, DerefMut};
use std::collections::HashMap;
use std::borrow::Cow;

use kailua_syntax::{Name, Str, Var, Params, E, Exp, UnOp, BinOp, FuncScope, SelfParam, S, Stmt, Block};
use ty::{Builtin, Ty, T, Union, Lattice};
use ty::flags::*;
use env::{Error, CheckResult, TyInfo, Env, Frame, Scope, Context};

pub trait Options {
    fn require_block(&mut self, path: &[u8]) -> CheckResult<Block> {
        Err("not implemented".into())
    }
}

struct ScopedChecker<'chk, 'env: 'chk>(&'chk mut Checker<'env>);

impl<'chk, 'env> Deref for ScopedChecker<'chk, 'env> {
    type Target = &'chk mut Checker<'env>;
    fn deref(&self) -> &&'chk mut Checker<'env> { &self.0 }
}

impl<'chk, 'env> DerefMut for ScopedChecker<'chk, 'env> {
    fn deref_mut(&mut self) -> &mut &'chk mut Checker<'env> { &mut self.0 }
}

impl<'chk, 'env> Drop for ScopedChecker<'chk, 'env> {
    fn drop(&mut self) { self.0.env.leave(); }
}

pub struct Checker<'env> {
    env: Env<'env>,
    opts: &'env mut Options,
}

impl<'env> Checker<'env> {
    pub fn new(context: &'env mut Context, opts: &'env mut Options) -> Checker<'env> {
        Checker { env: Env::new(context), opts: opts }
    }

    fn scoped<'chk>(&'chk mut self, scope: Scope) -> ScopedChecker<'chk, 'env> {
        self.env.enter(scope);
        ScopedChecker(self)
    }

    pub fn check_un_op(&mut self, op: UnOp, info: &TyInfo) -> CheckResult<TyInfo> {
        let ty = &info.ty;
        let flags = ty.flags();

        match op {
            UnOp::Neg if flags == T_INTEGER =>
                Ok(TyInfo::from(T::Integer)),

            UnOp::Neg if flags.is_numeric() =>
                Ok(TyInfo::from(T::Number)),

            UnOp::Not =>
                Ok(TyInfo::from(T::Boolean)),

            UnOp::Len if flags.is_tabular() =>
                Ok(TyInfo::from(T::Integer)),

            _ => Err(format!("tried to apply {} operator to {:?}", op.symbol(), ty))
        }
    }

    pub fn check_bin_op(&mut self, lhs: &TyInfo, op: BinOp, rhs: &TyInfo) -> CheckResult<TyInfo> {
        let lty = &lhs.ty;
        let rty = &rhs.ty;
        let lflags = lty.flags();
        let rflags = rty.flags();

        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Mod
                    if lflags == T_INTEGER && rflags == T_INTEGER =>
                Ok(TyInfo::from(T::Integer)),

            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Pow | BinOp::Mod
                    if lflags.is_numeric() && rflags.is_numeric() =>
                Ok(TyInfo::from(T::Number)),

            BinOp::Cat if lflags.is_stringy() && rflags.is_stringy() =>
                Ok(TyInfo::from(T::String)),

            BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge
                    if lflags.is_stringy() && rflags.is_stringy() && lflags & rflags != T_NONE =>
                Ok(TyInfo::from(T::Boolean)),

            BinOp::Eq | BinOp::Ne =>
                Ok(TyInfo::from(T::Boolean)),

            BinOp::And | BinOp::Or if lflags & !(T_NIL | T_FALSE) == T_NONE =>
                Ok(TyInfo::new(rty.clone())),

            BinOp::And | BinOp::Or =>
                Ok(TyInfo::new(lty.clone().union(rty.clone()))),

            _ => Err(format!("tried to apply {} operator to {:?} and {:?}", op.symbol(), lty, rty))
        }
    }

    fn check_index(&mut self, ety: &T, kty: &T) -> CheckResult<Option<TyInfo>> {
        if !ety.is_tabular() {
            return Err(format!("tried to index the non-table {:?}", ety));
        }

        // the type can be indexed, try to find the resulting type out
        match *ety {
            T::EmptyTable => return Err(format!("tried to index an empty table {:?}", ety)),

            T::SomeRecord(ref fields) => {
                if !kty.is_stringy() {
                    return Err(format!("tried to index {:?} with non-string {:?}", ety, kty));
                }

                // can `kty` be restricted to strings known in compile time?
                match *kty {
                    T::SomeString(ref s) =>
                        if let Some(vty) = fields.get(s) {
                            Ok(Some(TyInfo::from(*vty.clone())))
                        } else {
                            Ok(None)
                        },
                    /*
                    T::SomeStrings(_) => ...
                    */
                    _ => Ok(Some(TyInfo::from(T::Dynamic))),
                }
            }

            T::SomeTuple(ref fields) => {
                if !kty.is_integral() {
                    return Err(format!("tried to index {:?} with non-integer {:?}", ety, kty));
                }

                // can `kty` be restricted to integers known in compile time?
                match *kty {
                    T::SomeInteger(v) =>
                        if 0 <= v && (v as usize) < fields.len() {
                            Ok(Some(TyInfo::from(*fields[v as usize].clone())))
                        } else {
                            Err(format!("{:?} has no index {:?}", ety, v))
                        },
                    /*
                    T::SomeIntegers(_) => ...
                    */
                    _ => Ok(Some(TyInfo::from(T::Dynamic))),
                }
            }

            T::SomeArray(ref t) => {
                if !kty.is_integral() {
                    return Err(format!("tried to index {:?} with non-integer {:?}", ety, kty));
                }

                Ok(Some(TyInfo::from(*t.clone().into_owned())))
            }

            T::SomeMap(ref k, ref v) => {
                let k = &***k;
                // XXX subtyping
                if *k == T::Dynamic {
                    Ok(Some(TyInfo::from(T::Dynamic)))
                } else if k == kty { // XXX redundant
                    Ok(Some(TyInfo::from(*v.clone().into_owned())))
                } else {
                    Err(format!("tried to index {:?} with {:?}", ety, kty))
                }
            }

            // we don't know what ety is, but it should be partially possible to index it
            _ => Ok(Some(TyInfo::from(T::Dynamic))),
        }
    }

    pub fn visit(&mut self, chunk: &[Stmt]) -> CheckResult<()> {
        self.visit_block(chunk)
    }

    fn visit_block(&mut self, block: &[Stmt]) -> CheckResult<()> {
        let mut scope = self.scoped(Scope::new());
        for stmt in block {
            try!(scope.visit_stmt(stmt));
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
                            try!(self.env.assign_to_var(name, info));
                        }
                    }
                }
                if vars.len() > exps.len() {
                    for var in &vars[exps.len()..] {
                        if let &Var::Name(ref name) = var {
                            let info = TyInfo::from(T::Dynamic);
                            // XXX last exp should unpack
                            try!(self.env.assign_to_var(name, info));
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

                let mut scope = self.scoped(Scope::new());
                scope.env.add_local_var(name, TyInfo::from(T::Number));
                try!(scope.visit_block(block));
            }

            S::ForIn(ref names, ref exps, ref block) => {
                for exp in exps {
                    try!(self.visit_exp(exp));
                }

                let mut scope = self.scoped(Scope::new());
                for name in names {
                    scope.env.add_local_var(name, TyInfo::from(T::Dynamic));
                }
                try!(scope.visit_block(block));
            }

            S::FuncDecl(scope, ref name, ref params, ref block) => {
                let info = TyInfo::from(T::Dynamic);
                match scope {
                    FuncScope::Local => self.env.add_local_var(name, info),
                    FuncScope::Global => try!(self.env.assign_to_var(name, info)),
                }
                // `name` itself is available to the inner scope
                try!(self.visit_func_body(None, params, block));
            }

            S::MethodDecl(ref names, selfparam, ref params, ref block) => {
                // TODO verify names
                let selfinfo = match selfparam {
                    SelfParam::Yes => Some(TyInfo::from(T::Dynamic)),
                    SelfParam::No => None,
                };
                try!(self.visit_func_body(selfinfo, params, block));
            }

            S::Local(ref names, ref exps) => {
                for (i, exp) in exps.iter().enumerate() {
                    let info = try!(self.visit_exp(exp));
                    if i < names.len() {
                        // XXX last exp should unpack
                        self.env.add_local_var(&names[i], info);
                    }
                }
                if names.len() > exps.len() {
                    for name in &names[exps.len()..] {
                        let info = TyInfo::from(T::Nil);
                        // XXX last exp should unpack
                        self.env.add_local_var(name, info);
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

            S::KailuaAssume(ref name, ref kind, ref builtin) => {
                let builtin = if let Some(ref builtin) = *builtin {
                    match &***builtin {
                        b"require" => Some(Builtin::Require),
                        _ => {
                            println!("unrecognized builtin name {:?} for {:?} ignored",
                                     *builtin, *name);
                            None
                        }
                    }
                } else {
                    None
                };
                let info = TyInfo { ty: Union::from(T::from(kind)), builtin: builtin };
                try!(self.env.assume_var(name, info));
            }
        }
        Ok(())
    }

    fn visit_func_body(&mut self, selfinfo: Option<TyInfo>, params: &Params,
                       block: &[Stmt]) -> CheckResult<()> {
        let vainfo;
        if params.variadic {
            vainfo = Some(TyInfo::from(T::Dynamic));
        } else {
            vainfo = None;
        }
        let frame = Frame { vararg: vainfo, returns: Box::new(T::Dynamic) };

        let mut scope = self.scoped(Scope::new_function(frame));
        if let Some(selfinfo) = selfinfo {
            scope.env.add_local_var(&Name::from(&b"self"[..]), selfinfo);
        }
        for param in &params.args {
            scope.env.add_local_var(param, TyInfo::from(T::Dynamic));
        }
        scope.visit_block(block)
    }

    fn visit_var(&mut self, var: &Var) -> CheckResult<Option<TyInfo>> {
        match *var {
            Var::Name(ref name) => {
                // may refer to the global variable yet to be defined!
                if let Some(info) = self.env.get_var(name) {
                    Ok(Some(info.to_owned()))
                } else {
                    Ok(None)
                }
            },

            Var::Index(ref e, ref key) => {
                let ty = try!(self.visit_exp(e)).ty.simplify();
                let kty = try!(self.visit_exp(key)).ty.simplify();
                self.check_index(&ty, &kty)
            },
        }
    }

    fn visit_exp(&mut self, exp: &E) -> CheckResult<TyInfo> {
        match *exp {
            E::Nil => Ok(TyInfo::from(T::Nil)),
            E::False => Ok(TyInfo::from(T::False)),
            E::True => Ok(TyInfo::from(T::True)),
            E::Num(v) if v.floor() == v =>
                if i32::MIN as f64 <= v && v <= i32::MAX as f64 {
                    Ok(TyInfo::from(T::SomeInteger(v as i32)))
                } else {
                    Ok(TyInfo::from(T::Integer))
                },
            E::Num(_) => Ok(TyInfo::from(T::Number)),
            E::Str(ref s) => Ok(TyInfo::from(T::SomeString(Cow::Borrowed(s)))),

            E::Varargs => {
                if let Some(info) = self.env.get_vararg() {
                    Ok(info.to_owned())
                } else {
                    Err("vararg not declared in the innermost func".into())
                }
            },
            E::Var(ref name) => {
                if let Some(info) = self.env.get_var(name) {
                    Ok(info.to_owned())
                } else {
                    Err(format!("global or local variable {:?} not defined", *name))
                }
            },

            E::Func(ref params, ref block) => {
                try!(self.visit_func_body(None, params, block));
                Ok(TyInfo::from(T::Function))
            },
            E::Table(ref fields) => {
                enum Tab { Empty, Record(HashMap<Str, Ty>), Tuple(Vec<Ty>), Array(Ty), Map(Ty, Ty) }
                let mut tab = Tab::Empty;

                for &(ref key, ref value) in fields {
                    let kty;
                    if let Some(ref key) = *key {
                        kty = Some(try!(self.visit_exp(key)).ty.simplify());
                    } else {
                        kty = None;
                    }
                    let vty = try!(self.visit_exp(value)).ty.simplify();

                    // update the table type according to new field
                    tab = match (kty, tab) {
                        (Some(T::SomeString(s)), Tab::Empty) => {
                            let mut fields = HashMap::new();
                            fields.insert(s.into_owned(), Box::new(vty));
                            Tab::Record(fields)
                        },
                        (Some(T::SomeString(s)), Tab::Record(mut fields)) => {
                            // should override a duplicate field if any
                            fields.insert(s.into_owned(), Box::new(vty));
                            Tab::Record(fields)
                        },

                        // XXX tuple?
                        (None, Tab::Empty) => Tab::Array(Box::new(vty)),
                        (None, Tab::Array(t)) => Tab::Array(Box::new(vty.union(*t))),
                        (None, Tab::Tuple(mut fields)) => {
                            fields.push(Box::new(vty));
                            Tab::Tuple(fields)
                        },

                        (Some(kty), Tab::Empty) => Tab::Map(Box::new(kty), Box::new(vty)),

                        (kty, Tab::Record(fields)) => {
                            let kty = Box::new(kty.unwrap_or(T::Integer).union(T::String));
                            let mut vty = vty;
                            for (_, ty) in fields {
                                vty = vty.union(*ty);
                            }
                            Tab::Map(kty, Box::new(vty))
                        },

                        (kty, Tab::Tuple(fields)) => {
                            let kty = Box::new(kty.unwrap_or(T::Integer).union(T::Integer));
                            let mut vty = vty;
                            for ty in fields {
                                vty = vty.union(*ty);
                            }
                            Tab::Map(kty, Box::new(vty))
                        },

                        (kty, Tab::Array(t)) => {
                            let kty = Box::new(kty.unwrap_or(T::Integer).union(T::Integer));
                            let vty = Box::new(vty).union(t);
                            Tab::Map(kty, vty)
                        },

                        (kty, Tab::Map(k, v)) => {
                            let kty = Box::new(kty.unwrap_or(T::Integer)).union(k);
                            let vty = Box::new(vty).union(v);
                            Tab::Map(kty, vty)
                        },
                    };
                }

                // if the table remains intact, it is an empty record
                let tabty = match tab {
                    Tab::Empty => T::EmptyTable,
                    Tab::Record(fields) => T::SomeRecord(Cow::Owned(fields)),
                    Tab::Tuple(fields) => T::SomeTuple(Cow::Owned(fields)),
                    Tab::Array(t) => T::SomeArray(Cow::Owned(t)),
                    Tab::Map(k, v) => T::SomeMap(Cow::Owned(k), Cow::Owned(v)),
                };
                Ok(TyInfo::from(tabty))
            },

            E::FuncCall(ref func, ref args) => {
                let funcinfo = try!(self.visit_exp(func));
                if !funcinfo.ty.flags().is_callable() {
                    return Err(format!("tried to call a non-function type {:?}", funcinfo.ty));
                }

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
                            let mut sub = Checker { env: Env::new(self.env.context()),
                                                    opts: self.opts };
                            try!(sub.visit_block(&block));
                        }
                        Ok(TyInfo::from(T::Dynamic))
                    },

                    _ => Ok(TyInfo::from(T::Dynamic)),
                }
            },

            E::MethodCall(ref e, ref _method, ref args) => {
                let info = try!(self.visit_exp(e));
                if !info.ty.flags().is_tabular() {
                    return Err(format!("tried to index a non-table type {:?}", info.ty));
                }

                for arg in args {
                    try!(self.visit_exp(arg));
                }
                Ok(TyInfo::from(T::Dynamic))
            },

            E::Index(ref e, ref key) => {
                let ty = try!(self.visit_exp(e)).ty.simplify();
                let kty = try!(self.visit_exp(key)).ty.simplify();
                if let Some(vinfo) = try!(self.check_index(&ty, &kty)) {
                    Ok(vinfo)
                } else {
                    Err(format!("cannot index {:?} with {:?}", ty, kty))
                }
            },

            E::Un(op, ref e) => {
                let info = try!(self.visit_exp(e));
                self.check_un_op(op, &info)
            },

            E::Bin(ref l, op, ref r) => {
                let lhs = try!(self.visit_exp(l));
                let rhs = try!(self.visit_exp(r));
                self.check_bin_op(&lhs, op, &rhs)
            },
        }
    }
}

