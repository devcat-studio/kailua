use std::i32;
use std::ops::{Deref, DerefMut};
use std::borrow::Cow;

use kailua_syntax::{Name, Var, Params, E, UnOp, BinOp, FuncScope, SelfParam, S, Stmt, Block};
use diag::CheckResult;
use ty::{T, Seq, Lattice, TVarContext, Numbers, Strings, Tables, Function};
use ty::flags::*;
use env::{Builtin, TyInfo, Env, Frame, Scope, Context};

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

    fn context(&mut self) -> &mut Context {
        self.env.context()
    }

    fn scoped<'chk>(&'chk mut self, scope: Scope) -> ScopedChecker<'chk, 'env> {
        self.env.enter(scope);
        ScopedChecker(self)
    }

    pub fn check_un_op(&mut self, op: UnOp, info: &TyInfo) -> CheckResult<TyInfo> {
        let ty = &info.ty;

        macro_rules! check_op {
            ($sub:expr) => {
                if let Err(e) = $sub {
                    return Err(format!("tried to apply {} operator to {:?}: {}",
                                       op.symbol(), ty, e));
                }
            }
        }

        match op {
            UnOp::Neg => {
                check_op!(ty.assert_sub(&T::number(), self.context()));

                // it is possible to be more accurate here.
                // e.g. if ty = `v1 \/ integer` and it is known that `v1 <: integer`,
                // then `ty <: integer` and we can safely return an integer.
                // we don't do that though, since probing for <: risks the instantiation.
                if ty.has_tvar().is_none() && ty.flags() == T_INTEGER {
                    Ok(TyInfo::from(T::integer()))
                } else {
                    Ok(TyInfo::from(T::number()))
                }
            }

            UnOp::Not => {
                Ok(TyInfo::from(T::Boolean))
            }

            UnOp::Len => {
                check_op!(ty.assert_sub(&T::table(), self.context()));
                Ok(TyInfo::from(T::integer()))
            }
        }
    }

    pub fn check_bin_op(&mut self, lhs: &TyInfo, op: BinOp, rhs: &TyInfo) -> CheckResult<TyInfo> {
        let lty = &lhs.ty;
        let rty = &rhs.ty;

        macro_rules! check_op {
            ($sub:expr) => {
                if let Err(e) = $sub {
                    return Err(format!("tried to apply {} operator to {:?} and {:?}: {}",
                                       op.symbol(), lty, rty, e));
                }
            }
        }

        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Mod => {
                // technically speaking they coerce strings to numbers,
                // but that's probably not what you want
                check_op!(lty.assert_sub(&T::number(), self.context()));
                check_op!(rty.assert_sub(&T::number(), self.context()));

                // ? + integer = integer, ? + number = ? + ? = number, number + integer = number
                // see UnOp::Neg comment for the rationale
                let lflags = lty.flags();
                let rflags = rty.flags();
                if lty.has_tvar().is_none() && lflags.is_integral() &&
                   rty.has_tvar().is_none() && rflags.is_integral() &&
                   !(lflags.is_dynamic() && rflags.is_dynamic()) {
                    Ok(TyInfo::from(T::integer()))
                } else {
                    Ok(TyInfo::from(T::number()))
                }
            }

            BinOp::Div | BinOp::Pow => {
                check_op!(lty.assert_sub(&T::number(), self.context()));
                check_op!(rty.assert_sub(&T::number(), self.context()));
                Ok(TyInfo::from(T::number()))
            }

            BinOp::Cat => {
                let stringy = T::number() | T::string();
                check_op!(lty.assert_sub(&stringy, self.context()));
                check_op!(rty.assert_sub(&stringy, self.context()));
                Ok(TyInfo::from(T::string()))
            }

            BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                // this is a hard bit, since both operands should be either numbers or strings
                // but not both (as comparing number against string simply chokes).
                // due to the presence of union types, we cannot represent this constraint
                // with subtyping; equality does not work either, as there are large non-trivial
                // subsets of numbers and strings. for now we try to detect if operands are
                // definitely numbers or strings, and bail out when it is not possible.

                // filter any non-strings and non-numbers
                // avoid using assert_sub here, it is not accurate enough
                if !lty.is_stringy() || !rty.is_stringy() {
                    return Err(format!("tried to apply {} operator to {:?} and {:?}",
                                       op.symbol(), lty, rty));
                }

                let lnum = lty.has_numbers().is_some();
                let lstr = lty.has_strings().is_some();
                let rnum = rty.has_numbers().is_some();
                let rstr = rty.has_strings().is_some();
                if (lnum && lstr) || (rnum && rstr) {
                    Err(format!("operands {:?} and {:?} to operator {} should be \
                                 numbers or strings but not both", lty, rty, op.symbol()))
                } else if (lnum && rstr) || (lstr && rnum) {
                    Err(format!("operands {:?} and {:?} to operator {} should be \
                                 both numbers or both strings", lty, rty, op.symbol()))
                } else if lnum || rnum { // operands are definitely numbers
                    check_op!(lty.assert_sub(&T::number(), self.context()));
                    check_op!(rty.assert_sub(&T::number(), self.context()));
                    Ok(TyInfo::from(T::Boolean))
                } else if lstr || rstr { // operands are definitely strings
                    check_op!(lty.assert_sub(&T::string(), self.context()));
                    check_op!(rty.assert_sub(&T::string(), self.context()));
                    Ok(TyInfo::from(T::Boolean))
                } else { // XXX
                    Err(format!("cannot deduce if operands {:?} and {:?} to operator {} are \
                                 either numbers or strings", lty, rty, op.symbol()))
                }
            }

            BinOp::Eq | BinOp::Ne => { // works for any types
                Ok(TyInfo::from(T::Boolean))
            }

            BinOp::And | BinOp::Or => {
                Ok(TyInfo::from(lty.clone().union(rty.clone(), self.context())))
            }
        }
    }

    fn check_index(&mut self, ety: &T, kty: &T) -> CheckResult<Option<TyInfo>> {
        if !ety.is_tabular() {
            return Err(format!("tried to index the non-table {:?}", ety));
        }

        // the type can be indexed, try to find the resulting type out
        match ety.has_tables() {
            Some(&Tables::Empty) =>
                return Err(format!("tried to index an empty table {:?}", ety)),

            Some(&Tables::Record(ref fields)) => {
                if !kty.is_stringy() {
                    return Err(format!("tried to index {:?} with non-string {:?}", ety, kty));
                }

                // can `kty` be restricted to strings known in compile time?
                match kty.has_strings() {
                    Some(&Strings::One(ref s)) =>
                        if let Some(vty) = fields.get(s) {
                            Ok(Some(TyInfo::from(*vty.clone())))
                        } else {
                            Ok(None)
                        },
                    /*
                    Some(&Strings::Some(_)) => ...
                    */
                    _ => Ok(Some(TyInfo::from(T::Dynamic))),
                }
            }

            Some(&Tables::Tuple(ref fields)) => {
                if !kty.is_integral() {
                    return Err(format!("tried to index {:?} with non-integer {:?}", ety, kty));
                }

                // can `kty` be restricted to integers known in compile time?
                match kty.has_numbers() {
                    Some(&Numbers::One(v)) =>
                        if 0 <= v && (v as usize) < fields.len() {
                            Ok(Some(TyInfo::from(*fields[v as usize].clone())))
                        } else {
                            Err(format!("{:?} has no index {:?}", ety, v))
                        },
                    /*
                    Some(&Numbers::Some(_)) => ...
                    */
                    _ => Ok(Some(TyInfo::from(T::Dynamic))),
                }
            }

            Some(&Tables::Array(ref t)) => {
                if !kty.is_integral() {
                    return Err(format!("tried to index {:?} with non-integer {:?}", ety, kty));
                }

                Ok(Some(TyInfo::from((**t).clone())))
            }

            Some(&Tables::Map(ref k, ref v)) => {
                let k = &**k;
                // XXX subtyping
                if *k == T::Dynamic {
                    Ok(Some(TyInfo::from(T::Dynamic)))
                } else if k == kty { // XXX redundant
                    Ok(Some(TyInfo::from((**v).clone())))
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
                scope.env.add_local_var(name, TyInfo::from(T::number()));
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
                // `name` itself is available to the inner scope
                let funcv = self.context().gen_tvar();
                let info = TyInfo::from(T::TVar(funcv));
                match scope {
                    FuncScope::Local => self.env.add_local_var(name, info),
                    FuncScope::Global => try!(self.env.assign_to_var(name, info)),
                }
                let functy = try!(self.visit_func_body(None, params, block));
                try!(T::TVar(funcv).assert_eq(&functy.ty, self.context()));
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
                let info = TyInfo { ty: T::from(kind), builtin: builtin };
                try!(self.env.assume_var(name, info));
            }
        }
        Ok(())
    }

    fn visit_func_body(&mut self, selfinfo: Option<TyInfo>, params: &Params,
                       block: &[Stmt]) -> CheckResult<TyInfo> {
        let vainfo;
        if params.variadic {
            vainfo = Some(TyInfo::from(T::Dynamic));
        } else {
            vainfo = None;
        }
        let retv = self.context().gen_tvar();
        let frame = Frame { vararg: vainfo, returns: Box::new(T::TVar(retv)) };

        let mut scope = self.scoped(Scope::new_function(frame));
        if let Some(selfinfo) = selfinfo {
            scope.env.add_local_var(&Name::from(&b"self"[..]), selfinfo);
        }
        let mut args = Seq::new();
        for param in &params.args {
            let argv = scope.context().gen_tvar();
            scope.env.add_local_var(param, TyInfo::from(T::TVar(argv)));
            args.head.push(Box::new(T::TVar(argv)));
        }
        try!(scope.visit_block(block));

        let returns = Seq::from(Box::new(T::TVar(retv))); // XXX multiple returns
        Ok(TyInfo::from(T::func(Function { args: args, returns: returns })))
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
                let ty = try!(self.visit_exp(e)).ty;
                let kty = try!(self.visit_exp(key)).ty;
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
                    Ok(TyInfo::from(T::int(v as i32)))
                } else {
                    Ok(TyInfo::from(T::integer()))
                },
            E::Num(_) => Ok(TyInfo::from(T::number())),
            E::Str(ref s) => Ok(TyInfo::from(T::str(s.to_owned()))),

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
                Ok(try!(self.visit_func_body(None, params, block)))
            },
            E::Table(ref fields) => {
                let mut tab = Tables::Empty;

                for &(ref key, ref value) in fields {
                    let kty;
                    if let Some(ref key) = *key {
                        kty = Some(try!(self.visit_exp(key)).ty);
                    } else {
                        kty = None;
                    }
                    let vty = try!(self.visit_exp(value)).ty;

                    // update the table type according to new field
                    tab = tab.insert(kty, vty, self.context());
                }

                // if the table remains intact, it is an empty table
                Ok(TyInfo::from(T::Tables(Cow::Owned(tab))))
            },

            E::FuncCall(ref func, ref args) => {
                let funcinfo = try!(self.visit_exp(func));

                let mut argtys = Seq::new();
                for arg in args {
                    let argty = try!(self.visit_exp(arg)).ty;
                    argtys.head.push(Box::new(argty));
                }
                let retv = self.context().gen_tvar();
                let rettys = Seq::from(Box::new(T::TVar(retv)));
                let functy = T::func(Function { args: argtys, returns: rettys });

                if let Err(e) = funcinfo.ty.assert_sub(&functy, self.context()) {
                    return Err(format!("tried to call a non-function type {:?}: {}",
                                       funcinfo.ty, e));
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
                        Ok(TyInfo::from(T::Dynamic)) // XXX
                    },

                    _ => Ok(TyInfo::from(T::TVar(retv))),
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
                let ty = try!(self.visit_exp(e)).ty;
                let kty = try!(self.visit_exp(key)).ty;
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

