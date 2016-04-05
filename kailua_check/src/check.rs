use std::i32;
use std::cmp;
use std::ops::{Deref, DerefMut};
use std::borrow::Cow;

use kailua_syntax::{Name, Var, M, TypeSpec, Sig, Ex, UnOp, BinOp, FuncScope, SelfParam};
use kailua_syntax::{St, Stmt, Block};
use diag::CheckResult;
use ty::{T, Seq, Lattice, TypeContext, Numbers, Strings, Tables, Function, S, Slot};
use ty::{Builtin, Flags};
use ty::flags::*;
use env::{TyInfo, Env, Frame, Scope, Context};

pub trait Options {
    fn require_block(&mut self, path: &[u8]) -> CheckResult<Block> {
        Err("not implemented".into())
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Exit {
    None,   // keeps going
    Break,  // exits the current loop
    Return, // exits the current function
    Stop,   // never executes further statements (infinite loop or error)
}

impl Exit {
    fn or(self, other: Exit) -> Exit { cmp::min(self, other) }
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

    fn get_type_bounds(&self, ty: &T) -> (Flags, Flags) {
        let flags = ty.flags();
        let (lb, ub) = ty.has_tvar().map_or((T_NONE, T_NONE), |v| self.env.get_tvar_bounds(v));
        (flags | lb, flags | ub)
    }

    fn scoped<'chk>(&'chk mut self, scope: Scope) -> ScopedChecker<'chk, 'env> {
        self.env.enter(scope);
        ScopedChecker(self)
    }

    pub fn check_un_op(&mut self, op: UnOp, info: &TyInfo) -> CheckResult<TyInfo> {
        let slot = info.borrow();
        let ty = slot.unlift();

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
                check_op!(ty.assert_sub(&(T::table() | T::string()), self.context()));
                Ok(TyInfo::from(T::integer()))
            }
        }
    }

    pub fn check_bin_op(&mut self, lhs: &TyInfo, op: BinOp, rhs: &TyInfo) -> CheckResult<TyInfo> {
        let lslot = lhs.borrow();
        let rslot = rhs.borrow();
        let lty = lslot.unlift();
        let rty = rslot.unlift();

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
                // ? + integer = integer, ? + number = ? + ? = number, number + integer = number
                // see UnOp::Neg comment for the rationale
                let lflags = self.get_type_bounds(lty).1;
                let rflags = self.get_type_bounds(rty).1;
                if lflags.is_integral() && rflags.is_integral() &&
                   !(lflags.is_dynamic() && rflags.is_dynamic()) {
                    // we are definitely sure that it will be an integer
                    check_op!(lty.assert_sub(&T::integer(), self.context()));
                    check_op!(rty.assert_sub(&T::integer(), self.context()));
                    Ok(TyInfo::from(T::integer()))
                } else {
                    // technically speaking they coerce strings to numbers,
                    // but that's probably not what you want
                    check_op!(lty.assert_sub(&T::number(), self.context()));
                    check_op!(rty.assert_sub(&T::number(), self.context()));
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

            BinOp::And => {
                if lty.has_tvar().is_none() {
                    // True and T => T
                    if lty.is_truthy() { return Ok(TyInfo::from(rty.clone())); }
                    // False and T => False
                    if lty.is_falsy() { return Ok(TyInfo::from(lty.clone())); }
                }
                // unsure, both can be possible
                Ok(TyInfo::from(lty.union(&rty, self.context())))
            }

            BinOp::Or => {
                if lty.has_tvar().is_none() {
                    // True or T => True
                    if lty.is_truthy() { return Ok(TyInfo::from(lty.clone())); }
                    // False and T => T
                    if lty.is_falsy() { return Ok(TyInfo::from(rty.clone())); }
                }
                // unsure, both can be possible
                Ok(TyInfo::from(lty.union(&rty, self.context())))
            }
        }
    }

    // XXX consider moving to ty::slot
    fn check_index(&mut self, ety0: &TyInfo, kty0: &TyInfo) -> CheckResult<Option<TyInfo>> {
        let ety = ety0.borrow().unlift().clone();
        let kty = kty0.borrow().unlift().clone();

        if !self.get_type_bounds(&ety).1.is_tabular() {
            return Err(format!("tried to index the non-table {:?}", ety));
        }

        // the type can be indexed, try to find the resulting type out
        // TODO complete the adaptation process
        match ety.has_tables() {
            Some(&Tables::Empty) => {
                macro_rules! check {
                    ($sub:expr) => {
                        if let Err(e) = $sub {
                            return Err(format!("tried to index an empty table {:?}: {}", ety, e));
                        }
                    }
                }

                // try to adapt the table
                if kty.is_integral() {
                    // this is probably an array.
                    // generate a new type variable (as we don't know the value type yet)
                    // and try to adapt to an array of that variable.
                    let vvar = self.context().gen_tvar();
                    check!(ety0.accept(&TyInfo::from(T::array(S::Just(T::TVar(vvar)))),
                                       self.context()));
                    check!(kty.assert_sub(&T::integer(), self.context()));
                    Ok(Some(TyInfo::from(T::TVar(vvar))))
                } else {
                    return Err(format!("tried to index an empty table {:?}", ety));
                }
            }

            Some(&Tables::Record(ref fields)) => {
                if !kty.is_stringy() {
                    return Err(format!("tried to index {:?} with non-string {:?}", ety, kty));
                }

                // can `kty` be restricted to strings known in compile time?
                match kty.has_strings() {
                    Some(&Strings::One(ref s)) =>
                        if let Some(vty) = fields.get(s) {
                            Ok(Some((**vty).clone()))
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
                            Ok(Some((*fields[v as usize]).clone()))
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

                Ok(Some((**t).clone()))
            }

            Some(&Tables::Map(ref k, ref v)) => {
                let k = &**k;
                // XXX subtyping
                if *k == T::Dynamic {
                    Ok(Some(TyInfo::from(T::Dynamic)))
                } else if *k == kty { // XXX redundant
                    Ok(Some((**v).clone()))
                } else {
                    Err(format!("tried to index {:?} with {:?}", ety, kty))
                }
            }

            // we don't know what ety is, but it should be partially possible to index it
            _ => Ok(Some(TyInfo::from(T::Dynamic))),
        }
    }

    pub fn visit(&mut self, chunk: &[Stmt]) -> CheckResult<()> {
        try!(self.visit_block(chunk));
        Ok(())
    }

    fn visit_block(&mut self, block: &[Stmt]) -> CheckResult<Exit> {
        let mut scope = self.scoped(Scope::new());
        let mut exit = Exit::None;
        for stmt in block {
            if exit != Exit::None {
                // TODO warning
                continue;
            }
            exit = try!(scope.visit_stmt(stmt));
        }
        Ok(exit)
    }

    fn visit_stmt(&mut self, stmt: &St) -> CheckResult<Exit> {
        println!("visiting stmt {:?}", *stmt);
        match *stmt {
            St::Void(ref exp) => {
                try!(self.visit_exp(exp));
                Ok(Exit::None)
            }

            St::Assign(ref vars, ref exps) => {
                for varspec in vars {
                    try!(self.visit_var_with_spec(varspec));
                }
                for (i, exp) in exps.iter().enumerate() {
                    let info = try!(self.visit_exp(exp));
                    if i < vars.len() {
                        if let Var::Name(ref name) = vars[i].base {
                            // XXX last exp should unpack
                            try!(self.env.assign_to_var(name, info));
                        }
                    }
                }
                if vars.len() > exps.len() {
                    for var in &vars[exps.len()..] {
                        if let Var::Name(ref name) = var.base {
                            let info = TyInfo::from(T::Dynamic);
                            // XXX last exp should unpack
                            try!(self.env.assign_to_var(name, info));
                        }
                    }
                }
                Ok(Exit::None)
            }

            St::Do(ref block) => self.visit_block(block),

            St::While(ref cond, ref block) => {
                let ty = try!(self.visit_exp(cond));
                let slot = ty.borrow();
                let ty = slot.unlift();
                if ty.is_truthy() {
                    // infinite loop
                    let exit = try!(self.visit_block(block));
                    if exit >= Exit::Return { Ok(exit) } else { Ok(Exit::Stop) }
                } else if ty.is_falsy() {
                    // TODO warning
                    Ok(Exit::None)
                } else {
                    try!(self.visit_block(block)); // TODO scope merger
                    Ok(Exit::None)
                }
            }

            St::Repeat(ref block, ref cond) => {
                let exit = try!(self.visit_block(block)); // TODO scope merger
                let ty = try!(self.visit_exp(cond));
                if exit == Exit::None {
                    let slot = ty.borrow();
                    let ty = slot.unlift();
                    if ty.is_truthy() {
                        Ok(Exit::Stop)
                    } else if ty.is_falsy() {
                        Ok(exit)
                    } else {
                        Ok(Exit::None)
                    }
                } else {
                    // TODO warning?
                    if exit <= Exit::Break { Ok(Exit::None) } else { Ok(exit) }
                }
            }

            St::If(ref conds, ref lastblock) => {
                let mut exit = Exit::Stop;
                let mut ignoreelse = false;
                for &(ref cond, ref block) in conds {
                    if ignoreelse {
                        // TODO warning
                        continue;
                    }
                    let ty = try!(self.visit_exp(cond));
                    let slot = ty.borrow();
                    let ty = slot.unlift();
                    if ty.is_truthy() {
                        ignoreelse = true;
                        exit = exit.or(try!(self.visit_block(block)));
                    } else if ty.is_falsy() {
                        // TODO warning
                    } else {
                        exit = exit.or(try!(self.visit_block(block))); // TODO scope merger
                    }
                }
                if ignoreelse {
                    // TODO warning
                } else if let &Some(ref block) = lastblock {
                    exit = exit.or(try!(self.visit_block(block))); // TODO scope merger
                } else {
                    exit = Exit::None;
                }
                Ok(exit)
            }

            St::For(ref name, ref start, ref end, ref step, ref block) => {
                let start = try!(self.visit_exp(start));
                let end = try!(self.visit_exp(end));
                let step = if let &Some(ref step) = step {
                    try!(self.visit_exp(step))
                } else {
                    Slot::just(T::integer()) // to simplify the matter
                };

                let startslot = start.borrow();
                let endslot = end.borrow();
                let stepslot = step.borrow();

                let startty = startslot.unlift();
                let endty = endslot.unlift();
                let stepty = stepslot.unlift();

                // the similar logic is also present in check_bin_op
                let startflags = self.get_type_bounds(startty).1;
                let endflags = self.get_type_bounds(endty).1;
                let stepflags = self.get_type_bounds(stepty).1;
                let indty;
                if startflags.is_integral() && endflags.is_integral() && stepflags.is_integral() &&
                   !(startflags.is_dynamic() && endflags.is_dynamic() && stepflags.is_dynamic()) {
                    try!(startty.assert_sub(&T::integer(), self.context()));
                    try!(endty.assert_sub(&T::integer(), self.context()));
                    try!(stepty.assert_sub(&T::integer(), self.context()));
                    indty = T::integer();
                } else {
                    try!(startty.assert_sub(&T::number(), self.context()));
                    try!(endty.assert_sub(&T::number(), self.context()));
                    try!(stepty.assert_sub(&T::number(), self.context()));
                    indty = T::number();
                }

                let mut scope = self.scoped(Scope::new());
                scope.env.add_local_var(name, TyInfo::from(indty), true);
                let exit = try!(scope.visit_block(block));
                if exit >= Exit::Return { Ok(exit) } else { Ok(Exit::None) }
            }

            St::ForIn(ref names, ref exps, ref block) => {
                for exp in exps {
                    try!(self.visit_exp(exp));
                }

                let mut scope = self.scoped(Scope::new());
                for name in names {
                    scope.env.add_local_var(name, TyInfo::from(T::Dynamic), true);
                }
                let exit = try!(scope.visit_block(block));
                if exit >= Exit::Return { Ok(exit) } else { Ok(Exit::None) }
            }

            St::FuncDecl(scope, ref name, ref sig, ref block) => {
                // `name` itself is available to the inner scope
                let funcv = self.context().gen_tvar();
                let info = TyInfo::from(T::TVar(funcv));
                match scope {
                    FuncScope::Local => self.env.add_local_var(name, info, true),
                    FuncScope::Global => try!(self.env.assign_to_var(name, info)),
                }
                let functy = try!(self.visit_func_body(None, sig, block));
                try!(T::TVar(funcv).assert_eq(functy.borrow().unlift(), self.context()));
                Ok(Exit::None)
            }

            St::MethodDecl(ref names, selfparam, ref sig, ref block) => {
                // TODO verify names
                let selfinfo = match selfparam {
                    SelfParam::Yes => Some(TyInfo::from(T::Dynamic)),
                    SelfParam::No => None,
                };
                try!(self.visit_func_body(selfinfo, sig, block));
                Ok(Exit::None)
            }

            St::Local(ref names, ref exps) => {
                let add_local_var = |env: &mut Env, namespec: &TypeSpec<Name>, info: TyInfo| {
                    if let Some(ref kind) = namespec.kind {
                        let ty = T::from(kind);
                        let newinfo = match namespec.modf {
                            M::None => Slot::new(S::VarOrCurrently(ty, env.context().gen_mark())),
                            M::Var => Slot::new(S::Var(ty)),
                            M::Const => Slot::new(S::Const(ty)),
                        };
                        env.add_local_var(&namespec.base, newinfo, false);
                        env.assign_to_var(&namespec.base, info)
                    } else {
                        env.add_local_var(&namespec.base, info, true);
                        Ok(())
                    }
                };

                for (i, exp) in exps.iter().enumerate() {
                    let info = try!(self.visit_exp(exp));
                    if i < names.len() {
                        // XXX last exp should unpack
                        try!(add_local_var(&mut self.env, &names[i], info));
                    }
                }
                if names.len() > exps.len() {
                    for namespec in &names[exps.len()..] {
                        // XXX last exp should unpack
                        try!(add_local_var(&mut self.env, namespec, TyInfo::from(T::Nil)));
                    }
                }
                Ok(Exit::None)
            }

            St::Return(ref exps) => {
                let mut tys = Vec::new();
                for exp in exps {
                    tys.push(try!(self.visit_exp(exp)));
                }
                if tys.len() == 1 { // XXX temporary
                    let slot = tys[0].borrow();
                    let ty = slot.unlift();
                    let returns = self.env.get_frame().returns.clone(); // XXX redundant
                    try!(ty.assert_sub(&returns, self.context()));
                }
                Ok(Exit::Return)
            }

            St::Break => Ok(Exit::Break),

            St::KailuaAssume(ref name, kindm, ref kind, ref builtin) => {
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
                let ty = if let Some(builtin) = builtin {
                    T::Builtin(builtin, Box::new(T::from(kind)))
                } else {
                    T::from(kind)
                };
                let sty = match kindm {
                    M::None => S::VarOrCurrently(ty, self.context().gen_mark()),
                    M::Var => S::Var(ty),
                    M::Const => S::Const(ty),
                };
                try!(self.env.assume_var(name, Slot::new(sty)));
                Ok(Exit::None)
            }
        }
    }

    fn visit_func_body(&mut self, selfinfo: Option<TyInfo>, sig: &Sig,
                       block: &[Stmt]) -> CheckResult<TyInfo> {
        let vainfo;
        if sig.variadic {
            vainfo = Some(TyInfo::from(T::Dynamic));
        } else {
            vainfo = None;
        }
        let retv = self.context().gen_tvar();
        let frame = Frame { vararg: vainfo, returns: Box::new(T::TVar(retv)) };

        let mut scope = self.scoped(Scope::new_function(frame));
        if let Some(selfinfo) = selfinfo {
            scope.env.add_local_var(&Name::from(&b"self"[..]), selfinfo, true);
        }

        let mut args = Seq::new();
        for param in &sig.args {
            let ty;
            let sty;
            if let Some(ref kind) = param.kind {
                ty = T::from(kind);
                sty = match param.modf {
                    M::None | M::Var => Slot::new(S::Var(ty.clone())),
                    M::Const => Slot::new(S::Const(ty.clone())),
                };
            } else {
                let argv = scope.context().gen_tvar();
                ty = T::TVar(argv);
                sty = Slot::new(S::Var(T::TVar(argv)));
            }
            scope.env.add_local_var(&param.base, sty, false);
            args.head.push(Box::new(ty));
        }

        let returns = Seq::from(Box::new(T::TVar(retv))); // XXX multiple returns
        assert!(sig.returns.len() <= 1, "multiple returns not supported yet");
        if sig.returns.len() == 1 {
            T::TVar(retv).assert_sub(&T::from(&sig.returns[0]), scope.context()).unwrap();
        }

        try!(scope.visit_block(block));

        Ok(TyInfo::from(T::func(Function { args: args, returns: returns })))
    }

    fn visit_var_with_spec(&mut self, varspec: &TypeSpec<Var>) -> CheckResult<Option<TyInfo>> {
        // TODO fully process varspec
        match varspec.base {
            Var::Name(ref name) => {
                // may refer to the global variable yet to be defined!
                if let Some(info) = self.env.get_var(name) {
                    Ok(Some(info.to_owned()))
                } else {
                    Ok(None)
                }
            },

            Var::Index(ref e, ref key) => {
                let ty = try!(self.visit_exp(e));
                let kty = try!(self.visit_exp(key));
                self.check_index(&ty, &kty)
            },
        }
    }

    fn visit_exp(&mut self, exp: &Ex) -> CheckResult<TyInfo> {
        println!("visiting exp {:?}", *exp);
        match *exp {
            Ex::Nil => Ok(TyInfo::from(T::Nil)),
            Ex::False => Ok(TyInfo::from(T::False)),
            Ex::True => Ok(TyInfo::from(T::True)),
            Ex::Num(v) if v.floor() == v =>
                if i32::MIN as f64 <= v && v <= i32::MAX as f64 {
                    Ok(TyInfo::from(T::int(v as i32)))
                } else {
                    Ok(TyInfo::from(T::integer()))
                },
            Ex::Num(_) => Ok(TyInfo::from(T::number())),
            Ex::Str(ref s) => Ok(TyInfo::from(T::str(s.to_owned()))),

            Ex::Varargs => {
                if let Some(info) = self.env.get_vararg() {
                    Ok(info.to_owned())
                } else {
                    Err("vararg not declared in the innermost func".into())
                }
            },
            Ex::Var(ref name) => {
                if let Some(info) = self.env.get_var(name) {
                    Ok(info.to_owned())
                } else {
                    Err(format!("global or local variable {:?} not defined", *name))
                }
            },

            Ex::Func(ref sig, ref block) => {
                Ok(try!(self.visit_func_body(None, sig, block)))
            },
            Ex::Table(ref fields) => {
                let mut tab = Tables::Empty;

                for &(ref key, ref value) in fields {
                    let kty;
                    if let Some(ref key) = *key {
                        let key = try!(self.visit_exp(key));;
                        kty = Some(key.borrow().unlift().clone());
                    } else {
                        kty = None;
                    }
                    let vty = try!(self.visit_exp(value));
                    let vty = vty.borrow().unlift().clone();

                    // update the table type according to new field
                    tab = tab.insert(kty, vty, self.context());
                }

                // if the table remains intact, it is an empty table
                Ok(TyInfo::from(T::Tables(Cow::Owned(tab))))
            },

            Ex::FuncCall(ref func, ref args) => {
                let funcinfo = try!(self.visit_exp(func));
                let funcinfo = funcinfo.borrow();
                let funcinfo = funcinfo.unlift();

                let mut argtys = Seq::new();
                for arg in args {
                    let argty = try!(self.visit_exp(arg));
                    argtys.head.push(Box::new(argty.borrow().unlift().clone().into_send()));
                }
                let retv = self.context().gen_tvar();
                let rettys = Seq::from(Box::new(T::TVar(retv)));
                let functy = T::func(Function { args: argtys, returns: rettys });

                if let Err(e) = funcinfo.assert_sub(&functy, self.context()) {
                    return Err(format!("failed to call {:?}: {}", funcinfo, e));
                }

                match funcinfo.builtin() {
                    // require("foo")
                    Some(Builtin::Require) if args.len() >= 1 => {
                        if let Ex::Str(ref path) = *args[0] {
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

            Ex::MethodCall(ref e, ref _method, ref args) => {
                let info = try!(self.visit_exp(e));
                if !info.borrow().unlift().is_tabular() {
                    return Err(format!("tried to index a non-table type {:?}", info));
                }

                for arg in args {
                    try!(self.visit_exp(arg));
                }
                Ok(TyInfo::from(T::Dynamic))
            },

            Ex::Index(ref e, ref key) => {
                let ty = try!(self.visit_exp(e));
                let kty = try!(self.visit_exp(key));
                if let Some(vinfo) = try!(self.check_index(&ty, &kty)) {
                    Ok(vinfo)
                } else {
                    Err(format!("cannot index {:?} with {:?}", ty, kty))
                }
            },

            Ex::Un(op, ref e) => {
                let info = try!(self.visit_exp(e));
                self.check_un_op(op, &info)
            },

            Ex::Bin(ref l, op, ref r) => {
                let lhs = try!(self.visit_exp(l));
                let rhs = try!(self.visit_exp(r));
                self.check_bin_op(&lhs, op, &rhs)
            },
        }
    }
}

