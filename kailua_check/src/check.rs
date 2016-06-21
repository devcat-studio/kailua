use std::i32;
use std::cmp;
use std::iter;
use std::ops::{Deref, DerefMut};
use std::borrow::Cow;

use kailua_diag::{Span, Spanned, WithLoc, Report};
use kailua_syntax::{Name, Var, M, TypeSpec, Sig, Ex, Exp, UnOp, BinOp, NameScope};
use kailua_syntax::{St, Stmt, Block};
use diag::CheckResult;
use ty::{T, TySeq, Lattice, TypeContext, Tables, Function, Functions, TyWithNil};
use ty::{S, Slot, SlotSeq, SlotWithNil, Builtin};
use ty::flags::*;
use env::{Env, Frame, Scope, Context};
use options::Options;

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

// conditions out of boolean expression, used for assertion and branch typing
#[derive(Clone, Debug)]
enum Cond {
    Flags(Slot, Flags),
    And(Box<Cond>, Box<Cond>),
    Or(Box<Cond>, Box<Cond>),
    Not(Box<Cond>),
}

fn literal_ty_to_flags(info: &Slot) -> CheckResult<Option<Flags>> {
    let slot = info.borrow();
    if let Some(s) = slot.unlift().as_string() {
        let tyname = &***s;
        let flags = match tyname {
            b"nil" => T_NIL,
            b"number" => T_NUMBER,
            b"string" => T_STRING,
            b"boolean" => T_BOOLEAN,
            b"table" => T_TABLE,
            b"function" => T_FUNCTION,
            b"thread" => T_THREAD,
            b"userdata" => T_USERDATA,
            s => return Err(format!("unknown type name {:?}", s)),
        };
        Ok(Some(flags))
    } else {
        Ok(None)
    }
}

// AssertType built-in accepts more strings than Type
fn ext_literal_ty_to_flags(info: &Slot) -> CheckResult<Option<Flags>> {
    let slot = info.borrow();
    if let Some(s) = slot.unlift().as_string() {
        let tyname = &***s;
        let (tyname, nilflags) = if tyname.ends_with(b"?") {
            (&tyname[..tyname.len()-1], T_NIL)
        } else {
            (tyname, T_NONE)
        };
        let flags = match tyname {
            b"nil" => T_NIL,
            b"int" | b"integer" => T_INTEGER, // XXX the real impl should follow
            b"number" => T_NUMBER,
            b"string" => T_STRING,
            b"boolean" => T_BOOLEAN,
            b"table" => T_TABLE,
            b"function" => T_FUNCTION,
            b"thread" => T_THREAD,
            b"userdata" => T_USERDATA,
            s => return Err(format!("unknown type name {:?}", s)),
        };
        Ok(Some(nilflags | flags))
    } else {
        Ok(None)
    }
}

pub struct Checker<'env> {
    env: &'env mut Env<'env>,
    opts: &'env mut Options,
    report: &'env Report,
}

impl<'env> Checker<'env> {
    pub fn new(env: &'env mut Env<'env>, opts: &'env mut Options,
               report: &'env Report) -> Checker<'env> {
        Checker { env: env, opts: opts, report: report }
    }

    fn context(&mut self) -> &mut Context {
        self.env.context()
    }

    // returns a pair of type flags that is an exact lower and upper bound for that type
    // used as an approximate type bound testing like arithmetics;
    // better be replaced with a non-instantiating assertion though.
    fn get_type_bounds(&self, ty: &T) -> (/*lb*/ Flags, /*ub*/ Flags) {
        let flags = ty.flags();
        let (lb, ub) = ty.has_tvar().map_or((T_NONE, T_NONE), |v| self.env.get_tvar_bounds(v));
        (flags | lb, flags | ub)
    }

    // exactly resolves the type variable inside `ty` if possible
    // this is a requirement for table indexing and function calls
    fn resolve_exact_type<'a>(&mut self, ty: &T<'a>) -> Option<T<'a>> {
        match ty.split_tvar() {
            (None, None) => unreachable!(),
            (None, Some(t)) => Some(t),
            (Some(tv), None) => self.env.get_tvar_exact_type(tv),
            (Some(tv), Some(t)) => {
                if let Some(t_) = self.env.get_tvar_exact_type(tv) {
                    Some(t.union(&t_, self.env.context()))
                } else {
                    None
                }
            }
        }
    }

    fn scoped<'chk>(&'chk mut self, scope: Scope) -> ScopedChecker<'chk, 'env> {
        self.env.enter(scope);
        ScopedChecker(self)
    }

    fn check_un_op(&mut self, op: UnOp, info: &Slot) -> CheckResult<Slot> {
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
                    Ok(Slot::just(T::integer()))
                } else {
                    Ok(Slot::just(T::number()))
                }
            }

            UnOp::Not => {
                Ok(Slot::just(T::Boolean))
            }

            UnOp::Len => {
                check_op!(ty.assert_sub(&(T::table() | T::string()), self.context()));
                Ok(Slot::just(T::integer()))
            }
        }
    }

    fn check_bin_op(&mut self, lhs: &Slot, op: BinOp, rhs: &Slot) -> CheckResult<Slot> {
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
                    Ok(Slot::just(T::integer()))
                } else {
                    // technically speaking they coerce strings to numbers,
                    // but that's probably not what you want
                    check_op!(lty.assert_sub(&T::number(), self.context()));
                    check_op!(rty.assert_sub(&T::number(), self.context()));
                    Ok(Slot::just(T::number()))
                }
            }

            BinOp::Div | BinOp::Pow => {
                check_op!(lty.assert_sub(&T::number(), self.context()));
                check_op!(rty.assert_sub(&T::number(), self.context()));
                Ok(Slot::just(T::number()))
            }

            BinOp::Cat => {
                let stringy = T::number() | T::string();
                check_op!(lty.assert_sub(&stringy, self.context()));
                check_op!(rty.assert_sub(&stringy, self.context()));
                Ok(Slot::just(T::string()))
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
                    Ok(Slot::just(T::Boolean))
                } else if lstr || rstr { // operands are definitely strings
                    check_op!(lty.assert_sub(&T::string(), self.context()));
                    check_op!(rty.assert_sub(&T::string(), self.context()));
                    Ok(Slot::just(T::Boolean))
                } else { // XXX
                    Err(format!("cannot deduce if operands {:?} and {:?} to operator {} are \
                                 either numbers or strings", lty, rty, op.symbol()))
                }
            }

            BinOp::Eq | BinOp::Ne => { // works for any types
                Ok(Slot::just(T::Boolean))
            }

            BinOp::And => {
                if lty.is_dynamic() || rty.is_dynamic() {
                    return Ok(Slot::just(T::Dynamic));
                }

                if lty.has_tvar().is_none() {
                    // True and T => T
                    if lty.is_truthy() { return Ok(Slot::just(rty.clone())); }
                    // False and T => False
                    if lty.is_falsy() { return Ok(Slot::just(lty.clone())); }
                }
                // unsure, both can be possible
                Ok(Slot::just(lty.union(&rty, self.context())))
            }

            BinOp::Or => {
                if lty.is_dynamic() || rty.is_dynamic() {
                    return Ok(Slot::just(T::Dynamic));
                }

                if lty.has_tvar().is_none() {
                    // True or T => True
                    if lty.is_truthy() { return Ok(Slot::just(lty.clone())); }
                    // False or T => T
                    if lty.is_falsy() { return Ok(Slot::just(rty.clone())); }
                }
                // unsure, both can be possible
                Ok(Slot::just(lty.union(&rty, self.context())))
            }
        }
    }

    fn check_index(&mut self, ety0: &Slot, kty0: &Slot,
                   lval: bool) -> CheckResult<Option<Slot>> {
        let ety = ety0.borrow().unlift().clone();
        let kty = kty0.borrow().unlift().clone();

        if !self.get_type_bounds(&ety).1.is_tabular() {
            return Err(format!("tried to index the non-table {:?}", ety));
        }
        let ety = if let Some(ety) = self.resolve_exact_type(&ety) {
            ety
        } else {
            return Err(format!("the type {:?} is tabular but not known enough to index", ety));
        };

        macro_rules! check {
            ($sub:expr) => {
                if let Err(e) = $sub {
                    return Err(format!("failed to index a table {:?}: {}", ety, e));
                }
            }
        }

        // if a new field is about to be created, generate a new type variable
        // (as we don't know the value type yet)
        // and try to adapt to a table containing that variable.
        let new_slot = |context: &mut Context, linear: bool| {
            let tvar = T::TVar(context.gen_tvar());
            let slot = if linear {
                S::VarOrCurrently(tvar, context.gen_mark())
            } else {
                S::Var(tvar)
            };
            Slot::new(slot)
        };

        // try fields first if the key is a string or integer determined in the compile time.
        let litkey =
            if let Some(key) = kty.as_integer() {
                Some(key.into())
            } else if let Some(key) = kty.as_string() {
                Some(key.into())
            } else {
                None
            };
        if let Some(litkey) = litkey {
            match (ety.has_tables(), lval) {
                (Some(&Tables::Empty), true) => {
                    let vslot = new_slot(self.context(), true);
                    let fields = Some((litkey, vslot.clone())).into_iter().collect();
                    let adapted = T::Tables(Cow::Owned(Tables::Fields(fields)));
                    check!(ety0.accept(&Slot::just(adapted), self.context()));
                    return Ok(Some(vslot));
                }

                (Some(&Tables::Fields(ref fields)), true) => {
                    let mut fields = fields.clone();
                    let vslot = fields.entry(litkey)
                                      .or_insert_with(|| new_slot(self.context(), true))
                                      .clone();
                    let adapted = T::Tables(Cow::Owned(Tables::Fields(fields)));
                    check!(ety0.accept(&Slot::just(adapted), self.context()));
                    return Ok(Some(vslot));
                }

                // while we cannot adapt the table, we can resolve the field
                (Some(&Tables::Fields(ref fields)), false) => {
                    return Ok(fields.get(&litkey).map(|s| (*s).clone()));
                }

                _ => {}
            }
        }

        // try to adapt arrays and mappings otherwise.
        // XXX this is severely limited right now, due to the difficulty of union with type vars
        let intkey = self.get_type_bounds(&kty).1.is_integral();
        match (ety.has_tables(), lval) {
            // possible! this occurs when the ety was Dynamic.
            (None, _) => Ok(Some(Slot::just(T::Dynamic))),

            (Some(&Tables::Fields(..)), false) =>
                Err(format!("cannot index {:?} with index {:?} that cannot be resolved \
                             in compile time", ety, kty)),

            (Some(&Tables::Empty), true) => {
                let vslot = new_slot(self.context(), false);
                let tab = if intkey {
                    Tables::Array(SlotWithNil::from_slot(vslot.clone()))
                } else {
                    Tables::Map(Box::new(kty.into_send()),
                                SlotWithNil::from_slot(vslot.clone()))
                };
                let adapted = T::Tables(Cow::Owned(tab));
                check!(ety0.accept(&Slot::just(adapted), self.context()));
                Ok(Some(vslot))
            },

            (Some(&Tables::Empty), false) => Ok(None),

            (Some(&Tables::Array(ref value)), _) if intkey =>
                Ok(Some((**value).clone())),

            (Some(&Tables::Array(..)), false) =>
                Err(format!("cannot index an array {:?} with a non-integral index {:?}", ety, kty)),

            (Some(&Tables::Map(ref key, ref value)), false) => {
                check!(kty.assert_sub(key, self.context()));
                Ok(Some((**value).clone()))
            },

            (Some(&Tables::All), _) =>
                Err(format!("cannot index {:?} without downcasting", ety)),

            // Fields with no keys resolved in compile time, Array with non-integral keys, Map
            (Some(tab), true) => {
                // we cannot keep the specialized table type, lift and adapt to a mapping
                let tab = tab.clone().lift_to_map(self.context());
                let adapted = T::Tables(Cow::Owned(tab));
                check!(ety0.accept(&Slot::just(adapted), self.context()));

                // reborrow ety0 to check against the final mapping type
                if let Some(&Tables::Map(ref key, ref value)) =
                        ety0.borrow().unlift().has_tables() {
                    check!(kty.assert_sub(key, self.context()));
                    Ok(Some((**value).clone()))
                } else {
                    unreachable!()
                }
            }
        }
    }

    pub fn visit(&mut self, chunk: &Spanned<Block>) -> CheckResult<()> {
        try!(self.visit_block(chunk));
        Ok(())
    }

    fn visit_block(&mut self, block: &Spanned<Block>) -> CheckResult<Exit> {
        let mut scope = self.scoped(Scope::new());
        let mut exit = Exit::None;
        for stmt in &block.base {
            if exit != Exit::None {
                // TODO warning
                continue;
            }
            exit = try!(scope.visit_stmt(stmt));
        }
        Ok(exit)
    }

    fn visit_stmt(&mut self, stmt: &Spanned<Stmt>) -> CheckResult<Exit> {
        debug!("visiting stmt {:?}", *stmt);
        match *stmt.base {
            St::Void(ref exp) => {
                try!(self.visit_exp(exp));
                Ok(Exit::None)
            }

            St::Assign(ref vars, ref exps) => {
                for varspec in vars {
                    try!(self.visit_var_with_spec(varspec));
                }
                let infos = try!(self.visit_explist(exps));
                let infos = infos.into_iter().chain(iter::repeat(Slot::just(T::Nil)));
                for (var, info) in vars.iter().zip(infos) {
                    // TODO unify with visit_var_with_spec
                    if let Var::Name(ref name) = var.base.base {
                        try!(self.env.assign_to_var(name, info));
                    }
                }
                Ok(Exit::None)
            }

            St::Do(ref block) => self.visit_block(block),

            St::While(ref cond, ref block) => {
                let ty = try!(self.visit_exp(cond)).into_first();
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
                let ty = try!(self.visit_exp(cond)).into_first();
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
                    let ty = try!(self.visit_exp(cond)).into_first();
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
                let start = try!(self.visit_exp(start)).into_first();
                let end = try!(self.visit_exp(end)).into_first();
                let step = if let &Some(ref step) = step {
                    try!(self.visit_exp(step)).into_first()
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
                scope.env.add_local_var(name, Slot::just(indty), true);
                let exit = try!(scope.visit_block(block));
                if exit >= Exit::Return { Ok(exit) } else { Ok(Exit::None) }
            }

            St::ForIn(ref names, ref exps, ref block) => {
                for exp in exps {
                    try!(self.visit_exp(exp));
                }

                let mut scope = self.scoped(Scope::new());
                for name in names {
                    scope.env.add_local_var(name, Slot::just(T::Dynamic), true);
                }
                let exit = try!(scope.visit_block(block));
                if exit >= Exit::Return { Ok(exit) } else { Ok(Exit::None) }
            }

            St::FuncDecl(scope, ref name, ref sig, ref block) => {
                // `name` itself is available to the inner scope
                let funcv = self.context().gen_tvar();
                let info = Slot::just(T::TVar(funcv));
                match scope {
                    NameScope::Local => self.env.add_local_var(name, info, true),
                    NameScope::Global => try!(self.env.assign_to_var(name, info)),
                }
                let functy = try!(self.visit_func_body(None, sig, block));
                try!(T::TVar(funcv).assert_eq(functy.borrow().unlift(), self.context()));
                Ok(Exit::None)
            }

            St::MethodDecl(ref names, selfparam, ref sig, ref block) => {
                // TODO verify names
                let selfinfo = match selfparam {
                    Some(_) => Some(Slot::just(T::Dynamic)),
                    None => None,
                };
                try!(self.visit_func_body(selfinfo, sig, block));
                Ok(Exit::None)
            }

            St::Local(ref names, ref exps) => {
                let add_local_var = |env: &mut Env,
                                     namespec: &TypeSpec<Spanned<Name>>,
                                     info: Slot| {
                    if let Some(ref kind) = namespec.kind {
                        let ty = try!(T::from(kind, env));
                        let newinfo = match namespec.modf {
                            M::None => Slot::new(S::VarOrCurrently(ty, env.context().gen_mark())),
                            M::Var => Slot::new(S::Var(ty)),
                            M::Const => Slot::new(S::Const(ty)),
                        };
                        env.add_local_var(&namespec.base.base, newinfo, false);
                        env.assign_to_var(&namespec.base.base, info)
                    } else {
                        env.add_local_var(&namespec.base.base, info, true);
                        Ok(())
                    }
                };

                let infos = try!(self.visit_explist(exps));
                let infos = infos.into_iter().chain(iter::repeat(Slot::just(T::Nil)));
                for (namespec, info) in names.iter().zip(infos) {
                    try!(add_local_var(&mut self.env, namespec, info));
                }
                Ok(Exit::None)
            }

            St::Return(ref exps) => {
                let seq = try!(self.visit_explist(exps));
                let seq = seq.unlift(); // XXX wait, is it safe?
                let (returns, returns_exact) = {
                    let frame = self.env.get_frame();
                    (frame.returns.clone(), frame.returns_exact) // XXX redundant
                };
                if returns_exact {
                    try!(Some(seq).assert_sub(&returns, self.context()));
                } else {
                    // need to infer the return type
                    let returns = Some(seq).union(&returns, self.context());
                    self.env.get_frame_mut().returns = returns;
                }
                Ok(Exit::Return)
            }

            St::Break => Ok(Exit::Break),

            St::KailuaOpen(ref name) => {
                try!(self.env.context().open_library(name, self.opts, self.report));
                Ok(Exit::None)
            }

            St::KailuaType(ref name, ref kind) => {
                let ty = Box::new(try!(T::from(kind, &mut self.env)));
                try!(self.env.define_type(&name.base, ty));
                Ok(Exit::None)
            }

            St::KailuaAssume(scope, ref name, kindm, ref kind, ref builtin) => {
                let mut ty = try!(T::from(kind, &mut self.env));
                if let Some(ref bname) = *builtin {
                    if let Some(builtin) = Builtin::from_name(bname) {
                        ty = T::Builtin(builtin, Box::new(ty));
                    } else {
                        warn!("unrecognized builtin name {:?} for {:?} ignored", *bname, *name);
                    }
                }
                let sty = match kindm {
                    M::None => S::VarOrCurrently(ty, self.context().gen_mark()),
                    M::Var => S::Var(ty),
                    M::Const => S::Const(ty),
                };
                match scope {
                    NameScope::Local => try!(self.env.assume_var(name, Slot::new(sty))),
                    NameScope::Global => try!(self.env.assume_global_var(name, Slot::new(sty))),
                }
                Ok(Exit::None)
            }
        }
    }

    fn visit_func_body(&mut self, selfinfo: Option<Slot>, sig: &Sig,
                       block: &Spanned<Vec<Spanned<Stmt>>>) -> CheckResult<Slot> {
        let vatype = match sig.args.tail {
            None => None,
            // varargs present but types are unspecified
            Some(None) => Some(Box::new(TyWithNil::from(T::TVar(self.context().gen_tvar())))),
            Some(Some(ref k)) => Some(Box::new(TyWithNil::from(try!(T::from(k, &mut self.env))))),
        };
        let vainfo = vatype.clone().map(|t| TySeq { head: Vec::new(), tail: Some(t) });

        // we accumulate all known return types inside the frame
        // then checks if it matches with the `returns`.
        //
        // TODO the exception should be made to the recursive usage;
        // we probably need to put a type variable that is later equated to the actual returns
        let frame = if let Some(ref returns) = sig.returns {
            let returns = try!(TySeq::from_kind_seq(returns, &mut self.env));
            Frame { vararg: vainfo, returns: Some(returns), returns_exact: true }
        } else {
            Frame { vararg: vainfo, returns: None, returns_exact: false }
        };

        let mut scope = self.scoped(Scope::new_function(frame));
        if let Some(selfinfo) = selfinfo {
            scope.env.add_local_var(&Name::from(&b"self"[..]), selfinfo, true);
        }

        let mut argshead = Vec::new();
        for param in &sig.args.head {
            let ty;
            let sty;
            if let Some(ref kind) = param.kind {
                ty = try!(T::from(kind, &mut scope.env));
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
            argshead.push(Box::new(ty));
        }
        let args = TySeq { head: argshead, tail: vatype };

        if let Exit::None = try!(scope.visit_block(block)) {
            // the last statement is an implicit return
            let ret = Box::new(St::Return(Vec::new())).with_loc(Span::dummy());
            try!(scope.visit_stmt(&ret));
        }

        let returns = scope.env.get_frame_mut().returns.take().unwrap();
        Ok(Slot::just(T::func(Function { args: args, returns: returns })))
    }

    fn visit_var_with_spec(&mut self,
                           varspec: &TypeSpec<Spanned<Var>>) -> CheckResult<Option<Slot>> {
        // TODO fully process varspec
        match varspec.base.base {
            Var::Name(ref name) => {
                // may refer to the global variable yet to be defined!
                if let Some(info) = self.env.get_var(name) {
                    Ok(Some(info.to_owned()))
                } else {
                    Ok(None)
                }
            },

            Var::Index(ref e, ref key) => {
                let ty = try!(self.visit_exp(e)).into_first();
                let kty = try!(self.visit_exp(key)).into_first();
                self.check_index(&ty, &kty, true)
            },
        }
    }

    fn visit_func_call(&mut self, funcinfo: &T, args: &[Spanned<Exp>]) -> CheckResult<SlotSeq> {
        if !self.get_type_bounds(funcinfo).1.is_callable() {
            return Err(format!("tried to call the non-function {:?}", funcinfo));
        }
        if funcinfo.is_dynamic() {
            return Ok(SlotSeq::from(T::Dynamic));
        }
        let funcinfo = if let Some(ty) = self.resolve_exact_type(&funcinfo) {
            ty
        } else {
            return Err(format!("the type {:?} is callable but not known enough to call",
                               funcinfo));
        };

        macro_rules! check {
            ($sub:expr) => {
                if let Err(e) = $sub {
                    return Err(format!("failed to call {:?}: {}", funcinfo, e));
                }
            }
        }

        let argtys = try!(self.visit_explist(args));

        // handle builtins, which may return different things from the function signature
        match funcinfo.builtin() {
            // require("foo")
            Some(Builtin::Require) => {
                if args.len() < 1 {
                    return Err(format!("`require` needs at least one argument"));
                }
                if let Ex::Str(ref path) = *args[0].base {
                    let block = match self.opts.require_block(path) {
                        Ok(block) => block,
                        Err(e) => return Err(format!("failed to require {:?}: {}",
                                                     *path, e)),
                    };
                    let mut env = Env::new(self.env.context());
                    let mut sub = Checker::new(&mut env, self.opts, self.report);
                    try!(sub.visit_block(&block));
                }
                return Ok(SlotSeq::from(T::Dynamic)); // XXX
            },

            // assert(expr)
            Some(Builtin::Assert) => {
                if args.len() < 1 {
                    return Err(format!("`assert` built-in needs at least one argument"));
                }
                let (cond, _seq) = try!(self.collect_conds_from_exp(&args[0]));
                if let Some(cond) = cond {
                    try!(self.assert_cond(cond, false));
                }
            },

            // assert_not(expr)
            Some(Builtin::AssertNot) => {
                if args.len() < 1 {
                    return Err(format!("`assert-not` built-in needs at least one argument"));
                }
                let (cond, _seq) = try!(self.collect_conds_from_exp(&args[0]));
                if let Some(cond) = cond {
                    try!(self.assert_cond(cond, true));
                }
            },

            // assert_type(expr)
            Some(Builtin::AssertType) => {
                if args.len() < 2 {
                    return Err(format!("`assert-type` built-in needs at least two arguments"));
                }
                if let Some(flags) = try!(ext_literal_ty_to_flags(&argtys.head[1])) {
                    let cond = Cond::Flags(argtys.head[0].clone(), flags);
                    try!(self.assert_cond(cond, false));
                }
            },

            _ => {}
        }

        // check if funcinfo.args :> argtys
        let returns = match *funcinfo.has_functions().unwrap() {
            Functions::Simple(ref func) => {
                check!(argtys.unlift().assert_sub(&func.args, self.context()));
                func.returns.clone()
            }
            Functions::Multi(ref _funcs) => unimplemented!(), // XXX
            Functions::All => {
                return Err(format!("cannot call {:?} without downcasting", funcinfo));
            }
        };

        Ok(SlotSeq::from_seq(returns))
    }

    fn visit_exp(&mut self, exp: &Spanned<Exp>) -> CheckResult<SlotSeq> {
        debug!("visiting exp {:?}", *exp);
        match *exp.base {
            Ex::Nil => Ok(SlotSeq::from(T::Nil)),
            Ex::False => Ok(SlotSeq::from(T::False)),
            Ex::True => Ok(SlotSeq::from(T::True)),
            Ex::Num(v) if v.floor() == v =>
                if i32::MIN as f64 <= v && v <= i32::MAX as f64 {
                    Ok(SlotSeq::from(T::int(v as i32)))
                } else {
                    Ok(SlotSeq::from(T::integer()))
                },
            Ex::Num(_) => Ok(SlotSeq::from(T::number())),
            Ex::Str(ref s) => Ok(SlotSeq::from(T::str(s.to_owned()))),

            Ex::Varargs => {
                if let Some(vararg) = self.env.get_vararg() {
                    Ok(SlotSeq::from_seq(vararg.clone()))
                } else {
                    Err("vararg not declared in the innermost func".into())
                }
            },
            Ex::Var(ref name) => {
                if let Some(info) = self.env.get_var(name) {
                    Ok(SlotSeq::from_slot(info.clone()))
                } else {
                    Err(format!("global or local variable {:?} not defined", *name))
                }
            },

            Ex::Func(ref sig, ref block) => {
                Ok(SlotSeq::from_slot(try!(self.visit_func_body(None, sig, block))))
            },
            Ex::Table(ref fields) => {
                let mut tab = Tables::Empty;

                let mut len = 0;
                for (idx, &(ref key, ref value)) in fields.iter().enumerate() {
                    // if this is the last entry and no explicit index is set, splice the values
                    if idx == fields.len() - 1 && key.is_none() {
                        let vty = try!(self.visit_exp(value));
                        for ty in &vty.head {
                            let ty = ty.borrow().unlift().clone();
                            len += 1;
                            tab = tab.insert(T::int(len), ty, self.context());
                        }
                        if let Some(ty) = vty.tail {
                            // a simple array is no longer sufficient now
                            let ty = ty.borrow().unlift().clone();
                            tab = tab.insert(T::integer(), ty, self.context());
                        }
                    } else {
                        let kty = if let Some(ref key) = *key {
                            let key = try!(self.visit_exp(key)).into_first();
                            let key = key.borrow();
                            key.unlift().clone().into_send()
                        } else {
                            len += 1;
                            T::int(len)
                        };

                        // update the table type according to new field
                        let vty = try!(self.visit_exp(value)).into_first();
                        let vty = vty.borrow().unlift().clone();
                        tab = tab.insert(kty, vty, self.context());
                    }
                }

                // if the table remains intact, it is an empty table
                Ok(SlotSeq::from(T::Tables(Cow::Owned(tab))))
            },

            Ex::FuncCall(ref func, ref args) => {
                let funcinfo = try!(self.visit_exp(func)).into_first();
                let funcinfo = funcinfo.borrow();
                let funcinfo = funcinfo.unlift();
                self.visit_func_call(funcinfo, args)
            },

            Ex::MethodCall(ref e, ref _method, ref args) => {
                let info = try!(self.visit_exp(e)).into_first();
                if !info.borrow().unlift().is_tabular() {
                    return Err(format!("tried to index a non-table type {:?}", info));
                }

                for arg in args {
                    try!(self.visit_exp(arg));
                }
                Ok(SlotSeq::from(T::Dynamic))
            },

            Ex::Index(ref e, ref key) => {
                let ty = try!(self.visit_exp(e)).into_first();
                let kty = try!(self.visit_exp(key)).into_first();
                if let Some(vinfo) = try!(self.check_index(&ty, &kty, false)) {
                    Ok(SlotSeq::from_slot(vinfo))
                } else {
                    Err(format!("cannot index {:?} with {:?}", ty, kty))
                }
            },

            Ex::Un(op, ref e) => {
                let info = try!(self.visit_exp(e)).into_first();
                self.check_un_op(op.base, &info).map(SlotSeq::from_slot)
            },

            Ex::Bin(ref l, op, ref r) => {
                let lhs = try!(self.visit_exp(l)).into_first();
                let rhs = try!(self.visit_exp(r)).into_first();
                self.check_bin_op(&lhs, op.base, &rhs).map(SlotSeq::from_slot)
            },
        }
    }

    fn visit_explist(&mut self, exps: &[Spanned<Exp>]) -> CheckResult<SlotSeq> {
        let mut head = Vec::new();
        let mut last: Option<SlotSeq> = None;
        for exp in exps {
            if let Some(last) = last.take() {
                head.push(last.into_first());
            }
            last = Some(try!(self.visit_exp(exp)));
        }

        let mut seq = SlotSeq { head: head, tail: None };
        if let Some(last) = last {
            seq.head.extend(last.head.into_iter());
            seq.tail = last.tail;
        }
        Ok(seq)
    }

    fn collect_type_from_exp(&mut self,
                             exp: &Spanned<Exp>) -> CheckResult<(Option<Slot>, SlotSeq)> {
        if let Ex::FuncCall(ref func, ref args) = *exp.base {
            let funcinfo = try!(self.visit_exp(func)).into_first();
            let funcinfo = funcinfo.borrow();
            let funcinfo = funcinfo.unlift();
            let typeofexp = if funcinfo.builtin() == Some(Builtin::Type) {
                // there should be a single argument there
                if args.len() != 1 {
                    return Err(format!("{:?} should be called with one argument, received {:?}",
                                       *func, *args));
                }
                let arg = try!(self.visit_exp(&args[0])).into_first();
                Some(arg)
            } else {
                None
            };
            let seq = try!(self.visit_func_call(funcinfo, args));
            Ok((typeofexp, seq))
        } else {
            let seq = try!(self.visit_exp(exp));
            Ok((None, seq))
        }
    }

    // similar to visit_exp but also tries to collect Cond
    fn collect_conds_from_exp(&mut self,
                              exp: &Spanned<Exp>) -> CheckResult<(Option<Cond>, SlotSeq)> {
        debug!("collecting conditions from exp {:?}", *exp);
        match *exp.base {
            Ex::Un(Spanned { base: UnOp::Not, .. }, ref e) => {
                let (cond, seq) = try!(self.collect_conds_from_exp(e));
                let cond = match cond {
                    Some(Cond::Not(cond)) => match *cond {
                        Cond::Not(cond) => Some(*cond),
                        cond => Some(Cond::Not(Box::new(cond))),
                    },
                    Some(Cond::Flags(info, flags)) => Some(Cond::Flags(info, !flags)),
                    Some(cond) => Some(Cond::Not(Box::new(cond))),
                    None => None,
                };
                let info = seq.into_first();
                let info = try!(self.check_un_op(UnOp::Not, &info));
                Ok((cond, SlotSeq::from_slot(info)))
            }

            Ex::Bin(ref l, Spanned { base: BinOp::Eq, .. }, ref r) => {
                let (lty, linfo) = try!(self.collect_type_from_exp(l));
                let (rty, rinfo) = try!(self.collect_type_from_exp(r));

                let linfo = linfo.into_first();
                let rinfo = rinfo.into_first();

                // detect an expression of the form `type(x) == y`.
                // it is technically possible to detect `type(x) == type(y)` as well,
                // but it is not common and results in a very subtle semi-equivalence condition
                // that we cannot readily handle.
                let cond = match (lty, rty) {
                    (Some(ty), None) => {
                        if let Some(flags) = try!(literal_ty_to_flags(&rinfo)) {
                            Some(Cond::Flags(ty, flags))
                        } else {
                            None // the rhs is not a literal, so we don't what it is
                        }
                    },
                    (None, Some(ty)) => {
                        if let Some(flags) = try!(literal_ty_to_flags(&linfo)) {
                            Some(Cond::Flags(ty, flags))
                        } else {
                            None
                        }
                    },
                    (_, _) => None,
                };

                // TODO when cond is None try to assert the type equivalence;
                // it is currently not implemented due to bad interaction with sub-literal types
                Ok((cond, SlotSeq::from(T::Boolean)))
            }

            Ex::Bin(ref l, Spanned { base: BinOp::And, .. }, ref r) => {
                let (lcond, lseq) = try!(self.collect_conds_from_exp(l));
                let (rcond, rseq) = try!(self.collect_conds_from_exp(r));

                let cond = match (lcond, rcond) {
                    (None, cond) | (cond, None) => cond,
                    (Some(Cond::Flags(lty, lflags)), Some(Cond::Flags(rty, rflags))) => {
                        let identical = &*lty.borrow() as *const _ == &*rty.borrow() as *const _;
                        if identical {
                            Some(Cond::Flags(lty, lflags & rflags))
                        } else {
                            Some(Cond::And(Box::new(Cond::Flags(lty, lflags)),
                                           Box::new(Cond::Flags(rty, rflags))))
                        }
                    },
                    (Some(lcond), Some(rcond)) => {
                        Some(Cond::And(Box::new(lcond), Box::new(rcond)))
                    },
                };

                let linfo = lseq.into_first();
                let rinfo = rseq.into_first();
                let info = try!(self.check_bin_op(&linfo, BinOp::And, &rinfo));
                Ok((cond, SlotSeq::from_slot(info)))
            }

            Ex::Bin(ref l, Spanned { base: BinOp::Or, .. }, ref r) => {
                let (lcond, lseq) = try!(self.collect_conds_from_exp(l));
                let (rcond, rseq) = try!(self.collect_conds_from_exp(r));

                let cond = match (lcond, rcond) {
                    (None, cond) | (cond, None) => cond,
                    (Some(Cond::Flags(lty, lflags)), Some(Cond::Flags(rty, rflags))) => {
                        let identical = &*lty.borrow() as *const _ == &*rty.borrow() as *const _;
                        if identical {
                            Some(Cond::Flags(lty, lflags | rflags))
                        } else {
                            Some(Cond::Or(Box::new(Cond::Flags(lty, lflags)),
                                          Box::new(Cond::Flags(rty, rflags))))
                        }
                    },
                    (Some(lcond), Some(rcond)) => {
                        Some(Cond::Or(Box::new(lcond), Box::new(rcond)))
                    },
                };

                let linfo = lseq.into_first();
                let rinfo = rseq.into_first();
                let info = try!(self.check_bin_op(&linfo, BinOp::Or, &rinfo));
                Ok((cond, SlotSeq::from_slot(info)))
            }

            _ => {
                let seq = try!(self.visit_exp(exp));
                let info = seq.clone().into_first();
                Ok((Some(Cond::Flags(info, T_TRUTHY)), seq))
            }
        }
    }

    fn assert_cond(&mut self, cond: Cond, negated: bool) -> CheckResult<()> {
        debug!("asserting condition {:?} (negated {:?})", cond, negated);
        match cond {
            Cond::Flags(info, flags) => {
                let flags = if negated { !flags } else { flags };
                let filtered = {
                    let slot = info.borrow();
                    let ty = slot.unlift().clone().into_send();
                    try!(ty.filter_by_flags(flags, self.context()))
                };
                // TODO: won't work well with `var` slots
                try!(info.accept(&Slot::just(filtered), self.context()));
            }

            Cond::And(lcond, rcond) => {
                if !negated {
                    try!(self.assert_cond(*lcond, negated));
                    try!(self.assert_cond(*rcond, negated));
                }
            }

            Cond::Or(lcond, rcond) => {
                if negated {
                    try!(self.assert_cond(*lcond, negated));
                    try!(self.assert_cond(*rcond, negated));
                }
            }

            Cond::Not(cond) => {
                try!(self.assert_cond(*cond, !negated));
            }
        }

        Ok(())
    }
}

