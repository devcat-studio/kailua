use std::i32;
use std::cmp;
use std::ops::{Deref, DerefMut};
use std::borrow::Cow;
use take_mut::take;

use kailua_diag::{Span, Spanned, WithLoc, Report, Reporter};
use kailua_syntax::{Name, Var, M, TypeSpec, Sig, Ex, Exp, UnOp, BinOp, NameScope};
use kailua_syntax::{St, Stmt, Block};
use diag::CheckResult;
use ty::{T, TySeq, Lattice, TypeContext, Tables, Function, Functions, TyWithNil};
use ty::{F, Slot, SlotSeq, SlotWithNil, Builtin};
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

struct ScopedChecker<'chk, 'envr: 'chk, 'env: 'envr>(&'chk mut Checker<'envr, 'env>);

impl<'chk, 'envr, 'env> Deref for ScopedChecker<'chk, 'envr, 'env> {
    type Target = &'chk mut Checker<'envr, 'env>;
    fn deref(&self) -> &&'chk mut Checker<'envr, 'env> { &self.0 }
}

impl<'chk, 'envr, 'env> DerefMut for ScopedChecker<'chk, 'envr, 'env> {
    fn deref_mut(&mut self) -> &mut &'chk mut Checker<'envr, 'env> { &mut self.0 }
}

impl<'chk, 'envr, 'env> Drop for ScopedChecker<'chk, 'envr, 'env> {
    fn drop(&mut self) { self.0.env.leave(); }
}

// conditions out of boolean expression, used for assertion and branch typing
#[derive(Clone, Debug)]
enum Cond {
    Flags(Spanned<Slot>, Flags),
    And(Box<Cond>, Box<Cond>),
    Or(Box<Cond>, Box<Cond>),
    Not(Box<Cond>),
}

fn literal_ty_to_flags(info: &Slot) -> CheckResult<Option<Flags>> {
    if let Some(s) = info.unlift().as_string() {
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
    if let Some(s) = info.unlift().as_string() {
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

pub struct Checker<'envr, 'env: 'envr> {
    env: &'envr mut Env<'env>,
    opts: &'env mut Options,
}

impl<'envr, 'env> Checker<'envr, 'env> {
    pub fn new(env: &'envr mut Env<'env>, opts: &'env mut Options) -> Checker<'envr, 'env> {
        Checker { env: env, opts: opts }
    }

    fn context(&mut self) -> &mut Context {
        self.env.context()
    }

    fn scoped<'chk>(&'chk mut self, scope: Scope) -> ScopedChecker<'chk, 'envr, 'env> {
        self.env.enter(scope);
        ScopedChecker(self)
    }

    fn dummy_slotseq(&self) -> Spanned<SlotSeq> {
        let seq = SlotSeq { head: Vec::new(), tail: Some(SlotWithNil::from(T::Dynamic)) };
        seq.with_loc(Span::dummy())
    }

    fn check_un_op(&mut self, op: UnOp, info: &Spanned<Slot>) -> CheckResult<Slot> {
        macro_rules! check_op {
            ($sub:expr) => {
                if let Err(e) = $sub {
                    return Err(format!("tried to apply {} operator to {:-?}: {}",
                                       op.symbol(), info, e));
                }
            }
        }

        match op {
            UnOp::Neg => {
                check_op!(info.assert_sub(&T::number(), self.context()));

                // it is possible to be more accurate here.
                // e.g. if ty = `v1 \/ integer` and it is known that `v1 <: integer`,
                // then `ty <: integer` and we can safely return an integer.
                // we don't do that though, since probing for <: risks the instantiation.
                if info.get_tvar().is_none() && info.flags() == T_INTEGER {
                    Ok(Slot::just(T::integer()))
                } else {
                    Ok(Slot::just(T::number()))
                }
            }

            UnOp::Not => {
                Ok(Slot::just(T::Boolean))
            }

            UnOp::Len => {
                check_op!(info.assert_sub(&(T::table() | T::string()), self.context()));
                Ok(Slot::just(T::integer()))
            }
        }
    }

    fn check_bin_op(&mut self, lhs: &Spanned<Slot>, op: BinOp,
                    rhs: &Spanned<Slot>) -> CheckResult<Slot> {
        macro_rules! check_op {
            ($sub:expr) => {
                if let Err(e) = $sub {
                    return Err(format!("tried to apply {} operator to {:-?} and {:-?}: {}",
                                       op.symbol(), lhs, rhs, e));
                }
            }
        }

        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Mod => {
                // ? + integer = integer, ? + number = ? + ? = number, number + integer = number
                // see UnOp::Neg comment for the rationale
                let lflags = self.env.get_type_bounds(&lhs.unlift()).1;
                let rflags = self.env.get_type_bounds(&rhs.unlift()).1;
                if lflags.is_integral() && rflags.is_integral() &&
                   !(lflags.is_dynamic() && rflags.is_dynamic()) {
                    // we are definitely sure that it will be an integer
                    check_op!(lhs.assert_sub(&T::integer(), self.context()));
                    check_op!(rhs.assert_sub(&T::integer(), self.context()));
                    Ok(Slot::just(T::integer()))
                } else {
                    // technically speaking they coerce strings to numbers,
                    // but that's probably not what you want
                    check_op!(lhs.assert_sub(&T::number(), self.context()));
                    check_op!(rhs.assert_sub(&T::number(), self.context()));
                    Ok(Slot::just(T::number()))
                }
            }

            BinOp::Div | BinOp::Pow => {
                check_op!(lhs.assert_sub(&T::number(), self.context()));
                check_op!(rhs.assert_sub(&T::number(), self.context()));
                Ok(Slot::just(T::number()))
            }

            BinOp::Cat => {
                let stringy = T::number() | T::string();
                check_op!(lhs.assert_sub(&stringy, self.context()));
                check_op!(rhs.assert_sub(&stringy, self.context()));
                Ok(Slot::just(T::string()))
            }

            BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                // this is a hard bit, since both operands should be either numbers or strings
                // but not both (as comparing number against string simply chokes).
                // due to the presence of union types, we cannot represent this constraint
                // with subtyping; equality does not work either, as there are large non-trivial
                // subsets of numbers and strings. for now we try to detect if operands are
                // definitely numbers or strings, and bail out when it is not possible.

                let lflags = lhs.flags();
                let rflags = rhs.flags();

                // filter any non-strings and non-numbers
                // avoid using assert_sub here, it is not accurate enough
                if !lflags.is_stringy() || !rflags.is_stringy() {
                    return Err(format!("tried to apply {} operator to {:-?} and {:-?}",
                                       op.symbol(), lhs, rhs));
                }

                let lnum = lflags.intersects(T_NUMBER);
                let lstr = lflags.intersects(T_STRING);
                let rnum = rflags.intersects(T_NUMBER);
                let rstr = rflags.intersects(T_STRING);
                if (lnum && lstr) || (rnum && rstr) {
                    Err(format!("operands {:-?} and {:-?} to operator {} should be \
                                 numbers or strings but not both", lhs, rhs, op.symbol()))
                } else if (lnum && rstr) || (lstr && rnum) {
                    Err(format!("operands {:-?} and {:-?} to operator {} should be \
                                 both numbers or both strings", lhs, rhs, op.symbol()))
                } else if lnum || rnum { // operands are definitely numbers
                    check_op!(lhs.assert_sub(&T::number(), self.context()));
                    check_op!(rhs.assert_sub(&T::number(), self.context()));
                    Ok(Slot::just(T::Boolean))
                } else if lstr || rstr { // operands are definitely strings
                    check_op!(lhs.assert_sub(&T::string(), self.context()));
                    check_op!(rhs.assert_sub(&T::string(), self.context()));
                    Ok(Slot::just(T::Boolean))
                } else { // XXX
                    Err(format!("cannot deduce if operands {:-?} and {:-?} to operator {} are \
                                 either numbers or strings", lhs, rhs, op.symbol()))
                }
            }

            BinOp::Eq | BinOp::Ne => { // works for any types
                Ok(Slot::just(T::Boolean))
            }

            BinOp::And => {
                if lhs.is_dynamic() || rhs.is_dynamic() {
                    return Ok(Slot::just(T::Dynamic));
                }

                if lhs.get_tvar().is_none() {
                    // True and T => T
                    if lhs.is_truthy() { return Ok(Slot::just(rhs.unlift().clone())); }
                    // False and T => False
                    if lhs.is_falsy() { return Ok(Slot::just(lhs.unlift().clone())); }
                }
                // unsure, both can be possible
                Ok(Slot::just((*lhs.unlift()).union(&*rhs.unlift(), self.context())))
            }

            BinOp::Or => {
                if lhs.is_dynamic() || rhs.is_dynamic() {
                    return Ok(Slot::just(T::Dynamic));
                }

                if lhs.get_tvar().is_none() {
                    // True or T => True
                    if lhs.is_truthy() { return Ok(Slot::just(lhs.unlift().clone())); }
                    // False or T => T
                    if lhs.is_falsy() { return Ok(Slot::just(rhs.unlift().clone())); }
                }
                // unsure, both can be possible
                Ok(Slot::just((*lhs.unlift()).union(&*rhs.unlift(), self.context())))
            }
        }
    }

    fn check_callable(&mut self, func: &T, args: &TySeq) -> CheckResult<TySeq> {
        let func = try!(self.env.resolve_exact_type(&func).ok_or_else(|| {
            format!("the type {:?} is callable but not known enough to call", func)
        }));

        // check if func.args :> args
        let mut returns = match *func.get_functions().unwrap() {
            Functions::Simple(ref f) => {
                if let Err(e) = args.assert_sub(&f.args, self.context()) {
                    return Err(format!("failed to call {:?}: {}", func, e));
                }
                f.returns.clone()
            }
            Functions::Multi(ref _funcs) => { // TODO
                return Err(format!("overloaded function {:?} is not yet supported", func));
            }
            Functions::All => {
                return Err(format!("cannot call {:?} without downcasting", func));
            }
        };

        // XXX hack to allow generics for some significant functions
        if func.builtin() == Some(Builtin::GenericPairs) {
            (|| {
                let mut args = args.to_owned();
                let tab = match self.env.resolve_exact_type(args.ensure_at(0)) {
                    Some(tab) => tab,
                    None => return,
                };

                if !tab.is_tabular() { return; }
                let map = match tab.get_tables() {
                    Some(tab) => tab.clone().lift_to_map(self.context()),
                    None => return,
                };

                if let Tables::Map(k, v) = map {
                    // fix `returns` in place
                    let knil = (*k).clone() | T::Nil;
                    let v = v.into_slot_without_nil();
                    let v = v.unlift().clone().into_send();
                    *returns.ensure_at_mut(0) = Box::new(T::func(Function {
                        args: TySeq { head: vec![Box::new(tab.clone()), k.clone()], tail: None },
                        returns: TySeq { head: vec![Box::new(knil), Box::new(v)], tail: None },
                    }));
                    *returns.ensure_at_mut(1) = Box::new(tab);
                    *returns.ensure_at_mut(2) = k;
                }
            })();
        }

        Ok(returns)
    }

    fn check_index(&mut self, ety0: &Spanned<Slot>, kty0: &Spanned<Slot>,
                   lval: bool) -> CheckResult<Option<Slot>> {
        let ety = ety0.unlift().clone();
        let kty = kty0.unlift().clone();

        if !self.env.get_type_bounds(&ety).1.is_tabular() {
            return Err(format!("tried to index the non-table {:?}", ety));
        }
        let ety = if let Some(ety) = self.env.resolve_exact_type(&ety) {
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
            let flex = if linear {
                F::VarOrCurrently(context.gen_mark())
            } else {
                F::Var
            };
            Slot::new(flex, tvar)
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
            match (ety.get_tables(), lval) {
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
                    check!(vslot.adapt(ety0.flex(), self.context()));
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
        let intkey = self.env.get_type_bounds(&kty).1.is_integral();
        match (ety.get_tables(), lval) {
            // possible! this occurs when the ety was Dynamic.
            (None, _) => {
                let value = Slot::just(T::Dynamic);
                if lval { check!(value.adapt(ety0.flex(), self.context())); }
                Ok(Some(value))
            },

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

            (Some(&Tables::Array(ref value)), _) if intkey => {
                if lval {
                    check!(value.as_slot_without_nil().adapt(ety0.flex(), self.context()));
                }
                Ok(Some((*value).clone().into_slot()))
            },

            (Some(&Tables::Array(..)), false) =>
                Err(format!("cannot index an array {:?} with a non-integral index {:?}", ety, kty)),

            (Some(&Tables::Map(ref key, ref value)), false) => {
                check!(kty.assert_sub(key, self.context()));
                Ok(Some((*value).clone().into_slot()))
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
                if let Some(&Tables::Map(ref key, ref value)) = ety0.unlift().get_tables() {
                    check!(kty.assert_sub(key, self.context()));
                    check!(value.as_slot_without_nil().adapt(ety0.flex(), self.context()));
                    Ok(Some((*value).clone().into_slot()))
                } else {
                    unreachable!()
                }
            }
        }
    }

    pub fn visit(&mut self, chunk: &Spanned<Block>) -> CheckResult<()> {
        try!(self.visit_block(chunk));
        if self.env.can_continue() {
            Ok(())
        } else {
            // the report had recoverable error(s), but we now stop here
            Err(format!("stopped due to prior errors"))
        }
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
                let varinfos: Vec<_> = try!(vars.into_iter()
                                                .map(|varspec| self.visit_var_with_spec(varspec))
                                                .collect());
                let infos = try!(self.visit_explist(exps)).base;
                for (varinfo, info) in varinfos.into_iter().zip(infos.into_iter()) {
                    try!(varinfo.accept(&info, self.context()));
                }
                Ok(Exit::None)
            }

            St::Do(ref block) => self.visit_block(block),

            St::While(ref cond, ref block) => {
                let ty = try!(self.visit_exp(cond)).base.into_first();
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
                let ty = try!(self.visit_exp(cond)).base.into_first();
                if exit == Exit::None {
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
                    let ty = try!(self.visit_exp(cond)).base.into_first();
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
                let start = try!(self.visit_exp(start)).base.into_first();
                let end = try!(self.visit_exp(end)).base.into_first();
                let step = if let &Some(ref step) = step {
                    try!(self.visit_exp(step)).base.into_first()
                } else {
                    Slot::just(T::integer()) // to simplify the matter
                };

                // the similar logic is also present in check_bin_op
                let startflags = self.env.get_type_bounds(&start.unlift()).1;
                let endflags = self.env.get_type_bounds(&end.unlift()).1;
                let stepflags = self.env.get_type_bounds(&step.unlift()).1;
                let indty;
                if startflags.is_integral() && endflags.is_integral() && stepflags.is_integral() &&
                   !(startflags.is_dynamic() && endflags.is_dynamic() && stepflags.is_dynamic()) {
                    try!(start.assert_sub(&T::integer(), self.context()));
                    try!(end.assert_sub(&T::integer(), self.context()));
                    try!(step.assert_sub(&T::integer(), self.context()));
                    indty = T::integer();
                } else {
                    try!(start.assert_sub(&T::number(), self.context()));
                    try!(end.assert_sub(&T::number(), self.context()));
                    try!(step.assert_sub(&T::number(), self.context()));
                    indty = T::number();
                }

                let mut scope = self.scoped(Scope::new());
                scope.env.add_local_var(name, Slot::just(indty), true);
                let exit = try!(scope.visit_block(block));
                if exit >= Exit::Return { Ok(exit) } else { Ok(Exit::None) }
            }

            St::ForIn(ref names, ref exps, ref block) => {
                let Spanned { base: infos, span: expspan } = try!(self.visit_explist(exps));
                let mut infos = infos.into_iter();
                let func = infos.next().unwrap(); // iterator function
                let state = infos.next().unwrap(); // immutable state to the iterator
                let last = infos.next().unwrap(); // last value returned from the iterator

                // `func` is subject to similar constraints to `self.visit_func_call`
                let func = func.unlift();
                let indtys;
                if !self.env.get_type_bounds(&func).1.is_callable() {
                    try!(self.env.error(expspan, format!("The iterator returned a non-function \
                                                          {:?}", func)).done());
                    indtys = TySeq { head: vec![],
                                     tail: Some(Box::new(TyWithNil::from(T::Dynamic))) };
                } else if func.is_dynamic() {
                    // can't determine what func will return
                    indtys = TySeq { head: vec![],
                                     tail: Some(Box::new(TyWithNil::from(T::Dynamic))) };
                } else {
                    // last can be updated, so one should assume that its type can be much wider.
                    let indvar = T::TVar(self.context().gen_tvar());

                    // func <: function(state, last) -> (last, ...)
                    //
                    // note that this sets hard (and possibly tight) bounds to `indvar`.
                    // while the proper constraint solver should be able to deal with them
                    // in any order, our current half-baked solver requires them to be ordered
                    // in the decreasing order of size. since the initial `last` type is likely
                    // to be a subtype of function's own bounds, we assert function types first.
                    let state = state.unlift().clone().into_send();
                    let args = TySeq { head: vec![Box::new(state), Box::new(indvar.clone())],
                                       tail: None };
                    let mut returns = try!(self.check_callable(&func, &args));
                    try!(last.assert_sub(&indvar, self.context()));

                    // note that we ignore indvar here. it is only kept internally and
                    // not visible outside; returns is what we should assign to variables!
                    // we should still account for the fact that the first value cannot be nil.
                    take(returns.ensure_at_mut(0), |t| Box::new(t.without_nil()));

                    indtys = returns;
                }

                let mut scope = self.scoped(Scope::new());
                for (name, ty) in names.iter().zip(indtys.into_iter()) {
                    scope.env.add_local_var(name, Slot::new(F::Var, *ty), true);
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
                try!(T::TVar(funcv).assert_eq(&*functy.unlift(), self.context()));
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
                        let flex = match namespec.modf {
                            M::None => F::VarOrCurrently(env.context().gen_mark()),
                            M::Var => F::Var,
                            M::Const => F::Const,
                        };
                        env.add_local_var(&namespec.base.base, Slot::new(flex, ty), false);
                        env.assign_to_var(&namespec.base.base, info)
                    } else {
                        env.add_local_var(&namespec.base.base, info, true);
                        Ok(())
                    }
                };

                let infos = try!(self.visit_explist(exps)).base;
                for (namespec, info) in names.iter().zip(infos.into_iter()) {
                    try!(add_local_var(&mut self.env, namespec, info));
                }
                Ok(Exit::None)
            }

            St::Return(ref exps) => {
                let seq = try!(self.visit_explist(exps)).base;
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
                try!(self.env.context().open_library(name, self.opts));
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
                let flex = match kindm {
                    M::None => F::VarOrCurrently(self.context().gen_mark()),
                    M::Var => F::Var,
                    M::Const => F::Const,
                };
                let slot = Slot::new(flex, ty);
                match scope {
                    NameScope::Local => try!(self.env.assume_var(name, slot)),
                    NameScope::Global => try!(self.env.assume_global_var(name, slot)),
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
                let flex = match param.modf {
                    M::None | M::Var => F::Var,
                    M::Const => F::Const,
                };
                sty = Slot::new(flex, ty.clone());
            } else {
                let argv = scope.context().gen_tvar();
                ty = T::TVar(argv);
                sty = Slot::new(F::Var, T::TVar(argv));
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
                           varspec: &TypeSpec<Spanned<Var>>) -> CheckResult<Slot> {
        let specslot = if let Some(ref kind) = varspec.kind {
            let ty = try!(T::from(kind, &mut self.env));
            let flex = match varspec.modf {
                M::None => F::VarOrCurrently(self.context().gen_mark()),
                M::Var => F::Var,
                M::Const => F::Const,
            };
            Some(Slot::new(flex, ty))
        } else {
            None
        };

        match varspec.base.base {
            Var::Name(ref name) => {
                // may refer to the global variable yet to be defined!
                if let Some(info) = self.env.get_var(name).cloned() {
                    if let Some(ref slot) = specslot {
                        try!(info.accept(slot, self.context()));
                    }
                    Ok(info.to_owned())
                } else {
                    // we need a type to initialize the variable with.
                    // if we have the type spec, use it. otherwise use a type variable.
                    let slot = specslot.unwrap_or_else(|| {
                        Slot::new(F::Var, T::TVar(self.context().gen_tvar()))
                    });
                    try!(self.env.assign_to_var(name, slot.clone()));
                    Ok(slot)
                }
            },

            Var::Index(ref e, ref key) => {
                let ty = try!(self.visit_exp(e)).map(|seq| seq.into_first());
                let kty = try!(self.visit_exp(key)).map(|seq| seq.into_first());
                let slot = try!(self.check_index(&ty, &kty, true));
                // since we've requested a lvalue it would never return None
                Ok(slot.unwrap())
            },
        }
    }

    fn visit_func_call(&mut self, funcinfo: &Spanned<T>,
                       args: &[Spanned<Exp>]) -> CheckResult<SlotSeq> {
        if !self.env.get_type_bounds(funcinfo).1.is_callable() {
            return Err(format!("tried to call the non-function {:?}", funcinfo));
        }
        if funcinfo.is_dynamic() {
            return Ok(SlotSeq::from(T::Dynamic));
        }

        let argtys = try!(self.visit_explist(args));

        // handle builtins, which may return different things from the function signature
        match funcinfo.builtin() {
            // require("foo")
            Some(Builtin::Require) => {
                if args.len() < 1 {
                    return Err(format!("`require` needs at least one argument"));
                }

                if let Ex::Str(ref modname) = *args[0].base {
                    if let Some(slot) = try!(self.context().get_loaded_module(modname)) {
                        info!("requiring {:?} (cached)", modname);
                        return Ok(SlotSeq::from_slot(slot));
                    }
                    self.context().mark_module_as_loading(modname);

                    info!("requiring {:?}", modname);
                    let block = match self.opts.require_block(modname) {
                        Ok(block) => block,
                        Err(_) => {
                            try!(self.env.warn(args[0].span, "Cannot resolve the module name \
                                                              given to `require`").done());
                            return Ok(SlotSeq::from(T::All));
                        }
                    };
                    let mut env = Env::new(self.env.context());
                    {
                        let mut sub = Checker::new(&mut env, self.opts);
                        try!(sub.visit_block(&block));
                    }
                    return Ok(SlotSeq::from_slot(try!(env.return_from_module(modname))));
                } else {
                    return Ok(SlotSeq::from(T::All));
                }
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
                    let cond = Cond::Flags(argtys.head[0].clone().with_loc(args[0].span), flags);
                    try!(self.assert_cond(cond, false));
                }
            },

            _ => {}
        }

        let returns = try!(self.check_callable(funcinfo, &argtys.base.unlift()));
        Ok(SlotSeq::from_seq(returns))
    }

    fn visit_exp(&mut self, exp: &Spanned<Exp>) -> CheckResult<Spanned<SlotSeq>> {
        debug!("visiting exp {:?}", *exp);
        match *exp.base {
            Ex::Nil => Ok(SlotSeq::from(T::Nil).with_loc(exp)),
            Ex::False => Ok(SlotSeq::from(T::False).with_loc(exp)),
            Ex::True => Ok(SlotSeq::from(T::True).with_loc(exp)),
            Ex::Num(v) if v.floor() == v =>
                if i32::MIN as f64 <= v && v <= i32::MAX as f64 {
                    Ok(SlotSeq::from(T::int(v as i32)).with_loc(exp))
                } else {
                    Ok(SlotSeq::from(T::integer()).with_loc(exp))
                },
            Ex::Num(_) => Ok(SlotSeq::from(T::number()).with_loc(exp)),
            Ex::Str(ref s) => Ok(SlotSeq::from(T::str(s.to_owned())).with_loc(exp)),

            Ex::Varargs => {
                if let Some(vararg) = self.env.get_vararg() {
                    Ok(SlotSeq::from_seq(vararg.clone()).with_loc(exp))
                } else {
                    try!(self.env.error(exp, "Variadic arguments do not exist \
                                              in the innermost function").done());
                    Ok(self.dummy_slotseq())
                }
            },
            Ex::Var(ref name) => {
                if let Some(info) = self.env.get_var(name) {
                    Ok(SlotSeq::from_slot(info.clone()).with_loc(exp))
                } else {
                    try!(self.env.error(exp, format!("Global or local variable {:?} is \
                                                      not defined", *name)).done());
                    Ok(self.dummy_slotseq())
                }
            },

            Ex::Func(ref sig, ref block) => {
                Ok(SlotSeq::from_slot(try!(self.visit_func_body(None, sig, block))).with_loc(exp))
            },
            Ex::Table(ref fields) => {
                let mut tab = Tables::Empty;

                let mut len = 0;
                for (idx, &(ref key, ref value)) in fields.iter().enumerate() {
                    // if this is the last entry and no explicit index is set, splice the values
                    if idx == fields.len() - 1 && key.is_none() {
                        let vty = try!(self.visit_exp(value)).base;
                        for ty in &vty.head {
                            let ty = ty.unlift().clone();
                            len += 1;
                            tab = tab.insert(T::int(len), ty, self.context());
                        }
                        if let Some(ty) = vty.tail {
                            // a simple array is no longer sufficient now
                            let ty = ty.as_slot_without_nil().unlift().clone();
                            tab = tab.insert(T::integer(), ty, self.context());
                        }
                    } else {
                        let kty = if let Some(ref key) = *key {
                            let key = try!(self.visit_exp(key)).base.into_first();
                            let key = key.unlift();
                            key.clone().into_send()
                        } else {
                            len += 1;
                            T::int(len)
                        };

                        // update the table type according to new field
                        let vty = try!(self.visit_exp(value)).base.into_first();
                        let vty = vty.unlift().clone();
                        tab = tab.insert(kty, vty, self.context());
                    }
                }

                // if the table remains intact, it is an empty table
                Ok(SlotSeq::from(T::Tables(Cow::Owned(tab))).with_loc(exp))
            },

            Ex::FuncCall(ref func, ref args) => {
                let Spanned { base: funcseq, span } = try!(self.visit_exp(func));
                let funcinfo = funcseq.into_first();
                let funcinfo = funcinfo.unlift();
                let info = try!(self.visit_func_call(&funcinfo.to_ref().with_loc(span), args));
                Ok(info.with_loc(exp))
            },

            Ex::MethodCall(ref e, ref _method, ref args) => {
                let info = try!(self.visit_exp(e)).map(|seq| seq.into_first());
                if !info.unlift().is_tabular() {
                    try!(self.env.error(exp, format!("Tried to index a non-table type \
                                                      {:?}", info)).done());
                    return Ok(self.dummy_slotseq());
                }

                for arg in args {
                    try!(self.visit_exp(arg));
                }
                Ok(SlotSeq::from(T::Dynamic).with_loc(exp))
            },

            Ex::Index(ref e, ref key) => {
                let ty = try!(self.visit_exp(e)).map(|seq| seq.into_first());
                let kty = try!(self.visit_exp(key)).map(|seq| seq.into_first());
                if let Some(vinfo) = try!(self.check_index(&ty, &kty, false)) {
                    Ok(SlotSeq::from_slot(vinfo).with_loc(exp))
                } else {
                    Err(format!("cannot index {:?} with {:?}", ty, kty))
                }
            },

            Ex::Un(op, ref e) => {
                let info = try!(self.visit_exp(e)).map(|seq| seq.into_first());
                let info = try!(self.check_un_op(op.base, &info));
                Ok(SlotSeq::from_slot(info).with_loc(exp))
            },

            Ex::Bin(ref l, op, ref r) => {
                let lhs = try!(self.visit_exp(l)).map(|seq| seq.into_first());
                let rhs = try!(self.visit_exp(r)).map(|seq| seq.into_first());
                let info = try!(self.check_bin_op(&lhs, op.base, &rhs));
                Ok(SlotSeq::from_slot(info).with_loc(exp))
            },
        }
    }

    fn visit_explist(&mut self, exps: &[Spanned<Exp>]) -> CheckResult<Spanned<SlotSeq>> {
        let mut head = Vec::new();
        let mut last: Option<SlotSeq> = None;
        let mut span = Span::dummy();
        for exp in exps {
            if let Some(last) = last.take() {
                head.push(last.into_first());
            }
            let info = try!(self.visit_exp(exp));
            last = Some(info.base);
            span |= info.span;
        }

        let mut seq = SlotSeq { head: head, tail: None };
        if let Some(last) = last {
            seq.head.extend(last.head.into_iter());
            seq.tail = last.tail;
        }
        Ok(seq.with_loc(span))
    }

    fn collect_type_from_exp(&mut self, exp: &Spanned<Exp>)
            -> CheckResult<(Option<Spanned<Slot>>, Spanned<SlotSeq>)> {
        if let Ex::FuncCall(ref func, ref args) = *exp.base {
            let Spanned { base: funcseq, span: funcspan } = try!(self.visit_exp(func));
            let funcinfo = funcseq.into_first();
            let funcinfo = funcinfo.unlift();
            let typeofexp = if funcinfo.builtin() == Some(Builtin::Type) {
                // there should be a single argument there
                if args.len() != 1 {
                    try!(self.env.error(exp, format!("{:?} should be called with one argument, \
                                                      received {:?}", func, args)).done());
                    None
                } else {
                    let arg = try!(self.visit_exp(&args[0])).map(|seq| seq.into_first());
                    Some(arg)
                }
            } else {
                None
            };
            let seq = try!(self.visit_func_call(&funcinfo.to_ref().with_loc(funcspan), args));
            Ok((typeofexp, seq.with_loc(exp)))
        } else {
            let seq = try!(self.visit_exp(exp));
            Ok((None, seq))
        }
    }

    // similar to visit_exp but also tries to collect Cond
    fn collect_conds_from_exp(&mut self, exp: &Spanned<Exp>)
            -> CheckResult<(Option<Cond>, Spanned<SlotSeq>)> {
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
                let info = seq.map(|seq| seq.into_first());
                let info = try!(self.check_un_op(UnOp::Not, &info));
                Ok((cond, SlotSeq::from_slot(info).with_loc(exp)))
            }

            Ex::Bin(ref l, Spanned { base: BinOp::Eq, .. }, ref r) => {
                let (lty, linfo) = try!(self.collect_type_from_exp(l));
                let (rty, rinfo) = try!(self.collect_type_from_exp(r));

                let linfo = linfo.map(|seq| seq.into_first());
                let rinfo = rinfo.map(|seq| seq.into_first());

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
                Ok((cond, SlotSeq::from(T::Boolean).with_loc(exp)))
            }

            Ex::Bin(ref l, Spanned { base: BinOp::And, .. }, ref r) => {
                let (lcond, lseq) = try!(self.collect_conds_from_exp(l));
                let (rcond, rseq) = try!(self.collect_conds_from_exp(r));

                let cond = match (lcond, rcond) {
                    (None, cond) | (cond, None) => cond,
                    (Some(Cond::Flags(lty, lflags)), Some(Cond::Flags(rty, rflags))) => {
                        let identical = &*lty.unlift() as *const _ == &*rty.unlift() as *const _;
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

                let linfo = lseq.map(|seq| seq.into_first());
                let rinfo = rseq.map(|seq| seq.into_first());
                let info = try!(self.check_bin_op(&linfo, BinOp::And, &rinfo));
                Ok((cond, SlotSeq::from_slot(info).with_loc(exp)))
            }

            Ex::Bin(ref l, Spanned { base: BinOp::Or, .. }, ref r) => {
                let (lcond, lseq) = try!(self.collect_conds_from_exp(l));
                let (rcond, rseq) = try!(self.collect_conds_from_exp(r));

                let cond = match (lcond, rcond) {
                    (None, cond) | (cond, None) => cond,
                    (Some(Cond::Flags(lty, lflags)), Some(Cond::Flags(rty, rflags))) => {
                        let identical = &*lty.unlift() as *const _ == &*rty.unlift() as *const _;
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

                let linfo = lseq.map(|seq| seq.into_first());
                let rinfo = rseq.map(|seq| seq.into_first());
                let info = try!(self.check_bin_op(&linfo, BinOp::Or, &rinfo));
                Ok((cond, SlotSeq::from_slot(info).with_loc(exp)))
            }

            _ => {
                let seq = try!(self.visit_exp(exp));
                let info = seq.clone().map(|seq| seq.into_first());
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
                    let ty = info.unlift().clone().into_send();
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

