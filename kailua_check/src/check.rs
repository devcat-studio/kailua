use std::i32;
use std::cmp;
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::borrow::Cow;
use take_mut::take;

use kailua_diag::{Span, Spanned, WithLoc, Reporter};
use kailua_syntax::{Name, Var, M, TypeSpec, Sig, Ex, Exp, UnOp, BinOp, NameScope};
use kailua_syntax::{St, Stmt, Block};
use diag::CheckResult;
use ty::{T, TySeq, SpannedTySeq, Lattice, Displayed, Display, TypeContext};
use ty::{Tables, Function, Functions, TyWithNil};
use ty::{F, Slot, SlotSeq, SpannedSlotSeq, SlotWithNil, Builtin};
use ty::flags::*;
use env::{Env, Frame, Scope, Context};
use message as m;

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

pub struct Checker<'envr, 'env: 'envr> {
    env: &'envr mut Env<'env>,
}

impl<'envr, 'env> Checker<'envr, 'env> {
    pub fn new(env: &'envr mut Env<'env>) -> Checker<'envr, 'env> {
        Checker { env: env }
    }

    fn context(&mut self) -> &mut Context {
        self.env.context()
    }

    fn display<'a, 'c, T: Display>(&'c self, x: &'a T) -> Displayed<'a, 'c, T>
            where Displayed<'a, 'c, T>: fmt::Display {
        self.env.display(x)
    }

    fn scoped<'chk>(&'chk mut self, scope: Scope) -> ScopedChecker<'chk, 'envr, 'env> {
        self.env.enter(scope);
        ScopedChecker(self)
    }

    fn check_un_op(&mut self, op: UnOp, info: &Spanned<Slot>,
                   _expspan: Span) -> CheckResult<Slot> {
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

    fn check_bin_op(&mut self, lhs: &Spanned<Slot>, op: BinOp, rhs: &Spanned<Slot>,
                    expspan: Span) -> CheckResult<Slot> {
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

                // try to narrow them further. this operation is frequently used for
                // constructing larger (otherwise constant) literals.
                if let Some(lhs) = self.env.resolve_exact_type(&lhs.unlift())
                                           .and_then(|t| t.as_string().map(|s| s.to_owned())) {
                    if let Some(rhs) = self.env.resolve_exact_type(&rhs.unlift())
                                               .and_then(|t| t.as_string().map(|s| s.to_owned())) {
                        let mut s: Vec<u8> = lhs.into();
                        s.append(&mut rhs.into());
                        return Ok(Slot::just(T::str(s.into())));
                    }
                }

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
                    try!(self.env.error(expspan, m::WrongOperand { op: op.symbol(),
                                                                   lhs: self.display(lhs),
                                                                   rhs: self.display(rhs) })
                                 .done());
                    return Ok(Slot::just(T::Boolean));
                }

                let lnum = lflags.intersects(T_NUMBER);
                let lstr = lflags.intersects(T_STRING);
                let rnum = rflags.intersects(T_NUMBER);
                let rstr = rflags.intersects(T_STRING);
                if (lnum && lstr) || (rnum && rstr) {
                    if lnum && lstr {
                        try!(self.env.error(lhs,
                                            m::OperandIsBothNumOrStr { op: op.symbol(),
                                                                       operand: self.display(lhs) })
                                     .done());
                    }
                    if rnum && rstr {
                        try!(self.env.error(rhs,
                                            m::OperandIsBothNumOrStr { op: op.symbol(),
                                                                       operand: self.display(rhs) })
                                     .done());
                    }
                } else if (lnum && rstr) || (lstr && rnum) {
                    try!(self.env.error(expspan,
                                        m::OperandsAreNotBothNumOrStr { op: op.symbol(),
                                                                        lhs: self.display(lhs),
                                                                        rhs: self.display(rhs) })
                                 .done());
                } else if lnum || rnum { // operands are definitely numbers
                    check_op!(lhs.assert_sub(&T::number(), self.context()));
                    check_op!(rhs.assert_sub(&T::number(), self.context()));
                } else if lstr || rstr { // operands are definitely strings
                    check_op!(lhs.assert_sub(&T::string(), self.context()));
                    check_op!(rhs.assert_sub(&T::string(), self.context()));
                } else { // XXX
                    try!(self.env.error(expspan,
                                        m::CannotDeduceBothNumOrStr { op: op.symbol(),
                                                                      lhs: self.display(lhs),
                                                                      rhs: self.display(rhs) })
                                 .done());
                }
                Ok(Slot::just(T::Boolean))
            }

            BinOp::Eq | BinOp::Ne => { // works for any types
                Ok(Slot::just(T::Boolean))
            }

            BinOp::And => {
                if lhs.is_dynamic() || rhs.is_dynamic() {
                    return Ok(Slot::dummy());
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
                    return Ok(Slot::dummy());
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

    fn check_callable(&mut self, func: &Spanned<T>, args: &SpannedTySeq) -> CheckResult<TySeq> {
        let functy = if let Some(func) = self.env.resolve_exact_type(&func) {
            func
        } else {
            try!(self.env.error(func, m::CallToInexactType { func: self.display(func) }).done());
            return Ok(TySeq::dummy());
        };

        // check if func.args :> args
        let mut returns = match *functy.get_functions().unwrap() {
            Functions::Simple(ref f) => {
                let funcargs = f.args.clone().all_without_loc();
                if let Err(e) = args.assert_sub(&funcargs, self.context()) {
                    return Err(format!("failed to call {:?}: {}", func, e));
                }
                f.returns.clone()
            }
            Functions::Multi(ref _funcs) => { // TODO
                try!(self.env.error(func, m::CallToOverloadedFunc { func: self.display(func) })
                             .done());
                return Ok(TySeq::dummy());
            }
            Functions::All => {
                try!(self.env.error(func, m::CallToAnyFunc { func: self.display(func) }).done());
                return Ok(TySeq::dummy());
            }
        };

        // XXX hack to allow generics for some significant functions
        if functy.builtin() == Some(Builtin::GenericPairs) {
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

    fn check_index(&mut self, ety0: &Spanned<Slot>, kty0: &Spanned<Slot>, expspan: Span,
                   lval: bool) -> CheckResult<Option<Slot>> {
        let ety = ety0.unlift().clone();
        let kty = kty0.unlift().clone();

        if !self.env.get_type_bounds(&ety).1.is_tabular() {
            try!(self.env.error(ety0, m::IndexToNonTable { tab: self.display(ety0) }).done());
            return Ok(Some(Slot::dummy()));
        }
        let ety = if let Some(ety) = self.env.resolve_exact_type(&ety) {
            ety
        } else {
            try!(self.env.error(ety0, m::IndexToInexactType { tab: self.display(ety0) }).done());
            return Ok(Some(Slot::dummy()));
        };

        macro_rules! check {
            ($sub:expr) => {
                if $sub.is_err() {
                    try!(self.env.error(expspan, m::CannotIndex { tab: self.display(ety0),
                                                                  key: self.display(kty0) })
                                 .done());
                    return Ok(Some(Slot::dummy()));
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

        macro_rules! adapt_table {
            ($adapted:expr) => ({
                let adapted = T::Tables(Cow::Owned($adapted));
                if ety0.accept(&Slot::just(adapted.clone()), self.context()).is_err() {
                    try!(self.env.error(ety0,
                                        m::CannotAdaptTable { tab: self.display(ety0),
                                                              adapted: self.display(&adapted) })
                                 .note(kty0, m::AdaptTriggeredByIndex { key: self.display(kty0) })
                                 .done());
                    return Ok(Some(Slot::dummy()));
                }
            })
        }

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
                    adapt_table!(Tables::Fields(fields));
                    return Ok(Some(vslot));
                }

                (Some(&Tables::Fields(ref fields)), true) => {
                    let mut fields = fields.clone();
                    let vslot = fields.entry(litkey)
                                      .or_insert_with(|| new_slot(self.context(), true))
                                      .clone();
                    vslot.adapt(ety0.flex(), self.context());
                    adapt_table!(Tables::Fields(fields));
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
                // the flex should be retained
                if lval { value.adapt(ety0.flex(), self.context()); }
                Ok(Some(value))
            },

            (Some(&Tables::Fields(..)), false) => {
                try!(self.env.error(expspan,
                                    m::IndexToRecWithInexactStr { tab: self.display(ety0),
                                                                  key: self.display(&kty) })
                             .done());
                Ok(Some(Slot::dummy()))
            },

            (Some(&Tables::Empty), true) => {
                let vslot = new_slot(self.context(), false);
                let tab = if intkey {
                    Tables::Array(SlotWithNil::from_slot(vslot.clone()))
                } else {
                    Tables::Map(Box::new(kty.into_send()),
                                SlotWithNil::from_slot(vslot.clone()))
                };
                adapt_table!(tab);
                Ok(Some(vslot))
            },

            (Some(&Tables::Empty), false) => Ok(None),

            (Some(&Tables::Array(ref value)), _) if intkey => {
                if lval {
                    value.as_slot_without_nil().adapt(ety0.flex(), self.context());
                }
                Ok(Some((*value).clone().into_slot()))
            },

            (Some(&Tables::Array(..)), false) => {
                try!(self.env.error(expspan,
                                    m::IndexToArrayWithNonInt { tab: self.display(ety0),
                                                                key: self.display(&kty) })
                             .done());
                Ok(Some(Slot::dummy()))
            },

            (Some(&Tables::Map(ref key, ref value)), false) => {
                check!(kty.assert_sub(&**key, self.context()));
                Ok(Some((*value).clone().into_slot()))
            },

            (Some(&Tables::All), _) => {
                try!(self.env.error(ety0, m::IndexToAnyTable { tab: self.display(ety0) }).done());
                Ok(Some(Slot::dummy()))
            },

            // Fields with no keys resolved in compile time, Array with non-integral keys, Map
            (Some(tab), true) => {
                // we cannot keep the specialized table type, lift and adapt to a mapping
                let tab = tab.clone().lift_to_map(self.context());
                adapt_table!(tab);

                // reborrow ety0 to check against the final mapping type
                if let Some(&Tables::Map(ref key, ref value)) = ety0.unlift().get_tables() {
                    check!(kty.assert_sub(&**key, self.context()));
                    value.as_slot_without_nil().adapt(ety0.flex(), self.context());
                    Ok(Some((*value).clone().into_slot()))
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
                let varinfos: Vec<_> = try!(vars.iter()
                                                .map(|varspec| self.visit_var_with_spec(varspec))
                                                .collect());
                let infos = try!(self.visit_explist(exps));
                // unlike St::Local, do not tolerate the uninitialized variables
                for ((var, varinfo), info) in vars.iter().zip(varinfos.into_iter())
                                                         .zip(infos.into_iter_with_nil()) {
                    debug!("assigning {:?} to {:?} with type {:?}", info, var, varinfo);
                    if let Var::Name(ref name) = var.base.base {
                        self.env.mark_var_as_set(name);
                    }
                    try!(self.env.assign(&varinfo, &info));
                }
                Ok(Exit::None)
            }

            St::Do(ref block) => self.visit_block(block),

            St::While(ref cond, ref block) => {
                let ty = try!(self.visit_exp(cond)).into_first();
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
                let mut ignored_blocks = None; // or Some((first truthy cond span, blocks span))
                for &Spanned { base: (ref cond, ref block), span } in conds {
                    if let Some((_, ref mut blocks_span)) = ignored_blocks {
                        *blocks_span |= span;
                        continue;
                    }
                    let ty = try!(self.visit_exp(cond)).into_first();
                    if ty.is_truthy() {
                        ignored_blocks = Some((cond.span, Span::dummy()));
                        exit = exit.or(try!(self.visit_block(block)));
                    } else if ty.is_falsy() {
                        try!(self.env.warn(span, m::IgnoredIfCase {})
                                     .note(cond, m::IfCaseWithFalseyCond {})
                                     .done());
                    } else {
                        exit = exit.or(try!(self.visit_block(block))); // TODO scope merger
                    }
                }

                if let &Some(ref block) = lastblock {
                    if let Some((_, ref mut blocks_span)) = ignored_blocks {
                        *blocks_span |= block.span;
                    } else {
                        exit = exit.or(try!(self.visit_block(block))); // TODO scope merger
                    }
                } else {
                    if ignored_blocks.is_none() {
                        exit = Exit::None;
                    }
                }

                if let Some((truthy_span, blocks_span)) = ignored_blocks {
                    if blocks_span.is_dummy() {
                        try!(self.env.warn(truthy_span, m::IfCaseWithTruthyCond {}).done());
                    } else {
                        try!(self.env.warn(blocks_span, m::IgnoredIfCase {})
                                     .note(truthy_span, m::IfCaseWithTruthyCond {})
                                     .done());
                    }
                }

                Ok(exit)
            }

            St::For(ref name, ref start, ref end, ref step, ref block) => {
                let start = try!(self.visit_exp(start)).into_first();
                let end = try!(self.visit_exp(end)).into_first();
                let step = if let &Some(ref step) = step {
                    try!(self.visit_exp(step)).into_first()
                } else {
                    Slot::just(T::integer()).without_loc() // to simplify the matter
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
                try!(scope.env.add_local_var(name, Slot::just(indty).without_loc(), true, true));
                let exit = try!(scope.visit_block(block));
                if exit >= Exit::Return { Ok(exit) } else { Ok(Exit::None) }
            }

            St::ForIn(ref names, ref exps, ref block) => {
                let infos = try!(self.visit_explist(exps));
                let expspan = infos.all_span();
                let mut infos = infos.into_iter_with_nil();
                let func = infos.next().unwrap(); // iterator function
                let state = infos.next().unwrap(); // immutable state to the iterator
                let last = infos.next().unwrap(); // last value returned from the iterator

                // `func` is subject to similar constraints to `self.visit_func_call`
                let func = func.map(|t| t.unlift().clone().into_send());
                let indtys;
                if !self.env.get_type_bounds(&func).1.is_callable() {
                    try!(self.env.error(expspan, m::NonFuncIterator { iter: self.display(&func) })
                                 .done());
                    indtys = TySeq::dummy();
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
                    let state = state.map(|t| Box::new(t.unlift().clone().into_send()));
                    let args = SpannedTySeq { head: vec![state,
                                                         Box::new(indvar.clone()).without_loc()],
                                              tail: None };
                    let mut returns = try!(self.check_callable(&func.with_loc(expspan), &args));
                    try!(last.assert_sub(&indvar, self.context()));

                    // note that we ignore indvar here. it is only kept internally and
                    // not visible outside; returns is what we should assign to variables!
                    // we should still account for the fact that the first value cannot be nil.
                    take(returns.ensure_at_mut(0), |t| Box::new(t.without_nil()));

                    indtys = returns;
                }

                let mut scope = self.scoped(Scope::new());
                for (name, ty) in names.iter().zip(indtys.into_iter_with_nil()) {
                    try!(scope.env.add_local_var(name, Slot::new(F::Var, *ty).without_loc(),
                                                 true, true));
                }
                let exit = try!(scope.visit_block(block));
                if exit >= Exit::Return { Ok(exit) } else { Ok(Exit::None) }
            }

            St::FuncDecl(scope, ref name, ref sig, ref block) => {
                // `name` itself is available to the inner scope
                let funcv = self.context().gen_tvar();
                let info = Slot::just(T::TVar(funcv)).with_loc(stmt);
                match scope {
                    NameScope::Local => try!(self.env.add_local_var(name, info, true, true)),
                    NameScope::Global => try!(self.env.assign_to_var(name, info)),
                }
                let functy = try!(self.visit_func_body(None, sig, block));
                try!(T::TVar(funcv).assert_eq(&*functy.unlift(), self.context()));
                Ok(Exit::None)
            }

            St::MethodDecl(ref names, selfparam, ref sig, ref block) => {
                // TODO verify names
                let selfinfo = match selfparam {
                    Some(_) => Some(Slot::just(T::Dynamic).without_loc()),
                    None => None,
                };
                try!(self.visit_func_body(selfinfo, sig, block));
                Ok(Exit::None)
            }

            St::Local(ref names, ref exps) => {
                let add_local_var = |env: &mut Env,
                                     namespec: &TypeSpec<Spanned<Name>>,
                                     info: Option<Spanned<Slot>>| -> CheckResult<()> {
                    let initinfo = if let Some(ref kind) = namespec.kind {
                        let ty = try!(T::from(kind, env));
                        let flex = match namespec.modf {
                            M::None => F::VarOrCurrently(env.context().gen_mark()),
                            M::Var => F::Var,
                            M::Const => F::Const,
                        };
                        Slot::new(flex, ty).with_loc(kind)
                    } else {
                        let flex = F::VarOrCurrently(env.context().gen_mark());
                        Slot::new(flex, T::Nil).without_loc()
                    };
                    try!(env.add_local_var(&namespec.base, initinfo, false, false));
                    if let Some(info) = info {
                        try!(env.assign_to_var(&namespec.base, info));
                    }
                    Ok(())
                };

                let infos = try!(self.visit_explist(exps));
                for (namespec, info) in names.iter().zip(infos.into_iter_with_none()) {
                    try!(add_local_var(&mut self.env, namespec, info));
                }
                Ok(Exit::None)
            }

            St::Return(ref exps) => {
                let seq = try!(self.visit_explist(exps));
                let seq = seq.unlift(); // XXX wait, is it safe?
                let (returns, returns_exact) = {
                    let frame = self.env.get_frame();
                    let returns = frame.returns.as_ref().map(|seq| seq.clone().all_without_loc());
                    (returns, frame.returns_exact)
                };
                if returns_exact {
                    try!(Some(seq).assert_sub(&returns, self.context()));
                } else {
                    // need to infer the return type
                    let returns = Some(seq).union(&returns, self.context());
                    self.env.get_frame_mut().returns = returns.map(|seq| seq.unspan());
                }
                Ok(Exit::Return)
            }

            St::Break => Ok(Exit::Break),

            St::KailuaOpen(ref name) => {
                let opts = self.env.opts().clone();
                try!(self.env.context().open_library(name, opts));
                Ok(Exit::None)
            }

            St::KailuaType(ref name, ref kind) => {
                let ty = Box::new(try!(T::from(kind, &mut self.env)));
                try!(self.env.define_type(name, ty));
                Ok(Exit::None)
            }

            St::KailuaAssume(scope, ref name, kindm, ref kind) => {
                let ty = try!(T::from(kind, &mut self.env));
                let flex = match kindm {
                    M::None => F::VarOrCurrently(self.context().gen_mark()),
                    M::Var => F::Var,
                    M::Const => F::Const,
                };
                let slot = Slot::new(flex, ty).with_loc(kind);
                match scope {
                    NameScope::Local => try!(self.env.assume_var(name, slot)),
                    NameScope::Global => try!(self.env.assume_global_var(name, slot)),
                }
                Ok(Exit::None)
            }
        }
    }

    fn visit_func_body(&mut self, selfinfo: Option<Spanned<Slot>>, sig: &Sig,
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
            try!(scope.env.add_local_var(&Name::from(&b"self"[..]).without_loc(), selfinfo,
                                         true, true));
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
            try!(scope.env.add_local_var(&param.base, sty.without_loc(), false, true));
            argshead.push(Box::new(ty));
        }
        let args = TySeq { head: argshead, tail: vatype };

        if let Exit::None = try!(scope.visit_block(block)) {
            // the last statement is an implicit return
            let ret = Box::new(St::Return(Vec::new())).without_loc();
            try!(scope.visit_stmt(&ret));
        }

        let returns = scope.env.get_frame_mut().returns.take().unwrap();
        Ok(Slot::just(T::func(Function { args: args, returns: returns })))
    }

    fn visit_var_with_spec(&mut self,
                           varspec: &TypeSpec<Spanned<Var>>) -> CheckResult<Spanned<Slot>> {
        let slot = match varspec.base.base {
            Var::Name(ref name) => {
                // may refer to the global variable yet to be defined!
                if let Some(def) = self.env.get_var(name).cloned() {
                    if let Some(ref kind) = varspec.kind {
                        try!(self.env.error(kind, m::CannotRedefineGlobalVar { name: &name.base })
                                     .done());
                    }
                    self.env.mark_var_as_set(name);
                    def.slot
                } else {
                    // we need a type to initialize the variable with.
                    // if we have the type spec, use it. otherwise use a type variable.
                    let slot = if let Some(ref kind) = varspec.kind {
                        let ty = try!(T::from(kind, &mut self.env));
                        let flex = match varspec.modf {
                            M::None => F::VarOrCurrently(self.context().gen_mark()),
                            M::Var => F::Var,
                            M::Const => F::Const,
                        };
                        Slot::new(flex, ty).with_loc(kind)
                    } else {
                        Slot::new(F::Var, T::TVar(self.context().gen_tvar())).without_loc()
                    };

                    try!(self.env.assign_to_var(name, slot.clone()));
                    slot.base
                }
            },

            Var::Index(ref e, ref key) => {
                let ty = try!(self.visit_exp(e)).into_first();
                let kty = try!(self.visit_exp(key)).into_first();
                let slot = try!(self.check_index(&ty, &kty, varspec.base.span, true));
                // since we've requested a lvalue it would never return None
                slot.unwrap()
            },
        };

        // overwrite the span, the spec is irrelevant outside
        Ok(slot.with_loc(&varspec.base))
    }

    fn visit_func_call(&mut self, funcinfo: &Spanned<T>, args: &[Spanned<Exp>],
                       expspan: Span) -> CheckResult<SlotSeq> {
        if !self.env.get_type_bounds(funcinfo).1.is_callable() {
            try!(self.env.error(funcinfo, m::CallToNonFunc { func: self.display(funcinfo) })
                         .done());
            return Ok(SlotSeq::dummy());
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
                    try!(self.env.error(expspan,
                                        m::BuiltinGivenLessArgs { name: "require", nargs: 1 })
                                 .done());
                    return Ok(SlotSeq::dummy());
                }

                if let Some(modname) = self.env.resolve_exact_type(&argtys.head[0].unlift())
                                               .and_then(|t| t.as_string().map(|s| s.to_owned())) {
                    if let Some(slot) = try!(self.context().get_loaded_module(&modname, expspan)) {
                        info!("requiring {:?} (cached)", modname);
                        return Ok(SlotSeq::from_slot(slot));
                    }
                    self.context().mark_module_as_loading(&modname, expspan);

                    info!("requiring {:?}", modname);
                    let opts = self.env.opts().clone();
                    let block = match opts.borrow_mut().require_block(&modname) {
                        Ok(block) => block,
                        Err(_) => {
                            try!(self.env.warn(&args[0], m::CannotResolveModName {}).done());
                            return Ok(SlotSeq::from(T::All));
                        }
                    };
                    let mut env = Env::new(self.env.context(), opts);
                    {
                        let mut sub = Checker::new(&mut env);
                        try!(sub.visit_block(&block));
                    }
                    return Ok(SlotSeq::from_slot(try!(env.return_from_module(&modname, expspan))));
                } else {
                    return Ok(SlotSeq::from(T::All));
                }
            },

            // assert(expr)
            Some(Builtin::Assert) => {
                if args.len() < 1 {
                    // TODO should display the true name
                    try!(self.env.error(expspan,
                                        m::BuiltinGivenLessArgs { name: "assert", nargs: 1 })
                                 .done());
                    return Ok(SlotSeq::dummy());
                }

                let (cond, _seq) = try!(self.collect_conds_from_exp(&args[0]));
                if let Some(cond) = cond {
                    try!(self.assert_cond(cond, false));
                }
            },

            // assert_not(expr)
            Some(Builtin::AssertNot) => {
                if args.len() < 1 {
                    // TODO should display the true name
                    try!(self.env.error(expspan,
                                        m::BuiltinGivenLessArgs { name: "assert-not", nargs: 1 })
                                 .done());
                    return Ok(SlotSeq::dummy());
                }

                let (cond, _seq) = try!(self.collect_conds_from_exp(&args[0]));
                if let Some(cond) = cond {
                    try!(self.assert_cond(cond, true));
                }
            },

            // assert_type(expr)
            Some(Builtin::AssertType) => {
                if args.len() < 2 {
                    // TODO should display the true name
                    try!(self.env.error(expspan,
                                        m::BuiltinGivenLessArgs { name: "assert-type", nargs: 2 })
                                 .done());
                    return Ok(SlotSeq::dummy());
                }

                if let Some(flags) = try!(self.ext_literal_ty_to_flags(&argtys.head[1])) {
                    let cond = Cond::Flags(argtys.head[0].clone(), flags);
                    try!(self.assert_cond(cond, false));
                }
            },

            _ => {}
        }

        let returns = try!(self.check_callable(funcinfo, &argtys.unlift()));
        Ok(SlotSeq::from_seq(returns))
    }

    fn visit_exp(&mut self, exp: &Spanned<Exp>) -> CheckResult<SpannedSlotSeq> {
        Ok(try!(self.visit_exp_(exp)).all_with_loc(exp))
    }

    fn visit_exp_(&mut self, exp: &Spanned<Exp>) -> CheckResult<SlotSeq> {
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
                    try!(self.env.error(exp, m::NoVarargs {}).done());
                    Ok(SlotSeq::dummy())
                }
            },
            Ex::Var(ref name) => {
                if let Some(def) = self.env.get_var(name).cloned() {
                    try!(self.env.ensure_var(name));
                    Ok(SlotSeq::from_slot(def.slot))
                } else {
                    try!(self.env.error(exp, m::NoVar { name: name }).done());
                    Ok(SlotSeq::dummy())
                }
            },

            Ex::Func(ref sig, ref block) => {
                let returns = try!(self.visit_func_body(None, sig, block));
                Ok(SlotSeq::from_slot(returns))
            },
            Ex::Table(ref fields) => {
                let mut tab = Tables::Empty;

                let mut len = 0;
                for (idx, &(ref key, ref value)) in fields.iter().enumerate() {
                    // if this is the last entry and no explicit index is set, splice the values
                    if idx == fields.len() - 1 && key.is_none() {
                        let vty = try!(self.visit_exp(value));
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
                            let key = try!(self.visit_exp(key)).into_first();
                            let key = key.unlift();
                            key.clone().into_send()
                        } else {
                            len += 1;
                            T::int(len)
                        };

                        // update the table type according to new field
                        let vty = try!(self.visit_exp(value)).into_first();
                        let vty = vty.unlift().clone();
                        tab = tab.insert(kty, vty, self.context());
                    }
                }

                // if the table remains intact, it is an empty table
                Ok(SlotSeq::from(T::Tables(Cow::Owned(tab))))
            },

            Ex::FuncCall(ref func, ref args) => {
                let funcinfo = try!(self.visit_exp(func)).into_first();
                let funcinfo = funcinfo.map(|t| t.unlift().clone().into_send());
                self.visit_func_call(&funcinfo, args, exp.span)
            },

            Ex::MethodCall(ref e, ref _method, ref args) => {
                let info = try!(self.visit_exp(e)).into_first();
                if !info.unlift().is_tabular() {
                    try!(self.env.error(exp, m::IndexToNonTable { tab: self.display(&info) })
                                 .done());
                    return Ok(SlotSeq::dummy());
                }

                for arg in args {
                    try!(self.visit_exp(arg));
                }
                Ok(SlotSeq::from(T::Dynamic))
            },

            Ex::Index(ref e, ref key) => {
                let ty = try!(self.visit_exp(e)).into_first();
                let kty = try!(self.visit_exp(key)).into_first();
                if let Some(vinfo) = try!(self.check_index(&ty, &kty, exp.span, false)) {
                    Ok(SlotSeq::from_slot(vinfo))
                } else {
                    try!(self.env.error(exp, m::CannotIndex { tab: self.display(&ty),
                                                              key: self.display(&kty) })
                                 .done());
                    Ok(SlotSeq::dummy())
                }
            },

            Ex::Un(op, ref e) => {
                let info = try!(self.visit_exp(e)).into_first();
                let info = try!(self.check_un_op(op.base, &info, exp.span));
                Ok(SlotSeq::from_slot(info))
            },

            Ex::Bin(ref l, op, ref r) => {
                let lhs = try!(self.visit_exp(l)).into_first();
                let rhs = try!(self.visit_exp(r)).into_first();
                let info = try!(self.check_bin_op(&lhs, op.base, &rhs, exp.span));
                Ok(SlotSeq::from_slot(info))
            },
        }
    }

    fn visit_explist(&mut self, exps: &[Spanned<Exp>]) -> CheckResult<SpannedSlotSeq> {
        let mut head = Vec::new();
        let mut last: Option<SpannedSlotSeq> = None;
        for exp in exps {
            if let Some(last) = last.take() {
                head.push(last.into_first());
            }
            let info = try!(self.visit_exp(exp));
            last = Some(info);
        }

        let mut seq = SpannedSlotSeq { head: head, tail: None };
        if let Some(last) = last {
            seq.head.extend(last.head.into_iter());
            seq.tail = last.tail;
        }
        Ok(seq)
    }

    fn collect_type_from_exp(&mut self, exp: &Spanned<Exp>)
            -> CheckResult<(Option<Spanned<Slot>>, SpannedSlotSeq)> {
        if let Ex::FuncCall(ref func, ref args) = *exp.base {
            let funcseq = try!(self.visit_exp(func));
            let funcspan = funcseq.all_span();
            let funcinfo = funcseq.into_first();
            let funcinfo = funcinfo.unlift();
            let typeofexp = if funcinfo.builtin() == Some(Builtin::Type) {
                // there should be a single argument there
                if args.len() != 1 {
                    try!(self.env.error(exp, m::BuiltinGivenLessArgs { name: "type", nargs: 1 })
                                 .done());
                    None
                } else {
                    Some(try!(self.visit_exp(&args[0])).into_first())
                }
            } else {
                None
            };
            let seq = try!(self.visit_func_call(&funcinfo.to_ref().with_loc(funcspan),
                                                args, exp.span));
            Ok((typeofexp, seq.all_with_loc(exp)))
        } else {
            let seq = try!(self.visit_exp(exp));
            Ok((None, seq))
        }
    }

    // similar to visit_exp but also tries to collect Cond
    fn collect_conds_from_exp(&mut self, exp: &Spanned<Exp>)
            -> CheckResult<(Option<Cond>, SpannedSlotSeq)> {
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
                let info = try!(self.check_un_op(UnOp::Not, &info, exp.span));
                Ok((cond, SpannedSlotSeq::from_slot(info.with_loc(exp))))
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
                        if let Some(flags) = try!(self.literal_ty_to_flags(&rinfo)) {
                            Some(Cond::Flags(ty, flags))
                        } else {
                            None // the rhs is not a literal, so we don't what it is
                        }
                    },
                    (None, Some(ty)) => {
                        if let Some(flags) = try!(self.literal_ty_to_flags(&linfo)) {
                            Some(Cond::Flags(ty, flags))
                        } else {
                            None
                        }
                    },
                    (_, _) => None,
                };

                // TODO when cond is None try to assert the type equivalence;
                // it is currently not implemented due to bad interaction with sub-literal types
                Ok((cond, SpannedSlotSeq::from(T::Boolean.with_loc(exp))))
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

                let linfo = lseq.into_first();
                let rinfo = rseq.into_first();
                let info = try!(self.check_bin_op(&linfo, BinOp::And, &rinfo, exp.span));
                Ok((cond, SpannedSlotSeq::from_slot(info.with_loc(exp))))
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

                let linfo = lseq.into_first();
                let rinfo = rseq.into_first();
                let info = try!(self.check_bin_op(&linfo, BinOp::Or, &rinfo, exp.span));
                Ok((cond, SpannedSlotSeq::from_slot(info.with_loc(exp))))
            }

            _ => {
                let seq = try!(self.visit_exp(exp));
                let info = seq.into_first();
                // XXX should detect non-local slots and reject them!
                // probably we can do that via proper weakening, but who knows.
                Ok((Some(Cond::Flags(info.clone(), T_TRUTHY)), SpannedSlotSeq::from_slot(info)))
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
                try!(Slot::accept(&info, &Slot::just(filtered).without_loc(), self.context()));
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

    fn literal_ty_to_flags(&self, info: &Spanned<Slot>) -> CheckResult<Option<Flags>> {
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
                _ => {
                    try!(self.env.error(info, m::UnknownLiteralTypeName {}).done());
                    return Ok(None);
                }
            };
            Ok(Some(flags))
        } else {
            Ok(None)
        }
    }

    // AssertType built-in accepts more strings than Type
    fn ext_literal_ty_to_flags(&self, info: &Spanned<Slot>) -> CheckResult<Option<Flags>> {
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
                _ => {
                    try!(self.env.error(info, m::UnknownLiteralTypeName {}).done());
                    return Ok(None);
                }
            };
            Ok(Some(nilflags | flags))
        } else {
            Ok(None)
        }
    }
}

