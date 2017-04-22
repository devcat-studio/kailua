use std::i32;
use std::cmp;
use std::ops;
use std::borrow::Cow;
use std::collections::HashMap;
use take_mut::take;

use kailua_env::{Span, Spanned, WithLoc};
use kailua_diag::{self, Result, Report, Reporter};
use kailua_syntax::{Str, Name};
use kailua_syntax::ast::{self, NameRef, Var, TypeSpec, Kind, Sig, Ex, Exp, UnOp, BinOp};
use kailua_syntax::ast::{SelfParam, TypeScope, Args, St, Stmt, Block, K, Attr, M, MM, Varargs};
use diag::{TypeReport, TypeReportHint, TypeReportMore};
use ty::{Displayed, Display};
use ty::{Dyn, Nil, T, Ty, TySeq, SpannedTySeq, Lattice, Union, Dummy, TypeContext};
use ty::{Key, Tables, Function, Functions};
use ty::{F, Slot, SlotSeq, SpannedSlotSeq, Tag, Class, ClassId};
use ty::flags::*;
use env::{Env, Returns, Frame, Scope, Context, Types, SlotSpec};
use message as m;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Exit {
    None = 0,   // keeps going
    Break = 1,  // exits the current loop
    Return = 2, // exits the current function
    Stop = 3,   // never executes further statements (infinite loop or error)
}

impl Exit {
    fn loop_boundary(&self, normal_exit: Exit) -> Exit {
        match *self {
            Exit::None => normal_exit,
            Exit::Break => Exit::None,
            Exit::Return => Exit::Return,
            Exit::Stop => Exit::Stop,
        }
    }
}

// `exit1 | exit2` merges exits from two independent execution flows
impl ops::BitOr for Exit {
    type Output = Exit;
    fn bitor(self, other: Exit) -> Exit { cmp::min(self, other) }
}

impl ops::BitOrAssign for Exit {
    fn bitor_assign(&mut self, other: Exit) { *self = *self | other }
}

// `exit1 & exit2` merges exits from two sequential execution flows
impl ops::BitAnd for Exit {
    type Output = Exit;
    fn bitand(self, other: Exit) -> Exit { cmp::max(self, other) }
}

impl ops::BitAndAssign for Exit {
    fn bitand_assign(&mut self, other: Exit) { *self = *self & other }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum ExprExit {
    None = 0,
    Stop = 3,
    StopInside = 7, // indicates that the diverging expr is not the top-level (should warn)
}

#[derive(Clone, Debug)]
struct Exitable<T>(ExprExit, T);

impl ExprExit {
    // this is used to check if a single expression contains both converging and
    // diverging subexpressions; otherwise `with*` or `then` should be used.
    fn collide(self, other: ExprExit) -> ExprExit {
        match (self, other) {
            (ExprExit::None, ExprExit::None) => ExprExit::None,
            (ExprExit::Stop, ExprExit::Stop) => ExprExit::Stop,
            (_, _) => ExprExit::StopInside,
        }
    }

    fn with<T>(self, base: T) -> Exitable<T> {
        Exitable(self, base)
    }

    fn with_dummy<T: Dummy>(self) -> Exitable<T> {
        Exitable(self, T::dummy())
    }

    fn with_diverging<T: Dummy>(self) -> Exitable<T> {
        Exitable(cmp::max(self, ExprExit::Stop), T::dummy())
    }

    fn then<T>(self, next: Exitable<T>) -> Exitable<T> {
        Exitable(cmp::max(self, next.0), next.1)
    }

    fn to_stmt(self, span: Span, report: &Report) -> Result<Exit> {
        match self {
            ExprExit::None => Ok(Exit::None),
            ExprExit::Stop => Ok(Exit::Stop),
            ExprExit::StopInside => {
                report.warn(span, m::DivergingInExpr {}).done()?;
                Ok(Exit::Stop)
            },
        }
    }
}

impl<T> Exitable<T> {
    fn new(base: T) -> Exitable<T> {
        Exitable(ExprExit::None, base)
    }

    fn dummy() -> Exitable<T> where T: Dummy {
        Exitable(ExprExit::None, T::dummy())
    }

    fn diverging() -> Exitable<T> where T: Dummy {
        Exitable(ExprExit::Stop, T::dummy())
    }

    fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Exitable<U> {
        Exitable(self.0, f(self.1))
    }
}

struct ScopedChecker<'chk, 'inp: 'chk, 'envr: 'chk, 'env: 'envr, R: 'env + Report> {
    checker: &'chk mut Checker<'inp, 'envr, 'env, R>
}

impl<'chk, 'inp, 'envr, 'env, R: Report> ops::Deref for ScopedChecker<'chk, 'inp, 'envr, 'env, R> {
    type Target = &'chk mut Checker<'inp, 'envr, 'env, R>;
    fn deref(&self) -> &Self::Target { &self.checker }
}

impl<'chk, 'inp, 'envr, 'env, R: Report>
    ops::DerefMut for ScopedChecker<'chk, 'inp, 'envr, 'env, R>
{
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.checker }
}

impl<'chk, 'inp, 'envr, 'env, R: Report> Drop for ScopedChecker<'chk, 'inp, 'envr, 'env, R> {
    fn drop(&mut self) { self.checker.env.leave(); }
}

// conditions out of boolean expression, used for assertion and branch typing
#[derive(Clone, Debug)]
enum Cond {
    Flags(Spanned<Slot>, Flags),
    And(Box<Cond>, Box<Cond>),
    Or(Box<Cond>, Box<Cond>),
    Not(Box<Cond>),
}

#[derive(Copy, Clone, Debug)]
enum Bool {
    Unknown,
    Truthy,
    Falsy,
}

#[derive(Clone, Debug)]
enum Index {
    Missing, // field not found, creation not requested
    Created(Slot), // field not found, creation requested
    Found(Slot), // field found or the table is a map or vector
}

impl Index {
    fn dummy() -> Index {
        Index::Found(Slot::dummy())
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum NoCheck {
    User, // user-requested
    Module, // implied by module indexing
}

#[derive(Clone, Debug)]
struct Lvalue {
    found: bool,
    slot: Spanned<Slot>,
}

#[derive(Clone, Debug)]
struct PendingFuncBody<'inp> {
    tag: Option<Tag>,
    selfparam: Option<(&'inp Spanned<SelfParam>, Slot)>,
    sig: &'inp Sig,
    block: &'inp Spanned<Vec<Spanned<Stmt>>>,
    declspan: Span,
}

#[derive(Clone, Debug)]
struct PendingModule<'inp> {
    slot: Slot,
    func_bodies: Vec<PendingFuncBody<'inp>>,
}

impl<'inp> PendingModule<'inp> {
    fn new(slot: Slot) -> PendingModule<'inp> {
        PendingModule { slot: slot, func_bodies: Vec::new() }
    }
}

pub struct Checker<'inp, 'envr, 'env: 'envr, R: 'env> {
    env: &'envr mut Env<'env, R>,
    pending_modules: Vec<HashMap<*const Ty, PendingModule<'inp>>>,
}

impl<'inp, 'envr, 'env, R: Report> Checker<'inp, 'envr, 'env, R> {
    pub fn new(env: &'envr mut Env<'env, R>) -> Checker<'inp, 'envr, 'env, R> {
        Checker { env: env, pending_modules: Vec::new() }
    }

    fn types(&mut self) -> &mut Types {
        self.env.types()
    }

    fn context(&mut self) -> &mut Context<R> {
        self.env.context()
    }

    fn display<'a, 'c, T: Display>(&'c self, x: &'a T) -> Displayed<'a, T, &'c TypeContext> {
        self.env.display(x)
    }

    fn scoped<'chk>(&'chk mut self, scope: Scope) -> ScopedChecker<'chk, 'inp, 'envr, 'env, R> {
        self.env.enter(scope);
        ScopedChecker { checker: self }
    }

    // XXX in general reachability checking should continue when the type variable get resolved
    fn check_bool(&self, seq: TySeq) -> Bool {
        let empty_seq = seq.head.is_empty();
        let ty = seq.into_first();
        let (lb, ub) = self.env.get_type_bounds(&ty);
        if lb.is_truthy() {
            // caveat: an empty sequence _can_ result in nil and should not warn
            // (into_first does not distinguish this)
            if empty_seq {
                Bool::Unknown
            } else {
                Bool::Truthy
            }
        } else if ub == T_NONE || ub.is_falsy() {
            // this is tricky because a resolved implicit nil should also be caught.
            // however we don't want the unresolved type variable to be considered falsy.
            Bool::Falsy
        } else {
            Bool::Unknown
        }
    }

    fn check_un_op(&mut self, op: UnOp, info: &Spanned<Slot>, expspan: Span) -> Result<Slot> {
        let finalize = |r: TypeReport, checker: &mut Checker<R>| {
            checker.env.error(expspan,
                              m::WrongUnaryOperand { op: op.symbol(),
                                                     ty: checker.display(info) })
                       .report_types(r, TypeReportHint::None)
                       .done()
        };

        macro_rules! assert_sub {
            ($lhs:expr, $rhs:expr) => {
                match $lhs.assert_sub($rhs, self.types()) {
                    Ok(()) => {}
                    Err(r) => { finalize(r, self)?; }
                }
            }
        }

        match op {
            UnOp::Neg => {
                assert_sub!(&info, &T::Number);

                // it is possible to be more accurate here.
                // e.g. if ty = `v1 \/ integer` and it is known that `v1 <: integer`,
                // then `ty <: integer` and we can safely return an integer.
                // we don't do that though, since probing for <: risks the instantiation.
                if info.get_tvar().is_none() && info.flags() == T_INTEGER {
                    Ok(Slot::just(Ty::new(T::Integer)))
                } else {
                    Ok(Slot::just(Ty::new(T::Number)))
                }
            }

            UnOp::Not => {
                Ok(Slot::just(Ty::new(T::Boolean)))
            }

            UnOp::Len => {
                assert_sub!(&info, &(T::table() | T::String));
                Ok(Slot::just(Ty::new(T::Integer)))
            }
        }
    }

    fn check_bin_op(&mut self, lhs: &Spanned<Slot>, op: BinOp, rhs: &Spanned<Slot>,
                    expspan: Span) -> Result<Slot> {
        let finalize = |r: TypeReport, checker: &mut Checker<R>| {
            checker.env.error(expspan,
                              m::WrongBinaryOperands { op: op.symbol(),
                                                       lhs: checker.display(lhs),
                                                       rhs: checker.display(rhs) })
                       .report_types(r, TypeReportHint::None)
                       .done()
        };

        let finalize2 = |r1: Option<TypeReport>, r2: Option<TypeReport>, checker: &mut Checker<R>| {
            let mut more = checker.env.error(expspan,
                                             m::WrongBinaryOperands { op: op.symbol(),
                                                                      lhs: checker.display(lhs),
                                                                      rhs: checker.display(rhs) });
            if let Some(r) = r1 {
                more = more.report_types(r, TypeReportHint::None);
            }
            if let Some(r) = r2 {
                more = more.report_types(r, TypeReportHint::None);
            }
            more.done()
        };

        macro_rules! assert_sub_both {
            ($lhs1:expr, $lhs2:expr, $rhs:expr) => {
                match ($lhs1.assert_sub($rhs, self.types()),
                       $lhs2.assert_sub($rhs, self.types())) {
                    (Ok(()), Ok(())) => {}
                    (r1, r2) => { finalize2(r1.err(), r2.err(), self)?; }
                }
            }
        }

        macro_rules! union {
            ($lhs:expr, $rhs:expr, $explicit:expr) => {
                match $lhs.union($rhs, $explicit, self.types()) {
                    Ok(out) => out,
                    Err(r) => { finalize(r, self)?; Ty::dummy() },
                }
            }
        }

        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Mod => {
                // ? + integer = integer, ? + number = ? + ? = number, number + integer = number
                // see UnOp::Neg comment for the rationale
                let lflags = self.env.get_type_bounds(&lhs.unlift()).1;
                let rflags = self.env.get_type_bounds(&rhs.unlift()).1;
                let numty;
                if lflags.is_integral() && rflags.is_integral() &&
                   !(lflags.is_dynamic() && rflags.is_dynamic()) {
                    // we are definitely sure that it will be an integer
                    numty = T::Integer;
                } else {
                    // technically speaking they coerce strings to numbers,
                    // but that's probably not what you want
                    numty = T::Number;
                }
                assert_sub_both!(lhs, rhs, &numty);
                Ok(Slot::just(Ty::new(numty)))
            }

            BinOp::Div | BinOp::Pow => {
                assert_sub_both!(lhs, rhs, &T::Number);
                Ok(Slot::just(Ty::new(T::Number)))
            }

            BinOp::Cat => {
                assert_sub_both!(lhs, rhs, &(T::Number | T::String));

                // try to narrow them further. this operation is frequently used for
                // constructing larger (otherwise constant) literals.
                if let Some(lhs) = self.env.resolve_exact_type(&lhs.unlift())
                                           .and_then(|t| t.as_string().map(|s| s.to_owned())) {
                    if let Some(rhs) = self.env.resolve_exact_type(&rhs.unlift())
                                               .and_then(|t| t.as_string().map(|s| s.to_owned())) {
                        let mut s = lhs.into_bytes().into_vec();
                        s.append(&mut rhs.into_bytes().into_vec());
                        return Ok(Slot::just(Ty::new(T::Str(Cow::Owned(Str::from(s))))));
                    }
                }

                Ok(Slot::just(Ty::new(T::String)))
            }

            BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                // this is a hard bit, since both operands should be either numbers or strings
                // but not both (as comparing number against string simply chokes).
                // due to the presence of union types, we cannot represent this constraint
                // with subtyping; equality does not work either, as there are large non-trivial
                // subsets of numbers and strings. for now we try to detect if operands are
                // definitely numbers or strings, and bail out when it is not possible.

                let lflags = self.env.get_type_bounds(&lhs.unlift()).1;
                let rflags = self.env.get_type_bounds(&rhs.unlift()).1;

                // filter dynamics
                if lflags.is_dynamic() || rflags.is_dynamic() {
                    return Ok(Slot::just(Ty::new(T::Boolean)));
                }

                // filter any non-strings and non-numbers
                // avoid using assert_sub here, it is not accurate enough
                if !lflags.is_stringy() || !rflags.is_stringy() {
                    self.env.error(expspan, m::WrongBinaryOperands { op: op.symbol(),
                                                                     lhs: self.display(lhs),
                                                                     rhs: self.display(rhs) })
                            .done()?;
                    return Ok(Slot::just(Ty::new(T::Boolean)));
                }

                let lnum = lflags.intersects(T_NUMBER);
                let lstr = lflags.intersects(T_STRING);
                let rnum = rflags.intersects(T_NUMBER);
                let rstr = rflags.intersects(T_STRING);
                if (lnum && lstr) || (rnum && rstr) {
                    if lnum && lstr {
                        self.env.error(lhs,
                                       m::OperandIsBothNumOrStr { op: op.symbol(),
                                                                  operand: self.display(lhs) })
                                .done()?;
                    }
                    if rnum && rstr {
                        self.env.error(rhs,
                                       m::OperandIsBothNumOrStr { op: op.symbol(),
                                                                  operand: self.display(rhs) })
                                .done()?;
                    }
                } else if (lnum && rstr) || (lstr && rnum) {
                    self.env.error(expspan,
                                   m::OperandsAreNotBothNumOrStr { op: op.symbol(),
                                                                   lhs: self.display(lhs),
                                                                   rhs: self.display(rhs) })
                            .done()?;
                } else if lnum || rnum { // operands are definitely numbers
                    assert_sub_both!(lhs, rhs, &T::Number);
                } else if lstr || rstr { // operands are definitely strings
                    assert_sub_both!(lhs, rhs, &T::String);
                } else { // XXX
                    self.env.error(expspan,
                                   m::CannotDeduceBothNumOrStr { op: op.symbol(),
                                                                 lhs: self.display(lhs),
                                                                 rhs: self.display(rhs) })
                            .done()?;
                }
                Ok(Slot::just(Ty::new(T::Boolean)))
            }

            BinOp::Eq | BinOp::Ne => { // works for any types
                Ok(Slot::just(Ty::new(T::Boolean)))
            }

            BinOp::And => {
                if let Some(dyn) = Dyn::or(lhs.get_dynamic(), rhs.get_dynamic()) {
                    return Ok(Slot::just(Ty::new(T::Dynamic(dyn))));
                }

                match self.check_bool(TySeq::from(lhs.unlift().clone())) {
                    // True and T => T
                    Bool::Truthy => Ok(Slot::just(rhs.unlift().clone())),
                    // False and T => False
                    Bool::Falsy => Ok(Slot::just(lhs.unlift().clone())),
                    // unsure, both can be possible (but truthy types in lhs are not kept)
                    Bool::Unknown => {
                        let falsy_lhs = lhs.as_ref().map(|t| t.unlift().clone().falsy());
                        let rhs = rhs.as_ref().map(|t| t.unlift().clone());
                        Ok(Slot::just(union!(&falsy_lhs, &rhs, false)))
                    }
                }
            }

            BinOp::Or => {
                if let Some(dyn) = Dyn::or(lhs.get_dynamic(), rhs.get_dynamic()) {
                    return Ok(Slot::just(Ty::new(T::Dynamic(dyn))));
                }

                match self.check_bool(TySeq::from(lhs.unlift().clone())) {
                    // True or T => True
                    Bool::Truthy => Ok(Slot::just(lhs.unlift().clone())),
                    // False or T => T
                    Bool::Falsy => Ok(Slot::just(rhs.unlift().clone())),
                    // unsure, both can be possible (but falsy types in lhs are not kept)
                    Bool::Unknown => {
                        let truthy_lhs = lhs.as_ref().map(|t| t.unlift().clone().truthy());
                        let rhs = rhs.as_ref().map(|t| t.unlift().clone());
                        Ok(Slot::just(union!(&truthy_lhs, &rhs, false)))
                    }
                }
            }
        }
    }

    fn check_callable(&mut self, func: &Spanned<Ty>, args: &SpannedTySeq,
                      methodcall: bool) -> Result<Exitable<TySeq>> {
        debug!("checking if {:?} can be called with {:?} ({})",
               func, args, if methodcall { "method call" } else { "func call" });

        // check for `[internal constructor] <#x>`
        // (should be done before the resolution, as it is likely to fail!)
        if func.tag() == Some(Tag::Constructor) {
            self.env.error(func, m::CannotCallCtor {}).done()?;
            return Ok(Exitable::dummy());
        }

        // visit_func_call also does this, but check_callable can be called in the other way
        let functy = if let Some(func) = self.env.resolve_exact_type(&func) {
            func
        } else {
            self.env.error(func, m::CallToInexactType { func: self.display(func) }).done()?;
            return Ok(Exitable::dummy());
        };

        // check if generalize(func.args) :> args and gather generalize(func.returns)
        let mut returns = match *functy.get_functions().unwrap() {
            Functions::Simple(ref f) => {
                let generalize_tyseq = |seq: &TySeq, ctx: &mut TypeContext| {
                    let head = seq.head.iter().map(|t| t.clone().generalize(ctx)).collect();
                    let tail = seq.tail.as_ref().map(|t| t.clone().generalize(ctx));
                    TySeq { head: head, tail: tail }
                };

                let funcargs = generalize_tyseq(&f.args, self.types()).all_with_loc(func);
                if let Err(r) = args.assert_sub(&funcargs, self.types()) {
                    let hint = if methodcall {
                        TypeReportHint::MethodArgs
                    } else {
                        TypeReportHint::FuncArgs
                    };
                    self.env.error(func, m::CallToWrongType { func: self.display(func) })
                            .report_types(r, hint)
                            .done()?;
                    return Ok(Exitable::dummy());
                }

                if let Some(ref returns) = f.returns {
                    generalize_tyseq(returns, self.types())
                } else {
                    return Ok(Exitable::diverging());
                }
            },

            Functions::All => {
                self.env.error(func, m::CallToAnyFunc { func: self.display(func) }).done()?;
                return Ok(Exitable::dummy());
            },
        };

        // XXX hack to allow generics for some significant functions
        if functy.tag() == Some(Tag::GenericPairs) {
            (|| {
                let mut args = args.to_owned();
                let tab = match self.env.resolve_exact_type(args.ensure_at(0)) {
                    Some(tab) => tab,
                    None => return,
                };

                let (mut k, v) = if let Some(dyn) = tab.get_dynamic() {
                    // the table itself is dynamic, return the same dynamic types
                    (Ty::new(T::Dynamic(dyn)),
                     Slot::new(F::Dynamic(dyn), Ty::new(T::Dynamic(dyn))))
                } else if tab.is_tabular() {
                    let int_or_n = || {
                        Ty::new(T::Integer | T::Str(Cow::Owned(Str::from(b"n"[..].to_owned()))))
                    };

                    match tab.clone().unwrap() {
                        // map<k, v> -> (k, v)
                        T::Tables(Cow::Owned(Tables::Map(k, v))) =>
                            (k, v.with_nil()),
                        T::Tables(Cow::Borrowed(&Tables::Map(ref k, ref v))) =>
                            (k.clone(), v.clone().with_nil()),

                        // vector<v> -> (integer, v)
                        T::Tables(Cow::Owned(Tables::Array(v))) =>
                            (Ty::new(T::Integer), v.with_nil()),
                        T::Tables(Cow::Borrowed(&Tables::Array(ref v))) =>
                            (Ty::new(T::Integer), v.clone().with_nil()),

                        // vector<v> & {n: integer} -> (integer | "n", v)
                        T::Tables(Cow::Owned(Tables::ArrayN(v))) =>
                            (int_or_n(), v.with_nil()),
                        T::Tables(Cow::Borrowed(&Tables::ArrayN(ref v))) =>
                            (int_or_n(), v.clone().with_nil()),

                        _ => return,
                    }
                } else {
                    return
                };

                // replace the key with the third return type if it's not any
                // (this is primarily to detect ipairs)
                {
                    let third = returns.ensure_at(2);
                    match **third {
                        T::All => {}
                        _ => { k = third.clone(); }
                    }
                }

                // fix `returns` in place
                let knil = k.clone().with_nil();
                let v = v.unlift().clone().without_nil();
                *returns.ensure_at_mut(0) = Ty::new(T::func(Function {
                    args: TySeq { head: vec![tab.clone(), k.clone()], tail: None },
                    argnames: Vec::new(),
                    returns: Some(TySeq { head: vec![knil, v], tail: None }),
                }));
                *returns.ensure_at_mut(1) = tab;
                *returns.ensure_at_mut(2) = k;
            })();
        }

        Ok(Exitable::new(returns))
    }

    fn cannot_index(&self, span: Span, tab: &Slot, key: &Slot) -> Result<()> {
        // use a special message when the table is a record and key is a string literal
        match (tab.unlift().get_tables(), key.unlift().as_string()) {
            (Some(&Tables::Fields(_)), Some(s)) => {
                self.env.error(span,
                               m::CannotIndexWithStr { tab: self.display(tab), key: s })
                        .done()
            },
            _ => {
                self.env.error(span,
                               m::CannotIndex { tab: self.display(tab), key: self.display(key) })
                        .done()
            },
        }
    }

    // common routine for check_{l,r}val_index
    // when lval is true, the field is created as needed (otherwise it's an error)
    // when lval is false, the missing field is returned as Index::Missing
    fn check_index_common(&mut self, ety0: &Spanned<Slot>, kty0: &Spanned<Slot>, expspan: Span,
                          lval: bool) -> Result<Index> {
        debug!("indexing {:?} with {:?} as an {}-value", ety0, kty0, if lval { "l" } else { "r" });

        let mut ety0: Cow<Spanned<Slot>> = Cow::Borrowed(ety0);

        let ety = ety0.unlift().clone();
        let kty = kty0.unlift().clone();

        let (_, flags) = self.env.get_type_bounds(&ety);
        if !flags.is_tabular() {
            self.env.error(&*ety0, m::IndexToNonTable { tab: self.display(&*ety0) }).done()?;
            return Ok(Index::dummy());
        }

        // if lval is true, we are supposed to update the table and
        // therefore the table should have an appropriate flex
        if lval && ety0.accept_in_place(self.types()).is_err() {
            self.env.error(&*ety0, m::CannotUpdate { tab: self.display(&*ety0) }).done()?;
            return Ok(Index::dummy());
        }

        let new_slot = |flex: F, types: &mut Types| {
            // we don't yet know the exact value type, so generate a new type variable
            let tvar = T::TVar(types.gen_tvar());
            Slot::new(flex, Ty::new(tvar))
        };

        let mut ety = if let Some(ety) = self.env.resolve_exact_type(&ety) {
            ety
        } else {
            self.env.error(&*ety0,
                           m::IndexToInexactType { tab: self.display(&*ety0) }).done()?;
            return Ok(Index::dummy());
        };

        let clsinfo = match *ety {
            T::Class(Class::Prototype(cid)) => Some((cid, true)),
            T::Class(Class::Instance(cid)) => Some((cid, false)),
            T::Union(ref u) if !u.classes.is_empty() => {
                // the union is assumed to be simplified, so even if `u.classes` has one type
                // it is mixed with other types so it cannot be indexed.
                self.env.error(&*ety0, m::IndexToUnknownClass { cls: self.display(&*ety0) })
                        .done()?;
                return Ok(Index::dummy());
            },
            _ => None,
        };

        // nominal types cannot be indexed with non-compile-time values
        // (in some sense, nominal types are isomorphic to records)
        if let Some((cid, proto)) = clsinfo {
            let litkey =
                if let Some(key) = kty.as_integer() {
                    key.into()
                } else if let Some(key) = kty.as_string() {
                    key.into()
                } else {
                    self.env.error(expspan,
                                   m::IndexToClassWithUnknown { cls: self.display(&*ety0),
                                                                key: self.display(&kty) })
                            .done()?;
                    return Ok(Index::dummy());
                };

            // this "template" is used to make a reconstructed record type for indexing.
            // we can in principle reconstruct the record type out of that,
            // but we need to record any change back to the class definition
            // so we duplicate the core logic here.
            macro_rules! fields {
                ($x:ident) => (
                    self.env.context().get_class_fields_mut(cid).expect("invalid ClassId").$x
                )
            }

            if lval {
                // l-values. there are strong restrictions over class prototypes and instances.

                let (vslot, new) = if proto {
                    debug!("assigning to a field {:?} of the class prototype of {:?}", litkey, cid);

                    if litkey == &b"init"[..] {
                        // this method is special, and should be the first method defined ever
                        // it cannot be replaced later, so the duplicate check is skipped
                        if !fields!(class_ty).is_empty() {
                            // since `init` should be the first method ever defined,
                            // non-empty class_ty should always contain `init`.
                            self.env.error(expspan, m::CannotRedefineCtor {}).done()?;
                            return Ok(Index::dummy());
                        }

                        // should have a [constructor] tag to create a `new` method
                        let ty = T::TVar(self.types().gen_tvar());
                        let slot = Slot::new(F::Var, Ty::new(ty).with_tag(Tag::Constructor));
                        fields!(class_ty).insert(litkey, slot.clone());
                        (slot, true)
                    } else if litkey == &b"new"[..] {
                        // `new` is handled from the assignment (it cannot be copied from `init`
                        // because it initially starts as as a type variable, i.e. unknown)
                        // see also `Env::create_new_method_from_init`
                        self.env.error(expspan, m::ReservedNewMethod {}).done()?;
                        return Ok(Index::dummy());
                    } else if let Some(v) = fields!(class_ty).get(&litkey).cloned() {
                        // for other methods, it should be used as is...
                        (v, false)
                    } else {
                        // ...or created only when the `init` method is available.
                        if fields!(class_ty).is_empty() {
                            self.env.error(expspan, m::CannotDefineMethodsWithoutCtor {}).done()?;
                            return Ok(Index::dummy());
                        }

                        // prototypes always use Var slots; it is in principle append-only.
                        let slot = new_slot(F::Var, self.types());
                        fields!(class_ty).insert(litkey, slot.clone());
                        (slot, true)
                    }
                } else {
                    debug!("assigning to a field {:?} of the class instance of {:?}", litkey, cid);

                    if let Some(v) = fields!(instance_ty).get(&litkey).cloned() {
                        // existing fields can be used as is
                        (v, false)
                    } else if ety.tag() != Some(Tag::Constructible) {
                        // otherwise, only the constructor can add new fields
                        self.env.error(expspan, m::CannotAddFieldsToInstance {}).done()?;
                        return Ok(Index::dummy());
                    } else {
                        // the constructor (checked earlier) can add slots to instances.
                        let slot = new_slot(F::Var, self.types());
                        fields!(instance_ty).insert(litkey, slot.clone());
                        (slot, true)
                    }
                };

                vslot.adapt(ety0.flex(), self.types());
                if new {
                    return Ok(Index::Created(vslot));
                } else {
                    return Ok(Index::Found(vslot));
                }
            } else {
                // r-values. we just pick the method from the template.
                trace!("indexing to a field {:?} of the class {} of {:?}",
                       litkey, if proto { "prototype" } else { "instance" }, cid);

                let fields = self.env.context().get_class_fields_mut(cid).expect("invalid ClassId");
                // TODO should we re-adapt methods?
                if !proto {
                    // instance fields have a precedence over class fields
                    if let Some(info) = fields.instance_ty.get(&litkey).map(|v| (*v).clone()) {
                        return Ok(Index::Found(info));
                    }
                }
                if let Some(info) = fields.class_ty.get(&litkey).map(|v| (*v).clone()) {
                    return Ok(Index::Found(info));
                } else {
                    return Ok(Index::Missing);
                }
            }
        }

        // if ety is a string, we go through the previously defined string metatable
        if !flags.is_dynamic() && flags.intersects(T_STRING) {
            if flags.intersects(!T_STRING) {
                self.env.error(&*ety0,
                               m::IndexedTypeIsBothTableOrStr { indexed: self.display(&*ety0) })
                        .done()?;
                return Ok(Index::dummy());
            }

            if let Some(meta) = self.env.get_string_meta() {
                // receives the same span to the original string expression
                ety0 = Cow::Owned(meta.base.clone().with_loc(ety0.span));

                // still possible that the string metatable itself is not fully resolved (!)
                if let Some(ety_) = self.env.resolve_exact_type(&ety0.unlift()) {
                    // now the metatable should be a table proper
                    if !ety_.is_dynamic() && ety_.flags().intersects(!T_TABLE) {
                        self.env.error(&*ety0, m::NonTableStringMeta {})
                                .note(meta.span, m::PreviousStringMeta {})
                                .done()?;
                        return Ok(Index::dummy());
                    }

                    ety = ety_;
                } else {
                    self.env.error(&*ety0, m::IndexToInexactType { tab: self.display(&*ety0) })
                            .done()?;
                    return Ok(Index::dummy());
                }
            } else {
                self.env.error(&*ety0, m::UndefinedStringMeta {}).done()?;
                return Ok(Index::dummy());
            }
        }

        // this also handles the case where the string metatable itself is dynamic
        if let Some(dyn) = flags.get_dynamic() {
            let value = Slot::just(Ty::new(T::Dynamic(dyn)));
            // the flex should be retained
            if lval { value.adapt(ety0.flex(), self.types()); }
            return Ok(Index::Found(value));
        }

        macro_rules! check {
            ($sub:expr) => {
                match $sub {
                    Ok(v) => v,
                    Err(_) => {
                        self.cannot_index(expspan, &ety0, kty0)?;
                        return Ok(Index::dummy());
                    }
                }
            }
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
        let had_litkey = litkey.is_some();
        if let Some(litkey) = litkey {
            match ety.get_tables() {
                Some(&Tables::Fields(ref rvar)) => {
                    // find a field in the rvar
                    let mut vslot = None;
                    let _ = self.env.context().list_rvar_fields(rvar.clone(), &mut |k, v| {
                        if *k == litkey {
                            vslot = Some(v.clone());
                            Err(())
                        } else {
                            Ok(())
                        }
                    });

                    let (vslot, new) = match (vslot, lval) {
                        // the field already exists
                        (Some(vslot), _) => (vslot, false),

                        // the field does not exist but is used as an l-value
                        // should *not* extend the terminal rvar (from `list_rvar_fields`),
                        // since it has to be instantiated which we can't do without a ref
                        (None, true) => {
                            let vslot = new_slot(F::Unknown, self.types());
                            check!(self.types().assert_rvar_includes(rvar.clone(),
                                                                     &[(litkey, vslot.clone())]));
                            (vslot, true)
                        },

                        // the field does not exist and is used as an r-value, return nothing
                        (None, false) => return Ok(Index::Missing),
                    };

                    vslot.adapt(ety0.flex(), self.types());
                    if new {
                        return Ok(Index::Created(vslot));
                    } else {
                        return Ok(Index::Found(vslot));
                    }
                }

                Some(&Tables::ArrayN(ref value)) => {
                    // special case `n`, otherwise use the general case (rejects non-int keys)
                    if let Key::Str(ref s) = litkey {
                        if &s[..] == &b"n"[..] {
                            let nslot = Slot::new(value.flex(), Ty::new(T::Integer));
                            return Ok(Index::Found(nslot));
                        }
                    }
                }

                _ => {}
            }
        }

        // handle other cases. in principle arrays and maps should be constructed explicitly
        let intkey = self.env.get_type_bounds(&kty).1.is_integral();
        match ety.get_tables() {
            // possible! this happens when the string metatable was resolved *and* it is wrong.
            None => {
                let value = Slot::just(Ty::new(T::Dynamic(Dyn::Oops)));
                // the flex should be retained
                if lval { value.adapt(ety0.flex(), self.types()); }
                Ok(Index::Found(value))
            },

            Some(&Tables::Fields(..)) => {
                assert!(!had_litkey);
                self.env.error(expspan,
                               m::IndexToRecWithUnknownStr { tab: self.display(&*ety0),
                                                             key: self.display(&kty) })
                        .done()?;
                Ok(Index::dummy())
            },

            Some(&Tables::Array(ref value)) | Some(&Tables::ArrayN(ref value)) if intkey => {
                if lval { value.adapt(ety0.flex(), self.types()); }
                Ok(Index::Found((*value).clone().with_nil()))
            },

            Some(&Tables::Array(..)) | Some(&Tables::ArrayN(..)) => {
                self.env.error(expspan,
                               m::IndexToArrayWithNonInt { tab: self.display(&*ety0),
                                                           key: self.display(&kty) })
                        .done()?;
                Ok(Index::dummy())
            },

            Some(&Tables::Map(ref key, ref value)) => {
                check!(kty.assert_sub(&**key, self.types()));
                if lval { value.adapt(ety0.flex(), self.types()); }
                Ok(Index::Found((*value).clone().with_nil()))
            },

            Some(&Tables::All) => {
                self.env.error(&*ety0, m::IndexToAnyTable { tab: self.display(&*ety0) })
                        .done()?;
                Ok(Index::dummy())
            },
        }
    }

    fn check_rval_index(&mut self, ety: &Spanned<Slot>, kty: &Spanned<Slot>,
                        expspan: Span) -> Result<Slot> {
        match self.check_index_common(ety, kty, expspan, false)? {
            Index::Missing => {
                self.cannot_index(expspan, ety, kty)?;
                Ok(Slot::dummy())
            },
            Index::Created(..) => unreachable!(),
            Index::Found(slot) => Ok(slot),
        }
    }

    // this should be followed by assign_to_lval_index
    fn check_lval_index(&mut self, ety: &Spanned<Slot>, kty: &Spanned<Slot>,
                        expspan: Span) -> Result<Lvalue> {
        let (found, slot) = match self.check_index_common(ety, kty, expspan, true)? {
            Index::Missing => unreachable!(),
            Index::Created(slot) => (false, slot),
            Index::Found(slot) => (true, slot),
        };
        Ok(Lvalue { found: found, slot: slot.with_loc(expspan) })
    }

    // this should be preceded by check_lval_index with the equal parameters
    fn assign_to_lval_index(&mut self, ety: &Spanned<Slot>, kty: &Spanned<Slot>, lvalue: &Lvalue,
                            initrhs: &Spanned<Slot>, specrhs: Option<&SlotSpec>) -> Result<()> {
        if lvalue.found {
            // ignore specrhs, should have been handled by the caller
            self.env.assign(&lvalue.slot, initrhs)?;
        } else {
            if self.env.assign_new(&lvalue.slot, initrhs, specrhs).is_err() {
                let specrhs = specrhs.map_or(&initrhs.base, |spec| spec.slot());
                self.env.error(&lvalue.slot,
                               m::CannotCreateIndex {
                                   tab: self.display(ety), key: self.display(kty),
                                   specrhs: self.display(specrhs),
                               })
                        .done()?;
            }

            // assignment can alter its flexibility if the slot is newly created, so we need this
            self.register_module_if_needed(&lvalue.slot);
        }

        Ok(())
    }

    fn assume_field_slot(&mut self, static_: bool, rootslot: Spanned<Slot>,
                         names: &[Spanned<Name>], namespan: Span, slot: Slot) -> Result<Slot> {
        assert!(!names.is_empty());

        // if we are updating a table, we need to sever any row variable connections
        // between the new table and the old table (otherwise we may be silently
        // updating a row variable).

        #[derive(Clone, Debug)]
        enum TableExtract {
            // a record with flexibility & nilability (both for reconstruction),
            // all fields except `next_key` and the slot for `next_key` if any
            Rec(F, Nil, Vec<(Key, Slot)>, Option<Slot>),

            // a class prototype, only possible at the root
            Proto(ClassId),
        }

        let extract_table = |prev: &Slot, next_key: &Name, span: Span, env: &mut Env<R>,
                             allow_prototype: bool| {
            let ty = if let Some(ty) = env.resolve_exact_type(&prev.unlift()) {
                ty
            } else {
                env.error(span, m::AssumeFieldToUnknownType {}).done()?;
                return Ok(None);
            };

            if allow_prototype {
                match *ty {
                    T::Class(Class::Prototype(cid)) => {
                        return Ok(Some(TableExtract::Proto(cid)));
                    }

                    T::Class(Class::Instance(_)) => {
                        env.error(span, m::AssumeFieldToInstance { slot: env.display(prev) })
                           .done()?;
                        return Ok(None);
                    }

                    T::Union(ref u) if !u.classes.is_empty() => {
                        // the union is assumed to be simplified, so even if `u.classes` has
                        // one type it is mixed with other types so it cannot be indexed.
                        env.error(span, m::AssumeFieldToUnknownClass { cls: env.display(prev) })
                           .done()?;
                        return Ok(None);
                    }

                    _ => {}
                }
            }

            if !ty.is_tabular() {
                env.error(span, m::AssumeFieldToNonRecord { slot: env.display(prev) }).done()?;
                return Ok(None);
            }

            if let Some(&Tables::Fields(ref rvar)) = ty.get_tables() {
                if static_ {
                    env.error(span, m::AssumeFieldStaticToNonClass { slot: env.display(prev) })
                       .done()?;
                    // behave as if there were no `static`
                }

                let mut fields = env.types().get_rvar_fields(rvar.clone());
                let pos = fields.iter().position(|&(ref k, _)| {
                    match *k {
                        Key::Str(ref k) => **k == **next_key,
                        Key::Int(_) => false,
                    }
                });
                let popped = pos.map(|i| fields.remove(i).1);
                Ok(Some(TableExtract::Rec(prev.flex(), ty.nil(), fields, popped)))
            } else {
                env.error(span, m::AssumeFieldToNonRecord { slot: env.display(prev) }).done()?;
                Ok(None)
            }
        };

        let root = rootslot.base; // will be returned on error
        let mut table;
        let mut span = rootslot.span; // `a`, `a.b`, `a.b.c`, ...
        let mut tables = Vec::new();

        // handle the first table, which can be a class prototype
        let firstname = names.first().unwrap();
        span |= firstname.span;
        match extract_table(&root, firstname, span, self.env, true)? {
            Some(TableExtract::Proto(cid)) => {
                // this has to be a last field!
                if names.len() > 1 {
                    let cls = T::Class(Class::Instance(cid));
                    self.env.error(namespan,
                                   m::AssumeFieldNestedToClass { cls: self.display(&cls) })
                            .done()?;
                    return Ok(root);
                }

                let mut fields =
                    self.env.context().get_class_fields_mut(cid).expect("invalid ClassId");
                let firstname = Key::Str(firstname.base.clone().into());
                if static_ {
                    fields.class_ty.insert(firstname, slot);
                } else {
                    fields.instance_ty.insert(firstname, slot);
                }

                return Ok(root);
            }
            Some(TableExtract::Rec(flex, nil, fields, next)) => {
                tables.push((flex, nil, fields));
                table = next;
            }
            None => { // the error occurred and already reported
                return Ok(root);
            }
        }

        // extract the prior table and continue to index fields
        for name in &names[1..] {
            if let Some(prevtable) = table {
                span |= name.span;
                match extract_table(&prevtable, name, span, self.env, false)? {
                    Some(TableExtract::Proto(_)) => unreachable!(),
                    Some(TableExtract::Rec(flex, nil, fields, next)) => {
                        tables.push((flex, nil, fields));
                        table = next;
                    }
                    None => { // the error occurred and already reported
                        return Ok(root);
                    }
                }
            } else {
                self.env.error(span, m::AssumeFieldToMissing {}).done()?;
                return Ok(root);
            }
        }

        // the last `table` extracted is a field being replaced, so should be ignored
        drop(table);
        assert_eq!(names.len(), tables.len());

        // put the last table to the second-to-last table and so on
        let mut slot = slot;
        for (name, (flex, nil, mut fields)) in names.iter().rev().zip(tables.into_iter()) {
            fields.push((Key::Str(name.base.clone().into()), slot));
            let rvar = self.types().gen_rvar();
            self.types().assert_rvar_includes(rvar.clone(), &fields).expect(
                "cannot insert updated disjoint fields into a fresh row variable"
            );
            slot = Slot::new(flex,
                             Ty::new(T::Tables(Cow::Owned(Tables::Fields(rvar)))).or_nil(nil));
        }

        // the final table should be assumed back to the current scope
        Ok(slot)
    }

    fn check_assign(&mut self, vars: &'inp Spanned<Vec<TypeSpec<Spanned<Var>>>>,
                    exps: Option<&'inp Spanned<Vec<Spanned<Exp>>>>,
                    stmtspan: Span) -> Result<Exit> {
        #[derive(Debug)]
        enum VarRef<'a> {
            Name(&'a Spanned<NameRef>),
            // table slot, key slot, indexed lvalue
            Slot(Spanned<Slot>, Spanned<Slot>, Lvalue),
        }

        let mut exprexit = ExprExit::None;
        let varrefspecs = vars.iter().map(|varspec| {
            let varref = match varspec.base.base {
                Var::Name(ref nameref) => VarRef::Name(nameref),

                Var::Index(ref e, ref key) => {
                    let Exitable(exit1, ty) = self.visit_exp(e, None)?;
                    let Exitable(exit2, kty) = self.visit_exp(key, None)?;
                    exprexit = exprexit.collide(exit1).collide(exit2);
                    let ty = ty.into_first();
                    let kty = kty.into_first();
                    let lvalue = self.check_lval_index(&ty, &kty, varspec.base.span)?;
                    VarRef::Slot(ty, kty, lvalue)
                },

                Var::IndexName(ref e, ref key) => {
                    let Exitable(exit, ty) = self.visit_exp(e, None)?;
                    exprexit = exprexit.collide(exit);
                    let ty = ty.into_first();
                    let keystr = Str::from(key.base[..].to_owned());
                    let kty = Slot::just(Ty::new(T::Str(Cow::Owned(keystr)))).with_loc(key);
                    let lvalue = self.check_lval_index(&ty, &kty, varspec.base.span)?;
                    VarRef::Slot(ty, kty, lvalue)
                },
            };

            let varspec = self.visit_type_spec(varspec)?;
            Ok((varref, varspec))
        }).collect::<Result<Vec<_>>>()?;

        // the incomplete expression is parsed as Assign without rhs,
        // so we won't generate further errors due to mismatching types
        let mut infos = if let Some(ref exps) = exps {
            // construct hints to derive type inference
            let knowntypes = varrefspecs.iter().map(|&(ref varref, ref varspec)| {
                if let Some(ref spec) = *varspec {
                    spec.slot().clone()
                } else {
                    // no type spec, use the existing slot type as a substitute
                    match *varref {
                        VarRef::Name(nameref) => {
                            let def = self.env.get_var(nameref);
                            let defslot = def.and_then(|def| def.slot.slot().cloned());
                            // the slot may haven't been assigned;
                            // as we have to put _something_ to the SpannedSlotSeq,
                            // we just put a dummy so that it would get ignored.
                            defslot.unwrap_or_else(|| Slot::dummy()).with_loc(nameref)
                        },
                        VarRef::Slot(_, _, ref lvalue) => lvalue.slot.clone(),
                    }
                }
            }).collect();

            let hint = SpannedSlotSeq { head: knowntypes, tail: None, span: vars.span };
            let Exitable(exit, slotseq) = self.visit_explist(exps, Some(hint))?;
            exprexit = exprexit.collide(exit);
            Some(slotseq.into_iter_with_nil())
        } else {
            None
        };

        // unlike St::Local, do not tolerate the uninitialized variables
        for (var, (varref, specinfo)) in vars.iter().zip(varrefspecs.into_iter()) {
            // ideally should be done via zip, but then concrete types will collide
            // just ignore the assignment when info is None instead
            let info = infos.as_mut().and_then(|it| it.next());
            debug!("assigning {:?} to {:?} with type {:?} (specified) and {:?} (given)",
                   info, var, specinfo, varref);

            match varref {
                VarRef::Name(nameref) => {
                    if let Some(specinfo) = specinfo {
                        // variable declaration
                        self.env.add_var(nameref, Some(specinfo), info)?;
                    } else {
                        // variable assignment
                        if let Some(info) = info {
                            self.env.assign_to_var(nameref, info)?;
                        }
                    }

                    // map the name span to the resulting slot
                    if let Some(varslot) = self.env.get_var(nameref)
                                                   .and_then(|var| var.slot.slot().cloned()) {
                        let varslot = varslot.with_loc(nameref);
                        self.context().spanned_slots_mut().insert(varslot);
                    }
                }

                // indexed assignment
                VarRef::Slot(ety, kty, lvalue) => {
                    if lvalue.found {
                        if let Some(ref specinfo) = specinfo {
                            // now we know that this assignment cannot declare a field
                            self.env.error(specinfo.slot(), m::TypeSpecToIndex {}).done()?;
                        }
                    }
                    if let Some(info) = info {
                        self.assign_to_lval_index(&ety, &kty, &lvalue, &info, specinfo.as_ref())?;
                    }
                    // allow lhs to be recorded even when info is missing (for completion)
                    self.context().spanned_slots_mut().insert(lvalue.slot);
                }
            }
        }

        exprexit.to_stmt(stmtspan, self.env)
    }

    #[cfg(feature = "no_implicit_func_sig")]
    fn error_on_implicit_sig(&mut self, sig: &Sig) -> Result<()> {
        if sig.args.head.iter().any(|spec| spec.kind.is_none()) {
            self.env.error(&sig.args, m::ImplicitSigOnNamedFunc {}).done()?;
        }
        Ok(())
    }

    #[cfg(not(feature = "no_implicit_func_sig"))]
    fn error_on_implicit_sig(&mut self, _sig: &Sig) -> Result<()> {
        Ok(())
    }

    pub fn visit(&mut self, chunk: &'inp Spanned<Block>) -> Result<()> {
        self.visit_block(chunk)?;
        Ok(())
    }

    fn visit_block(&mut self, block: &'inp Spanned<Block>) -> Result<Exit> {
        // `self.pending_modules` should be kept in sync, even when the checking fails
        self.pending_modules.push(HashMap::new());
        let exit;
        let ret;
        {
            let mut scope = self.scoped(Scope::new());
            exit = scope.visit_block_(block);
            ret = scope.check_pending_modules();
        }
        self.pending_modules.pop().expect("no matching pending module list");
        let exit = exit?;
        ret?;
        Ok(exit)
    }

    fn check_pending_modules(&mut self) -> Result<()> {
        // we cannot remove the list of pending modules until we are done,
        // because pending type checking may refer (or even add) to them.
        loop {
            // drain pending declarations
            let bodies: Vec<_> = {
                let mut modules = self.pending_modules.last_mut().unwrap();
                if !modules.is_empty() {
                    debug!("finishing pending type checking for {:?}", modules.values());
                }
                modules.iter_mut().flat_map(|(_, module)| module.func_bodies.drain(..)).collect()
            };

            if bodies.is_empty() {
                break; // we are done
            }

            // handle the pending type checking
            for body in bodies {
                // we can discard the output type, because it should be same to the previous type
                // as long as the signature is explicit and identical
                self.visit_func_body(body.tag, None, body.selfparam, body.sig,
                                     body.block, body.declspan, None)?;
            }
        }

        // remove the module flexibility from remaining slots
        // (done here to avoid non-determistic error messages in the test)
        for (_, module) in self.pending_modules.last().unwrap().iter() {
            module.slot.unmark_as_module();
        }

        Ok(())
    }

    fn visit_block_(&mut self, block: &'inp Spanned<Block>) -> Result<Exit> {
        let mut exit = Exit::None;
        let mut ignored_stmts: Option<Span> = None;
        for stmt in &block.base {
            if exit != Exit::None {
                ignored_stmts = Some(ignored_stmts.unwrap_or(Span::dummy()) | stmt.span);
                // the exit return can no longer affect this block's return
                self.visit_stmt(stmt)?;
            } else {
                exit = self.visit_stmt(stmt)?;
            }
        }
        #[cfg(feature = "warn_on_dead_code")] {
            if let Some(span) = ignored_stmts {
                self.env.warn(span, m::DeadCode {}).done()?;
            }
        }
        Ok(exit)
    }

    fn visit_stmt(&mut self, stmt: &'inp Spanned<Stmt>) -> Result<Exit> {
        debug!("visiting stmt {:?}", *stmt);

        match *stmt.base {
            // it should not happen, but for the purpose of checker, the error nodes are ignored
            St::Oops => Ok(Exit::None),

            St::Void(ref exp) => {
                let (exit, _) = self.visit_exp_from_stmt(exp, None)?;
                Ok(exit)
            },

            St::Assign(ref vars, ref exps) => {
                self.check_assign(vars, exps.as_ref(), stmt.span)
            },

            St::Do(ref block) => self.visit_block(block),

            St::While(ref cond, ref block) => {
                let (mut exit, ty) = self.visit_exp_from_stmt(cond, None)?;
                let boolean = self.check_bool(ty.unspan().unlift());

                // the "normal" exit when the loop body doesn't do anything special
                let normal_exit = match boolean {
                    Bool::Truthy => Some(Exit::Stop), // infinite loop
                    Bool::Falsy => None,
                    Bool::Unknown => Some(Exit::None),
                };

                // warn if the block has no chance to run
                #[cfg(feature = "warn_on_dead_code")] {
                    if normal_exit.is_none() || exit >= Exit::Break {
                        self.env.warn(block, m::DeadCode {}).done()?;
                    }
                }

                if let Some(normal_exit) = normal_exit {
                    exit &= self.visit_block(block)?;
                    exit = exit.loop_boundary(normal_exit);
                }
                Ok(exit)
            }

            St::Repeat(ref block, ref cond) => {
                let mut exit = self.visit_block(block)?;
                let (exit_, ty) = self.visit_exp_from_stmt(cond, None)?;
                exit &= exit_;
                if exit == Exit::None {
                    match self.check_bool(ty.unspan().unlift()) {
                        Bool::Truthy => Ok(Exit::Stop),
                        Bool::Falsy => Ok(exit),
                        Bool::Unknown => Ok(Exit::None),
                    }
                } else {
                    Ok(exit.loop_boundary(Exit::None))
                }
            }

            St::If(ref conds, ref lastblock) => {
                // conditions *can* diverge, which complicates the exit computation.
                // if we have something like `if C1 then B1 elseif C2 then B2 ... else E end`,
                // the final exit should be `(C1 & B1) | (C1 & C2 & B2) | ... | (C1 & C2 & ... & E)`
                // because all conditions up to that block will be executed for each block.
                let mut exit = Exit::Stop; // (C1 & B1) | ... | (C1 & ... & Ck & Bk)
                let mut condexit = Exit::None; // C1 & ... & Ck

                let mut ignored_blocks = None; // or Some((first truthy cond span, blocks span))
                for &Spanned { base: (ref cond, ref block), span } in conds {
                    // do not update the exit, as blocks after a truthy condition will be ignored
                    if let Some((_, ref mut blocks_span)) = ignored_blocks {
                        *blocks_span |= span;
                        continue;
                    }

                    let (condexit_, ty) = self.visit_exp_from_stmt(cond, None)?;
                    condexit &= condexit_;
                    let boolean = self.check_bool(ty.unspan().unlift());
                    match boolean {
                        Bool::Truthy => {
                            ignored_blocks = Some((cond.span, Span::dummy()));
                            exit |= condexit & self.visit_block(block)?;
                        }
                        Bool::Falsy => {
                            #[cfg(feature = "warn_on_useless_conds")] {
                                self.env.warn(span, m::IgnoredIfCase {})
                                        .note(cond, m::IfCaseWithFalsyCond {})
                                        .done()?;
                            }
                            exit |= condexit;
                        }
                        Bool::Unknown => {
                            exit |= condexit & self.visit_block(block)?;
                        }
                    }
                }

                if let &Some(ref block) = lastblock {
                    if let Some((_, ref mut blocks_span)) = ignored_blocks {
                        *blocks_span |= block.span;
                    } else {
                        exit |= condexit & self.visit_block(block)?;
                    }
                } else {
                    if ignored_blocks.is_none() {
                        exit |= condexit;
                    }
                }

                #[cfg(feature = "warn_on_useless_conds")] {
                    if let Some((truthy_span, blocks_span)) = ignored_blocks {
                        if blocks_span.is_dummy() {
                            self.env.warn(truthy_span, m::IfCaseWithTruthyCond {}).done()?;
                        } else {
                            self.env.warn(blocks_span, m::IgnoredIfCase {})
                                    .note(truthy_span, m::IfCaseWithTruthyCond {})
                                    .done()?;
                        }
                    }
                }

                Ok(exit)
            }

            St::For(ref localname, ref start, ref end, ref step, _blockscope, ref block) => {
                let mut expspan = start.span | end.span;

                // any of them can diverge, we treat them as like a single expression
                let Exitable(exit1, start) = self.visit_exp(start, None)?;
                let Exitable(exit2, end) = self.visit_exp(end, None)?;
                let Exitable(exit3, step) = if let &Some(ref step) = step {
                    expspan |= step.span;
                    let step = self.visit_exp(step, None)?;
                    step.map(|slot| slot.into_first().map(|s| s.unlift().clone()))
                } else {
                    Exitable::new(Ty::new(T::Integer).without_loc()) // to simplify the matter
                };

                let mut exit = exit1.collide(exit2).collide(exit3).to_stmt(expspan, self.env)?;
                let start = start.into_first().map(|s| s.unlift().clone());
                let end = end.into_first().map(|s| s.unlift().clone());

                // the similar logic is also present in check_bin_op
                let startflags = self.env.get_type_bounds(&start).1;
                let endflags = self.env.get_type_bounds(&end).1;
                let stepflags = self.env.get_type_bounds(&step).1;
                let indty;
                if startflags.is_integral() && endflags.is_integral() && stepflags.is_integral() &&
                   !(startflags.is_dynamic() && endflags.is_dynamic() && stepflags.is_dynamic()) {
                    indty = T::Integer;
                } else {
                    indty = T::Number;
                }
                match (start.assert_sub(&indty.clone().without_loc(), self.types()),
                       end.assert_sub(&indty.clone().without_loc(), self.types()),
                       step.assert_sub(&indty.clone().without_loc(), self.types())) {
                    (Ok(()), Ok(()), Ok(())) => {}
                    (r1, r2, r3) => {
                        let span = start.span | end.span | step.span;
                        let mut more = self.env.error(span, m::NonNumericFor {});
                        if let Err(r) = r1 {
                            more = more.report_types(r, TypeReportHint::None);
                        }
                        if let Err(r) = r2 {
                            more = more.report_types(r, TypeReportHint::None);
                        }
                        if let Err(r) = r3 {
                            more = more.report_types(r, TypeReportHint::None);
                        }
                        more.done()?;
                    }
                }

                // warn if the block has no chance to run
                #[cfg(feature = "warn_on_dead_code")] {
                    if exit >= Exit::Break {
                        self.env.warn(block, m::DeadCode {}).done()?;
                    }
                }

                let mut scope = self.scoped(Scope::new());
                let indty = Slot::var(Ty::new(indty));
                let nameref = NameRef::Local(localname.base.clone()).with_loc(localname);
                scope.env.add_var(&nameref, None, Some(indty.without_loc()))?;

                exit &= scope.visit_block(block)?;
                Ok(exit.loop_boundary(Exit::None))
            }

            St::ForIn(ref names, ref exps, _blockscope, ref block) => {
                let (mut exit, infos) = self.visit_explist_from_stmt(exps, None)?;
                let expspan = infos.all_span();
                let mut infos = infos.into_iter_with_nil();
                let func = infos.next().unwrap(); // iterator function
                let state = infos.next().unwrap(); // immutable state to the iterator
                let last = infos.next().unwrap(); // last value returned from the iterator

                // `func` is subject to similar constraints to `self.visit_func_call`
                let func = func.map(|t| t.unlift().clone());
                let indtys;
                if !self.env.get_type_bounds(&func).1.is_callable() {
                    self.env.error(expspan, m::NonFuncIterator { iter: self.display(&func) })
                            .done()?;
                    indtys = TySeq::dummy();
                } else if let Some(dyn) = func.get_dynamic() {
                    // can't determine what func will return
                    indtys = TySeq { head: vec![],
                                     tail: Some(Ty::new(T::Dynamic(dyn))) };
                } else {
                    // last can be updated, so one should assume that its type can be much wider.
                    let indvar = T::TVar(self.types().gen_tvar());

                    // func <: function(state, last) -> (last, ...)
                    //
                    // note that this sets hard (and possibly tight) bounds to `indvar`.
                    // while the proper constraint solver should be able to deal with them
                    // in any order, our current half-baked solver requires them to be ordered
                    // in the decreasing order of size. since the initial `last` type is likely
                    // to be a subtype of function's own bounds, we assert function types first.
                    //
                    // note: `indvar` is set to Nil::Absent to avoid an unexpected subtyping error
                    let state = state.map(|t| t.unlift().clone());
                    let indvarty = Ty::new(indvar.clone()).or_nil(Nil::Absent);
                    let args = SpannedTySeq { head: vec![state, indvarty.without_loc()],
                                              tail: None,
                                              span: Span::dummy() };
                    let Exitable(exit_, mut returns) =
                        self.check_callable(&func.clone().with_loc(expspan), &args, false)?;
                    exit &= exit_.to_stmt(expspan, self.env)?;

                    if let Err(r) = last.assert_sub(&indvar, self.types()) {
                        // it is very hard to describe, but it is conceptually
                        // an extension of check_callable
                        self.env.error(expspan,
                                       m::BadFuncIterator { iter: self.display(&func) })
                                .report_types(r, TypeReportHint::None)
                                .done()?;
                    }

                    // note that we ignore indvar here. it is only kept internally and
                    // not visible outside; returns is what we should assign to variables!
                    // we should still account for the fact that the first value cannot be nil.
                    take(returns.ensure_at_mut(0), |t| t.without_nil());

                    indtys = returns;
                }

                // warn if the block has no chance to run
                #[cfg(feature = "warn_on_dead_code")] {
                    if exit >= Exit::Break {
                        self.env.warn(block, m::DeadCode {}).done()?;
                    }
                }

                let mut scope = self.scoped(Scope::new());
                for (localname, ty) in names.iter().zip(indtys.into_iter_with_nil()) {
                    let ty = Slot::var(ty);
                    let nameref = NameRef::Local(localname.base.clone()).with_loc(localname);
                    scope.env.add_var(&nameref, None, Some(ty.without_loc()))?;
                }

                exit &= scope.visit_block(block)?;
                Ok(exit.loop_boundary(Exit::None))
            }

            St::FuncDecl(ref name, ref sig, _blockscope, ref block, nextscope) => {
                self.error_on_implicit_sig(sig)?;

                // `name` itself is available to the inner scope
                let funcv = self.types().gen_tvar();
                let info = Slot::just(Ty::new(T::TVar(funcv))).with_loc(stmt);
                if let (&NameRef::Local(..), None) = (&name.base, nextscope) {
                    // this is very rare but valid case where the local variable is
                    // overwritten by a local function decl (so the NameRef is local
                    // but there is no new sibling scope). it's equivalent to assignment.
                    self.env.assign_to_var(name, info)?;
                } else {
                    // otherwise it is a new variable.
                    self.env.add_var(name, None, Some(info))?;
                }
                let (tag, no_check) = self.visit_sig_attrs(&sig.attrs)?;
                let functy = self.visit_func_body(tag, no_check, None, sig, block,
                                                  stmt.span, None)?;
                if let Err(r) = Ty::new(T::TVar(funcv)).assert_eq(&*functy.unlift(), self.types()) {
                    self.env.error(stmt, m::BadRecursiveCall {})
                        .report_types(r, TypeReportHint::None)
                        .done()?;
                }
                Ok(Exit::None)
            }

            St::MethodDecl(Spanned { base: (ref name, ref meths), .. },
                           ref selfparam, ref sig, _blockscope, ref block) => {
                assert!(meths.len() >= 1);

                self.error_on_implicit_sig(sig)?;

                // find a slot for the first name
                let info = if self.env.get_var(name).is_some() {
                    self.env.ensure_var(name)?
                } else {
                    self.env.error(name, m::NoVar { name: self.env.get_name(name) }).done()?;
                    Slot::dummy()
                };

                // reduce the expr a.b.c...y.z into (a["b"]["c"]...["y"]).z
                let mut info = info.with_loc(name);
                for subname in &meths[..meths.len()-1] {
                    let keystr = Str::from(subname.base[..].to_owned());
                    let kty = Slot::just(Ty::new(T::Str(Cow::Owned(keystr)))).with_loc(subname);
                    let subspan = info.span | subname.span; // a subexpr for this indexing
                    info = self.check_rval_index(&info, &kty, subspan)?.with_loc(subspan);
                }
                let method = meths.last().unwrap();

                // gather the lvalue (this should happen first because we should determine
                // if this is a module indexing)
                let subspan = info.span | method.span;
                let keystr = Str::from(method.base[..].to_owned());
                let kty = Slot::just(Ty::new(T::Str(Cow::Owned(keystr)))).with_loc(method);
                let lvalue = self.check_lval_index(&info, &kty, subspan)?;

                // now prepare the right-hand side (i.e. method decl)
                let (tag, mut no_check) = self.visit_sig_attrs(&sig.attrs)?;
                if no_check.is_none() && info.flex() == F::Module {
                    // module indexing causes the rhs not to be checked right now (like NO_CHECK)
                    no_check = Some(NoCheck::Module);
                }
                let selfinfo = if let Some(ref selfparam) = *selfparam {
                    let slot = self.visit_self_param(selfparam.span, &info, no_check, &method)?;
                    Some((selfparam, slot))
                } else {
                    None
                };
                let methinfo = self.visit_func_body(tag, no_check, selfinfo.clone(), sig, block,
                                                    stmt.span, None)?;

                // if this is a module indexing (that is, an assignment to the module field slot
                // and the declaration was not already [NO_CHECK]), we will keep the arguments to
                // `visit_func_body` to repeat the checking at the end of scope.
                //
                // note that checking in the different position is sane, because all names are
                // scoped (e.g. `local` after the method declaration won't affect the body).
                // types are currently not, but probably it should too.
                if no_check == Some(NoCheck::Module) {
                    debug!("adding a pending type checking to {:?}", info);
                    let key = &*info.unlift() as *const Ty;
                    let modules = self.pending_modules.iter_mut().rev();
                    let module = modules.filter_map(|modules| modules.get_mut(&key)).next().expect(
                        "slots with F::Module not registered in the current checker"
                    );
                    module.func_bodies.push(PendingFuncBody {
                        tag: tag, selfparam: selfinfo, sig: sig, block: block, declspan: stmt.span,
                    });
                }

                self.assign_to_lval_index(&info, &kty, &lvalue, &methinfo.with_loc(stmt), None)?;
                Ok(Exit::None)
            }

            St::Local(ref names, ref exps, _nextscope) => {
                // collect specified types first (required for hints)
                let nameinfos = names.iter().map(|namespec| {
                    let info = self.visit_type_spec(namespec)?;
                    Ok((&namespec.base, info))
                }).collect::<Result<Vec<_>>>()?;

                let hint = SpannedSlotSeq {
                    head: nameinfos.iter().map(|&(name, ref info)| {
                        if let Some(ref spec) = *info {
                            spec.slot().clone()
                        } else {
                            Slot::dummy().with_loc(name)
                        }
                    }).collect(),
                    tail: None,
                    span: names.span,
                };
                let (exit, infos) = self.visit_explist_from_stmt(exps, Some(hint))?;

                for ((localname, specinfo), info) in nameinfos.into_iter()
                                                              .zip(infos.into_iter_with_none()) {
                    let nameref = NameRef::Local(localname.base.clone()).with_loc(localname);
                    self.env.add_var(&nameref, specinfo, info)?;
                }
                Ok(exit)
            }

            St::Return(ref exps) => {
                let hint = match self.env.get_frame().returns {
                    Returns::None | Returns::Never => None,
                    Returns::Implicit(ref returns) | Returns::Explicit(ref returns) =>
                        Some(SpannedSlotSeq::from_seq(returns.clone().all_with_loc(stmt))),
                };
                let (exit, seq) = self.visit_explist_from_stmt(exps, hint)?;
                self.visit_return(seq, stmt.span)?;
                Ok(exit & Exit::Return)
            }

            St::Break => Ok(Exit::Break),

            St::KailuaOpen(ref name) => {
                let opts = self.env.opts().clone();
                self.env.context().open_library(name, opts)?;
                Ok(Exit::None)
            }

            St::KailuaType(scope, ref name, ref kind) => {
                // self-redefinition is handled separately, as we cannot distingiush
                // `--# type local A = <some type> / --# type A = A` from `--# type A = <some type>`
                if let K::Named(ref name_) = *kind.base {
                    if name.base == name_.base {
                        match scope {
                            TypeScope::Local => {} // invalid, will error below
                            TypeScope::Global => {
                                self.env.redefine_global_type(name, kind.span)?;
                                return Ok(Exit::None);
                            }
                            TypeScope::Exported => {
                                self.env.reexport_local_type(name, kind.span)?;
                                return Ok(Exit::None);
                            }
                        }
                    }
                }

                let ty = Ty::from_kind(kind, &mut self.env)?;
                match scope {
                    TypeScope::Local => self.env.define_local_type(name, ty)?,
                    TypeScope::Global => self.env.define_global_type(name, ty)?,
                    TypeScope::Exported => self.env.define_and_export_type(name, ty)?,
                }
                Ok(Exit::None)
            }

            St::KailuaAssume(ref newname, ref name, kindm, ref kind, _nextscope) => {
                let slot = self.visit_kind(kindm, kind)?;
                self.env.assume_var(&newname.clone().with_loc(name), slot)?;
                Ok(Exit::None)
            }

            St::KailuaAssumeField(static_, Spanned { base: (ref rootname, ref names), span },
                                  kindm, ref kind) => {
                if self.env.get_var(rootname).is_some() {
                    let slot = self.visit_kind(kindm, kind)?;
                    let rootslot = self.env.ensure_var(rootname)?.with_loc(rootname);
                    let newslot = self.assume_field_slot(static_, rootslot, names, span,
                                                         slot.base)?;
                    self.env.assume_var(rootname, newslot.with_loc(rootname))?;
                } else {
                    self.env.error(rootname, m::NoVar { name: self.env.get_name(rootname) })
                            .done()?;
                }
                Ok(Exit::None)
            }

            St::KailuaAssumeMethod(Spanned { base: (ref rootname, ref names), span },
                                   kindm, ref funckind) => {
                assert!(!names.is_empty());

                if self.env.get_var(rootname).is_some() {
                    let flex = F::from(kindm);
                    let mut func = Function::from_kind(funckind, &mut self.env)?;

                    let rootslot = self.env.ensure_var(rootname)?.with_loc(rootname);

                    // convert `method(...) --> ...` to `function(self: Self, ...) --> ...`
                    // where `Self` is an inferred type from `rootslot`
                    let selfinfo = self.visit_self_param(stmt.span, &rootslot, None, &names[0])?;
                    func.args.head.insert(0, selfinfo.unlift().clone());
                    func.argnames.insert(0, Some(Name::from(&b"self"[..]).without_loc()));

                    let slot = Slot::new(
                        flex, Ty::new(T::Functions(Cow::Owned(Functions::Simple(func)))),
                    );

                    // the final slot should be static
                    let newslot = self.assume_field_slot(true, rootslot, names, span, slot)?;
                    self.env.assume_var(rootname, newslot.with_loc(rootname))?;
                } else {
                    self.env.error(rootname, m::NoVar { name: self.env.get_name(rootname) })
                            .done()?;
                }

                Ok(Exit::None)
            }
        }
    }

    fn visit_return(&mut self, seq: SpannedSlotSeq, stmtspan: Span) -> Result<()> {
        // function types destroy flexibility, primarily because the return type is
        // a slot only in the inside view. in the outside it's always Var,
        // which should be ensured by `visit_func_call`.
        let seq = seq.unlift();

        match self.env.get_frame().returns.clone() {
            Returns::None => {
                self.env.get_frame_mut().returns = Returns::Implicit(seq.unspan());
            }

            Returns::Never => {
                // we consider this an error even if the return expression stops first,
                // as we want to enforce that a diverging function has *no* `return`s
                self.env.error(stmtspan, m::ReturnInDivergingFunc {}).done()?;
            }

            Returns::Implicit(returns) => {
                // need to infer the return type, but not _that_ much
                let returns = returns.all_with_loc(stmtspan);
                match seq.union(&returns, false, self.types()) {
                    Ok(returns) => {
                        self.env.get_frame_mut().returns =
                            Returns::Implicit(returns.unspan());
                    }
                    Err(r) => {
                        self.env.error(stmtspan, m::CannotExtendImplicitReturnType {})
                                .report_types(r, TypeReportHint::Returns)
                                .done()?;
                    }
                };
            }

            Returns::Explicit(returns) => {
                let returns = returns.all_with_loc(stmtspan);
                if let Err(r) = seq.assert_sub(&returns, self.types()) {
                    self.env.error(stmtspan, m::CannotReturn { returns: self.display(&returns),
                                                               ty: self.display(&seq) })
                            .report_types(r, TypeReportHint::Returns)
                            .done()?;
                }
            }
        }

        Ok(())
    }

    fn visit_sig_attrs(&mut self,
                       attrs: &[Spanned<Attr>]) -> Result<(Option<Tag>, Option<NoCheck>)> {
        let mut tag = None;
        let mut no_check = None;

        for attr in attrs {
            if *attr.name.base == *b"NO_CHECK" {
                // [NO_CHECK] is special
                if no_check.is_some() {
                    self.env.warn(attr, m::DuplicateAttrInSig {}).done()?;
                } else {
                    no_check = Some(NoCheck::User);
                }
            } else {
                // None is simply ignored, `Tag::from` has already reported the error
                if let Some(tag_) = Tag::from(attr, self.env)? {
                    if tag.is_some() {
                        self.env.warn(attr, m::DuplicateAttrInSig {}).done()?;
                    } else {
                        tag = Some(tag_);
                    }
                }
            }
        }

        Ok((tag, no_check))
    }

    fn visit_self_param(&mut self, selfparamspan: Span, tableinfo: &Spanned<Slot>,
                        no_check: Option<NoCheck>, method: &Spanned<Name>) -> Result<Slot> {
        // try to infer the type for `self`:
        // - if `tableinfo` is a class prototype `self` should be a corresponding instance
        // - if `tableinfo` is a string metatable `self` should be a string
        let mut inferred = None;
        if let Some(tableinfo) = self.env.resolve_exact_type(&tableinfo.unlift()) {
            if tableinfo.nil() != Nil::Noisy {
                // (except when it is unioned with nil)
                if tableinfo.tag() == Some(Tag::StringMeta) {
                    inferred = Some(Ty::new(T::String));
                } else if let T::Class(Class::Prototype(cid)) = *tableinfo {
                    let inst = T::Class(Class::Instance(cid));
                    if *method.base == *b"init" {
                        // [constructible] <class instance #cid>
                        inferred = Some(Ty::new(inst).with_tag(Tag::Constructible));
                    } else {
                        // <class instance #cid>
                        inferred = Some(Ty::new(inst));
                    }
                }
            }
        }

        // if we couldn't infer the type we try to use a fresh type variable,
        // except when [NO_CHECK] is requested (requires a fixed type)
        if let Some(ty) = inferred {
            Ok(Slot::var(ty))
        } else {
            match no_check {
                Some(NoCheck::User) => {
                    self.env.error(selfparamspan, m::NoCheckRequiresTypedSelf {}).done()?;
                }
                Some(NoCheck::Module) => {
                    self.env.error(selfparamspan, m::ModuleRequiresTypedSelf {}).done()?;
                }
                None => {}
            }

            // <fresh type variable>
            let tv = T::TVar(self.types().gen_tvar());
            Ok(Slot::var(Ty::new(tv)))
        }
    }

    fn visit_func_body(&mut self, tag: Option<Tag>, no_check: Option<NoCheck>,
                       selfparam: Option<(&Spanned<SelfParam>, Slot)>, sig: &Sig,
                       block: &'inp Spanned<Vec<Spanned<Stmt>>>, declspan: Span,
                       hint: Option<Spanned<Slot>>) -> Result<Slot> {
        // if the hint exists and has a functional portion,
        // collect first `sig.args.head.len()` types for missing argument types,
        // and a repeating part of remaining type sequence for a missing variadic argument type.
        // also collect the return type(s) which can be used as is.
        //
        // one edge case: if the signature has `n` arguments plus a variadic argument,
        // and the hint has `m` arguments plus a variadic argument,
        // then when `n < m` the variadic argument would get `n - m` non-repeating types!
        // since this is forbidden from the signature we treat this as an error case
        // and drop the type hint for the variadic argument altogether.
        let hint: Option<(Vec<Ty>, Option<Ty>, Option<TySeq>)> = hint.and_then(|hint| {
            self.env.resolve_exact_type(&hint.unlift()).and_then(|ty| {
                if let Some(&Functions::Simple(ref f)) = ty.get_functions() {
                    let mut args = f.args.clone();
                    if !sig.args.head.is_empty() {
                        args.ensure_at(sig.args.head.len() - 1);
                    }
                    let hinthead = args.head.drain(..sig.args.head.len()).collect();
                    let hinttail = if args.head.is_empty() { args.tail } else { None };
                    Some((hinthead, hinttail, f.returns.clone()))
                } else {
                    None
                }
            })
        });
        let (hinthead, hinttail, hintreturns) = match hint {
            Some((h, t, Some(ret))) => (Some(h), t, Some(Returns::Explicit(ret))),
            Some((h, t, None)) => (Some(h), t, Some(Returns::Never)),
            None => (None, None, None),
        };

        let vatype = match sig.args.tail {
            None => None,

            Some(Varargs { kind: None, .. }) => {
                // varargs present but types are unspecified
                if let Some(hint) = hinttail {
                    // use a hint instead ([NO_CHECK] can rely on this hint as well)
                    Some(hint)
                } else if let Some(no_check) = no_check {
                    // [NO_CHECK] always requires a type
                    match no_check {
                        NoCheck::User => {
                            self.env.error(declspan, m::NoCheckRequiresTypedVarargs {}).done()?;
                        }
                        NoCheck::Module => {
                            self.env.error(declspan, m::ModuleRequiresTypedVarargs {}).done()?;
                        }
                    }
                    return Ok(Slot::dummy());
                } else {
                    #[cfg(feature = "no_implicit_func_sig")] {
                        // implicit signature disabled, raise an error and continue
                        self.env.error(declspan, m::ImplicitVarargsTypeOnAnonymousFunc {}).done()?;
                        Some(Ty::dummy())
                    }
                    #[cfg(not(feature = "no_implicit_func_sig"))] {
                        // fill a fresh type variable in
                        Some(Ty::new(T::TVar(self.types().gen_tvar())))
                    }
                }
            },

            Some(Varargs { kind: Some(ref k), .. }) => {
                Some(Ty::from_kind(k, &mut self.env)?)
            },
        };

        let vainfo = vatype.clone().map(|t| TySeq { head: Vec::new(), tail: Some(t) });

        // we accumulate all known return types inside the frame
        // then checks if it matches with the `returns`.
        //
        // TODO the exception should be made to the recursive usage;
        // we probably need to put a type variable that is later equated to the actual returns
        let returns = if let Some(ref returns) = sig.returns {
            match *returns {
                ast::Returns::Never(_) => Returns::Never,
                ast::Returns::Seq(ref seq) =>
                    Returns::Explicit(TySeq::from_kind_seq(seq, |kind| kind, &mut self.env)?),
            }
        } else if let Some(hint) = hintreturns {
            // use a hint if possible ([NO_CHECK] can rely on this hint as well)
            hint
        } else if let Some(no_check) = no_check {
            match no_check {
                NoCheck::User => {
                    self.env.error(declspan, m::NoCheckRequiresTypedReturns {}).done()?;
                }
                NoCheck::Module => {
                    self.env.error(declspan, m::ModuleRequiresTypedReturns {}).done()?;
                }
            }
            return Ok(Slot::dummy());
        } else {
            Returns::None
        };
        let frame = Frame { vararg: vainfo, returns: returns };

        let mut argshead = Vec::new();
        let mut argnames = Vec::new();

        let mut scope = self.scoped(Scope::new_function(frame));
        if let Some((selfparam, selfinfo)) = selfparam {
            let ty = selfinfo.unlift().clone();
            let selfid = selfparam.clone().map(|param| param.0);
            scope.env.add_local_var_already_set(&selfid, selfinfo.without_loc())?;
            argshead.push(ty);
            argnames.push(Some(Name::from(&b"self"[..]).with_loc(selfparam)));
        }

        let mut hinthead = hinthead.map(|tys| tys.into_iter());
        for param in &sig.args.head {
            // need to consume the iterator in lockstep
            let hint = hinthead.as_mut().map(|it| it.next().unwrap());

            let ty;
            let sty;
            if let Some(slot) = scope.visit_type_spec(param)? {
                // the type has been specified (can be implicit, but we don't have initializations)
                #[cfg(feature = "no_implicit_func_sig")] {
                    // still error for `--: const` or `--: module` when implicit signature disabled
                    if param.kind.is_none() {
                        scope.env.error(&param.base, m::ImplicitArgTypeOnAnonymousFunc {}).done()?;
                    }
                }
                sty = slot.unwrap().base;
                ty = sty.unlift().clone();
            } else if let Some(hint) = hint {
                // use a hint instead ([NO_CHECK] can rely on this hint as well)
                ty = hint;
                sty = Slot::new(F::Var, ty.clone());
            } else if let Some(no_check) = no_check {
                // [NO_CHECK] always requires a type
                match no_check {
                    NoCheck::User => {
                        scope.env.error(&param.base, m::NoCheckRequiresTypedArgs {}).done()?;
                    }
                    NoCheck::Module => {
                        scope.env.error(&param.base, m::ModuleRequiresTypedArgs {}).done()?;
                    }
                }
                return Ok(Slot::dummy());
            } else {
                #[cfg(feature = "no_implicit_func_sig")] {
                    // implicit signature disabled, raise an error and continue
                    scope.env.error(&param.base, m::ImplicitArgTypeOnAnonymousFunc {}).done()?;
                    ty = Ty::dummy();
                    sty = Slot::dummy();
                }
                #[cfg(not(feature = "no_implicit_func_sig"))] {
                    // fill a fresh type variable in
                    let argv = scope.types().gen_tvar();
                    ty = Ty::new(T::TVar(argv));
                    sty = Slot::new(F::Var, Ty::new(T::TVar(argv)));
                }
            }

            scope.env.add_local_var_already_set(&param.base, sty.without_loc())?;
            argshead.push(ty);

            let name = param.base.name(scope.env.scope_map());
            argnames.push(Some(name.clone().with_loc(&param.base)));
        }

        // if we are in the legacy compat mode, also add `arg` to the scope as needed
        if let Some(Varargs { legacy_arg: Some(ref argid), .. }) = sig.args.tail {
            let argtab = Tables::ArrayN(Slot::var(vatype.as_ref().unwrap().clone()));
            let arginfo = Slot::var(Ty::new(T::Tables(Cow::Owned(argtab)))).with_loc(argid);
            scope.env.add_local_var_already_set(argid, arginfo)?;
        }

        let args = TySeq { head: argshead, tail: vatype };

        if no_check.is_none() {
            if let Exit::None = scope.visit_block(block)? {
                // the last statement is an implicit return
                let span = Span::from(block.span.end()); // conceptually at the end of block
                scope.visit_return(SpannedSlotSeq::new(span), span)?;
            }
        }

        let returns = match scope.env.get_frame().returns {
            Returns::Implicit(ref ret) | Returns::Explicit(ref ret) => Some(ret.clone()),
            Returns::Never | Returns::None => None,
        };
        let func = Function { args: args, argnames: argnames, returns: returns };
        Ok(Slot::just(Ty::new(T::func(func)).with_tag(tag)))
    }

    fn visit_func_call(&mut self, functy: &Spanned<Ty>, selfinfo: Option<Spanned<Slot>>,
                       args: &'inp Spanned<Args>, expspan: Span) -> Result<Exitable<SlotSeq>> {
        let functy = if let Some(func) = self.env.resolve_exact_type(functy) {
            func.with_loc(functy)
        } else {
            self.env.error(functy, m::CallToInexactType { func: self.display(functy) }).done()?;
            return Ok(Exitable::dummy());
        };

        // construct hints; they are given at the best effort basis
        let hint = if let Some(&Functions::Simple(ref f)) = functy.get_functions() {
            let mut args = f.args.clone();
            if selfinfo.is_some() && !args.head.is_empty() {
                args.head.remove(0); // args do not contain self, so do hints
            }
            Some(SlotSeq::from_seq(args).all_with_loc(&functy))
        } else {
            None
        };

        // should be visited first, otherwise a WHATEVER function will ignore slots in arguments
        let (nargs, Exitable(exit, mut argtys)) = match args.base {
            Args::List(ref ee) => {
                (ee.len(), self.visit_explist_with_span(ee, args.span, hint)?)
            },

            Args::Str(ref s) => {
                let argstr = Str::from(s[..].to_owned());
                let strty = T::Str(Cow::Owned(argstr)).with_loc(args);
                (1, Exitable::new(SpannedSlotSeq::from(strty)))
            },

            Args::Table(ref fields) => {
                let hint = hint.map(|seq| seq.into_first());
                let table = self.visit_table(fields, args.span, hint)?;
                (1, table.map(|table| SpannedSlotSeq::from(table.with_loc(args))))
            },
        };

        if !self.env.get_type_bounds(&functy).1.is_callable() {
            self.env.error(&functy, m::CallToNonFunc { func: self.display(&functy) }).done()?;
            return Ok(exit.with_dummy());
        }
        if let Some(dyn) = functy.get_dynamic() {
            return Ok(exit.with(SlotSeq::from(T::Dynamic(dyn))));
        }

        // handle tags, which may return different things from the function signature
        match functy.tag() {
            // require("foo")
            Some(Tag::Require) => {
                if nargs < 1 {
                    self.env.error(expspan, m::BuiltinGivenLessArgs { name: "require", nargs: 1 })
                            .done()?;
                    return Ok(exit.with_dummy());
                }

                let arg = self.env.resolve_exact_type(&argtys.ensure_at(0).unlift());
                if let Some(modname) = arg.and_then(|t| t.as_string().map(|s| s.to_owned())) {
                    let mut module = self.context().get_loaded_module(&modname, expspan)?;

                    if module.is_none() {
                        let modname = (&modname[..]).with_loc(&argtys.head[0]);

                        self.context().mark_module_as_loading(&modname, expspan);

                        info!("requiring {:?}", modname);
                        let opts = self.env.opts().clone();
                        let chunk = match opts.borrow_mut().require_chunk(modname, self.env) {
                            Ok(chunk) => chunk,
                            Err(_) => {
                                self.env.warn(argtys.ensure_at(0),
                                              m::CannotResolveModName {}).done()?;
                                return Ok(exit.with(SlotSeq::from(T::All)));
                            }
                        };
                        let mut env = Env::new(self.env.context(), opts, chunk.map);
                        let exit = {
                            let mut sub = Checker::new(&mut env);
                            sub.visit_block(&chunk.block)?
                        };
                        module = env.return_from_module(&modname, exit >= Exit::Stop, expspan)?;
                    }

                    if let Some(module) = module {
                        self.env.import_types(module.exported_types.with_loc(expspan))?;
                        if let Some(ref returns) = module.returns {
                            return Ok(exit.with(SlotSeq::from(returns.clone())));
                        } else {
                            // the module never returns, subsequent statements won't execute
                            return Ok(exit.with_diverging())
                        }
                    } else {
                        return Ok(exit.with_dummy());
                    }
                } else {
                    return Ok(exit.with(SlotSeq::from(T::All)));
                }
            }

            // assert(expr)
            Some(Tag::Assert) => {
                if nargs < 1 {
                    // TODO should display the true name
                    self.env.error(expspan, m::BuiltinGivenLessArgs { name: "assert", nargs: 1 })
                            .done()?;
                    return Ok(exit.with_dummy());
                }

                // non-list arguments have no usable conditions (always evaluate to true)
                if let Args::List(ref args) = args.base {
                    if let (Some(cond), _seq) = self.collect_conds_from_exp(&args[0])? {
                        self.assert_cond(cond, false)?;
                    }
                }
            }

            // assert_not(expr)
            Some(Tag::AssertNot) => {
                if nargs < 1 {
                    // TODO should display the true name
                    self.env.error(expspan,
                                   m::BuiltinGivenLessArgs { name: "assert-not", nargs: 1 })
                            .done()?;
                    return Ok(exit.with_dummy());
                }

                if let Args::List(ref args) = args.base {
                    if let (Some(cond), _seq) = self.collect_conds_from_exp(&args[0])? {
                        self.assert_cond(cond, true)?;
                    }
                }
            }

            // assert_type(expr)
            Some(Tag::AssertType) => {
                if nargs < 2 {
                    // TODO should display the true name
                    self.env.error(expspan,
                                   m::BuiltinGivenLessArgs { name: "assert-type", nargs: 2 })
                            .done()?;
                    return Ok(exit.with_dummy());
                }

                if let Some(flags) = self.ext_literal_ty_to_flags(argtys.ensure_at(1))? {
                    let cond = Cond::Flags(argtys.ensure_at(0).clone(), flags);
                    self.assert_cond(cond, false)?;
                }
            }

            // class()
            Some(Tag::MakeClass) => {
                let cid = self.context().make_class(None, expspan); // TODO parent
                return Ok(exit.with(SlotSeq::from(T::Class(Class::Prototype(cid)))));
            }

            // kailua_test.gen_tvar()
            Some(Tag::KailuaGenTvar) => {
                return Ok(exit.with(SlotSeq::from(T::TVar(self.types().gen_tvar()))));
            }

            // kailua_test.assert_tvar()
            Some(Tag::KailuaAssertTvar) => {
                if nargs < 1 {
                    // TODO should display the true name
                    self.env.error(expspan,
                                   m::BuiltinGivenLessArgs { name: "kailua-assert-tvar", nargs: 1 })
                            .done()?;
                    return Ok(exit.with_dummy());
                }

                let is_tvar =
                    if let T::TVar(_) = **argtys.ensure_at(0).unlift() { true } else { false };
                if !is_tvar {
                    let arg = argtys.ensure_at(0);
                    self.env.error(arg, m::NotTVar { slot: self.display(arg) }).done()?;
                }
                return Ok(exit.with(SlotSeq::new()));
            }

            _ => {}
        }

        let methodcall = if let Some(selfinfo) = selfinfo {
            argtys.head.insert(0, selfinfo);
            true
        } else {
            false
        };

        let Exitable(retexit, returns) =
            self.check_callable(&functy, &argtys.unlift(), methodcall)?;

        // merge exits; do not use `ExprExit::then` as this is the only way to generate Stop.
        // TODO this should be Var instead of Just!!!!!
        Ok(Exitable(cmp::max(exit, retexit), SlotSeq::from_seq(returns)))
    }

    fn visit_table(&mut self, fields: &'inp [(Option<Spanned<Exp>>, Spanned<Exp>)], tabspan: Span,
                   hint: Option<Spanned<Slot>>) -> Result<Exitable<T<'static>>> {
        // the finally resolved type depends on the hint type
        #[derive(Debug)]
        enum Target {
            Any,
            Fields(bool /*explicitly typed*/, Vec<(Key, Slot)>),
            // need to ensure that there is no hole and everything is 1-based
            Array(Spanned<Slot>, i32 /*min*/, i32 /*max*/, i32 /*count*/),
            Map(Spanned<Ty>, Spanned<Slot>),
        }

        // if the hint exists and has vectors or maps in a tabular portion,
        // we do not add to the fields but instead simply check if keys and values are subtypes
        let target = hint.and_then(|hint| {
            self.env.resolve_exact_type(&hint.unlift()).and_then(|ty| {
                match ty.get_tables() {
                    Some(&Tables::All) => Some(Target::Any),
                    Some(&Tables::Array(ref v)) | Some(&Tables::ArrayN(ref v)) => {
                        let v = v.clone().with_nil().with_loc(&hint);
                        Some(Target::Array(v, i32::MAX, i32::MIN, 0))
                    },
                    Some(&Tables::Map(ref k, ref v)) => {
                        let k = k.clone().with_loc(&hint);
                        let v = v.clone().with_nil().with_loc(&hint);
                        Some(Target::Map(k, v))
                    },
                    Some(&Tables::Fields(_)) => Some(Target::Fields(true, Vec::new())),
                    None => None,
                }
            })
        });
        let mut target = target.unwrap_or_else(|| Target::Fields(false, Vec::new()));

        let mut fieldspans = HashMap::new();

        let mut add_field = |target: &mut Target, env: &mut Env<R>,
                             k: Spanned<Ty>, v: Spanned<Slot>, varargs: bool| -> Result<()> {
            // extract an literal portion if possible
            let litkey = env.resolve_exact_type(&k).and_then(|ty| {
                if let Some(v) = ty.as_integer() {
                    Some(Key::from(v))
                } else if let Some(s) = ty.as_string() {
                    Some(Key::from(s))
                } else {
                    None
                }
            });

            // check for the duplicate keys if the key is literal
            let mut dup = false;
            if let Some(ref key) = litkey {
                if let Some(&prevspan) = fieldspans.get(key) {
                    env.error(&k, m::TableLitWithDuplicateKey { key: key })
                       .note(prevspan, m::PreviousKeyInTableLit {})
                       .done()?;
                    dup = true; // do not add this field if the key is significant
                } else {
                    fieldspans.insert(key.clone(), k.span);
                }
            }

            match *target {
                Target::Any => {}

                Target::Fields(explicit, _) if varargs => {
                    // guard this case first in place of more general errors
                    let span = k.span | v.span;
                    let mut more = env.error(span, m::TableLitWithUnboundSeq {});
                    if !explicit {
                        more = more.note(span, m::TableLitIsImplicitlyRec {});
                    }
                    more.done()?;
                }

                Target::Fields(explicit, ref mut fields) => {
                    if let Some(key) = litkey {
                        if !dup {
                            fields.push((key, Slot::just(v.unlift().clone())));
                        }
                    } else {
                        let mut more =
                            env.error(&k, m::TableLitWithInvalidRecKey { key: env.display(&k) });
                        if !explicit {
                            more = more.note(&k, m::TableLitIsImplicitlyRec {});
                        }
                        more.done()?;
                    }
                }

                Target::Array(ref mut vty, ref mut min, ref mut max, ref mut count) => {
                    // for varargs `k` would be Integer, but that is fine
                    // (the actual keys merged into `k` are always consecutive)
                    if !varargs {
                        // check if `k` is a non-duplicate integer, and if so update the counters
                        if let Some(Key::Int(k)) = litkey {
                            if !dup {
                                *count += 1;
                                if *min > k { *min = k; }
                                if *max < k { *max = k; }
                            }
                        } else {
                            env.error(&k, m::TableLitWithInvalidArrayKey { key: env.display(&k) })
                               .done()?;
                        }
                    }

                    let v_ = v.as_ref().map(|s| s.unlift());
                    let vty_ = vty.as_ref().map(|s| s.unlift());
                    if let Err(r) = v_.assert_sub(&vty_, env.types()) {
                        env.error(&v,
                                  m::TableLitWithInvalidArrayValue {
                                      given: env.display(&v), value: env.display(vty),
                                  })
                           .report_types(r, TypeReportHint::None)
                           .done()?;
                    }
                }

                Target::Map(ref mut kty, ref mut vty) => {
                    if let Err(r) = k.assert_sub(kty, env.types()) {
                        env.error(&k,
                                  m::TableLitWithInvalidMapKey {
                                      given: env.display(&k),
                                      key: env.display(kty), value: env.display(vty),
                                  })
                           .report_types(r, TypeReportHint::None)
                           .done()?;
                    }

                    let v_ = v.as_ref().map(|s| s.unlift());
                    let vty_ = vty.as_ref().map(|s| s.unlift());
                    if let Err(r) = v_.assert_sub(&vty_, env.types()) {
                        env.error(&v,
                                  m::TableLitWithInvalidMapValue {
                                      given: env.display(&v),
                                      key: env.display(kty), value: env.display(vty),
                                  })
                           .report_types(r, TypeReportHint::None)
                           .done()?;
                    }
                }
            }

            Ok(())
        };

        let mut len = 0;
        let mut exprexit = ExprExit::None;
        for (idx, &(ref key, ref value)) in fields.iter().enumerate() {
            // if this is the last entry and no explicit index is set, splice the values
            if idx == fields.len() - 1 && key.is_none() {
                let Exitable(exit, vty) = self.visit_exp(value, None)?;
                exprexit = exprexit.collide(exit);
                for ty in vty.head.into_iter() {
                    len += 1;
                    let kty = Ty::new(T::Int(len)).with_loc(ty.span.begin());
                    add_field(&mut target, &mut self.env, kty, ty, false)?;
                }
                if let Some(ty) = vty.tail {
                    let kty = Ty::new(T::Integer).with_loc(vty.span.end());
                    add_field(&mut target, &mut self.env, kty, ty, true)?;
                }
            } else {
                let kty = if let Some(ref key) = *key {
                    let Exitable(exit, key) = self.visit_exp(key, None)?;
                    exprexit = exprexit.collide(exit);
                    key.into_first().map(|slot| slot.unlift().clone())
                } else {
                    len += 1;
                    Ty::new(T::Int(len)).with_loc(value.span.begin())
                };

                let Exitable(exit, vty) = self.visit_exp(value, None)?;
                exprexit = exprexit.collide(exit);
                let vty = vty.into_first();
                add_field(&mut target, &mut self.env, kty, vty, false)?;
            }
        }

        let table = match target {
            Target::Any => Tables::All,

            Target::Fields(_, fields) => {
                let rvar = self.types().gen_rvar();
                if !fields.is_empty() {
                    // really should not fail...
                    self.types().assert_rvar_includes(rvar.clone(), &fields).expect(
                        "cannot insert disjoint fields into a fresh row variable"
                    );
                }
                Tables::Fields(rvar)
            },

            Target::Array(vty, min, max, count) => {
                // check if the array has all keys from 1 to # of keys
                if count != 0 {
                    if min != 1 {
                        self.env.error(tabspan, m::TableLitWithNonOneMinArrayKey {}).done()?;
                    } else if max != count {
                        self.env.error(tabspan, m::TableLitWithMissingArrayKey {}).done()?;
                    }
                }
                Tables::Array(vty.base)
            },

            Target::Map(kty, vty) => Tables::Map(kty.base, vty.base),
        };

        Ok(exprexit.with(T::Tables(Cow::Owned(table))))
    }

    fn visit_exp_from_stmt(&mut self, exp: &'inp Spanned<Exp>, hint: Option<SpannedSlotSeq>)
        -> Result<(Exit, SpannedSlotSeq)>
    {
        let Exitable(exit, base) = self.visit_exp(exp, hint)?;
        Ok((exit.to_stmt(exp.span, self.env)?, base))
    }

    // hint is used to drive the inference to the already known type.
    // this is mainly used for anonymous functions.
    fn visit_exp(&mut self, exp: &'inp Spanned<Exp>, hint: Option<SpannedSlotSeq>)
        -> Result<Exitable<SpannedSlotSeq>>
    {
        let Exitable(exit, slotseq) = self.visit_exp_(exp, hint)?;

        // mark the resulting slot (sequence) to the span
        let slot = if let Some(slot) = slotseq.head.first() {
            slot.clone()
        } else if let Some(ref slot) = slotseq.tail {
            slot.clone().with_nil()
        } else {
            Slot::just(Ty::silent_nil())
        };
        self.context().spanned_slots_mut().insert(slot.with_loc(exp));

        Ok(exit.with(slotseq.all_with_loc(exp)))
    }

    fn visit_exp_(&mut self, exp: &'inp Spanned<Exp>, hint: Option<SpannedSlotSeq>)
        -> Result<Exitable<SlotSeq>>
    {
        debug!("visiting exp {:?}", *exp);

        let ret = match *exp.base {
            // it should not happen, but for the purpose of checker, the error nodes are dummies
            Ex::Oops => Exitable::dummy(),

            // the explicit type `nil` is different from `nil` from an implicit expression
            Ex::Nil => Exitable::new(SlotSeq::from(Ty::silent_nil())),
            Ex::False => Exitable::new(SlotSeq::from(T::False)),
            Ex::True => Exitable::new(SlotSeq::from(T::True)),
            Ex::Num(v) if v.floor() == v =>
                if i32::MIN as f64 <= v && v <= i32::MAX as f64 {
                    Exitable::new(SlotSeq::from(T::Int(v as i32)))
                } else {
                    Exitable::new(SlotSeq::from(T::Integer))
                },
            Ex::Num(_) => Exitable::new(SlotSeq::from(T::Number)),
            Ex::Str(ref s) => {
                let str = Str::from(s[..].to_owned());
                Exitable::new(SlotSeq::from(T::Str(Cow::Owned(str))))
            },

            Ex::Varargs => {
                if let Some(vararg) = self.env.get_vararg() {
                    Exitable::new(SlotSeq::from_seq(vararg.clone()))
                } else {
                    self.env.error(exp, m::NoVarargs {}).done()?;
                    Exitable::dummy()
                }
            },
            Ex::Var(ref name) => {
                if self.env.get_var(name).is_some() {
                    Exitable::new(SlotSeq::from(self.env.ensure_var(name)?))
                } else {
                    self.env.error(exp, m::NoVar { name: self.env.get_name(name) }).done()?;
                    Exitable::dummy()
                }
            },

            Ex::Exp(ref e) => {
                let Exitable(exit, slotseq) = self.visit_exp(e, hint)?;
                // sequence flattens to the first
                exit.with(SlotSeq::from(slotseq.into_first().base))
            },
            Ex::Func(ref sig, _scope, ref block) => {
                let hint = hint.map(|seq| seq.into_first());
                let (tag, no_check) = self.visit_sig_attrs(&sig.attrs)?;
                let returns = self.visit_func_body(tag, no_check, None, sig, block,
                                                   exp.span, hint)?;
                Exitable::new(SlotSeq::from(returns))
            },
            Ex::Table(ref fields) => {
                let hint = hint.map(|seq| seq.into_first());
                self.visit_table(fields, exp.span, hint)?.map(SlotSeq::from)
            },

            Ex::FuncCall(ref func, ref args) => {
                let Exitable(exit, funcinfo) = self.visit_exp(func, None)?;
                let funcinfo = funcinfo.into_first().map(|t| t.unlift().clone());
                exit.then(self.visit_func_call(&funcinfo, None, args, exp.span)?)
            },

            Ex::MethodCall(Spanned { base: (ref e, ref method), span }, ref args) => {
                let keystr = Str::from(method.base[..].to_owned());
                let Exitable(exit, ty) = self.visit_exp(e, None)?;
                let ty = ty.into_first();
                let kty = Slot::just(Ty::new(T::Str(Cow::Owned(keystr)))).with_loc(method.span);
                let methinfo = self.check_rval_index(&ty, &kty, exp.span)?;
                self.context().spanned_slots_mut().insert(methinfo.clone().with_loc(span));
                let methinfo = methinfo.unlift().clone().with_loc(span);
                exit.then(self.visit_func_call(&methinfo, Some(ty), args, exp.span)?)
            },

            Ex::Index(ref e, ref key) => {
                let Exitable(exit1, ty) = self.visit_exp(e, None)?;
                let Exitable(exit2, kty) = self.visit_exp(key, None)?;
                let ty = ty.into_first();
                let kty = kty.into_first();
                let exit = exit1.collide(exit2);
                exit.with(SlotSeq::from(self.check_rval_index(&ty, &kty, exp.span)?))
            },
            Ex::IndexName(ref e, ref key) => {
                let keystr = Str::from(key.base[..].to_owned());
                let Exitable(exit, ty) = self.visit_exp(e, None)?;
                let ty = ty.into_first();
                let kty = Slot::just(Ty::new(T::Str(Cow::Owned(keystr)))).with_loc(key);
                exit.with(SlotSeq::from(self.check_rval_index(&ty, &kty, exp.span)?))
            },

            Ex::Un(op, ref e) => {
                // allow `#{...}` to be checked without an error
                let hint = match op.base {
                    UnOp::Len => {
                        let taborstr = Ty::new(T::table() | T::String);
                        Some(SpannedSlotSeq::from(Slot::just(taborstr).with_loc(op.span)))
                    },
                    _ => None
                };

                let Exitable(exit, info) = self.visit_exp(e, hint)?;
                let info = info.into_first();
                let info = self.check_un_op(op.base, &info, exp.span)?;
                exit.with(SlotSeq::from(info))
            },

            Ex::Bin(ref l, op, ref r) => {
                let Exitable(exit1, lhs) = self.visit_exp(l, None)?;
                let Exitable(exit2, rhs) = self.visit_exp(r, None)?;
                let lhs = lhs.into_first();
                let rhs = rhs.into_first();
                let info = self.check_bin_op(&lhs, op.base, &rhs, exp.span)?;
                exit1.collide(exit2).with(SlotSeq::from(info))
            },
        };

        trace!("typed exp {:?} as {:?}", exp, ret);
        Ok(ret)
    }

    fn visit_explist_from_stmt(&mut self, exps: &'inp Spanned<Vec<Spanned<Exp>>>,
                               hint: Option<SpannedSlotSeq>) -> Result<(Exit, SpannedSlotSeq)> {
        let Exitable(exit, base) = self.visit_explist(exps, hint)?;
        Ok((exit.to_stmt(exps.span, self.env)?, base))
    }

    fn visit_explist(&mut self, exps: &'inp Spanned<Vec<Spanned<Exp>>>,
                     hint: Option<SpannedSlotSeq>) -> Result<Exitable<SpannedSlotSeq>> {
        self.visit_explist_with_span(&exps.base, exps.span, hint)
    }

    fn visit_explist_with_span(&mut self, exps: &'inp [Spanned<Exp>], expspan: Span,
                               mut hint: Option<SpannedSlotSeq>)
        -> Result<Exitable<SpannedSlotSeq>>
    {
        // the last expression is special, so split it first or return the empty sequence
        let (lastexp, exps) = if let Some(exps) = exps.split_last() {
            exps
        } else {
            return Ok(Exitable::new(SpannedSlotSeq::new(expspan)));
        };

        // extract first `exps.len()` slots from the hint, copying the tail as needed
        let mut hinthead = if let Some(ref mut hint) = hint {
            if !exps.is_empty() {
                hint.ensure_at(exps.len() - 1);
            }
            // need to collect to a vector so that `hinthead` is independent from `hint`
            Some(hint.head.drain(..exps.len()).collect::<Vec<_>>().into_iter())
        } else {
            None
        };

        // visit each expression except for the last
        let mut head = Vec::new();
        let mut exprexit = ExprExit::None;
        for exp in exps {
            let hint = hinthead.as_mut().map(|it| {
                let slot = it.next().unwrap();
                let slotspan = slot.span;
                SpannedSlotSeq {
                    head: vec![slot],
                    tail: Some(Slot::dummy().without_loc()),
                    span: slotspan,
                }
            });
            let Exitable(exit, info) = self.visit_exp(exp, hint)?;
            head.push(info.into_first());
            exprexit = exprexit.collide(exit);
        }

        // for the last expression, use the entire remaining sequence as a hint
        // and expand its result to the final sequence
        let Exitable(exit, last) = self.visit_exp(lastexp, hint)?;
        head.extend(last.head.into_iter());
        exprexit = exprexit.collide(exit);

        Ok(exprexit.with(SpannedSlotSeq { head: head, tail: last.tail, span: expspan }))
    }

    fn visit_kind(&mut self, modf: M, kind: &Spanned<Kind>) -> Result<Spanned<Slot>> {
        let ty = Ty::from_kind(kind, &mut self.env)?;
        Ok(Slot::new(F::from(modf), ty).with_loc(kind))
    }

    fn visit_type_spec<X>(&mut self,
                          spec: &TypeSpec<Spanned<X>>) -> Result<Option<SlotSpec>> {
        // no modifier and kind requested, this should be considered a missing type spec
        if spec.modf == MM::None && spec.kind.is_none() {
            return Ok(None);
        }

        let (explicit, ty) = if let Some(ref kind) = spec.kind {
            (true, Ty::from_kind(kind, &mut self.env)?.with_loc(kind))
        } else {
            (false, Ty::new(T::TVar(self.types().gen_tvar())).with_loc(&spec.base))
        };
        let slot = ty.map(|ty| Slot::new(F::from(spec.modf), ty));

        self.register_module_if_needed(&slot);

        if explicit {
            Ok(Some(SlotSpec::Explicit(slot)))
        } else {
            Ok(Some(SlotSpec::Implicit(slot)))
        }
    }

    fn register_module_if_needed(&mut self, slot: &Slot) {
        if slot.flex() == F::Module {
            debug!("registering {:?} to the current scope", slot);
            let key = &*slot.unlift() as *const Ty;
            let modules = self.pending_modules.last_mut().unwrap();
            modules.insert(key, PendingModule::new(slot.clone()));
        }
    }

    fn collect_type_from_exp(&mut self, exp: &'inp Spanned<Exp>)
            -> Result<(Option<Spanned<Slot>>, SpannedSlotSeq)> {
        if let Ex::FuncCall(ref func, ref args) = *exp.base {
            let Exitable(_, funcseq) = self.visit_exp(func, None)?;
            let funcspan = funcseq.all_span();
            let funcinfo = funcseq.into_first();
            let funcinfo = funcinfo.unlift();
            let typeofexp = if funcinfo.tag() == Some(Tag::Type) {
                // there should be a single argument there
                match args.base {
                    Args::List(ref args) if args.len() >= 1 => {
                        let Exitable(_, info) = self.visit_exp(&args[0], None)?;
                        Some(info.into_first())
                    },
                    Args::List(_) => {
                        self.env.error(exp, m::BuiltinGivenLessArgs { name: "type", nargs: 1 })
                                .done()?;
                        None
                    },
                    Args::Str(ref s) => {
                        let argstr = Str::from(s[..].to_owned());
                        Some(Slot::just(Ty::new(T::Str(Cow::Owned(argstr)))).with_loc(args))
                    },
                    Args::Table(ref fields) => {
                        let Exitable(_, table) = self.visit_table(fields, args.span, None)?;
                        Some(Slot::just(Ty::new(table)).with_loc(args))
                    },
                }
            } else {
                None
            };
            let Exitable(_, seq) = self.visit_func_call(&funcinfo.clone().with_loc(funcspan),
                                                        None, args, exp.span)?;
            Ok((typeofexp, seq.all_with_loc(exp)))
        } else {
            let Exitable(_, seq) = self.visit_exp(exp, None)?;
            Ok((None, seq))
        }
    }

    // similar to visit_exp but also tries to collect Cond
    fn collect_conds_from_exp(&mut self, exp: &'inp Spanned<Exp>)
            -> Result<(Option<Cond>, SpannedSlotSeq)> {
        debug!("collecting conditions from exp {:?}", *exp);

        match *exp.base {
            Ex::Un(Spanned { base: UnOp::Not, .. }, ref e) => {
                let (cond, seq) = self.collect_conds_from_exp(e)?;
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
                let info = self.check_un_op(UnOp::Not, &info, exp.span)?;
                Ok((cond, SpannedSlotSeq::from(info.with_loc(exp))))
            }

            Ex::Bin(ref l, Spanned { base: BinOp::Eq, .. }, ref r) => {
                let (lty, linfo) = self.collect_type_from_exp(l)?;
                let (rty, rinfo) = self.collect_type_from_exp(r)?;

                let linfo = linfo.into_first();
                let rinfo = rinfo.into_first();

                // detect an expression of the form `type(x) == y`.
                // it is technically possible to detect `type(x) == type(y)` as well,
                // but it is not common and results in a very subtle semi-equivalence condition
                // that we cannot readily handle.
                let cond = match (lty, rty) {
                    (Some(ty), None) => {
                        if let Some(flags) = self.literal_ty_to_flags(&rinfo)? {
                            Some(Cond::Flags(ty, flags))
                        } else {
                            None // the rhs is not a literal, so we don't what it is
                        }
                    },
                    (None, Some(ty)) => {
                        if let Some(flags) = self.literal_ty_to_flags(&linfo)? {
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
                let (lcond, lseq) = self.collect_conds_from_exp(l)?;
                let (rcond, rseq) = self.collect_conds_from_exp(r)?;

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
                let info = self.check_bin_op(&linfo, BinOp::And, &rinfo, exp.span)?;
                Ok((cond, SpannedSlotSeq::from(info.with_loc(exp))))
            }

            Ex::Bin(ref l, Spanned { base: BinOp::Or, .. }, ref r) => {
                let (lcond, lseq) = self.collect_conds_from_exp(l)?;
                let (rcond, rseq) = self.collect_conds_from_exp(r)?;

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
                let info = self.check_bin_op(&linfo, BinOp::Or, &rinfo, exp.span)?;
                Ok((cond, SpannedSlotSeq::from(info.with_loc(exp))))
            }

            _ => {
                let Exitable(_, seq) = self.visit_exp(exp, None)?;
                let info = seq.into_first();
                // XXX should detect non-local slots and reject them!
                // probably we can do that via proper weakening, but who knows.
                Ok((Some(Cond::Flags(info.clone(), T_TRUTHY)), SpannedSlotSeq::from(info)))
            }
        }
    }

    fn assert_cond(&mut self, cond: Cond, negated: bool) -> Result<()> {
        debug!("asserting condition {:?} (negated {:?})", cond, negated);

        match cond {
            Cond::Flags(info, flags) => {
                let flags = if negated { !flags } else { flags };
                // XXX this is temporary, the entire condition assertion should be changed!
                info.filter_by_flags(flags, self.types()).map_err(|_| kailua_diag::Stop)?;
                debug!("resulted in {:?}", info);
            }

            Cond::And(lcond, rcond) => {
                if !negated {
                    self.assert_cond(*lcond, negated)?;
                    self.assert_cond(*rcond, negated)?;
                }
            }

            Cond::Or(lcond, rcond) => {
                if negated {
                    self.assert_cond(*lcond, negated)?;
                    self.assert_cond(*rcond, negated)?;
                }
            }

            Cond::Not(cond) => {
                self.assert_cond(*cond, !negated)?;
            }
        }

        Ok(())
    }

    fn literal_ty_to_flags(&self, info: &Spanned<Slot>) -> Result<Option<Flags>> {
        if let Some(s) = info.unlift().as_string() {
            let tyname = &s[..];
            let flags = match tyname {
                b"nil" => T_NOISY_NIL,
                b"number" => T_NUMBER,
                b"string" => T_STRING,
                b"boolean" => T_BOOLEAN,
                b"table" => T_TABLE,
                b"function" => T_FUNCTION,
                b"thread" => T_THREAD,
                b"userdata" => T_USERDATA,
                _ => {
                    self.env.error(info, m::UnknownLiteralTypeName {}).done()?;
                    return Ok(None);
                }
            };
            Ok(Some(flags))
        } else {
            Ok(None)
        }
    }

    // AssertType tag accepts more strings than Type
    fn ext_literal_ty_to_flags(&self, info: &Spanned<Slot>) -> Result<Option<Flags>> {
        if let Some(s) = info.unlift().as_string() {
            let tyname = &s[..];
            let (tyname, nilflags) = if tyname.ends_with(b"?") {
                (&tyname[..tyname.len()-1], T_NOISY_NIL)
            } else {
                (tyname, T_NONE)
            };
            let flags = match tyname {
                b"nil" => T_NOISY_NIL,
                b"int" | b"integer" => T_INTEGER, // XXX the real impl should follow
                b"number" => T_NUMBER,
                b"string" => T_STRING,
                b"boolean" => T_BOOLEAN,
                b"table" => T_TABLE,
                b"function" => T_FUNCTION,
                b"thread" => T_THREAD,
                b"userdata" => T_USERDATA,
                _ => {
                    self.env.error(info, m::UnknownLiteralTypeName {}).done()?;
                    return Ok(None);
                }
            };
            Ok(Some(nilflags | flags))
        } else {
            Ok(None)
        }
    }
}

