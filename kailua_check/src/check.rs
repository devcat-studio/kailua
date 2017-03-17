use std::i32;
use std::cmp;
use std::ops::{Deref, DerefMut};
use std::borrow::Cow;
use std::collections::HashMap;
use take_mut::take;

use kailua_env::{Span, Spanned, WithLoc};
use kailua_diag::{self, Report, Reporter};
use kailua_syntax::{Str, Name, NameRef, Var, TypeSpec, Kind, Sig, Ex, Exp, UnOp, BinOp};
use kailua_syntax::{SelfParam, TypeScope, Args, St, Stmt, Block, K};
use diag::{CheckResult, TypeReport, TypeReportHint, TypeReportMore, Displayed, Display};
use ty::{Dyn, Nil, T, Ty, TySeq, SpannedTySeq, Lattice, Union, TypeContext};
use ty::{Key, Tables, Function, Functions};
use ty::{F, Slot, SlotSeq, SpannedSlotSeq, Tag, Class, ClassId};
use ty::flags::*;
use env::{Env, Returns, Frame, Scope, Context};
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

struct ScopedChecker<'chk, 'envr: 'chk, 'env: 'envr, R: 'env + Report> {
    checker: &'chk mut Checker<'envr, 'env, R>
}

impl<'chk, 'envr, 'env, R: Report> Deref for ScopedChecker<'chk, 'envr, 'env, R> {
    type Target = &'chk mut Checker<'envr, 'env, R>;
    fn deref(&self) -> &Self::Target { &self.checker }
}

impl<'chk, 'envr, 'env, R: Report> DerefMut for ScopedChecker<'chk, 'envr, 'env, R> {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.checker }
}

impl<'chk, 'envr, 'env, R: Report> Drop for ScopedChecker<'chk, 'envr, 'env, R> {
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

pub struct Checker<'envr, 'env: 'envr, R: 'env> {
    env: &'envr mut Env<'env, R>,
}

impl<'envr, 'env, R: Report> Checker<'envr, 'env, R> {
    pub fn new(env: &'envr mut Env<'env, R>) -> Checker<'envr, 'env, R> {
        Checker { env: env }
    }

    fn context(&mut self) -> &mut Context<R> {
        self.env.context()
    }

    fn display<'a, 'c, T: Display>(&'c self, x: &'a T) -> Displayed<'a, 'c, T> {
        self.env.display(x)
    }

    fn scoped<'chk>(&'chk mut self, scope: Scope) -> ScopedChecker<'chk, 'envr, 'env, R> {
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

    fn check_un_op(&mut self, op: UnOp, info: &Spanned<Slot>,
                   expspan: Span) -> CheckResult<Slot> {
        let finalize = |r: TypeReport, checker: &mut Checker<R>| {
            checker.env.error(expspan,
                              m::WrongUnaryOperand { op: op.symbol(),
                                                     ty: checker.display(info) })
                       .report_types(r, TypeReportHint::None)
                       .done()
        };

        macro_rules! assert_sub {
            ($lhs:expr, $rhs:expr) => {
                match $lhs.assert_sub($rhs, self.context()) {
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
                    expspan: Span) -> CheckResult<Slot> {
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
                match ($lhs1.assert_sub($rhs, self.context()),
                       $lhs2.assert_sub($rhs, self.context())) {
                    (Ok(()), Ok(())) => {}
                    (r1, r2) => { finalize2(r1.err(), r2.err(), self)?; }
                }
            }
        }

        macro_rules! union {
            ($lhs:expr, $rhs:expr, $explicit:expr) => {
                match $lhs.union($rhs, $explicit, self.context()) {
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
                      methodcall: bool) -> CheckResult<TySeq> {
        debug!("checking if {:?} can be called with {:?} ({})",
               func, args, if methodcall { "method call" } else { "func call" });

        // check for `[internal constructor] <#x>`
        // (should be done before the resolution, as it is likely to fail!)
        if func.tag() == Some(Tag::Constructor) {
            self.env.error(func, m::CannotCallCtor {}).done()?;
            return Ok(TySeq::dummy());
        }

        let functy = if let Some(func) = self.env.resolve_exact_type(&func) {
            func
        } else {
            self.env.error(func, m::CallToInexactType { func: self.display(func) }).done()?;
            return Ok(TySeq::dummy());
        };

        // check if generalize(func.args) :> args and gather generalize(func.returns)
        let mut returns = match *functy.get_functions().unwrap() {
            Functions::Simple(ref f) => {
                let generalize_tyseq = |seq: &TySeq, ctx: &mut TypeContext| {
                    let head = seq.head.iter().map(|t| t.clone().generalize(ctx)).collect();
                    let tail = seq.tail.as_ref().map(|t| t.clone().generalize(ctx));
                    TySeq { head: head, tail: tail }
                };

                let funcargs = generalize_tyseq(&f.args, self.context()).all_without_loc();
                if let Err(r) = args.assert_sub(&funcargs, self.context()) {
                    let hint = if methodcall {
                        TypeReportHint::MethodArgs
                    } else {
                        TypeReportHint::FuncArgs
                    };
                    self.env.error(func, m::CallToWrongType { func: self.display(func) })
                            .report_types(r, hint)
                            .done()?;
                    return Ok(TySeq::dummy());
                }
                generalize_tyseq(&f.returns, self.context())
            },

            Functions::All => {
                self.env.error(func, m::CallToAnyFunc { func: self.display(func) }).done()?;
                return Ok(TySeq::dummy());
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

                if !tab.is_tabular() { return; }

                let (k, v) = match tab.clone().unwrap() {
                    // map<k, v> -> (k, v)
                    T::Tables(Cow::Owned(Tables::Map(k, v))) =>
                        (k, v),
                    T::Tables(Cow::Borrowed(&Tables::Map(ref k, ref v))) =>
                        (k.clone(), v.clone()),

                    // vector<v> -> (integer, v)
                    T::Tables(Cow::Owned(Tables::Array(v))) =>
                        (Ty::new(T::Integer), v),
                    T::Tables(Cow::Borrowed(&Tables::Array(ref v))) =>
                        (Ty::new(T::Integer), v.clone()),

                    _ => return,
                };

                // fix `returns` in place
                let knil = k.clone().with_nil();
                let v = v.unlift().clone().without_nil();
                *returns.ensure_at_mut(0) = Ty::new(T::func(Function {
                    args: TySeq { head: vec![tab.clone(), k.clone()], tail: None },
                    returns: TySeq { head: vec![knil, v], tail: None },
                }));
                *returns.ensure_at_mut(1) = tab;
                *returns.ensure_at_mut(2) = k;
            })();
        }

        Ok(returns)
    }

    // common routine for check_{l,r}val_index
    // when lval is true, the field is created as needed (otherwise it's an error)
    // when lval is false, the missing field is returned as Index::Missing
    fn check_index_common(&mut self, ety0: &Spanned<Slot>, kty0: &Spanned<Slot>, expspan: Span,
                          lval: bool) -> CheckResult<Index> {
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
        if lval && ety0.accept_in_place(self.context()).is_err() {
            self.env.error(&*ety0, m::CannotUpdateConst { tab: self.display(&*ety0) }).done()?;
            return Ok(Index::dummy());
        }

        let new_slot = |context: &mut Context<R>| {
            // we don't yet know the exact value type, so generate a new type variable
            let tvar = T::TVar(context.gen_tvar());
            Slot::new(F::Var, Ty::new(tvar))
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

        let ety = if let Some((cid, proto)) = clsinfo {
            // nominal types cannot be indexed with non-compile-time values
            // (in some sense, nominal types are isomorphic to records)
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
                ($x:ident) => (self.context().get_class_mut(cid).expect("invalid ClassId").$x)
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
                        let ty = T::TVar(self.context().gen_tvar());
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
                        let slot = new_slot(self.context());
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
                        // the constructor (checked earlier) can add Currently slots to instances.
                        let slot = new_slot(self.context());
                        fields!(instance_ty).insert(litkey, slot.clone());
                        (slot, true)
                    }
                };

                vslot.adapt(ety0.flex(), self.context());
                if new {
                    return Ok(Index::Created(vslot));
                } else {
                    return Ok(Index::Found(vslot));
                }
            } else {
                // r-values. we just pick the method from the template.
                trace!("indexing to a field {:?} of the class {} of {:?}",
                       litkey, if proto { "prototype" } else { "instance" }, cid);

                let cdef = self.context().get_class_mut(cid).expect("invalid ClassId");
                // TODO should we re-adapt methods?
                if !proto {
                    // instance fields have a precedence over class fields
                    if let Some(info) = cdef.instance_ty.get(&litkey).map(|v| (*v).clone()) {
                        return Ok(Index::Found(info));
                    }
                }
                if let Some(info) = cdef.class_ty.get(&litkey).map(|v| (*v).clone()) {
                    return Ok(Index::Found(info));
                } else {
                    return Ok(Index::Missing);
                }
            }
        } else if !flags.is_dynamic() && flags.intersects(T_STRING) {
            // if ety is a string, we go through the previously defined string metatable
            // note that ety may not be fully resolved here!
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
                if let Some(ety) = self.env.resolve_exact_type(&ety0.unlift()) {
                    // now the metatable should be a table proper
                    if !ety.is_dynamic() && ety.flags().intersects(!T_TABLE) {
                        self.env.error(&*ety0, m::NonTableStringMeta {})
                                .note(meta.span, m::PreviousStringMeta {})
                                .done()?;
                        return Ok(Index::dummy());
                    }

                    ety
                } else {
                    self.env.error(&*ety0, m::IndexToInexactType { tab: self.display(&*ety0) })
                            .done()?;
                    return Ok(Index::dummy());
                }
            } else {
                self.env.error(&*ety0, m::UndefinedStringMeta {}).done()?;
                return Ok(Index::dummy());
            }
        } else {
            // normal tables, we need to resolve it fully
            if let Some(ety) = self.env.resolve_exact_type(&ety) {
                ety
            } else {
                self.env.error(&*ety0,
                               m::IndexToInexactType { tab: self.display(&*ety0) }).done()?;
                return Ok(Index::dummy());
            }
        };

        // this also handles the case where the string metatable itself is dynamic
        if let Some(dyn) = flags.get_dynamic() {
            let value = Slot::just(Ty::new(T::Dynamic(dyn)));
            // the flex should be retained
            if lval { value.adapt(ety0.flex(), self.context()); }
            return Ok(Index::Found(value));
        }

        macro_rules! check {
            ($sub:expr) => {
                match $sub {
                    Ok(v) => v,
                    Err(_) => {
                        self.env.error(expspan, m::CannotIndex { tab: self.display(&*ety0),
                                                                 key: self.display(kty0) })
                                .done()?;
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
            match (ety.get_tables(), lval) {
                (Some(&Tables::Fields(ref rvar)), lval) => {
                    // find a field in the rvar
                    let mut vslot = None;
                    let _ = self.context().list_rvar_fields(rvar.clone(), &mut |k, v| {
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
                            let vslot = new_slot(self.context());
                            check!(self.context().assert_rvar_includes(rvar.clone(),
                                                                       &[(litkey, vslot.clone())]));
                            (vslot, true)
                        },

                        // the field does not exist and is used as an r-value, return nothing
                        (None, false) => return Ok(Index::Missing),
                    };

                    vslot.adapt(ety0.flex(), self.context());
                    if new {
                        return Ok(Index::Created(vslot));
                    } else {
                        return Ok(Index::Found(vslot));
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
                if lval { value.adapt(ety0.flex(), self.context()); }
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

            Some(&Tables::Array(ref value)) if intkey => {
                if lval { value.adapt(ety0.flex(), self.context()); }
                Ok(Index::Found((*value).clone().with_nil()))
            },

            Some(&Tables::Array(..)) => {
                self.env.error(expspan,
                               m::IndexToArrayWithNonInt { tab: self.display(&*ety0),
                                                           key: self.display(&kty) })
                        .done()?;
                Ok(Index::dummy())
            },

            Some(&Tables::Map(ref key, ref value)) => {
                check!(kty.assert_sub(&**key, self.context()));
                if lval { value.adapt(ety0.flex(), self.context()); }
                Ok(Index::Found((*value).clone().with_nil()))
            },

            Some(&Tables::All) => {
                self.env.error(&*ety0,
                               m::IndexToAnyTable { tab: self.display(&*ety0) }).done()?;
                Ok(Index::dummy())
            },
        }
    }

    fn check_rval_index(&mut self, ety: &Spanned<Slot>, kty: &Spanned<Slot>,
                        expspan: Span) -> CheckResult<Slot> {
        match self.check_index_common(ety, kty, expspan, false)? {
            Index::Missing => {
                self.env.error(expspan, m::CannotIndex { tab: self.display(&ety),
                                                         key: self.display(&kty) })
                        .done()?;
                Ok(Slot::dummy())
            },
            Index::Created(..) => unreachable!(),
            Index::Found(slot) => Ok(slot),
        }
    }

    // this should be followed by assign_to_lval_index
    fn check_lval_index(&mut self, ety: &Spanned<Slot>, kty: &Spanned<Slot>,
                        expspan: Span) -> CheckResult<(bool, Slot)> {
        match self.check_index_common(ety, kty, expspan, true)? {
            Index::Missing => unreachable!(),
            Index::Created(slot) => Ok((false, slot)),
            Index::Found(slot) => Ok((true, slot)),
        }
    }

    // this should be preceded by check_lval_index with the equal parameters
    fn assign_to_lval_index(&mut self, found: bool, ety: &Spanned<Slot>, kty: &Spanned<Slot>,
                            lhs: &Spanned<Slot>, initrhs: &Spanned<Slot>,
                            specrhs: Option<&Spanned<Slot>>) -> CheckResult<()> {
        if found {
            // ignore specrhs, should have been handled by the caller
            self.env.assign(lhs, initrhs)?;
        } else {
            if self.env.assign_new(lhs, initrhs, specrhs).is_err() {
                self.env.error(lhs,
                               m::CannotCreateIndex {
                                   tab: self.display(ety), key: self.display(kty),
                                   specrhs: self.display(specrhs.unwrap_or(initrhs)),
                               })
                        .done()?;
            }
        }
        Ok(())
    }

    fn assume_field_slot(&mut self, static_: bool, rootname: &Spanned<NameRef>,
                         names: &[Spanned<Name>], namespan: Span, slot: Slot) -> CheckResult<Slot> {
        assert!(!names.is_empty());

        // if we are updating a table, we need to sever any row variable connections
        // between the new table and the old table (otherwise we may be silently
        // updating a row variable).

        #[derive(Clone, Debug)]
        enum TableExtract {
            // a record with flexibility & nilability (both for reconstruction),
            // all fields except `next_key` and the slot for `next_key` if any
            Rec(F, Nil, Vec<(Key, Slot)>, Option<Slot>),

            // a class prototype, only possible at the rootname
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

                let mut fields = env.context().get_rvar_fields(rvar.clone());
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

        // resolve rootname
        // (we are keeping root to return it on an error---the new scoped id should retain that)
        let root = if self.env.get_var(rootname).is_some() {
            self.env.ensure_var(rootname)?
        } else {
            self.env.error(rootname, m::NoVar { name: self.env.get_name(rootname) }).done()?;
            return Ok(Slot::dummy());
        };

        let mut table;
        let mut span = rootname.span; // `a`, `a.b`, `a.b.c`, ...
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

                let mut def = self.context().get_class_mut(cid).expect("invalid ClassId");
                let firstname = Key::Str(firstname.base.clone().into());
                if static_ {
                    def.class_ty.insert(firstname, slot);
                } else {
                    def.instance_ty.insert(firstname, slot);
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
            let rvar = self.context().gen_rvar();
            self.context().assert_rvar_includes(rvar.clone(), &fields).expect(
                "cannot insert updated disjoint fields into a fresh row variable"
            );
            slot = Slot::new(flex,
                             Ty::new(T::Tables(Cow::Owned(Tables::Fields(rvar)))).or_nil(nil));
        }

        // the final table should be assumed back to the current scope
        Ok(slot)
    }

    fn check_assign(&mut self, vars: &Spanned<Vec<TypeSpec<Spanned<Var>>>>,
                    exps: Option<&Spanned<Vec<Spanned<Exp>>>>) -> CheckResult<()> {
        #[derive(Debug)]
        enum VarRef<'a> {
            Name(&'a Spanned<NameRef>),
            // is not newly created?, table slot, key slot, indexd slot
            Slot(bool, Spanned<Slot>, Spanned<Slot>, Spanned<Slot>),
        }

        let varrefspecs = vars.iter().map(|varspec| {
            let varref = match varspec.base.base {
                Var::Name(ref nameref) => VarRef::Name(nameref),

                Var::Index(ref e, ref key) => {
                    let ty = self.visit_exp(e, None)?.into_first();
                    let kty = self.visit_exp(key, None)?.into_first();
                    let (found, slot) = self.check_lval_index(&ty, &kty, varspec.base.span)?;
                    VarRef::Slot(found, ty, kty, slot.with_loc(&varspec.base))
                },

                Var::IndexName(ref e, ref key) => {
                    let ty = self.visit_exp(e, None)?.into_first();
                    let keystr = Str::from(key.base[..].to_owned());
                    let kty = Slot::just(Ty::new(T::Str(Cow::Owned(keystr)))).with_loc(key);
                    let (found, slot) = self.check_lval_index(&ty, &kty, varspec.base.span)?;
                    VarRef::Slot(found, ty, kty, slot.with_loc(&varspec.base))
                },
            };

            let varspec = if let Some(ref kind) = varspec.kind {
                Some(self.visit_kind(F::from(varspec.modf), kind)?)
            } else {
                None
            };

            Ok((varref, varspec))
        }).collect::<CheckResult<Vec<_>>>()?;

        // the incomplete expression is parsed as Assign without rhs,
        // so we won't generate further errors due to mismatching types
        let mut infos = if let Some(ref exps) = exps {
            // construct hints to derive type inference
            let knowntypes = varrefspecs.iter().map(|&(ref varref, ref varspec)| {
                if let Some(ref spec) = *varspec {
                    spec.clone()
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
                        VarRef::Slot(_, _, _, ref slot) => slot.clone(),
                    }
                }
            }).collect();

            let hint = SpannedSlotSeq { head: knowntypes, tail: None, span: vars.span };
            Some(self.visit_explist(exps, Some(hint))?.into_iter_with_nil())
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
                    let varslot = self.env.get_var(nameref).unwrap().slot.slot().cloned();
                    if let Some(varslot) = varslot {
                        let varslot = varslot.with_loc(nameref);
                        self.context().spanned_slots_mut().insert(varslot);
                    }
                }

                // indexed assignment
                VarRef::Slot(found, ety, kty, slot) => {
                    if found {
                        if let Some(ref specinfo) = specinfo {
                            // now we know that this assignment cannot declare a field
                            self.env.error(specinfo, m::TypeSpecToIndex {}).done()?;
                        }
                    }
                    if let Some(info) = info {
                        self.assign_to_lval_index(found, &ety, &kty, &slot,
                                                  &info, specinfo.as_ref())?;
                    }
                    // allow lhs to be recorded even when info is missing (for completion)
                    self.context().spanned_slots_mut().insert(slot);
                }
            }
        }

        Ok(())
    }

    #[cfg(feature = "no_implicit_func_sig")]
    fn error_on_implicit_sig(&mut self, sig: &Sig) -> CheckResult<()> {
        if sig.args.head.iter().any(|spec| spec.kind.is_none()) {
            self.env.error(&sig.args, m::ImplicitSigOnNamedFunc {}).done()?;
        }
        Ok(())
    }

    #[cfg(not(feature = "no_implicit_func_sig"))]
    fn error_on_implicit_sig(&mut self, _sig: &Sig) -> CheckResult<()> {
        Ok(())
    }

    pub fn visit(&mut self, chunk: &Spanned<Block>) -> CheckResult<()> {
        self.visit_block(chunk)?;
        Ok(())
    }

    fn visit_block(&mut self, block: &Spanned<Block>) -> CheckResult<Exit> {
        let mut scope = self.scoped(Scope::new());
        let mut exit = Exit::None;
        let mut ignored_stmts: Option<Span> = None;
        for stmt in &block.base {
            if exit != Exit::None {
                ignored_stmts = Some(ignored_stmts.unwrap_or(Span::dummy()) | stmt.span);
                // the exit return can no longer affect this block's return
                scope.visit_stmt(stmt)?;
            } else {
                exit = scope.visit_stmt(stmt)?;
            }
        }
        #[cfg(feature = "warn_on_dead_code")] {
            if let Some(span) = ignored_stmts {
                scope.env.warn(span, m::DeadCode {}).done()?;
            }
        }
        Ok(exit)
    }

    fn visit_stmt(&mut self, stmt: &Spanned<Stmt>) -> CheckResult<Exit> {
        debug!("visiting stmt {:?}", *stmt);

        match *stmt.base {
            // it should not happen, but for the purpose of checker, the error nodes are ignored
            St::Oops => Ok(Exit::None),

            St::Void(ref exp) => {
                self.visit_exp(exp, None)?;
                Ok(Exit::None)
            }

            St::Assign(ref vars, ref exps) => {
                self.check_assign(vars, exps.as_ref())?;
                Ok(Exit::None)
            }

            St::Do(ref block) => self.visit_block(block),

            St::While(ref cond, ref block) => {
                let ty = self.visit_exp(cond, None)?;
                let boolean = self.check_bool(ty.unspan().unlift());
                match boolean {
                    Bool::Truthy => { // infinite loop
                        let exit = self.visit_block(block)?;
                        if exit >= Exit::Return { Ok(exit) } else { Ok(Exit::Stop) }
                    },
                    Bool::Falsy => {
                        Ok(Exit::None)
                    },
                    Bool::Unknown => {
                        self.visit_block(block)?;
                        Ok(Exit::None)
                    },
                }
            }

            St::Repeat(ref block, ref cond) => {
                let exit = self.visit_block(block)?;
                let ty = self.visit_exp(cond, None)?;
                if exit == Exit::None {
                    match self.check_bool(ty.unspan().unlift()) {
                        Bool::Truthy => Ok(Exit::Stop),
                        Bool::Falsy => Ok(exit),
                        Bool::Unknown => Ok(Exit::None),
                    }
                } else {
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

                    let ty = self.visit_exp(cond, None)?;
                    let boolean = self.check_bool(ty.unspan().unlift());
                    match boolean {
                        Bool::Truthy => {
                            ignored_blocks = Some((cond.span, Span::dummy()));
                            exit = exit.or(self.visit_block(block)?);
                        }
                        Bool::Falsy => {
                            #[cfg(feature = "warn_on_useless_conds")] {
                                self.env.warn(span, m::IgnoredIfCase {})
                                        .note(cond, m::IfCaseWithFalsyCond {})
                                        .done()?;
                            }
                        }
                        Bool::Unknown => {
                            exit = exit.or(self.visit_block(block)?);
                        }
                    }
                }

                if let &Some(ref block) = lastblock {
                    if let Some((_, ref mut blocks_span)) = ignored_blocks {
                        *blocks_span |= block.span;
                    } else {
                        exit = exit.or(self.visit_block(block)?);
                    }
                } else {
                    if ignored_blocks.is_none() {
                        exit = Exit::None;
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
                let start = self.visit_exp(start, None)?.into_first().map(|s| s.unlift().clone());
                let end = self.visit_exp(end, None)?.into_first().map(|s| s.unlift().clone());
                let step = if let &Some(ref step) = step {
                    self.visit_exp(step, None)?.into_first().map(|s| s.unlift().clone())
                } else {
                    Ty::new(T::Integer).without_loc() // to simplify the matter
                };

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
                match (start.assert_sub(&indty.clone().without_loc(), self.context()),
                       end.assert_sub(&indty.clone().without_loc(), self.context()),
                       step.assert_sub(&indty.clone().without_loc(), self.context())) {
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

                let mut scope = self.scoped(Scope::new());
                let indty = Slot::var(Ty::new(indty));
                let nameref = NameRef::Local(localname.base.clone()).with_loc(localname);
                scope.env.add_var(&nameref, None, Some(indty.without_loc()))?;
                let exit = scope.visit_block(block)?;
                if exit >= Exit::Return { Ok(exit) } else { Ok(Exit::None) }
            }

            St::ForIn(ref names, ref exps, _blockscope, ref block) => {
                let infos = self.visit_explist(exps, None)?;
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
                    let indvar = T::TVar(self.context().gen_tvar());

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
                    let mut returns = self.check_callable(&func.clone().with_loc(expspan),
                                                          &args, false)?;
                    if let Err(r) = last.assert_sub(&indvar, self.context()) {
                        // it is very hard to describe, but it is conceptually
                        // an extension of check_callable
                        self.env.error(expspan, m::BadFuncIterator { iter: self.display(&func) })
                                .report_types(r, TypeReportHint::None)
                                .done()?;
                    }

                    // note that we ignore indvar here. it is only kept internally and
                    // not visible outside; returns is what we should assign to variables!
                    // we should still account for the fact that the first value cannot be nil.
                    take(returns.ensure_at_mut(0), |t| t.without_nil());

                    indtys = returns;
                }

                let mut scope = self.scoped(Scope::new());
                for (localname, ty) in names.iter().zip(indtys.into_iter_with_nil()) {
                    let ty = Slot::var(ty);
                    let nameref = NameRef::Local(localname.base.clone()).with_loc(localname);
                    scope.env.add_var(&nameref, None, Some(ty.without_loc()))?;
                }
                let exit = scope.visit_block(block)?;
                if exit >= Exit::Return { Ok(exit) } else { Ok(Exit::None) }
            }

            St::FuncDecl(ref name, ref sig, _blockscope, ref block, nextscope) => {
                self.error_on_implicit_sig(sig)?;

                // `name` itself is available to the inner scope
                let funcv = self.context().gen_tvar();
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
                let functy = self.visit_func_body(None, sig, block, stmt.span, None)?;
                if let Err(r) = T::TVar(funcv).assert_eq(&*functy.unlift(), self.context()) {
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

                // now prepare the right-hand side (i.e. method decl)
                let method = meths.last().unwrap();
                let selfinfo = if let Some(ref selfparam) = *selfparam {
                    let given = if let Some(ref kind) = selfparam.kind {
                        let ty = Ty::from_kind(kind, &mut self.env)?;
                        let flex = F::from(selfparam.modf);
                        Some((flex, ty))
                    } else {
                        None
                    };

                    // if `info` is a class prototype we know the exact type for `self`
                    let inferred = if info.unlift().nil() != Nil::Noisy {
                        // (except when it is unioned with nil)
                        if let T::Class(Class::Prototype(cid)) = **info.unlift() {
                            let inst = T::Class(Class::Instance(cid));
                            if *method.base == *b"init" {
                                // var [constructible] <class instance #cid>
                                Some((F::Var, Ty::new(inst).with_tag(Tag::Constructible)))
                            } else {
                                // var <class instance #cid>
                                Some((F::Var, Ty::new(inst)))
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    let slot = match (given, inferred) {
                        // if both `given` and `inferred` are present, try to unify them
                        (Some(given), Some(inferred)) => {
                            if given.1.assert_eq(&inferred.1, self.context()).is_err() {
                                let span = selfparam.kind.as_ref().unwrap().span;
                                self.env.error(span, m::BadSelfTypeInMethod {}).done()?;
                            }

                            // they may have the same type but different flex.
                            // as given flex is no less stringent than inferred flex,
                            // we overwrite the inferred flex to the given flex.
                            Slot::new(given.0, inferred.1)
                        }

                        // if only one of them is present, use that
                        (Some((flex, ty)), None) | (None, Some((flex, ty))) => {
                            Slot::new(flex, ty)
                        }

                        // if both are missing, we try to use a fresh type variable,
                        // except when [no_check] is requested (requires a fixed type)
                        (None, None) => {
                            if sig.attrs.iter().any(|a| *a.name.base == *b"no_check") {
                                self.env.error(&selfparam.base, m::NoCheckRequiresTypedSelf {})
                                        .done()?;
                            }

                            // var <fresh type variable>
                            let tv = T::TVar(self.context().gen_tvar());
                            Slot::new(F::Var, Ty::new(tv))
                        }
                    };

                    Some((&selfparam.base, slot))
                } else {
                    None
                };
                let methinfo = self.visit_func_body(selfinfo, sig, block, stmt.span, None)?;

                // finally, go through the ordinary table update
                let subspan = info.span | method.span;
                let keystr = Str::from(method.base[..].to_owned());
                let kty = Slot::just(Ty::new(T::Str(Cow::Owned(keystr)))).with_loc(method);
                let (found, slot) = self.check_lval_index(&info, &kty, subspan)?;
                self.assign_to_lval_index(found, &info, &kty, &slot.with_loc(subspan),
                                          &methinfo.with_loc(stmt), None)?;

                Ok(Exit::None)
            }

            St::Local(ref names, ref exps, _nextscope) => {
                // collect specified types first (required for hints)
                let nameinfos = names.iter().map(|namespec| {
                    let info = if let Some(ref kind) = namespec.kind {
                        Some(self.visit_kind(F::from(namespec.modf), kind)?)
                    } else {
                        None
                    };
                    Ok((&namespec.base, info))
                }).collect::<CheckResult<Vec<_>>>()?;

                let hint = SpannedSlotSeq {
                    head: nameinfos.iter().map(|&(name, ref info)| {
                        info.clone().unwrap_or_else(|| Slot::dummy().with_loc(name))
                    }).collect(),
                    tail: None,
                    span: names.span,
                };
                let infos = self.visit_explist(exps, Some(hint))?;

                for ((localname, specinfo), info) in nameinfos.into_iter()
                                                              .zip(infos.into_iter_with_none()) {
                    let nameref = NameRef::Local(localname.base.clone()).with_loc(localname);
                    self.env.add_var(&nameref, specinfo, info)?;
                }
                Ok(Exit::None)
            }

            St::Return(ref exps) => {
                let returns =
                    self.env.get_frame().returns.as_ref().map(|seq| seq.clone().all_without_loc());

                let hint = match returns {
                    Returns::None => None,
                    Returns::Implicit(ref returns) | Returns::Explicit(ref returns) =>
                        Some(SpannedSlotSeq::from_seq(returns.clone())),
                };
                let seq = self.visit_explist(exps, hint)?;

                // function types destroy flexibility, primarily because the return type is
                // a slot only in the inside view. in the outside it's always Var,
                // which should be ensured by `visit_func_call`.
                let seq = seq.unlift();
                match returns {
                    Returns::None => {
                        self.env.get_frame_mut().returns = Returns::Implicit(seq.unspan());
                    }
                    Returns::Implicit(returns) => {
                        // need to infer the return type, but not _that_ much
                        match seq.union(&returns, false, self.context()) {
                            Ok(returns) => {
                                self.env.get_frame_mut().returns =
                                    Returns::Implicit(returns.unspan());
                            }
                            Err(r) => {
                                self.env.error(stmt, m::CannotExtendImplicitReturnType {})
                                        .report_types(r, TypeReportHint::Returns)
                                        .done()?;
                            }
                        };
                    }
                    Returns::Explicit(returns) => {
                        if let Err(r) = seq.assert_sub(&returns, self.context()) {
                            self.env.error(stmt, m::CannotReturn { returns: self.display(&returns),
                                                                   ty: self.display(&seq) })
                                    .report_types(r, TypeReportHint::Returns)
                                    .done()?;
                        }
                    }
                }
                Ok(Exit::Return)
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
                let slot = self.visit_kind(F::from(kindm), kind)?;
                self.env.assume_var(&newname.clone().with_loc(name), slot)?;
                Ok(Exit::None)
            }

            St::KailuaAssumeField(static_, Spanned { base: (ref rootname, ref names), span },
                                  kindm, ref kind) => {
                let slot = self.visit_kind(F::from(kindm), kind)?;
                let newslot = self.assume_field_slot(static_, rootname, names, span, slot.base)?;
                self.env.assume_var(rootname, newslot.with_loc(rootname))?;
                Ok(Exit::None)
            }
        }
    }

    fn visit_func_body(&mut self, selfparam: Option<(&Spanned<SelfParam>, Slot)>, sig: &Sig,
                       block: &Spanned<Vec<Spanned<Stmt>>>, declspan: Span,
                       hint: Option<Spanned<Slot>>) -> CheckResult<Slot> {
        // if no check is requested, the signature should be complete
        let no_check = sig.attrs.iter().any(|a| *a.name.base == *b"no_check");

        // if the hint exists and is a function,
        // collect first `sig.args.head.len()` types for missing argument types,
        // and a repeating part of remaining type sequence for a missing variadic argument type.
        //
        // one edge case: if the signature has `n` arguments plus a variadic argument,
        // and the hint has `m` arguments plus a variadic argument,
        // then when `n < m` the variadic argument would get `n - m` non-repeating types!
        // since this is forbidden from the signature we treat this as an error case
        // and drop the type hint for the variadic argument altogether.
        let hint: Option<(Vec<Ty>, Option<Ty>)> = hint.and_then(|hint| {
            self.env.resolve_exact_type(&hint.unlift()).and_then(|ty| {
                if let Some(&Functions::Simple(ref f)) = ty.get_functions() {
                    let mut args = f.args.clone();
                    if !sig.args.head.is_empty() {
                        args.ensure_at(sig.args.head.len() - 1);
                    }
                    let hinthead = args.head.drain(..sig.args.head.len()).collect();
                    let hinttail = if args.head.is_empty() { args.tail } else { None };
                    Some((hinthead, hinttail))
                } else {
                    None
                }
            })
        });
        let (hinthead, hinttail) = match hint {
            Some((h, t)) => (Some(h), t),
            None => (None, None),
        };

        let vatype = match sig.args.tail {
            None => None,

            Some(None) => {
                // varargs present but types are unspecified
                if no_check {
                    // [no_check] always requires a type
                    self.env.error(declspan, m::NoCheckRequiresTypedVarargs {}).done()?;
                    return Ok(Slot::dummy());
                } else if let Some(hint) = hinttail {
                    // use a hint instead
                    Some(hint)
                } else {
                    #[cfg(feature = "no_implicit_func_sig")] {
                        // implicit signature disabled, raise an error and continue
                        self.env.error(declspan, m::ImplicitVarargsTypeOnAnonymousFunc {}).done()?;
                        Some(Ty::dummy())
                    }
                    #[cfg(not(feature = "no_implicit_func_sig"))] {
                        // fill a fresh type variable in
                        Some(Ty::new(T::TVar(self.context().gen_tvar())))
                    }
                }
            },

            Some(Some(ref k)) => Some(Ty::from_kind(k, &mut self.env)?),
        };

        let vainfo = vatype.clone().map(|t| TySeq { head: Vec::new(), tail: Some(t) });

        // we accumulate all known return types inside the frame
        // then checks if it matches with the `returns`.
        //
        // TODO the exception should be made to the recursive usage;
        // we probably need to put a type variable that is later equated to the actual returns
        let frame = if let Some(ref returns) = sig.returns {
            let returns = TySeq::from_kind_seq(returns, &mut self.env)?;
            Frame { vararg: vainfo, returns: Returns::Explicit(returns) }
        } else if no_check {
            self.env.error(declspan, m::NoCheckRequiresTypedReturns {}).done()?;
            return Ok(Slot::dummy());
        } else {
            Frame { vararg: vainfo, returns: Returns::None }
        };

        let mut argshead = Vec::new();

        let mut scope = self.scoped(Scope::new_function(frame));
        if let Some((selfparam, selfinfo)) = selfparam {
            let ty = selfinfo.unlift().clone();
            let selfid = selfparam.clone().map(|param| param.0);
            scope.env.add_local_var_already_set(&selfid, selfinfo.without_loc())?;
            argshead.push(ty);
        }

        let mut hinthead = hinthead.map(|tys| tys.into_iter());
        for param in &sig.args.head {
            // need to consume the iterator in lockstep
            let hint = hinthead.as_mut().map(|it| it.next().unwrap());

            let ty;
            let sty;
            if let Some(ref kind) = param.kind {
                // the type has been specified
                ty = Ty::from_kind(kind, &mut scope.env)?;
                let flex = F::from(param.modf);
                sty = Slot::new(flex, ty.clone());
            } else if no_check {
                // [no_check] always requires a type
                scope.env.error(&param.base, m::NoCheckRequiresTypedArgs {}).done()?;
                return Ok(Slot::dummy());
            } else if let Some(hint) = hint {
                // use a hint instead
                ty = hint;
                sty = Slot::new(F::Var, ty.clone());
            } else {
                #[cfg(feature = "no_implicit_func_sig")] {
                    // implicit signature disabled, raise an error and continue
                    scope.env.error(&param.base, m::ImplicitArgTypeOnAnonymousFunc {}).done()?;
                    ty = Ty::dummy();
                    sty = Slot::dummy();
                }
                #[cfg(not(feature = "no_implicit_func_sig"))] {
                    // fill a fresh type variable in
                    let argv = scope.context().gen_tvar();
                    ty = Ty::new(T::TVar(argv));
                    sty = Slot::new(F::Var, Ty::new(T::TVar(argv)));
                }
            }

            scope.env.add_local_var_already_set(&param.base, sty.without_loc())?;
            argshead.push(ty);
        }
        let args = TySeq { head: argshead, tail: vatype };

        if !no_check {
            if let Exit::None = scope.visit_block(block)? {
                // the last statement is an implicit return
                let pos = block.span.end(); // the span is conceptually at the end of block
                let ret = Box::new(St::Return(Vec::new().with_loc(pos))).with_loc(pos);
                scope.visit_stmt(&ret)?;
            }
        }

        let returns = match scope.env.get_frame().returns {
            Returns::Implicit(ref returns) | Returns::Explicit(ref returns) => returns.clone(),
            Returns::None => {
                // the function is diverging. we don't have the exact bottom type
                // (`nil!` can be coerced to `nil`) but we at least try.
                let bottom = Ty::new(T::None).or_nil(Nil::Absent);
                TySeq { head: Vec::new(), tail: Some(bottom) }
            },
        };
        Ok(Slot::just(Ty::new(T::func(Function { args: args, returns: returns }))))
    }

    fn visit_func_call(&mut self, functy: &Spanned<Ty>, selfinfo: Option<Spanned<Slot>>,
                       args: &Spanned<Args>, expspan: Span) -> CheckResult<SlotSeq> {
        // should be visited first, otherwise a WHATEVER function will ignore slots in arguments
        let (nargs, mut argtys) = match args.base {
            Args::List(ref ee) => {
                // at this stage the hint is given at the best effort basis
                let hint = self.env.resolve_exact_type(functy).and_then(|ty| {
                    if let Some(&Functions::Simple(ref f)) = ty.get_functions() {
                        let mut args = f.args.clone();
                        if selfinfo.is_some() && !args.head.is_empty() {
                            args.head.remove(0);
                        }
                        Some(SlotSeq::from_seq(args).all_with_loc(functy))
                    } else {
                        None
                    }
                });

                (ee.len(), self.visit_explist_with_span(ee, args.span, hint)?)
            },
            Args::Str(ref s) => {
                let argstr = Str::from(s[..].to_owned());
                (1, SpannedSlotSeq::from(T::Str(Cow::Owned(argstr)).with_loc(args)))
            },
            Args::Table(ref fields) => {
                (1, SpannedSlotSeq::from(self.visit_table(fields)?.with_loc(args)))
            },
        };

        if !self.env.get_type_bounds(functy).1.is_callable() {
            self.env.error(functy, m::CallToNonFunc { func: self.display(functy) }).done()?;
            return Ok(SlotSeq::dummy());
        }
        if let Some(dyn) = functy.get_dynamic() {
            return Ok(SlotSeq::from(T::Dynamic(dyn)));
        }

        // handle tags, which may return different things from the function signature
        match functy.tag() {
            // require("foo")
            Some(Tag::Require) => {
                if nargs < 1 {
                    self.env.error(expspan, m::BuiltinGivenLessArgs { name: "require", nargs: 1 })
                            .done()?;
                    return Ok(SlotSeq::dummy());
                }

                let arg = self.env.resolve_exact_type(&argtys.ensure_at(0).unlift());
                if let Some(modname) = arg.and_then(|t| t.as_string().map(|s| s.to_owned())) {
                    let mut module = self.context().get_loaded_module(&modname, expspan)?;

                    if module.is_none() {
                        self.context().mark_module_as_loading(&modname, expspan);

                        info!("requiring {:?}", modname);
                        let opts = self.env.opts().clone();
                        let chunk = match opts.borrow_mut().require_chunk(&modname) {
                            Ok(chunk) => chunk,
                            Err(_) => {
                                self.env.warn(argtys.ensure_at(0),
                                m::CannotResolveModName {}).done()?;
                                return Ok(SlotSeq::from(T::All));
                            }
                        };
                        let mut env = Env::new(self.env.context(), opts, chunk.map);
                        {
                            let mut sub = Checker::new(&mut env);
                            sub.visit_block(&chunk.block)?;
                        }
                        module = env.return_from_module(&modname, expspan)?;
                    }

                    if let Some(module) = module {
                        self.env.import_types(module.exported_types.with_loc(expspan))?;
                        return Ok(SlotSeq::from(module.returns.clone()));
                    } else {
                        return Ok(SlotSeq::dummy());
                    }
                } else {
                    return Ok(SlotSeq::from(T::All));
                }
            }

            // assert(expr)
            Some(Tag::Assert) => {
                if nargs < 1 {
                    // TODO should display the true name
                    self.env.error(expspan, m::BuiltinGivenLessArgs { name: "assert", nargs: 1 })
                            .done()?;
                    return Ok(SlotSeq::dummy());
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
                    return Ok(SlotSeq::dummy());
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
                    return Ok(SlotSeq::dummy());
                }

                if let Some(flags) = self.ext_literal_ty_to_flags(argtys.ensure_at(1))? {
                    let cond = Cond::Flags(argtys.ensure_at(0).clone(), flags);
                    self.assert_cond(cond, false)?;
                }
            }

            // class()
            Some(Tag::MakeClass) => {
                let cid = self.context().make_class(None, expspan); // TODO parent
                return Ok(SlotSeq::from(T::Class(Class::Prototype(cid))));
            }

            // kailua_test.gen_tvar()
            Some(Tag::KailuaGenTvar) => {
                return Ok(SlotSeq::from(T::TVar(self.context().gen_tvar())));
            }

            // kailua_test.assert_tvar()
            Some(Tag::KailuaAssertTvar) => {
                if nargs < 1 {
                    // TODO should display the true name
                    self.env.error(expspan,
                                   m::BuiltinGivenLessArgs { name: "kailua-assert-tvar", nargs: 1 })
                            .done()?;
                    return Ok(SlotSeq::dummy());
                }

                let is_tvar =
                    if let T::TVar(_) = **argtys.ensure_at(0).unlift() { true } else { false };
                if !is_tvar {
                    let arg = argtys.ensure_at(0);
                    self.env.error(arg, m::NotTVar { slot: self.display(arg) }).done()?;
                }
                return Ok(SlotSeq::new());
            }

            _ => {}
        }

        let methodcall = if let Some(selfinfo) = selfinfo {
            argtys.head.insert(0, selfinfo);
            true
        } else {
            false
        };
        let returns = self.check_callable(functy, &argtys.unlift(), methodcall)?;
        // TODO this should be Var instead of Just!!!!!
        Ok(SlotSeq::from_seq(returns))
    }

    fn visit_table(&mut self,
                   fields: &[(Option<Spanned<Exp>>, Spanned<Exp>)]) -> CheckResult<T<'static>> {
        let mut newfields = Vec::new();

        let mut fieldspans = HashMap::new();
        let mut add_field = |newfields: &mut Vec<(Key, Slot)>, env: &Env<R>, k: Key, v: Slot,
                             span: Span| -> CheckResult<()> {
            if let Some(&prevspan) = fieldspans.get(&k) {
                env.error(span, m::TableLitWithDuplicateKey { key: &k })
                   .note(prevspan, m::PreviousKeyInTableLit {})
                   .done()?;
            } else {
                newfields.push((k.clone(), v));
                fieldspans.insert(k, span);
            }
            Ok(())
        };

        let mut len = 0;
        for (idx, &(ref key, ref value)) in fields.iter().enumerate() {
            let span = key.as_ref().map_or(Span::dummy(), |k| k.span) | value.span;

            // if this is the last entry and no explicit index is set, splice the values
            if idx == fields.len() - 1 && key.is_none() {
                let vty = self.visit_exp(value, None)?;
                for ty in vty.head.into_iter() {
                    len += 1;
                    add_field(&mut newfields, &self.env, Key::Int(len), ty.base, span)?;
                }
                if let Some(ty) = vty.tail {
                    // a record is no longer sufficient now, and as we don't want to infer
                    // anything larger than a record, this is an error
                    self.env.error(&ty, m::TableLitWithUnboundSeq {}).done()?;
                    continue;
                }
            } else {
                let litkey = if let Some(ref key) = *key {
                    let key = self.visit_exp(key, None)?.into_first();
                    let kty = key.unlift();
                    let kty = if let Some(kty) = self.env.resolve_exact_type(&kty) {
                        kty
                    } else {
                        self.env.error(&key, m::TableLitWithUnknownKey { key: self.display(&kty) })
                                .done()?;
                        continue;
                    };

                    // kty should be a known integer or string, otherwise invalid (for Kailua)
                    if let Some(kty) = kty.as_integer() {
                        Key::from(kty)
                    } else if let Some(kty) = kty.as_string() {
                        Key::from(kty)
                    } else {
                        self.env.error(&key, m::TableLitWithUnknownKey { key: self.display(&kty) })
                                .done()?;
                        continue;
                    }
                } else {
                    len += 1;
                    Key::Int(len)
                };

                let vty = self.visit_exp(value, None)?.into_first();
                add_field(&mut newfields, &self.env, litkey, vty.base, span)?;
            }
        }

        let rvar = self.context().gen_rvar();
        if !newfields.is_empty() {
            // really should not fail...
            self.context().assert_rvar_includes(rvar.clone(), &newfields).expect(
                "cannot insert disjoint fields into a fresh row variable"
            );
        }
        Ok(T::Tables(Cow::Owned(Tables::Fields(rvar))))
    }

    // hint is used to drive the inference to the already known type.
    // this is mainly used for anonymous functions.
    fn visit_exp(&mut self, exp: &Spanned<Exp>,
                 hint: Option<SpannedSlotSeq>) -> CheckResult<SpannedSlotSeq> {
        let slotseq = self.visit_exp_(exp, hint)?;

        // mark the resulting slot (sequence) to the span
        let slot = if let Some(slot) = slotseq.head.first() {
            slot.clone()
        } else if let Some(ref slot) = slotseq.tail {
            slot.clone().with_nil()
        } else {
            Slot::just(Ty::silent_nil())
        };
        self.context().spanned_slots_mut().insert(slot.with_loc(exp));

        Ok(slotseq.all_with_loc(exp))
    }

    fn visit_exp_(&mut self, exp: &Spanned<Exp>,
                  hint: Option<SpannedSlotSeq>) -> CheckResult<SlotSeq> {
        debug!("visiting exp {:?}", *exp);

        let ret = match *exp.base {
            // it should not happen, but for the purpose of checker, the error nodes are dummies
            Ex::Oops => Ok(SlotSeq::dummy()),

            // the explicit type `nil` is different from `nil` from an implicit expression
            Ex::Nil => Ok(SlotSeq::from(Ty::silent_nil())),
            Ex::False => Ok(SlotSeq::from(T::False)),
            Ex::True => Ok(SlotSeq::from(T::True)),
            Ex::Num(v) if v.floor() == v =>
                if i32::MIN as f64 <= v && v <= i32::MAX as f64 {
                    Ok(SlotSeq::from(T::Int(v as i32)))
                } else {
                    Ok(SlotSeq::from(T::Integer))
                },
            Ex::Num(_) => Ok(SlotSeq::from(T::Number)),
            Ex::Str(ref s) => {
                let str = Str::from(s[..].to_owned());
                Ok(SlotSeq::from(T::Str(Cow::Owned(str))))
            },

            Ex::Varargs => {
                if let Some(vararg) = self.env.get_vararg() {
                    Ok(SlotSeq::from_seq(vararg.clone()))
                } else {
                    self.env.error(exp, m::NoVarargs {}).done()?;
                    Ok(SlotSeq::dummy())
                }
            },
            Ex::Var(ref name) => {
                if self.env.get_var(name).is_some() {
                    Ok(SlotSeq::from(self.env.ensure_var(name)?))
                } else {
                    self.env.error(exp, m::NoVar { name: self.env.get_name(name) }).done()?;
                    Ok(SlotSeq::dummy())
                }
            },

            Ex::Exp(ref e) => Ok(SlotSeq::from(self.visit_exp(e, hint)?.into_first().base)),
            Ex::Func(ref sig, _scope, ref block) => {
                let hint = hint.map(|seq| seq.into_first());
                let returns = self.visit_func_body(None, sig, block, exp.span, hint)?;
                Ok(SlotSeq::from(returns))
            },
            Ex::Table(ref fields) => Ok(SlotSeq::from(self.visit_table(fields)?)),

            Ex::FuncCall(ref func, ref args) => {
                let funcinfo = self.visit_exp(func, None)?.into_first();
                let funcinfo = funcinfo.map(|t| t.unlift().clone());
                self.visit_func_call(&funcinfo, None, args, exp.span)
            },

            Ex::MethodCall(Spanned { base: (ref e, ref method), span }, ref args) => {
                let keystr = Str::from(method.base[..].to_owned());
                let ty = self.visit_exp(e, None)?.into_first();
                let kty = Slot::just(Ty::new(T::Str(Cow::Owned(keystr)))).with_loc(method.span);
                let methinfo = self.check_rval_index(&ty, &kty, exp.span)?;
                self.context().spanned_slots_mut().insert(methinfo.clone().with_loc(span));
                let methinfo = methinfo.unlift().clone().with_loc(span);
                self.visit_func_call(&methinfo, Some(ty), args, exp.span)
            },

            Ex::Index(ref e, ref key) => {
                let ty = self.visit_exp(e, None)?.into_first();
                let kty = self.visit_exp(key, None)?.into_first();
                Ok(SlotSeq::from(self.check_rval_index(&ty, &kty, exp.span)?))
            },
            Ex::IndexName(ref e, ref key) => {
                let keystr = Str::from(key.base[..].to_owned());
                let ty = self.visit_exp(e, None)?.into_first();
                let kty = Slot::just(Ty::new(T::Str(Cow::Owned(keystr)))).with_loc(key);
                Ok(SlotSeq::from(self.check_rval_index(&ty, &kty, exp.span)?))
            },

            Ex::Un(op, ref e) => {
                let info = self.visit_exp(e, None)?.into_first();
                let info = self.check_un_op(op.base, &info, exp.span)?;
                Ok(SlotSeq::from(info))
            },

            Ex::Bin(ref l, op, ref r) => {
                let lhs = self.visit_exp(l, None)?.into_first();
                let rhs = self.visit_exp(r, None)?.into_first();
                let info = self.check_bin_op(&lhs, op.base, &rhs, exp.span)?;
                Ok(SlotSeq::from(info))
            },
        };

        if let Ok(ref ret) = ret {
            trace!("typed exp {:?} as {:?}", exp, ret);
        }
        ret
    }

    fn visit_explist(&mut self, exps: &Spanned<Vec<Spanned<Exp>>>,
                     hint: Option<SpannedSlotSeq>) -> CheckResult<SpannedSlotSeq> {
        self.visit_explist_with_span(&exps.base, exps.span, hint)
    }

    fn visit_explist_with_span(&mut self, exps: &[Spanned<Exp>], expspan: Span,
                               mut hint: Option<SpannedSlotSeq>) -> CheckResult<SpannedSlotSeq> {
        // the last expression is special, so split it first or return the empty sequence
        let (lastexp, exps) = if let Some(exps) = exps.split_last() {
            exps
        } else {
            return Ok(SpannedSlotSeq::new(expspan));
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
            let info = self.visit_exp(exp, hint)?;
            head.push(info.into_first());
        }

        // for the last expression, use the entire remaining sequence as a hint
        // and expand its result to the final sequence
        let last = self.visit_exp(lastexp, hint)?;
        head.extend(last.head.into_iter());
        Ok(SpannedSlotSeq { head: head, tail: last.tail, span: expspan })
    }

    fn visit_kind(&mut self, flex: F, kind: &Spanned<Kind>) -> CheckResult<Spanned<Slot>> {
        let ty = Ty::from_kind(kind, &mut self.env)?;
        Ok(Slot::new(flex, ty).with_loc(kind))
    }

    fn collect_type_from_exp(&mut self, exp: &Spanned<Exp>)
            -> CheckResult<(Option<Spanned<Slot>>, SpannedSlotSeq)> {
        if let Ex::FuncCall(ref func, ref args) = *exp.base {
            let funcseq = self.visit_exp(func, None)?;
            let funcspan = funcseq.all_span();
            let funcinfo = funcseq.into_first();
            let funcinfo = funcinfo.unlift();
            let typeofexp = if funcinfo.tag() == Some(Tag::Type) {
                // there should be a single argument there
                match args.base {
                    Args::List(ref args) if args.len() >= 1 => {
                        Some(self.visit_exp(&args[0], None)?.into_first())
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
                        Some(Slot::just(Ty::new(self.visit_table(fields)?)).with_loc(args))
                    },
                }
            } else {
                None
            };
            let seq = self.visit_func_call(&funcinfo.clone().with_loc(funcspan), None,
                                           args, exp.span)?;
            Ok((typeofexp, seq.all_with_loc(exp)))
        } else {
            let seq = self.visit_exp(exp, None)?;
            Ok((None, seq))
        }
    }

    // similar to visit_exp but also tries to collect Cond
    fn collect_conds_from_exp(&mut self, exp: &Spanned<Exp>)
            -> CheckResult<(Option<Cond>, SpannedSlotSeq)> {
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
                let seq = self.visit_exp(exp, None)?;
                let info = seq.into_first();
                // XXX should detect non-local slots and reject them!
                // probably we can do that via proper weakening, but who knows.
                Ok((Some(Cond::Flags(info.clone(), T_TRUTHY)), SpannedSlotSeq::from(info)))
            }
        }
    }

    fn assert_cond(&mut self, cond: Cond, negated: bool) -> CheckResult<()> {
        debug!("asserting condition {:?} (negated {:?})", cond, negated);

        match cond {
            Cond::Flags(info, flags) => {
                let flags = if negated { !flags } else { flags };
                // XXX this is temporary, the entire condition assertion should be changed!
                info.filter_by_flags(flags, self.context()).map_err(|_| kailua_diag::Stop)?;
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

    fn literal_ty_to_flags(&self, info: &Spanned<Slot>) -> CheckResult<Option<Flags>> {
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
    fn ext_literal_ty_to_flags(&self, info: &Spanned<Slot>) -> CheckResult<Option<Flags>> {
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

