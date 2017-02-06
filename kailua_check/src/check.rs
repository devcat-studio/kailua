use std::i32;
use std::cmp;
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap};
use take_mut::take;

use kailua_env::{Span, Spanned, WithLoc};
use kailua_diag::{Report, Reporter};
use kailua_syntax::{Str, NameRef, Var, M, TypeSpec, Kind, Sig, Ex, Exp, UnOp, BinOp};
use kailua_syntax::{SelfParam, Args, St, Stmt, Block, K};
use diag::CheckResult;
use ty::{Dyn, Nil, T, Ty, TySeq, SpannedTySeq, Lattice, Union, Displayed, Display, TypeContext};
use ty::{Key, Tables, Function, Functions};
use ty::{F, Slot, SlotSeq, SpannedSlotSeq, Tag, Class};
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

    fn display<'a, 'c, T: Display>(&'c self, x: &'a T) -> Displayed<'a, 'c, T>
            where Displayed<'a, 'c, T>: fmt::Display {
        self.env.display(x)
    }

    fn scoped<'chk>(&'chk mut self, scope: Scope) -> ScopedChecker<'chk, 'envr, 'env, R> {
        self.env.enter(scope);
        ScopedChecker { checker: self }
    }

    // XXX in general reachability checking should continue when the type variable get resolved
    fn check_bool(&self, ty: &Ty) -> Bool {
        let (lb, ub) = self.env.get_type_bounds(ty);
        if lb.is_truthy() {
            Bool::Truthy
        } else if ub == T_NONE || ub.is_falsy() {
            // this is tricky because a resolved implicit nil should also be caught.
            // however we don't want the unresolved type variable to be considered falsy.
            Bool::Falsy
        } else {
            Bool::Unknown
        }
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
                check_op!(info.assert_sub(&T::Number, self.context()));

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
                check_op!(info.assert_sub(&(T::table() | T::String), self.context()));
                Ok(Slot::just(Ty::new(T::Integer)))
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
                    check_op!(lhs.assert_sub(&T::Integer, self.context()));
                    check_op!(rhs.assert_sub(&T::Integer, self.context()));
                    Ok(Slot::just(Ty::new(T::Integer)))
                } else {
                    // technically speaking they coerce strings to numbers,
                    // but that's probably not what you want
                    check_op!(lhs.assert_sub(&T::Number, self.context()));
                    check_op!(rhs.assert_sub(&T::Number, self.context()));
                    Ok(Slot::just(Ty::new(T::Number)))
                }
            }

            BinOp::Div | BinOp::Pow => {
                check_op!(lhs.assert_sub(&T::Number, self.context()));
                check_op!(rhs.assert_sub(&T::Number, self.context()));
                Ok(Slot::just(Ty::new(T::Number)))
            }

            BinOp::Cat => {
                let stringy = T::Number | T::String;
                check_op!(lhs.assert_sub(&stringy, self.context()));
                check_op!(rhs.assert_sub(&stringy, self.context()));

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

                // filter any non-strings and non-numbers
                // avoid using assert_sub here, it is not accurate enough
                if !lflags.is_stringy() || !rflags.is_stringy() {
                    self.env.error(expspan, m::WrongOperand { op: op.symbol(),
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
                    check_op!(lhs.assert_sub(&T::Number, self.context()));
                    check_op!(rhs.assert_sub(&T::Number, self.context()));
                } else if lstr || rstr { // operands are definitely strings
                    check_op!(lhs.assert_sub(&T::String, self.context()));
                    check_op!(rhs.assert_sub(&T::String, self.context()));
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

                match self.check_bool(&lhs.unlift()) {
                    // True and T => T
                    Bool::Truthy => Ok(Slot::just(rhs.unlift().clone())),
                    // False and T => False
                    Bool::Falsy => Ok(Slot::just(lhs.unlift().clone())),
                    // unsure, both can be possible (but truthy types in lhs are not kept)
                    Bool::Unknown => {
                        let falsy_lhs = lhs.as_ref().map(|t| t.unlift().clone().falsy());
                        let rhs = rhs.as_ref().map(|t| t.unlift().clone());
                        Ok(Slot::just(falsy_lhs.union(&rhs, false, self.context())?))
                    }
                }
            }

            BinOp::Or => {
                if let Some(dyn) = Dyn::or(lhs.get_dynamic(), rhs.get_dynamic()) {
                    return Ok(Slot::just(Ty::new(T::Dynamic(dyn))));
                }

                match self.check_bool(&lhs.unlift()) {
                    // True or T => True
                    Bool::Truthy => Ok(Slot::just(lhs.unlift().clone())),
                    // False or T => T
                    Bool::Falsy => Ok(Slot::just(rhs.unlift().clone())),
                    // unsure, both can be possible (but falsy types in lhs are not kept)
                    Bool::Unknown => {
                        let truthy_lhs = lhs.as_ref().map(|t| t.unlift().clone().truthy());
                        let rhs = rhs.as_ref().map(|t| t.unlift().clone());
                        Ok(Slot::just(truthy_lhs.union(&rhs, false, self.context())?))
                    }
                }
            }
        }
    }

    fn check_callable(&mut self, func: &Spanned<Ty>, args: &SpannedTySeq) -> CheckResult<TySeq> {
        debug!("checking if {:?} can be called with {:?}", func, args);

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

        // check if func.args :> args
        let mut returns = match *functy.get_functions().unwrap() {
            Functions::Simple(ref f) => {
                let funcargs = f.args.clone().all_without_loc();
                if let Err(e) = args.assert_sub(&funcargs, self.context()) {
                    return Err(format!("failed to call {:?}: {}", func, e));
                }
                f.returns.clone()
            }
            Functions::All => {
                self.env.error(func, m::CallToAnyFunc { func: self.display(func) }).done()?;
                return Ok(TySeq::dummy());
            }
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

    fn check_index(&mut self, ety0: &Spanned<Slot>, kty0: &Spanned<Slot>, expspan: Span,
                   lval: bool) -> CheckResult<Option<Slot>> {
        debug!("indexing {:?} with {:?} as an {}-value", ety0, kty0, if lval { "l" } else { "r" });

        let mut ety0: Cow<Spanned<Slot>> = Cow::Borrowed(ety0);

        let ety = ety0.unlift().clone();
        let kty = kty0.unlift().clone();

        let (_, flags) = self.env.get_type_bounds(&ety);
        if !flags.is_tabular() {
            self.env.error(&*ety0, m::IndexToNonTable { tab: self.display(&*ety0) }).done()?;
            return Ok(Some(Slot::dummy()));
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
                return Ok(Some(Slot::dummy()));
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
                    return Ok(Some(Slot::dummy()));
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
                            return Ok(Some(Slot::dummy()));
                        }

                        // should have a [constructor] tag to create a `new` method
                        let ty = T::TVar(self.context().gen_tvar());
                        let slot = Slot::new(F::Var, Ty::new(ty).with_tag(Tag::Constructor));
                        fields!(class_ty).insert(litkey, slot.clone());
                        (slot, true)
                    } else if litkey == &b"new"[..] {
                        // `new` is handled from the assignment (it cannot be copied from `init`
                        // because it initially starts as as a type variable, i.e. unknown)
                        self.env.error(expspan, m::ReservedNewMethod {}).done()?;
                        return Ok(Some(Slot::dummy()));
                    } else if let Some(v) = fields!(class_ty).get(&litkey).cloned() {
                        // for other methods, it should be used as is...
                        (v, false)
                    } else {
                        // ...or created only when the `init` method is available.
                        if fields!(class_ty).is_empty() {
                            self.env.error(expspan, m::CannotDefineMethodsWithoutCtor {}).done()?;
                            return Ok(Some(Slot::dummy()));
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
                        return Ok(Some(Slot::dummy()));
                    } else {
                        // the constructor (checked earlier) can add Currently slots to instances.
                        let slot = new_slot(self.context());
                        fields!(instance_ty).insert(litkey, slot.clone());
                        (slot, true)
                    }
                };

                vslot.adapt(ety0.flex(), self.context());

                // if the table is altered in any way, we need to "adapt" the template
                if new {
                    // the following is like `adapt_table` but specialized for this particular case.
                    // `accept_in_place` simulates the self-assignment (not possible with `accept`).
                    ety0.adapt(F::Var, self.context());
                    if let Err(e) = ety0.accept_in_place(self.context()) {
                        error!("{}", e);
                        self.env.error(&*ety0, m::CannotAdaptClass { cls: self.display(&*ety0) })
                                .note(kty0, m::AdaptTriggeredByIndex { key: self.display(kty0) })
                                .done()?;
                        return Ok(Some(Slot::dummy()));
                    }
                }

                return Ok(Some(vslot));
            } else {
                // r-values. we just pick the method from the template.

                let cdef = self.context().get_class_mut(cid).expect("invalid ClassId");
                // TODO should we re-adapt methods?
                if !proto {
                    // instance fields have a precedence over class fields
                    if let Some(info) = cdef.instance_ty.get(&litkey).map(|v| (*v).clone()) {
                        return Ok(Some(info));
                    }
                }
                return Ok(cdef.class_ty.get(&litkey).map(|v| (*v).clone()));
            }
        } else if !flags.is_dynamic() && flags.intersects(T_STRING) {
            // if ety is a string, we go through the previously defined string metatable
            // note that ety may not be fully resolved here!
            if flags.intersects(!T_STRING) {
                self.env.error(&*ety0,
                               m::IndexedTypeIsBothTableOrStr { indexed: self.display(&*ety0) })
                        .done()?;
                return Ok(Some(Slot::dummy()));
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
                        return Ok(Some(Slot::dummy()));
                    }

                    ety
                } else {
                    self.env.error(&*ety0, m::IndexToInexactType { tab: self.display(&*ety0) })
                            .done()?;
                    return Ok(Some(Slot::dummy()));
                }
            } else {
                self.env.error(&*ety0, m::UndefinedStringMeta {}).done()?;
                return Ok(Some(Slot::dummy()));
            }
        } else {
            // normal tables, we need to resolve it fully
            if let Some(ety) = self.env.resolve_exact_type(&ety) {
                ety
            } else {
                self.env.error(&*ety0,
                               m::IndexToInexactType { tab: self.display(&*ety0) }).done()?;
                return Ok(Some(Slot::dummy()));
            }
        };

        // this also handles the case where the string metatable itself is dynamic
        if let Some(dyn) = flags.get_dynamic() {
            let value = Slot::just(Ty::new(T::Dynamic(dyn)));
            // the flex should be retained
            if lval { value.adapt(ety0.flex(), self.context()); }
            return Ok(Some(value));
        }

        macro_rules! check {
            ($sub:expr) => {
                match $sub {
                    Ok(v) => v,
                    Err(_) => {
                        self.env.error(expspan, m::CannotIndex { tab: self.display(&*ety0),
                                                                 key: self.display(kty0) })
                                .done()?;
                        return Ok(Some(Slot::dummy()));
                    }
                }
            }
        }

        // if a new field is about to be created, make a new slot and
        // and try to adapt to a table containing that variable.
        // this "adaptation" only happens for l-value assignments.
        macro_rules! adapt_table {
            ($adapted:expr) => ({
                // the table itself may be at the Just slot, most primarily by
                // being directly constructed from the table constructor.
                ety0.adapt(F::Var, self.context());

                let adapted = Ty::new(T::Tables(Cow::Owned($adapted)));
                if ety0.accept(&Slot::just(adapted.clone()), self.context(), false).is_err() {
                    self.env.error(&*ety0,
                                   m::CannotAdaptTable { tab: self.display(&*ety0),
                                                         adapted: self.display(&adapted) })
                            .note(kty0, m::AdaptTriggeredByIndex { key: self.display(kty0) })
                            .done()?;
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
        let had_litkey = litkey.is_some();
        if let Some(litkey) = litkey {
            match (ety.get_tables(), lval) {
                (Some(&Tables::Empty), true) => {
                    let vslot = new_slot(self.context());
                    let fields = Some((litkey, vslot.clone())).into_iter().collect();
                    adapt_table!(Tables::Fields(fields));
                    return Ok(Some(vslot));
                }

                (Some(&Tables::Fields(ref fields)), true) => {
                    let mut fields = fields.clone();
                    let vslot = fields.entry(litkey)
                                      .or_insert_with(|| new_slot(self.context()))
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
            // possible! this happens when the string metatable was resolved *and* it is wrong.
            (None, _) => {
                let value = Slot::just(Ty::new(T::Dynamic(Dyn::Oops)));
                // the flex should be retained
                if lval { value.adapt(ety0.flex(), self.context()); }
                Ok(Some(value))
            },

            (Some(&Tables::Fields(..)), _) => {
                assert!(!had_litkey);
                self.env.error(expspan,
                               m::IndexToRecWithUnknownStr { tab: self.display(&*ety0),
                                                             key: self.display(&kty) })
                        .done()?;
                Ok(Some(Slot::dummy()))
            },

            (Some(&Tables::Empty), true) => {
                let vslot = new_slot(self.context());
                let tab = if intkey {
                    Tables::Array(vslot.clone())
                } else {
                    Tables::Map(kty.clone(), vslot.clone())
                };
                adapt_table!(tab);
                Ok(Some(vslot))
            },

            (Some(&Tables::Empty), false) => Ok(None),

            (Some(&Tables::Array(ref value)), _) if intkey => {
                if lval { value.adapt(ety0.flex(), self.context()); }
                Ok(Some((*value).clone().with_nil()))
            },

            (Some(&Tables::Array(..)), _) => {
                self.env.error(expspan,
                               m::IndexToArrayWithNonInt { tab: self.display(&*ety0),
                                                           key: self.display(&kty) })
                        .done()?;
                Ok(Some(Slot::dummy()))
            },

            (Some(&Tables::Map(ref key, ref value)), false) => {
                check!(kty.assert_sub(&**key, self.context()));
                Ok(Some((*value).clone().with_nil()))
            },

            (Some(&Tables::Map(ref key, ref value)), true) => {
                let key = check!((**key).union(&kty, false, self.context()));
                value.adapt(ety0.flex(), self.context());
                adapt_table!(Tables::Map(key, value.clone()));
                Ok(Some(value.clone().with_nil()))
            },

            (Some(&Tables::All), _) => {
                self.env.error(&*ety0,
                               m::IndexToAnyTable { tab: self.display(&*ety0) }).done()?;
                Ok(Some(Slot::dummy()))
            },
        }
    }

    pub fn visit(&mut self, chunk: &Spanned<Block>) -> CheckResult<()> {
        self.visit_block(chunk)?;
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
            exit = scope.visit_stmt(stmt)?;
        }
        Ok(exit)
    }

    fn visit_stmt(&mut self, stmt: &Spanned<Stmt>) -> CheckResult<Exit> {
        debug!("visiting stmt {:?}", *stmt);

        match *stmt.base {
            // it should not happen, but for the purpose of checker, the error nodes are ignored
            St::Oops => Ok(Exit::None),

            St::Void(ref exp) => {
                self.visit_exp(exp)?;
                Ok(Exit::None)
            }

            St::Assign(ref vars, ref exps) => {
                #[derive(Debug)]
                enum VarRef<'a> {
                    Name(&'a Spanned<NameRef>),
                    Slot(Spanned<Slot>),
                }

                let varrefs = vars.iter().map(|varspec| {
                    match varspec.base.base {
                        Var::Name(ref nameref) => Ok(VarRef::Name(nameref)),

                        Var::Index(ref e, ref key) => {
                            if let Some(ref kind) = varspec.kind {
                                self.env.error(kind, m::TypeSpecToIndex {}).done()?;
                            }

                            let ty = self.visit_exp(e)?.into_first();
                            let kty = self.visit_exp(key)?.into_first();
                            let slot = self.check_index(&ty, &kty, varspec.base.span, true)?;
                            // since we've requested a lvalue it would never return None
                            Ok(VarRef::Slot(slot.unwrap().with_loc(&varspec.base)))
                        },

                        Var::IndexName(ref e, ref key) => {
                            if let Some(ref kind) = varspec.kind {
                                self.env.error(kind, m::TypeSpecToIndex {}).done()?;
                            }

                            let ty = self.visit_exp(e)?.into_first();
                            let keystr = Str::from(key.base[..].to_owned());
                            let kty = Slot::just(Ty::new(T::Str(Cow::Owned(keystr)))).with_loc(key);
                            let slot = self.check_index(&ty, &kty, varspec.base.span, true)?;
                            // since we've requested a lvalue it would never return None
                            Ok(VarRef::Slot(slot.unwrap().with_loc(&varspec.base)))
                        },
                    }
                }).collect::<CheckResult<Vec<_>>>()?;

                // the incomplete expression is parsed as Assign without rhs,
                // so we won't generate further errors due to mismatching types
                let mut infos = match *exps {
                    Some(ref exps) => Some(self.visit_explist(exps)?.into_iter_with_nil()),
                    None => None,
                };

                // unlike St::Local, do not tolerate the uninitialized variables
                for (var, varref) in vars.iter().zip(varrefs.into_iter()) {
                    // ideally should be done via zip, but then concrete types will collide
                    let info = infos.as_mut().and_then(|it| it.next());
                    debug!("assigning {:?} to {:?} with type {:?}", info, var, varref);

                    match varref {
                        VarRef::Name(nameref) => {
                            match *var {
                                // variable declaration
                                TypeSpec { modf, kind: Some(ref kind), .. } => {
                                    let specinfo = self.visit_kind(F::from(modf), kind)?;
                                    self.env.add_var(nameref, Some(specinfo), info)?;
                                }

                                // variable assignment
                                TypeSpec { kind: None, .. } => {
                                    if let Some(info) = info {
                                        self.env.assign_to_var(nameref, info)?;
                                    }
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
                        VarRef::Slot(slot) => {
                            if let Some(info) = info {
                                self.env.assign(&slot, &info)?;
                            }
                            self.context().spanned_slots_mut().insert(slot);
                        }
                    }
                }
                Ok(Exit::None)
            }

            St::Do(ref block) => self.visit_block(block),

            St::While(ref cond, ref block) => {
                let ty = self.visit_exp(cond)?.into_first();
                let boolean = self.check_bool(&ty.unlift());
                match boolean {
                    Bool::Truthy => { // infinite loop
                        let exit = self.visit_block(block)?;
                        if exit >= Exit::Return { Ok(exit) } else { Ok(Exit::Stop) }
                    },
                    Bool::Falsy => {
                        // TODO warning
                        Ok(Exit::None)
                    },
                    Bool::Unknown => {
                        self.visit_block(block)?; // TODO scope merger
                        Ok(Exit::None)
                    },
                }
            }

            St::Repeat(ref block, ref cond) => {
                let exit = self.visit_block(block)?; // TODO scope merger
                let ty = self.visit_exp(cond)?.into_first();
                if exit == Exit::None {
                    match self.check_bool(&ty.unlift()) {
                        Bool::Truthy => Ok(Exit::Stop),
                        Bool::Falsy => Ok(exit),
                        Bool::Unknown => Ok(Exit::None),
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

                    let ty = self.visit_exp(cond)?.into_first();
                    let boolean = self.check_bool(&ty.unlift());
                    match boolean {
                        Bool::Truthy => {
                            ignored_blocks = Some((cond.span, Span::dummy()));
                            exit = exit.or(self.visit_block(block)?);
                        }
                        Bool::Falsy => {
                            self.env.warn(span, m::IgnoredIfCase {})
                                    .note(cond, m::IfCaseWithFalsyCond {})
                                    .done()?;
                        }
                        Bool::Unknown => {
                            exit = exit.or(self.visit_block(block)?); // TODO scope merger
                        }
                    }
                }

                if let &Some(ref block) = lastblock {
                    if let Some((_, ref mut blocks_span)) = ignored_blocks {
                        *blocks_span |= block.span;
                    } else {
                        exit = exit.or(self.visit_block(block)?); // TODO scope merger
                    }
                } else {
                    if ignored_blocks.is_none() {
                        exit = Exit::None;
                    }
                }

                if let Some((truthy_span, blocks_span)) = ignored_blocks {
                    if blocks_span.is_dummy() {
                        self.env.warn(truthy_span, m::IfCaseWithTruthyCond {}).done()?;
                    } else {
                        self.env.warn(blocks_span, m::IgnoredIfCase {})
                                .note(truthy_span, m::IfCaseWithTruthyCond {})
                                .done()?;
                    }
                }

                Ok(exit)
            }

            St::For(ref localname, ref start, ref end, ref step, _blockscope, ref block) => {
                let start = self.visit_exp(start)?.into_first();
                let end = self.visit_exp(end)?.into_first();
                let step = if let &Some(ref step) = step {
                    self.visit_exp(step)?.into_first()
                } else {
                    Slot::just(Ty::new(T::Integer)).without_loc() // to simplify the matter
                };

                // the similar logic is also present in check_bin_op
                let startflags = self.env.get_type_bounds(&start.unlift()).1;
                let endflags = self.env.get_type_bounds(&end.unlift()).1;
                let stepflags = self.env.get_type_bounds(&step.unlift()).1;
                let indty;
                if startflags.is_integral() && endflags.is_integral() && stepflags.is_integral() &&
                   !(startflags.is_dynamic() && endflags.is_dynamic() && stepflags.is_dynamic()) {
                    start.assert_sub(&T::Integer, self.context())?;
                    end.assert_sub(&T::Integer, self.context())?;
                    step.assert_sub(&T::Integer, self.context())?;
                    indty = T::Integer;
                } else {
                    start.assert_sub(&T::Number, self.context())?;
                    end.assert_sub(&T::Number, self.context())?;
                    step.assert_sub(&T::Number, self.context())?;
                    indty = T::Number;
                }

                let mut scope = self.scoped(Scope::new());
                let indty = Slot::var(Ty::new(indty), scope.context());
                let nameref = NameRef::Local(localname.base.clone()).with_loc(localname);
                scope.env.add_var(&nameref, None, Some(indty.without_loc()))?;
                let exit = scope.visit_block(block)?;
                if exit >= Exit::Return { Ok(exit) } else { Ok(Exit::None) }
            }

            St::ForIn(ref names, ref exps, _blockscope, ref block) => {
                let infos = self.visit_explist(exps)?;
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
                    let mut returns = self.check_callable(&func.with_loc(expspan), &args)?;
                    last.assert_sub(&indvar, self.context())?;

                    // note that we ignore indvar here. it is only kept internally and
                    // not visible outside; returns is what we should assign to variables!
                    // we should still account for the fact that the first value cannot be nil.
                    take(returns.ensure_at_mut(0), |t| t.without_nil());

                    indtys = returns;
                }

                let mut scope = self.scoped(Scope::new());
                for (localname, ty) in names.iter().zip(indtys.into_iter_with_nil()) {
                    let ty = Slot::var(ty, scope.context());
                    let nameref = NameRef::Local(localname.base.clone()).with_loc(localname);
                    scope.env.add_var(&nameref, None, Some(ty.without_loc()))?;
                }
                let exit = scope.visit_block(block)?;
                if exit >= Exit::Return { Ok(exit) } else { Ok(Exit::None) }
            }

            St::FuncDecl(ref name, ref sig, _blockscope, ref block, nextscope) => {
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
                let functy = self.visit_func_body(None, sig, block, stmt.span)?;
                T::TVar(funcv).assert_eq(&*functy.unlift(), self.context())?;
                Ok(Exit::None)
            }

            St::MethodDecl(Spanned { base: (ref name, ref meths), .. },
                           ref selfparam, ref sig, _blockscope, ref block) => {
                assert!(meths.len() >= 1);

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
                    if let Some(subinfo) = self.check_index(&info, &kty, subspan, false)? {
                        info = subinfo.with_loc(subspan);
                    } else {
                        self.env.error(subspan, m::CannotIndex { tab: self.display(&info),
                                                                 key: self.display(&kty) })
                                .done()?;
                        info = Slot::dummy().with_loc(subspan);
                    }
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
                let methinfo = self.visit_func_body(selfinfo, sig, block, stmt.span)?;

                // finally, go through the ordinary table update
                let subspan = info.span | method.span;
                let keystr = Str::from(method.base[..].to_owned());
                let kty = Slot::just(Ty::new(T::Str(Cow::Owned(keystr)))).with_loc(method);
                let slot = self.check_index(&info, &kty, subspan, true)?;
                // since we've requested a lvalue it would never return None
                self.env.assign(&slot.unwrap().with_loc(subspan), &methinfo.with_loc(stmt))?;

                Ok(Exit::None)
            }

            St::Local(ref names, ref exps, _nextscope) => {
                let infos = self.visit_explist(exps)?;
                for (namespec, info) in names.iter().zip(infos.into_iter_with_none()) {
                    let specinfo = if let Some(ref kind) = namespec.kind {
                        Some(self.visit_kind(F::from(namespec.modf), kind)?)
                    } else {
                        None
                    };
                    let localname = &namespec.base;
                    let nameref = NameRef::Local(localname.base.clone()).with_loc(localname);
                    self.env.add_var(&nameref, specinfo, info)?;
                }
                Ok(Exit::None)
            }

            St::Return(ref exps) => {
                let seq = self.visit_explist(exps)?;
                let seq = seq.unlift(); // XXX wait, is it safe?
                let (returns, returns_exact) = {
                    let frame = self.env.get_frame();
                    let returns = frame.returns.as_ref().map(|seq| seq.clone().all_without_loc());
                    (returns, frame.returns_exact)
                };
                if returns_exact {
                    Some(seq).assert_sub(&returns, self.context())?;
                } else {
                    // need to infer the return type, but not _that_ much
                    let returns = Some(seq).union(&returns, false, self.context())?;
                    self.env.get_frame_mut().returns = returns.map(|seq| seq.unspan());
                }
                Ok(Exit::Return)
            }

            St::Break => Ok(Exit::Break),

            St::KailuaOpen(ref name) => {
                let opts = self.env.opts().clone();
                self.env.context().open_library(name, opts)?;
                Ok(Exit::None)
            }

            St::KailuaType(ref name, ref kind) => {
                let ty = Ty::from_kind(kind, &mut self.env)?;
                self.env.define_type(name, ty)?;
                Ok(Exit::None)
            }

            St::KailuaAssume(ref name, kindm, ref kind, _nextscope) => {
                let slot = self.visit_kind(F::from(kindm), kind)?;
                self.env.assume_var(name, slot)?;
                Ok(Exit::None)
            }
        }
    }

    fn visit_func_body(&mut self, selfparam: Option<(&Spanned<SelfParam>, Slot)>, sig: &Sig,
                       block: &Spanned<Vec<Spanned<Stmt>>>, declspan: Span) -> CheckResult<Slot> {
        // if no check is requested, the signature should be complete
        let no_check = sig.attrs.iter().any(|a| *a.name.base == *b"no_check");

        let vatype = match sig.args.tail {
            None => None,
            Some(None) => {
                // varargs present but types are unspecified
                if no_check {
                    self.env.error(declspan, m::NoCheckRequiresTypedVarargs {}).done()?;
                    return Ok(Slot::dummy());
                }
                Some(Ty::new(T::TVar(self.context().gen_tvar())))
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
            Frame { vararg: vainfo, returns: Some(returns), returns_exact: true }
        } else if no_check {
            self.env.error(declspan, m::NoCheckRequiresTypedReturns {}).done()?;
            return Ok(Slot::dummy());
        } else {
            Frame { vararg: vainfo, returns: None, returns_exact: false }
        };

        let mut argshead = Vec::new();

        let mut scope = self.scoped(Scope::new_function(frame));
        if let Some((selfparam, selfinfo)) = selfparam {
            let ty = selfinfo.unlift().clone();
            let selfid = selfparam.clone().map(|param| param.0);
            scope.env.add_local_var_already_set(&selfid, selfinfo.without_loc())?;
            argshead.push(ty);
        }

        for param in &sig.args.head {
            let ty;
            let sty;
            if let Some(ref kind) = param.kind {
                ty = Ty::from_kind(kind, &mut scope.env)?;
                let flex = F::from(param.modf);
                sty = Slot::new(flex, ty.clone());
            } else if no_check {
                scope.env.error(&param.base, m::NoCheckRequiresTypedArgs {}).done()?;
                return Ok(Slot::dummy());
            } else {
                let argv = scope.context().gen_tvar();
                ty = Ty::new(T::TVar(argv));
                sty = Slot::new(F::Var, Ty::new(T::TVar(argv)));
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

        let returns = scope.env.get_frame_mut().returns.take().unwrap();
        Ok(Slot::just(Ty::new(T::func(Function { args: args, returns: returns }))))
    }

    fn visit_func_call(&mut self, funcinfo: &Spanned<Ty>, selfinfo: Option<Spanned<Slot>>,
                       args: &Spanned<Args>, expspan: Span) -> CheckResult<SlotSeq> {
        // should be visited first, otherwise a WHATEVER function will ignore slots in arguments
        let (nargs, mut argtys) = match args.base {
            Args::List(ref ee) =>
                (ee.len(), self.visit_explist_with_span(ee, args.span)?),
            Args::Str(ref s) => {
                let argstr = Str::from(s[..].to_owned());
                (1, SpannedSlotSeq::from(T::Str(Cow::Owned(argstr)).with_loc(args)))
            },
            Args::Table(ref fields) =>
                (1, SpannedSlotSeq::from(self.visit_table(fields)?.with_loc(args))),
        };

        if !self.env.get_type_bounds(funcinfo).1.is_callable() {
            self.env.error(funcinfo, m::CallToNonFunc { func: self.display(funcinfo) }).done()?;
            return Ok(SlotSeq::dummy());
        }
        if let Some(dyn) = funcinfo.get_dynamic() {
            return Ok(SlotSeq::from(T::Dynamic(dyn)));
        }

        // handle tags, which may return different things from the function signature
        match funcinfo.tag() {
            // require("foo")
            Some(Tag::Require) => {
                if nargs < 1 {
                    self.env.error(expspan, m::BuiltinGivenLessArgs { name: "require", nargs: 1 })
                            .done()?;
                    return Ok(SlotSeq::dummy());
                }

                if let Some(modname) = self.env.resolve_exact_type(&argtys.head[0].unlift())
                                               .and_then(|t| t.as_string().map(|s| s.to_owned())) {
                    if let Some(slot) = self.context().get_loaded_module(&modname, expspan)? {
                        info!("requiring {:?} (cached)", modname);
                        return Ok(SlotSeq::from(slot));
                    }
                    self.context().mark_module_as_loading(&modname, expspan);

                    info!("requiring {:?}", modname);
                    let opts = self.env.opts().clone();
                    let chunk = match opts.borrow_mut().require_chunk(&modname) {
                        Ok(chunk) => chunk,
                        Err(_) => {
                            self.env.warn(&argtys.head[0], m::CannotResolveModName {}).done()?;
                            return Ok(SlotSeq::from(T::All));
                        }
                    };
                    let mut env = Env::new(self.env.context(), opts, chunk.map);
                    {
                        let mut sub = Checker::new(&mut env);
                        sub.visit_block(&chunk.block)?;
                    }
                    return Ok(SlotSeq::from(env.return_from_module(&modname, expspan)?));
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

                // other cases are eliminated by `nargs < 2` condition
                if let Args::List(_) = args.base {
                    if let Some(flags) = self.ext_literal_ty_to_flags(&argtys.head[1])? {
                        let cond = Cond::Flags(argtys.head[0].clone(), flags);
                        self.assert_cond(cond, false)?;
                    }
                }
            }

            // class()
            Some(Tag::MakeClass) => {
                let cid = self.context().make_class(None, expspan); // TODO parent
                return Ok(SlotSeq::from(T::Class(Class::Prototype(cid))));
            }

            _ => {}
        }

        if let Some(selfinfo) = selfinfo {
            argtys.head.insert(0, selfinfo);
        }
        let returns = self.check_callable(funcinfo, &argtys.unlift())?;
        Ok(SlotSeq::from_seq(returns))
    }

    fn visit_table(&mut self,
                   fields: &[(Option<Spanned<Exp>>, Spanned<Exp>)]) -> CheckResult<T<'static>> {
        let mut fieldset = BTreeMap::new();

        let mut fieldspans = HashMap::new();
        let mut add_field = |fieldset: &mut BTreeMap<Key, Slot>, env: &Env<R>, k: Key, v: Slot,
                             span: Span| -> CheckResult<()> {
            use std::collections::btree_map::Entry;
            if let Entry::Vacant(e) = fieldset.entry(k.clone()) {
                e.insert(v);
                fieldspans.insert(k, span);
            } else {
                let prevspan = fieldspans[&k];
                env.error(span, m::TableLitWithDuplicateKey { key: &k })
                   .note(prevspan, m::PreviousKeyInTableLit {})
                   .done()?;
            }
            Ok(())
        };

        let mut len = 0;
        for (idx, &(ref key, ref value)) in fields.iter().enumerate() {
            let span = key.as_ref().map_or(Span::dummy(), |k| k.span) | value.span;

            // if this is the last entry and no explicit index is set, splice the values
            if idx == fields.len() - 1 && key.is_none() {
                let vty = self.visit_exp(value)?;
                for ty in vty.head.into_iter() {
                    len += 1;
                    add_field(&mut fieldset, &self.env, Key::Int(len), ty.base, span)?;
                }
                if let Some(ty) = vty.tail {
                    // a record is no longer sufficient now, and as we don't want to infer
                    // anything larger than a record, this is an error
                    self.env.error(&ty, m::TableLitWithUnboundSeq {}).done()?;
                    continue;
                }
            } else {
                let litkey = if let Some(ref key) = *key {
                    let key = self.visit_exp(key)?.into_first();
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

                let vty = self.visit_exp(value)?.into_first();
                add_field(&mut fieldset, &self.env, litkey, vty.base, span)?;
            }
        }

        if fieldset.is_empty() {
            Ok(T::Tables(Cow::Owned(Tables::Empty)))
        } else {
            Ok(T::Tables(Cow::Owned(Tables::Fields(fieldset))))
        }
    }

    fn visit_exp(&mut self, exp: &Spanned<Exp>) -> CheckResult<SpannedSlotSeq> {
        let slotseq = self.visit_exp_(exp)?;

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

    fn visit_exp_(&mut self, exp: &Spanned<Exp>) -> CheckResult<SlotSeq> {
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

            Ex::Exp(ref e) => Ok(SlotSeq::from(self.visit_exp(e)?.into_first().base)),
            Ex::Func(ref sig, _scope, ref block) => {
                let returns = self.visit_func_body(None, sig, block, exp.span)?;
                Ok(SlotSeq::from(returns))
            },
            Ex::Table(ref fields) => Ok(SlotSeq::from(self.visit_table(fields)?)),

            Ex::FuncCall(ref func, ref args) => {
                let funcinfo = self.visit_exp(func)?.into_first();
                let funcinfo = funcinfo.map(|t| t.unlift().clone());
                self.visit_func_call(&funcinfo, None, args, exp.span)
            },

            Ex::MethodCall(Spanned { base: (ref e, ref method), span }, ref args) => {
                let keystr = Str::from(method.base[..].to_owned());
                let ty = self.visit_exp(e)?.into_first();
                let kty = Slot::just(Ty::new(T::Str(Cow::Owned(keystr)))).with_loc(method.span);
                if let Some(methinfo) = self.check_index(&ty, &kty, exp.span, false)? {
                    self.context().spanned_slots_mut().insert(methinfo.clone().with_loc(span));
                    let methinfo = methinfo.unlift().clone().with_loc(span);
                    self.visit_func_call(&methinfo, Some(ty), args, exp.span)
                } else {
                    self.env.error(exp, m::CannotIndex { tab: self.display(&ty),
                                                         key: self.display(&kty) })
                            .done()?;
                    Ok(SlotSeq::dummy())
                }
            },

            Ex::Index(ref e, ref key) => {
                let ty = self.visit_exp(e)?.into_first();
                let kty = self.visit_exp(key)?.into_first();
                if let Some(vinfo) = self.check_index(&ty, &kty, exp.span, false)? {
                    Ok(SlotSeq::from(vinfo))
                } else {
                    self.env.error(exp, m::CannotIndex { tab: self.display(&ty),
                                                         key: self.display(&kty) })
                            .done()?;
                    Ok(SlotSeq::dummy())
                }
            },
            Ex::IndexName(ref e, ref key) => {
                let keystr = Str::from(key.base[..].to_owned());
                let ty = self.visit_exp(e)?.into_first();
                let kty = Slot::just(Ty::new(T::Str(Cow::Owned(keystr)))).with_loc(key);
                if let Some(vinfo) = self.check_index(&ty, &kty, exp.span, false)? {
                    Ok(SlotSeq::from(vinfo))
                } else {
                    self.env.error(exp, m::CannotIndex { tab: self.display(&ty),
                                                         key: self.display(&kty) })
                            .done()?;
                    Ok(SlotSeq::dummy())
                }
            },

            Ex::Un(op, ref e) => {
                let info = self.visit_exp(e)?.into_first();
                let info = self.check_un_op(op.base, &info, exp.span)?;
                Ok(SlotSeq::from(info))
            },

            Ex::Bin(ref l, op, ref r) => {
                let lhs = self.visit_exp(l)?.into_first();
                let rhs = self.visit_exp(r)?.into_first();
                let info = self.check_bin_op(&lhs, op.base, &rhs, exp.span)?;
                Ok(SlotSeq::from(info))
            },
        };

        if let Ok(ref ret) = ret {
            trace!("typed exp {:?} as {:?}", exp, ret);
        }
        ret
    }

    fn visit_explist(&mut self, exps: &Spanned<Vec<Spanned<Exp>>>) -> CheckResult<SpannedSlotSeq> {
        self.visit_explist_with_span(&exps.base, exps.span)
    }

    fn visit_explist_with_span(&mut self, exps: &[Spanned<Exp>],
                               expspan: Span) -> CheckResult<SpannedSlotSeq> {
        let mut head = Vec::new();
        let mut last: Option<SpannedSlotSeq> = None;
        for exp in exps {
            if let Some(last) = last.take() {
                head.push(last.into_first());
            }
            let info = self.visit_exp(exp)?;
            last = Some(info);
        }

        let mut seq = SpannedSlotSeq { head: head, tail: None, span: expspan };
        if let Some(last) = last {
            seq.head.extend(last.head.into_iter());
            seq.tail = last.tail;
        }
        Ok(seq)
    }

    fn visit_kind(&mut self, flex: F, kind: &Spanned<Kind>) -> CheckResult<Spanned<Slot>> {
        let ty = Ty::from_kind(kind, &mut self.env)?;
        Ok(Slot::new(flex, ty).with_loc(kind))
    }

    fn collect_type_from_exp(&mut self, exp: &Spanned<Exp>)
            -> CheckResult<(Option<Spanned<Slot>>, SpannedSlotSeq)> {
        if let Ex::FuncCall(ref func, ref args) = *exp.base {
            let funcseq = self.visit_exp(func)?;
            let funcspan = funcseq.all_span();
            let funcinfo = funcseq.into_first();
            let funcinfo = funcinfo.unlift();
            let typeofexp = if funcinfo.tag() == Some(Tag::Type) {
                // there should be a single argument there
                match args.base {
                    Args::List(ref args) if args.len() >= 1 => {
                        Some(self.visit_exp(&args[0])?.into_first())
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
            let seq = self.visit_exp(exp)?;
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
                let seq = self.visit_exp(exp)?;
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
                info.filter_by_flags(flags, self.context())?;
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

