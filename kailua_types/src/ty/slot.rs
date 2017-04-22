use std::fmt;
use std::mem;
use std::ops::Deref;
use std::sync::Arc;
use std::sync::atomic::{Ordering, AtomicUsize};
use take_mut::take;
use parking_lot::{RwLock, RwLockReadGuard};

use kailua_env::{Span, Spanned};
use kailua_syntax::ast::{M, MM};
use diag::Origin;
use super::{Dyn, Nil, T, Ty, TypeContext, Lattice, Union, Dummy, TVar, Tag};
use super::{TypeReport, TypeResult};
use super::display::{Display, DisplayState, DisplayName};
use super::flags::Flags;

/// A slot type flexibility.
///
/// This is a superset of the type modifier, because of the existence of r-value only types.
#[derive(Copy, Clone, PartialEq)]
pub enum F {
    /// Unknown flexibility (e.g. right after the generation but before the initialization)
    Unknown,

    /// Dynamic slot; all assignments are allowed and ignored.
    Dynamic(Dyn),

    /// Temporary r-value slot.
    ///
    /// Gets updated to `Var` (default) or `Const` in place depending on the unification.
    Just,

    /// Covariant immutable slot.
    Const,

    /// Invariant mutable slot.
    Var,

    /// Postponed invariant mutable slot.
    ///
    /// This is a variant of `Var` but only allows for indexing (no subtyping), and any types
    /// assigned via indexing will be marked as type-checked later.
    Module,
}

impl From<M> for F {
    fn from(modf: M) -> F {
        match modf {
            M::None => F::Var,
            M::Const => F::Const,
        }
    }
}

impl From<MM> for F {
    fn from(modf: MM) -> F {
        match modf {
            MM::None => F::Var,
            MM::Const => F::Const,
            MM::Module => F::Module,
        }
    }
}

impl<'a> fmt::Debug for F {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            F::Unknown            => write!(f, "_"),
            F::Dynamic(Dyn::User) => write!(f, "?"),
            F::Dynamic(Dyn::Oops) => write!(f, "?!"),
            F::Just               => write!(f, "just"),
            F::Const              => write!(f, "const"),
            F::Var                => write!(f, "var"),
            F::Module             => write!(f, "module"),
        }
    }
}

const UNKNOWN_BITS: usize = 0x00;
const DYNAMIC_USER_BITS: usize = 0x01;
const DYNAMIC_OOPS_BITS: usize = 0x02;
const JUST_BITS: usize = 0x03;
const CONST_BITS: usize = 0x04;
const VAR_BITS: usize = 0x05;
const MODULE_BITS: usize = 0x06;
const FLEX_MASK: usize = 0x07;

fn usize_from_flex(flex: F) -> usize {
    match flex {
        F::Unknown            => UNKNOWN_BITS,
        F::Dynamic(Dyn::User) => DYNAMIC_USER_BITS,
        F::Dynamic(Dyn::Oops) => DYNAMIC_OOPS_BITS,
        F::Just               => JUST_BITS,
        F::Const              => CONST_BITS,
        F::Var                => VAR_BITS,
        F::Module             => MODULE_BITS,
    }
}

fn flex_from_usize(v: usize) -> F {
    match v & FLEX_MASK {
        UNKNOWN_BITS      => F::Unknown,
        DYNAMIC_USER_BITS => F::Dynamic(Dyn::User),
        DYNAMIC_OOPS_BITS => F::Dynamic(Dyn::Oops),
        JUST_BITS         => F::Just,
        CONST_BITS        => F::Const,
        VAR_BITS          => F::Var,
        MODULE_BITS       => F::Module,
        _                 => panic!("unknown flex bits {:#x}", v),
    }
}

#[derive(Copy, Clone, Debug)]
struct Bits(usize);

impl Bits {
    fn new(flex: F) -> Bits {
        Bits(usize_from_flex(flex))
    }

    fn from(a: AtomicUsize) -> Bits {
        Bits(a.into_inner())
    }

    fn load(a: &AtomicUsize) -> Bits {
        Bits(a.load(Ordering::SeqCst))
    }

    fn make(&self) -> AtomicUsize {
        AtomicUsize::new(self.0)
    }

    fn flex(&self) -> F {
        flex_from_usize(self.0)
    }

    fn with_flex(&self, flex: F) -> Bits {
        Bits((self.0 & !FLEX_MASK) | usize_from_flex(flex))
    }

    fn try_store(&mut self, new: Bits, a: &AtomicUsize) -> bool {
        let old = a.compare_and_swap(self.0, new.0, Ordering::SeqCst);
        if self.0 == old {
            self.0 = new.0;
            true
        } else {
            self.0 = old;
            false
        }
    }

    fn try_set_flex(&mut self, flex: F, a: &AtomicUsize) -> bool {
        let new = self.with_flex(flex);
        self.try_store(new, a)
    }
}

impl PartialEq for Bits {
    fn eq(&self, other: &Bits) -> bool {
        (self.0 ^ other.0) & FLEX_MASK == 0
    }
}

/// A proxy to the value type from `Ty::unlift()`.
pub struct UnliftedSlot<'a>(RwLockReadGuard<'a, Ty>);

impl<'a> Deref for UnliftedSlot<'a> {
    type Target = Ty;
    fn deref(&self) -> &Ty { &*self.0 }
}

impl<'a, 'b> Lattice<UnliftedSlot<'b>> for UnliftedSlot<'a> {
    fn assert_sub(&self, other: &UnliftedSlot<'b>, ctx: &mut TypeContext) -> TypeResult<()> {
        (**self).assert_sub(&**other, ctx)
    }

    fn assert_eq(&self, other: &UnliftedSlot<'b>, ctx: &mut TypeContext) -> TypeResult<()> {
        (**self).assert_eq(&**other, ctx)
    }
}

impl<'a> Lattice<Ty> for UnliftedSlot<'a> {
    fn assert_sub(&self, other: &Ty, ctx: &mut TypeContext) -> TypeResult<()> {
        (**self).assert_sub(other, ctx)
    }

    fn assert_eq(&self, other: &Ty, ctx: &mut TypeContext) -> TypeResult<()> {
        (**self).assert_eq(other, ctx)
    }
}

impl<'a, 'b> Lattice<T<'b>> for UnliftedSlot<'a> {
    fn assert_sub(&self, other: &T<'b>, ctx: &mut TypeContext) -> TypeResult<()> {
        (**self).assert_sub(other, ctx)
    }

    fn assert_eq(&self, other: &T<'b>, ctx: &mut TypeContext) -> TypeResult<()> {
        (**self).assert_eq(other, ctx)
    }
}

/// Slot types, used for variables and table values.
///
/// Externally this is rarely used separately because slot types need to be mutable;
/// the `Slot` container should be used instead in most cases.
pub struct S {
    bits: AtomicUsize,
    ty: RwLock<Ty>,
}

impl S {
    pub fn new(flex: F, ty: Ty) -> S {
        S { bits: Bits::new(flex).make(), ty: RwLock::new(ty) }
    }

    fn bits(&self) -> Bits {
        Bits::load(&self.bits)
    }

    pub fn flex(&self) -> F {
        self.bits().flex()
    }

    pub fn unlift<'a>(&'a self) -> UnliftedSlot<'a> {
        UnliftedSlot(self.ty.read())
    }

    fn map_ty<F: FnOnce(Ty) -> Ty>(self, f: F) -> S {
        S { bits: Bits::from(self.bits).make(), ty: RwLock::new(f(self.ty.into_inner())) }
    }

    pub fn with_nil(self) -> S {
        self.map_ty(|t| t.with_nil())
    }

    pub fn without_nil(self) -> S {
        self.map_ty(|t| t.without_nil())
    }

    // in addition to `Ty::coerce`, also updates the flexibility
    pub fn coerce(self) -> S {
        let flex = match self.flex() {
            F::Just | F::Const | F::Var => F::Var,
            flex => flex,
        };
        S::new(flex, self.ty.into_inner().coerce())
    }

    pub fn generalize(self, ctx: &mut TypeContext) -> S {
        self.map_ty(|t| t.generalize(ctx))
    }

    // self and other may be possibly different slots and being merged by union
    // mainly used by `and`/`or` operators and table lifting
    pub fn union(&self, other: &S, explicit: bool, ctx: &mut TypeContext) -> TypeResult<S> {
        (|| {
            let (flex, ty) = match (self.flex(), other.flex()) {
                (F::Dynamic(dyn1), F::Dynamic(dyn2)) => {
                    let dyn = dyn1.union(dyn2);
                    (F::Dynamic(dyn), Ty::new(T::Dynamic(dyn)))
                },
                (F::Dynamic(dyn), _) | (_, F::Dynamic(dyn)) =>
                    (F::Dynamic(dyn), Ty::new(T::Dynamic(dyn))),

                // modules and unknowns cannot be unioned (even to itself)
                (F::Module, _) | (_, F::Module) |
                (F::Unknown, _) | (_, F::Unknown) =>
                    return Err(ctx.gen_report()),

                // it's fine to merge r-values
                (F::Just, F::Just) => {
                    let ty = self.ty.read().union(&*other.ty.read(), explicit, ctx)?;
                    (F::Just, ty)
                },

                // merging Var requires a and b are identical
                // (the user is expected to annotate a and b if it's not the case)
                (F::Var, F::Var) |
                (F::Var, F::Just) |
                (F::Just, F::Var) => {
                    let ty = self.ty.read();
                    ty.assert_eq(&*other.ty.read(), ctx)?;
                    (F::Var, (*ty).clone())
                },

                // Var and Just are lifted to Const otherwise
                (F::Const, F::Just) |
                (F::Const, F::Const) |
                (F::Const, F::Var) |
                (F::Just, F::Const) |
                (F::Var, F::Const) => {
                    let ty = self.ty.read().union(&*other.ty.read(), explicit, ctx)?;
                    (F::Const, ty)
                },
            };

            Ok(S::new(flex, ty))
        })().map_err(|r: TypeReport| r.cannot_union(Origin::Slot, self, other, explicit, ctx))
    }

    pub fn assert_sub(&self, other: &S, ctx: &mut TypeContext) -> TypeResult<()> {
        debug!("asserting a constraint {:?} <: {:?}", *self, *other);

        (|| {
            match (self.flex(), other.flex()) {
                (_, F::Dynamic(_)) | (F::Dynamic(_), _) => Ok(()),

                (F::Just, _) | (_, F::Const) =>
                    self.ty.read().assert_sub(&*other.ty.read(), ctx),
                (F::Var, F::Var) =>
                    self.ty.read().assert_eq(&*other.ty.read(), ctx),

                (_, _) => Err(ctx.gen_report()),
            }
        })().map_err(|r: TypeReport| r.not_sub(Origin::Slot, self, other, ctx))
    }

    // this can replace F::Unknown, and thus is mutable
    pub fn assert_eq(&self, other: &S, ctx: &mut TypeContext) -> TypeResult<()> {
        debug!("asserting a constraint {:?} = {:?}", *self, *other);

        (|| {
            // if one flex is unknown, use the other's flex (this should be atomic)
            let mut lbits = self.bits();
            let mut rbits = other.bits();
            if lbits.flex() != rbits.flex() {
                while lbits.flex() == F::Unknown {
                    lbits.try_set_flex(rbits.flex(), &self.bits);
                }
                while rbits.flex() == F::Unknown {
                    rbits.try_set_flex(lbits.flex(), &other.bits);
                }
            }

            match (lbits.flex(), rbits.flex()) {
                (_, F::Dynamic(_)) | (F::Dynamic(_), _) => Ok(()),

                (F::Just, F::Just) | (F::Const, F::Const) |
                (F::Var, F::Var) | (F::Module, F::Module) =>
                    self.ty.read().assert_eq(&*other.ty.read(), ctx),

                (_, _) => Err(ctx.gen_report()),
            }
        })().map_err(|r: TypeReport| r.not_eq(Origin::Slot, self, other, ctx))
    }
}

impl PartialEq for S {
    fn eq(&self, other: &S) -> bool {
        Bits::load(&self.bits) == Bits::load(&other.bits) && *self.ty.read() == *other.ty.read()
    }
}

impl Clone for S {
    fn clone(&self) -> S {
        S { bits: Bits::load(&self.bits).make(), ty: RwLock::new((*self.ty.read()).clone()) }
    }
}

impl Display for S {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        if st.is_slot_seen(self) {
            return write!(f, "<...>");
        }

        let (write_ty, prefix) = match (self.flex(), &st.locale[..]) {
            (F::Unknown, "ko") => (false, "<초기화되지 않음>"),
            (F::Unknown, _)    => (false, "<not initialized>"),

            (F::Dynamic(Dyn::User), _)    => (false, "WHATEVER"),
            (F::Dynamic(Dyn::Oops), "ko") => (false, "<오류>"),
            (F::Dynamic(Dyn::Oops), _)    => (false, "<error>"),

            // Just can be seen as a mutable value yet to be assigned
            (F::Just,  _) => (true, ""),
            (F::Const, _) => (true, "const "),
            (F::Var,   _) => (true, ""),

            (F::Module, "ko") => (true, "<초기화중> "),
            (F::Module, _)    => (true, "<initializing> "),
        };

        let ret = if write_ty {
            if let Some(ty) = self.ty.try_read() {
                write!(f, "{}{}", prefix, (*ty).display(st))
            } else {
                write!(f, "{}<...>", prefix)
            }
        } else {
            write!(f, "{}", prefix)
        };
        st.unmark_slot(self);
        ret
    }
}

impl fmt::Debug for S {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ty) = self.ty.try_read() {
            write!(f, "{:?} {:?}", self.flex(), *ty)
        } else {
            write!(f, "{:?} <...>", self.flex())
        }
    }
}

/// A container for slot types.
#[derive(Clone)]
pub struct Slot(Arc<S>);

impl Slot {
    pub fn new<'a>(flex: F, ty: Ty) -> Slot {
        Slot(Arc::new(S::new(flex, ty)))
    }

    pub fn from(s: S) -> Slot {
        Slot(Arc::new(s))
    }

    pub fn just(t: Ty) -> Slot {
        Slot::new(F::Just, t)
    }

    pub fn var(t: Ty) -> Slot {
        Slot::new(F::Var, t)
    }

    pub fn dummy() -> Slot {
        Slot::new(F::Dynamic(Dyn::Oops), Ty::dummy())
    }

    pub fn unlift<'a>(&'a self) -> UnliftedSlot<'a> {
        self.0.unlift()
    }

    // one tries to assign to `self` through parent with `flex`. how should `self` change?
    // (only makes sense when `self` is a Just slot, otherwise no-op)
    pub fn adapt(&self, flex: F, _ctx: &mut TypeContext) {
        match flex {
            F::Const | F::Var => {
                let mut bits = self.0.bits();
                while bits.flex() == F::Just {
                    bits.try_set_flex(flex, &self.0.bits);
                }
            }
            _ => {}
        }
    }

    // one tries to assign `rhs` to `self`. is it *accepted*, and if so how should they change?
    // if `init` is true, this is the first time assignment with lax requirements.
    // (in this case `self` is the type derived from the specification.)
    pub fn accept(&self, rhs: &Slot, ctx: &mut TypeContext, init: bool) -> TypeResult<()> {
        // accepting itself is always fine and has no effect,
        // but it has to be filtered since it will borrow twice otherwise
        if self.0.deref() as *const _ == rhs.0.deref() as *const _ { return Ok(()); }

        debug!("accepting {:?} into {:?}{}", rhs, self, if init { " (initializing)" } else { "" });

        (|| {
            let mut bits = self.0.bits();
            loop {
                match (bits.flex(), rhs.flex(), init) {
                    (F::Dynamic(_), _, _) => return Ok(()),

                    (_, F::Unknown, _) |
                    (F::Unknown, _, _) |
                    (F::Const, _, false) => return Err(ctx.gen_report()),

                    (_, F::Dynamic(_), _) => return Ok(()),

                    // as long as the type is in agreement, Var can be assigned
                    (F::Var, _, _) =>
                        return rhs.0.ty.read().assert_sub(&*self.0.ty.read(), ctx),

                    // Module requires the strict equality on the initialization
                    (F::Module, _, true) =>
                        return rhs.0.ty.read().assert_eq(&*self.0.ty.read(), ctx),
                    (F::Module, _, false) =>
                        return rhs.0.ty.read().assert_sub(&*self.0.ty.read(), ctx),

                    // assignment to Const slot is for initialization only
                    (F::Const, _, true) =>
                        return rhs.0.ty.read().assert_sub(&*self.0.ty.read(), ctx),

                    // Just becomes Var when assignment happens
                    (F::Just, _, _) => {
                        bits.try_set_flex(F::Var, &self.0.bits);
                        // retry until the flex _really_ changes to Var
                    },
                }
            }
        })().map_err(|r: TypeReport| r.cannot_assign(Origin::Slot, &*self, &*rhs, ctx))
    }

    // one tries to modify `self` in place. the new value, when expressed as a type, is
    // NOT a subtype of the previous value, but is of the same kind (e.g. records, arrays etc).
    // is it *accepted*, and if so how should the slot change?
    //
    // conceptually this is same to `accept`, but some special types (notably nominal types)
    // have a representation that is hard to express with `accept` only.
    pub fn accept_in_place(&self, ctx: &mut TypeContext) -> TypeResult<()> {
        let mut bits = self.0.bits();
        loop {
            match bits.flex() {
                F::Dynamic(_) => return Ok(()),
                F::Unknown | F::Const => {
                    return Err(ctx.gen_report().cannot_assign_in_place(Origin::Slot, &*self, ctx));
                },
                F::Var => return Ok(()),
                F::Just => {
                    bits.try_set_flex(F::Var, &self.0.bits);
                    // retry until the flex _really_ changes to Var
                },
                // this requires an additional processing
                F::Module => return Ok(()),
            }
        }
    }

    pub fn unmark_as_module(&self) {
        let mut bits = self.0.bits();
        while bits.flex() == F::Module {
            bits.try_set_flex(F::Var, &self.0.bits);
        }
    }

    pub fn filter_by_flags(&self, flags: Flags, ctx: &mut TypeContext) -> TypeResult<()> {
        // when filter_by_flags fails, the slot itself has no valid type
        let mut ty = self.0.ty.write();
        let t = mem::replace(&mut *ty, Ty::new(T::None).or_nil(Nil::Absent));
        *ty = t.filter_by_flags(flags, ctx).map_err(|r| {
            r.cannot_filter_by_flags(Origin::Slot, &*self, ctx)
        })?;
        Ok(())
    }

    // following methods are direct analogues to value type's ones, whenever applicable

    pub fn flex(&self) -> F { self.0.flex() }
    pub fn flags(&self) -> Flags { self.unlift().flags() }

    pub fn is_integral(&self) -> bool { self.flags().is_integral() }
    pub fn is_numeric(&self)  -> bool { self.flags().is_numeric() }
    pub fn is_stringy(&self)  -> bool { self.flags().is_stringy() }
    pub fn is_tabular(&self)  -> bool { self.flags().is_tabular() }
    pub fn is_callable(&self) -> bool { self.flags().is_callable() }
    pub fn is_truthy(&self)   -> bool { self.flags().is_truthy() }
    pub fn is_falsy(&self)    -> bool { self.flags().is_falsy() }

    pub fn get_dynamic(&self) -> Option<Dyn> { self.flags().get_dynamic() }
    pub fn get_tvar(&self) -> Option<TVar> { self.unlift().get_tvar() }
    pub fn can_omit(&self) -> bool { self.unlift().can_omit() }
    pub fn tag(&self) -> Option<Tag> { self.unlift().tag() }
    pub fn nil(&self) -> Nil { self.unlift().nil() }

    pub fn with_nil(&self) -> Slot {
        Slot::from((*self.0).clone().with_nil())
    }

    pub fn without_nil(&self) -> Slot {
        Slot::from((*self.0).clone().without_nil())
    }

    pub fn coerce(&self) -> Slot {
        Slot::from((*self.0).clone().coerce())
    }

    pub fn generalize(&self, ctx: &mut TypeContext) -> Slot {
        Slot::from((*self.0).clone().generalize(ctx))
    }

    // should *not* create a new slot! (the resulting slot is not a different type,
    // but a same type with a display hint; the hint *should* be global.)
    pub fn set_display(self, disp: DisplayName) -> Slot {
        take(&mut *self.0.ty.write(), |t| t.and_display(disp));
        self
    }
}

impl Dummy for Slot {
    fn dummy() -> Slot { Self::dummy() }
}

impl Union for Slot {
    type Output = Slot;

    fn union(&self, other: &Slot, explicit: bool, ctx: &mut TypeContext) -> TypeResult<Slot> {
        // if self and other point to the same slot, do not try to borrow mutably
        if self.0.deref() as *const _ == other.0.deref() as *const _ { return Ok(self.clone()); }

        // now it is safe to borrow mutably
        Ok(Slot::from(self.0.union(&other.0, explicit, ctx)?))
    }
}

impl Lattice for Slot {
    fn assert_sub(&self, other: &Slot, ctx: &mut TypeContext) -> TypeResult<()> {
        if self.0.deref() as *const _ == other.0.deref() as *const _ { return Ok(()); }

        self.0.assert_sub(&other.0, ctx)
    }

    fn assert_eq(&self, other: &Slot, ctx: &mut TypeContext) -> TypeResult<()> {
        if self.0.deref() as *const _ == other.0.deref() as *const _ { return Ok(()); }

        self.0.assert_eq(&other.0, ctx)
    }
}

// when the RHS is a normal type the LHS is automatically unlifted; useful for rvalue-only ops.
// note that this makes a big difference from lifting the RHS.
// also, for the convenience, this does NOT print the LHS with the flexibility
// (which doesn't matter here).
impl<'a> Lattice<T<'a>> for Spanned<Slot> {
    fn assert_sub(&self, other: &T<'a>, ctx: &mut TypeContext) -> TypeResult<()> {
        self.unlift().assert_sub(other, ctx).map_err(|r| {
            r.not_sub_attach_span(self.span, Span::dummy())
        })
    }

    fn assert_eq(&self, other: &T<'a>, ctx: &mut TypeContext) -> TypeResult<()> {
        self.unlift().assert_eq(other, ctx).map_err(|r| {
            r.not_eq_attach_span(self.span, Span::dummy())
        })
    }
}

impl<'a> Lattice<Ty> for Spanned<Slot> {
    fn assert_sub(&self, other: &Ty, ctx: &mut TypeContext) -> TypeResult<()> {
        self.unlift().assert_sub(other, ctx).map_err(|r| {
            r.not_sub_attach_span(self.span, Span::dummy())
        })
    }

    fn assert_eq(&self, other: &Ty, ctx: &mut TypeContext) -> TypeResult<()> {
        self.unlift().assert_eq(other, ctx).map_err(|r| {
            r.not_eq_attach_span(self.span, Span::dummy())
        })
    }
}

impl PartialEq for Slot {
    fn eq(&self, other: &Slot) -> bool {
        if self.0.deref() as *const _ == other.0.deref() as *const _ { return true; }
        *self.0 == *other.0
    }
}

impl Display for Slot {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        self.0.fmt_displayed(f, st)
    }
}

impl fmt::Debug for Slot {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.sign_minus() {
            if let Some(ty) = self.0.ty.try_read() {
                fmt::Debug::fmt(&*ty, f)
            } else {
                write!(f, "<...>")
            }
        } else {
            fmt::Debug::fmt(&self.0, f)
        }
    }
}

#[cfg(test)]
#[allow(unused_variables, dead_code)]
mod tests {
    use ty::{T, Ty, Lattice, NoTypeContext};
    use super::*;

    #[test]
    fn test_sub() {
        let just = |t| Slot::new(F::Just, Ty::new(t));
        let cnst = |t| Slot::new(F::Const, Ty::new(t));
        let var = |t| Slot::new(F::Var, Ty::new(t));

        assert!(just(T::Integer).assert_sub(&just(T::Integer), &mut NoTypeContext).is_ok());
        assert!(just(T::Integer).assert_sub(&just(T::Number), &mut NoTypeContext).is_ok());
        assert!(just(T::Number).assert_sub(&just(T::Integer), &mut NoTypeContext).is_err());

        assert!(var(T::Integer).assert_sub(&var(T::Integer), &mut NoTypeContext).is_ok());
        assert!(var(T::Integer).assert_sub(&var(T::Number), &mut NoTypeContext).is_err());
        assert!(var(T::Number).assert_sub(&var(T::Integer), &mut NoTypeContext).is_err());

        assert!(cnst(T::Integer).assert_sub(&cnst(T::Integer), &mut NoTypeContext).is_ok());
        assert!(cnst(T::Integer).assert_sub(&cnst(T::Number), &mut NoTypeContext).is_ok());
        assert!(cnst(T::Number).assert_sub(&cnst(T::Integer), &mut NoTypeContext).is_err());
    }
}

