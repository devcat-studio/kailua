use std::fmt;
use std::ops::Deref;
use diag::CheckResult;
use super::{T, S, Slot, Lattice, TypeContext};

// Array and Map values should NOT contain nil,
// since there are always keys that do not map to the value of that type
// and indexing always results in the value type plus nil.
// in order to normalize this use case we (implicitly) add nils as soon as
// it is assigned to Array and Map values.
// internally the slot itself does not have nils.

#[derive(Clone, PartialEq)]
pub struct TyWithNil { ty: T<'static> }

impl TyWithNil {
    pub fn from<'a>(t: T<'a>) -> TyWithNil {
        TyWithNil { ty: t.into_send().without_nil() }
    }

    pub fn into_type_without_nil(self) -> T<'static> {
        self.ty
    }

    pub fn into_type(self) -> T<'static> {
        self.ty | T::Nil
    }
}

impl Deref for TyWithNil {
    type Target = T<'static>;
    fn deref(&self) -> &T<'static> { &self.ty }
}

impl Lattice for TyWithNil {
    type Output = TyWithNil;
    fn do_union(&self, other: &TyWithNil, ctx: &mut TypeContext) -> TyWithNil {
        // union cannot introduce nils
        TyWithNil { ty: self.ty.union(&other.ty, ctx) }
    }
    fn do_assert_sub(&self, other: &TyWithNil, ctx: &mut TypeContext) -> CheckResult<()> {
        // since both self and other have been canonicalized
        self.ty.assert_sub(&other.ty, ctx)
    }
    fn do_assert_eq(&self, other: &TyWithNil, ctx: &mut TypeContext) -> CheckResult<()> {
        // since both self and other have been canonicalized
        self.ty.assert_eq(&other.ty, ctx)
    }
}

impl fmt::Debug for TyWithNil {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { fmt::Debug::fmt(&self.ty, f) }
}

#[derive(Clone, PartialEq)]
pub struct SlotWithNil { slot: Slot }

impl SlotWithNil {
    pub fn from_slot(s: Slot) -> SlotWithNil {
        SlotWithNil { slot: s.without_nil() }
    }

    pub fn new<'a>(s: S<'a>) -> SlotWithNil {
        SlotWithNil { slot: Slot::new(s.into_send().without_nil()) }
    }

    pub fn from<'a>(t: T<'a>) -> SlotWithNil {
        SlotWithNil { slot: Slot::just(t.into_send().without_nil()) }
    }

    pub fn from_ty_with_nil(t: TyWithNil) -> SlotWithNil {
        SlotWithNil { slot: Slot::just(t.ty) }
    }

    pub fn into_slot(self) -> Slot {
        let s = match *self.slot.borrow() {
            S::Any                      => S::Any,
            S::Just(ref t)              => S::Just(t.clone().into_send() | T::Nil),
            S::Const(ref t)             => S::Const(t.clone().into_send() | T::Nil),
            S::Var(ref t)               => S::Var(t.clone().into_send() | T::Nil),
            S::Currently(ref t)         => S::Currently(t.clone().into_send() | T::Nil),
            S::VarOrConst(ref t, m)     => S::VarOrConst(t.clone().into_send() | T::Nil, m),
            S::VarOrCurrently(ref t, m) => S::VarOrCurrently(t.clone().into_send() | T::Nil, m),
        };
        Slot::new(s)
    }
}

impl Deref for SlotWithNil {
    type Target = Slot;
    fn deref(&self) -> &Slot { &self.slot }
}

impl Lattice for SlotWithNil {
    type Output = SlotWithNil;
    fn do_union(&self, other: &SlotWithNil, ctx: &mut TypeContext) -> SlotWithNil {
        // union cannot introduce nils
        SlotWithNil { slot: self.slot.union(&other.slot, ctx) }
    }
    fn do_assert_sub(&self, other: &SlotWithNil, ctx: &mut TypeContext) -> CheckResult<()> {
        // since both self and other have been canonicalized
        self.slot.assert_sub(&other.slot, ctx)
    }
    fn do_assert_eq(&self, other: &SlotWithNil, ctx: &mut TypeContext) -> CheckResult<()> {
        // since both self and other have been canonicalized
        self.slot.assert_eq(&other.slot, ctx)
    }
}

impl fmt::Debug for SlotWithNil {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { fmt::Debug::fmt(&self.slot, f) }
}

