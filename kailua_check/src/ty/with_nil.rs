use std::fmt;
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

    pub fn as_type_without_nil(&self) -> &T<'static> {
        &self.ty
    }

    pub fn into_type_without_nil(self) -> T<'static> {
        self.ty
    }

    pub fn into_type(self) -> T<'static> {
        self.ty | T::Nil
    }
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
        SlotWithNil { slot: Slot::from(s.into_send().without_nil()) }
    }

    pub fn from<'a>(t: T<'a>) -> SlotWithNil {
        SlotWithNil { slot: Slot::just(t.into_send().without_nil()) }
    }

    pub fn from_ty_with_nil(t: TyWithNil) -> SlotWithNil {
        SlotWithNil { slot: Slot::just(t.ty) }
    }

    pub fn as_slot_without_nil(&self) -> &Slot {
        &self.slot
    }

    pub fn into_slot_without_nil(self) -> Slot {
        self.slot
    }

    pub fn into_slot(self) -> Slot {
        Slot::new(self.slot.flex(), self.slot.unlift().clone().into_send() | T::Nil)
    }
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

