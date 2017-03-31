use std::fmt;
use std::mem;
use std::ops::Deref;
use std::sync::Arc;
use parking_lot::{RwLock, RwLockReadGuard};

use kailua_env::{Span, Spanned};
use kailua_syntax::{M, MM};
use diag::Origin;
use super::{Dyn, Nil, T, Ty, TypeContext, Lattice, Union, Dummy, TVar, Tag};
use super::{Display, DisplayState, TypeReport, TypeResult};
use super::flags::Flags;

// slot type flexibility (a superset of type mutability)
#[derive(Copy, Clone, PartialEq)]
pub enum F {
    // unknown flexibility (e.g. right after the generation but before the initialization)
    Unknown,
    // dynamic slot; all assignments are allowed and ignored
    Dynamic(Dyn),
    // temporary r-value slot
    // updates in place to Var (default) or Const depending on the unification
    Just,
    // covariant immutable slot
    Const,
    // invariant mutable slot
    Var,
    // postponed invariant mutable slot
    // this is a variant of Var but only allows for indexing (no subtyping), and any types
    // assigned via indexing will be marked as type-checked later
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

// slot types
#[derive(Clone, PartialEq)]
pub struct S {
    flex: F,
    ty: Ty,
}

impl S {
    pub fn flex(&self) -> F {
        self.flex
    }

    pub fn unlift(&self) -> &Ty {
        &self.ty
    }

    pub fn with_nil(self) -> S {
        S { flex: self.flex, ty: self.ty.with_nil() }
    }

    pub fn without_nil(self) -> S {
        S { flex: self.flex, ty: self.ty.without_nil() }
    }

    // in addition to `Ty::coerce`, also updates the flexibility
    pub fn coerce(self) -> S {
        let flex = match self.flex {
            F::Just | F::Const | F::Var => F::Var,
            flex => flex,
        };
        S { flex: flex, ty: self.ty.coerce() }
    }

    pub fn generalize(self, ctx: &mut TypeContext) -> S {
        S { flex: self.flex, ty: self.ty.generalize(ctx) }
    }

    // self and other may be possibly different slots and being merged by union
    // mainly used by `and`/`or` operators and table lifting
    pub fn union(&mut self, other: &mut S, explicit: bool,
                 ctx: &mut TypeContext) -> TypeResult<S> {
        (|| {
            let (flex, ty) = match (self.flex, other.flex) {
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
                (F::Just, F::Just) => (F::Just, self.ty.union(&other.ty, explicit, ctx)?),

                // merging Var requires a and b are identical
                // (the user is expected to annotate a and b if it's not the case)
                (F::Var, F::Var) |
                (F::Var, F::Just) |
                (F::Just, F::Var) => {
                    self.ty.assert_eq(&other.ty, ctx)?;
                    (F::Var, self.ty.clone())
                },

                // Var and Just are lifted to Const otherwise
                (F::Const, F::Just) |
                (F::Const, F::Const) |
                (F::Const, F::Var) |
                (F::Just, F::Const) |
                (F::Var, F::Const) =>
                    (F::Const, self.ty.union(&other.ty, explicit, ctx)?),
            };

            Ok(S { flex: flex, ty: ty })
        })().map_err(|r: TypeReport| r.cannot_union(Origin::Slot, self, other, explicit, ctx))
    }

    pub fn assert_sub(&self, other: &S, ctx: &mut TypeContext) -> TypeResult<()> {
        debug!("asserting a constraint {:?} <: {:?}", *self, *other);

        (|| {
            match (self.flex, other.flex) {
                (_, F::Dynamic(_)) | (F::Dynamic(_), _) => Ok(()),

                (F::Just, _) | (_, F::Const) => self.ty.assert_sub(&other.ty, ctx),
                (F::Var, F::Var) => self.ty.assert_eq(&other.ty, ctx),

                (_, _) => Err(ctx.gen_report()),
            }
        })().map_err(|r: TypeReport| r.not_sub(Origin::Slot, self, other, ctx))
    }

    // this can replace F::Unknown, and thus is mutable
    pub fn assert_eq(&mut self, other: &mut S, ctx: &mut TypeContext) -> TypeResult<()> {
        debug!("asserting a constraint {:?} = {:?}", *self, *other);

        (|| {
            // if one flex is unknown, use the other's flex
            if self.flex == F::Unknown {
                self.flex = other.flex;
            } else if other.flex == F::Unknown {
                other.flex = self.flex;
            }

            match (self.flex, other.flex) {
                (_, F::Dynamic(_)) | (F::Dynamic(_), _) => Ok(()),

                (F::Just, F::Just) | (F::Const, F::Const) |
                (F::Var, F::Var) | (F::Module, F::Module) =>
                    self.ty.assert_eq(&other.ty, ctx),

                (_, _) => Err(ctx.gen_report()),
            }
        })().map_err(|r: TypeReport| r.not_eq(Origin::Slot, self, other, ctx))
    }
}

impl Display for S {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        match (self.flex, &st.locale[..]) {
            (F::Unknown, "ko") => write!(f, "<초기화되지 않음>"),
            (F::Unknown, _)    => write!(f, "<not initialized>"),

            (F::Dynamic(Dyn::User), _)    => write!(f, "WHATEVER"),
            (F::Dynamic(Dyn::Oops), "ko") => write!(f, "<오류>"),
            (F::Dynamic(Dyn::Oops), _)    => write!(f, "<error>"),

            // Just can be seen as a mutable value yet to be assigned
            (F::Just,  _) => write!(f, "{}", self.ty.display(st)),
            (F::Const, _) => write!(f, "const {}", self.ty.display(st)),
            (F::Var,   _) => write!(f, "{}", self.ty.display(st)),

            (F::Module, "ko") => write!(f, "<초기화중> {}", self.ty.display(st)),
            (F::Module, _)    => write!(f, "<initializing> {}", self.ty.display(st)),
        }
    }
}

impl fmt::Debug for S {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {:?}", self.flex, self.ty)
    }
}

pub struct UnliftedSlot<'a>(RwLockReadGuard<'a, S>);

impl<'a> Deref for UnliftedSlot<'a> {
    type Target = Ty;
    fn deref(&self) -> &Ty { self.0.unlift() }
}

#[derive(Clone)]
pub struct Slot(Arc<RwLock<S>>);

impl Slot {
    pub fn new<'a>(flex: F, ty: Ty) -> Slot {
        Slot(Arc::new(RwLock::new(S { flex: flex, ty: ty })))
    }

    pub fn from(s: S) -> Slot {
        Slot(Arc::new(RwLock::new(s)))
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
        UnliftedSlot(self.0.read())
    }

    // one tries to assign to `self` through parent with `flex`. how should `self` change?
    // (only makes sense when `self` is a Just slot, otherwise no-op)
    pub fn adapt(&self, flex: F, _ctx: &mut TypeContext) {
        let mut slot = self.0.write();
        if slot.flex == F::Just {
            slot.flex = match flex {
                F::Const | F::Var => flex,
                _ => F::Just,
            };
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

        let mut lhs = self.0.write();
        let rhs = rhs.0.read();

        (|| {
            match (lhs.flex, rhs.flex, init) {
                (F::Dynamic(_), _, _) => Ok(()),

                (_, F::Unknown, _) |
                (F::Unknown, _, _) |
                (F::Const, _, false) => Err(ctx.gen_report()),

                (_, F::Dynamic(_), _) => Ok(()),

                // as long as the type is in agreement, Var can be assigned
                (F::Var, _, _) => rhs.ty.assert_sub(&lhs.ty, ctx),

                // Module requires the strict equality on the initialization
                (F::Module, _, true) => rhs.ty.assert_eq(&lhs.ty, ctx),
                (F::Module, _, false) => rhs.ty.assert_sub(&lhs.ty, ctx),

                // assignment to Const slot is for initialization only
                (F::Const, _, true) => rhs.ty.assert_sub(&lhs.ty, ctx),

                // Just becomes Var when assignment happens
                (F::Just, _, _) => {
                    rhs.ty.assert_sub(&lhs.ty, ctx)?;
                    lhs.flex = F::Var;
                    Ok(())
                }
            }
        })().map_err(|r: TypeReport| r.cannot_assign(Origin::Slot, &*lhs, &*rhs, ctx))
    }

    // one tries to modify `self` in place. the new value, when expressed as a type, is
    // NOT a subtype of the previous value, but is of the same kind (e.g. records, arrays etc).
    // is it *accepted*, and if so how should the slot change?
    //
    // conceptually this is same to `accept`, but some special types (notably nominal types)
    // have a representation that is hard to express with `accept` only.
    pub fn accept_in_place(&self, ctx: &mut TypeContext) -> TypeResult<()> {
        let mut s = self.0.write();
        match s.flex {
            F::Dynamic(_) => Ok(()),
            F::Unknown | F::Const => {
                Err(ctx.gen_report().cannot_assign_in_place(Origin::Slot, &*s, ctx))
            },
            F::Var => Ok(()),
            F::Just => {
                s.flex = F::Var;
                Ok(())
            },
            // this requires an additional processing
            F::Module => Ok(()),
        }
    }

    pub fn unmark_as_module(&self) {
        let mut s = self.0.write();
        if s.flex == F::Module {
            s.flex = F::Var;
        }
    }

    pub fn filter_by_flags(&self, flags: Flags, ctx: &mut TypeContext) -> TypeResult<()> {
        // when filter_by_flags fails, the slot itself has no valid type
        let mut s = self.0.write();
        let t = mem::replace(&mut s.ty, Ty::new(T::None).or_nil(Nil::Absent));
        s.ty = t.filter_by_flags(flags, ctx).map_err(|r| {
            r.cannot_filter_by_flags(Origin::Slot, &*s, ctx)
        })?;
        Ok(())
    }

    // following methods are direct analogues to value type's ones, whenever applicable

    pub fn flex(&self) -> F { self.0.read().flex() }
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
        let s = self.0.read();
        Slot::from(s.clone().with_nil())
    }

    pub fn without_nil(&self) -> Slot {
        let s = self.0.read();
        Slot::from(s.clone().without_nil())
    }

    pub fn coerce(&self) -> Slot {
        let s = self.0.read();
        Slot::from(s.clone().coerce())
    }

    pub fn generalize(&self, ctx: &mut TypeContext) -> Slot {
        let s = self.0.read();
        Slot::from(s.clone().generalize(ctx))
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
        Ok(Slot::from(self.0.write().union(&mut other.0.write(), explicit, ctx)?))
    }
}

impl Lattice for Slot {
    fn assert_sub(&self, other: &Slot, ctx: &mut TypeContext) -> TypeResult<()> {
        if self.0.deref() as *const _ == other.0.deref() as *const _ { return Ok(()); }

        self.0.read().assert_sub(&other.0.read(), ctx)
    }

    fn assert_eq(&self, other: &Slot, ctx: &mut TypeContext) -> TypeResult<()> {
        if self.0.deref() as *const _ == other.0.deref() as *const _ { return Ok(()); }

        self.0.write().assert_eq(&mut other.0.write(), ctx)
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
        *self.0.read() == *other.0.read()
    }
}

impl Display for Slot {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        if let Some(s) = self.0.try_read() {
            s.fmt_displayed(f, st)
        } else {
            write!(f, "<...>")
        }
    }
}

impl fmt::Debug for Slot {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(s) = self.0.try_read() {
            if f.sign_minus() {
                fmt::Debug::fmt(&s.ty, f)
            } else {
                fmt::Debug::fmt(&*s, f)
            }
        } else {
            write!(f, "<recursion>")
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

