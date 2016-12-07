use std::fmt;
use std::mem;
use std::ops::Deref;
use std::rc::Rc;
use std::cell::{Ref, RefCell};

use kailua_env::Spanned;
use kailua_diag::Reporter;
use kailua_syntax::M;
use diag::CheckResult;
use super::{Dyn, Nil, T, Ty, TypeContext, Lattice, Union, Display, Mark, TVar, Tag};
use super::{error_not_sub, error_not_eq};
use super::flags::Flags;
use message as m;

// slot type flexibility (a superset of type mutability)
#[derive(Copy, Clone)]
pub enum F {
    // invalid top type (no type information available)
    // for the typing convenience the type information itself is retained, but never used.
    Any,
    // dynamic slot; all assignments are allowed and ignored
    Dynamic(Dyn),
    // temporary r-value slot
    // coerces to VarOrCurrently when used in the table fields
    Just,
    // covariant immutable slot
    Const,
    // invariant mutable slot
    Var,
    // uniquely mutable slot
    // - assigning Currently to Currently updates and converts the slot type to Var
    // - assigning other types to Currently updates the slot type
    Currently,
    // either Var (when mark is true) or Const (when mark is false)
    VarOrConst(Mark),
    // either Var (when mark is true) or Currently (when mark is false)
    // - due to the implementation issues, weakening results in forcing Var
    VarOrCurrently(Mark),
}

impl F {
    pub fn from(modf: M) -> F {
        match modf {
            M::None => F::Var,
            M::Const => F::Const,
        }
    }

    // is possibly linear, and thus should be uniquely owned?
    // (there are some cases that this is impossible, e.g. array types)
    pub fn is_linear(&self) -> bool {
        match *self {
            F::Currently | F::VarOrCurrently(_) => true,
            _ => false,
        }
    }

    pub fn weaken(&self, ctx: &mut TypeContext) -> CheckResult<F> {
        match *self {
            F::Any => Ok(F::Any),

            F::Dynamic(dyn) => Ok(F::Dynamic(dyn)),

            // if the Just slot contains a table it should be recursively weakened.
            // this is not handled here, but via `Slot::adapt` which gets called
            // whenever the Just slot is returned from the lvalue table (`Checker::check_index`).
            F::Just => Ok(F::VarOrCurrently(ctx.gen_mark())),

            F::Const => Ok(F::Const),

            F::Var => Ok(F::VarOrConst(ctx.gen_mark())),

            F::Currently => Ok(F::Any),

            F::VarOrConst(m) => Ok(F::VarOrConst(m)),

            F::VarOrCurrently(m) => {
                // we originally had F::PossiblyVarOrConst for this case.
                // but it, having two different marks, was particularly hard to
                // fit with the deterministic typing framework.
                // since weaken is only called when the upvalue is required,
                // it is safe to assume that m should be true.
                m.assert_true(ctx)?;
                Ok(F::VarOrConst(ctx.gen_mark()))
            },
        }
    }

    pub fn resolve(&self, ctx: &TypeContext) -> F {
        match *self {
            F::VarOrConst(m) => match ctx.get_mark_exact(m) {
                Some(true) => F::Var,
                Some(false) => F::Const,
                None => *self,
            },
            F::VarOrCurrently(m) => match ctx.get_mark_exact(m) {
                Some(true) => F::Var,
                Some(false) => F::Currently,
                None => *self,
            },
            _ => *self,
        }
    }
}

impl PartialEq for F {
    fn eq(&self, other: &F) -> bool {
        match (*self, *other) {
            (F::Any, F::Any) => true,
            (F::Dynamic(dyn1), F::Dynamic(dyn2)) => dyn1 == dyn2,
            (F::Just, F::Just) => true,
            (F::Const, F::Const) => true,
            (F::Var, F::Var) => true,
            (F::Currently, F::Currently) => true,

            // Mark::any() is used for debugging comparison
            (F::VarOrConst(am), F::VarOrConst(bm)) =>
                am == Mark::any() || bm == Mark::any() || am == bm,
            (F::VarOrCurrently(am), F::VarOrCurrently(bm)) =>
                am == Mark::any() || bm == Mark::any() || am == bm,

            (_, _) => false,
        }
    }
}

impl<'a> fmt::Debug for F {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            F::Any                => write!(f, "any"),
            F::Dynamic(Dyn::User) => write!(f, "?"),
            F::Dynamic(Dyn::Oops) => write!(f, "?!"),
            F::Just               => write!(f, "just"),
            F::Const              => write!(f, "const"),
            F::Var                => write!(f, "var"),
            F::Currently          => write!(f, "currently"),
            F::VarOrConst(m)      => write!(f, "{:?}?var:const", m),
            F::VarOrCurrently(m)  => write!(f, "{:?}?var:currently", m),
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

    pub fn weaken(&self, ctx: &mut TypeContext) -> CheckResult<S> {
        Ok(S { flex: self.flex.weaken(ctx)?, ty: self.ty.clone() })
    }

    pub fn with_nil(self) -> S {
        S { flex: self.flex, ty: self.ty.with_nil() }
    }

    pub fn without_nil(self) -> S {
        S { flex: self.flex, ty: self.ty.without_nil() }
    }

    // self and other may be possibly different slots and being merged by union
    // (thus Currently cannot be merged, even if the type is identical)
    // mainly used by `and`/`or` operators and table lifting
    pub fn union(&mut self, other: &mut S, explicit: bool,
                 ctx: &mut TypeContext) -> CheckResult<S> {
        // Currently is first *changed* to Var, since it will be going to be shared anyway
        if self.flex.is_linear() { self.flex = F::Var; }
        if other.flex.is_linear() { other.flex = F::Var; }

        let (flex, ty) = match (self.flex, other.flex) {
            (F::Dynamic(dyn1), F::Dynamic(dyn2)) => {
                let dyn = dyn1.union(dyn2);
                (F::Dynamic(dyn), Ty::new(T::Dynamic(dyn)))
            },
            (F::Dynamic(dyn), _) | (_, F::Dynamic(dyn)) =>
                (F::Dynamic(dyn), Ty::new(T::Dynamic(dyn))),
            (F::Any, _) | (_, F::Any) => (F::Any, Ty::new(T::None)),

            // it's fine to merge r-values
            (F::Just, F::Just) => (F::Just, self.ty.union(&other.ty, explicit, ctx)?),

            // merging Var will result in Const unless a and b are identical
            (F::Var, F::Var) |
            (F::Var, F::Just) |
            (F::Just, F::Var) => {
                let m = ctx.gen_mark();
                assert_eq!(ctx.assert_mark_require_eq(m, &self.ty, &other.ty),
                           Ok(())); // can't fail
                (F::VarOrConst(m), self.ty.union(&other.ty, explicit, ctx)?)
            },

            // Var and VarConst are lifted to Const otherwise, regardless of marks
            (F::Const, F::Just) |
            (F::Const, F::Const) |
            (F::Const, F::Var) |
            (F::Const, F::VarOrConst(_)) |
            (F::Just, F::Const) |
            (F::Just, F::VarOrConst(_)) |
            (F::Var, F::Const) |
            (F::Var, F::VarOrConst(_)) |
            (F::VarOrConst(_), F::Just) |
            (F::VarOrConst(_), F::Const) |
            (F::VarOrConst(_), F::Var) |
            (F::VarOrConst(_), F::VarOrConst(_)) =>
                (F::Const, self.ty.union(&other.ty, explicit, ctx)?),

            // other cases are impossible
            (F::Currently, _) | (_, F::Currently) => unreachable!(),
            (F::VarOrCurrently(_), _) | (_, F::VarOrCurrently(_)) => unreachable!(),
        };

        Ok(S { flex: flex, ty: ty })
    }

    pub fn assert_sub(&self, other: &S, ctx: &mut TypeContext) -> CheckResult<()> {
        debug!("asserting a constraint {:?} <: {:?}", *self, *other);

        macro_rules! m {
            ($(; true, $tm:expr)*
             $(; false, $fm:expr)*
             $(; eq, $em1:expr, $em2:expr)*
             $(; imply, $im1:expr, $im2:expr)*
             $(; require_eq, $rm:expr)*) => ({
                $($tm.assert_true(ctx)?;)*
                $($fm.assert_false(ctx)?;)*
                $($em1.assert_eq($em2, ctx)?;)*
                $($im1.assert_imply($im2, ctx)?;)*
                $($rm.assert_require_eq(&other.ty, &self.ty, ctx)?;)*
            });

            (a <: b $($t:tt)*) => ({ self.ty.assert_sub(&other.ty, ctx)?; m!($($t)*) });
            (a = b $($t:tt)*) => ({ self.ty.assert_eq(&other.ty, ctx)?; m!($($t)*) });
        }

        match (self.flex, other.flex) {
            (_, F::Any) => {}
            (_, F::Dynamic(_)) | (F::Dynamic(_), _) => {}

            (F::Just, F::Just) => m!(a <: b),

            (F::Just,               F::Const) => m!(a <: b),
            (F::Const,              F::Const) => m!(a <: b),
            (F::Var,                F::Const) => m!(a <: b),
            (F::VarOrConst(_),      F::Const) => m!(a <: b),
            (F::VarOrCurrently(am), F::Const) => m!(a <: b; true, am),

            (F::Just,               F::Var) => m!(a <: b),
            (F::Var,                F::Var) => m!(a = b),
            (F::VarOrConst(am),     F::Var) => m!(a = b; true, am),
            (F::VarOrCurrently(am), F::Var) => m!(a = b; true, am),

            (F::Just,               F::Currently) => m!(a <: b),
            (F::Currently,          F::Currently) => m!(a = b),
            (F::VarOrCurrently(am), F::Currently) => m!(a = b; true, am),

            (F::Just,               F::VarOrConst(_))  => m!(a <: b),
            (F::Const,              F::VarOrConst(bm)) => m!(a <: b; false, bm),
            (F::Var,                F::VarOrConst(bm)) => m!(a <: b; false, bm),
            (F::VarOrConst(am),     F::VarOrConst(bm)) => m!(a <: b; imply, bm, am; require_eq, bm),
            (F::VarOrCurrently(am), F::VarOrConst(bm)) => m!(a <: b; true, am; require_eq, bm),

            (F::Just,               F::VarOrCurrently(_))  => m!(a <: b),
            (F::Var,                F::VarOrCurrently(bm)) => m!(a = b; true, bm),
            (F::Currently,          F::VarOrCurrently(bm)) => m!(a = b; false, bm),
            (F::VarOrConst(am),     F::VarOrCurrently(bm)) => m!(a = b; true, am; true, bm),
            (F::VarOrCurrently(am), F::VarOrCurrently(bm)) => m!(a = b; eq, am, bm),

            (_, _) => return error_not_sub(self, other),
        }

        Ok(())
    }

    pub fn assert_eq(&self, other: &S, ctx: &mut TypeContext) -> CheckResult<()> {
        debug!("asserting a constraint {:?} = {:?}", *self, *other);

        macro_rules! m {
            ($(; true, $tm:expr)*
             $(; false, $fm:expr)*
             $(; eq, $em1:expr, $em2:expr)*) => ({
                $($tm.assert_true(ctx)?;)*
                $($fm.assert_false(ctx)?;)*
                $($em1.assert_eq($em2, ctx)?;)*
            });

            (a = b $($t:tt)*) => ({ self.ty.assert_eq(&other.ty, ctx)?; m!($($t)*) });
        }

        match (self.flex, other.flex) {
            (F::Any, F::Any) => {}
            (_, F::Dynamic(_)) | (F::Dynamic(_), _) => {}

            (F::Just, F::Just) => m!(a = b),

            (F::Currently, F::Currently)          => m!(a = b),
            (F::Currently, F::VarOrCurrently(bm)) => m!(a = b; false, bm),

            (F::Const, F::Const)          => m!(a = b),
            (F::Const, F::VarOrConst(bm)) => m!(a = b; false, bm),

            (F::Var, F::Var)                => m!(a = b),
            (F::Var, F::VarOrConst(bm))     => m!(a = b; true, bm),
            (F::Var, F::VarOrCurrently(bm)) => m!(a = b; true, bm),

            (F::VarOrConst(am), F::Const)              => m!(a = b; false, am),
            (F::VarOrConst(am), F::Var)                => m!(a = b; true, am),
            (F::VarOrConst(am), F::VarOrConst(bm))     => m!(a = b; eq, am, bm),
            (F::VarOrConst(am), F::VarOrCurrently(bm)) => m!(a = b; true, am; true, bm),

            (F::VarOrCurrently(am), F::Var)                => m!(a = b; true, am),
            (F::VarOrCurrently(am), F::Currently)          => m!(a = b; false, am),
            (F::VarOrCurrently(am), F::VarOrConst(bm))     => m!(a = b; true, am; true, bm),
            (F::VarOrCurrently(am), F::VarOrCurrently(bm)) => m!(a = b; eq, am, bm),

            (_, _) => return error_not_eq(self, other),
        }

        Ok(())
    }
}

impl fmt::Debug for S {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {:?}", self.flex, self.ty)
    }
}

#[derive(Clone, PartialEq)]
pub struct Slot(Rc<RefCell<S>>);

impl Slot {
    pub fn new<'a>(flex: F, ty: Ty) -> Slot {
        Slot(Rc::new(RefCell::new(S { flex: flex, ty: ty })))
    }

    pub fn from(s: S) -> Slot {
        Slot(Rc::new(RefCell::new(s)))
    }

    pub fn just(t: Ty) -> Slot {
        Slot::new(F::Just, t)
    }

    pub fn var(t: Ty, ctx: &mut TypeContext) -> Slot {
        Slot::new(F::VarOrCurrently(ctx.gen_mark()), t)
    }

    pub fn dummy() -> Slot {
        Slot::new(F::Dynamic(Dyn::Oops), Ty::dummy())
    }

    pub fn unlift<'a>(&'a self) -> Ref<'a, Ty> {
        Ref::map(self.0.borrow(), |s| s.unlift())
    }

    // one tries to assign to `self` through parent with `flex`. how should `self` change?
    // (only makes sense when `self` is a Just slot, otherwise no-op)
    pub fn adapt(&self, flex: F, ctx: &mut TypeContext) {
        let mut slot = self.0.borrow_mut();
        if slot.flex == F::Just {
            slot.flex = match flex {
                F::Const => F::Const,
                F::Var => F::Var,
                F::Currently => F::VarOrCurrently(ctx.gen_mark()),
                F::VarOrConst(m) => F::VarOrConst(m),
                F::VarOrCurrently(m) => F::VarOrCurrently(m),
                _ => F::Just,
            };
        }
    }

    // one tries to assign `rhs` to `self`. is it *accepted*, and if so how should they change?
    // if `init` is true, this is the first time assignment with lax requirements.
    pub fn accept(&self, rhs: &Slot, ctx: &mut TypeContext, init: bool) -> CheckResult<()> {
        // accepting itself is always fine and has no effect,
        // but it has to be filtered since it will borrow twice otherwise
        if self.0.deref() as *const _ == rhs.0.deref() as *const _ { return Ok(()); }

        let mut lhs = self.0.borrow_mut();
        let mut rhs = rhs.0.borrow_mut();

        // assumes that lhs.flex is already known to be linear.
        let is_shared = |lty: &T, rhs: &S|
            lty.is_referential() && rhs.flex().is_linear() && rhs.unlift().is_referential();

        match (lhs.flex, rhs.flex, init) {
            (F::Dynamic(_), _, _) => {}

            (_, F::Any, _) |
            (F::Any, _, _) |
            (F::Just, _, _) |
            (F::Const, _, false) => {
                return Err(format!("impossible to assign {:?} to {:?}", rhs, lhs));
            }

            // dynamic rhs *may* change the flex of lhs as well, if the initial flex permits
            (F::Currently, F::Dynamic(dyn), _) | (F::VarOrCurrently(_), F::Dynamic(dyn), _) => {
                lhs.flex = F::Dynamic(dyn);
                lhs.ty = Ty::new(T::Dynamic(dyn));
            }

            (_, F::Dynamic(_), _) => {}

            // as long as the type is in agreement, Var can be assigned
            (F::Var, _, _) => {
                rhs.ty.assert_sub(&lhs.ty, ctx)?;
            }

            // assignment to Const slot is for initialization only
            (F::Const, _, true) => {
                rhs.ty.assert_sub(&lhs.ty, ctx)?;
            }

            // non-Currently value can be assigned to Currently while changing its type
            (F::Currently, _, _) => {
                // if both lhs and rhs are linear and tabular,
                // the linearity is broken and both lhs and rhs have to be changed as well
                if is_shared(&lhs.ty, &rhs) {
                    lhs.flex = F::Var;
                    rhs.flex = F::Var;
                }
                lhs.ty = rhs.ty.clone();
            }

            // assigning to VarOrConst asserts the mark and makes it Var
            (F::VarOrConst(m), _, false) => {
                m.assert_true(ctx)?;
                rhs.ty.assert_sub(&lhs.ty, ctx)?;
                lhs.flex = F::Var;
            }

            // ...except for the first time
            (F::VarOrConst(_), _, true) => {
                rhs.ty.assert_sub(&lhs.ty, ctx)?;
            }

            // assigning to VarOrCurrently adds a new equality requirement to the mark
            (F::VarOrCurrently(_), _, _) => {
                // ditto as above
                if is_shared(&lhs.ty, &rhs) {
                    lhs.flex = F::Var;
                    rhs.flex = F::Var;
                } else {
                    let m = ctx.gen_mark(); // the prior mark had an incompatible base type
                    m.assert_require_sup(&lhs.ty, &rhs.ty, ctx)?;
                    lhs.flex = F::VarOrCurrently(m);
                }
                lhs.ty = rhs.ty.clone();
            }
        };

        Ok(())
    }

    // one tries to modify `self` in place. the new value, when expressed as a type, is
    // NOT a subtype of the previous value, but is of the same kind (e.g. records, arrays etc).
    // is it *accepted*, and if so how should the slot change?
    //
    // conceptually this is same to `accept`, but some special types (notably nominal types)
    // have a representation that is hard to express with `accept` only.
    pub fn accept_in_place(&self, ctx: &mut TypeContext) -> CheckResult<()> {
        let s = self.0.borrow();

        match s.flex {
            F::Dynamic(_) => {}

            F::Any | F::Just | F::Const | F::Var | F::VarOrConst(_) => {
                return Err(format!("impossible to update {:?}", s));
            }

            // since the value is not shared via this "assignment", it can remain as Currently.
            F::Currently => {}

            // it cannot be Var
            F::VarOrCurrently(m) => {
                m.assert_false(ctx)?;
            }
        };

        Ok(())
    }

    pub fn filter_by_flags(&self, flags: Flags, ctx: &mut TypeContext) -> CheckResult<()> {
        // when filter_by_flags fails, the slot itself has no valid type
        let mut s = self.0.borrow_mut();
        let t = mem::replace(&mut s.ty, Ty::new(T::None).or_nil(Nil::Absent));
        s.ty = t.filter_by_flags(flags, ctx)?;
        Ok(())
    }

    // following methods are direct analogues to value type's ones, whenever applicable

    pub fn flex(&self) -> F { self.0.borrow().flex() }
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
    pub fn tag(&self) -> Option<Tag> { self.unlift().tag() }

    pub fn with_nil(&self) -> Slot {
        let s = self.0.borrow();
        Slot::from(s.clone().with_nil())
    }

    pub fn without_nil(&self) -> Slot {
        let s = self.0.borrow();
        Slot::from(s.clone().without_nil())
    }

    pub fn weaken(&self, ctx: &mut TypeContext) -> CheckResult<Slot> {
        let s = self.0.borrow();
        Ok(Slot::from(s.weaken(ctx)?))
    }
}

impl Union for Slot {
    type Output = Slot;

    fn union(&self, other: &Slot, explicit: bool, ctx: &mut TypeContext) -> CheckResult<Slot> {
        // if self and other point to the same slot, do not try to borrow mutably
        if self.0.deref() as *const _ == other.0.deref() as *const _ { return Ok(self.clone()); }

        // now it is safe to borrow mutably
        Ok(Slot::from(self.0.borrow_mut().union(&mut other.0.borrow_mut(), explicit, ctx)?))
    }
}

impl Lattice for Slot {
    fn assert_sub(&self, other: &Slot, ctx: &mut TypeContext) -> CheckResult<()> {
        self.0.borrow().assert_sub(&other.0.borrow(), ctx)
    }

    fn assert_eq(&self, other: &Slot, ctx: &mut TypeContext) -> CheckResult<()> {
        self.0.borrow().assert_eq(&other.0.borrow(), ctx)
    }
}

// when the RHS is a normal type the LHS is automatically unlifted; useful for rvalue-only ops.
// note that this makes a big difference from lifting the RHS.
// also, for the convenience, this does NOT print the LHS with the flexibility
// (which doesn't matter here).
impl<'a> Lattice<T<'a>> for Spanned<Slot> {
    fn assert_sub(&self, other: &T<'a>, ctx: &mut TypeContext) -> CheckResult<()> {
        let unlifted = self.unlift();
        if let Err(e) = unlifted.assert_sub(other, ctx) {
            ctx.error(self.span, m::NotSubtype { sub: unlifted.display(ctx),
                                                 sup: other.display(ctx) })
               .done()?;
            Err(e) // XXX not sure if we can recover here
        } else {
            Ok(())
        }
    }

    fn assert_eq(&self, other: &T<'a>, ctx: &mut TypeContext) -> CheckResult<()> {
        let unlifted = self.unlift();
        if let Err(e) = unlifted.assert_eq(other, ctx) {
            ctx.error(self.span, m::NotEqual { lhs: unlifted.display(ctx),
                                               rhs: other.display(ctx) })
               .done()?;
            Err(e) // XXX not sure if we can recover here
        } else {
            Ok(())
        }
    }
}

impl<'a> Lattice<Ty> for Spanned<Slot> {
    fn assert_sub(&self, other: &Ty, ctx: &mut TypeContext) -> CheckResult<()> {
        let unlifted = self.unlift();
        if let Err(e) = unlifted.assert_sub(other, ctx) {
            ctx.error(self.span, m::NotSubtype { sub: unlifted.display(ctx),
                                                 sup: other.display(ctx) })
               .done()?;
            Err(e) // XXX not sure if we can recover here
        } else {
            Ok(())
        }
    }

    fn assert_eq(&self, other: &Ty, ctx: &mut TypeContext) -> CheckResult<()> {
        let unlifted = self.unlift();
        if let Err(e) = unlifted.assert_eq(other, ctx) {
            ctx.error(self.span, m::NotEqual { lhs: unlifted.display(ctx),
                                               rhs: other.display(ctx) })
               .done()?;
            Err(e) // XXX not sure if we can recover here
        } else {
            Ok(())
        }
    }
}

impl Display for Slot {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, ctx: &TypeContext) -> fmt::Result {
        let s = &*self.0.borrow();

        // for the purpose of display, Currently is most powerful *and* most constrained,
        // Var is second, and Const is least powerful.
        let prefix = match s.flex.resolve(ctx) {
            F::Any                => return write!(f, "<inaccessible type>"),
            F::Dynamic(Dyn::User) => return write!(f, "WHATEVER"),
            F::Dynamic(Dyn::Oops) => return write!(f, "<error type>"),
            F::Just               => "", // can be seen as a mutable value yet to be assigned
            F::Const              => "const ",
            F::Var                => "",
            F::Currently          => "<currently> ",
            F::VarOrConst(_)      => "",
            F::VarOrCurrently(_)  => "<currently> ",
        };

        write!(f, "{}{}", prefix, s.ty.display(ctx))
    }
}

impl fmt::Debug for Slot {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.sign_minus() {
            fmt::Debug::fmt(&self.0.borrow().ty, f)
        } else {
            fmt::Debug::fmt(&self.0.borrow(), f)
        }
    }
}

#[cfg(test)]
#[allow(unused_variables, dead_code)]
mod tests {
    use kailua_diag::NoReport;
    use std::rc::Rc;
    use ty::{T, Ty, Lattice, TypeContext, NoTypeContext};
    use super::*;

    #[test]
    fn test_sub() {
        use env::Context;

        let mut ctx = Context::new(Rc::new(NoReport));

        let just = |t| Slot::new(F::Just, Ty::new(t));
        let cnst = |t| Slot::new(F::Const, Ty::new(t));
        let var = |t| Slot::new(F::Var, Ty::new(t));
        let curr = |t| Slot::new(F::Currently, Ty::new(t));
        let varcnst =
            |ctx: &mut TypeContext, t| Slot::new(F::VarOrConst(ctx.gen_mark()), Ty::new(t));
        let varcurr =
            |ctx: &mut TypeContext, t| Slot::new(F::VarOrCurrently(ctx.gen_mark()), Ty::new(t));

        assert_eq!(just(T::Integer).assert_sub(&just(T::Integer), &mut NoTypeContext), Ok(()));
        assert_eq!(just(T::Integer).assert_sub(&just(T::Number), &mut NoTypeContext), Ok(()));
        assert!(just(T::Number).assert_sub(&just(T::Integer), &mut NoTypeContext).is_err());

        assert_eq!(var(T::Integer).assert_sub(&var(T::Integer), &mut NoTypeContext), Ok(()));
        assert!(var(T::Integer).assert_sub(&var(T::Number), &mut NoTypeContext).is_err());
        assert!(var(T::Number).assert_sub(&var(T::Integer), &mut NoTypeContext).is_err());

        assert_eq!(cnst(T::Integer).assert_sub(&cnst(T::Integer), &mut NoTypeContext), Ok(()));
        assert_eq!(cnst(T::Integer).assert_sub(&cnst(T::Number), &mut NoTypeContext), Ok(()));
        assert!(cnst(T::Number).assert_sub(&cnst(T::Integer), &mut NoTypeContext).is_err());

        assert_eq!(curr(T::Integer).assert_sub(&curr(T::Integer), &mut NoTypeContext), Ok(()));
        assert!(curr(T::Integer).assert_sub(&curr(T::Number), &mut NoTypeContext).is_err());
        assert!(curr(T::Number).assert_sub(&curr(T::Integer), &mut NoTypeContext).is_err());

        {
            let v1 = ctx.gen_tvar();
            let s1 = varcurr(&mut ctx, T::Integer);
            let s2 = varcurr(&mut ctx, T::TVar(v1));
            // marks are linked and v1 = integer
            assert_eq!(s1.assert_sub(&s2, &mut ctx), Ok(()));
            // force the mark to be true, i.e. both s1 and s2 are Var
            assert_eq!(s1.assert_sub(&var(T::Integer), &mut ctx), Ok(()));
            assert_eq!(s2.assert_sub(&cnst(T::Integer), &mut ctx), Ok(()));
            assert!(cnst(T::Integer).assert_sub(&s2, &mut ctx).is_err());
        }
    }
}

