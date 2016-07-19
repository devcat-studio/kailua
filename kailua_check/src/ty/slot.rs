use std::fmt;
use std::ops::Deref;
use std::rc::Rc;
use std::cell::{Ref, RefCell};

use kailua_diag::{Spanned, Reporter};
use diag::CheckResult;
use super::{T, TypeContext, Lattice, Display, Mark, TVar, Builtin};
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
    Dynamic,
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

            F::Dynamic => Ok(F::Dynamic),

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
                try!(m.assert_true(ctx));
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
            (F::Dynamic, F::Dynamic) => true,
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
            F::Any               => write!(f, "any"),
            F::Dynamic           => write!(f, "?"),
            F::Just              => write!(f, "just"),
            F::Const             => write!(f, "const"),
            F::Var               => write!(f, "var"),
            F::Currently         => write!(f, "currently"),
            F::VarOrConst(m)     => write!(f, "{:?}?var:const", m),
            F::VarOrCurrently(m) => write!(f, "{:?}?var:currently", m),
        }
    }
}

// slot types
#[derive(Clone, PartialEq)]
pub struct S<'a> {
    flex: F,
    ty: T<'a>,
}

impl<'a> S<'a> {
    pub fn flex(&self) -> F {
        self.flex
    }

    pub fn unlift<'b>(&'b self) -> &'b T<'a> {
        &self.ty
    }

    pub fn weaken<'b: 'a>(&'b self, ctx: &mut TypeContext) -> CheckResult<S<'b>> {
        Ok(S { flex: try!(self.flex.weaken(ctx)), ty: self.ty.to_ref() })
    }

    // used for value slots in array and mapping types
    pub fn without_nil(self) -> S<'a> {
        S { flex: self.flex, ty: self.ty.without_nil() }
    }

    pub fn into_send(self) -> S<'static> {
        S { flex: self.flex, ty: self.ty.into_send() }
    }

    // self and other may be possibly different slots and being merged by union
    // (thus Currently cannot be merged, even if the type is identical)
    // mainly used by `and`/`or` operators and table lifting
    pub fn union<'b>(&mut self, other: &mut S<'b>, ctx: &mut TypeContext) -> S<'static> {
        // Currently is first *changed* to Var, since it will be going to be shared anyway
        if self.flex.is_linear() { self.flex = F::Var; }
        if other.flex.is_linear() { other.flex = F::Var; }

        let (flex, ty) = match (self.flex, other.flex) {
            (F::Dynamic, _) | (_, F::Dynamic) => (F::Dynamic, T::Dynamic),
            (F::Any, _) | (_, F::Any) => (F::Any, T::None),

            // it's fine to merge r-values
            (F::Just, F::Just) => (F::Just, self.ty.union(&other.ty, ctx)),

            // merging Var will result in Const unless a and b are identical
            (F::Var, F::Var) |
            (F::Var, F::Just) |
            (F::Just, F::Var) => {
                let m = ctx.gen_mark();
                assert_eq!(ctx.assert_mark_require_eq(m, &self.ty, &other.ty),
                           Ok(())); // can't fail
                (F::VarOrConst(m), self.ty.union(&other.ty, ctx))
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
                (F::Const, self.ty.union(&other.ty, ctx)),

            // other cases are impossible
            (F::Currently, _) | (_, F::Currently) => unreachable!(),
            (F::VarOrCurrently(_), _) | (_, F::VarOrCurrently(_)) => unreachable!(),
        };

        S { flex: flex, ty: ty }
    }

    pub fn assert_sub<'b>(&self, other: &S<'b>, ctx: &mut TypeContext) -> CheckResult<()> {
        debug!("asserting a constraint {:?} <: {:?}", *self, *other);

        macro_rules! m {
            ($(; true, $tm:expr)*
             $(; false, $fm:expr)*
             $(; eq, $em1:expr, $em2:expr)*
             $(; imply, $im1:expr, $im2:expr)*
             $(; require_eq, $rm:expr)*) => ({
                $(try!($tm.assert_true(ctx));)*
                $(try!($fm.assert_false(ctx));)*
                $(try!($em1.assert_eq($em2, ctx));)*
                $(try!($im1.assert_imply($im2, ctx));)*
                $(try!($rm.assert_require_eq(&other.ty, &self.ty, ctx));)*
            });

            (a <: b $($t:tt)*) => ({ try!(self.ty.assert_sub(&other.ty, ctx)); m!($($t)*) });
            (a = b $($t:tt)*) => ({ try!(self.ty.assert_eq(&other.ty, ctx)); m!($($t)*) });
        }

        match (self.flex, other.flex) {
            (_, F::Any) => {}
            (_, F::Dynamic) | (F::Dynamic, _) => {}

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

    pub fn assert_eq<'b>(&self, other: &S<'b>, ctx: &mut TypeContext) -> CheckResult<()> {
        debug!("asserting a constraint {:?} = {:?}", *self, *other);

        macro_rules! m {
            ($(; true, $tm:expr)*
             $(; false, $fm:expr)*
             $(; eq, $em1:expr, $em2:expr)*) => ({
                $(try!($tm.assert_true(ctx));)*
                $(try!($fm.assert_false(ctx));)*
                $(try!($em1.assert_eq($em2, ctx));)*
            });

            (a = b $($t:tt)*) => ({ try!(self.ty.assert_eq(&other.ty, ctx)); m!($($t)*) });
        }

        match (self.flex, other.flex) {
            (F::Any, F::Any) => {}
            (_, F::Dynamic) | (F::Dynamic, _) => {}

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

impl<'a> fmt::Debug for S<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {:?}", self.flex, self.ty)
    }
}

#[derive(Clone, PartialEq)]
pub struct Slot(Rc<RefCell<S<'static>>>);

impl Slot {
    pub fn new<'a>(flex: F, ty: T<'a>) -> Slot {
        Slot(Rc::new(RefCell::new(S { flex: flex, ty: ty.into_send() })))
    }

    pub fn from<'a>(s: S<'a>) -> Slot {
        Slot(Rc::new(RefCell::new(s.into_send())))
    }

    pub fn just<'a>(t: T<'a>) -> Slot {
        Slot::new(F::Just, t)
    }

    pub fn var<'a>(t: T<'a>, ctx: &mut TypeContext) -> Slot {
        Slot::new(F::VarOrCurrently(ctx.gen_mark()), t)
    }

    pub fn dummy() -> Slot {
        Slot::new(F::Dynamic, T::Dynamic)
    }

    pub fn unlift<'a>(&'a self) -> Ref<'a, T<'static>> {
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
            (_, F::Dynamic, _) | (F::Dynamic, _, _) => {}

            (_, F::Any, _) |
            (F::Any, _, _) |
            (F::Just, _, _) |
            (F::Const, _, false) => {
                return Err(format!("impossible to assign {:?} to {:?}", rhs, lhs));
            }

            // as long as the type is in agreement, Var can be assigned
            (F::Var, _, _) => {
                try!(rhs.ty.assert_sub(&lhs.ty, ctx));
            }

            // assignment to Const slot is for initialization only
            (F::Const, _, true) => {
                try!(rhs.ty.assert_sub(&lhs.ty, ctx));
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
                try!(m.assert_true(ctx));
                try!(rhs.ty.assert_sub(&lhs.ty, ctx));
                lhs.flex = F::Var;
            }

            // ...except for the first time
            (F::VarOrConst(_), _, true) => {
                try!(rhs.ty.assert_sub(&lhs.ty, ctx));
            }

            // assigning to VarOrCurrently adds a new equality requirement to the mark
            (F::VarOrCurrently(_), _, _) => {
                // ditto as above
                if is_shared(&lhs.ty, &rhs) {
                    lhs.flex = F::Var;
                    rhs.flex = F::Var;
                } else {
                    let m = ctx.gen_mark(); // the prior mark had an incompatible base type
                    try!(m.assert_require_sup(&lhs.ty, &rhs.ty, ctx));
                    lhs.flex = F::VarOrCurrently(m);
                }
                lhs.ty = rhs.ty.clone();
            }
        };

        Ok(())
    }

    // following methods are direct analogues to value type's ones, whenever applicable

    pub fn flex(&self) -> F { self.0.borrow().flex() }
    pub fn flags(&self) -> Flags { self.unlift().flags() }

    pub fn is_dynamic(&self)  -> bool { self.flags().is_dynamic() }
    pub fn is_integral(&self) -> bool { self.flags().is_integral() }
    pub fn is_numeric(&self)  -> bool { self.flags().is_numeric() }
    pub fn is_stringy(&self)  -> bool { self.flags().is_stringy() }
    pub fn is_tabular(&self)  -> bool { self.flags().is_tabular() }
    pub fn is_callable(&self) -> bool { self.flags().is_callable() }
    pub fn is_truthy(&self)   -> bool { self.flags().is_truthy() }
    pub fn is_falsy(&self)    -> bool { self.flags().is_falsy() }

    pub fn get_tvar(&self) -> Option<TVar> { self.unlift().get_tvar() }
    pub fn builtin(&self) -> Option<Builtin> { self.unlift().builtin() }

    pub fn without_nil(&self) -> Slot {
        let s = self.0.borrow();
        Slot::from(s.clone().into_send().without_nil())
    }

    pub fn weaken(&self, ctx: &mut TypeContext) -> CheckResult<Slot> {
        let s = self.0.borrow();
        Ok(Slot::from(try!(s.weaken(ctx))))
    }
}

impl Lattice for Slot {
    type Output = Slot;

    fn union(&self, other: &Slot, ctx: &mut TypeContext) -> Slot {
        // if self and other point to the same slot, do not try to borrow mutably
        if self.0.deref() as *const _ == other.0.deref() as *const _ { return self.clone(); }

        // now it is safe to borrow mutably
        Slot::from(self.0.borrow_mut().union(&mut other.0.borrow_mut(), ctx))
    }

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
    type Output = Slot;

    fn union(&self, _other: &T<'a>, _ctx: &mut TypeContext) -> Slot {
        panic!("Lattice::union(Spanned<Slot>, T) is not supported")
    }

    fn assert_sub(&self, other: &T<'a>, ctx: &mut TypeContext) -> CheckResult<()> {
        let unlifted = self.unlift();
        if let Err(e) = unlifted.assert_sub(other, ctx) {
            try!(ctx.error(self.span, m::NotSubtype { sub: unlifted.display(ctx),
                                                      sup: other.display(ctx) })
                    .done());
            Err(e) // XXX not sure if we can recover here
        } else {
            Ok(())
        }
    }

    fn assert_eq(&self, other: &T<'a>, ctx: &mut TypeContext) -> CheckResult<()> {
        let unlifted = self.unlift();
        if let Err(e) = unlifted.assert_eq(other, ctx) {
            try!(ctx.error(self.span, m::NotEqual { lhs: unlifted.display(ctx),
                                                    rhs: other.display(ctx) })
                    .done());
            Err(e) // XXX not sure if we can recover here
        } else {
            Ok(())
        }
    }
}

impl Display for Slot {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, ctx: &TypeContext) -> fmt::Result {
        let s = &*self.0.borrow();
        match s.flex.resolve(ctx) {
            F::Any     => write!(f, "<inaccessible type>"),
            F::Dynamic => write!(f, "WHATEVER"),
            F::Const   => write!(f, "const {}", s.ty.display(ctx)),
            F::Var     => write!(f, "var {}", s.ty.display(ctx)),

            // other unresolved flexibilities are not directly visible to the user
            _ => write!(f, "{}", s.ty.display(ctx)),
        }
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
    use ty::{T, Lattice, TypeContext, NoTypeContext};
    use super::*;

    #[test]
    fn test_sub() {
        use env::Context;

        let mut ctx = Context::new(Rc::new(NoReport));

        let just = |t| Slot::new(F::Just, t);
        let cnst = |t| Slot::new(F::Const, t);
        let var = |t| Slot::new(F::Var, t);
        let curr = |t| Slot::new(F::Currently, t);
        let varcnst = |ctx: &mut TypeContext, t| Slot::new(F::VarOrConst(ctx.gen_mark()), t);
        let varcurr = |ctx: &mut TypeContext, t| Slot::new(F::VarOrCurrently(ctx.gen_mark()), t);

        assert_eq!(just(T::integer()).assert_sub(&just(T::integer()), &mut NoTypeContext), Ok(()));
        assert_eq!(just(T::integer()).assert_sub(&just(T::number()), &mut NoTypeContext), Ok(()));
        assert!(just(T::number()).assert_sub(&just(T::integer()), &mut NoTypeContext).is_err());

        assert_eq!(var(T::integer()).assert_sub(&var(T::integer()), &mut NoTypeContext), Ok(()));
        assert!(var(T::integer()).assert_sub(&var(T::number()), &mut NoTypeContext).is_err());
        assert!(var(T::number()).assert_sub(&var(T::integer()), &mut NoTypeContext).is_err());

        assert_eq!(cnst(T::integer()).assert_sub(&cnst(T::integer()), &mut NoTypeContext), Ok(()));
        assert_eq!(cnst(T::integer()).assert_sub(&cnst(T::number()), &mut NoTypeContext), Ok(()));
        assert!(cnst(T::number()).assert_sub(&cnst(T::integer()), &mut NoTypeContext).is_err());

        assert_eq!(curr(T::integer()).assert_sub(&curr(T::integer()), &mut NoTypeContext), Ok(()));
        assert!(curr(T::integer()).assert_sub(&curr(T::number()), &mut NoTypeContext).is_err());
        assert!(curr(T::number()).assert_sub(&curr(T::integer()), &mut NoTypeContext).is_err());

        {
            let v1 = ctx.gen_tvar();
            let s1 = varcurr(&mut ctx, T::integer());
            let s2 = varcurr(&mut ctx, T::TVar(v1));
            // marks are linked and v1 = integer
            assert_eq!(s1.assert_sub(&s2, &mut ctx), Ok(()));
            // force the mark to be true, i.e. both s1 and s2 are Var
            assert_eq!(s1.assert_sub(&var(T::integer()), &mut ctx), Ok(()));
            assert_eq!(s2.assert_sub(&cnst(T::integer()), &mut ctx), Ok(()));
            assert!(cnst(T::integer()).assert_sub(&s2, &mut ctx).is_err());
        }
    }
}

