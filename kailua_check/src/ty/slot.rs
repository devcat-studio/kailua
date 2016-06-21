use std::mem;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;
use std::cell::{Ref, RefMut, RefCell};

use diag::CheckResult;
use super::{T, TypeContext, Lattice, Mark};
use super::{error_not_sub, error_not_eq};

// slot types
#[derive(Clone)]
pub enum S<'a> {
    // invalid top type (no type information available)
    Any,
    // temporary r-value slot
    // coerces to VarOrCurrently when used in the table fields
    Just(T<'a>),
    // covariant immutable slot
    Const(T<'a>),
    // invariant mutable slot
    Var(T<'a>),
    // uniquely mutable slot
    // - assigning Currently to Currently updates and converts the slot type to Var
    // - assigning other types to Currently updates the slot type
    Currently(T<'a>),
    // either Var (when mark is true) or Const (when mark is false)
    VarOrConst(T<'a>, Mark),
    // either Var (when mark is true) or Currently (when mark is false)
    // - due to the implementation issues, weakening results in forcing Var
    VarOrCurrently(T<'a>, Mark),
}

impl<'a> S<'a> {
    // is possibly linear, and thus should be uniquely owned?
    // (there are some cases that this is impossible, e.g. array types)
    pub fn is_linear(&self) -> bool {
        match *self {
            S::Currently(..) | S::VarOrCurrently(..) => true,
            _ => false,
        }
    }

    // not designed to work with Any.
    pub fn unlift<'b>(&'b self) -> &'b T<'a> {
        match *self {
            S::Any => panic!("S::unlift called with S::Any"),
            S::Just(ref t) | S::Const(ref t) | S::Var(ref t) | S::Currently(ref t) |
                S::VarOrConst(ref t, _) | S::VarOrCurrently(ref t, _) => t,
        }
    }

    pub fn weaken<'b: 'a>(&'b self, ctx: &mut TypeContext) -> CheckResult<S<'b>> {
        match self {
            &S::Any                  => Ok(S::Any),
            // TODO recursive weaken for Just w/ Tables
            &S::Just(ref t)          => Ok(S::VarOrCurrently(t.to_ref(), ctx.gen_mark())),
            &S::Const(ref t)         => Ok(S::Const(t.to_ref())),
            &S::Var(ref t)           => Ok(S::VarOrConst(t.to_ref(), ctx.gen_mark())),
            &S::Currently(_)         => Ok(S::Any),
            &S::VarOrConst(ref t, m) => Ok(S::VarOrConst(t.to_ref(), m)),
            &S::VarOrCurrently(ref t, m) => {
                // we originally had S::PossiblyVarOrConst for this case.
                // but it, having two different marks, was particularly hard to
                // fit with the deterministic typing framework.
                // since weaken is only called when the upvalue is required,
                // it is safe to assume that m should be true.
                try!(m.assert_true(ctx));
                Ok(S::VarOrConst(t.to_ref(), ctx.gen_mark()))
            },
        }
    }

    // used for value slots in array and mapping types
    pub fn without_nil(self) -> S<'a> {
        match self {
            S::Any                  => S::Any,
            S::Just(t)              => S::Just(t.without_nil()),
            S::Const(t)             => S::Const(t.without_nil()),
            S::Var(t)               => S::Var(t.without_nil()),
            S::Currently(t)         => S::Currently(t.without_nil()),
            S::VarOrConst(t, m)     => S::VarOrConst(t.without_nil(), m),
            S::VarOrCurrently(t, m) => S::VarOrCurrently(t.without_nil(), m),
        }
    }

    pub fn into_send(self) -> S<'static> {
        match self {
            S::Any                  => S::Any,
            S::Just(t)              => S::Just(t.into_send()),
            S::Const(t)             => S::Const(t.into_send()),
            S::Var(t)               => S::Var(t.into_send()),
            S::Currently(t)         => S::Currently(t.into_send()),
            S::VarOrConst(t, m)     => S::VarOrConst(t.into_send(), m),
            S::VarOrCurrently(t, m) => S::VarOrCurrently(t.into_send(), m),
        }
    }

    // self and other may be possibly different slots and being merged by union
    // (thus Currently cannot be merged, even if the type is identical)
    // mainly used by `and`/`or` operators and table lifting
    pub fn union<'b>(&mut self, other: &mut S<'b>, ctx: &mut TypeContext) -> S<'static> {
        // Currently is first *changed* to Var, since it will be going to be shared anyway
        *self = match mem::replace(self, S::Any) {
            S::Currently(t) | S::VarOrCurrently(t, _) => S::Var(t),
            s => s,
        };
        *other = match mem::replace(other, S::Any) {
            S::Currently(t) | S::VarOrCurrently(t, _) => S::Var(t),
            s => s,
        };

        match (self, other) {
            (&mut S::Any, _) | (_, &mut S::Any) => S::Any,

            // it's fine to merge r-values
            (&mut S::Just(ref a), &mut S::Just(ref b)) => S::Just(a.union(b, ctx)),

            // merging Var will result in Const unless a and b are identical
            (&mut S::Var(ref a), &mut S::Var(ref b)) |
            (&mut S::Var(ref a), &mut S::Just(ref b)) |
            (&mut S::Just(ref a), &mut S::Var(ref b)) => {
                let m = ctx.gen_mark();
                assert_eq!(ctx.assert_mark_require_eq(m, a, b), Ok(())); // can't fail
                S::VarOrConst(a.union(b, ctx), m)
            }

            // Var and VarConst are lifted to Const otherwise, regardless of marks
            (&mut S::Const(ref a), &mut S::Just(ref b)) |
            (&mut S::Const(ref a), &mut S::Const(ref b)) |
            (&mut S::Const(ref a), &mut S::Var(ref b)) |
            (&mut S::Const(ref a), &mut S::VarOrConst(ref b, _)) |
            (&mut S::Just(ref a), &mut S::Const(ref b)) |
            (&mut S::Just(ref a), &mut S::VarOrConst(ref b, _)) |
            (&mut S::Var(ref a), &mut S::Const(ref b)) |
            (&mut S::Var(ref a), &mut S::VarOrConst(ref b, _)) |
            (&mut S::VarOrConst(ref a, _), &mut S::Just(ref b)) |
            (&mut S::VarOrConst(ref a, _), &mut S::Const(ref b)) |
            (&mut S::VarOrConst(ref a, _), &mut S::Var(ref b)) |
            (&mut S::VarOrConst(ref a, _), &mut S::VarOrConst(ref b, _)) =>
                S::Const(a.union(b, ctx)),

            // other cases are impossible
            (&mut S::Currently(..), _) | (_, &mut S::Currently(..)) => unreachable!(),
            (&mut S::VarOrCurrently(..), _) | (_, &mut S::VarOrCurrently(..)) => unreachable!(),
        }
    }

    pub fn assert_sub<'b>(&self, other: &S<'b>, ctx: &mut TypeContext) -> CheckResult<()> {
        debug!("asserting a constraint {:?} <: {:?}", *self, *other);

        macro_rules! m {
            ($(; true, $tm:expr)*
             $(; false, $fm:expr)*
             $(; eq, $em1:expr, $em2:expr)*
             $(; imply, $im1:expr, $im2:expr)*
             $(; require_eq, $rm:expr, $rt1:expr, $rt2:expr)*) => ({
                $(try!($tm.assert_true(ctx));)*
                $(try!($fm.assert_false(ctx));)*
                $(try!($em1.assert_eq($em2, ctx));)*
                $(try!($im1.assert_imply($im2, ctx));)*
                $(try!($rm.assert_require_eq($rt1, $rt2, ctx));)*
            });

            ($a:ident <: $b:ident $($t:tt)*) => ({ try!($a.assert_sub($b, ctx)); m!($($t)*) });
            ($a:ident = $b:ident $($t:tt)*) => ({ try!($a.assert_eq($b, ctx)); m!($($t)*) });
        }

        match (self, other) {
            (_, &S::Any) => {}

            (&S::Just(ref a), &S::Just(ref b)) => m!(a <: b),

            (&S::Just(ref a), &S::Const(ref b)) => m!(a <: b),
            (&S::Const(ref a), &S::Const(ref b)) => m!(a <: b),
            (&S::Var(ref a), &S::Const(ref b)) => m!(a <: b),
            (&S::VarOrConst(ref a, _), &S::Const(ref b)) => m!(a <: b),
            (&S::VarOrCurrently(ref a, am), &S::Const(ref b)) => m!(a <: b; true, am),

            (&S::Just(ref a), &S::Var(ref b)) => m!(a <: b),
            (&S::Var(ref a), &S::Var(ref b)) => m!(a = b),
            (&S::VarOrConst(ref a, am), &S::Var(ref b)) => m!(a = b; true, am),
            (&S::VarOrCurrently(ref a, am), &S::Var(ref b)) => m!(a = b; true, am),

            (&S::Just(ref a), &S::Currently(ref b)) => m!(a <: b),
            (&S::Currently(ref a), &S::Currently(ref b)) => m!(a = b),
            (&S::VarOrCurrently(ref a, am), &S::Currently(ref b)) => m!(a = b; true, am),

            (&S::Just(ref a), &S::VarOrConst(ref b, _)) => m!(a <: b),
            (&S::Const(ref a), &S::VarOrConst(ref b, bm)) => m!(a <: b; false, bm),
            (&S::Var(ref a), &S::VarOrConst(ref b, bm)) => m!(a <: b; false, bm),
            (&S::VarOrConst(ref a, am), &S::VarOrConst(ref b, bm)) =>
                m!(a <: b; imply, bm, am; require_eq, bm, b, a),
            (&S::VarOrCurrently(ref a, am), &S::VarOrConst(ref b, bm)) =>
                m!(a <: b; true, am; require_eq, bm, b, a),

            (&S::Just(ref a), &S::VarOrCurrently(ref b, _)) => m!(a <: b),
            (&S::Var(ref a), &S::VarOrCurrently(ref b, bm)) => m!(a = b; true, bm),
            (&S::Currently(ref a), &S::VarOrCurrently(ref b, bm)) => m!(a = b; false, bm),
            (&S::VarOrConst(ref a, am), &S::VarOrCurrently(ref b, bm)) =>
                m!(a = b; true, am; true, bm),
            (&S::VarOrCurrently(ref a, am), &S::VarOrCurrently(ref b, bm)) =>
                m!(a = b; eq, am, bm),

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

            ($a:ident = $b:ident $($t:tt)*) => ({ try!($a.assert_eq($b, ctx)); m!($($t)*) });
        }

        match (self, other) {
            (&S::Any, &S::Any) => {}

            (&S::Just(ref a), &S::Just(ref b)) => m!(a = b),

            (&S::Currently(ref a), &S::Currently(ref b)) => m!(a = b),
            (&S::Currently(ref a), &S::VarOrCurrently(ref b, bm)) => m!(a = b; false, bm),

            (&S::Const(ref a), &S::Const(ref b)) => m!(a = b),
            (&S::Const(ref a), &S::VarOrConst(ref b, bm)) => m!(a = b; false, bm),

            (&S::Var(ref a), &S::Var(ref b)) => m!(a = b),
            (&S::Var(ref a), &S::VarOrConst(ref b, bm)) => m!(a = b; true, bm),
            (&S::Var(ref a), &S::VarOrCurrently(ref b, bm)) => m!(a = b; true, bm),

            (&S::VarOrConst(ref a, am), &S::Const(ref b)) => m!(a = b; false, am),
            (&S::VarOrConst(ref a, am), &S::Var(ref b)) => m!(a = b; true, am),
            (&S::VarOrConst(ref a, am), &S::VarOrConst(ref b, bm)) =>
                m!(a = b; eq, am, bm),
            (&S::VarOrConst(ref a, am), &S::VarOrCurrently(ref b, bm)) =>
                m!(a = b; true, am; true, bm),

            (&S::VarOrCurrently(ref a, am), &S::Var(ref b)) => m!(a = b; true, am),
            (&S::VarOrCurrently(ref a, am), &S::Currently(ref b)) => m!(a = b; false, am),
            (&S::VarOrCurrently(ref a, am), &S::VarOrConst(ref b, bm)) =>
                m!(a = b; true, am; true, bm),
            (&S::VarOrCurrently(ref a, am), &S::VarOrCurrently(ref b, bm)) =>
                m!(a = b; eq, am, bm),

            (_, _) => return error_not_eq(self, other),
        }

        Ok(())
    }
}

impl<'a, 'b> PartialEq<S<'b>> for S<'a> {
    fn eq(&self, other: &S<'b>) -> bool {
        match (self, other) {
            (&S::Any, &S::Any) => true,
            (&S::Just(ref a), &S::Just(ref b)) => *a == *b,
            (&S::Const(ref a), &S::Const(ref b)) => *a == *b,
            (&S::Var(ref a), &S::Var(ref b)) => *a == *b,
            (&S::Currently(ref a), &S::Currently(ref b)) => *a == *b,

            // Mark::any() is used for debugging comparison
            (&S::VarOrConst(ref a, am), &S::VarOrConst(ref b, bm)) =>
                (am == Mark::any() || bm == Mark::any() || am == bm) && *a == *b,
            (&S::VarOrCurrently(ref a, am), &S::VarOrCurrently(ref b, bm)) =>
                (am == Mark::any() || bm == Mark::any() || am == bm) && *a == *b,

            (_, _) => false,
        }
    }
}

impl<'a> fmt::Debug for S<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            S::Any                      => write!(f, "<any>"),
            S::Just(ref t)              => write!(f, "just {:?}", *t),
            S::Const(ref t)             => write!(f, "const {:?}", *t),
            S::Var(ref t)               => write!(f, "var {:?}", *t),
            S::Currently(ref t)         => write!(f, "currently {:?}", *t),
            S::VarOrConst(ref t, m)     => write!(f, "{:?}?var:const {:?}", m, *t),
            S::VarOrCurrently(ref t, m) => write!(f, "{:?}?var:currently {:?}", m, *t),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Slot(Rc<RefCell<S<'static>>>);

impl Slot {
    pub fn new<'a>(s: S<'a>) -> Slot {
        Slot(Rc::new(RefCell::new(s.into_send())))
    }

    pub fn just<'a>(t: T<'a>) -> Slot {
        Slot::new(S::Just(t.into_send()))
    }

    pub fn borrow<'a>(&'a self) -> Ref<'a, S<'static>> { self.0.borrow() }
    pub fn borrow_mut<'a>(&'a mut self) -> RefMut<'a, S<'static>> { self.0.borrow_mut() }

    pub fn without_nil(&self) -> Slot {
        let s = self.0.borrow();
        Slot::new(s.clone().into_send().without_nil())
    }

    pub fn weaken(&self, ctx: &mut TypeContext) -> CheckResult<Slot> {
        let s = self.0.borrow();
        Ok(Slot::new(try!(s.weaken(ctx))))
    }

    pub fn accept(&self, rhs: &Slot, ctx: &mut TypeContext) -> CheckResult<()> {
        // accepting itself is always fine and has no effect,
        // but it has to be filtered since it will borrow twice otherwise
        if self.0.deref() as *const _ == rhs.0.deref() as *const _ { return Ok(()); }

        let mut lhandle = self.0.borrow_mut();
        let mut rhandle = rhs.0.borrow_mut();

        // TODO reduce cloning
        let lty = lhandle.clone();
        let rty = rhandle.clone();

        let is_shared = |s: &T, t: &S|
            s.is_referential() &&
            match *t {
                S::Currently(ref t) | S::VarOrCurrently(ref t, _) => t.is_referential(),
                _ => false,
            };

        let (lty, rty) = match (lty, rty) {
            (lty, rty @ S::Any) |
            (lty @ S::Any, rty) |
            (lty @ S::Const(_), rty) |
            (lty @ S::Just(_), rty) => {
                return Err(format!("impossible to assign {:?} to {:?}", rty, lty));
            }

            // as long as the type is in agreement, Var can be assigned
            (S::Var(s), t) => {
                try!(t.unlift().assert_sub(&s, ctx));
                (S::Var(s), t)
            }

            // non-Currently value can be assigned to Currently while changing its type
            (S::Currently(s), t) => {
                // if both s and t are Currently and tabular,
                // the linearity is broken and both s and t have to be changed as well
                if is_shared(&s, &t) {
                    let t = t.unlift();
                    (S::Var(t.clone()), S::Var(t.clone()))
                } else {
                    (S::Currently(t.unlift().clone()), t)
                }
            }

            // assigning to VarOrConst asserts the mark and makes it Var
            (S::VarOrConst(s, m), t) => {
                try!(m.assert_true(ctx));
                try!(t.unlift().assert_sub(&s, ctx));
                (S::Var(s), t)
            }

            // assigning to VarOrCurrently adds a new equality requirement to the mark
            (S::VarOrCurrently(s, _), t) => {
                // ditto as above
                if is_shared(&s, &t) {
                    let t = t.unlift();
                    (S::Var(t.clone()), S::Var(t.clone()))
                } else {
                    let t_ = t.unlift().clone();
                    let m = ctx.gen_mark(); // the prior mark had an incompatible base type
                    try!(m.assert_require_sup(&s, &t_, ctx));
                    (S::VarOrCurrently(t_, m), t)
                }
            }
        };

        *lhandle = lty;
        *rhandle = rty;
        Ok(())
    }

    pub fn is_linear(&self) -> bool { self.0.borrow().is_linear() }
}

impl Lattice for Slot {
    type Output = Slot;

    fn do_union(&self, other: &Slot, ctx: &mut TypeContext) -> Slot {
        // if self and other point to the same slot, do not try to borrow mutably
        if self.0.deref() as *const _ == other.0.deref() as *const _ { return self.clone(); }

        // now it is safe to borrow mutably
        Slot::new(self.0.borrow_mut().union(&mut other.0.borrow_mut(), ctx))
    }

    fn do_assert_sub(&self, other: &Slot, ctx: &mut TypeContext) -> CheckResult<()> {
        self.0.borrow().assert_sub(&other.0.borrow(), ctx)
    }

    fn do_assert_eq(&self, other: &Slot, ctx: &mut TypeContext) -> CheckResult<()> {
        self.0.borrow().assert_eq(&other.0.borrow(), ctx)
    }
}

impl fmt::Debug for Slot {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.0.borrow(), f)
    }
}

#[cfg(test)] 
mod tests {
    use ty::{T, Lattice, TypeContext};
    use super::*;

    #[test]
    fn test_sub() {
        use env::Context;

        let mut ctx = Context::new();

        let just = |t| Slot::new(S::Just(t));
        let cnst = |t| Slot::new(S::Const(t));
        let var = |t| Slot::new(S::Var(t));
        let curr = |t| Slot::new(S::Currently(t));
        let varcnst = |ctx: &mut TypeContext, t| Slot::new(S::VarOrConst(t, ctx.gen_mark()));
        let varcurr = |ctx: &mut TypeContext, t| Slot::new(S::VarOrCurrently(t, ctx.gen_mark()));

        assert_eq!(just(T::integer()).assert_sub(&just(T::integer()), &mut ()), Ok(()));
        assert_eq!(just(T::integer()).assert_sub(&just(T::number()), &mut ()), Ok(()));
        assert!(just(T::number()).assert_sub(&just(T::integer()), &mut ()).is_err());

        assert_eq!(var(T::integer()).assert_sub(&var(T::integer()), &mut ()), Ok(()));
        assert!(var(T::integer()).assert_sub(&var(T::number()), &mut ()).is_err());
        assert!(var(T::number()).assert_sub(&var(T::integer()), &mut ()).is_err());

        assert_eq!(cnst(T::integer()).assert_sub(&cnst(T::integer()), &mut ()), Ok(()));
        assert_eq!(cnst(T::integer()).assert_sub(&cnst(T::number()), &mut ()), Ok(()));
        assert!(cnst(T::number()).assert_sub(&cnst(T::integer()), &mut ()).is_err());

        assert_eq!(curr(T::integer()).assert_sub(&curr(T::integer()), &mut ()), Ok(()));
        assert!(curr(T::integer()).assert_sub(&curr(T::number()), &mut ()).is_err());
        assert!(curr(T::number()).assert_sub(&curr(T::integer()), &mut ()).is_err());

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

