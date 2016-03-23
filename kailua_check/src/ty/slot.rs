use std::mem;
use std::fmt;
use std::cell::RefCell;

use diag::CheckResult;
use super::{T, TypeContext, Lattice, Mark};
use super::{error_not_sub, error_not_eq};

// slot types
#[derive(Clone)]
pub enum S<'a> {
    // invalid top type (no type information available)
    Any,
    // temporary r-value slot
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
    pub fn weaken(self, ctx: &mut TypeContext) -> CheckResult<S<'a>> {
        match self {
            S::Any              => Ok(S::Any),
            S::Just(t)          => Ok(S::VarOrCurrently(t, ctx.gen_mark())),
            S::Const(t)         => Ok(S::Const(t)),
            S::Var(t)           => Ok(S::VarOrConst(t, ctx.gen_mark())),
            S::Currently(_)     => Ok(S::Any),
            S::VarOrConst(t, m) => Ok(S::VarOrConst(t, m)),
            S::VarOrCurrently(t, m) => {
                // we originally had S::PossiblyVarOrConst for this case.
                // but it, having two different marks, was particularly hard to
                // fit with the deterministic typing framework.
                // since weaken is only called when the upvalue is required,
                // it is safe to assume that m should be true.
                try!(m.assert_true(ctx));
                Ok(S::VarOrConst(t, ctx.gen_mark()))
            },
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

    pub fn assert_sub<'b>(&self, other: &S<'b>, ctx: &mut TypeContext) -> CheckResult<()> {
        println!("asserting a constraint {:?} <: {:?}", *self, *other);

        macro_rules! m {
            ($(; true, $tm:expr)*
             $(; false, $fm:expr)*
             $(; eq, $em1:expr, $em2:expr)*
             $(; imply, $im1:expr, $im2:expr)*
             $(; require, $rm:expr, $rt1:expr, $rt2:expr)*) => ({
                $(try!($tm.assert_true(ctx));)*
                $(try!($fm.assert_false(ctx));)*
                $(try!($em1.assert_eq($em2, ctx));)*
                $(try!($im1.assert_imply($im2, ctx));)*
                $(try!($rm.assert_require($rt1, $rt2, ctx));)*
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
                m!(a <: b; imply, bm, am; require, bm, b, a),
            (&S::VarOrCurrently(ref a, am), &S::VarOrConst(ref b, bm)) =>
                m!(a <: b; true, am; require, bm, b, a),

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
        println!("asserting a constraint {:?} = {:?}", *self, *other);

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

pub struct Slot(pub RefCell<S<'static>>);

impl Slot {
    pub fn new<'a>(s: S<'a>) -> Slot {
        Slot(RefCell::new(s.into_send()))
    }

    pub fn weaken(self, ctx: &mut TypeContext) -> CheckResult<Slot> {
        {
            let mut s = self.0.borrow_mut();
            let news = try!(mem::replace(&mut *s, S::Any).weaken(ctx));
            *s = news;
        }
        Ok(self)
    }

    pub fn accept(&self, rhs: &Slot, ctx: &mut TypeContext) -> CheckResult<()> {
        // accepting itself is always fine and has no effect,
        // but it has to be filtered since it will borrow twice otherwise
        if self as *const Slot == rhs as *const Slot { return Ok(()); }

        let mut lhandle = self.0.borrow_mut();
        let mut rhandle = rhs.0.borrow_mut();

        let lty = mem::replace(&mut *lhandle, S::Any);
        let rty = mem::replace(&mut *rhandle, S::Any);

        fn unlift<'a, 'b>(s: &'a S<'b>) -> &'a T<'b> {
            match *s {
                S::Any                      => unreachable!(),
                S::Just(ref t)              => t,
                S::Const(ref t)             => t,
                S::Var(ref t)               => t,
                S::Currently(ref t)         => t,
                S::VarOrConst(ref t, _)     => t,
                S::VarOrCurrently(ref t, _) => t,
            }
        }

        let (lty, rty) = match (lty, rty) {
            (lty, rty @ S::Any) |
            (lty @ S::Any, rty) |
            (lty @ S::Const(_), rty) |
            (lty @ S::Just(_), rty) => {
                return Err(format!("impossible to assign {:?} to {:?}", rty, lty));
            }

            // assigning Currently to Currently breaks the linearity, so both become Var
            (S::Currently(..), S::Currently(t)) |
            (S::Currently(..), S::VarOrCurrently(t,_)) |
            (S::VarOrCurrently(..), S::Currently(t)) |
            (S::VarOrCurrently(..), S::VarOrCurrently(t,_)) => {
                (S::Var(t.clone()), S::Var(t))
            }

            // as long as the type is same, Var can be assigned
            (S::Var(s), t) => {
                try!(s.assert_eq(unlift(&t), ctx));
                (S::Var(s), t)
            }

            // non-Currently value can be assigned to Currently while changing its type
            (S::Currently(_), t) => {
                (S::Currently(unlift(&t).clone()), t)
            }

            // assigning to VarOrConst asserts the mark and makes it Var
            (S::VarOrConst(s, m), t) => {
                try!(m.assert_true(ctx));
                try!(s.assert_eq(unlift(&t), ctx));
                (S::Var(s), t)
            }

            // assigning to VarOrCurrently adds a new equality requirement to the mark
            (S::VarOrCurrently(s, m), t) => {
                let t_ = unlift(&t).clone();
                try!(m.assert_require(&s, &t_, ctx));
                (S::VarOrCurrently(t_, m), t)
            }
        };

        *lhandle = lty;
        *rhandle = rty;
        Ok(())
    }

    pub fn assert_sub(&self, other: &Slot, ctx: &mut TypeContext) -> CheckResult<()> {
        self.0.borrow().assert_sub(&other.0.borrow(), ctx)
    }

    pub fn assert_eq(&self, other: &Slot, ctx: &mut TypeContext) -> CheckResult<()> {
        self.0.borrow().assert_eq(&other.0.borrow(), ctx)
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

        let mut just = |t| Slot::new(S::Just(t));
        let mut cnst = |t| Slot::new(S::Const(t));
        let mut var = |t| Slot::new(S::Var(t));
        let mut curr = |t| Slot::new(S::Currently(t));
        let mut varcnst =
            |ctx: &mut TypeContext, t| Slot::new(S::VarOrConst(t, ctx.gen_mark()));
        let mut varcurr =
            |ctx: &mut TypeContext, t| Slot::new(S::VarOrCurrently(t, ctx.gen_mark()));

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

