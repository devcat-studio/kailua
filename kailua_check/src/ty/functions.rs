use std::fmt;

use diag::CheckResult;
use super::{Ty, Seq, TypeContext, Lattice};
use super::{error_not_sub, error_not_eq};

#[derive(Clone, PartialEq)]
pub struct Function {
    pub args: Seq<Ty>,
    pub returns: Seq<Ty>,
}

impl Function {
    fn assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        try!(other.args.assert_sub(&self.args, ctx)); // contravariant
        try!(self.returns.assert_sub(&other.returns, ctx)); // covariant
        Ok(())
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        try!(self.args.assert_eq(&other.args, ctx));
        try!(self.returns.assert_eq(&other.returns, ctx));
        Ok(())
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{:?}", self.args));
        match (self.returns.head.len(), self.returns.tail.is_some()) {
            (0, false) => {}
            (1, false) => try!(write!(f, " -> {:?}", self.returns.head[0])),
            (_, _) => try!(write!(f, " -> {:?}", self.returns)),
        }
        Ok(())
    }
}

#[derive(Clone)]
pub enum Functions {
    Simple(Function),
    Multi(Vec<Function>), // overloaded functions (i.e. intersection)
    All,
}

impl Lattice for Functions {
    type Output = Option<Functions>;

    fn normalize(self) -> Option<Functions> {
        if let Functions::Multi(set) = self {
            match set.len() {
                0 => None,
                1 => Some(Functions::Simple(set.into_iter().next().unwrap())),
                _ => Some(Functions::Multi(set)),
            }
        } else {
            Some(self)
        }
    }

    fn union(self, other: Functions, _: &mut TypeContext) -> Option<Functions> {
        match (self, other) {
            (Functions::All, _) => Some(Functions::All),
            (_, Functions::All) => Some(Functions::All),

            (Functions::Simple(a), Functions::Simple(b)) =>
                if a == b { Some(Functions::Simple(a)) } else { Some(Functions::All) },
            (Functions::Multi(a), Functions::Multi(b)) =>
                if a == b { Some(Functions::Multi(a)) } else { Some(Functions::All) },
            (_, _) => Some(Functions::All),
        }
    }

    fn intersect(self, other: Functions, _: &mut TypeContext) -> Option<Functions> {
        match (self, other) {
            (Functions::All, funcs) => Some(funcs),
            (funcs, Functions::All) => Some(funcs),

            (Functions::Simple(a), Functions::Simple(b)) =>
                if a == b {
                    Some(Functions::Simple(a))
                } else {
                    Some(Functions::Multi(vec![a, b]))
                },

            (Functions::Simple(a), Functions::Multi(mut b)) => {
                if !b.contains(&a) { b.push(a); }
                Some(Functions::Multi(b))
            },
            (Functions::Multi(mut a), Functions::Simple(b)) => {
                if !a.contains(&b) { a.push(b); }
                Some(Functions::Multi(a))
            },
            (Functions::Multi(mut a), Functions::Multi(b)) => {
                if a != b { a.extend(b.into_iter()); }
                Some(Functions::Multi(a))
            },
        }
    }

    fn assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        let ok = match (self, other) {
            (&Functions::All, _) => false,
            (_, &Functions::All) => true,

            (&Functions::Simple(ref a), &Functions::Simple(ref b)) => return a.assert_sub(b, ctx),
            (&Functions::Simple(ref a), &Functions::Multi(ref b)) => {
                // a <: b1 /\ b2 === a <: b1 AND a <: b2
                for i in b { try!(a.assert_sub(i, ctx)); }
                true
            }
            (&Functions::Multi(ref _a), &Functions::Simple(ref _b)) => {
                // a1 /\ a2 <: b === a1 <: b OR a2 <: b
                //for i in a { try!(err_on_instantiation(ctx, |ctx| i.assert_sub(b, ctx))); }
                //true
                unimplemented!()
            }
            (&Functions::Multi(ref _a), &Functions::Multi(ref _b)) => {
                // a1 /\ a2 <: b1 /\ b2 === (a1 /\ a2 <: b1) AND (a1 /\ a2 <: b2)
                //                      === (a1 <: b1 OR a2 <: b1) AND (a1 <: b2 OR a2 <: b2)
                unimplemented!()
            }
        };

        if ok { Ok(()) } else { error_not_sub(self, other) }
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        match (self, other) {
            (&Functions::All, &Functions::All) => Ok(()),
            (&Functions::Simple(ref a), &Functions::Simple(ref b)) => a.assert_eq(b, ctx),
            (&Functions::Multi(ref a), &Functions::Multi(ref b)) => {
                // again, it is impossible to align a and b here. just check the identity.
                if a.len() != b.len() {
                    Err(format!("union of functions {:?} and {:?} cannot be directly compared",
                                *self, *other))
                } else {
                    for (i, j) in a.iter().zip(b.iter()) {
                        try!(i.assert_eq(j, ctx));
                    }
                    Ok(())
                }
            }
            (_, _) => error_not_eq(self, other),
        }
    }
}

impl PartialEq for Functions {
    fn eq(&self, other: &Functions) -> bool {
        match (self, other) {
            (&Functions::Simple(ref a), &Functions::Simple(ref b)) => *a == *b,
            (&Functions::Simple(ref a), &Functions::Multi(ref b)) if b.len() == 1 => *a == b[0],
            (&Functions::Multi(ref a), &Functions::Simple(ref b)) if a.len() == 1 => a[0] == *b,
            (&Functions::Multi(ref a), &Functions::Multi(ref b)) => *a == *b,
            (_, _) => false,
        }
    }
}

impl fmt::Debug for Functions {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Functions::All => write!(f, "function"),
            Functions::Simple(ref fty) => fmt::Debug::fmt(fty, f),
            Functions::Multi(ref fs) => {
                let mut first = true;
                for fty in fs {
                    if first { first = false; } else { try!(write!(f, "&")); }
                    try!(write!(f, "{:?}", *fty));
                }
                Ok(())
            }
        }
    }
}

