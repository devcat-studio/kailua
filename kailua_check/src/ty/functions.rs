use std::fmt;

use diag::CheckResult;
use super::{T, TySeq, TypeContext, Lattice, Display};
use super::{error_not_sub, error_not_eq};

#[derive(Clone, PartialEq)]
pub struct Function {
    pub args: TySeq,
    pub returns: TySeq,
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

    fn fmt_generic<WriteTy, WriteTySeq>(&self, f: &mut fmt::Formatter,
                                        mut write_ty: WriteTy,
                                        mut write_tyseq: WriteTySeq) -> fmt::Result
            where WriteTy: FnMut(&T, &mut fmt::Formatter) -> fmt::Result,
                  WriteTySeq: FnMut(&TySeq, &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "function"));
        try!(write_tyseq(&self.args, f));
        match (self.returns.head.len(), self.returns.tail.is_some()) {
            (0, false) => write!(f, " -> ()"),
            (1, false) => {
                try!(write!(f, " -> "));
                write_ty(&self.returns.head[0], f)
            },
            (_, _) => {
                try!(write!(f, " -> "));
                write_tyseq(&self.returns, f)
            },
        }
    }
}

impl Display for Function {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, ctx: &TypeContext) -> fmt::Result {
        self.fmt_generic(f, |t, f| fmt::Display::fmt(&t.display(ctx), f),
                            |s, f| fmt::Display::fmt(&s.display(ctx), f))
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_generic(f, |t, f| fmt::Debug::fmt(t, f), fmt::Debug::fmt)
    }
}

#[derive(Clone)]
pub enum Functions {
    Simple(Function),
    Multi(Vec<Function>), // overloaded functions (i.e. intersection)
    All,
}

impl Functions {
    fn fmt_generic<WriteFunc>(&self, f: &mut fmt::Formatter,
                              mut write_func: WriteFunc) -> fmt::Result
            where WriteFunc: FnMut(&Function, &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Functions::All => write!(f, "function"),
            Functions::Simple(ref fty) => write_func(fty, f),
            Functions::Multi(ref fs) => {
                let mut first = true;
                for fty in fs {
                    if first { first = false; } else { try!(write!(f, "&")); }
                    try!(write_func(fty, f));
                }
                Ok(())
            }
        }
    }
}

impl Lattice for Functions {
    type Output = Functions;

    fn do_union(&self, other: &Functions, _: &mut TypeContext) -> Functions {
        match (self, other) {
            (&Functions::All, _) => Functions::All,
            (_, &Functions::All) => Functions::All,

            (&Functions::Simple(ref a), &Functions::Simple(ref b)) =>
                if a == b { Functions::Simple(a.clone()) } else { Functions::All },
            (&Functions::Multi(ref a), &Functions::Multi(ref b)) =>
                if a == b { Functions::Multi(a.clone()) } else { Functions::All },
            (_, _) => Functions::All,
        }
    }

    fn do_assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        let ok = match (self, other) {
            (_, &Functions::All) => true,
            (&Functions::All, _) => false,

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

    fn do_assert_eq(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
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

impl Display for Functions {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, ctx: &TypeContext) -> fmt::Result {
        self.fmt_generic(f, |t, f| fmt::Display::fmt(&t.display(ctx), f))
    }
}

impl fmt::Debug for Functions {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_generic(f, fmt::Debug::fmt)
    }
}

