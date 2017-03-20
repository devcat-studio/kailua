use std::fmt;

use diag::{Origin, TypeReport, TypeResult};
use super::{Display, DisplayState, T, TySeq, TypeContext, Lattice};

#[derive(Clone, PartialEq)]
pub struct Function {
    pub args: TySeq,
    pub returns: TySeq,
}

impl Function {
    fn assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> TypeResult<()> {
        other.args.assert_sub(&self.args, ctx)?; // contravariant
        self.returns.assert_sub(&other.returns, ctx)?; // covariant
        Ok(())
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TypeContext) -> TypeResult<()> {
        self.args.assert_eq(&other.args, ctx)?;
        self.returns.assert_eq(&other.returns, ctx)?;
        Ok(())
    }

    fn fmt_generic<WriteTy, WriteTySeq>(&self, f: &mut fmt::Formatter,
                                        mut write_ty: WriteTy,
                                        mut write_tyseq: WriteTySeq) -> fmt::Result
            where WriteTy: FnMut(&T, &mut fmt::Formatter) -> fmt::Result,
                  WriteTySeq: FnMut(&TySeq, &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function")?;
        write_tyseq(&self.args, f)?;
        match (self.returns.head.len(), self.returns.tail.is_some()) {
            (0, false) => write!(f, " --> ()"),
            (1, false) => {
                write!(f, " --> ")?;
                write_ty(&self.returns.head[0], f)
            },
            (_, _) => {
                write!(f, " --> ")?;
                write_tyseq(&self.returns, f)
            },
        }
    }
}

impl Display for Function {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        self.fmt_generic(f, |t, f| fmt::Display::fmt(&t.display(st), f),
                            |s, f| fmt::Display::fmt(&s.display(st), f))
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
    All,
}

impl Functions {
    fn fmt_generic<WriteFunc>(&self, f: &mut fmt::Formatter,
                              mut write_func: WriteFunc) -> fmt::Result
            where WriteFunc: FnMut(&Function, &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Functions::All => write!(f, "function"),
            Functions::Simple(ref fty) => write_func(fty, f),
        }
    }
}

impl Lattice for Functions {
    fn assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> TypeResult<()> {
        (|| {
            match (self, other) {
                (_, &Functions::All) => Ok(()),
                (&Functions::All, _) => Err(ctx.gen_report()),

                (&Functions::Simple(ref a), &Functions::Simple(ref b)) => a.assert_sub(b, ctx),
            }
        })().map_err(|r: TypeReport| r.not_sub(Origin::Functions, self, other, ctx))
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TypeContext) -> TypeResult<()> {
        (|| {
            match (self, other) {
                (&Functions::All, &Functions::All) => Ok(()),
                (&Functions::Simple(ref a), &Functions::Simple(ref b)) => a.assert_eq(b, ctx),
                (_, _) => Err(ctx.gen_report()),
            }
        })().map_err(|r: TypeReport| r.not_eq(Origin::Functions, self, other, ctx))
    }
}

impl PartialEq for Functions {
    fn eq(&self, other: &Functions) -> bool {
        match (self, other) {
            (&Functions::All, &Functions::All) => true,
            (&Functions::Simple(ref a), &Functions::Simple(ref b)) => *a == *b,
            (_, _) => false,
        }
    }
}

impl Display for Functions {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        self.fmt_generic(f, |t, f| fmt::Display::fmt(&t.display(st), f))
    }
}

impl fmt::Debug for Functions {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_generic(f, fmt::Debug::fmt)
    }
}

