use std::fmt;
use kailua_env::Spanned;
use kailua_diag::Result;
use kailua_syntax::{Name, FuncKind, Returns};

use diag::{Origin, TypeReport, TypeResult, unquotable_name};
use super::{Display, DisplayState, Ty, TySeq, TypeContext, TypeResolver, Lattice};

#[derive(Clone, PartialEq)]
pub struct Function {
    pub args: TySeq,
    pub argnames: Vec<Option<Spanned<Name>>>, // diagnostics only
    pub returns: Option<TySeq>, // None if diverges
}

impl Function {
    pub fn from_kind(func: &Spanned<FuncKind>, resolv: &mut TypeResolver) -> Result<Function> {
        let args = TySeq::from_kind_seq(&func.args, |namekind| &namekind.1, resolv)?;
        let mut argnames = Vec::new();
        for (i, &(ref name, _)) in func.args.head.iter().enumerate() {
            if let Some(ref name) = *name {
                argnames.resize(i, None);
                argnames.push(Some(name.clone()));
            }
        }
        let returns = match func.returns {
            Returns::Seq(ref seq) => Some(TySeq::from_kind_seq(seq, |kind| kind, resolv)?),
            Returns::Never(_span) => None,
        };
        Ok(Function { args: args, argnames: argnames, returns: returns })
    }

    fn assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> TypeResult<()> {
        // contravariant
        other.args.assert_sub(&self.args, ctx)?;

        // covariant, ! <: any seq
        match (&self.returns, &other.returns) {
            (&Some(ref lhs), &Some(ref rhs)) => lhs.assert_sub(rhs, ctx),
            (&Some(ref lhs), &None) => {
                Err(ctx.gen_report().not_sub(Origin::Functions, lhs, "!", ctx))
            },
            (&None, _) => Ok(()),
        }
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TypeContext) -> TypeResult<()> {
        self.args.assert_eq(&other.args, ctx)?;

        match (&self.returns, &other.returns) {
            (&Some(ref lhs), &Some(ref rhs)) => lhs.assert_sub(rhs, ctx),
            (&Some(ref lhs), &None) => {
                Err(ctx.gen_report().not_sub(Origin::Functions, lhs, "!", ctx))
            },
            (&None, &Some(ref rhs)) => {
                Err(ctx.gen_report().not_sub(Origin::Functions, "!", rhs, ctx))
            },
            (&None, &None) => Ok(()),
        }
    }

    fn fmt_generic<WriteTy, WriteTySeq>(&self, f: &mut fmt::Formatter,
                                        mut write_ty: WriteTy,
                                        mut write_tyseq: WriteTySeq) -> fmt::Result
            where WriteTy: FnMut(&Ty, &mut fmt::Formatter, bool) -> fmt::Result,
                  WriteTySeq: FnMut(&TySeq, &mut fmt::Formatter) -> fmt::Result {
        // we cannot directly print self.args as they should be interleaved with self.argnames
        write!(f, "function(")?;
        let mut first = true;
        let mut names = self.argnames.iter();
        for t in &self.args.head {
            if first { first = false; } else { write!(f, ", ")?; }
            if let Some(name) = names.next() {
                if let Some(ref name) = *name {
                    if unquotable_name(name) {
                        write!(f, "{:-?}: ", name)?;
                    } else {
                        write!(f, "`{:-?}`: ", name)?;
                    }
                }
            }
            write_ty(t, f, false)?;
        }
        if let Some(ref t) = self.args.tail {
            if !first { write!(f, ", ")?; }
            write_ty(t, f, true)?;
            write!(f, "...")?;
        }
        write!(f, ")")?;

        match self.returns {
            Some(ref returns) => match (returns.head.len(), returns.tail.is_some()) {
                (0, false) => write!(f, " --> ()"),
                (1, false) => {
                    write!(f, " --> ")?;
                    write_ty(&returns.head[0], f, false)
                },
                (_, _) => {
                    write!(f, " --> ")?;
                    write_tyseq(returns, f)
                },
            },
            None => write!(f, " --> !"),
        }
    }
}

impl Display for Function {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        self.fmt_generic(
            f,
            |t, f, without_nil| {
                let t = t.display(st);
                if without_nil { write!(f, "{:#}", t) } else { write!(f, "{}", t) }
            },
            |s, f| fmt::Display::fmt(&s.display(st), f),
        )
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_generic(
            f,
            |t, f, without_nil| {
                if without_nil { write!(f, "{:#?}", t) } else { write!(f, "{:?}", t) }
            },
            fmt::Debug::fmt,
        )
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

