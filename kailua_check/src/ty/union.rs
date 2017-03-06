use std::fmt;
use std::borrow::Cow;
use std::collections::BTreeSet;

use kailua_diag::Locale;
use kailua_syntax::Str;
use diag::{TypeReport, TypeResult, Origin, Display};
use super::{T, TypeContext, Lattice, Union};
use super::{Numbers, Strings, Tables, Functions, Class};
use super::flags::*;

// expanded value types for unions
#[derive(Clone, PartialEq)]
pub struct Unioned {
    pub simple: UnionedSimple,
    pub numbers: Option<Numbers>,
    pub strings: Option<Strings>,
    pub tables: Option<Tables>,
    pub functions: Option<Functions>,
    pub classes: BTreeSet<Class>,
}

impl Unioned {
    pub fn empty() -> Unioned {
        Unioned {
            simple: U_NONE, numbers: None, strings: None, tables: None,
            functions: None, classes: BTreeSet::new(),
        }
    }

    pub fn explicit_int(v: i32) -> Unioned {
        Unioned {
            simple: U_NONE, numbers: Some(Numbers::One(v)), strings: None, tables: None,
            functions: None, classes: BTreeSet::new(),
        }
    }

    pub fn explicit_str(s: Str) -> Unioned {
        Unioned {
            simple: U_NONE, numbers: None, strings: Some(Strings::One(s)), tables: None,
            functions: None, classes: BTreeSet::new(),
        }
    }

    pub fn from<'a>(ty: &T<'a>, ctx: &mut TypeContext) -> TypeResult<Unioned> {
        let mut u = Unioned::empty();

        match ty {
            &T::Dynamic(_) | &T::All => {
                panic!("Unioned::from called with T::Dynamic or T::All");
            }
            &T::TVar(_) => {
                return Err(ctx.gen_report().put(Origin::Union,
                                                "a type not yet fully resolved \
                                                 cannot be unioned".into()));
            }

            &T::None     => {}
            &T::Boolean  => { u.simple = U_BOOLEAN; }
            &T::True     => { u.simple = U_TRUE; }
            &T::False    => { u.simple = U_FALSE; }
            &T::Thread   => { u.simple = U_THREAD; }
            &T::UserData => { u.simple = U_USERDATA; }

            &T::Number     => { u.numbers = Some(Numbers::All); }
            &T::Integer    => { u.numbers = Some(Numbers::Int); }
            &T::Int(v)     => { u.numbers = Some(Numbers::One(v)); }
            &T::String     => { u.strings = Some(Strings::All); }
            &T::Str(ref s) => { u.strings = Some(Strings::One(s.clone().into_owned())); }

            &T::Tables(ref tab)     => { u.tables = Some(tab.clone().into_owned()); }
            &T::Functions(ref func) => { u.functions = Some(func.clone().into_owned()); }
            &T::Class(c)            => { u.classes.insert(c); }

            &T::Union(ref u) => return Ok(u.clone().into_owned()), // ignore `u` above
        }

        Ok(u)
    }

    pub fn flags(&self) -> Flags {
        let mut flags = Flags::from_bits_truncate(self.simple.bits());
        match self.numbers {
            None => {}
            Some(Numbers::All) => { flags.insert(T_NUMBER); }
            Some(_)            => { flags.insert(T_INTEGER); }
        }
        if self.strings.is_some()   { flags.insert(T_STRING); }
        if self.tables.is_some()    { flags.insert(T_TABLE); }
        if self.functions.is_some() { flags.insert(T_FUNCTION); }
        if !self.classes.is_empty() { flags.insert(T_TABLE); }
        flags
    }

    pub fn visit<'a, E, F>(&'a self, mut f: F) -> Result<(), E>
            where F: FnMut(T<'a>) -> Result<(), E> {
        if self.simple.contains(U_TRUE) {
            if self.simple.contains(U_FALSE) { f(T::Boolean)?; } else { f(T::True)?; }
        } else if self.simple.contains(U_FALSE) {
            f(T::False)?;
        }
        if self.simple.contains(U_THREAD) { f(T::Thread)?; }
        if self.simple.contains(U_USERDATA) { f(T::UserData)?; }
        match self.numbers {
            Some(Numbers::All) => { f(T::Number)?; }
            Some(Numbers::Int) => { f(T::Integer)?; }
            Some(Numbers::Some(ref vv)) => {
                for &v in vv { f(T::Int(v))?; }
            }
            Some(Numbers::One(v)) => { f(T::Int(v))?; }
            None => {}
        }
        match self.strings {
            Some(Strings::All) => { f(T::String)?; }
            Some(Strings::Some(ref ss)) => {
                for s in ss { f(T::Str(Cow::Borrowed(s)))?; }
            }
            Some(Strings::One(ref s)) => { f(T::Str(Cow::Borrowed(s)))?; }
            None => {}
        }
        if let Some(ref tab) = self.tables { f(T::Tables(Cow::Borrowed(tab)))? }
        if let Some(ref func) = self.functions { f(T::Functions(Cow::Borrowed(func)))? }
        for &c in &self.classes { f(T::Class(c))? }
        Ok(())
    }

    pub fn simplify(self) -> T<'static> {
        let single = {
            let mut single = None;
            let ret = self.visit(|ty| {
                if single.is_some() { return Err(()); }

                // avoid simplifying explicitly written literal types
                match ty {
                    T::Int(_) | T::Str(_) => return Err(()),
                    _ => {}
                }

                single = Some(ty);
                Ok(())
            });
            if ret.is_ok() {
                Some(single.unwrap_or(T::None).into_send())
            } else {
                None
            }
        };
        single.unwrap_or_else(|| T::Union(Cow::Owned(self)))
    }

    fn fmt_generic<WriteTy>(&self, f: &mut fmt::Formatter, mut write_ty: WriteTy) -> fmt::Result
            where WriteTy: FnMut(&T, &mut fmt::Formatter) -> fmt::Result {
        // count up to 2 items so that we can determine if parentheses are needed
        let mut count = 0;
        let _ = self.visit(|_| {
            if count < 2 {
                count += 1;
                Ok(())
            } else {
                Err(())
            }
        });

        if count != 1 {
            write!(f, "(")?;
        }
        let mut first = true;
        self.visit(|ty| {
            if first {
                first = false;
            } else {
                write!(f, "|")?;
            }
            write_ty(&ty, f)
        })?;
        if count != 1 {
            write!(f, ")")?
        }
        Ok(())
    }
}

impl Union for Unioned {
    type Output = Unioned;

    fn union(&self, other: &Unioned, explicit: bool,
             ctx: &mut TypeContext) -> TypeResult<Unioned> {
        (|| {
            let simple = self.simple | other.simple;

            macro_rules! union_options {
                ($lhs:expr, $rhs:expr, |$l:ident, $r:ident| $merge:expr) => (
                    match ($lhs, $rhs) {
                        (&Some(ref $l), &Some(ref $r)) => Some($merge),
                        (&Some(ref lhs), &None) => Some(lhs.clone()),
                        (&None, &Some(ref rhs)) => Some(rhs.clone()),
                        (&None, &None) => None,
                    }
                )
            }

            // numbers and strings can be unioned, though only the explicitly constructed
            // literal types can use unions; implicit unions always resolve to integer or number
            let numbers = union_options!(&self.numbers, &other.numbers, |lhs, rhs| {
                lhs.union(rhs, explicit, ctx)?
            });
            let strings = union_options!(&self.strings, &other.strings, |lhs, rhs| {
                lhs.union(rhs, explicit, ctx)?
            });

            // tables and functions cannot be unioned in any case;
            // unequal table and function components always results in an error
            let tables = union_options!(&self.tables, &other.tables, |lhs, rhs| {
                lhs.assert_eq(rhs, ctx)?;
                lhs.clone()
            });
            let functions = union_options!(&self.functions, &other.functions, |lhs, rhs| {
                lhs.assert_eq(rhs, ctx)?;
                lhs.clone()
            });

            let mut classes = self.classes.clone();
            classes.extend(other.classes.iter().cloned());

            Ok(Unioned {
                simple: simple, numbers: numbers, strings: strings,
                tables: tables, functions: functions, classes: classes,
            })
        })().map_err(|r: TypeReport| r.cannot_union(Origin::Union, self, other, explicit, ctx))
    }
}

impl Lattice for Unioned {
    fn assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> TypeResult<()> {
        (|| {
            macro_rules! assert_sub_options {
                ($lhs:expr, $rhs:expr) => (
                    match ($lhs, $rhs) {
                        (&Some(ref lhs), &Some(ref rhs)) => lhs.assert_sub(rhs, ctx)?,
                        (&Some(_), &None) => return Err(ctx.gen_report()),
                        (&None, _) => {}
                    }
                )
            }

            if self.simple.intersects(!other.simple) {
                return Err(ctx.gen_report());
            }

            assert_sub_options!(&self.numbers, &other.numbers);
            assert_sub_options!(&self.strings, &other.strings);
            assert_sub_options!(&self.tables, &other.tables);
            assert_sub_options!(&self.functions, &other.functions);

            if !self.classes.is_subset(&other.classes) {
                return Err(ctx.gen_report());
            }

            Ok(())
        })().map_err(|r: TypeReport| r.not_sub(Origin::Union, self, other, ctx))
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TypeContext) -> TypeResult<()> {
        (|| {
            if self.simple != other.simple {
                return Err(ctx.gen_report());
            }

            macro_rules! assert_eq_options {
                ($lhs:expr, $rhs:expr) => (
                    match ($lhs, $rhs) {
                        (&Some(ref lhs), &Some(ref rhs)) => lhs.assert_eq(rhs, ctx)?,
                        (&None, &None) => {}
                        (_, _) => return Err(ctx.gen_report()),
                    }
                )
            }

            assert_eq_options!(&self.numbers, &other.numbers);
            assert_eq_options!(&self.strings, &other.strings);
            assert_eq_options!(&self.tables, &other.tables);
            assert_eq_options!(&self.functions, &other.functions);

            if self.classes != other.classes {
                return Err(ctx.gen_report());
            }

            Ok(())
        })().map_err(|r: TypeReport| r.not_eq(Origin::Union, self, other, ctx))
    }
}

impl Display for Unioned {
    fn fmt_displayed(&self, f: &mut fmt::Formatter,
                     locale: Locale, ctx: &TypeContext) -> fmt::Result {
        self.fmt_generic(f, |t, f| fmt::Display::fmt(&t.display(ctx).localized(locale), f))
    }
}

impl fmt::Debug for Unioned {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_generic(f, |t, f| fmt::Debug::fmt(t, f))
    }
}

#[test]
fn test_unioned_simplify() {
    assert_eq!(Unioned::empty().simplify(), T::None);
    assert_eq!(Unioned::explicit_int(42).simplify(),
               T::Union(Cow::Owned(Unioned::explicit_int(42))));
    assert_eq!(Unioned::explicit_str(b"foo"[..].into()).simplify(),
               T::Union(Cow::Owned(Unioned::explicit_str(b"foo"[..].into()))));
    assert_eq!(Unioned { numbers: Some(Numbers::All), ..Unioned::empty() }.simplify(),
               T::Number);
}

