use std::fmt;
use std::borrow::Cow;
use std::collections::BTreeSet;

use diag::CheckResult;
use super::{T, TypeContext, NoTypeContext, Lattice, Union, Display};
use super::{Numbers, Strings, Tables, Functions, Class};
use super::{error_not_sub, error_not_eq};
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

    pub fn from<'a>(ty: &T<'a>) -> CheckResult<Unioned> {
        let mut u = Unioned::empty();

        match ty {
            &T::Dynamic(_) | &T::All => panic!("Unioned::from called with T::Dynamic or T::All"),
            &T::TVar(_) => return Err("a type not yet fully resolved cannot be unioned".into()),

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
        write!(f, "(")?;
        let mut first = true;
        self.visit(|ty| {
            if first {
                first = false;
            } else {
                write!(f, "|")?;
            }
            write_ty(&ty, f)
        })?;
        write!(f, ")")
    }
}

impl Union for Unioned {
    type Output = Unioned;

    fn union(&self, other: &Unioned, ctx: &mut TypeContext) -> CheckResult<Unioned> {
        let simple    = self.simple | other.simple;
        let numbers   = self.numbers.union(&other.numbers, ctx)?;
        let strings   = self.strings.union(&other.strings, ctx)?;
        let tables    = self.tables.union(&other.tables, ctx)?;
        let functions = self.functions.union(&other.functions, ctx)?;

        let mut classes = self.classes.clone();
        classes.extend(other.classes.iter().cloned());

        Ok(Unioned {
            simple: simple, numbers: numbers, strings: strings, tables: tables,
            functions: functions, classes: classes,
        })
    }
}

impl Lattice for Unioned {
    fn assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        if self.simple.intersects(!other.simple) {
            return error_not_sub(self, other);
        }

        self.numbers.assert_sub(&other.numbers, &mut NoTypeContext)?;
        self.strings.assert_sub(&other.strings, &mut NoTypeContext)?;

        // XXX err on unions with possible overlapping instantiation for now
        if self.tables.is_some() && self.functions.is_some() {
            return error_not_sub(self, other);
        }
        if other.tables.is_some() && other.functions.is_some() {
            return error_not_sub(self, other);
        }

        self.tables.assert_sub(&other.tables, ctx)?;
        self.functions.assert_sub(&other.functions, ctx)?;

        if !self.classes.is_subset(&other.classes) {
            return error_not_sub(self, other);
        }

        Ok(())
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        if self.simple != other.simple {
            return error_not_eq(self, other);
        }

        self.numbers.assert_eq(&other.numbers, &mut NoTypeContext)?;
        self.strings.assert_eq(&other.strings, &mut NoTypeContext)?;
        self.tables.assert_eq(&other.tables, ctx)?;
        self.functions.assert_eq(&other.functions, ctx)?;

        if self.classes != other.classes {
            return error_not_eq(self, other);
        }

        Ok(())
    }
}

impl Display for Unioned {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, ctx: &TypeContext) -> fmt::Result {
        self.fmt_generic(f, |t, f| fmt::Display::fmt(&t.display(ctx), f))
    }
}

impl fmt::Debug for Unioned {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_generic(f, |t, f| fmt::Debug::fmt(t, f))
    }
}

