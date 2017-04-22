use std::fmt;
use std::borrow::Cow;
use std::collections::BTreeSet;

use kailua_syntax::Str;
use diag::{TypeReport, TypeResult, Origin};
use super::display::{Display, DisplayState, DisplayName};
use super::{T, TypeContext, Lattice, Union};
use super::{Numbers, Strings, Tables, Functions, Class};
use super::flags::*;

/// An expanded value type for unions.
///
/// This type is only constructed explicitly (that's why it doesn't contain, e.g. type variables).
/// Any implicit type operation will try to destroy the union.
/// As an exception, normally an explicitly constructed union type with
/// a single boolean, number or string literal should be simplified to a simple value type
/// but they are retained in order to coerce explicitly written literal types.
#[derive(Clone, PartialEq)]
pub struct Unioned {
    /// A set of types that do not interact to each other in the union type.
    pub simple: UnionedSimple,

    /// Number types, if any.
    pub numbers: Option<Numbers>,

    /// String types, if any.
    pub strings: Option<Strings>,

    /// Table types, if any.
    pub tables: Option<Tables>,

    /// Function types, if any.
    pub functions: Option<Functions>,

    /// A set of nominal types.
    pub classes: BTreeSet<Class>,

    /// Optional display hints.
    ///
    /// This contains a list of the display name and its (approximate) upper bound.
    /// A type operation that shrinks the union type will remove the hint
    /// if the union type no longer contains any portion of a type represented by the hint.
    /// It's conceptually same to the display hint in the value type (`Ty`),
    /// but as we don't store `Ty` itself in the union type it has to be duplicated.
    ///
    /// In order to support this operation, `Unioned::filter_display_hints` method will
    /// receive type flags that have been removed to filter the hints and
    /// remove any hint with empty flags.
    pub display_hints: Vec<(Flags, DisplayName)>,
}

impl Unioned {
    pub fn empty() -> Unioned {
        Unioned {
            simple: U_NONE, numbers: None, strings: None, tables: None,
            functions: None, classes: BTreeSet::new(), display_hints: Vec::new(),
        }
    }

    pub fn explicit_bool(b: bool) -> Unioned {
        let simple = if b { U_TRUE } else { U_FALSE };
        Unioned {
            simple: simple, numbers: None, strings: None, tables: None,
            functions: None, classes: BTreeSet::new(), display_hints: Vec::new(),
        }
    }

    pub fn explicit_int(v: i32) -> Unioned {
        Unioned {
            simple: U_NONE, numbers: Some(Numbers::One(v)), strings: None, tables: None,
            functions: None, classes: BTreeSet::new(), display_hints: Vec::new(),
        }
    }

    pub fn explicit_str(s: Str) -> Unioned {
        Unioned {
            simple: U_NONE, numbers: None, strings: Some(Strings::One(s)), tables: None,
            functions: None, classes: BTreeSet::new(), display_hints: Vec::new(),
        }
    }

    pub fn from<'a>(ty: &T<'a>, ctx: &mut TypeContext) -> TypeResult<Unioned> {
        let mut u = Unioned::empty();

        match ty {
            &T::Dynamic(_) | &T::All => {
                panic!("Unioned::from called with T::Dynamic or T::All");
            }
            &T::TVar(_) => {
                return Err(ctx.gen_report().cannot_union_single(ty, ctx));
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

    pub fn visit<'a, E, F>(&'a self, mask: Flags, mut f: F) -> Result<(), E>
        where F: FnMut(T<'a>) -> Result<(), E>,
    {
        let simple = self.simple & UnionedSimple::from_bits_truncate(mask.bits());
        if simple.contains(U_TRUE) {
            if simple.contains(U_FALSE) { f(T::Boolean)?; } else { f(T::True)?; }
        } else if simple.contains(U_FALSE) {
            f(T::False)?;
        }
        if simple.contains(U_THREAD) { f(T::Thread)?; }
        if simple.contains(U_USERDATA) { f(T::UserData)?; }

        match self.numbers {
            Some(Numbers::All) if mask.intersects(T_NUMBER) => { f(T::Number)?; }
            Some(Numbers::Int) if mask.intersects(T_INTEGER) => { f(T::Integer)?; }
            Some(Numbers::Some(ref vv)) if mask.intersects(T_INTEGER) => {
                for &v in vv { f(T::Int(v))?; }
            }
            Some(Numbers::One(v)) if mask.intersects(T_INTEGER) => { f(T::Int(v))?; }
            _ => {}
        }

        if mask.contains(T_STRING) {
            match self.strings {
                Some(Strings::All) => { f(T::String)?; }
                Some(Strings::Some(ref ss)) => {
                    for s in ss { f(T::Str(Cow::Borrowed(s)))?; }
                }
                Some(Strings::One(ref s)) => {
                    f(T::Str(Cow::Borrowed(s)))?;
                }
                None => {}
            }
        }

        if mask.contains(T_TABLE) {
            if let Some(ref tab) = self.tables {
                f(T::Tables(Cow::Borrowed(tab)))?;
            }
        }

        if mask.contains(T_FUNCTION) {
            if let Some(ref func) = self.functions {
                f(T::Functions(Cow::Borrowed(func)))?;
            }
        }

        if mask.contains(T_TABLE) {
            for &c in &self.classes { f(T::Class(c))?; }
        }

        Ok(())
    }

    pub fn add_display_hint(&mut self, flags: Flags, name: &DisplayName) {
        if !self.display_hints.iter().any(|&(_, ref n)| *n == *name) { // TODO O(n^2)
            self.display_hints.push((flags, name.clone()));
        }
    }

    pub fn filter_display_hints(&mut self, removed: Flags) {
        for &mut (ref mut flags, _) in &mut self.display_hints {
            *flags &= !removed;
        }
        self.display_hints.retain(|&(flags, _)| flags != T_NONE);
    }

    pub fn simplify(self) -> T<'static> {
        let single = {
            let mut single = None;
            let ret = self.visit(T_ALL, |ty| {
                if single.is_some() { return Err(()); }

                // avoid simplifying explicitly written literal types
                match ty {
                    T::Int(_) | T::Str(_) | T::True | T::False => return Err(()),
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

    fn fmt_generic<WriteTy, WriteName>(&self, f: &mut fmt::Formatter,
                                       hints: &[(Flags, DisplayName)],
                                       mut write_ty: WriteTy,
                                       mut write_name: WriteName) -> fmt::Result
        where WriteTy: FnMut(&T, &mut fmt::Formatter) -> fmt::Result,
              WriteName: FnMut(&DisplayName, &mut fmt::Formatter) -> fmt::Result
    {
        // we always ignore variable hints
        let keep_hint = |&&(_, ref name): &&(Flags, DisplayName)| {
            if let DisplayName::Var(_) = *name { false } else { true }
        };

        // named types are printed at the end,
        // but flags associated to them should not be printed before them
        let (mut count, mask) =
            hints.iter().filter(&keep_hint)
                        .fold((0, T_ALL), |(c, f), &(flags, _)| (c + 1, f & !flags));

        // count up to 2 items so that we can determine if parentheses are needed
        let _ = self.visit(mask, |_| {
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
        self.visit(mask, |ty| {
            if first {
                first = false;
            } else {
                write!(f, "|")?;
            }
            write_ty(&ty, f)
        })?;
        for &(_, ref name) in hints.iter().filter(keep_hint) {
            if first {
                first = false;
            } else {
                write!(f, "|")?;
            }
            write_name(name, f)?;
        }
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
        trace!("calculating an {} union of {:?} (Unioned) and {:?} (Unioned)",
               if explicit { "explicit" } else { "implicit" }, *self, *other);

        (|| {
            let simple = self.simple | other.simple;

            // in the explicit case `true | false` is not allowed
            if explicit {
                let llit = self.simple.intersects(U_BOOLEAN) && !self.simple.contains(U_BOOLEAN);
                let rlit = other.simple.intersects(U_BOOLEAN) && !other.simple.contains(U_BOOLEAN);
                let ubool = simple.contains(U_BOOLEAN);
                if llit && rlit && ubool {
                    return Err(ctx.gen_report());
                }
            }

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

            // tables cannot be unioned except when one operand is a record and another is
            // a supertype of that record. otherwise (including the case of two records)
            // they should be equal, so records can be seemingly unioned due to row extension
            let tables = union_options!(&self.tables, &other.tables, |lhs, rhs| {
                lhs.union(rhs, explicit, ctx)?
            });

            // functions cannot be unioned at all and unequal function always results in an error
            let functions = union_options!(&self.functions, &other.functions, |lhs, rhs| {
                lhs.assert_eq(rhs, ctx)?;
                lhs.clone()
            });

            let mut classes = self.classes.clone();
            classes.extend(other.classes.iter().cloned());

            let mut u = Unioned {
                simple: simple, numbers: numbers, strings: strings,
                tables: tables, functions: functions, classes: classes,
                display_hints: self.display_hints.clone(),
            };
            for &(flags, ref name) in &other.display_hints {
                u.add_display_hint(flags, name);
            }

            Ok(u)
        })().map_err(|r: TypeReport| r.cannot_union(Origin::Union, self, other, explicit, ctx))
    }
}

impl Lattice for Unioned {
    fn assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> TypeResult<()> {
        trace!("asserting a constraint {:?} (Unioned) <: {:?} (Unioned)", *self, *other);

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
        trace!("asserting a constraint {:?} (Unioned) = {:?} (Unioned)", *self, *other);

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
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        self.fmt_generic(
            f, &self.display_hints,
            |t, f| fmt::Display::fmt(&t.display(st), f),
            |name, f| fmt::Display::fmt(&name.display(st), f),
        )
    }
}

impl fmt::Debug for Unioned {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_generic(
            f, &[],
            |t, f| fmt::Debug::fmt(t, f),
            |_, _| panic!("no names allowed"),
        )?;

        if !self.display_hints.is_empty() {
            write!(f, " <hint {:?}>", &self.display_hints)?;
        }

        Ok(())
    }
}

#[test]
fn test_unioned_simplify() {
    assert_eq!(Unioned::empty().simplify(), T::None);
    assert_eq!(Unioned::explicit_bool(true).simplify(),
               T::Union(Cow::Owned(Unioned::explicit_bool(true))));
    assert_eq!(Unioned::explicit_int(42).simplify(),
               T::Union(Cow::Owned(Unioned::explicit_int(42))));
    assert_eq!(Unioned::explicit_str(b"foo"[..].into()).simplify(),
               T::Union(Cow::Owned(Unioned::explicit_str(b"foo"[..].into()))));
    assert_eq!(Unioned { numbers: Some(Numbers::All), ..Unioned::empty() }.simplify(),
               T::Number);
}

