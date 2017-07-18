use std::fmt;
use std::ops;
use std::mem;
use std::borrow::Cow;
use std::result;
use std::collections::{hash_map, HashMap};

use kailua_env::{Spanned, WithLoc};
use kailua_syntax::{Str, Name};
use kailua_syntax::ast::{K, Kind, SlotKind};
use kailua_diag::{Result, Reporter};
use diag::{Origin, TypeReport, TypeResult, TypeReportHint, TypeReportMore};
use super::display::{Display, DisplayState, DisplayName};
use super::{TypeContext, NoTypeContext, TypeResolver};
use super::{F, Slot, Lattice, Union, Dummy};
use super::{Numbers, Strings, Key, Tables, Function, Functions, Unioned, TVar, Tag, Class};
use super::flags::*;
use message as m;

/// A dynamic type.
///
/// The dynamic type comes from gradual typing and
/// most operations with the dynamic type will result in the dynamic type.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Dyn {
    /// User-generated dynamic type, i.e. `WHATEVER`.
    User,

    /// Error type from parser and checker.
    Oops,
}

impl Dyn {
    pub fn or(lhs: Option<Dyn>, rhs: Option<Dyn>) -> Option<Dyn> {
        match (lhs, rhs) {
            (Some(Dyn::Oops), _) | (_, Some(Dyn::Oops)) => Some(Dyn::Oops),
            (Some(Dyn::User), _) | (_, Some(Dyn::User)) => Some(Dyn::User),
            (None, None) => None,
        }
    }

    pub fn union(&self, rhs: Dyn) -> Dyn {
        match (*self, rhs) {
            (Dyn::Oops, _) | (_, Dyn::Oops) => Dyn::Oops,
            (Dyn::User, Dyn::User) => Dyn::User,
        }
    }
}

/// A value type, except for `nil` which is specially treated.
///
/// Unions are also special in that they can be generated only from the type specification.
/// Also, literal types in `T` are "implicit" in that they will be converted to supertypes
/// when being assigned (otherwise it would be very cumbersome to use);
/// the type specification generates unions for literal types (even when there is only one item),
/// so explicitly written literal types are retained on assignment.
#[derive(Clone)]
pub enum T<'a> {
    /// Dynamic type.
    Dynamic(Dyn),

    /// `any` (the top type).
    All,

    /// A bottom type.
    ///
    /// This is not equivalent to a diverging type (only allowed in return types)
    /// because in Kailua `T`, `T!` and `T?` can be almost freely mixed.
    /// Consequently, this represents either a silent nil (`nil`), a noisy nil (`nil?`)
    /// or a type that can be used in any way except for the conversion (`nil!`).
    None,

    /// `boolean`.
    Boolean,

    /// `true` generated from an implicit Lua expression.
    True,

    /// `false` generated from an implicit Lua expression.
    False,

    /// `integer`.
    Integer,

    /// `number`.
    Number,

    /// `string`.
    String,

    /// `thread`.
    Thread,

    /// `userdata`.
    UserData,

    /// An integer literal type generated from an implicit Lua expression.
    Int(i32),

    /// A string literal type generated from an implicit Lua expression.
    Str(Cow<'a, Str>),

    /// Table types.
    Tables(Cow<'a, Tables>),

    /// Function types.
    Functions(Cow<'a, Functions>),

    /// Nominal types.
    Class(Class),

    /// A type variable.
    TVar(TVar),

    /// Union types, or explicit literal types.
    Union(Cow<'a, Unioned>),
}

impl<'a> T<'a> {
    pub fn dummy() -> T<'a> { T::Dynamic(Dyn::Oops) }

    pub fn table()           -> T<'a> { T::Tables(Cow::Owned(Tables::All)) }
    /*
    pub fn empty_table()     -> T<'a> { T::Tables(Cow::Owned(Tables::Fields(RVar::fresh()))) }
    */
    pub fn function()        -> T<'a> { T::Functions(Cow::Owned(Functions::All)) }
    pub fn func(f: Function) -> T<'a> { T::Functions(Cow::Owned(Functions::Simple(f))) }

    pub fn ints<I: IntoIterator<Item=i32>>(i: I) -> T<'a> {
        let mut u = Unioned::empty();
        u.numbers = Some(Numbers::Some(i.into_iter().collect()));
        T::Union(Cow::Owned(u))
    }
    pub fn strs<I: IntoIterator<Item=Str>>(i: I) -> T<'a> {
        let mut u = Unioned::empty();
        u.strings = Some(Strings::Some(i.into_iter().collect()));
        T::Union(Cow::Owned(u))
    }
    /*
    pub fn tuple<'b, I: IntoIterator<Item=Slot>>(i: I) -> T<'a> {
        let i = i.into_iter().enumerate();
        let fields = i.map(|(i,v)| ((i as i32 + 1).into(), v));
        T::Tables(Cow::Owned(Tables::Fields(fields.collect(), RVar::fresh())))
    }
    pub fn record<'b, I: IntoIterator<Item=(Str,Slot)>>(i: I) -> T<'a> {
        let i = i.into_iter();
        let fields = i.map(|(k,v)| (k.into(), v));
        T::Tables(Cow::Owned(Tables::Fields(fields.collect(), RVar::fresh())))
    }
    */
    pub fn array(v: Slot) -> T<'a> {
        T::Tables(Cow::Owned(Tables::Array(v)))
    }
    pub fn map<X: Into<Ty>>(k: X, v: Slot) -> T<'a> {
        T::Tables(Cow::Owned(Tables::Map(k.into(), v)))
    }

    pub fn flags(&self) -> Flags {
        match *self {
            T::Dynamic(Dyn::User) => T_ALL | T_WHATEVER,
            T::Dynamic(Dyn::Oops) => T_ALL | T_DYNAMIC,

            T::All      => T_ALL,
            T::None     => T_NONE,
            T::Boolean  => T_BOOLEAN,
            T::True     => T_TRUE,
            T::False    => T_FALSE,
            T::Thread   => T_THREAD,
            T::UserData => T_USERDATA,

            T::Number   => T_NUMBER,
            T::Integer  => T_INTEGER,
            T::Int(_)   => T_INTEGER,
            T::String   => T_STRING,
            T::Str(_)   => T_STRING,

            T::Tables(..) => T_TABLE,
            T::Functions(..) => T_FUNCTION,
            T::Class(..) => T_TABLE,

            T::TVar(..) => T_NONE,
            T::Union(ref u) => u.flags(),
        }
    }

    pub fn to_ref<'b: 'a>(&'b self) -> T<'b> {
        match *self {
            T::Dynamic(dyn) => T::Dynamic(dyn),

            T::All      => T::All,
            T::None     => T::None,
            T::Boolean  => T::Boolean,
            T::True     => T::True,
            T::False    => T::False,
            T::Thread   => T::Thread,
            T::UserData => T::UserData,

            T::Number     => T::Number,
            T::Integer    => T::Integer,
            T::Int(v)     => T::Int(v),
            T::String     => T::String,
            T::Str(ref s) => T::Str(Cow::Borrowed(&**s)),

            T::Tables(ref tab) => T::Tables(Cow::Borrowed(&**tab)),
            T::Functions(ref func) => T::Functions(Cow::Borrowed(&**func)),
            T::Class(c) => T::Class(c),
            T::TVar(v) => T::TVar(v),
            T::Union(ref u) => T::Union(Cow::Borrowed(&**u)),
        }
    }

    // used for simplifying logical conditions

    pub fn truthy<'b>(&'b self) -> Cow<'b, T<'a>> {
        match *self {
            T::Boolean => Cow::Owned(T::True),
            T::False => Cow::Owned(T::None),
            T::Union(ref u) if u.simple.intersects(U_FALSE) => {
                let mut u = u.clone().into_owned();
                u.simple.remove(U_FALSE);
                u.filter_display_hints(T_FALSE);
                Cow::Owned(u.simplify())
            },
            _ => Cow::Borrowed(self),
        }
    }

    pub fn falsy<'b>(&'b self) -> Cow<'b, T<'a>> {
        match *self {
            T::None | T::False => Cow::Borrowed(self),
            T::Boolean => Cow::Owned(T::False),
            // XXX should we really keep the display hints for this case????
            T::Union(ref u) if u.simple & U_FALSE != U_NONE => Cow::Owned(T::False),
            _ => Cow::Owned(T::None),
        }
    }

    pub fn is_dynamic(&self)  -> bool { self.flags().is_dynamic() }
    pub fn is_integral(&self) -> bool { self.flags().is_integral() }
    pub fn is_numeric(&self)  -> bool { self.flags().is_numeric() }
    pub fn is_stringy(&self)  -> bool { self.flags().is_stringy() }
    pub fn is_tabular(&self)  -> bool { self.flags().is_tabular() }
    pub fn is_callable(&self) -> bool { self.flags().is_callable() }
    pub fn is_truthy(&self)   -> bool { self.flags().is_truthy() }
    pub fn is_falsy(&self)    -> bool { self.flags().is_falsy() }

    pub fn get_dynamic(&self) -> Option<Dyn> {
        match *self {
            T::Dynamic(dyn) => Some(dyn),
            _ => None,
        }
    }

    pub fn get_tables(&self) -> Option<&Tables> {
        match *self {
            T::Tables(ref tab) => Some(tab),
            T::Union(ref u) => u.tables.as_ref(),
            _ => None,
        }
    }

    pub fn get_functions(&self) -> Option<&Functions> {
        match *self {
            T::Functions(ref func) => Some(func),
            T::Union(ref u) => u.functions.as_ref(),
            _ => None,
        }
    }

    pub fn get_tvar(&self) -> Option<TVar> {
        match *self {
            T::TVar(tv) => Some(tv),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<&Str> {
        // unlike flags, type variable should not be present
        match *self {
            T::Str(ref s) => Some(s.as_ref()),
            T::Union(ref u) if u.flags() == T_STRING => {
                match *u.strings.as_ref().unwrap() {
                    Strings::One(ref s) => Some(s),
                    Strings::Some(ref set) if set.len() == 1 => Some(set.iter().next().unwrap()),
                    _ => None,
                }
            },
            _ => None,
        }
    }

    pub fn as_integer(&self) -> Option<i32> {
        // unlike flags, type variable should not be present
        match *self {
            T::Int(v) => Some(v),
            T::Union(ref u) if u.flags() == T_INTEGER => {
                match *u.numbers.as_ref().unwrap() {
                    Numbers::One(v) => Some(v),
                    Numbers::Some(ref set) if set.len() == 1 => Some(*set.iter().next().unwrap()),
                    _ => None,
                }
            },
            _ => None,
        }
    }

    /// Coerces "implicit" types into "explicit" types.
    ///
    /// This is the only possible way in Kailua for implicit coercions to happen,
    /// and does the following:
    ///
    /// - Removes implicit literal types from `self`,
    ///   by replacing them with `boolean`, `integer` or `string`.
    ///   This does not affect explicit literal types from `Ty::from_kind`.
    ///   (This happens in the shallow way, as implicit types are incrementally constructed.)
    ///
    /// This is used when new variable or field has been added without explicit types,
    /// or upper & exact bounds for type variables get updated (lower bounds do not).
    pub fn coerce<'b>(&'b self) -> Cow<'b, T<'a>> {
        match *self {
            T::True | T::False => Cow::Owned(T::Boolean),
            T::Int(_) => Cow::Owned(T::Integer),
            T::Str(_) => Cow::Owned(T::String),
            _ => Cow::Borrowed(self),
        }
    }

    /// Replaces type and row variables present in given type to their fresh copies.
    ///
    /// Primarily used for function calls where all type variable and row variables are
    /// made fresh per each call, so that prior calls cannot affect constraints to later calls.
    pub fn generalize(self, ctx: &mut TypeContext) -> T<'static> {
        match self {
            T::Dynamic(dyn) => T::Dynamic(dyn),

            T::All        => T::All,
            T::None       => T::None,
            T::Boolean    => T::Boolean,
            T::True       => T::True,
            T::False      => T::False,
            T::Thread     => T::Thread,
            T::UserData   => T::UserData,

            T::Number     => T::Number,
            T::Integer    => T::Integer,
            T::Int(v)     => T::Int(v),
            T::String     => T::String,
            T::Str(s)     => T::Str(Cow::Owned(s.into_owned())),

            // tables are recursively altered
            T::Tables(tab) => T::Tables(Cow::Owned(tab.into_owned().generalize(ctx))),

            // functions are _not_ recursively altered (will be generalized at call site)
            T::Functions(func) => T::Functions(Cow::Owned(func.into_owned())),

            T::Class(c) => T::Class(c),
            T::TVar(tv) => T::TVar(ctx.copy_tvar(tv)),

            // unions _are_ recursively altered
            T::Union(u) => {
                let mut u = u.into_owned();
                u.tables = u.tables.take().map(|tab| tab.generalize(ctx));
                T::Union(Cow::Owned(u))
            },
        }
    }

    pub fn into_send(self) -> T<'static> {
        match self {
            T::Dynamic(dyn) => T::Dynamic(dyn),

            T::All        => T::All,
            T::None       => T::None,
            T::Boolean    => T::Boolean,
            T::True       => T::True,
            T::False      => T::False,
            T::Thread     => T::Thread,
            T::UserData   => T::UserData,

            T::Number     => T::Number,
            T::Integer    => T::Integer,
            T::Int(v)     => T::Int(v),
            T::String     => T::String,
            T::Str(s)     => T::Str(Cow::Owned(s.into_owned())),

            T::Tables(tab)     => T::Tables(Cow::Owned(tab.into_owned())),
            T::Functions(func) => T::Functions(Cow::Owned(func.into_owned())),
            T::Class(c)        => T::Class(c),
            T::TVar(tv)        => T::TVar(tv),

            T::Union(u) => T::Union(Cow::Owned(u.into_owned())),
        }
    }

    pub fn filter_by_flags<'b>(&'b self, flags: Flags,
                               ctx: &mut TypeContext) -> TypeResult<Cow<'b, T<'a>>> {
        fn flags_to_ubound(flags: Flags) -> T<'static> {
            assert!(!flags.intersects(T_DYNAMIC));

            let mut t = T::None;
            if flags.contains(T_TRUE)       { t = t | T::True; }
            if flags.contains(T_FALSE)      { t = t | T::False; }
            if flags.contains(T_NONINTEGER) { t = t | T::Number; }
            if flags.contains(T_INTEGER)    { t = t | T::Integer; }
            if flags.contains(T_STRING)     { t = t | T::String; }
            if flags.contains(T_TABLE)      { t = t | T::table(); }
            if flags.contains(T_FUNCTION)   { t = t | T::function(); }
            if flags.contains(T_THREAD)     { t = t | T::Thread; }
            if flags.contains(T_USERDATA)   { t = t | T::UserData; }
            t
        }

        fn narrow_numbers<'a>(num: Cow<'a, Numbers>, flags: Flags) -> Option<Cow<'a, Numbers>> {
            let is_all = match num.as_ref() { &Numbers::All => true, _ => false };
            match (flags & T_NUMBER, is_all) {
                (T_NONINTEGER, false) => None,
                (T_INTEGER, true) => Some(Cow::Owned(Numbers::Int)),
                (T_NONE, _) => None,
                (_, _) => Some(num),
            }
        }

        fn narrow_tvar(tvar: TVar, flags: Flags, ctx: &mut TypeContext) -> TypeResult<TVar> {
            let ubound = flags_to_ubound(flags);

            // make a type variable i such that i <: ubound and i <: tvar
            let i = ctx.gen_tvar();
            ctx.assert_tvar_sub_tvar(i, tvar)?;
            ctx.assert_tvar_sub(i, &Ty::new(ubound))?;

            Ok(i)
        }

        let flags_or_none = |bit| {
            if flags.contains(bit) { Cow::Borrowed(self) } else { Cow::Owned(T::None) }
        };

        match *self {
            T::Dynamic(_) => Ok(Cow::Borrowed(self)),
            T::None => Ok(Cow::Borrowed(self)),
            T::All => Ok(Cow::Owned(flags_to_ubound(flags))),
            T::Boolean => match flags & T_BOOLEAN {
                T_BOOLEAN => Ok(Cow::Borrowed(self)),
                T_TRUE => Ok(Cow::Owned(T::True)),
                T_FALSE => Ok(Cow::Owned(T::False)),
                _ => Ok(Cow::Owned(T::None)),
            },
            T::Number => match flags & T_NUMBER {
                T_NUMBER | T_NONINTEGER => Ok(Cow::Borrowed(self)),
                T_INTEGER => Ok(Cow::Owned(T::Integer)),
                _ => Ok(Cow::Owned(T::None)),
            },

            T::Integer  => Ok(flags_or_none(T_INTEGER)),
            T::Int(_)   => Ok(flags_or_none(T_INTEGER)),
            T::True     => Ok(flags_or_none(T_TRUE)),
            T::False    => Ok(flags_or_none(T_FALSE)),
            T::Thread   => Ok(flags_or_none(T_THREAD)),
            T::UserData => Ok(flags_or_none(T_USERDATA)),
            T::String   => Ok(flags_or_none(T_STRING)),

            T::Str(_)       => Ok(flags_or_none(T_STRING)),
            T::Tables(_)    => Ok(flags_or_none(T_TABLE)),
            T::Functions(_) => Ok(flags_or_none(T_FUNCTION)),
            T::Class(_)     => Ok(flags_or_none(T_TABLE)),

            T::TVar(tv) => Ok(Cow::Owned(T::TVar(narrow_tvar(tv, flags, ctx)?))),

            T::Union(ref u) => {
                // compile a list of flags to remove, and only alter if there is any removal
                let removed = !flags & u.flags();
                if removed.is_empty() {
                    return Ok(Cow::Borrowed(self));
                }

                let mut u = u.clone().into_owned();
                let removed_simple = UnionedSimple::from_bits_truncate(removed.bits());
                if !removed_simple.is_empty() { u.simple &= !removed_simple; }
                if removed.intersects(T_NUMBER) {
                    let num = Cow::Owned(u.numbers.unwrap());
                    u.numbers = narrow_numbers(num, flags).map(|num| num.into_owned());
                }
                if removed.contains(T_STRING)   { u.strings   = None; }
                if removed.contains(T_TABLE)    { u.tables    = None; }
                if removed.contains(T_FUNCTION) { u.functions = None; }
                u.filter_display_hints(removed);
                Ok(Cow::Owned(u.simplify()))
            },
        }
    }
}

impl<'a> Dummy for T<'a> {
    fn dummy() -> T<'a> { Self::dummy() }
}

impl<'a> Union<Unioned> for T<'a> {
    type Output = Unioned;

    fn union(&self, other: &Unioned, explicit: bool,
             ctx: &mut TypeContext) -> TypeResult<Unioned> {
        trace!("calculating an {} union of {:?} (T) and {:?} (Union)",
               if explicit { "explicit" } else { "implicit" }, *self, *other);

        let lhs = Unioned::from(self, ctx)?;
        lhs.union(other, explicit, ctx)
    }
}

impl<'a> Lattice<Unioned> for T<'a> {
    // assumes that the Unioned itself has been simplified.
    fn assert_sub(&self, other: &Unioned, ctx: &mut TypeContext) -> TypeResult<()> {
        trace!("asserting a constraint {:?} (T) <: {:?} (Unioned)", *self, *other);

        (|| {
            // try to match each component
            let ok = match *self {
                T::Dynamic(_) | T::None => true,

                T::Boolean  => other.simple.contains(U_BOOLEAN),
                T::True     => other.simple.contains(U_TRUE),
                T::False    => other.simple.contains(U_FALSE),
                T::Thread   => other.simple.contains(U_THREAD),
                T::UserData => other.simple.contains(U_USERDATA),

                T::Number => match other.numbers {
                    Some(Numbers::All) => true,
                    _ => false,
                },
                T::Integer => match other.numbers {
                    Some(Numbers::All) | Some(Numbers::Int) => true,
                    _ => false,
                },
                T::Int(lhs) => match other.numbers {
                    Some(Numbers::All) | Some(Numbers::Int) => true,
                    Some(Numbers::Some(ref rhs)) => rhs.contains(&lhs),
                    Some(Numbers::One(rhs)) => lhs == rhs,
                    _ => false,
                },

                T::String => match other.strings {
                    Some(Strings::All) => true,
                    _ => false,
                },
                T::Str(ref lhs) => match other.strings {
                    Some(Strings::All) => true,
                    Some(Strings::Some(ref rhs)) => rhs.contains(lhs),
                    Some(Strings::One(ref rhs)) => **lhs == *rhs,
                    _ => false,
                },

                T::Tables(ref lhs) => {
                    if let Some(ref num) = other.tables {
                        return lhs.assert_sub(num, ctx);
                    }
                    false
                }
                T::Functions(ref lhs) => {
                    if let Some(ref num) = other.functions {
                        return lhs.assert_sub(num, ctx);
                    }
                    false
                },
                T::Class(c) => other.classes.contains(&c),

                T::TVar(lhs) => {
                    let otherty = &Ty::new(T::Union(Cow::Owned(other.clone())));
                    return ctx.assert_tvar_sub(lhs, otherty);
                },

                T::Union(ref lhs) => {
                    return lhs.assert_sub(other, ctx);
                },

                _ => false,
            };

            if ok {
                Ok(())
            } else {
                // the union sans type variable is not a subtype of self.
                // XXX we can try asserting an additional constraint to the union's type variable
                // if any, but for now we bail out
                Err(ctx.gen_report())
            }
        })().map_err(|r: TypeReport| r.not_sub(Origin::TUnion, self, other, ctx))
    }

    // assumes that the Unioned itself has been simplified.
    fn assert_eq(&self, other: &Unioned, ctx: &mut TypeContext) -> TypeResult<()> {
        trace!("asserting a constraint {:?} (T) = {:?} (Unioned)", *self, *other);

        (|| {
            match *self {
                T::Dynamic(_) => Ok(()),
                T::Union(ref lhs) => lhs.assert_eq(other, ctx),
                _ => Err(ctx.gen_report()),
            }
        })().map_err(|r: TypeReport| r.not_eq(Origin::TUnion, self, other, ctx))
    }
}

impl<'a> T<'a> {
    // not intended to be used in public, because it can result in either T or Ty.
    // Ty is used if it can have different nils, tags or display hints.
    fn union_with_hints<'b>(&self, selfhint: Option<&DisplayHint>,
                            other: &T<'b>, otherhint: Option<&DisplayHint>, explicit: bool,
                            ctx: &mut TypeContext) -> TypeResult<result::Result<T<'static>, Ty>> {
        fn resolve<'t, 'u>(t: &'t T<'u>, ctx: &mut TypeContext)
            -> (Cow<'t, T<'u>>, Option<(Nil, Option<Tag>)>, Option<DisplayHint>)
        {
            if let &T::TVar(tv) = t {
                if let Some(ty) = ctx.get_tvar_exact_type(tv) {
                    let nil = ty.nil();
                    let tag = ty.tag();
                    let hint = ty.display_hint().cloned();
                    return (Cow::Owned(ty.unwrap()), Some((nil, tag)), hint);
                }
            }
            (Cow::Borrowed(t), None, None)
        }

        (|| {
            // resolve type variables, which may result in Ty
            let (t1, niltag1, hint1) = resolve(self, ctx);
            let (t2, niltag2, hint2) = resolve(other, ctx);

            const BOTH: (bool, bool) = (true, true);
            const LEFT: (bool, bool) = (true, false);
            const RIGHT: (bool, bool) = (false, true);
            const NONE: (bool, bool) = (false, false);

            // union two `T`s, and determine which of two possible display hints should be kept
            let (t, (keep1, keep2)) = match (&*t1, &*t2) {
                // dynamic eclipses everything else
                (&T::Dynamic(dyn1), &T::Dynamic(dyn2)) => {
                    let dyn = dyn1.union(dyn2);
                    (T::Dynamic(dyn), (dyn1 == dyn, dyn2 == dyn))
                },
                (&T::Dynamic(dyn), _) => (T::Dynamic(dyn), LEFT),
                (_, &T::Dynamic(dyn)) => (T::Dynamic(dyn), RIGHT),

                // top eclipses everything else except for dynamic and oops
                (&T::All, &T::All) => (T::All, BOTH),
                (&T::All, _) => (T::All, LEFT),
                (_, &T::All) => (T::All, RIGHT),

                (&T::None, &T::None) => (T::None, BOTH),
                (&T::None, ty) => (ty.clone().into_send(), LEFT),
                (ty, &T::None) => (ty.clone().into_send(), RIGHT),

                (&T::Boolean,  &T::Boolean)  => (T::Boolean, BOTH),
                (&T::Boolean,  &T::True)     => (T::Boolean, LEFT),
                (&T::Boolean,  &T::False)    => (T::Boolean, LEFT),
                (&T::True,     &T::Boolean)  => (T::Boolean, RIGHT),
                (&T::False,    &T::Boolean)  => (T::Boolean, RIGHT),
                (&T::True,     &T::True)     => (T::True, BOTH),
                (&T::True,     &T::False)    if !explicit => (T::Boolean, NONE),
                (&T::False,    &T::True)     if !explicit => (T::Boolean, NONE),
                (&T::False,    &T::False)    => (T::False, BOTH),
                (&T::Thread,   &T::Thread)   => (T::Thread, BOTH),
                (&T::UserData, &T::UserData) => (T::UserData, BOTH),

                (&T::Number,  &T::Number)  => (T::Number, BOTH),
                (&T::Integer, &T::Number)  => (T::Number, RIGHT),
                (&T::Int(_),  &T::Number)  => (T::Number, RIGHT),
                (&T::Number,  &T::Integer) => (T::Number, LEFT),
                (&T::Number,  &T::Int(_))  => (T::Number, LEFT),
                (&T::Integer, &T::Integer) => (T::Integer, BOTH),
                (&T::Int(_),  &T::Integer) => (T::Integer, RIGHT),
                (&T::Integer, &T::Int(_))  => (T::Integer, LEFT),
                (&T::String,  &T::String)  => (T::String, BOTH),
                (&T::Str(_),  &T::String)  => (T::String, RIGHT),
                (&T::String,  &T::Str(_))  => (T::String, LEFT),

                (&T::Int(a), &T::Int(b)) if a == b => (T::Int(a), BOTH),
                (&T::Int(_), &T::Int(_)) if !explicit => (T::Integer, NONE),

                (&T::Str(ref a), &T::Str(ref b)) if a == b => {
                    (T::Str(Cow::Owned((**a).to_owned())), BOTH)
                },
                (&T::Str(_), &T::Str(_)) if !explicit => (T::String, NONE),

                (&T::Class(a), &T::Class(b)) if a == b => (T::Class(a), BOTH),
                (&T::Class(_), &T::Class(_)) if !explicit => return Err(ctx.gen_report()),

                // tables cannot be unioned except when one operand is a record and another is
                // a supertype of that record. otherwise (including the case of two records)
                // they should be equal, so records can be seemingly unioned due to row extension
                (&T::Tables(ref a), &T::Tables(ref b)) => {
                    let u = a.union(b, explicit, ctx)?;
                    let keep = (u == **a, u == **b);
                    (T::Tables(Cow::Owned(u)), keep)
                },

                // functions cannot be unioned at all and unequal function always errors
                (&T::Functions(ref a), &T::Functions(ref b)) => {
                    a.assert_eq(b, ctx)?;
                    (T::Functions(Cow::Owned(a.clone().into_owned())), BOTH)
                },

                // unresolved type variables should be equal to each other to be unioned
                (&T::TVar(a), &T::TVar(b)) => {
                    ctx.assert_tvar_eq_tvar(a, b)?;
                    (T::TVar(a), BOTH)
                },
                (&T::TVar(a), b) => {
                    // TODO check if b has no tvars etc
                    ctx.assert_tvar_eq(a, &Ty::new(b.clone().into_send()))?;
                    (T::TVar(a), BOTH)
                },
                (a, &T::TVar(b)) => {
                    // TODO check if a has no tvars etc
                    ctx.assert_tvar_eq(b, &Ty::new(a.clone().into_send()))?;
                    (T::TVar(b), BOTH)
                },

                (a, b) => {
                    let mut a_ = Unioned::from(&a, ctx)?;
                    let mut b_ = Unioned::from(&b, ctx)?;

                    // unions have their own display hints, so we have to copy
                    // selfhint and otherhint to newly constructed Unioned.
                    // a and b are not tvars, so there are no other display hints
                    // we have to take care of (otherwise we need some checks...).
                    if let Some(hint) = selfhint {
                        // but we cannot include hints including ? or tags.
                        if hint.includes_nil != Nil::Noisy && hint.includes_tag.is_none() {
                            a_.add_display_hint(a.flags(), &hint.name);
                        }
                    }
                    if let Some(hint) = otherhint {
                        if hint.includes_nil != Nil::Noisy && hint.includes_tag.is_none() {
                            b_.add_display_hint(b.flags(), &hint.name);
                        }
                    }

                    let u = a_.union(&b_, explicit, ctx)?.simplify();
                    let keep = (u == *a, u == *b);
                    (u, keep)
                },
            };

            // calculate display hints retained.
            // - selfhint/otherhint shadows hint1/hint2 when given
            // - hints not marked as kept are ignored no matter they come from
            // - conflicting hints are discarded together
            let mut selfhint = selfhint.map(Cow::Borrowed).or(hint1.map(Cow::Owned));
            let mut otherhint = otherhint.map(Cow::Borrowed).or(hint2.map(Cow::Owned));
            if !keep1 { selfhint = None; }
            if !keep2 { otherhint = None; }
            let hint = match (selfhint, otherhint) {
                (Some(hint1), Some(hint2)) => {
                    if *hint1 == *hint2 { Some(hint1) } else { None }
                },
                (Some(hint1), None) => Some(hint1),
                (None, Some(hint2)) => Some(hint2),
                (None, None) => None,
            };

            // adjust for non-T parts and display hints
            match (niltag1, niltag2, hint) {
                (Some((nil1, tag1)), Some((nil2, tag2)), hint) => {
                    // both have non-T parts and display hints, only add them when they are same
                    let mut inner = TyInner::new(t, nil1.union(nil2));
                    if tag1 == tag2 { inner.set_tag(tag1); }
                    inner.set_display_hint(hint.map(|hint| Box::new(hint.into_owned())));
                    Ok(Err(Ty { inner: Box::new(inner) }))
                },
                (Some((nil, tag)), None, hint) | (None, Some((nil, tag)), hint) => {
                    let mut inner = TyInner::new(t, nil);
                    inner.set_tag(tag);
                    inner.set_display_hint(hint.map(|hint| Box::new(hint.into_owned())));
                    Ok(Err(Ty { inner: Box::new(inner) }))
                },
                (None, None, Some(hint)) => {
                    let mut inner = TyInner::new(t, Nil::Silent);
                    inner.set_display_hint(Some(Box::new(hint.into_owned())));
                    Ok(Err(Ty { inner: Box::new(inner) }))
                }
                (None, None, None) => Ok(Ok(t)),
            }
        })().map_err(|r: TypeReport| r.cannot_union(Origin::T, self, other, explicit, ctx))
    }

    fn union<'b>(&self, other: &T<'b>, explicit: bool,
                 ctx: &mut TypeContext) -> TypeResult<result::Result<T<'static>, Ty>> {
        self.union_with_hints(None, other, None, explicit, ctx)
    }
}

impl<'a, 'b> Lattice<T<'b>> for T<'a> {
    fn assert_sub(&self, other: &T<'b>, ctx: &mut TypeContext) -> TypeResult<()> {
        debug!("asserting a constraint {:?} (T) <: {:?} (T)", *self, *other);

        (|| {
            let ok = match (self, other) {
                (&T::Dynamic(_), _) | (_, &T::Dynamic(_)) => true,

                (_, &T::All) => true,

                (&T::None, _) => true,
                (_, &T::None) => false,

                (&T::Boolean,  &T::Boolean)  => true,
                (&T::True,     &T::Boolean)  => true,
                (&T::True,     &T::True)     => true,
                (&T::False,    &T::Boolean)  => true,
                (&T::False,    &T::False)    => true,
                (&T::Thread,   &T::Thread)   => true,
                (&T::UserData, &T::UserData) => true,

                (&T::Number,     &T::Number)     => true,
                (&T::Integer,    &T::Number)     => true,
                (&T::Int(_),     &T::Number)     => true,
                (&T::Integer,    &T::Integer)    => true,
                (&T::Int(_),     &T::Integer)    => true,
                (&T::Int(a),     &T::Int(b))     => a == b,
                (&T::String,     &T::String)     => true,
                (&T::Str(_),     &T::String)     => true,
                (&T::Str(ref a), &T::Str(ref b)) => *a == *b,

                (&T::Tables(ref a),    &T::Tables(ref b))    => return a.assert_sub(b, ctx),
                (&T::Functions(ref a), &T::Functions(ref b)) => return a.assert_sub(b, ctx),

                (&T::Class(Class::Prototype(a)), &T::Class(Class::Prototype(b))) => {
                    a == b // prototypes are NOT compatible to each other!
                },
                (&T::Class(Class::Instance(a)), &T::Class(Class::Instance(b))) => {
                    ctx.is_subclass_of(a, b)
                },

                (&T::Union(ref a), &T::Union(ref b)) => return a.assert_sub(b, ctx),
                (&T::Union(_), &T::TVar(b)) => {
                    // do NOT try to split `T|U <: x` into `T <: x AND U <: x` if possible
                    return ctx.assert_tvar_sup(b, &Ty::new(self.clone().into_send()));
                },
                (&T::Union(ref a), b) => {
                    // a1 \/ a2 <: b === a1 <: b AND a2 <: b
                    return a.visit(T_ALL, |i| i.assert_sub(b, ctx));
                },

                (a, &T::Union(ref b)) => return a.assert_sub(&**b, ctx),

                (&T::TVar(a), &T::TVar(b)) => return a.assert_sub(&b, ctx),
                (a, &T::TVar(b)) => return ctx.assert_tvar_sup(b, &Ty::new(a.clone().into_send())),
                (&T::TVar(a), b) => return ctx.assert_tvar_sub(a, &Ty::new(b.clone().into_send())),

                (_, _) => false,
            };

            if ok { Ok(()) } else { Err(ctx.gen_report()) }
        })().map_err(|r: TypeReport| r.not_sub(Origin::T, self, other, ctx))
    }

    fn assert_eq(&self, other: &T<'b>, ctx: &mut TypeContext) -> TypeResult<()> {
        debug!("asserting a constraint {:?} (T) = {:?} (T)", *self, *other);

        (|| {
            let ok = match (self, other) {
                (&T::Dynamic(_), _) | (_, &T::Dynamic(_)) => true,

                (&T::All,  &T::All)  => true,
                (&T::None, &T::None) => true,

                (&T::Boolean,  &T::Boolean)  => true,
                (&T::True,     &T::True)     => true,
                (&T::False,    &T::False)    => true,
                (&T::Thread,   &T::Thread)   => true,
                (&T::UserData, &T::UserData) => true,

                (&T::Number,     &T::Number)     => true,
                (&T::Integer,    &T::Integer)    => true,
                (&T::Int(a),     &T::Int(b))     => a == b,
                (&T::String,     &T::String)     => true,
                (&T::Str(ref a), &T::Str(ref b)) => *a == *b,

                (&T::Tables(ref a),    &T::Tables(ref b))    => return a.assert_eq(b, ctx),
                (&T::Functions(ref a), &T::Functions(ref b)) => return a.assert_eq(b, ctx),
                (&T::Class(a),         &T::Class(b))         => a == b,

                (&T::TVar(a), &T::TVar(b)) => return a.assert_eq(&b, ctx),
                (a, &T::TVar(b)) => return ctx.assert_tvar_eq(b, &Ty::new(a.clone().into_send())),
                (&T::TVar(a), b) => return ctx.assert_tvar_eq(a, &Ty::new(b.clone().into_send())),

                (a, &T::Union(ref b)) => return a.assert_eq(&**b, ctx),
                (&T::Union(ref _a), _b) => false, // XXX for now

                (_, _) => false,
            };

            if ok { Ok(()) } else { Err(ctx.gen_report()) }
        })().map_err(|r: TypeReport| r.not_eq(Origin::T, self, other, ctx))
    }
}

impl<'a, 'b> ops::BitOr<T<'b>> for T<'a> {
    type Output = T<'static>;
    fn bitor(self, rhs: T<'b>) -> T<'static> {
        let ty: TypeResult<_> = self.union(&rhs, false, &mut NoTypeContext);
        let ty: result::Result<T<'static>, Ty> = ty.expect("T | T failed");
        ty.expect("type variable should not exist in T | T")
    }
}

// not intended to be complete equality, but enough for testing
impl<'a, 'b> PartialEq<T<'b>> for T<'a> {
    fn eq(&self, other: &T<'b>) -> bool {
        match (self, other) {
            (&T::Dynamic(dyn1), &T::Dynamic(dyn2)) => dyn1 == dyn2,

            (&T::All,      &T::All)      => true,
            (&T::None,     &T::None)     => true,
            (&T::Boolean,  &T::Boolean)  => true,
            (&T::True,     &T::True)     => true,
            (&T::False,    &T::False)    => true,
            (&T::Thread,   &T::Thread)   => true,
            (&T::UserData, &T::UserData) => true,

            (&T::Number,     &T::Number)     => true,
            (&T::Integer,    &T::Integer)    => true,
            (&T::Int(a),     &T::Int(b))     => a == b,
            (&T::String,     &T::String)     => true,
            (&T::Str(ref a), &T::Str(ref b)) => *a == *b,

            (&T::Tables(ref a),    &T::Tables(ref b))    => *a == *b,
            (&T::Functions(ref a), &T::Functions(ref b)) => *a == *b,
            (&T::Class(a),         &T::Class(b))         => a == b,
            (&T::TVar(a),          &T::TVar(b))          => a == b,
            (&T::Union(ref a),     &T::Union(ref b))     => a == b,

            (_, _) => false,
        }
    }
}

impl<'a> Display for T<'a> {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        match *self {
            T::Dynamic(Dyn::User) => write!(f, "WHATEVER"),
            T::Dynamic(Dyn::Oops) => {
                match &st.locale[..] {
                    "ko" => write!(f, "<오류>"),
                    _    => write!(f, "<error>"),
                }
            },

            T::All      => write!(f, "any"),
            T::None     => {
                match &st.locale[..] {
                    "ko" => write!(f, "<불가능한 타입>"),
                    _    => write!(f, "<impossible type>"),
                }
            },
            T::Boolean  => write!(f, "boolean"),
            T::True     => write!(f, "true"),
            T::False    => write!(f, "false"),
            T::Thread   => write!(f, "thread"),
            T::UserData => write!(f, "userdata"),

            T::Number     => write!(f, "number"),
            T::Integer    => write!(f, "integer"),
            T::Int(v)     => write!(f, "{}", v),
            T::String     => write!(f, "string"),
            T::Str(ref s) => write!(f, "{:?}", s),

            T::TVar(tv) => {
                if st.is_tvar_seen(tv) {
                    return write!(f, "<...>");
                } else {
                    let ret = if let Some(t) = st.context.get_tvar_exact_type(tv) {
                        fmt::Display::fmt(&t.display(st), f)
                    } else {
                        match &st.locale[..] {
                            "ko" => write!(f, "<알 수 없는 타입>"),
                            _    => write!(f, "<unknown type>"),
                        }
                    };
                    st.unmark_tvar(tv);
                    ret
                }
            },

            T::Tables(ref tab)      => fmt::Display::fmt(&tab.display(st), f),
            T::Functions(ref func)  => fmt::Display::fmt(&func.display(st), f),
            T::Class(c)             => fmt::Display::fmt(&c.display(st), f),
            T::Union(ref u)         => fmt::Display::fmt(&u.display(st), f),
        }
    }
}

impl<'a> fmt::Debug for T<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            T::Dynamic(Dyn::User) => write!(f, "WHATEVER"),
            T::Dynamic(Dyn::Oops) => write!(f, "<error>"),

            T::All      => write!(f, "any"),
            T::None     => write!(f, "<bottom>"),
            T::Boolean  => write!(f, "boolean"),
            T::True     => write!(f, "true"),
            T::False    => write!(f, "false"),
            T::Thread   => write!(f, "thread"),
            T::UserData => write!(f, "userdata"),

            T::Number     => write!(f, "number"),
            T::Integer    => write!(f, "integer"),
            T::Int(v)     => write!(f, "{}", v),
            T::String     => write!(f, "string"),
            T::Str(ref s) => write!(f, "{:?}", s),

            T::Tables(ref tab)     => fmt::Debug::fmt(tab, f),
            T::Functions(ref func) => fmt::Debug::fmt(func, f),
            T::Class(ref c)        => fmt::Debug::fmt(c, f),
            T::TVar(ref tv)        => fmt::Debug::fmt(tv, f),
            T::Union(ref u)        => fmt::Debug::fmt(u, f),
        }
    }
}

/// Different kinds of `nil`.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Nil {
    /// `nil` is assumed to be present, but the assumption is not checked.
    Silent,

    /// `nil` is assumed to be present, and the assumption gets checked
    /// (e.g. `integer?` doesn't support arithmetic operations).
    Noisy,

    /// `nil` is assumed to be absent.
    Absent,
}

impl Nil {
    pub fn with_nil(&self) -> Nil {
        match *self {
            Nil::Silent => Nil::Silent,
            Nil::Noisy => Nil::Noisy,
            Nil::Absent => Nil::Silent,
        }
    }

    pub fn without_nil(&self) -> Nil {
        match *self {
            Nil::Silent => Nil::Silent,
            Nil::Noisy => Nil::Silent,
            Nil::Absent => Nil::Absent,
        }
    }

    pub fn union(&self, other: Nil) -> Nil {
        match (*self, other) {
            (Nil::Noisy, _) | (_, Nil::Noisy) => Nil::Noisy,
            (Nil::Silent, _) | (_, Nil::Silent) => Nil::Silent,
            (Nil::Absent, Nil::Absent) => Nil::Absent,
        }
    }

    pub fn is_sub(&self, other: Nil) -> bool {
        match (*self, other) {
            (Nil::Noisy, Nil::Absent) => false,
            (_, _) => true,
        }
    }

    pub fn is_eq(&self, other: Nil) -> bool {
        match (*self, other) {
            (Nil::Absent, Nil::Noisy) => false,
            (Nil::Noisy, Nil::Absent) => false,
            (_, _) => true,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct DisplayHint {
    // these two fields determine what part of `nil` and `tag` in `TyInner` are
    // included in the type represented by the hint, and thus should not appear.
    includes_nil: Nil,
    includes_tag: Option<Tag>,

    name: DisplayName,
}

// XXX probably the layout can be improved a lot
#[derive(Clone, PartialEq)]
struct TyInner {
    ty: T<'static>,
    nil: Nil,
    tag: Option<Tag>,
    display_hint: Option<Box<DisplayHint>>,
}

impl TyInner {
    fn new(ty: T<'static>, nil: Nil) -> TyInner {
        TyInner { ty: ty, nil: nil, tag: None, display_hint: None }
    }

    fn nil(&self) -> Nil { self.nil }
    fn set_nil(&mut self, nil: Nil) { self.nil = nil; }

    fn ty(&self) -> &T<'static> { &self.ty }
    fn ty_mut(&mut self) -> &mut T<'static> { &mut self.ty }

    fn remap_ty<F>(&mut self, f: F)
        where F: FnOnce(T<'static>) -> T<'static>
    {
        let ty = mem::replace(&mut self.ty, T::None);
        self.ty = f(ty);
    }

    fn remap_ty_and_hint<F>(&mut self, f: F)
        where F: for<'a> FnOnce(&'a T<'static>) -> Cow<'a, T<'static>>
    {
        if let Cow::Owned(newty) = f(&self.ty) {
            self.ty = newty;
            self.display_hint = None;
        }
    }

    fn remap_ty_and_hint_res<E, F>(&mut self, f: F) -> result::Result<(), E>
        where F: for<'a> FnOnce(&'a T<'static>) -> result::Result<Cow<'a, T<'static>>, E>
    {
        if let Cow::Owned(newty) = f(&self.ty)? {
            self.ty = newty;
            self.display_hint = None;
        }
        Ok(())
    }

    fn tag(&self) -> Option<Tag> { self.tag }
    fn set_tag(&mut self, tag: Option<Tag>) { self.tag = tag; }

    fn display_hint(&self) -> Option<&DisplayHint> {
        self.display_hint.as_ref().map(|hint| &**hint)
    }

    fn set_display_hint(&mut self, disp: Option<Box<DisplayHint>>) {
        self.display_hint = disp;
    }

    fn set_display_hint_with_name(&mut self, disp: Option<DisplayName>) {
        self.display_hint = disp.map(|name| Box::new(DisplayHint {
            includes_nil: self.nil,
            includes_tag: self.tag,
            name: name
        }));
    }

    fn union_nil(&mut self, nil: Nil) {
        self.nil = self.nil.union(nil);
    }
    fn union_tag(&mut self, tag: Option<Tag>) {
        self.tag = if self.tag == tag { tag } else { None };
    }

    fn unwrap_ty(self) -> T<'static> { self.ty }
}

/// A value type, which is also a sort of pointer (to handle recursively defined `T`).
///
/// The full value type consists of a non-nilable portion (`T`), the `nil` (`Nil`),
/// an optional type tag (`Tag`) and an optional display name (`DisplayName`).
#[derive(Clone, PartialEq)]
pub struct Ty {
    inner: Box<TyInner>,
}

impl Ty {
    pub fn dummy() -> Ty {
        Ty { inner: Box::new(TyInner::new(T::dummy(), Nil::Silent)) }
    }

    pub fn silent_nil() -> Ty {
        Ty { inner: Box::new(TyInner::new(T::None, Nil::Silent)) }
    }

    pub fn noisy_nil() -> Ty {
        Ty { inner: Box::new(TyInner::new(T::None, Nil::Noisy)) }
    }

    pub fn new(ty: T<'static>) -> Ty {
        Ty { inner: Box::new(TyInner::new(ty, Nil::Silent)) }
    }

    pub fn from_kind(kind: &Spanned<Kind>, resolv: &mut TypeResolver) -> Result<Ty> {
        let slot_from_slotkind = |slotkind: &SlotKind, resolv: &mut TypeResolver| -> Result<Slot> {
            let ty = Ty::from_kind(&slotkind.kind, resolv)?;
            let flex = F::from(slotkind.modf);
            Ok(Slot::new(flex, ty))
        };

        let ty = match *kind.base {
            K::Oops              => Ty::new(T::Dynamic(Dyn::Oops)), // typically from a parser error
            K::Dynamic           => Ty::new(T::Dynamic(Dyn::User)),
            K::Any               => Ty::new(T::All),
            K::Nil               => Ty::new(T::None),
            K::Boolean           => Ty::new(T::Boolean),
            K::Number            => Ty::new(T::Number),
            K::Integer           => Ty::new(T::Integer),
            K::String            => Ty::new(T::String),
            K::Table             => Ty::new(T::Tables(Cow::Owned(Tables::All))),
            K::Function          => Ty::new(T::Functions(Cow::Owned(Functions::All))),
            K::Thread            => Ty::new(T::Thread),
            K::UserData          => Ty::new(T::UserData),
            K::Named(ref name)   => resolv.ty_from_name(name)?,
            K::WithNil(ref k)    => Ty::from_kind(k, resolv)?.or_nil(Nil::Noisy),
            K::WithoutNil(ref k) => Ty::from_kind(k, resolv)?.or_nil(Nil::Absent),
            // XXX think about the possibility of nil? and nil! more

            K::Error(..) => {
                resolv.error(kind, m::UnsupportedErrorType {}).done()?;
                Ty::new(T::Dynamic(Dyn::Oops))
            },

            K::BooleanLit(b) => {
                Ty::new(T::Union(Cow::Owned(Unioned::explicit_bool(b))))
            },
            K::IntegerLit(v) => {
                Ty::new(T::Union(Cow::Owned(Unioned::explicit_int(v))))
            },
            K::StringLit(ref s) => {
                Ty::new(T::Union(Cow::Owned(Unioned::explicit_str(s.to_owned()))))
            },

            K::EmptyTable => {
                let rvar = resolv.context_mut().gen_rvar();
                resolv.context_mut().assert_rvar_closed(rvar.clone()).expect(
                    "cannot make a fresh row variable not extensible"
                );
                Ty::new(T::Tables(Cow::Owned(Tables::Fields(rvar))))
            },

            K::Record(ref fields, extensible) => {
                // while the parser checks for duplicates, AST does not actually prevent them
                let mut newfields = Vec::new();
                let mut seen = HashMap::new(); // value denotes the first span
                for &(ref name, ref slotkind) in fields {
                    let slot = slot_from_slotkind(&slotkind.base, resolv)?;
                    match seen.entry(name.base.clone()) {
                        hash_map::Entry::Occupied(e) => {
                            resolv.error(name.span,
                                         m::DuplicateFieldNameInRec {
                                             name: &Name::from(name.base.clone())
                                         })
                                  .note(*e.get(), m::FirstFieldNameInRec {})
                                  .done()?;
                        }
                        hash_map::Entry::Vacant(e) => {
                            e.insert(name.span);
                            newfields.push((name.base.clone().into(), slot));
                        }
                    }
                }
                let rvar = resolv.context_mut().gen_rvar();
                resolv.context_mut().assert_rvar_includes(rvar.clone(), &newfields).expect(
                    "cannot insert disjoint fields into a fresh row variable"
                );
                if !extensible {
                    resolv.context_mut().assert_rvar_closed(rvar.clone()).expect(
                        "cannot make a fresh row variable not extensible"
                    );
                }
                Ty::new(T::Tables(Cow::Owned(Tables::Fields(rvar))))
            }

            K::Tuple(ref fields) => {
                let mut newfields = Vec::new();
                for (i, slotkind) in fields.iter().enumerate() {
                    let key = Key::Int(i as i32 + 1);
                    let slot = slot_from_slotkind(slotkind, resolv)?;
                    newfields.push((key, slot));
                }
                let rvar = resolv.context_mut().gen_rvar();
                resolv.context_mut().assert_rvar_includes(rvar.clone(), &newfields).expect(
                    "cannot insert disjoint fields into a fresh row variable"
                );
                // tuples are always not extensible
                resolv.context_mut().assert_rvar_closed(rvar.clone()).expect(
                    "cannot make a fresh row variable not extensible"
                );
                Ty::new(T::Tables(Cow::Owned(Tables::Fields(rvar))))
            },

            K::Array(ref v) => {
                let slot = slot_from_slotkind(v, resolv)?;
                Ty::new(T::Tables(Cow::Owned(Tables::Array(slot))))
            },

            K::Map(ref k, ref v) => {
                let key = Ty::from_kind(k, resolv)?.without_nil();
                let slot = slot_from_slotkind(v, resolv)?;
                Ty::new(T::Tables(Cow::Owned(Tables::Map(key, slot))))
            },

            K::Func(ref func) => {
                let func = Function::from_kind(func, resolv)?;
                Ty::new(T::Functions(Cow::Owned(Functions::Simple(func))))
            }

            K::Union(ref kinds) => {
                assert!(!kinds.is_empty());
                // put (inaccurate) spans to report correctly
                let mut ty = Ty::from_kind(&kinds[0], resolv)?.with_loc(kind);
                for k in &kinds[1..] {
                    let t = Ty::from_kind(k, resolv)?.with_loc(k);
                    match ty.union(&t, true, resolv.context_mut()) {
                        Ok(t) => {
                            ty = t.with_loc(kind);
                        }
                        Err(r) => {
                            resolv.error(kind, m::UnsupportedUnionTypeSpec {})
                                  .report_types(r, TypeReportHint::None)
                                  .done()?;
                            return Ok(Ty::dummy());
                        }
                    }
                }
                ty.base
            }

            K::Attr(ref kind, ref attr) => {
                let mut ty = Ty::from_kind(kind, resolv)?;
                // None is simply ignored, `Tag::from` has already reported the error
                if let Some(tag) = Tag::from(attr, resolv)? {
                    if ty.inner.tag().is_some() {
                        resolv.warn(attr, m::DuplicateAttr { ty: ty.display(resolv.context()) })
                              .done()?;
                    } else {
                        ty.inner.set_tag(Some(tag));
                    }
                }
                ty
            }
        };

        Ok(ty)
    }

    pub fn nil(&self) -> Nil {
        self.inner.nil()
    }

    pub fn or_nil(mut self, nil: Nil) -> Ty {
        self.inner.set_nil(nil);
        self
    }

    pub fn with_nil(mut self) -> Ty {
        let nil = self.inner.nil().with_nil();
        self.inner.set_nil(nil);
        self
    }

    pub fn without_nil(mut self) -> Ty {
        let nil = self.inner.nil().without_nil();
        self.inner.set_nil(nil);
        self
    }

    pub fn union_nil(mut self, nil: Nil) -> Ty {
        self.inner.union_nil(nil);
        self
    }

    // used for sequence and row relations:
    // if this returns false, the other type in given position can't be omitted
    pub fn can_omit(&self) -> bool {
        self.nil() == Nil::Noisy
    }

    pub fn tag(&self) -> Option<Tag> {
        self.inner.tag()
    }

    pub fn with_tag<T: Into<Option<Tag>>>(mut self, tag: T) -> Ty {
        self.inner.set_tag(tag.into());
        self
    }

    pub fn and_display(mut self, disp: DisplayName) -> Ty {
        // update if there is no hint already set, or the new display can override the old hint
        if self.inner.display_hint().map_or(true, |hint| disp.can_override(&hint.name)) {
            self.inner.set_display_hint_with_name(Some(disp.clone()));

            // also update the union if any, but only when there are no other hints for union
            if let T::Union(ref mut u) = *self.inner.ty_mut() {
                if u.display_hints.is_empty() {
                    let flags = u.flags();
                    u.to_mut().display_hints.push((flags, disp));
                }
            }
        }

        self
    }

    pub fn truthy(mut self) -> Ty {
        self.inner.remap_ty_and_hint(|t| t.truthy());
        let nil = self.inner.nil().without_nil();
        self.inner.set_nil(nil);
        self
    }

    pub fn falsy(mut self) -> Ty {
        self.inner.remap_ty_and_hint(|t| t.falsy());
        self
    }

    pub fn is_dynamic(&self)  -> bool { self.flags().is_dynamic() }
    pub fn is_integral(&self) -> bool { self.flags().is_integral() }
    pub fn is_numeric(&self)  -> bool { self.flags().is_numeric() }
    pub fn is_stringy(&self)  -> bool { self.flags().is_stringy() }
    pub fn is_tabular(&self)  -> bool { self.flags().is_tabular() }
    pub fn is_callable(&self) -> bool { self.flags().is_callable() }
    pub fn is_truthy(&self)   -> bool { self.flags().is_truthy() }
    pub fn is_falsy(&self)    -> bool { self.flags().is_falsy() }

    pub fn as_string(&self) -> Option<&Str> {
        if self.inner.nil() == Nil::Noisy { None } else { self.inner.ty().as_string() }
    }

    pub fn as_integer(&self) -> Option<i32> {
        if self.inner.nil() == Nil::Noisy { None } else { self.inner.ty().as_integer() }
    }

    pub fn coerce(mut self) -> Ty {
        self.inner.remap_ty_and_hint(|t| t.coerce());
        self
    }

    pub fn generalize(mut self, ctx: &mut TypeContext) -> Ty {
        self.inner.remap_ty(|t| t.generalize(ctx));
        // this will only alter tvars/rvars, so display hint doesn't change
        self
    }

    pub fn flags(&self) -> Flags {
        let mut flags = self.inner.ty().flags();
        if self.inner.nil() == Nil::Noisy {
            flags |= T_NOISY_NIL;
        }
        flags
    }

    pub fn filter_by_flags(mut self, flags: Flags, ctx: &mut TypeContext) -> TypeResult<Ty> {
        self.inner.remap_ty_and_hint_res(|t| t.filter_by_flags(flags, ctx))?;
        if !flags.contains(T_NOISY_NIL) {
            let nil = self.inner.nil().without_nil();
            self.inner.set_nil(nil);
        }
        Ok(self)
    }

    fn display_hint(&self) -> Option<&DisplayHint> {
        self.inner.display_hint()
    }

    fn display_repr(&self, st: &DisplayState,
                    f: &fmt::Formatter) -> (Nil, Option<Tag>, Option<&DisplayName>) {
        let mut nil = self.inner.nil();
        let mut tag = self.inner.tag();
        let mut name = None;

        // we should ignore display hints at the top level if requested (by `{:0}`).
        // the request is required because we may want to inspect the actual type for named types.
        if st.is_top_level() && f.sign_aware_zero_pad() {
            return (nil, tag, name);
        }

        if let Some(hint) = self.inner.display_hint() {
            // if the hint is of a variable, only apply the hint when
            // the type appears inside other types *and* the type is complex enough
            if let DisplayName::Var(_) = hint.name {
                let complex = match *self.inner.ty() {
                    T::Tables(_) | T::Union(_) => true,
                    _ => false,
                };
                if st.is_top_level() || !complex {
                    return (nil, tag, name);
                }
            }

            if tag == hint.includes_tag {
                tag = None;
            }

            // since non-silent nils are "operators", they overwrite original nils.
            // (e.g. if `T` is `string?`, `T!` is `string!`)
            // so unless they are identical the value of `nil` should be retained.
            if hint.includes_nil == nil {
                nil = Nil::Silent;
            }

            name = Some(&hint.name);
        }

        (nil, tag, name)
    }

    pub fn unwrap(self) -> T<'static> {
        self.inner.unwrap_ty()
    }
}

impl Dummy for Ty {
    fn dummy() -> Ty { Self::dummy() }
}

impl<'a> From<T<'a>> for Ty {
    fn from(ty: T<'a>) -> Ty {
        Ty { inner: Box::new(TyInner::new(ty.into_send(), Nil::Silent)) }
    }
}

impl ops::Deref for Ty {
    type Target = T<'static>;
    fn deref(&self) -> &T<'static> { self.inner.ty() }
}

impl ops::DerefMut for Ty {
    fn deref_mut(&mut self) -> &mut T<'static> { self.inner.ty_mut() }
}

fn tag_is_sub(lhs: Option<Tag>, rhs: Option<Tag>) -> bool {
    match (lhs, rhs) {
        // some tag requires the subtyping, so if any operand has such tag
        // and the tag doesn't match bail out
        (Some(ltag), Some(rtag)) =>
            ltag == rtag || !(ltag.needs_subtype() || rtag.needs_subtype()),

        (None, Some(rtag)) => !rtag.needs_subtype(),

        // every tagged types are subtypes of the original type
        (_, None) => true,
    }
}

fn tag_is_eq(lhs: Option<Tag>, rhs: Option<Tag>) -> bool {
    match (lhs, rhs) {
        // some tag requires the subtyping, so if any operand has such tag
        // and the tag doesn't match bail out
        (Some(ltag), Some(rtag)) =>
            ltag == rtag || !(ltag.needs_subtype() || rtag.needs_subtype()),

        (Some(ltag), None) => !ltag.needs_subtype(),
        (None, Some(rtag)) => !rtag.needs_subtype(),

        (None, None) => true,
    }
}

macro_rules! define_ty_impls {
    ($(impl[$($param:tt)*] $l:ident: $lhs:ty, $r:ident: $rhs:ty {
        origin = $origin:expr;
        text = $ltext:expr, $rtext:expr;
        ty = $lty:expr, $rty:expr;
        tag = $ltag:expr, $rtag:expr;
        nil = $lnil:expr, $rnil:expr;
        display_hint = $lhint:expr, $rhint:expr;
        as_is = $lhs_as_is:expr, $rhs_as_is:expr;
        without_nil = $lhs_without_nil:expr, _;
        union_tag = $union_tag:expr;
        union_nil = $union_nil:expr;
    })*) => ($(
        impl<$($param)*> Union<$rhs> for $lhs {
            type Output = Ty;

            fn union(&self, other: &$rhs, explicit: bool,
                     ctx: &mut TypeContext) -> TypeResult<Ty> {
                trace!(concat!("calculating an {} union of {:?} (", $ltext, ") and {:?} (",
                               $rtext, ")"),
                       if explicit { "explicit" } else { "implicit" }, *self, *other);

                let $l = self;
                let $r = other;
                let mut ty = match $lty.union_with_hints($lhint, $rty, $rhint, explicit, ctx) {
                    Ok(Ok(t)) => Ty { inner: Box::new(TyInner::new(t, Nil::Absent)) },
                    Ok(Err(ty)) => ty,
                    Err(r) => return Err(r.cannot_union($origin, self, other, explicit, ctx)),
                };
                ty.inner.union_nil($union_nil);
                ty.inner.union_tag($union_tag);
                Ok(ty)
            }
        }

        impl<$($param)*> Lattice<$rhs> for $lhs {
            fn assert_sub(&self, other: &$rhs, ctx: &mut TypeContext) -> TypeResult<()> {
                debug!(concat!("asserting a constraint {:?} (", $ltext, ") <: {:?} (", $rtext, ")"),
                       self, other);

                (|| {
                    let $l = self;
                    let $r = other;

                    if !tag_is_sub($ltag, $rtag) {
                        return Err(ctx.gen_report());
                    }

                    match ($lty, $rty) {
                        // Dynamic and All always contain nil, so handled separately here
                        (&T::Dynamic(_), _) | (_, &T::Dynamic(_)) | (_, &T::All) => Ok(()),

                        // for remaining cases handle nils as follows:
                        //
                        // ty1 | nil1 <: ty2 | nil2
                        // ==> (ty1 <: ty2 | nil2) AND (nil1 <: ty2 | nil2)
                        // ==> ((ty1 <: ty2) OR (ty1 <: nil2)) AND ((nil1 <: ty2) OR (nil1 <: nil2))
                        //
                        // it should be noted that `ty <: nil` is false unless ty is bottom
                        // (ignored here because we don't make use of bottom type in general),
                        // and `nil <: ty` is equivalent to `nil <: Nil::Absent`, which translate to
                        // `nil != Nil::Noisy` because it's the only case that <: on nils can fail.

                        // if both ty1 and ty2 are type variables v1 and v2, we have two conditions:
                        // - `(v1 <: v2) AND (nil1 <: v2)`
                        // - `(v1 <: v2) AND (nil1 <: nil2)`
                        // (as noted above `ty1 <: nil2` is ignored as it's same to `ty1 = bottom`.)
                        //
                        // unlike other cases, `nil1 <: v2` cannot be combined to `v1 <: v2`.
                        // so we add a constraint `v1 <: v2` first, and depending on `nil1 <: nil2`,
                        // we conditionally add another constraint `nil1 <: v2`.
                        //
                        // the second constraint is currently not easy to satisfy due to the lack of
                        // proper constraint solver, but it is correct per se because an inability
                        // to solve constraints does not make a wrong code checked anyway.
                        (&T::TVar(v1), &T::TVar(v2)) => {
                            ctx.assert_tvar_sub_tvar(v1, v2)?;
                            let lnil = $lnil;
                            if !lnil.is_sub($rnil) {
                                ctx.assert_tvar_sup(v2, &Ty::new(T::None).or_nil(lnil))?;
                            }
                            Ok(())
                        },

                        // if ty2 is a type variable v2 and ty1 is not,
                        // first check for `nil1 <: nil2` and if true `ty1 <: v2` is
                        // a sufficient condition and added to constraints.
                        // otherwise `nil1 <: v2` is also required, so a combined condition
                        // `ty1 | nil1 <: v2` is added to constraints instead (easier to solve).
                        (_, &T::TVar(v2)) => {
                            if $lnil.is_sub($rnil) {
                                // nil can be removed from the constraint
                                ctx.assert_tvar_sup(v2, $lhs_without_nil)
                            } else {
                                ctx.assert_tvar_sup(v2, $lhs_as_is)
                            }
                        },

                        // if ty1 is a type variable v1 and ty2 is not,
                        // `v1 <: ty2 | nil2` is added to constraints and
                        // `nil1 <: nil2` gets checked. `nil1 <: ty2` is ignored as
                        // when `nil1 != Nil::Noisy` the condition `nil1 <: nil2` is always true.
                        (&T::TVar(v1), _) => {
                            if $lnil.is_sub($rnil) {
                                ctx.assert_tvar_sub(v1, $rhs_as_is)
                            } else {
                                Err(ctx.gen_report())
                            }
                        },

                        // finally, unless ty1 or ty2 are type variables, any condition involving
                        // both tyX and nilY cannot be true (see above) so just check for
                        // `ty1 <: ty2` and `nil1 <: nil2`.
                        (ty1, ty2) => {
                            if $lnil.is_sub($rnil) {
                                ty1.assert_sub(ty2, ctx)
                            } else {
                                Err(ctx.gen_report())
                            }
                        },
                    }
                })().map_err(|r: TypeReport| r.not_sub($origin, self, other, ctx))
            }

            fn assert_eq(&self, other: &$rhs, ctx: &mut TypeContext) -> TypeResult<()> {
                debug!(concat!("asserting a constraint {:?} (", $ltext, ") = {:?} (", $rtext, ")"),
                       self, other);

                (|| {
                    let $l = self;
                    let $r = other;

                    match ($lty, $rty) {
                        // Dynamic and All always contain nil, so handled separately here
                        (&T::Dynamic(_), _) | (_, &T::Dynamic(_)) | (&T::All, &T::All) => {
                            if !tag_is_eq($ltag, $rtag) {
                                return Err(ctx.gen_report());
                            }
                            Ok(())
                        },

                        // conditions here are derived from the conditions of assert_sub,
                        // applied twice (as T <: U and U <: T implies T = U)

                        (&T::TVar(v1), &T::TVar(v2)) => {
                            if !tag_is_eq($ltag, $rtag) {
                                return Err(ctx.gen_report());
                            }

                            // unlike assert_sub, if nil differs we cannot easily derive
                            // constraints for variables, so we require nils to be equal
                            if $lnil.is_eq($rnil) {
                                ctx.assert_tvar_eq_tvar(v1, v2)
                            } else {
                                Err(ctx.gen_report())
                            }
                        },

                        (_, &T::TVar(v2)) => {
                            // do not check tags, v2 will get tags if lhs had them
                            if $lnil.is_eq($rnil) {
                                // nil can be removed from the constraint
                                ctx.assert_tvar_eq(v2, $lhs_without_nil)
                            } else {
                                ctx.assert_tvar_eq(v2, $lhs_as_is)
                            }
                        },

                        (&T::TVar(v1), _) => {
                            // do not check tags, v1 will get tags if rhs had them
                            if $lnil.is_eq($rnil) {
                                ctx.assert_tvar_eq(v1, $rhs_as_is)
                            } else {
                                Err(ctx.gen_report())
                            }
                        },

                        (ty1, ty2) => {
                            if !tag_is_eq($ltag, $rtag) {
                                return Err(ctx.gen_report());
                            }

                            if $lnil.is_eq($rnil) {
                                ty1.assert_eq(ty2, ctx)
                            } else {
                                Err(ctx.gen_report())
                            }
                        },
                    }
                })().map_err(|r: TypeReport| r.not_eq($origin, self, other, ctx))
            }
        }
    )*)
}

define_ty_impls! {
    impl['a] lhs: T<'a>, rhs: Ty {
        origin = Origin::TTy;
        text = "T w/o nil", "Ty";
        ty  = lhs,         rhs.inner.ty();
        tag = None,        rhs.inner.tag();
        nil = Nil::Absent, rhs.inner.nil();
        display_hint = None, rhs.inner.display_hint();
        as_is       = &Ty::new(lhs.clone().into_send()).or_nil(Nil::Absent), rhs;
        without_nil = &Ty::new(lhs.clone().into_send()).or_nil(Nil::Absent), _;
        union_tag = None;
        union_nil = rhs.inner.nil();
    }

    impl['a] lhs: Ty, rhs: T<'a> {
        origin = Origin::TTy;
        text = "Ty", "T w/o nil";
        ty  = lhs.inner.ty(),  rhs;
        tag = lhs.inner.tag(), None;
        nil = lhs.inner.nil(), Nil::Absent;
        display_hint = lhs.inner.display_hint(), None;
        as_is       = lhs, &Ty::new(rhs.clone().into_send()).or_nil(Nil::Absent);
        without_nil = &lhs.clone().without_nil(), _;
        union_tag = None;
        union_nil = lhs.inner.nil();
    }

    impl[] lhs: Ty, rhs: Ty {
        origin = Origin::Ty;
        text = "Ty", "Ty";
        ty  = lhs.inner.ty(),  rhs.inner.ty();
        tag = lhs.inner.tag(), rhs.inner.tag();
        nil = lhs.inner.nil(), rhs.inner.nil();
        display_hint = lhs.inner.display_hint(), rhs.inner.display_hint();
        as_is       = lhs, rhs;
        without_nil = &lhs.clone().without_nil(), _;
        union_tag = {
            let (ltag, rtag) = (lhs.inner.tag(), rhs.inner.tag());
            if ltag == rtag { ltag } else { None }
        };
        union_nil = lhs.inner.nil().union(rhs.inner.nil());
    }
}

impl Display for Ty {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        let ty = self.inner.ty();
        let (nil, tag, name) = self.display_repr(st, f);
        let nil = if f.alternate() { nil.with_nil() } else { nil };

        if let Some(tag) = tag {
            write!(f, "[{}] ", tag.display(st))?;
        }

        if let Some(name) = name {
            write!(f, "{}", name.display(st))?;
        } else {
            match (ty, nil) {
                // nil-derived types have their own representations
                (&T::None, Nil::Silent) => return write!(f, "nil"),
                (&T::None, Nil::Noisy) => return write!(f, "nil"),

                (_, _) => ty.fmt_displayed(f, st)?,
            }
        }

        match nil {
            Nil::Silent => Ok(()),
            Nil::Noisy => write!(f, "?"),
            Nil::Absent => write!(f, "!"),
        }
    }
}

impl fmt::Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ty = self.inner.ty();
        let nil = self.inner.nil();
        let tag = self.inner.tag();
        let nil = if f.alternate() { nil.with_nil() } else { nil };

        if let Some(tag) = tag {
            write!(f, "[{:?}] ", tag)?;
        }

        let nil_repr = |nil| {
            match nil {
                Nil::Silent => "",
                Nil::Noisy => "?",
                Nil::Absent => "!",
            }
        };

        match (ty, nil) {
            // nil-derived types have their own representations
            (&T::None, Nil::Silent) => write!(f, "nil")?,
            (&T::None, Nil::Noisy) => write!(f, "nil!")?,

            (_, _) => {
                fmt::Debug::fmt(ty, f)?;
                write!(f, "{}", nil_repr(nil))?;
            }
        }

        if let Some(hint) = self.inner.display_hint() {
            write!(f, " <hint: ")?;
            if let Some(tag) = hint.includes_tag {
                write!(f, "[{:?}] ", tag)?;
            }
            write!(f, "_{} = {:?}>", nil_repr(hint.includes_nil), hint.name)?;
        }

        Ok(())
    }
}

#[cfg(test)]
#[allow(unused_variables, dead_code)]
mod tests {
    use kailua_diag::Locale;
    use kailua_syntax::Str;
    use std::borrow::Cow;
    use ty::{Lattice, Union, TypeContext, NoTypeContext, F, Slot, Tag};
    use env::{Types, DummyClassProvider};
    use super::*;

    macro_rules! hash {
        ($($k:ident = $v:expr),*) => (vec![$((s(stringify!($k)), $v)),*])
    }

    fn s(x: &str) -> Str { Str::from(x.as_bytes().to_owned()) }
    fn os(x: &str) -> Cow<'static, Str> { Cow::Owned(Str::from(x.as_bytes().to_owned())) }
    fn just(t: T) -> Slot { Slot::new(F::Just, Ty::from(t)) }
    fn var(t: T) -> Slot { Slot::new(F::Var, Ty::from(t)) }
    fn cnst(t: T) -> Slot { Slot::new(F::Const, Ty::from(t)) }
    fn nil(t: T) -> Ty { Ty::from(t).or_nil(Nil::Noisy) }

    macro_rules! check_base {
        (@explicitness explicit) => (true);
        (@explicitness implicit) => (false);

        ($l:expr, $r:expr; [$e:tt]=_) => ({
            let mut types = Types::new(Locale::dummy(), Box::new(DummyClassProvider));
            let actualunion = $l.union(&$r, check_base!(@explicitness $e), &mut types);
            if actualunion.is_ok() {
                panic!("{:?} | {:?} ({}) = expected Err(_), actual {:?}",
                       $l, $r, stringify!($e), actualunion);
            }
        });

        ($l:expr, $r:expr; [$e:tt]=$u:expr) => ({
            let union = $u;
            let mut types = Types::new(Locale::dummy(), Box::new(DummyClassProvider));
            let actualunion = $l.union(&$r, check_base!(@explicitness $e), &mut types);
            if actualunion.is_err() || *actualunion.as_ref().unwrap() != union {
                panic!("{:?} | {:?} ({}) = expected Ok({:?}), actual {:?}",
                       $l, $r, stringify!($e), union, actualunion);
            }
            actualunion.unwrap()
        });

        ($l:expr, $r:expr; explicit=_, implicit=$ui:expr) => ({
            let left = $l;
            let right = $r;
            let eunion = check_base!(left, right; [explicit]=_);
            let iunion = check_base!(left, right; [implicit]=$ui);
            (left, right, eunion, iunion)
        });

        ($l:expr, $r:expr; explicit=$ue:expr, implicit=_) => ({
            let left = $l;
            let right = $r;
            let eunion = check_base!(left, right; [explicit]=$ue);
            let iunion = check_base!(left, right; [implicit]=_);
            (left, right, eunion, iunion)
        });

        ($l:expr, $r:expr; explicit=$ue:expr, implicit=$ui:expr) => ({
            let left = $l;
            let right = $r;
            let eunion = check_base!(left, right; [explicit]=$ue);
            let iunion = check_base!(left, right; [implicit]=$ui);
            (left, right, eunion, iunion)
        });

        ($l:expr, $r:expr; _) => ({
            let left = $l;
            let right = $r;
            check_base!(&left, &right; [explicit]=_);
            check_base!(&left, &right; [implicit]=_);
            (left, right, ())
        });

        ($l:expr, $r:expr; $u:expr) => ({
            let (left, right, eunion, iunion) = check_base!($l, $r; explicit=$u, implicit=$u);
            if eunion != iunion {
                panic!("{:?} | {:?} = explicit {:?}, implicit {:?}",
                       left, right, eunion, iunion);
            }
            (left, right, eunion)
        });
    }

    #[test]
    fn test_union_t() {
        macro_rules! check {
            ($l:expr, $r:expr; explicit=_, implicit=$ui:expr) =>
                (check_base!($l, $r; explicit=_, implicit=Ok($ui)));
            ($l:expr, $r:expr; explicit=$ue:expr, implicit=_) =>
                (check_base!($l, $r; explicit=Ok($ue), implicit=_));
            ($l:expr, $r:expr; explicit=$ue:expr, implicit=$ui:expr) =>
                (check_base!($l, $r; explicit=Ok($ue), implicit=Ok($ui)));
            ($l:expr, $r:expr; _) => (check_base!($l, $r; _));
            ($l:expr, $r:expr; $u:expr) => (check_base!($l, $r; Ok($u)));
        }

        // dynamic & top vs. everything else
        check!(T::Dynamic(Dyn::Oops), T::Dynamic(Dyn::Oops); T::Dynamic(Dyn::Oops));
        check!(T::Dynamic(Dyn::Oops), T::Dynamic(Dyn::User); T::Dynamic(Dyn::Oops));
        check!(T::Dynamic(Dyn::User), T::Dynamic(Dyn::Oops); T::Dynamic(Dyn::Oops));
        check!(T::Dynamic(Dyn::User), T::Dynamic(Dyn::User); T::Dynamic(Dyn::User));
        check!(T::Dynamic(Dyn::User), T::Integer; T::Dynamic(Dyn::User));
        /*
        check!(T::tuple(vec![var(T::Integer), cnst(T::Boolean)]), T::Dynamic(Dyn::User);
               T::Dynamic(Dyn::User));
        */
        check!(T::All, T::Boolean; T::All);
        check!(T::Dynamic(Dyn::User), T::All; T::Dynamic(Dyn::User));
        check!(T::All, T::All; T::All);

        // integer literals
        check!(T::Integer, T::Number; T::Number);
        check!(T::Number, T::Integer; T::Number);
        check!(T::Number, T::Number; T::Number);
        check!(T::Integer, T::Integer; T::Integer);
        check!(T::Int(3), T::Int(3); T::Int(3));
        check!(T::Int(3), T::Number; T::Number);
        check!(T::Integer, T::Int(3); T::Integer);
        check!(T::Int(3), T::Int(4);
               explicit=T::ints(vec![3, 4]), implicit=T::Integer);
        check!(T::ints(vec![3, 4]), T::Int(3); T::ints(vec![3, 4]));
        check!(T::Int(5), T::ints(vec![3, 4]);
               explicit=T::ints(vec![3, 4, 5]), implicit=T::Integer);
        check!(T::ints(vec![3, 4]), T::ints(vec![5, 4, 7]);
               explicit=T::ints(vec![3, 4, 5, 7]), implicit=T::Integer);
        check!(T::ints(vec![3, 4, 5]), T::ints(vec![2, 3, 4]);
               explicit=T::ints(vec![2, 3, 4, 5]), implicit=T::Integer);
        check!(T::ints(vec![3, 4, 5]), T::ints(vec![3, 4, 5]); T::ints(vec![3, 4, 5]));

        // string literals
        check!(T::String, T::Str(os("hello")); T::String);
        check!(T::Str(os("hello")), T::String; T::String);
        check!(T::Str(os("hello")), T::Str(os("hello")); T::Str(os("hello")));
        check!(T::Str(os("hello")), T::Str(os("goodbye"));
               explicit=T::strs(vec![s("hello"), s("goodbye")]), implicit=T::String);
        check!(T::Str(os("hello")), T::strs(vec![s("goodbye")]);
               explicit=T::strs(vec![s("hello"), s("goodbye")]), implicit=T::String);
        check!(T::strs(vec![s("hello"), s("goodbye")]), T::Str(os("goodbye"));
               T::strs(vec![s("hello"), s("goodbye")]));
        check!(T::strs(vec![s("hello"), s("goodbye")]),
               T::strs(vec![s("what"), s("goodbye")]);
               explicit=T::strs(vec![s("hello"), s("goodbye"), s("what")]), implicit=T::String);
        check!(T::strs(vec![s("a"), s("b"), s("c")]),
               T::strs(vec![s("b"), s("c"), s("d")]);
               explicit=T::strs(vec![s("a"), s("b"), s("c"), s("d")]), implicit=T::String);
        check!(T::strs(vec![s("x"), s("y"), s("z")]),
               T::strs(vec![s("x"), s("y"), s("z")]);
               T::strs(vec![s("x"), s("y"), s("z")]));

        // tables
        check!(T::table(), T::table(); T::table());
        check!(T::table(), T::array(just(T::Integer)); T::table());
        check!(T::array(var(T::Integer)), T::table(); T::table());
        check!(T::table(), T::array(cnst(T::Integer)); T::table());
        check!(T::array(just(T::Integer)), T::array(just(T::Integer));
               T::array(just(T::Integer)));
        check!(T::array(var(T::Integer)), T::array(var(T::Integer));
               T::array(var(T::Integer)));
        check!(T::array(cnst(T::Integer)), T::array(cnst(T::Integer));
               T::array(cnst(T::Integer)));
        check!(T::array(just(T::Int(3))), T::array(just(T::Int(4))); _);
        check!(T::array(cnst(T::Int(3))), T::array(cnst(T::Int(4))); _);
        check!(T::array(var(T::Int(3))), T::array(var(T::Int(4))); _);
        check!(T::array(var(T::Int(3))), T::array(just(T::Int(4))); _);
        /*
        check!(T::tuple(vec![just(T::Integer), just(T::String)]),
               T::tuple(vec![just(T::Integer), just(T::String)]);
               T::tuple(vec![just(T::Integer), just(T::String)]));
        check!(T::tuple(vec![just(T::Integer), just(T::String)]),
               T::tuple(vec![just(T::Number), just(T::Dynamic(Dyn::User)), just(T::Boolean)]);
               _);
        check!(T::tuple(vec![just(T::Integer), just(T::String)]),
               T::tuple(vec![just(T::Number), just(T::Boolean), just(T::Dynamic(Dyn::User))]);
               _);
        check!(T::tuple(vec![var(T::Integer), cnst(T::String)]),
               T::tuple(vec![cnst(T::String), just(T::Number), var(T::Boolean)]);
               _);
        check!(T::tuple(vec![cnst(T::Integer)]),
               T::tuple(vec![cnst(T::Number), cnst(T::String)]);
               _);
        check!(T::tuple(vec![just(T::Integer), var(T::String), cnst(T::Boolean)]),
               T::empty_table();
               _);
        check!(T::record(hash![foo=just(T::Integer), bar=just(T::String)]),
               T::record(hash![foo=just(T::Integer), bar=just(T::String)]);
               T::record(hash![foo=just(T::Integer), bar=just(T::String)]));
        check!(T::record(hash![foo=just(T::Integer), bar=just(T::String)]),
               T::record(hash![quux=just(T::Boolean)]);
               _);
        check!(T::record(hash![foo=just(T::Int(3)), bar=just(T::String)]),
               T::record(hash![foo=just(T::Int(4))]);
               _);
        check!(T::record(hash![foo=just(T::Integer), bar=just(T::Number),
                                    quux=just(T::array(just(T::Dynamic(Dyn::User))))]),
               T::record(hash![foo=just(T::Number), bar=just(T::String),
                                    quux=just(T::array(just(T::Boolean)))]);
               _);
        check!(T::record(hash![foo=just(T::Int(3)), bar=just(T::Number)]),
               T::map(T::String, just(T::Integer));
               _);
        */
        check!(T::map(T::String, just(T::Integer)),
               T::map(T::String, just(T::Integer));
               T::map(T::String, just(T::Integer)));
        check!(T::map(T::String, cnst(T::Integer)),
               T::map(T::String, just(T::Integer));
               _);
        /*
        check!(T::array(just(T::Integer)),
               T::tuple(vec![just(T::String)]);
               _);
        */
        check!(T::map(T::Str(os("wat")), just(T::Integer)),
               T::map(T::String, just(T::Int(42)));
               _);
        check!(T::array(just(T::Number)),
               T::map(T::Dynamic(Dyn::User), just(T::Integer));
               _);
        /*
        check!(T::empty_table(),
               T::empty_table();
               T::empty_table());
        check!(T::empty_table(),
               T::array(just(T::Integer));
               _);
        */
        check!(T::array(just(T::Integer)),
               T::array(just(T::Integer));
               T::array(just(T::Integer)));

        // others
        check!(T::Thread, T::Thread; T::Thread);
        check!(T::UserData, T::UserData; T::UserData);
        check!(T::All, T::UserData; T::All);
        check!(T::Thread, T::Dynamic(Dyn::User); T::Dynamic(Dyn::User));

        // general unions
        check!(T::True, T::True; T::True);
        check!(T::False, T::False; T::False);
        check!(T::True, T::False; explicit=_, implicit=T::Boolean);
        check!(T::Int(3) | T::String, T::Str(os("wat")) | T::Int(4);
               explicit = T::ints(vec![3, 4]) | T::String,
               implicit = T::Integer | T::String);
        let m1 = T::map(T::String, Slot::new(F::Just, Ty::from(T::Integer)));
        let m2 = T::map(T::String, Slot::new(F::Just, Ty::from(T::Integer).or_nil(Nil::Noisy)));
        m1.assert_eq(&m2, &mut NoTypeContext).unwrap();
    }

    #[test]
    fn test_union_ty() {
        macro_rules! check {
            ($l:expr, $r:expr; explicit=_, implicit=$ui:expr) =>
                (check_base!($l, $r; explicit=_, implicit=$ui));
            ($l:expr, $r:expr; explicit=$ue:expr, implicit=_) =>
                (check_base!($l, $r; explicit=$ue, implicit=_));
            ($l:expr, $r:expr; explicit=$ue:expr, implicit=$ui:expr) =>
                (check_base!($l, $r; explicit=$ue, implicit=$ui));
            ($l:expr, $r:expr; _) => (check_base!($l, $r; _));
            ($l:expr, $r:expr; $u:expr) => (check_base!($l, $r; $u));
        }

        check!(nil(T::Int(3)), nil(T::Int(4));
               explicit=nil(T::ints(vec![3, 4])),
               implicit=nil(T::Integer));
        check!(nil(T::Int(3) | T::UserData), nil(T::Thread | T::Int(4));
               explicit=nil(T::Thread | T::ints(vec![3, 4]) | T::UserData),
               implicit=nil(T::Thread | T::Integer | T::UserData));
        check!(nil(T::ints(vec![3, 5])), T::Int(4) | T::String;
               explicit=nil(T::String | T::ints(vec![3, 4, 5])),
               implicit=nil(T::String | T::Integer));
    }

    #[test]
    fn test_sub() {
        /*
        assert_eq!(T::record(hash![foo=just(T::Int(3)), bar=just(T::Integer)]).assert_sub(
                       &T::map(T::Str(os("foo")) | T::Str(os("bar")), just(T::Number)),
                       &mut NoTypeContext),
                   Ok(()));
        */

        // tag subtyping
        let str = Ty::new(T::String);
        let substr = Ty::new(T::String).with_tag(Tag::_Subtype);
        let nosubstr = Ty::new(T::String).with_tag(Tag::_NoSubtype);
        let nosubtrue = Ty::new(T::True).with_tag(Tag::_NoSubtype);
        let nosubtrueorstr = Ty::new(T::True | T::String).with_tag(Tag::_NoSubtype);
        let nosubboolorstr = Ty::new(T::Boolean | T::String).with_tag(Tag::_NoSubtype);
        assert!(substr.assert_sub(&substr, &mut NoTypeContext).is_ok());
        assert!(substr.assert_sub(&str, &mut NoTypeContext).is_ok());
        assert!(str.assert_sub(&substr, &mut NoTypeContext).is_err());
        assert!(nosubstr.assert_sub(&nosubstr, &mut NoTypeContext).is_ok());
        assert!(nosubstr.assert_sub(&str, &mut NoTypeContext).is_ok());
        assert!(str.assert_sub(&nosubstr, &mut NoTypeContext).is_ok());
        assert!(nosubstr.assert_sub(&substr, &mut NoTypeContext).is_err());
        assert!(substr.assert_sub(&nosubstr, &mut NoTypeContext).is_err());
        assert!(nosubtrue.assert_sub(&nosubstr, &mut NoTypeContext).is_err());
        assert!(nosubstr.assert_sub(&nosubtrueorstr, &mut NoTypeContext).is_ok());
        assert!(nosubtrue.assert_sub(&nosubtrueorstr, &mut NoTypeContext).is_ok());
        assert!(nosubboolorstr.assert_sub(&nosubtrueorstr, &mut NoTypeContext).is_err());
        assert!(nosubtrueorstr.assert_sub(&nosubboolorstr, &mut NoTypeContext).is_ok());
        assert!(nosubboolorstr.assert_sub(&substr, &mut NoTypeContext).is_err());
        assert!(nosubboolorstr.assert_sub(&nosubboolorstr, &mut NoTypeContext).is_ok());

        let mut types = Types::new(Locale::dummy(), Box::new(DummyClassProvider));

        {
            let v1 = types.gen_tvar();
            // v1 <: integer
            assert!(T::TVar(v1).assert_sub(&T::Integer, &mut types).is_ok());
            // v1 <: integer
            assert!(T::TVar(v1).assert_sub(&T::Integer, &mut types).is_ok());
            // v1 <: integer AND v1 <: string (!)
            assert!(T::TVar(v1).assert_sub(&T::String, &mut types).is_err());
        }

        {
            let v1 = types.gen_tvar();
            let v2 = types.gen_tvar();
            // v1 <: v2
            assert!(T::TVar(v1).assert_sub(&T::TVar(v2), &mut types).is_ok());
            // v1 <: v2 <: string
            assert!(T::TVar(v2).assert_sub(&T::String, &mut types).is_ok());
            // v1 <: v2 <: string AND v1 <: integer (!)
            assert!(T::TVar(v1).assert_sub(&T::Integer, &mut types).is_err());
        }

        /*
        {
            let v1 = types.gen_tvar();
            let v2 = types.gen_tvar();
            let t1 = T::record(hash![a=just(T::Integer), b=just(T::TVar(v1))]);
            let t2 = T::record(hash![a=just(T::TVar(v2)), b=just(T::String), c=just(T::Boolean)]);
            // {a=just integer, b=just v1} <: {a=just v2, b=just string, c=just boolean}
            assert!(t1.assert_sub(&t2, &mut types).is_ok());
            // ... AND v1 <: string
            assert!(T::TVar(v1).assert_sub(&T::String, &mut types).is_ok());
            // ... AND v1 <: string AND v2 :> integer
            assert!(T::Integer.assert_sub(&T::TVar(v2), &mut types).is_ok());
            // {a=just integer, b=just v1} = {a=just v2, b=just string, c=just boolean} (!)
            assert!(t1.assert_eq(&t2, &mut types).is_err());
        }
        */

        {
            let v1 = types.gen_tvar();
            let tv1 = Ty::new(T::TVar(v1));
            let tv1nil = Ty::new(T::TVar(v1)).or_nil(Nil::Noisy);
            let intnil = Ty::new(T::Integer).or_nil(Nil::Noisy);
            // v1? <: nil?
            assert!(tv1nil.assert_sub(&intnil, &mut types).is_ok());
            // v1 <: nil?
            assert!(tv1.assert_sub(&intnil, &mut types).is_ok());
            // v1 :> nil?
            assert!(intnil.assert_sub(&tv1, &mut types).is_ok());
        }
    }

    #[test]
    fn test_eq() {
        // tag subtyping
        let str = Ty::new(T::String);
        let substr = Ty::new(T::String).with_tag(Tag::_Subtype);
        let nosubnil = Ty::new(T::String).with_tag(Tag::_NoSubtype);
        let nosubtrue = Ty::new(T::True).with_tag(Tag::_NoSubtype);
        let nosubtrueorstr = Ty::new(T::True | T::String).with_tag(Tag::_NoSubtype);
        let nosubboolorstr = Ty::new(T::Boolean | T::String).with_tag(Tag::_NoSubtype);
        assert!(substr.assert_eq(&substr, &mut NoTypeContext).is_ok());
        assert!(substr.assert_eq(&str, &mut NoTypeContext).is_err());
        assert!(str.assert_eq(&substr, &mut NoTypeContext).is_err());
        assert!(nosubnil.assert_eq(&nosubnil, &mut NoTypeContext).is_ok());
        assert!(nosubnil.assert_eq(&str, &mut NoTypeContext).is_ok());
        assert!(str.assert_eq(&nosubnil, &mut NoTypeContext).is_ok());
        assert!(nosubnil.assert_eq(&substr, &mut NoTypeContext).is_err());
        assert!(substr.assert_eq(&nosubnil, &mut NoTypeContext).is_err());
        assert!(nosubtrue.assert_eq(&nosubnil, &mut NoTypeContext).is_err());
        assert!(nosubnil.assert_eq(&nosubtrueorstr, &mut NoTypeContext).is_err());
        assert!(nosubtrue.assert_eq(&nosubtrueorstr, &mut NoTypeContext).is_err());
        assert!(nosubboolorstr.assert_eq(&nosubtrueorstr, &mut NoTypeContext).is_err());
        assert!(nosubtrueorstr.assert_eq(&nosubboolorstr, &mut NoTypeContext).is_err());
        assert!(nosubboolorstr.assert_eq(&substr, &mut NoTypeContext).is_err());
        assert!(nosubboolorstr.assert_eq(&nosubboolorstr, &mut NoTypeContext).is_ok());
    }
}

