use std::fmt;
use std::ops;
use std::mem;
use std::borrow::Cow;

use kailua_env::{Spanned, WithLoc};
use kailua_syntax::{K, Kind, SlotKind, Str};
use kailua_diag::Reporter;
use diag::CheckResult;
use super::{F, Slot};
use super::{TypeContext, NoTypeContext, TypeResolver, Lattice, Union, TySeq, Display};
use super::{Numbers, Strings, Key, Tables, Function, Functions, Unioned, TVar, Tag, Class};
use super::{error_not_sub, error_not_eq};
use super::flags::*;
use message as m;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Dyn {
    User, // user-generated dynamic type: WHATEVER
    Oops, // checker-generated dynamic type: <error type>
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

// a value type excluding nil (which is specially treated).
#[derive(Clone)]
pub enum T<'a> {
    Dynamic(Dyn),                       // dynamic type
    All,                                // any (top)
    None,                               // (bottom); possibly nil or nil!
    Boolean,                            // boolean
    True,                               // true
    False,                              // false
    Integer,                            // integer
    Number,                             // number
    String,                             // string
    Thread,                             // thread
    UserData,                           // userdata
    Int(i32),                           // an integer literal
    Str(Cow<'a, Str>),                  // a string literal
    Tables(Cow<'a, Tables>),            // table, ...
    Functions(Cow<'a, Functions>),      // function, ...
    Class(Class),                       // nominal type
    TVar(TVar),                         // type variable
    Union(Cow<'a, Unioned>),            // union types A | B | ...
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

    pub fn truthy(self) -> T<'a> {
        match self {
            T::Boolean => T::True,
            T::False => T::None,
            T::Union(u) => {
                if u.simple.intersects(U_FALSE) {
                    let mut u = u.into_owned();
                    u.simple.remove(U_FALSE);
                    u.simplify()
                } else {
                    T::Union(u)
                }
            },
            t => t,
        }
    }

    pub fn falsy(&self) -> T<'static> {
        match *self {
            T::Boolean | T::False => T::False,
            T::Union(ref u) if u.simple & U_FALSE != U_NONE => T::False,
            _ => T::None,
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

    // XXX for now
    pub fn is_referential(&self) -> bool { self.flags().is_tabular() }

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

    pub fn split_tvar(&self) -> (Option<TVar>, Option<T<'a>>) {
        match *self {
            T::TVar(tv) => (Some(tv), None),
            _ => (None, Some(self.clone())),
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

    pub fn filter_by_flags(self, flags: Flags, ctx: &mut TypeContext) -> CheckResult<T<'a>> {
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

        fn narrow_tvar(tvar: TVar, flags: Flags, ctx: &mut TypeContext) -> CheckResult<TVar> {
            let ubound = flags_to_ubound(flags);

            // make a type variable i such that i <: ubound and i <: tvar
            let i = ctx.gen_tvar();
            ctx.assert_tvar_sub_tvar(i, tvar)?;
            ctx.assert_tvar_sub(i, &Ty::new(ubound))?;

            Ok(i)
        }

        let flags_or_none = |bit, t| if flags.contains(bit) { t } else { T::None };

        match self {
            T::Dynamic(dyn) => Ok(T::Dynamic(dyn)),
            T::None => Ok(T::None),
            T::All => Ok(flags_to_ubound(flags)),
            T::Boolean => match flags & T_BOOLEAN {
                T_BOOLEAN => Ok(T::Boolean),
                T_TRUE => Ok(T::True),
                T_FALSE => Ok(T::False),
                _ => Ok(T::None),
            },
            T::Number => match flags & T_NUMBER {
                T_NUMBER | T_NONINTEGER => Ok(T::Number),
                T_INTEGER => Ok(T::Integer),
                _ => Ok(T::None),
            },
            T::Integer         => Ok(flags_or_none(T_INTEGER,  T::Integer)),
            T::Int(v)          => Ok(flags_or_none(T_INTEGER,  T::Int(v))),
            T::True            => Ok(flags_or_none(T_TRUE,     T::True)),
            T::False           => Ok(flags_or_none(T_FALSE,    T::False)),
            T::Thread          => Ok(flags_or_none(T_THREAD,   T::Thread)),
            T::UserData        => Ok(flags_or_none(T_USERDATA, T::UserData)),
            T::String          => Ok(flags_or_none(T_STRING,   T::String)),
            T::Str(s)          => Ok(flags_or_none(T_STRING,   T::Str(s))),
            T::Tables(tab)     => Ok(flags_or_none(T_TABLE,    T::Tables(tab))),
            T::Functions(func) => Ok(flags_or_none(T_FUNCTION, T::Functions(func))),
            T::Class(c)        => Ok(flags_or_none(T_TABLE,    T::Class(c))),

            T::TVar(tv) => {
                Ok(T::TVar(narrow_tvar(tv, flags, ctx)?))
            },
            T::Union(u) => {
                // compile a list of flags to remove, and only alter if there is any removal
                let removed = !flags & u.flags();
                if removed.is_empty() { return Ok(T::Union(u)); }

                let mut u = u.into_owned();
                let removed_simple = UnionedSimple::from_bits_truncate(removed.bits());
                if !removed_simple.is_empty() { u.simple &= !removed_simple; }
                if removed.intersects(T_NUMBER) {
                    let num = Cow::Owned(u.numbers.unwrap());
                    u.numbers = narrow_numbers(num, flags).map(|num| num.into_owned());
                }
                if removed.contains(T_STRING)   { u.strings   = None; }
                if removed.contains(T_TABLE)    { u.tables    = None; }
                if removed.contains(T_FUNCTION) { u.functions = None; }
                Ok(u.simplify())
            },
        }
    }
}

impl<'a> Union<Unioned> for T<'a> {
    type Output = Unioned;

    fn union(&self, other: &Unioned, explicit: bool,
             ctx: &mut TypeContext) -> CheckResult<Unioned> {
        Unioned::from(self)?.union(other, explicit, ctx)
    }
}

impl<'a> Lattice<Unioned> for T<'a> {
    // assumes that the Unioned itself has been simplified.
    fn assert_sub(&self, other: &Unioned, ctx: &mut TypeContext) -> CheckResult<()> {
        // try to match each component
        match *self {
            T::Dynamic(_) | T::None => return Ok(()),

            T::Boolean  => if other.simple.contains(U_BOOLEAN)  { return Ok(()); },
            T::True     => if other.simple.contains(U_TRUE)     { return Ok(()); },
            T::False    => if other.simple.contains(U_FALSE)    { return Ok(()); },
            T::Thread   => if other.simple.contains(U_THREAD)   { return Ok(()); },
            T::UserData => if other.simple.contains(U_USERDATA) { return Ok(()); },

            T::Number => match other.numbers {
                Some(Numbers::All) => { return Ok(()); }
                _ => {}
            },
            T::Integer => match other.numbers {
                Some(Numbers::All) | Some(Numbers::Int) => { return Ok(()); }
                _ => {}
            },
            T::Int(lhs) => match other.numbers {
                Some(Numbers::All) | Some(Numbers::Int) => { return Ok(()); }
                Some(Numbers::Some(ref rhs)) if rhs.contains(&lhs) => { return Ok(()); }
                Some(Numbers::One(rhs)) if lhs == rhs => { return Ok(()); }
                _ => {}
            },

            T::String => match other.strings {
                Some(Strings::All) => { return Ok(()); }
                _ => {}
            },
            T::Str(ref lhs) => match other.strings {
                Some(Strings::All) => { return Ok(()); }
                Some(Strings::Some(ref rhs)) if rhs.contains(lhs) => { return Ok(()); }
                Some(Strings::One(ref rhs)) if **lhs == *rhs => { return Ok(()); }
                _ => {}
            },

            T::Tables(ref lhs) =>
                if let Some(ref num) = other.tables { return lhs.assert_sub(num, ctx); },
            T::Functions(ref lhs) =>
                if let Some(ref num) = other.functions { return lhs.assert_sub(num, ctx); },
            T::Class(c) => if other.classes.contains(&c) { return Ok(()); },

            T::TVar(lhs) => {
                return ctx.assert_tvar_sub(lhs, &Ty::new(T::Union(Cow::Owned(other.clone()))));
            },

            T::Union(ref lhs) => return lhs.assert_sub(other, ctx),

            _ => {}
        }

        // the union sans type variable is not a subtype of self.
        // XXX we can try asserting an additional constraint to the union's type variable if any,
        // but for now we bail out
        error_not_sub(self, other)
    }

    // assumes that the Unioned itself has been simplified.
    fn assert_eq(&self, other: &Unioned, ctx: &mut TypeContext) -> CheckResult<()> {
        match *self {
            T::Dynamic(_) => return Ok(()),
            T::Union(ref lhs) => return lhs.assert_eq(other, ctx),
            _ => {}
        }

        error_not_eq(self, other)
    }
}

impl<'a> T<'a> {
    // not intended to be used in public, because it can result in either T or Ty
    fn union<'b>(&self, other: &T<'b>, explicit: bool,
                 ctx: &mut TypeContext) -> CheckResult<Result<T<'static>, Ty>> {
        fn resolve<'t, 'u>(t: &'t T<'u>,
                           ctx: &mut TypeContext) -> (Cow<'t, T<'u>>, Option<(Nil, Option<Tag>)>) {
            if let &T::TVar(tv) = t {
                if let Some(ty) = ctx.get_tvar_exact_type(tv) {
                    let nil = ty.nil();
                    let tag = ty.tag();
                    return (Cow::Owned(ty.unwrap()), Some((nil, tag)));
                }
            }
            (Cow::Borrowed(t), None)
        }

        // resolve type variables, which may result in Ty
        let (t1, niltag1) = resolve(self, ctx);
        let (t2, niltag2) = resolve(other, ctx);

        let t = match (&*t1, &*t2) {
            // dynamic eclipses everything else
            (&T::Dynamic(dyn1), &T::Dynamic(dyn2)) => T::Dynamic(dyn1.union(dyn2)),
            (&T::Dynamic(dyn), _) => T::Dynamic(dyn),
            (_, &T::Dynamic(dyn)) => T::Dynamic(dyn),

            // top eclipses everything else except for dynamic and oops
            (&T::All, _) => T::All,
            (_, &T::All) => T::All,

            (&T::None, ty) => ty.clone().into_send(),
            (ty, &T::None) => ty.clone().into_send(),

            (&T::Boolean,  &T::Boolean)  => T::Boolean,
            (&T::Boolean,  &T::True)     => T::Boolean,
            (&T::Boolean,  &T::False)    => T::Boolean,
            (&T::True,     &T::Boolean)  => T::Boolean,
            (&T::False,    &T::Boolean)  => T::Boolean,
            (&T::True,     &T::True)     => T::True,
            (&T::True,     &T::False)    => T::Boolean,
            (&T::False,    &T::True)     => T::Boolean,
            (&T::False,    &T::False)    => T::False,
            (&T::Thread,   &T::Thread)   => T::Thread,
            (&T::UserData, &T::UserData) => T::UserData,

            (&T::Number,     &T::Number)     => T::Number,
            (&T::Integer,    &T::Number)     => T::Number,
            (&T::Int(_),     &T::Number)     => T::Number,
            (&T::Number,     &T::Integer)    => T::Number,
            (&T::Number,     &T::Int(_))     => T::Number,
            (&T::Integer,    &T::Integer)    => T::Integer,
            (&T::Int(_),     &T::Integer)    => T::Integer,
            (&T::Integer,    &T::Int(_))     => T::Integer,
            (&T::String,     &T::String)     => T::String,
            (&T::Str(_),     &T::String)     => T::String,
            (&T::String,     &T::Str(_))     => T::String,

            (&T::Int(a), &T::Int(b)) if a == b => T::Int(a),
            (&T::Int(_), &T::Int(_)) if !explicit => T::Integer,

            (&T::Str(ref a), &T::Str(ref b)) if a == b => T::Str(Cow::Owned((**a).to_owned())),
            (&T::Str(_),     &T::Str(_))     if !explicit => T::String,

            (&T::Class(a), &T::Class(b)) if a == b => T::Class(a),
            (&T::Class(_), &T::Class(_)) if !explicit => {
                return Err("invalid implicit union".into());
            },

            // tables and functions cannot be `union`ed in any way and
            // any attempt to union different types of those kinds is an error
            (&T::Tables(ref a), &T::Tables(ref b)) => {
                a.assert_eq(b, ctx)?;
                T::Tables(Cow::Owned(a.clone().into_owned()))
            },
            (&T::Functions(ref a), &T::Functions(ref b)) => {
                a.assert_eq(b, ctx)?;
                T::Functions(Cow::Owned(a.clone().into_owned()))
            },

            // unresolved type variables should be equal to each other to be unioned
            (&T::TVar(a), &T::TVar(b)) => {
                ctx.assert_tvar_eq_tvar(a, b)?;
                T::TVar(a)
            },
            (&T::TVar(a), b) => {
                ctx.assert_tvar_eq(a, &Ty::new(b.clone().into_send()))?;
                T::TVar(a)
            },
            (a, &T::TVar(b)) => {
                ctx.assert_tvar_eq(b, &Ty::new(a.clone().into_send()))?;
                T::TVar(b)
            },

            (a, b) => Unioned::from(&a)?.union(&Unioned::from(&b)?, explicit, ctx)?.simplify(),
        };

        match (niltag1, niltag2) {
            (Some((nil1, tag1)), Some((nil2, tag2))) => {
                let nil = nil1.union(nil2);
                let tag = if tag1 == tag2 { tag1 } else { None };
                Ok(Err(Ty { inner: Box::new(TyInner::new(t, nil, tag)) }))
            },
            (Some((nil, tag)), None) | (None, Some((nil, tag))) => {
                Ok(Err(Ty { inner: Box::new(TyInner::new(t, nil, tag)) }))
            },
            (None, None) => Ok(Ok(t)),
        }
    }
}

impl<'a, 'b> Lattice<T<'b>> for T<'a> {
    fn assert_sub(&self, other: &T<'b>, ctx: &mut TypeContext) -> CheckResult<()> {
        debug!("asserting a constraint {:?} <: {:?}", *self, *other);

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

            // prototypes are NOT compatible to each other!
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
                return a.visit(|i| i.assert_sub(b, ctx));
            },

            (a, &T::Union(ref b)) => return a.assert_sub(&**b, ctx),

            (&T::TVar(a), &T::TVar(b)) => return a.assert_sub(&b, ctx),
            (a, &T::TVar(b)) => return ctx.assert_tvar_sup(b, &Ty::new(a.clone().into_send())),
            (&T::TVar(a), b) => return ctx.assert_tvar_sub(a, &Ty::new(b.clone().into_send())),

            (_, _) => false,
        };

        if ok { Ok(()) } else { error_not_sub(self, other) }
    }

    fn assert_eq(&self, other: &T<'b>, ctx: &mut TypeContext) -> CheckResult<()> {
        debug!("asserting a constraint {:?} = {:?}", *self, *other);

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

        if ok { Ok(()) } else { error_not_eq(self, other) }
    }
}

impl<'a, 'b> ops::BitOr<T<'b>> for T<'a> {
    type Output = T<'static>;
    fn bitor(self, rhs: T<'b>) -> T<'static> {
        let ty: CheckResult<_> = self.union(&rhs, false, &mut NoTypeContext);
        let ty: Result<T<'static>, Ty> = ty.expect("T | T failed");
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
    fn fmt_displayed(&self, f: &mut fmt::Formatter, ctx: &TypeContext) -> fmt::Result {
        match *self {
            T::Dynamic(Dyn::User) => write!(f, "WHATEVER"),
            T::Dynamic(Dyn::Oops) => write!(f, "<error type>"),

            T::All      => write!(f, "any"),
            T::None     => write!(f, "<impossible type>"),
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
                if let Some(t) = ctx.get_tvar_exact_type(tv) {
                    fmt::Display::fmt(&t.display(ctx), f)
                } else {
                    write!(f, "<unknown type>")
                }
            },

            T::Tables(ref tab)     => fmt::Display::fmt(&tab.display(ctx), f),
            T::Functions(ref func) => fmt::Display::fmt(&func.display(ctx), f),
            T::Class(c)            => ctx.fmt_class(c, f),
            T::Union(ref u)        => fmt::Display::fmt(&u.display(ctx), f),
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

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Nil {
    Silent, // nil can be present but not checked; risks runtime errors
    Noisy,  // nil can be present and checked
    Absent, // nil cannot be present
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

// XXX probably the layout can be improved a lot
#[derive(Clone, PartialEq)]
struct TyInner {
    ty: T<'static>,
    nil: Nil,
    tag: Option<Tag>,
}

impl TyInner {
    fn new(ty: T<'static>, nil: Nil, tag: Option<Tag>) -> TyInner {
        TyInner { ty: ty, nil: nil, tag: tag }
    }

    fn nil(&self) -> Nil { self.nil }
    fn set_nil(&mut self, nil: Nil) { self.nil = nil; }

    fn ty(&self) -> &T<'static> { &self.ty }
    fn ty_mut(&mut self) -> &mut T<'static> { &mut self.ty }

    fn remap_ty<F>(&mut self, f: F) where F: FnOnce(T<'static>) -> T<'static> {
        let ty = mem::replace(&mut self.ty, T::None);
        self.ty = f(ty);
    }

    fn remap_ty_res<E, F>(&mut self, f: F) -> Result<(), E>
            where F: FnOnce(T<'static>) -> Result<T<'static>, E> {
        let ty = mem::replace(&mut self.ty, T::None);
        self.ty = f(ty)?;
        Ok(())
    }

    fn tag(&self) -> Option<Tag> { self.tag }
    fn set_tag(&mut self, tag: Option<Tag>) { self.tag = tag; }

    fn union_nil(&mut self, nil: Nil) {
        self.nil = self.nil.union(nil);
    }
    fn union_tag(&mut self, tag: Option<Tag>) {
        self.tag = if self.tag == tag { tag } else { None };
    }

    fn unwrap_ty(self) -> T<'static> { self.ty }
}

// a "type pointer". nil is implicitly unioned to type.
#[derive(Clone, PartialEq)]
pub struct Ty {
    inner: Box<TyInner>,
}

impl Ty {
    pub fn dummy() -> Ty {
        Ty { inner: Box::new(TyInner::new(T::dummy(), Nil::Silent, None)) }
    }

    pub fn silent_nil() -> Ty {
        Ty { inner: Box::new(TyInner::new(T::None, Nil::Silent, None)) }
    }

    pub fn noisy_nil() -> Ty {
        Ty { inner: Box::new(TyInner::new(T::None, Nil::Noisy, None)) }
    }

    pub fn new(ty: T<'static>) -> Ty {
        Ty { inner: Box::new(TyInner::new(ty, Nil::Silent, None)) }
    }

    pub fn from_kind(kind: &Spanned<Kind>, resolv: &mut TypeResolver) -> CheckResult<Ty> {
        let slot_from_slotkind = |slotkind: &SlotKind,
                                  resolv: &mut TypeResolver| -> CheckResult<Slot> {
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
            K::BooleanLit(true)  => Ty::new(T::True),
            K::BooleanLit(false) => Ty::new(T::False),
            K::Number            => Ty::new(T::Number),
            K::Integer           => Ty::new(T::Integer),
            K::IntegerLit(v)     => Ty::new(T::Int(v)),
            K::String            => Ty::new(T::String),
            K::StringLit(ref s)  => Ty::new(T::Str(Cow::Owned(s.to_owned()))),
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

            K::EmptyTable => {
                Ty::new(T::Tables(Cow::Owned(Tables::Fields(resolv.context().gen_rvar()))))
            },

            K::Record(ref fields) => {
                let mut newfields = Vec::new();
                for &(ref name, ref slotkind) in fields {
                    let slot = slot_from_slotkind(&slotkind.base, resolv)?;
                    newfields.push((name.base.clone().into(), slot));
                }
                let rvar = resolv.context().gen_rvar();
                resolv.context().assert_rvar_includes(rvar.clone(), &newfields)?;
                Ty::new(T::Tables(Cow::Owned(Tables::Fields(rvar))))
            }

            K::Tuple(ref fields) => {
                let mut newfields = Vec::new();
                for (i, slotkind) in fields.iter().enumerate() {
                    let key = Key::Int(i as i32 + 1);
                    let slot = slot_from_slotkind(slotkind, resolv)?;
                    newfields.push((key, slot));
                }
                let rvar = resolv.context().gen_rvar();
                resolv.context().assert_rvar_includes(rvar.clone(), &newfields)?;
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
                let func = Function {
                    args: TySeq::from_kind_seq(&func.args, resolv)?,
                    returns: TySeq::from_kind_seq(&func.returns, resolv)?,
                };
                Ty::new(T::Functions(Cow::Owned(Functions::Simple(func))))
            }

            K::Union(ref kinds) => {
                assert!(!kinds.is_empty());
                // put (inaccurate) spans to report correctly
                let mut ty = Ty::from_kind(&kinds[0], resolv)?.with_loc(kind);
                for k in &kinds[1..] {
                    let t = Ty::from_kind(k, resolv)?.with_loc(k);
                    ty = ty.union(&t, true, resolv.context())?.with_loc(kind);
                }
                ty.base
            }

            K::Attr(ref kind, ref attr) => {
                let mut ty = Ty::from_kind(kind, resolv)?;
                // None is simply ignored, `Tag::from` has already reported the error
                if let Some(tag) = Tag::from(attr, resolv)? {
                    // XXX check for the duplicate tag
                    ty.inner.set_tag(Some(tag));
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

    pub fn tag(&self) -> Option<Tag> {
        self.inner.tag()
    }

    pub fn with_tag<T: Into<Option<Tag>>>(mut self, tag: T) -> Ty {
        self.inner.set_tag(tag.into());
        self
    }

    pub fn truthy(mut self) -> Ty {
        self.inner.remap_ty(|t| t.truthy());
        let nil = self.inner.nil().without_nil();
        self.inner.set_nil(nil);
        self
    }

    pub fn falsy(mut self) -> Ty {
        self.inner.remap_ty(|t| t.falsy());
        self
    }

    pub fn split_tvar(&self) -> (Option<TVar>, Option<Ty>) {
        let (tv, ty) = self.inner.ty().split_tvar();
        let ty = ty.map(|t| {
            Ty { inner: Box::new(TyInner::new(t, self.inner.nil(), self.inner.tag())) }
        });
        (tv, ty)
    }

    pub fn flags(&self) -> Flags {
        let mut flags = self.inner.ty().flags();
        if self.inner.nil() == Nil::Noisy {
            flags |= T_NOISY_NIL;
        }
        flags
    }

    pub fn filter_by_flags(mut self, flags: Flags, ctx: &mut TypeContext) -> CheckResult<Ty> {
        self.inner.remap_ty_res(|t| t.filter_by_flags(flags, ctx))?;
        if !flags.contains(T_NOISY_NIL) {
            let nil = self.inner.nil().without_nil();
            self.inner.set_nil(nil);
        }
        Ok(self)
    }

    pub fn unwrap(self) -> T<'static> {
        self.inner.unwrap_ty()
    }
}

impl<'a> From<T<'a>> for Ty {
    fn from(ty: T<'a>) -> Ty {
        Ty { inner: Box::new(TyInner::new(ty.into_send(), Nil::Silent, None)) }
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
        text = $ltext:expr, $rtext:expr;
        ty = $lty:expr, $rty:expr;
        tag = $ltag:expr, $rtag:expr;
        nil = $lnil:expr, $rnil:expr;
        as_is = $lhs_as_is:expr, $rhs_as_is:expr;
        without_nil = $lhs_without_nil:expr, _;
        union_tag = $union_tag:expr;
        union_nil = $union_nil:expr;
    })*) => ($(
        impl<$($param)*> Union<$rhs> for $lhs {
            type Output = Ty;

            fn union(&self, other: &$rhs, explicit: bool,
                     ctx: &mut TypeContext) -> CheckResult<Ty> {
                let $l = self;
                let $r = other;
                let mut ty = match $lty.union($rty, explicit, ctx)? {
                    Ok(t) => Ty { inner: Box::new(TyInner::new(t, Nil::Absent, None)) },
                    Err(ty) => ty,
                };
                ty.inner.union_nil($union_nil);
                ty.inner.union_tag($union_tag);
                Ok(ty)
            }
        }

        impl<$($param)*> Lattice<$rhs> for $lhs {
            fn assert_sub(&self, other: &$rhs, ctx: &mut TypeContext) -> CheckResult<()> {
                debug!(concat!("asserting a constraint {:?} (", $ltext, ") <: {:?} (", $rtext, ")"),
                       self, other);

                let $l = self;
                let $r = other;

                if !tag_is_sub($ltag, $rtag) {
                    return error_not_sub(self, other);
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
                    // (as noted above, `ty1 <: nil2` is ignored as it is same to `ty1 = bottom`.)
                    //
                    // unlike other cases, `nil1 <: v2` cannot be combined to `v1 <: v2`.
                    // so we add a constraint `v1 <: v2` first, and depending on `nil1 <: nil2`,
                    // we conditionally add another constraint `nil1 <: v2`.
                    //
                    // the second constraint is currently not easy to satisfy due to the lack of
                    // proper constraint solver, but it is correct per se because an inability to
                    // solve constraints does not make a wrong code checked anyway.
                    (&T::TVar(v1), &T::TVar(v2)) => {
                        ctx.assert_tvar_sub_tvar(v1, v2)?;
                        let lnil = $lnil;
                        if !lnil.is_sub($rnil) {
                            ctx.assert_tvar_sup(v2, &Ty::new(T::None).or_nil(lnil))?;
                        }
                        Ok(())
                    },

                    // if ty2 is a type variable v2 and ty1 is not, first check for `nil1 <: nil2`
                    // and if true `ty1 <: v2` is a sufficient condition and added to constraints.
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

                    // if ty1 is a type variable v1 and ty2 is not, `v1 <: ty2 | nil2` is added to
                    // constraints and `nil1 <: nil2` gets checked. `nil1 <: ty2` is ignored as
                    // when `nil1 != Nil::Noisy` the condition `nil1 <: nil2` is always true.
                    (&T::TVar(v1), _) => {
                        if $lnil.is_sub($rnil) {
                            ctx.assert_tvar_sub(v1, $rhs_as_is)
                        } else {
                            error_not_sub(self, other)
                        }
                    },

                    // finally, unless ty1 or ty2 are type variables, any condition involving
                    // both tyX and nilY cannot be true (see above) so just check for
                    // `ty1 <: ty2` and `nil1 <: nil2`.
                    (ty1, ty2) => {
                        if $lnil.is_sub($rnil) {
                            ty1.assert_sub(ty2, ctx)
                        } else {
                            error_not_sub(self, other)
                        }
                    },
                }
            }

            fn assert_eq(&self, other: &$rhs, ctx: &mut TypeContext) -> CheckResult<()> {
                debug!(concat!("asserting a constraint {:?} (", $ltext, ") = {:?} (", $rtext, ")"),
                       self, other);

                let $l = self;
                let $r = other;

                if !tag_is_eq($ltag, $rtag) {
                    return error_not_eq(self, other);
                }

                match ($lty, $rty) {
                    // Dynamic and All always contain nil, so handled separately here
                    (&T::Dynamic(_), _) | (_, &T::Dynamic(_)) | (&T::All, &T::All) => Ok(()),

                    // conditions here are derived from the conditions of assert_sub,
                    // applied twice (as T <: U and U <: T implies T = U)

                    (&T::TVar(v1), &T::TVar(v2)) => {
                        // unlike assert_sub, if nil differs we cannot easily derive
                        // constraints for variables, so we require nils to be equal
                        if $lnil.is_eq($rnil) {
                            ctx.assert_tvar_eq_tvar(v1, v2)
                        } else {
                            error_not_eq(self, other)
                        }
                    },

                    (_, &T::TVar(v2)) => {
                        if $lnil.is_eq($rnil) {
                            // nil can be removed from the constraint
                            ctx.assert_tvar_eq(v2, $lhs_without_nil)
                        } else {
                            ctx.assert_tvar_eq(v2, $lhs_as_is)
                        }
                    },

                    (&T::TVar(v1), _) => {
                        if $lnil.is_eq($rnil) {
                            ctx.assert_tvar_eq(v1, $rhs_as_is)
                        } else {
                            error_not_eq(self, other)
                        }
                    },

                    (ty1, ty2) => {
                        if $lnil.is_eq($rnil) {
                            ty1.assert_eq(ty2, ctx)
                        } else {
                            error_not_eq(self, other)
                        }
                    },
                }
            }
        }
    )*)
}

define_ty_impls! {
    impl['a] lhs: T<'a>, rhs: Ty {
        text = "T w/o nil", "Ty";
        ty  = lhs,         rhs.inner.ty();
        tag = None,        rhs.inner.tag();
        nil = Nil::Absent, rhs.inner.nil();
        as_is       = &Ty::new(lhs.clone().into_send()).or_nil(Nil::Absent), rhs;
        without_nil = &Ty::new(lhs.clone().into_send()).or_nil(Nil::Absent), _;
        union_tag = None;
        union_nil = rhs.inner.nil();
    }

    impl['a] lhs: Ty, rhs: T<'a> {
        text = "Ty", "T w/o nil";
        ty  = lhs.inner.ty(),  rhs;
        tag = lhs.inner.tag(), None;
        nil = lhs.inner.nil(), Nil::Absent;
        as_is       = lhs, &Ty::new(rhs.clone().into_send()).or_nil(Nil::Absent);
        without_nil = &lhs.clone().without_nil(), _;
        union_tag = None;
        union_nil = lhs.inner.nil();
    }

    impl[] lhs: Ty, rhs: Ty {
        text = "Ty", "Ty";
        ty  = lhs.inner.ty(),  rhs.inner.ty();
        tag = lhs.inner.tag(), rhs.inner.tag();
        nil = lhs.inner.nil(), rhs.inner.nil();
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
    fn fmt_displayed(&self, f: &mut fmt::Formatter, ctx: &TypeContext) -> fmt::Result {
        if let Some(tag) = self.tag() {
            write!(f, "[{}] ", tag.name())?;
        }

        let ty = self.inner.ty();
        let nil = self.inner.nil();

        let nil = if f.alternate() { nil.with_nil() } else { nil };
        match (ty, nil) {
            // nil-derived types have their own representations
            (&T::None, Nil::Silent) => return write!(f, "nil"),
            (&T::None, Nil::Noisy) => return write!(f, "nil"),

            (_, _) => ty.fmt_displayed(f, ctx)?,
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
        if let Some(tag) = self.tag() {
            write!(f, "[{}] ", tag.name())?;
        }

        let ty = self.inner.ty();
        let nil = self.inner.nil();

        let nil = if f.alternate() { nil.with_nil() } else { nil };
        match (ty, nil) {
            // nil-derived types have their own representations
            (&T::None, Nil::Silent) => return write!(f, "nil"),
            (&T::None, Nil::Noisy) => return write!(f, "nil!"),

            (_, _) => fmt::Debug::fmt(ty, f)?,
        }

        match nil {
            Nil::Silent => Ok(()),
            Nil::Noisy => write!(f, "?"),
            Nil::Absent => write!(f, "!"),
        }
    }
}

#[cfg(test)]
#[allow(unused_variables, dead_code)]
mod tests {
    use kailua_diag::NoReport;
    use kailua_syntax::Str;
    use std::borrow::Cow;
    use ty::{Lattice, Union, TypeContext, NoTypeContext, F, Slot, Tag};
    use env::Context;
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
            let mut ctx = Context::new(NoReport);
            let actualunion = $l.union(&$r, check_base!(@explicitness $e), &mut ctx);
            if actualunion.is_ok() {
                panic!("{:?} | {:?} ({}) = expected Err(_), actual {:?}",
                       $l, $r, stringify!($e), actualunion);
            }
        });

        ($l:expr, $r:expr; [$e:tt]=$u:expr) => ({
            let union = Ok($u);
            let mut ctx = Context::new(NoReport);
            let actualunion = $l.union(&$r, check_base!(@explicitness $e), &mut ctx);
            if actualunion != union {
                panic!("{:?} | {:?} ({}) = expected {:?}, actual {:?}",
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
        check!(T::table(), T::array(just(T::Integer)); _);
        check!(T::table(), T::array(var(T::Integer)); _);
        check!(T::table(), T::array(cnst(T::Integer)); _);
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
        check!(T::True, T::False; T::Boolean);
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

        let mut ctx = Context::new(NoReport);

        {
            let v1 = ctx.gen_tvar();
            // v1 <: integer
            assert_eq!(T::TVar(v1).assert_sub(&T::Integer, &mut ctx), Ok(()));
            // v1 <: integer
            assert_eq!(T::TVar(v1).assert_sub(&T::Integer, &mut ctx), Ok(()));
            // v1 <: integer AND v1 <: string (!)
            assert!(T::TVar(v1).assert_sub(&T::String, &mut ctx).is_err());
        }

        {
            let v1 = ctx.gen_tvar();
            let v2 = ctx.gen_tvar();
            // v1 <: v2
            assert_eq!(T::TVar(v1).assert_sub(&T::TVar(v2), &mut ctx), Ok(()));
            // v1 <: v2 <: string
            assert_eq!(T::TVar(v2).assert_sub(&T::String, &mut ctx), Ok(()));
            // v1 <: v2 <: string AND v1 <: integer (!)
            assert!(T::TVar(v1).assert_sub(&T::Integer, &mut ctx).is_err());
        }

        /*
        {
            let v1 = ctx.gen_tvar();
            let v2 = ctx.gen_tvar();
            let t1 = T::record(hash![a=just(T::Integer), b=just(T::TVar(v1))]);
            let t2 = T::record(hash![a=just(T::TVar(v2)), b=just(T::String), c=just(T::Boolean)]);
            // {a=just integer, b=just v1} <: {a=just v2, b=just string, c=just boolean}
            assert_eq!(t1.assert_sub(&t2, &mut ctx), Ok(()));
            // ... AND v1 <: string
            assert_eq!(T::TVar(v1).assert_sub(&T::String, &mut ctx), Ok(()));
            // ... AND v1 <: string AND v2 :> integer
            assert_eq!(T::Integer.assert_sub(&T::TVar(v2), &mut ctx), Ok(()));
            // {a=just integer, b=just v1} = {a=just v2, b=just string, c=just boolean} (!)
            assert!(t1.assert_eq(&t2, &mut ctx).is_err());
        }
        */

        {
            let v1 = ctx.gen_tvar();
            let tv1 = Ty::new(T::TVar(v1));
            let tv1nil = Ty::new(T::TVar(v1)).or_nil(Nil::Noisy);
            let intnil = Ty::new(T::Integer).or_nil(Nil::Noisy);
            // v1? <: nil?
            assert_eq!(tv1nil.assert_sub(&intnil, &mut ctx), Ok(()));
            // v1 <: nil?
            assert_eq!(tv1.assert_sub(&intnil, &mut ctx), Ok(()));
            // v1 :> nil?
            assert_eq!(intnil.assert_sub(&tv1, &mut ctx), Ok(()));
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

