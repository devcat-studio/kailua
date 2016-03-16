use std::fmt;
use std::ops;
use std::borrow::{Cow, ToOwned};

use kailua_syntax::{K, Kind, Str};
use diag::CheckResult;

pub use self::literals::{Numbers, Strings};
pub use self::tables::Tables;
pub use self::functions::{Function, Functions};
pub use self::union::Union;

mod literals;
mod tables;
mod functions;
mod union;

fn error_not_bottom<T: fmt::Debug>(t: T) -> CheckResult<()> {
    Err(format!("impossible constraint requested: {:?} is bottom", t))
}

fn error_not_sub<T: fmt::Debug, U: fmt::Debug>(t: T, u: U) -> CheckResult<()> {
    Err(format!("impossible constraint requested: {:?} <: {:?}", t, u))
}

fn error_not_eq<T: fmt::Debug, U: fmt::Debug>(t: T, u: U) -> CheckResult<()> {
    Err(format!("impossible constraint requested: {:?} = {:?}", t, u))
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct TVar(pub u32);

impl fmt::Debug for TVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<#{}>", self.0)
    }
}

pub trait TVarContext {
    fn last_tvar(&self) -> Option<TVar>;
    fn gen_tvar(&mut self) -> TVar;
    fn assert_tvar_sub(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()>;
    fn assert_tvar_sup(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()>;
    fn assert_tvar_eq(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()>;
    fn assert_tvar_sub_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()>;
    fn assert_tvar_eq_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()>;
}

pub trait Lattice<Other = Self> {
    type Output;
    fn normalize(self) -> Self::Output;
    fn union(self, other: Other, ctx: &mut TVarContext) -> Self::Output;
    fn intersect(self, other: Other, ctx: &mut TVarContext) -> Self::Output;
    fn assert_sub(&self, other: &Other, ctx: &mut TVarContext) -> CheckResult<()>;
    fn assert_eq(&self, other: &Other, ctx: &mut TVarContext) -> CheckResult<()>;
}

impl<T: Lattice<Output=Option<T>> + fmt::Debug> Lattice for Option<T> {
    type Output = Option<T>;

    fn normalize(self) -> Option<T> {
        self.and_then(Lattice::normalize)
    }

    fn union(self, other: Option<T>, ctx: &mut TVarContext) -> Option<T> {
        match (self, other) {
            (Some(a), Some(b)) => a.union(b, ctx),
            (Some(a), None) => Some(a),
            (None, Some(b)) => Some(b),
            (None, None) => None,
        }
    }

    fn intersect(self, other: Option<T>, ctx: &mut TVarContext) -> Option<T> {
        match (self, other) {
            (Some(a), Some(b)) => a.intersect(b, ctx),
            (_, _) => None,
        }
    }

    fn assert_sub(&self, other: &Option<T>, ctx: &mut TVarContext) -> CheckResult<()> {
        match (self, other) {
            (&Some(ref a), &Some(ref b)) => a.assert_sub(b, ctx),
            (&Some(ref a), &None) => error_not_bottom(a),
            (&None, _) => Ok(())
        }
    }

    fn assert_eq(&self, other: &Option<T>, ctx: &mut TVarContext) -> CheckResult<()> {
        match (self, other) {
            (&Some(ref a), &Some(ref b)) => a.assert_eq(b, ctx),
            (&Some(ref a), &None) => error_not_bottom(a),
            (&None, &Some(ref b)) => error_not_bottom(b),
            (&None, &None) => Ok(())
        }
    }
}

// used when operands should not have any type variables
impl TVarContext for () {
    fn last_tvar(&self) -> Option<TVar> {
        None
    }
    fn gen_tvar(&mut self) -> TVar {
        panic!("gen_tvar is not supposed to be called here");
    }
    fn assert_tvar_sub(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()> {
        panic!("assert_tvar_sub({:?}, {:?}) is not supposed to be called here", lhs, *rhs);
    }
    fn assert_tvar_sup(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()> {
        panic!("assert_tvar_sup({:?}, {:?}) is not supposed to be called here", lhs, *rhs);
    }
    fn assert_tvar_eq(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()> {
        panic!("assert_tvar_eq({:?}, {:?}) is not supposed to be called here", lhs, *rhs);
    }
    fn assert_tvar_sub_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()> {
        panic!("assert_tvar_sub_tvar({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
    fn assert_tvar_eq_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()> {
        panic!("assert_tvar_eq_tvar({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
}

// it is generally hard to determine if a <: b \/ c (i.e. a <: b OR a <: c)
// and a /\ b <: c (i.e. a <: c OR b <: c) in the presence of
// immediate variable instantiation; it requires the costly backtracking.
//
// since we don't need the full union and intersection types
// (they are mostly created manually, or sometimes via the path merger),
// we instead provide the "best effort" inference by disallowing instantiation.
fn err_on_instantiation<T, F>(ctx: &mut TVarContext, f: F) -> CheckResult<T>
        where F: FnOnce(&mut TVarContext) -> CheckResult<T> {
    let last = ctx.last_tvar();
    let ret = try!(f(ctx));
    if last != ctx.last_tvar() {
        Err(format!("type variable cannot be instantiated inside a union or intersection"))
    } else {
        Ok(ret)
    }
}

impl Lattice for TVar {
    type Output = TVar;

    fn normalize(self) -> Self { self }

    fn union(self, other: Self, ctx: &mut TVarContext) -> Self {
        let u = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub_tvar(self, u), Ok(()));
        assert_eq!(ctx.assert_tvar_sub_tvar(other, u), Ok(()));
        u
    }

    fn intersect(self, other: Self, ctx: &mut TVarContext) -> Self {
        let i = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub_tvar(i, self), Ok(()));
        assert_eq!(ctx.assert_tvar_sub_tvar(i, other), Ok(()));
        i
    }

    fn assert_sub(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        ctx.assert_tvar_sub_tvar(*self, *other)
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        ctx.assert_tvar_eq_tvar(*self, *other)
    }
}

#[derive(Clone, PartialEq)]
pub struct Seq<T> {
    pub head: Vec<T>,
    pub tail: Option<T>,
}

impl<T> Seq<T> {
    pub fn new() -> Seq<T> {
        Seq { head: Vec::new(), tail: None }
    }

    pub fn from(t: T) -> Seq<T> {
        Seq { head: vec![t], tail: None }
    }
}

impl<T: Lattice + fmt::Debug> Seq<T> {
    fn assert_sub(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        let mut selfhead = self.head.iter();
        let mut otherhead = other.head.iter();
        let selftail = self.tail.as_ref();
        let othertail = other.tail.as_ref();

        loop {
            match (selfhead.next(), otherhead.next(), selftail, othertail) {
                (Some(a), Some(b), _, _) => try!(a.assert_sub(b, ctx)),
                (Some(a), None, _, Some(b)) => try!(a.assert_sub(b, ctx)),
                (Some(a), None, _, None) => return error_not_bottom(a),
                (None, Some(b), Some(a), _) => try!(a.assert_sub(b, ctx)),
                (None, Some(_), None, _) => {},

                // terminal conditions
                (None, None, Some(a), Some(b)) => return a.assert_sub(b, ctx),
                (None, None, Some(a), None) => return error_not_bottom(a),
                (None, None, None, _) => return Ok(()),
            }
        }
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        let mut selfhead = self.head.iter();
        let mut otherhead = other.head.iter();
        let selftail = self.tail.as_ref();
        let othertail = other.tail.as_ref();

        loop {
            match (selfhead.next(), otherhead.next(), selftail, othertail) {
                (Some(a), Some(b), _, _) => try!(a.assert_eq(b, ctx)),
                (Some(a), None, _, Some(b)) => try!(a.assert_eq(b, ctx)),
                (Some(a), None, _, None) => return error_not_bottom(a),
                (None, Some(b), Some(a), _) => try!(a.assert_eq(b, ctx)),
                (None, Some(b), None, _) => return error_not_bottom(b),

                // terminal conditions
                (None, None, Some(a), Some(b)) => return a.assert_eq(b, ctx),
                (None, None, Some(a), None) => return error_not_bottom(a),
                (None, None, None, Some(b)) => return error_not_bottom(b),
                (None, None, None, None) => return Ok(()),
            }
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Seq<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "("));
        let mut first = true;
        for t in &self.head {
            if first { first = false; } else { try!(write!(f, ", ")); }
            try!(write!(f, "{:?}", t));
        }
        if let Some(ref t) = self.tail {
            if !first { try!(write!(f, ", ")); }
            try!(write!(f, ", {:?}...", t));
        }
        write!(f, ")")
    }
}

bitflags! {
    flags Flags: u16 {
        const T_NONE       = 0b0_0000_0000,
        const T_DYNAMIC    = 0b0_0000_0001,
        const T_NIL        = 0b0_0000_0010,
        const T_TRUE       = 0b0_0000_0100,
        const T_FALSE      = 0b0_0000_1000,
        const T_BOOLEAN    = 0b0_0000_1100,
        const T_NONINTEGER = 0b0_0001_0000,
        const T_INTEGER    = 0b0_0010_0000,
        const T_NUMBER     = 0b0_0011_0000,
        const T_STRING     = 0b0_0100_0000,
        const T_TABLE      = 0b0_1000_0000,
        const T_FUNCTION   = 0b1_0000_0000,

        const T_INTEGRAL   = T_DYNAMIC.bits | T_INTEGER.bits,
        // strings can be also used in place of numbers in Lua but omitted here
        const T_NUMERIC    = T_DYNAMIC.bits | T_NUMBER.bits,
        const T_STRINGY    = T_DYNAMIC.bits | T_NUMBER.bits | T_STRING.bits,
        const T_TABULAR    = T_DYNAMIC.bits | T_STRING.bits | T_TABLE.bits,
        // "default" types that metatables are set or can be set
        // XXX shouldn't this be customizable?
        const T_CALLABLE   = T_DYNAMIC.bits | T_FUNCTION.bits,
    }
}

impl Flags {
    pub fn is_integral(&self) -> bool {
        (*self & T_DYNAMIC != T_NONE) || ((*self & T_INTEGRAL != T_NONE) &&
                                          (*self & !T_INTEGRAL == T_NONE))
    }

    pub fn is_numeric(&self) -> bool {
        (*self & T_DYNAMIC != T_NONE) || ((*self & T_NUMERIC != T_NONE) &&
                                          (*self & !T_NUMERIC == T_NONE))
    }

    pub fn is_stringy(&self) -> bool {
        (*self & T_DYNAMIC != T_NONE) || ((*self & T_STRINGY != T_NONE) &&
                                          (*self & !T_STRINGY == T_NONE))
    }

    pub fn is_tabular(&self) -> bool {
        (*self & T_DYNAMIC != T_NONE) || ((*self & T_TABULAR != T_NONE) &&
                                          (*self & !T_TABULAR == T_NONE))
    }

    pub fn is_callable(&self) -> bool {
        (*self & T_DYNAMIC != T_NONE) || ((*self & T_CALLABLE != T_NONE) &&
                                          (*self & !T_CALLABLE == T_NONE))
    }
}

pub mod flags {
    pub use super::{T_NONE, T_DYNAMIC, T_NIL, T_TRUE, T_FALSE, T_BOOLEAN,
                    T_NONINTEGER, T_INTEGER, T_NUMBER, T_STRING, T_TABLE, T_FUNCTION,
                    T_INTEGRAL, T_NUMERIC, T_STRINGY, T_TABULAR, T_CALLABLE};
}

// "shallow" types used for enumeration and construction
#[derive(Clone)]
pub enum T<'a> {
    Dynamic,                            // ?
    None,                               // (bottom)
    Nil,                                // nil
    Boolean,                            // boolean
    True,                               // true
    False,                              // false
    Numbers(Cow<'a, Numbers>),          // number, ...
    Strings(Cow<'a, Strings>),          // string, ...
    Tables(Cow<'a, Tables>),            // table, ...
    Functions(Cow<'a, Functions>),      // function, ...
    TVar(TVar),                         // type variable
    Union(Cow<'a, Union>),              // union types A | B | ...
}

impl<'a> T<'a> {
    pub fn number()          -> T<'a> { T::Numbers(Cow::Owned(Numbers::All)) }
    pub fn integer()         -> T<'a> { T::Numbers(Cow::Owned(Numbers::Int)) }
    pub fn int(v: i32)       -> T<'a> { T::Numbers(Cow::Owned(Numbers::One(v))) }
    pub fn string()          -> T<'a> { T::Strings(Cow::Owned(Strings::All)) }
    pub fn str(s: Str)       -> T<'a> { T::Strings(Cow::Owned(Strings::One(s))) }
    pub fn table()           -> T<'a> { T::Tables(Cow::Owned(Tables::All)) }
    pub fn empty_table()     -> T<'a> { T::Tables(Cow::Owned(Tables::Empty)) }
    pub fn function()        -> T<'a> { T::Functions(Cow::Owned(Functions::All)) }
    pub fn func(f: Function) -> T<'a> { T::Functions(Cow::Owned(Functions::Simple(f))) }

    pub fn ints<I: IntoIterator<Item=i32>>(i: I) -> T<'a> {
        T::Numbers(Cow::Owned(Numbers::Some(i.into_iter().collect())))
    }
    pub fn strs<I: IntoIterator<Item=Str>>(i: I) -> T<'a> {
        T::Strings(Cow::Owned(Strings::Some(i.into_iter().collect())))
    }
    pub fn tuple<I: IntoIterator<Item=Ty>>(i: I) -> T<'a> {
        T::Tables(Cow::Owned(Tables::Tuple(i.into_iter().collect())))
    }
    pub fn record<I: IntoIterator<Item=(Str,Ty)>>(i: I) -> T<'a> {
        T::Tables(Cow::Owned(Tables::Record(i.into_iter().collect())))
    }
    pub fn array(v: Ty) -> T<'a> {
        T::Tables(Cow::Owned(Tables::Array(v)))
    }
    pub fn map(k: Ty, v: Ty) -> T<'a> {
        T::Tables(Cow::Owned(Tables::Map(k, v)))
    }

    pub fn from(kind: &K) -> T<'a> {
        match *kind {
            K::Dynamic           => T::Dynamic,
            K::Nil               => T::Nil,
            K::Boolean           => T::Boolean,
            K::BooleanLit(true)  => T::True,
            K::BooleanLit(false) => T::False,
            K::Number            => T::Numbers(Cow::Owned(Numbers::All)),
            K::Integer           => T::Numbers(Cow::Owned(Numbers::Int)),
            K::IntegerLit(v)     => T::Numbers(Cow::Owned(Numbers::One(v))),
            K::String            => T::Strings(Cow::Owned(Strings::All)),
            K::StringLit(ref s)  => T::Strings(Cow::Owned(Strings::One(s.to_owned()))),
            K::Table             => T::Tables(Cow::Owned(Tables::All)),
            K::Function          => T::Functions(Cow::Owned(Functions::All)),

            K::Union(ref kinds) => {
                assert!(!kinds.is_empty());
                let mut ty = T::from(&kinds[0]);
                for kind in &kinds[1..] {
                    ty = ty | T::from(kind);
                }
                ty
            }
        }
    }

    pub fn flags(&self) -> Flags {
        match *self {
            T::Dynamic => T_DYNAMIC,
            T::None    => T_NONE,
            T::Nil     => T_NIL,
            T::Boolean => T_BOOLEAN,
            T::True    => T_TRUE,
            T::False   => T_FALSE,

            T::Numbers(ref num) => match &**num {
                &Numbers::One(..) | &Numbers::Some(..) | &Numbers::Int => T_INTEGER,
                &Numbers::All => T_NUMBER,
            },
            T::Strings(..) => T_STRING,
            T::Tables(..) => T_TABLE,
            T::Functions(..) => T_FUNCTION,

            T::TVar(..) => T_NONE,
            T::Union(ref u) => u.flags(),
        }
    }

    pub fn is_none(&self) -> bool {
        match *self {
            T::None => true,
            T::Numbers(ref num) =>
                if let &Numbers::Some(ref set) = &**num { set.is_empty() } else { false },
            T::Strings(ref str) =>
                if let &Strings::Some(ref set) = &**str { set.is_empty() } else { false },
            T::Functions(ref func) =>
                if let &Functions::Multi(ref fs) = &**func { fs.is_empty() } else { false },
            T::Union(ref u) => u.flags() == T_NONE,
            _ => false,
        }
    }

    pub fn is_dynamic(&self)  -> bool { self.flags() & T_DYNAMIC != T_NONE }
    pub fn is_integral(&self) -> bool { self.flags().is_integral() }
    pub fn is_numeric(&self)  -> bool { self.flags().is_numeric() }
    pub fn is_stringy(&self)  -> bool { self.flags().is_stringy() }
    pub fn is_tabular(&self)  -> bool { self.flags().is_tabular() }
    pub fn is_callable(&self) -> bool { self.flags().is_callable() }

    pub fn has_true(&self) -> bool {
        match *self {
            T::Boolean | T::True => true,
            T::Union(ref u) => u.has_true,
            _ => false,
        }
    }

    pub fn has_false(&self) -> bool {
        match *self {
            T::Boolean | T::False => true,
            T::Union(ref u) => u.has_false,
            _ => false,
        }
    }

    pub fn has_numbers(&self) -> Option<&Numbers> {
        match *self {
            T::Numbers(ref num) => Some(num),
            T::Union(ref u) => u.numbers.as_ref(),
            _ => None,
        }
    }

    pub fn has_strings(&self) -> Option<&Strings> {
        match *self {
            T::Strings(ref str) => Some(str),
            T::Union(ref u) => u.strings.as_ref(),
            _ => None,
        }
    }

    pub fn has_tables(&self) -> Option<&Tables> {
        match *self {
            T::Tables(ref tab) => Some(tab),
            T::Union(ref u) => u.tables.as_ref(),
            _ => None,
        }
    }

    pub fn has_functions(&self) -> Option<&Functions> {
        match *self {
            T::Functions(ref func) => Some(func),
            T::Union(ref u) => u.functions.as_ref(),
            _ => None,
        }
    }

    pub fn has_tvar(&self) -> Option<TVar> {
        match *self {
            T::TVar(tv) => Some(tv),
            T::Union(ref u) => u.tvar,
            _ => None,
        }
    }

    // XXX should be in S instead
    pub fn accept(&self, rhs: &T) -> bool {
        let flags = self.flags();
        let rhsflags = rhs.flags();
        if flags & rhsflags != rhsflags { return false; }

        // not covered by flags
        match (rhs.has_numbers(), self.has_numbers()) {
            (Some(r), Some(l)) => { if r.assert_sub(l, &mut ()).is_err() { return false; } }
            (None, Some(_)) => return false,
            (_, None) => {}
        }
        match (rhs.has_strings(), self.has_strings()) {
            (Some(r), Some(l)) => { if r.assert_sub(l, &mut ()).is_err() { return false; } }
            (None, Some(_)) => return false,
            (_, None) => {}
        }

        true
    }

    pub fn into_send(self) -> T<'static> {
        match self {
            T::Dynamic    => T::Dynamic,
            T::None       => T::None,
            T::Nil        => T::Nil,
            T::Boolean    => T::Boolean,
            T::True       => T::True,
            T::False      => T::False,

            T::Numbers(num)    => T::Numbers(Cow::Owned(num.into_owned())),
            T::Strings(str)    => T::Strings(Cow::Owned(str.into_owned())),
            T::Tables(tab)     => T::Tables(Cow::Owned(tab.into_owned())),
            T::Functions(func) => T::Functions(Cow::Owned(func.into_owned())),
            T::TVar(tv)        => T::TVar(tv),

            T::Union(u) => T::Union(Cow::Owned(u.into_owned())),
        }
    }
}

impl<'a, 'b> Lattice<T<'b>> for T<'a> {
    type Output = T<'static>;

    fn normalize(self) -> T<'static> {
        match self {
            T::Dynamic    => T::Dynamic,
            T::None       => T::None,
            T::Nil        => T::Nil,
            T::Boolean    => T::Boolean,
            T::True       => T::True,
            T::False      => T::False,

            T::Numbers(num) => {
                if let Some(num) = num.into_owned().normalize() {
                    T::Numbers(Cow::Owned(num))
                } else {
                    T::None
                }
            }

            T::Strings(str) => {
                if let Some(str) = str.into_owned().normalize() {
                    T::Strings(Cow::Owned(str))
                } else {
                    T::None
                }
            }

            T::Tables(tab) => {
                if let Some(tab) = tab.into_owned().normalize() {
                    T::Tables(Cow::Owned(tab))
                } else {
                    T::None
                }
            }

            T::Functions(func) => {
                if let Some(func) = func.into_owned().normalize() {
                    T::Functions(Cow::Owned(func))
                } else {
                    T::None
                }
            }

            T::TVar(tv) => T::TVar(tv),
            T::Union(u) => T::Union(Cow::Owned(u.into_owned())),
        }
    }

    fn union(self, other: T<'b>, ctx: &mut TVarContext) -> T<'static> {
        match (self, other) {
            // dynamic eclipses everything else
            (T::Dynamic, _) => T::Dynamic,
            (_, T::Dynamic) => T::Dynamic,

            (T::None, ty) => ty.into_send(),
            (ty, T::None) => ty.into_send(),

            (T::Nil,     T::Nil)     => T::Nil,
            (T::Boolean, T::Boolean) => T::Boolean,
            (T::Boolean, T::True)    => T::Boolean,
            (T::Boolean, T::False)   => T::Boolean,
            (T::True,    T::Boolean) => T::Boolean,
            (T::False,   T::Boolean) => T::Boolean,
            (T::True,    T::True)    => T::True,
            (T::True,    T::False)   => T::Boolean,
            (T::False,   T::True)    => T::Boolean,
            (T::False,   T::False)   => T::False,

            (T::Numbers(a), T::Numbers(b)) => {
                if let Some(num) = a.into_owned().union(b.into_owned(), ctx) {
                    T::Numbers(Cow::Owned(num))
                } else {
                    T::None
                }
            }

            (T::Strings(a), T::Strings(b)) => {
                if let Some(str) = a.into_owned().union(b.into_owned(), ctx) {
                    T::Strings(Cow::Owned(str))
                } else {
                    T::None
                }
            }

            (T::Tables(a), T::Tables(b)) => {
                if let Some(tab) = a.into_owned().union(b.into_owned(), ctx) {
                    T::Tables(Cow::Owned(tab))
                } else {
                    T::None
                }
            }

            (T::Functions(a), T::Functions(b)) => {
                if let Some(func) = a.into_owned().union(b.into_owned(), ctx) {
                    T::Functions(Cow::Owned(func))
                } else {
                    T::None
                }
            }

            (T::TVar(a), T::TVar(b)) => T::TVar(a.union(b, ctx)),

            (a, b) => Union::from(a).union(Union::from(b), ctx).simplify(),
        }
    }

    fn intersect(self, other: T<'b>, ctx: &mut TVarContext) -> T<'static> {
        match (self, other) {
            (T::Dynamic, ty) => ty.into_send(),
            (ty, T::Dynamic) => ty.into_send(),

            (T::None, _) => T::None,
            (_, T::None) => T::None,

            (T::Nil,     T::Nil)     => T::Nil,
            (T::Boolean, T::Boolean) => T::Boolean,
            (T::Boolean, T::True)    => T::True,
            (T::Boolean, T::False)   => T::False,
            (T::True,    T::Boolean) => T::True,
            (T::False,   T::Boolean) => T::False,
            (T::True,    T::True)    => T::True,
            (T::False,   T::False)   => T::False,

            (T::Numbers(a), T::Numbers(b)) => {
                if let Some(num) = a.into_owned().intersect(b.into_owned(), ctx) {
                    T::Numbers(Cow::Owned(num))
                } else {
                    T::None
                }
            }

            (T::Strings(a), T::Strings(b)) => {
                if let Some(str) = a.into_owned().intersect(b.into_owned(), ctx) {
                    T::Strings(Cow::Owned(str))
                } else {
                    T::None
                }
            }

            (T::Tables(a), T::Tables(b)) => {
                if let Some(tab) = a.into_owned().intersect(b.into_owned(), ctx) {
                    T::Tables(Cow::Owned(tab))
                } else {
                    T::None
                }
            }

            (T::Functions(a), T::Functions(b)) => {
                if let Some(func) = a.into_owned().intersect(b.into_owned(), ctx) {
                    T::Functions(Cow::Owned(func))
                } else {
                    T::None
                }
            }

            (T::TVar(a), T::TVar(b)) => T::TVar(a.intersect(b, ctx)),

            (a, T::Union(b)) => Union::from(a).intersect(b.into_owned(), ctx).simplify(),
            (T::Union(a), b) => a.into_owned().intersect(Union::from(b), ctx).simplify(),
            (_, _) => T::None,
        }
    }

    fn assert_sub(&self, other: &T<'b>, ctx: &mut TVarContext) -> CheckResult<()> {
        println!("asserting a constraint {:?} <: {:?}", *self, *other);

        let ok = match (self, other) {
            (&T::Dynamic, _) => true,
            (_, &T::Dynamic) => true,

            (&T::None, _) => true,
            (_, &T::None) => false,

            (&T::Nil,     &T::Nil)     => true,
            (&T::Boolean, &T::Boolean) => true,
            (&T::True,    &T::Boolean) => true,
            (&T::True,    &T::True)    => true,
            (&T::False,   &T::Boolean) => true,
            (&T::False,   &T::False)   => true,

            (&T::Numbers(ref a),   &T::Numbers(ref b))   => return a.assert_sub(b, ctx),
            (&T::Strings(ref a),   &T::Strings(ref b))   => return a.assert_sub(b, ctx),
            (&T::Tables(ref a),    &T::Tables(ref b))    => return a.assert_sub(b, ctx),
            (&T::Functions(ref a), &T::Functions(ref b)) => return a.assert_sub(b, ctx),

            (&T::Union(ref a), &T::Union(ref b)) => return a.assert_sub(b, ctx),
            (&T::Union(ref a), b) => {
                // a1 \/ a2 <: b === a1 <: b AND a2 <: b
                return a.visit(|i| i.assert_sub(b, ctx));
            },

            // a <: b1 \/ b2 === a <: b1 OR a <: b2
            (&T::Nil,     &T::Union(ref b)) => b.has_nil,
            (&T::Boolean, &T::Union(ref b)) => b.has_true && b.has_false,
            (&T::True,    &T::Union(ref b)) => b.has_true,
            (&T::False,   &T::Union(ref b)) => b.has_false,

            (&T::Numbers(ref a), &T::Union(ref b)) => {
                if let Some(ref num) = b.numbers { return a.assert_sub(num, ctx); }
                false
            },
            (&T::Strings(ref a), &T::Union(ref b)) => {
                if let Some(ref str) = b.strings { return a.assert_sub(str, ctx); }
                false
            },
            (&T::Tables(ref a), &T::Union(ref b)) => {
                if let Some(ref tab) = b.tables { return a.assert_sub(tab, ctx); }
                false
            },
            (&T::Functions(ref a), &T::Union(ref b)) => {
                if let Some(ref func) = b.functions { return a.assert_sub(func, ctx); }
                false
            },
            // XXX a <: T \/ b === a <: T OR a <: b
            (&T::TVar(a), &T::Union(ref b)) if b.tvar.is_some() => false,

            (&T::TVar(a), &T::TVar(b)) => return a.assert_sub(&b, ctx),
            (a, &T::TVar(b)) => return ctx.assert_tvar_sup(b, a),
            (&T::TVar(a), b) => return ctx.assert_tvar_sub(a, b),

            (_, _) => false,
        };

        if ok { Ok(()) } else { error_not_sub(self, other) }
    }

    fn assert_eq(&self, other: &T<'b>, ctx: &mut TVarContext) -> CheckResult<()> {
        println!("asserting a constraint {:?} = {:?}", *self, *other);

        let ok = match (self, other) {
            (&T::Dynamic, _) => true,
            (_, &T::Dynamic) => true,

            (&T::None, _) => true,
            (_, &T::None) => false,

            (&T::Nil,     &T::Nil)     => true,
            (&T::Boolean, &T::Boolean) => true,
            (&T::True,    &T::True)    => true,
            (&T::False,   &T::False)   => true,

            (&T::Numbers(ref a),   &T::Numbers(ref b))   => return a.assert_eq(b, ctx),
            (&T::Strings(ref a),   &T::Strings(ref b))   => return a.assert_eq(b, ctx),
            (&T::Tables(ref a),    &T::Tables(ref b))    => return a.assert_eq(b, ctx),
            (&T::Functions(ref a), &T::Functions(ref b)) => return a.assert_eq(b, ctx),

            (&T::TVar(a), &T::TVar(b)) => return a.assert_eq(&b, ctx),
            (a, &T::TVar(b)) => return ctx.assert_tvar_eq(b, a),
            (&T::TVar(a), b) => return ctx.assert_tvar_eq(a, b),

            (&T::Union(ref a), &T::Union(ref b)) => return a.assert_eq(b, ctx),
            (&T::Union(ref a), b) => unimplemented!(), // XXX for now
            (a, &T::Union(ref b)) => unimplemented!(), // XXX for now

            (_, _) => false,
        };

        if ok { Ok(()) } else { error_not_eq(self, other) }
    }
}

impl<'a, 'b> ops::BitOr<T<'b>> for T<'a> {
    type Output = T<'static>;
    fn bitor(self, rhs: T<'b>) -> T<'static> { self.union(rhs, &mut ()) }
}

impl<'a, 'b> ops::BitAnd<T<'b>> for T<'a> {
    type Output = T<'static>;
    fn bitand(self, rhs: T<'b>) -> T<'static> { self.intersect(rhs, &mut ()) }
}

// not intended to be complete equality, but enough for testing
impl<'a, 'b> PartialEq<T<'b>> for T<'a> {
    fn eq(&self, other: &T<'b>) -> bool {
        match (self, other) {
            (&T::Dynamic, &T::Dynamic) => true,
            (&T::None,    &T::None)    => true,
            (&T::Nil,     &T::Nil)     => true,
            (&T::Boolean, &T::Boolean) => true,
            (&T::True,    &T::True)    => true,
            (&T::False,   &T::False)   => true,

            (&T::Numbers(ref a),   &T::Numbers(ref b))   => *a == *b,
            (&T::Strings(ref a),   &T::Strings(ref b))   => *a == *b,
            (&T::Tables(ref a),    &T::Tables(ref b))    => *a == *b,
            (&T::Functions(ref a), &T::Functions(ref b)) => *a == *b,
            (&T::TVar(a),          &T::TVar(b))          => a == b,
            (&T::Union(ref a),     &T::Union(ref b))     => a == b,

            (_, _) => false,
        }
    }
}

impl<'a> fmt::Debug for T<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            T::Dynamic => write!(f, "?"),
            T::None    => write!(f, "<bottom>"),
            T::Nil     => write!(f, "nil"),
            T::Boolean => write!(f, "boolean"),
            T::True    => write!(f, "true"),
            T::False   => write!(f, "false"),

            T::Numbers(ref num)    => fmt::Debug::fmt(num, f),
            T::Strings(ref str)    => fmt::Debug::fmt(str, f),
            T::Tables(ref tab)     => fmt::Debug::fmt(tab, f),
            T::Functions(ref func) => fmt::Debug::fmt(func, f),
            T::TVar(tv)            => write!(f, "<#{}>", tv.0),
            T::Union(ref u)        => fmt::Debug::fmt(u, f),
        }
    }
}

impl<'a> From<T<'a>> for Union { fn from(x: T<'a>) -> Union { Union::from(x) } }

impl<'a> From<K> for T<'a> { fn from(x: K) -> T<'a> { T::from(&x) } }

pub type Ty = Box<T<'static>>;

impl<'a, 'b> Lattice<Box<T<'b>>> for Box<T<'a>> {
    type Output = Ty;

    fn normalize(self) -> Ty {
        Box::new((*self).normalize())
    }

    fn union(self, other: Box<T<'b>>, ctx: &mut TVarContext) -> Ty {
        Box::new((*self).union(*other, ctx))
    }

    fn intersect(self, other: Box<T<'b>>, ctx: &mut TVarContext) -> Ty {
        Box::new((*self).intersect(*other, ctx))
    }

    fn assert_sub(&self, other: &Box<T<'b>>, ctx: &mut TVarContext) -> CheckResult<()> {
        (**self).assert_sub(&**other, ctx)
    }

    fn assert_eq(&self, other: &Box<T<'b>>, ctx: &mut TVarContext) -> CheckResult<()> {
        (**self).assert_eq(&**other, ctx)
    }
}

impl From<Kind> for Ty { fn from(x: Kind) -> Ty { Box::new(From::from(*x)) } }

#[cfg(test)] 
mod tests {
    use kailua_syntax::Str;
    use super::*;

    macro_rules! hash {
        ($($k:ident = $v:expr),*) => (vec![$((s(stringify!($k)), $v)),*])
    }

    fn b<T>(x: T) -> Box<T> { Box::new(x) }
    fn s(x: &str) -> Str { Str::from(x.as_bytes().to_owned()) }

    #[test]
    fn test_lattice() {
        macro_rules! check {
            ($l:expr, $r:expr; $u:expr, $i:expr) => ({
                let left = $l;
                let right = $r;
                let union = $u;
                let intersect = $i;
                let actualunion = left.clone().union(right.clone(), &mut ());
                if actualunion != union {
                    panic!("{:?} | {:?} = expected {:?}, actual {:?}",
                           left, right, union, actualunion);
                }
                let actualintersect = left.clone().intersect(right.clone(), &mut ());
                if actualintersect != intersect {
                    panic!("{:?} & {:?} = expected {:?}, actual {:?}",
                           left, right, intersect, actualintersect);
                }
            })
        }

        // dynamic vs. everything else
        check!(T::Dynamic, T::Dynamic; T::Dynamic, T::Dynamic);
        check!(T::Dynamic, T::integer(); T::Dynamic, T::integer());
        check!(T::tuple(vec![b(T::integer()), b(T::Boolean)]), T::Dynamic;
               T::Dynamic, T::tuple(vec![b(T::integer()), b(T::Boolean)]));

        // integer literals
        check!(T::integer(), T::number(); T::number(), T::integer());
        check!(T::number(), T::integer(); T::number(), T::integer());
        check!(T::number(), T::number(); T::number(), T::number());
        check!(T::integer(), T::integer(); T::integer(), T::integer());
        check!(T::int(3), T::int(3); T::int(3), T::int(3));
        check!(T::int(3), T::number(); T::number(), T::int(3));
        check!(T::integer(), T::int(3); T::integer(), T::int(3));
        check!(T::int(3), T::int(4); T::ints(vec![3, 4]), T::None);
        check!(T::ints(vec![3, 4]), T::int(3);
               T::ints(vec![3, 4]), T::int(3));
        check!(T::int(5), T::ints(vec![3, 4]);
               T::ints(vec![3, 4, 5]), T::None);
        check!(T::ints(vec![3, 4]), T::ints(vec![5, 4, 7]);
               T::ints(vec![3, 4, 5, 7]), T::int(4));
        check!(T::ints(vec![3, 4, 5]), T::ints(vec![2, 3, 4]);
               T::ints(vec![2, 3, 4, 5]), T::ints(vec![3, 4]));

        // string literals
        check!(T::string(), T::str(s("hello")); T::string(), T::str(s("hello")));
        check!(T::str(s("hello")), T::string(); T::string(), T::str(s("hello")));
        check!(T::str(s("hello")), T::str(s("hello"));
               T::str(s("hello")), T::str(s("hello")));
        check!(T::str(s("hello")), T::str(s("goodbye"));
               T::strs(vec![s("hello"), s("goodbye")]), T::None);
        check!(T::str(s("hello")), T::strs(vec![s("goodbye")]);
               T::strs(vec![s("hello"), s("goodbye")]), T::None);
        check!(T::strs(vec![s("hello"), s("goodbye")]), T::str(s("goodbye"));
               T::strs(vec![s("hello"), s("goodbye")]), T::str(s("goodbye")));
        check!(T::strs(vec![s("hello"), s("goodbye")]),
               T::strs(vec![s("what"), s("goodbye")]);
               T::strs(vec![s("hello"), s("goodbye"), s("what")]),
               T::str(s("goodbye")));
        check!(T::strs(vec![s("a"), s("b"), s("c")]),
               T::strs(vec![s("b"), s("c"), s("d")]);
               T::strs(vec![s("a"), s("b"), s("c"), s("d")]),
               T::strs(vec![s("b"), s("c")]));

        // tables
        check!(T::table(), T::array(b(T::integer())); T::table(), T::array(b(T::integer())));
        check!(T::array(b(T::integer())), T::array(b(T::integer()));
               T::array(b(T::integer())), T::array(b(T::integer())));
        check!(T::array(b(T::int(3))), T::array(b(T::int(4)));
               T::array(b(T::ints(vec![3, 4]))), T::array(b(T::None)));
        check!(T::tuple(vec![b(T::integer()), b(T::string())]),
               T::tuple(vec![b(T::number()), b(T::Dynamic), b(T::Boolean)]);
               T::tuple(vec![b(T::number()), b(T::Dynamic), b(T::Boolean | T::Nil)]),
               T::tuple(vec![b(T::integer()), b(T::string())]));
        check!(T::tuple(vec![b(T::integer()), b(T::string())]),
               T::tuple(vec![b(T::number()), b(T::Boolean), b(T::Dynamic)]);
               T::tuple(vec![b(T::number()), b(T::string() | T::Boolean), b(T::Dynamic)]),
               T::empty_table()); // boolean & string = _|_, so no way to reconcile
        check!(T::record(hash![foo=b(T::integer()), bar=b(T::string())]),
               T::record(hash![quux=b(T::Boolean)]);
               T::record(hash![foo=b(T::integer()), bar=b(T::string()), quux=b(T::Boolean)]),
               T::empty_table());
        check!(T::record(hash![foo=b(T::int(3)), bar=b(T::string())]),
               T::record(hash![foo=b(T::int(4))]);
               T::record(hash![foo=b(T::ints(vec![3, 4])), bar=b(T::string())]),
               T::empty_table());
        check!(T::record(hash![foo=b(T::integer()), bar=b(T::number()),
                                    quux=b(T::array(b(T::Dynamic)))]),
               T::record(hash![foo=b(T::number()), bar=b(T::string()),
                                    quux=b(T::array(b(T::Boolean)))]);
               T::record(hash![foo=b(T::number()), bar=b(T::number() | T::string()),
                                    quux=b(T::array(b(T::Dynamic)))]),
               T::record(hash![foo=b(T::integer()),
                                    quux=b(T::array(b(T::Boolean)))]));
        check!(T::record(hash![foo=b(T::int(3)), bar=b(T::number())]),
               T::map(b(T::string()), b(T::integer()));
               T::map(b(T::string()), b(T::number())),
               T::record(hash![foo=b(T::int(3)), bar=b(T::integer())]));
        check!(T::map(b(T::str(s("wat"))), b(T::integer())),
               T::map(b(T::string()), b(T::int(42)));
               T::map(b(T::string()), b(T::integer())),
               T::map(b(T::str(s("wat"))), b(T::int(42))));
        check!(T::array(b(T::number())), T::map(b(T::Dynamic), b(T::integer()));
               T::map(b(T::Dynamic), b(T::number())), T::array(b(T::integer())));
        check!(T::empty_table(), T::array(b(T::integer()));
               T::array(b(T::integer())), T::empty_table());

        // general unions
        check!(T::True, T::False; T::Boolean, T::None);
        check!(T::int(3) | T::Nil, T::int(4) | T::Nil;
               T::ints(vec![3, 4]) | T::Nil, T::Nil);
        check!(T::ints(vec![3, 5]) | T::Nil, T::int(4) | T::string();
               T::string() | T::ints(vec![3, 4, 5]) | T::Nil, T::None);
        check!(T::int(3) | T::string(), T::str(s("wat")) | T::int(4);
               T::ints(vec![3, 4]) | T::string(), T::str(s("wat")));
        check!(T::array(b(T::integer())), T::tuple(vec![b(T::string())]);
               T::map(b(T::integer()), b(T::integer() | T::string())), T::empty_table());
        //assert_eq!(T::map(b(T::string()), b(T::integer())),
        //           T::map(b(T::string()), b(T::integer() | T::Nil)));
    }

    #[test]
    fn test_sub() {
        use env::Context;

        let mut ctx = Context::new();

        {
            let v1 = ctx.gen_tvar();
            // v1 <: integer
            assert_eq!(T::TVar(v1).assert_sub(&T::integer(), &mut ctx), Ok(()));
            // v1 <: integer
            assert_eq!(T::TVar(v1).assert_sub(&T::integer(), &mut ctx), Ok(()));
            // v1 <: integer AND v1 <: string (!)
            assert!(T::TVar(v1).assert_sub(&T::string(), &mut ctx).is_err());
        }

        {
            let v1 = ctx.gen_tvar();
            let v2 = ctx.gen_tvar();
            // v1 <: v2
            assert_eq!(T::TVar(v1).assert_sub(&T::TVar(v2), &mut ctx), Ok(()));
            // v1 <: v2 <: string
            assert_eq!(T::TVar(v2).assert_sub(&T::string(), &mut ctx), Ok(()));
            // v1 <: v2 <: string AND v1 <: integer (!)
            assert!(T::TVar(v1).assert_sub(&T::integer(), &mut ctx).is_err());
        }

        {
            let v1 = ctx.gen_tvar();
            let v2 = ctx.gen_tvar();
            let t1 = T::record(hash![a=b(T::integer()), b=b(T::TVar(v1))]);
            let t2 = T::record(hash![a=b(T::TVar(v2)), b=b(T::string()), c=b(T::Boolean)]);
            // {a=integer, b=v1} <: {a=v2, b=string, c=boolean}
            assert_eq!(t1.assert_sub(&t2, &mut ctx), Ok(()));
            // ... AND v1 <: string
            assert_eq!(T::TVar(v1).assert_sub(&T::string(), &mut ctx), Ok(()));
            // ... AND v1 <: string AND v2 :> integer
            assert_eq!(T::integer().assert_sub(&T::TVar(v2), &mut ctx), Ok(()));
            // {a=integer, b=v1} = {a=v2, b=string, c=boolean} (!)
            assert!(t1.assert_eq(&t2, &mut ctx).is_err());
        }
    }
}

