use std::fmt;
use std::ops;
use std::hash::Hash;
use std::borrow::{Cow, ToOwned};
use std::collections::{HashSet, HashMap};

use kailua_syntax::{K, Kind, Str};
use diag::CheckResult;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Builtin {
    Require,        // (fixed string) -> table & sideeffect
}

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
// and a /\ b <: c (i.e. a <: c OR b <: C) in the presence of
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
                (None, Some(b), None, _) => {},

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
            if first { first = false; } else { try!(write!(f, ", ")); }
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

#[derive(Clone)]
pub enum Numbers {
    One(i32),
    Some(HashSet<i32>),
    Int,
    All,
}

impl Lattice for Numbers {
    type Output = Option<Numbers>;

    fn normalize(self) -> Option<Numbers> {
        match self {
            Numbers::Some(set) => match set.len() {
                0 => None,
                1 => Some(Numbers::One(set.into_iter().next().unwrap())),
                _ => Some(Numbers::Some(set)),
            },
            num => Some(num),
        }
    }

    fn union(self, other: Numbers, _: &mut TVarContext) -> Option<Numbers> {
        match (self, other) {
            (Numbers::All, _) => Some(Numbers::All),
            (_, Numbers::All) => Some(Numbers::All),

            (Numbers::Int, _) => Some(Numbers::Int),
            (_, Numbers::Int) => Some(Numbers::Int),

            (Numbers::Some(mut a), Numbers::Some(b)) => {
                a.extend(b.into_iter());
                Some(Numbers::Some(a))
            }
            (Numbers::Some(mut a), Numbers::One(b)) => {
                a.insert(b);
                Some(Numbers::Some(a))
            }
            (Numbers::One(a), Numbers::Some(mut b)) => {
                b.insert(a);
                Some(Numbers::Some(b))
            }
            (Numbers::One(a), Numbers::One(b)) => {
                if a == b {
                    Some(Numbers::One(a))
                } else {
                    let mut ab = HashSet::new();
                    ab.insert(a);
                    ab.insert(b);
                    Some(Numbers::Some(ab))
                }
            }
        }
    }

    fn intersect(self, other: Numbers, _: &mut TVarContext) -> Option<Numbers> {
        match (self, other) {
            (Numbers::One(a), Numbers::One(b)) =>
                if a == b { Some(Numbers::One(a)) } else { None },
            (Numbers::One(a), Numbers::Some(b)) =>
                if b.contains(&a) { Some(Numbers::One(a)) } else { None },
            (Numbers::Some(a), Numbers::One(b)) =>
                if a.contains(&b) { Some(Numbers::One(b)) } else { None },
            (Numbers::Some(a), Numbers::Some(b)) => {
                let set: HashSet<i32> = a.intersection(&b).cloned().collect();
                if set.is_empty() { None } else { Some(Numbers::Some(set)) }
            }

            (Numbers::One(v), _) => Some(Numbers::One(v)),
            (_, Numbers::One(v)) => Some(Numbers::One(v)),

            (Numbers::Some(set), _) => Some(Numbers::Some(set)),
            (_, Numbers::Some(set)) => Some(Numbers::Some(set)),

            (Numbers::Int, _) => Some(Numbers::Int),
            (_, Numbers::Int) => Some(Numbers::Int),

            (Numbers::All, Numbers::All) => Some(Numbers::All),
        }
    }

    fn assert_sub(&self, other: &Self, _: &mut TVarContext) -> CheckResult<()> {
        let ok = match (self, other) {
            (&Numbers::One(a), &Numbers::One(b)) => a == b,
            (&Numbers::One(a), &Numbers::Some(ref b)) => b.contains(&a),
            (&Numbers::Some(ref a), &Numbers::One(b)) => a.len() == 1 && a.contains(&b),
            (&Numbers::Some(ref a), &Numbers::Some(ref b)) => a.is_subset(b),

            (&Numbers::One(..), _) => true,
            (_, &Numbers::One(..)) => false,

            (&Numbers::Some(..), _) => true,
            (_, &Numbers::Some(..)) => false,

            (&Numbers::Int, _) => true,
            (_, &Numbers::Int) => false,

            (&Numbers::All, &Numbers::All) => true,
        };

        if ok { Ok(()) } else { error_not_sub(self, other) }
    }

    fn assert_eq(&self, other: &Self, _ctx: &mut TVarContext) -> CheckResult<()> {
        if *self == *other { Ok(()) } else { error_not_eq(self, other) }
    }
}

impl PartialEq for Numbers {
    fn eq(&self, other: &Numbers) -> bool {
        match (self, other) {
            (&Numbers::All, &Numbers::All) => true,
            (&Numbers::Int, &Numbers::Int) => true,
            (&Numbers::Some(ref a), &Numbers::Some(ref b)) => *a == *b,
            (&Numbers::Some(ref a), &Numbers::One(b)) => a.len() == 1 && a.contains(&b),
            (&Numbers::One(a), &Numbers::Some(ref b)) => b.len() == 1 && b.contains(&a),
            (&Numbers::One(a), &Numbers::One(b)) => a == b,
            (_, _) => false,
        }
    }
}

impl fmt::Debug for Numbers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Numbers::All => write!(f, "number"),
            Numbers::Int => write!(f, "integer"),
            Numbers::Some(ref set) => {
                try!(write!(f, "("));
                let mut first = true;
                for v in set.iter() {
                    if first { first = false; } else { try!(write!(f, "|")); }
                    try!(write!(f, "{:?}", *v));
                }
                write!(f, ")")
            }
            Numbers::One(v) => write!(f, "{:?}", v),
        }
    }
}

#[derive(Clone)]
pub enum Strings {
    One(Str),
    Some(HashSet<Str>),
    All,
}

impl Strings {
    fn assert_sup_str(&self, other: &Str, _: &mut TVarContext) -> CheckResult<()> {
        let ok = match *self {
            Strings::One(ref s) => *s == *other,
            Strings::Some(ref set) => set.contains(other),
            Strings::All => true,
        };

        if ok { Ok(()) } else { error_not_sub(self, other) }
    }
}

impl Lattice for Strings {
    type Output = Option<Strings>;

    fn normalize(self) -> Option<Strings> {
        match self {
            Strings::Some(set) => match set.len() {
                0 => None,
                1 => Some(Strings::One(set.into_iter().next().unwrap())),
                _ => Some(Strings::Some(set)),
            },
            str => Some(str),
        }
    }

    fn union(self, other: Strings, _: &mut TVarContext) -> Option<Strings> {
        match (self, other) {
            (Strings::All, _) => Some(Strings::All),
            (_, Strings::All) => Some(Strings::All),

            (Strings::Some(mut a), Strings::Some(b)) => {
                a.extend(b.into_iter());
                Some(Strings::Some(a))
            }
            (Strings::Some(mut a), Strings::One(b)) => {
                a.insert(b);
                Some(Strings::Some(a))
            }
            (Strings::One(a), Strings::Some(mut b)) => {
                b.insert(a);
                Some(Strings::Some(b))
            }
            (Strings::One(a), Strings::One(b)) => {
                if a == b {
                    Some(Strings::One(a))
                } else {
                    let mut ab = HashSet::new();
                    ab.insert(a);
                    ab.insert(b);
                    Some(Strings::Some(ab))
                }
            }
        }
    }

    fn intersect(self, other: Strings, _: &mut TVarContext) -> Option<Strings> {
        match (self, other) {
            (Strings::One(a), Strings::One(b)) =>
                if a == b { Some(Strings::One(a)) } else { None },
            (Strings::One(a), Strings::Some(b)) =>
                if b.contains(&a) { Some(Strings::One(a)) } else { None },
            (Strings::Some(a), Strings::One(b)) =>
                if a.contains(&b) { Some(Strings::One(b)) } else { None },
            (Strings::Some(a), Strings::Some(b)) => {
                let set: HashSet<Str> = a.intersection(&b).cloned().collect();
                if set.is_empty() { None } else { Some(Strings::Some(set)) }
            }

            (Strings::One(v), _) => Some(Strings::One(v)),
            (_, Strings::One(v)) => Some(Strings::One(v)),

            (Strings::Some(set), _) => Some(Strings::Some(set)),
            (_, Strings::Some(set)) => Some(Strings::Some(set)),

            (Strings::All, Strings::All) => Some(Strings::All),
        }
    }

    fn assert_sub(&self, other: &Self, _: &mut TVarContext) -> CheckResult<()> {
        let ok = match (self, other) {
            (&Strings::One(ref a), &Strings::One(ref b)) => *a == *b,
            (&Strings::One(ref a), &Strings::Some(ref b)) => b.contains(a),
            (&Strings::Some(ref a), &Strings::One(ref b)) => a.len() == 1 && a.contains(b),
            (&Strings::Some(ref a), &Strings::Some(ref b)) => a.is_subset(b),

            (&Strings::One(..), _) => true,
            (_, &Strings::One(..)) => false,

            (&Strings::Some(..), _) => true,
            (_, &Strings::Some(..)) => false,

            (&Strings::All, &Strings::All) => true,
        };

        if ok { Ok(()) } else { error_not_sub(self, other) }
    }

    fn assert_eq(&self, other: &Self, _ctx: &mut TVarContext) -> CheckResult<()> {
        if *self == *other { Ok(()) } else { error_not_eq(self, other) }
    }
}

impl PartialEq for Strings {
    fn eq(&self, other: &Strings) -> bool {
        match (self, other) {
            (&Strings::All, &Strings::All) => true,
            (&Strings::Some(ref a), &Strings::Some(ref b)) => *a == *b,
            (&Strings::Some(ref a), &Strings::One(ref b)) => a.len() == 1 && a.contains(b),
            (&Strings::One(ref a), &Strings::Some(ref b)) => b.len() == 1 && b.contains(a),
            (&Strings::One(ref a), &Strings::One(ref b)) => *a == *b,
            (_, _) => false,
        }
    }
}

impl fmt::Debug for Strings {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Strings::All => write!(f, "string"),
            Strings::Some(ref set) => {
                try!(write!(f, "("));
                let mut first = true;
                for s in set.iter() {
                    if first { first = false; } else { try!(write!(f, "|")); }
                    try!(write!(f, "{:?}", *s));
                }
                write!(f, ")")
            }
            Strings::One(ref s) => write!(f, "{:?}", *s),
        }
    }
}

#[derive(Clone)]
pub enum Tables {
    Empty,
    Record(HashMap<Str, Ty>),
    Tuple(Vec<Ty>),
    Array(Ty),
    Map(Ty, Ty),
    All,
}

impl Tables {
    pub fn lift_to_map(self, ctx: &mut TVarContext) -> Tables {
        match self {
            Tables::Empty => Tables::Empty,
            Tables::Record(fields) => {
                let mut value = T::None;
                for (_, ty) in fields { value = value.union(*ty, ctx); }
                Tables::Map(Box::new(T::string()), Box::new(value))
            },
            Tables::Tuple(fields) => {
                let mut value = T::None;
                for ty in fields { value = value.union(*ty, ctx); }
                Tables::Map(Box::new(T::integer()), Box::new(value))
            },
            Tables::Array(value) => Tables::Map(Box::new(T::integer()), value),
            Tables::Map(key, value) => Tables::Map(key, value),
            Tables::All => Tables::All,
        }
    }

    pub fn insert(self, key: Option<T<'static>>, value: T<'static>,
                  ctx: &mut TVarContext) -> Tables {
        // a single string key is special
        if let Some(ref key) = key {
            if key.flags() == T_STRING {
                if let Some(&Strings::One(ref s)) = key.has_strings() {
                    match self {
                        Tables::Empty => {
                            let mut fields = HashMap::new();
                            fields.insert(s.to_owned(), Box::new(value));
                            return Tables::Record(fields);
                        }
                        Tables::Record(mut fields) => {
                            // should override a duplicate field if any
                            fields.insert(s.to_owned(), Box::new(value));
                            return Tables::Record(fields);
                        }
                        _ => {}
                    }
                }
            }
        }

        // otherwise do not try to make a record
        match (key, self) {
            // XXX tuple?
            (None, Tables::Empty) => Tables::Array(Box::new(value)),
            (None, Tables::Array(t)) =>
                Tables::Array(Box::new(value.union(*t, ctx))),
            (None, Tables::Tuple(mut fields)) => {
                fields.push(Box::new(value));
                Tables::Tuple(fields)
            },

            (Some(key), Tables::Empty) => Tables::Map(Box::new(key), Box::new(value)),

            // fall back to the map when in doubt
            (key, tab) => match tab.lift_to_map(ctx) {
                Tables::Map(key_, value_) => {
                    let key = key.unwrap_or(T::integer()).union(*key_, ctx);
                    let value = value.union(*value_, ctx);
                    Tables::Map(Box::new(key), Box::new(value))
                },
                tab => tab,
            }
        }
    }
}

impl Lattice for Tables {
    type Output = Option<Tables>;

    fn normalize(self) -> Option<Tables> {
        match self {
            Tables::Record(fields) => {
                if fields.is_empty() { return Some(Tables::Empty); }

                let norm_kv = |(k, v): (Str, Ty)| {
                    let v = v.normalize();
                    if v.is_none() { None } else { Some((k, v)) }
                };
                let fields: HashMap<Str, Ty> = fields.into_iter().filter_map(norm_kv).collect();
                if fields.is_empty() {
                    Some(Tables::Empty)
                } else {
                    Some(Tables::Record(fields))
                }
            },

            Tables::Tuple(fields) => {
                if fields.is_empty() { return Some(Tables::Empty); }

                let norm = |v: Ty| {
                    let v = v.normalize();
                    if v.is_none() { None } else { Some(v) }
                };
                if let Some(fields) = fields.into_iter().map(norm).collect() {
                    Some(Tables::Tuple(fields))
                } else {
                    Some(Tables::Empty)
                }
            },

            tab => Some(tab),
        }
    }

    fn union(self, other: Tables, ctx: &mut TVarContext) -> Option<Tables> {
        fn union_rec_tup(rec: HashMap<Str, Ty>, mut tup: Vec<Ty>,
                         ctx: &mut TVarContext) -> Tables {
            let mut uty = tup.pop().unwrap();
            for ty in tup {
                uty = uty.union(ty, ctx);
            }
            for (_, ty) in rec {
                uty = uty.union(ty, ctx);
            }
            Tables::Map(Box::new(T::integer().union(T::string(), ctx)), uty)
        }

        fn union_rec_map(fields: HashMap<Str, Ty>, key: Ty, value: Ty,
                         ctx: &mut TVarContext) -> Tables {
            let mut uty = value;
            for (_, ty) in fields {
                uty = uty.union(ty, ctx);
            }
            Tables::Map(key.union(Box::new(T::string()), ctx), uty)
        }

        fn union_tup_map(fields: Vec<Ty>, key: Ty, value: Ty, ctx: &mut TVarContext) -> Tables {
            let mut uty = value;
            for ty in fields {
                uty = uty.union(ty, ctx);
            }
            Tables::Map(key.union(Box::new(T::integer()), ctx), uty)
        }

        let tab = match (self, other) {
            (Tables::Empty, tab) => tab,
            (tab, Tables::Empty) => tab,

            (Tables::All, _) => Tables::All,
            (_, Tables::All) => Tables::All,

            (Tables::Record(mut fields1), Tables::Record(fields2)) => {
                for (k, v2) in fields2 {
                    if let Some(v1) = fields1.remove(&k) {
                        fields1.insert(k, v1.union(v2, ctx));
                    } else {
                        fields1.insert(k, v2);
                    }
                }
                Tables::Record(fields1)
            },

            (Tables::Record(fields1), Tables::Tuple(fields2)) =>
                union_rec_tup(fields1, fields2, ctx),
            (Tables::Tuple(fields1), Tables::Record(fields2)) =>
                union_rec_tup(fields2, fields1, ctx),

            (Tables::Record(fields), Tables::Array(value)) =>
                union_rec_map(fields, Box::new(T::integer()), value, ctx),
            (Tables::Array(value), Tables::Record(fields)) =>
                union_rec_map(fields, Box::new(T::integer()), value, ctx),

            (Tables::Record(fields), Tables::Map(key, value)) =>
                union_rec_map(fields, key, value, ctx),
            (Tables::Map(key, value), Tables::Record(fields)) =>
                union_rec_map(fields, key, value, ctx),

            (Tables::Tuple(mut fields1), Tables::Tuple(mut fields2)) => {
                if fields1.len() < fields2.len() {
                    fields1.resize(fields2.len(), Box::new(T::Nil));
                } else if fields1.len() > fields2.len() {
                    fields2.resize(fields1.len(), Box::new(T::Nil));
                }
                let tys = fields1.into_iter().zip(fields2.into_iter());
                Tables::Tuple(tys.map(|(lty, rty)| lty.union(rty, ctx)).collect())
            },

            (Tables::Tuple(fields), Tables::Array(value)) =>
                union_tup_map(fields, Box::new(T::integer()), value, ctx),
            (Tables::Array(value), Tables::Tuple(fields)) =>
                union_tup_map(fields, Box::new(T::integer()), value, ctx),

            (Tables::Tuple(fields), Tables::Map(key, value)) =>
                union_tup_map(fields, key, value, ctx),
            (Tables::Map(key, value), Tables::Tuple(fields)) =>
                union_tup_map(fields, key, value, ctx),

            (Tables::Array(value1), Tables::Array(value2)) =>
                Tables::Array(value1.union(value2, ctx)),

            (Tables::Map(key1, value1), Tables::Map(key2, value2)) =>
                Tables::Map(key1.union(key2, ctx), value1.union(value2, ctx)),

            (Tables::Array(value1), Tables::Map(key2, value2)) =>
                Tables::Map(key2.union(Box::new(T::integer()), ctx), value1.union(value2, ctx)),
            (Tables::Map(key1, value1), Tables::Array(value2)) =>
                Tables::Map(key1.union(Box::new(T::integer()), ctx), value1.union(value2, ctx)),
        };

        Some(tab)
    }

    fn intersect(self, other: Tables, ctx: &mut TVarContext) -> Option<Tables> {
        fn intersect_tup_arr(fields: Vec<Ty>, value: Ty,
                             ctx: &mut TVarContext) -> Tables {
            let mut newfields = Vec::new();
            for ty in fields {
                let v = ty.intersect(value.clone(), ctx);
                if v.is_none() { return Tables::Empty; }
                newfields.push(v);
            }
            Tables::Tuple(newfields)
        }

        fn intersect_rec_map(fields: HashMap<Str, Ty>, key: Ty, value: Ty,
                             ctx: &mut TVarContext) -> Tables {
            fn merge<F: Fn(&Str) -> bool>(fields: HashMap<Str, Ty>, value: Ty,
                                          ctx: &mut TVarContext, cond: F) -> Tables {
                let mut newfields = HashMap::new();
                for (k, ty) in fields {
                    if cond(&k) { 
                        let v = ty.intersect(value.clone(), ctx);
                        if !v.is_none() { newfields.insert(k, v); }
                    }
                }
                Tables::Record(newfields)
            }

            let key = Union::from(*key);
            if key.has_dynamic {
                merge(fields, value, ctx, |_| true)
            } else {
                match key.strings {
                    None => Tables::Empty,
                    Some(Strings::One(ref s)) => merge(fields, value, ctx, |k| *s == *k),
                    Some(Strings::Some(ref set)) => merge(fields, value, ctx, |k| set.contains(k)),
                    Some(Strings::All) => merge(fields, value, ctx, |_| true),
                }
            }
        }

        fn intersect_tup_map(fields: Vec<Ty>, key: Ty, value: Ty,
                             ctx: &mut TVarContext) -> Tables {
            fn merge<F: Fn(i32) -> bool>(fields: Vec<Ty>, value: Ty,
                                         ctx: &mut TVarContext, cond: F) -> Tables {
                let mut newfields = Vec::new();
                for (k, ty) in fields.into_iter().enumerate() {
                    if cond(k as i32) { 
                        let v = ty.intersect(value.clone(), ctx);
                        if !v.is_none() { newfields.push(v); }
                    } else {
                        newfields.push(Box::new(T::None));
                    }
                }
                Tables::Tuple(newfields)
            };

            let key = Union::from(*key);
            if key.has_dynamic {
                merge(fields, value, ctx, |_| true)
            } else {
                match key.numbers {
                    None => Tables::Empty,
                    Some(Numbers::One(v)) => merge(fields, value, ctx, |k| k == v),
                    Some(Numbers::Some(ref set)) => merge(fields, value, ctx, |k| set.contains(&k)),
                    Some(Numbers::Int) | Some(Numbers::All) => merge(fields, value, ctx, |_| true),
                }
            }
        }

        fn intersect_arr_map(elem: Ty, key: Ty, value: Ty,
                             ctx: &mut TVarContext) -> Tables {
            let key = Union::from(*key);
            if key.has_dynamic {
                Tables::Array(elem.intersect(value, ctx))
            } else {
                match key.numbers {
                    None | Some(Numbers::One(..)) | Some(Numbers::Some(..)) => Tables::Empty,
                    Some(Numbers::Int) | Some(Numbers::All) =>
                        Tables::Array(elem.intersect(value, ctx)),
                }
            }
        }

        let tab = match (self, other) {
            (Tables::Empty, _) => Tables::Empty,
            (_, Tables::Empty) => Tables::Empty,

            (Tables::All, tab) => tab,
            (tab, Tables::All) => tab,

            (Tables::Record(mut fields1), Tables::Record(fields2)) => {
                let mut fields = HashMap::new();
                for (k, v2) in fields2 {
                    if let Some(v1) = fields1.remove(&k) {
                        let v = v1.intersect(v2, ctx);
                        if !v.is_none() { fields.insert(k, v); }
                    }
                }
                Tables::Record(fields)
            },

            (Tables::Record(_), Tables::Tuple(_)) => Tables::Empty,
            (Tables::Tuple(_), Tables::Record(_)) => Tables::Empty,

            (Tables::Record(_), Tables::Array(_)) => Tables::Empty,
            (Tables::Array(_), Tables::Record(_)) => Tables::Empty,

            (Tables::Record(fields), Tables::Map(key, value)) =>
                intersect_rec_map(fields, key, value, ctx),
            (Tables::Map(key, value), Tables::Record(fields)) =>
                intersect_rec_map(fields, key, value, ctx),

            (Tables::Tuple(fields1), Tables::Tuple(fields2)) => {
                let mut fields = Vec::new();
                for (ty1, ty2) in fields1.into_iter().zip(fields2.into_iter()) {
                    let ty = ty1.intersect(ty2, ctx);
                    if ty.is_none() { return Some(Tables::Empty); }
                    fields.push(ty);
                }
                Tables::Tuple(fields)
            },

            (Tables::Tuple(fields), Tables::Array(value)) =>
                intersect_tup_arr(fields, value, ctx),
            (Tables::Array(value), Tables::Tuple(fields)) =>
                intersect_tup_arr(fields, value, ctx),

            (Tables::Tuple(fields), Tables::Map(key, value)) =>
                intersect_tup_map(fields, key, value, ctx),
            (Tables::Map(key, value), Tables::Tuple(fields)) =>
                intersect_tup_map(fields, key, value, ctx),

            (Tables::Array(value1), Tables::Array(value2)) =>
                Tables::Array(value1.intersect(value2, ctx)),

            (Tables::Map(key1, value1), Tables::Map(key2, value2)) =>
                Tables::Map(key1.intersect(key2, ctx), value1.intersect(value2, ctx)),

            (Tables::Array(value1), Tables::Map(key2, value2)) =>
                intersect_arr_map(value1, key2, value2, ctx),
            (Tables::Map(key1, value1), Tables::Array(value2)) =>
                intersect_arr_map(value2, key1, value1, ctx),
        };

        Some(tab)
    }

    fn assert_sub(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        let ok = match (self, other) {
            (&Tables::Empty, _) => true,
            (_, &Tables::Empty) => false,

            (&Tables::All, _) => false,
            (_, &Tables::All) => true,

            (&Tables::Record(ref a), &Tables::Record(ref b)) => {
                for (k, av) in a {
                    let none = T::None;
                    let bv = b.get(k).map_or(&none, |t| &*t);
                    try!((**av).assert_sub(bv, ctx));
                }
                true
            },

            (&Tables::Record(..), &Tables::Tuple(..)) => false,
            (&Tables::Tuple(..), &Tables::Record(..)) => false,

            (&Tables::Record(..), &Tables::Array(..)) => false,
            (&Tables::Array(..), &Tables::Record(..)) => false,

            (&Tables::Record(ref fields), &Tables::Map(ref key, ref value)) => {
                let mut ok = true;
                for (k, ty) in fields {
                    if let Some(str) = key.has_strings() {
                        try!(str.assert_sup_str(k, ctx));
                    } else {
                        ok = false;
                        break;
                    }
                    try!(ty.assert_sub(value, ctx));
                }
                ok
            },
            (&Tables::Map(..), &Tables::Record(..)) => false,

            (&Tables::Tuple(ref fields1), &Tables::Tuple(ref fields2)) => {
                for (ty1, ty2) in fields1.iter().zip(fields2.iter()) {
                    try!(ty1.assert_sub(ty2, ctx));
                }
                fields1.len() <= fields2.len()
            },

            (&Tables::Tuple(ref fields), &Tables::Array(ref value)) => {
                for ty in fields { try!(ty.assert_sub(value, ctx)); }
                true
            },
            (&Tables::Array(..), &Tables::Tuple(..)) => false,

            (&Tables::Tuple(ref fields), &Tables::Map(ref key, ref value)) => {
                for (i, ty) in fields.iter().enumerate() {
                    try!(T::int(i as i32).assert_sub(key, ctx));
                    try!(ty.assert_sub(value, ctx));
                }
                true
            },
            (&Tables::Map(..), &Tables::Tuple(..)) => false,

            (&Tables::Array(ref value1), &Tables::Array(ref value2)) => {
                try!(value1.assert_sub(value2, ctx));
                true
            },

            (&Tables::Map(ref key1, ref value1), &Tables::Map(ref key2, ref value2)) => {
                try!(key1.assert_sub(key2, ctx));
                try!(value1.assert_sub(value2, ctx));
                true
            },

            (&Tables::Array(ref value1), &Tables::Map(ref key2, ref value2)) => {
                try!(T::integer().assert_sub(key2, ctx));
                try!(value1.assert_sub(value2, ctx));
                true
            },
            (&Tables::Map(..), &Tables::Array(..)) => false,
        };

        if ok { Ok(()) } else { error_not_sub(self, other) }
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        let ok = match (self, other) {
            (&Tables::All, &Tables::All) => true,
            (&Tables::Empty, &Tables::Empty) => true,
            (&Tables::Array(ref a), &Tables::Array(ref b)) => return a.assert_eq(b, ctx),
            (&Tables::Map(ref ak, ref av), &Tables::Map(ref bk, ref bv)) => {
                try!(ak.assert_eq(bk, ctx));
                try!(av.assert_eq(bv, ctx));
                true
            }
            (&Tables::Tuple(ref a), &Tables::Tuple(ref b)) => {
                if a.len() == b.len() {
                    for (i, j) in a.iter().zip(b.iter()) {
                        try!(i.assert_eq(j, ctx));
                    }
                    true
                } else {
                    false
                }
            }
            (&Tables::Record(ref a), &Tables::Record(ref b)) => {
                for (k, va) in a {
                    if let Some(vb) = b.get(k) {
                        try!(va.assert_eq(vb, ctx));
                    } else {
                        return error_not_eq(self, other);
                    }
                }
                for (k, vb) in b {
                    if !a.contains_key(k) { return error_not_eq(self, other); }
                }
                true
            }
            (_, _) => false,
        };

        if ok { Ok(()) } else { error_not_eq(self, other) }
    }
}

impl PartialEq for Tables {
    fn eq(&self, other: &Tables) -> bool {
        match (self, other) {
            (&Tables::All, &Tables::All) => true,
            (&Tables::Empty, &Tables::Empty) => true,

            (&Tables::Array(ref a), &Tables::Array(ref b)) => *a == *b,
            (&Tables::Map(ref ak, ref av), &Tables::Map(ref bk, ref bv)) =>
                *ak == *bk && *av == *bv,

            (&Tables::Tuple(ref a), &Tables::Tuple(ref b)) => *a == *b,
            (&Tables::Tuple(ref a), &Tables::Record(ref b)) => a.is_empty() && b.is_empty(),
            (&Tables::Tuple(ref a), &Tables::Empty) => a.is_empty(),
            (&Tables::Record(ref a), &Tables::Tuple(ref b)) => a.is_empty() && b.is_empty(),
            (&Tables::Record(ref a), &Tables::Record(ref b)) => *a == *b,
            (&Tables::Record(ref a), &Tables::Empty) => a.is_empty(),
            (&Tables::Empty, &Tables::Tuple(ref b)) => b.is_empty(),
            (&Tables::Empty, &Tables::Record(ref b)) => b.is_empty(),

            (_, _) => false,
        }
    }
}

impl fmt::Debug for Tables {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Tables::All => write!(f, "table"),
            Tables::Empty => write!(f, "{{}}"),
            Tables::Record(ref fields) => {
                try!(write!(f, "{{"));
                let mut first = true;
                for (name, t) in fields.iter() {
                    if first { first = false; } else { try!(write!(f, ", ")); }
                    try!(write!(f, "{:?} = {:?}", *name, *t));
                }
                write!(f, "}}")
            }
            Tables::Tuple(ref fields) => {
                try!(write!(f, "{{"));
                let mut first = true;
                for t in fields.iter() {
                    if first { first = false; } else { try!(write!(f, ", ")); }
                    try!(write!(f, "{:?}", *t));
                }
                write!(f, "}}")
            }
            Tables::Array(ref t) => write!(f, "{{{:?}}}", *t),
            Tables::Map(ref k, ref v) => write!(f, "{{[{:?}] = {:?}}}", *k, *v),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Function {
    pub args: Seq<Ty>,
    pub returns: Seq<Ty>,
}

impl Function {
    fn assert_sub(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        try!(other.args.assert_sub(&self.args, ctx)); // contravariant
        try!(self.returns.assert_sub(&other.returns, ctx)); // covariant
        Ok(())
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
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

impl Functions {
    pub fn from(func: Function) -> Functions {
        Functions::Simple(func)
    }
}

impl Lattice for Functions {
    type Output = Option<Functions>;

    fn normalize(self) -> Option<Functions> {
        if let Functions::Multi(mut set) = self {
            match set.len() {
                0 => None,
                1 => Some(Functions::Simple(set.into_iter().next().unwrap())),
                _ => Some(Functions::Multi(set)),
            }
        } else {
            Some(self)
        }
    }

    fn union(self, other: Functions, _: &mut TVarContext) -> Option<Functions> {
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

    fn intersect(self, other: Functions, _: &mut TVarContext) -> Option<Functions> {
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

    fn assert_sub(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        let ok = match (self, other) {
            (&Functions::All, _) => false,
            (_, &Functions::All) => true,

            (&Functions::Simple(ref a), &Functions::Simple(ref b)) => return a.assert_sub(b, ctx),
            (&Functions::Simple(ref a), &Functions::Multi(ref b)) => {
                // a <: b1 /\ b2 === a <: b1 AND a <: b2
                for i in b { try!(a.assert_sub(i, ctx)); }
                true
            }
            (&Functions::Multi(ref a), &Functions::Simple(ref b)) => {
                // a1 /\ a2 <: b === a1 <: b OR a2 <: b
                //for i in a { try!(err_on_instantiation(ctx, |ctx| i.assert_sub(b, ctx))); }
                //true
                unimplemented!()
            }
            (&Functions::Multi(ref a), &Functions::Multi(ref b)) => {
                // a1 /\ a2 <: b1 /\ b2 === (a1 /\ a2 <: b1) AND (a1 /\ a2 <: b2)
                //                      === (a1 <: b1 OR a2 <: b1) AND (a1 <: b2 OR a2 <: b2)
                unimplemented!()
            }
        };

        if ok { Ok(()) } else { error_not_sub(self, other) }
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
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

// "true" types. constructed out of `T` "simple" types.
#[derive(Clone, PartialEq)]
pub struct Union {
    pub has_dynamic: bool, // XXX
    pub has_nil: bool,
    pub has_true: bool,
    pub has_false: bool,
    pub numbers: Option<Numbers>,
    pub strings: Option<Strings>,
    pub tables: Option<Tables>,
    pub functions: Option<Functions>,
    pub tvar: Option<TVar>,
}

impl Union {
    pub fn from<'a>(ty: T<'a>) -> Union {
        let mut u = Union {
            has_dynamic: false, has_nil: false, has_true: false, has_false: false,
            numbers: None, strings: None, tables: None, functions: None, tvar: None,
        };

        fn singleton<X: Hash + Eq>(x: X) -> HashSet<X> {
            let mut set = HashSet::with_capacity(1);
            set.insert(x);
            set
        }

        match ty {
            T::Dynamic => { u.has_dynamic = true; }
            T::None    => {}
            T::Nil     => { u.has_nil = true; }
            T::Boolean => { u.has_true = true; u.has_false = true; }
            T::True    => { u.has_true = true; }
            T::False   => { u.has_false = true; }

            T::Numbers(num)    => { u.numbers = Some(num.into_owned()); }
            T::Strings(str)    => { u.strings = Some(str.into_owned()); }
            T::Tables(tab)     => { u.tables = Some(tab.into_owned()); }
            T::Functions(func) => { u.functions = Some(func.into_owned()); }
            T::TVar(tv)        => { u.tvar = Some(tv); }

            T::Union(u) => return u.into_owned() // ignore `u` above
        }

        u
    }

    pub fn flags(&self) -> Flags {
        let mut flags = T_NONE;
        if self.has_dynamic  { flags = flags | T_DYNAMIC; }
        if self.has_nil      { flags = flags | T_NIL; }
        if self.has_true     { flags = flags | T_TRUE; }
        if self.has_false    { flags = flags | T_FALSE; }
        match self.numbers {
            None => {}
            Some(Numbers::All) => { flags = flags | T_NUMBER; }
            Some(_)  => { flags = flags | T_INTEGER; }
        }
        if self.strings.is_some()   { flags = flags | T_STRING; }
        if self.tables.is_some()    { flags = flags | T_TABLE; }
        if self.functions.is_some() { flags = flags | T_FUNCTION; }
        flags
    }

    pub fn visit<'a, E, F>(&'a self, mut f: F) -> Result<(), E>
            where F: FnMut(T<'a>) -> Result<(), E> {
        // dynamic type eschews every other types
        if self.has_dynamic { return f(T::Dynamic); }

        if self.has_nil { try!(f(T::Nil)); }
        if self.has_true {
            if self.has_false { try!(f(T::Boolean)); } else { try!(f(T::True)); }
        } else if self.has_false {
            try!(f(T::False));
        }
        if let Some(ref num) = self.numbers { try!(f(T::Numbers(Cow::Borrowed(num)))) }
        if let Some(ref str) = self.strings { try!(f(T::Strings(Cow::Borrowed(str)))) }
        if let Some(ref tab) = self.tables { try!(f(T::Tables(Cow::Borrowed(tab)))) }
        if let Some(ref func) = self.functions { try!(f(T::Functions(Cow::Borrowed(func)))) }
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

    pub fn accept(&self, rhs: &Union) -> bool {
        let flags = self.flags();
        let rhsflags = rhs.flags();
        if flags & rhsflags != rhsflags { return false; }

        // not covered by flags
        if rhs.numbers.assert_sub(&self.numbers, &mut ()).is_err() { return false; }
        if rhs.strings.assert_sub(&self.strings, &mut ()).is_err() { return false; }

        true
    }
}

impl Lattice for Union {
    type Output = Union;

    fn normalize(mut self) -> Self {
        self.numbers   = self.numbers.normalize();
        self.strings   = self.strings.normalize();
        self.tables    = self.tables.normalize();
        self.functions = self.functions.normalize();
        self
    }

    fn union(mut self, other: Union, ctx: &mut TVarContext) -> Union {
        self.has_dynamic |= other.has_dynamic;
        self.has_nil     |= other.has_nil;
        self.has_true    |= other.has_true;
        self.has_false   |= other.has_false;

        self.numbers   = self.numbers.union(other.numbers, ctx);
        self.strings   = self.strings.union(other.strings, ctx);
        self.tables    = self.tables.union(other.tables, ctx);
        self.functions = self.functions.union(other.functions, ctx);

        self.tvar = match (self.tvar, other.tvar) {
            (Some(a), Some(b)) => Some(a.union(b, ctx)),
            (a, b) => a.or(b),
        };

        self
    }

    fn intersect(mut self, other: Union, ctx: &mut TVarContext) -> Union {
        self.has_dynamic &= other.has_dynamic;
        self.has_nil     &= other.has_nil;
        self.has_true    &= other.has_true;
        self.has_false   &= other.has_false;

        self.numbers   = self.numbers.intersect(other.numbers, ctx);
        self.strings   = self.strings.intersect(other.strings, ctx);
        self.tables    = self.tables.intersect(other.tables, ctx);
        self.functions = self.functions.intersect(other.functions, ctx);

        self.tvar = match (self.tvar, other.tvar) {
            (Some(a), Some(b)) => Some(a.intersect(b, ctx)),
            (a, b) => None,
        };

        self
    }

    fn assert_sub(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        // exit early if either side is dynamic
        if self.has_dynamic || other.has_dynamic { return Ok(()); }

        if (self.has_nil && !other.has_nil) || (self.has_true && !other.has_true) ||
                                               (self.has_false && !other.has_false) {
            return error_not_sub(self, other);
        }

        try!(self.numbers.assert_sub(&other.numbers, &mut ()));
        try!(self.strings.assert_sub(&other.strings, &mut ()));

        // XXX err on unions with possible overlapping instantiation for now
        let count = if self.tables.is_some() { 1 } else { 0 } +
                    if self.functions.is_some() { 1 } else { 0 } +
                    if self.tvar.is_some() { 1 } else { 0 };
        if count > 1 { unimplemented!() }

        let count = if other.tables.is_some() { 1 } else { 0 } +
                    if other.functions.is_some() { 1 } else { 0 } +
                    if other.tvar.is_some() { 1 } else { 0 };
        if count > 1 { unimplemented!() }

        try!(self.tables.assert_sub(&other.tables, ctx));
        try!(self.functions.assert_sub(&other.functions, ctx));

        match (self.tvar, other.tvar) {
            (Some(a), Some(b)) => a.assert_sub(&b, ctx),
            (Some(a), None) => error_not_sub(self, other),
            (None, _) => Ok(()),
        }
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        // exit early if either side is dynamic
        if self.has_dynamic || other.has_dynamic { return Ok(()); }

        match (self.tvar, self.flags(), other.tvar, other.flags()) {
            (Some(a), T_NONE, Some(b), T_NONE) =>
                return ctx.assert_tvar_eq_tvar(a, b),
            (Some(a), T_NONE, _, _) =>
                return ctx.assert_tvar_eq(a, &T::Union(Cow::Borrowed(other))),
            (_, _, Some(b), T_NONE) =>
                return ctx.assert_tvar_eq(b, &T::Union(Cow::Borrowed(self))),
            (Some(_), _, _, _) | (_, _, Some(_), _) =>
                // XXX if we have a type variable in the union,
                // the type variable essentially eschews all differences between two input types
                // and there is no error condition except for conflicting instantiation.
                unimplemented!(),
            (None, _, None, _) => {}
        }

        if self.has_nil != other.has_nil || self.has_true != other.has_true ||
                                            self.has_false != other.has_false {
            return error_not_eq(self, other);
        }

        try!(self.numbers.assert_eq(&other.numbers, &mut ()));
        try!(self.strings.assert_eq(&other.strings, &mut ()));
        try!(self.tables.assert_eq(&other.tables, ctx));
        try!(self.functions.assert_eq(&other.functions, ctx));
        Ok(())
    }
}

impl fmt::Debug for Union {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "("));
        let mut first = true;
        try!(self.visit(|ty| {
            if first {
                first = false;
            } else {
                try!(write!(f, "|"));
            }
            fmt::Debug::fmt(&ty, f)
        }));
        write!(f, ")")
    }
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
    pub fn number()      -> T<'a> { T::Numbers(Cow::Owned(Numbers::All)) }
    pub fn integer()     -> T<'a> { T::Numbers(Cow::Owned(Numbers::Int)) }
    pub fn int(v: i32)   -> T<'a> { T::Numbers(Cow::Owned(Numbers::One(v))) }
    pub fn string()      -> T<'a> { T::Strings(Cow::Owned(Strings::All)) }
    pub fn str(s: Str)   -> T<'a> { T::Strings(Cow::Owned(Strings::One(s))) }
    pub fn table()       -> T<'a> { T::Tables(Cow::Owned(Tables::All)) }
    pub fn empty_table() -> T<'a> { T::Tables(Cow::Owned(Tables::Empty)) }
    pub fn function()    -> T<'a> { T::Functions(Cow::Owned(Functions::All)) }

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

            (&T::TVar(a), &T::TVar(b)) => return a.assert_sub(&b, ctx),
            (a, &T::TVar(b)) => return ctx.assert_tvar_sup(b, a),
            (&T::TVar(a), b) => return ctx.assert_tvar_sub(a, b),

            (&T::Union(ref a), &T::Union(ref b)) => return a.assert_sub(b, ctx),
            (&T::Union(ref a), b) => {
                // a1 \/ a2 <: b === a1 <: b AND a2 <: b
                return a.visit(|i| i.assert_sub(b, ctx));
            },
            (a, &T::Union(ref b)) => {
                // a <: b1 \/ b2 === a <: b1 OR a <: b2
                false // XXX
            },

            (_, _) => false,
        };

        if ok { Ok(()) } else { error_not_sub(self, other) }
    }

    fn assert_eq(&self, other: &T<'b>, ctx: &mut TVarContext) -> CheckResult<()> {
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
            assert_eq!(t1.assert_sub(&t2, &mut ctx), Ok(()));
            assert_eq!(T::TVar(v1).assert_sub(&T::string(), &mut ctx), Ok(()));
            assert_eq!(T::integer().assert_sub(&T::TVar(v2), &mut ctx), Ok(()));
            assert!(t1.assert_eq(&t2, &mut ctx).is_err());
        }
    }
}

