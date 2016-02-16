use std::fmt;
use std::hash::Hash;
use std::borrow::{Cow, ToOwned};
use std::collections::{HashSet, HashMap};
use kailua_syntax::{K, Kind, Str};

use env::Env;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Builtin {
    Require,        // (fixed string) -> table & sideeffect
}

pub trait Unionable<Other = Self> {
    type Output;
    fn union(self, other: Other) -> Self::Output;
}

// used to create constraints for Self to Env
pub trait Unifiable {
    fn assert_sub(&self, other: &Self, env: &mut Env) -> bool;
    fn assert_eq(&self, other: &Self, env: &mut Env) -> bool;
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct TVar(pub u32);

/*
impl Unifiable for TVar {
    fn assert_sub(&self, other: &Self, env: &mut Env) -> bool {
        env.context().assert_tvar_sub(*self, *other)
    }

    fn assert_eq(&self, other: &Self, env: &mut Env) -> bool {
        env.context().assert_tvar_eq(*self, *other)
    }
}
*/

#[derive(Clone, PartialEq)]
pub struct Seq<T> {
    pub head: Vec<T>,
    pub tail: Option<T>,
}

impl<T: fmt::Debug> fmt::Debug for Seq<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "("));
        let mut first = true;
        for e in &self.head {
            if first { first = false; } else { try!(write!(f, ", ")); }
            try!(write!(f, "{:?}", *e));
        }
        if let Some(ref e) = self.tail {
            if first { first = false; } else { try!(write!(f, ", ")); }
            try!(write!(f, "{:?}...", *e));
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
    None,
    SomeInt(HashSet<i32>),
    Int,
    All,
}

impl Unionable for Numbers {
    type Output = Numbers;

    fn union(self, other: Numbers) -> Numbers {
        match (self, other) {
            (Numbers::None, set) => set,
            (set, Numbers::None) => set,

            (Numbers::All, _) => Numbers::All,
            (_, Numbers::All) => Numbers::All,

            (Numbers::Int, _) => Numbers::Int,
            (_, Numbers::Int) => Numbers::Int,

            (Numbers::SomeInt(mut a), Numbers::SomeInt(b)) => {
                a.extend(b.into_iter());
                Numbers::SomeInt(a)
            }
        }
    }
}

impl PartialEq for Numbers {
    fn eq(&self, other: &Numbers) -> bool {
        match (self, other) {
            (&Numbers::None, &Numbers::None) => true,
            (&Numbers::Int, &Numbers::Int) => true,
            (&Numbers::All, &Numbers::All) => true,

            (&Numbers::SomeInt(ref a), &Numbers::SomeInt(ref b)) => *a == *b,
            (&Numbers::SomeInt(ref a), &Numbers::None) => a.is_empty(),
            (&Numbers::None, &Numbers::SomeInt(ref b)) => b.is_empty(),

            (_, _) => false,
        }
    }
}

#[derive(Clone)]
pub enum Strings {
    None,
    Some(HashSet<Str>),
    All,
}

impl Unionable for Strings {
    type Output = Strings;

    fn union(self, other: Strings) -> Strings {
        match (self, other) {
            (Strings::None, set) => set,
            (set, Strings::None) => set,

            (Strings::All, _) => Strings::All,
            (_, Strings::All) => Strings::All,

            (Strings::Some(mut a), Strings::Some(b)) => {
                a.extend(b.into_iter());
                Strings::Some(a)
            }
        }
    }
}

impl PartialEq for Strings {
    fn eq(&self, other: &Strings) -> bool {
        match (self, other) {
            (&Strings::None, &Strings::None) => true,
            (&Strings::All, &Strings::All) => true,

            (&Strings::Some(ref a), &Strings::Some(ref b)) => *a == *b,
            (&Strings::Some(ref a), &Strings::None) => a.is_empty(),
            (&Strings::None, &Strings::Some(ref b)) => b.is_empty(),

            (_, _) => false,
        }
    }
}

#[derive(Clone)]
pub enum Tables {
    None,
    Record(HashMap<Str, Ty>),
    Tuple(Vec<Ty>),
    Array(Ty),
    Map(Ty, Ty),
    All,
}

impl Unionable for Tables {
    type Output = Tables;

    fn union(self, other: Tables) -> Tables {
        fn union_rec_tup(rec: HashMap<Str, Ty>, mut tup: Vec<Ty>) -> Tables {
            if rec.is_empty() { return Tables::Tuple(tup); }
            if tup.is_empty() { return Tables::Record(rec); }

            let mut uty = tup.pop().unwrap();
            for ty in tup {
                uty = uty.union(ty);
            }
            for (_, ty) in rec {
                uty = uty.union(ty);
            }
            Tables::Map(Box::new(T::Integer.union(T::String)), uty)
        }

        fn union_rec_map(fields: HashMap<Str, Ty>, key: Ty, value: Ty) -> Tables {
            if fields.is_empty() { return Tables::Map(key, value); }

            let mut uty = value;
            for (_, ty) in fields {
                uty = uty.union(ty);
            }
            Tables::Map(key.union(Box::new(T::String)), uty)
        }

        fn union_tup_map(fields: Vec<Ty>, key: Ty, value: Ty) -> Tables {
            if fields.is_empty() { return Tables::Map(key, value); }

            let mut uty = value;
            for ty in fields {
                uty = uty.union(ty);
            }
            Tables::Map(key.union(Box::new(T::Integer)), uty)
        }

        match (self, other) {
            (Tables::None, tab) => tab,
            (tab, Tables::None) => tab,

            (Tables::All, _) => Tables::All,
            (_, Tables::All) => Tables::All,

            (Tables::Record(mut fields1), Tables::Record(fields2)) => {
                for (k, v2) in fields2 {
                    if let Some(v1) = fields1.remove(&k) {
                        fields1.insert(k, v1.union(v2));
                    } else {
                        fields1.insert(k, v2);
                    }
                }
                Tables::Record(fields1)
            },

            (Tables::Record(fields1), Tables::Tuple(fields2)) => union_rec_tup(fields1, fields2),
            (Tables::Tuple(fields1), Tables::Record(fields2)) => union_rec_tup(fields2, fields1),

            (Tables::Record(fields), Tables::Array(value)) =>
                union_rec_map(fields, Box::new(T::Integer), value),
            (Tables::Array(value), Tables::Record(fields)) =>
                union_rec_map(fields, Box::new(T::Integer), value),

            (Tables::Record(fields), Tables::Map(key, value)) => union_rec_map(fields, key, value),
            (Tables::Map(key, value), Tables::Record(fields)) => union_rec_map(fields, key, value),

            (Tables::Tuple(mut fields1), Tables::Tuple(mut fields2)) => {
                if fields1.len() < fields2.len() {
                    fields1.resize(fields2.len(), Box::new(T::Nil));
                } else if fields1.len() > fields2.len() {
                    fields2.resize(fields1.len(), Box::new(T::Nil));
                }
                let tys = fields1.into_iter().zip(fields2.into_iter());
                Tables::Tuple(tys.map(|(lty, rty)| lty.union(rty)).collect())
            },

            (Tables::Tuple(fields), Tables::Array(value)) =>
                union_tup_map(fields, Box::new(T::Integer), value),
            (Tables::Array(value), Tables::Tuple(fields)) =>
                union_tup_map(fields, Box::new(T::Integer), value),

            (Tables::Tuple(fields), Tables::Map(key, value)) => union_tup_map(fields, key, value),
            (Tables::Map(key, value), Tables::Tuple(fields)) => union_tup_map(fields, key, value),

            (Tables::Array(value1), Tables::Array(value2)) => Tables::Array(value1.union(value2)),

            (Tables::Map(key1, value1), Tables::Map(key2, value2)) =>
                Tables::Map(key1.union(key2), value1.union(value2)),

            (Tables::Array(value1), Tables::Map(key2, value2)) =>
                Tables::Map(key2.union(Box::new(T::Integer)), value1.union(value2)),
            (Tables::Map(key1, value1), Tables::Array(value2)) =>
                Tables::Map(key1.union(Box::new(T::Integer)), value1.union(value2)),
        }
    }
}

impl PartialEq for Tables {
    fn eq(&self, other: &Tables) -> bool {
        match (self, other) {
            (&Tables::None, &Tables::None) => true,
            (&Tables::All, &Tables::All) => true,

            (&Tables::Array(ref a), &Tables::Array(ref b)) => *a == *b,
            (&Tables::Map(ref ak, ref av), &Tables::Map(ref bk, ref bv)) => *ak == *bk && *av == *bv,

            (&Tables::Tuple(ref a), &Tables::Tuple(ref b)) => *a == *b,
            (&Tables::Tuple(ref a), &Tables::Record(ref b)) => a.is_empty() && b.is_empty(),
            (&Tables::Record(ref a), &Tables::Tuple(ref b)) => a.is_empty() && b.is_empty(),
            (&Tables::Record(ref a), &Tables::Record(ref b)) => *a == *b,

            (_, _) => false,
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Function {
    pub args: Seq<Ty>,
    pub returns: Seq<Ty>,
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
    None,
    Some(Vec<Vec<Function>>), // disjunctive normal form: [f /\ f] \/ [f /\ f] \/ ...
    All,
}

impl Functions {
    pub fn from(func: Function) -> Functions {
        Functions::Some(vec![vec![func]])
    }
}

impl Unionable for Functions {
    type Output = Functions;

    fn union(self, other: Functions) -> Functions {
        match (self, other) {
            (Functions::None, set) => set,
            (set, Functions::None) => set,

            (Functions::All, _) => Functions::All,
            (_, Functions::All) => Functions::All,

            (Functions::Some(mut a), Functions::Some(b)) => {
                a.extend(b.into_iter());
                Functions::Some(a)
            }
        }
    }
}

impl PartialEq for Functions {
    fn eq(&self, other: &Functions) -> bool {
        match (self, other) {
            (&Functions::None, &Functions::None) => true,
            (&Functions::All, &Functions::All) => true,

            (&Functions::Some(ref a), &Functions::Some(ref b)) => *a == *b,
            (&Functions::Some(ref a), &Functions::None) => a.is_empty(),
            (&Functions::None, &Functions::Some(ref b)) => b.is_empty(),

            (_, _) => false,
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
    pub numbers: Numbers,
    pub strings: Strings,
    pub tables: Tables,
    pub functions: Functions,
    //pub tvar: Option<TVar>,
}

impl Union {
    pub fn from<'a>(ty: T<'a>) -> Union {
        let mut u = Union {
            has_dynamic:  false,
            has_nil:      false,
            has_true:     false,
            has_false:    false,
            numbers:      Numbers::None,
            strings:      Strings::None,
            tables:       Tables::None,
            functions:    Functions::None,
        };

        fn singleton<X: Hash + Eq>(x: X) -> HashSet<X> {
            let mut set = HashSet::with_capacity(1);
            set.insert(x);
            set
        }

        match ty {
            T::Dynamic => { u.has_dynamic = true; }
            T::Nil     => { u.has_nil = true; }
            T::Boolean => { u.has_true = true; u.has_false = true; }
            T::True    => { u.has_true = true; }
            T::False   => { u.has_false = true; }

            T::Number            => { u.numbers = Numbers::All; }
            T::Integer           => { u.numbers = Numbers::Int; }
            T::SomeInteger(v)    => { u.numbers = Numbers::SomeInt(singleton(v)); }
            T::SomeIntegers(set) => { u.numbers = Numbers::SomeInt(set.into_owned()); }

            T::String           => { u.strings = Strings::All; }
            T::SomeString(s)    => { u.strings = Strings::Some(singleton(s.into_owned())); }
            T::SomeStrings(set) => { u.strings = Strings::Some(set.into_owned()); }

            T::Table              => { u.tables = Tables::All; }
            T::SomeRecord(fields) => { u.tables = Tables::Record(fields.into_owned()); }
            T::SomeTuple(fields)  => { u.tables = Tables::Tuple(fields.into_owned()); }
            T::SomeArray(t)       => { u.tables = Tables::Array(t.into_owned()); }
            T::SomeMap(k, v)      => { u.tables = Tables::Map(k.into_owned(), v.into_owned()); }

            T::Function        => { u.functions = Functions::All; }
            T::SomeFunction(f) => { u.functions = Functions::Some(vec![f.into_owned()]); }

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
            Numbers::None => {}
            Numbers::SomeInt(..) | Numbers::Int => { flags = flags | T_INTEGER; }
            Numbers::All => { flags = flags | T_NUMBER; }
        }
        match self.strings {
            Strings::None => {}
            Strings::Some(..) | Strings::All => { flags = flags | T_STRING; }
        }
        match self.tables {
            Tables::None => {}
            _ => { flags = flags | T_TABLE; }
        }
        match self.functions {
            Functions::None => {}
            _ => { flags = flags | T_FUNCTION; }
        }
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
        match self.numbers {
            Numbers::None => {},
            Numbers::SomeInt(ref set) if set.len() == 1 => {
                let &v = set.iter().next().unwrap();
                try!(f(T::SomeInteger(v)))
            },
            Numbers::SomeInt(ref set) => try!(f(T::SomeIntegers(Cow::Borrowed(set)))),
            Numbers::Int => try!(f(T::Integer)),
            Numbers::All => try!(f(T::Number)),
        }
        match self.strings {
            Strings::None => {},
            Strings::Some(ref set) if set.len() == 1 => {
                let s = set.iter().next().unwrap();
                try!(f(T::SomeString(Cow::Borrowed(s))))
            },
            Strings::Some(ref set) => try!(f(T::SomeStrings(Cow::Borrowed(set)))),
            Strings::All => try!(f(T::String)),
        }
        match self.tables {
            Tables::None => {},
            Tables::Record(ref fields) => try!(f(T::SomeRecord(Cow::Borrowed(fields)))),
            Tables::Tuple(ref fields) => try!(f(T::SomeTuple(Cow::Borrowed(fields)))),
            Tables::Array(ref t) => try!(f(T::SomeArray(Cow::Borrowed(t)))),
            Tables::Map(ref k, ref v) => try!(f(T::SomeMap(Cow::Borrowed(k), Cow::Borrowed(v)))),
            Tables::All => try!(f(T::Table)),
        }
        match self.functions {
            Functions::None => {},
            Functions::Some(ref set) => {
                for overloadedfn in set {
                    try!(f(T::SomeFunction(Cow::Borrowed(overloadedfn))));
                }
            },
            Functions::All => try!(f(T::Function)),
        }
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
                Some(single.expect("Union is empty").into_send())
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
        if let (&Numbers::SomeInt(ref selfints),
                &Numbers::SomeInt(ref rhsints)) = (&self.numbers, &rhs.numbers) {
            if !selfints.is_superset(rhsints) { return false; }
        }
        if let (&Strings::Some(ref selfstrs),
                &Strings::Some(ref rhsstrs)) = (&self.strings, &rhs.strings) {
            if !selfstrs.is_superset(rhsstrs) { return false; }
        }

        true
    }
}

impl Unionable for Union {
    type Output = Union;

    fn union(mut self, other: Union) -> Union {
        self.has_dynamic |= other.has_dynamic;
        self.has_nil     |= other.has_nil;
        self.has_true    |= other.has_true;
        self.has_false   |= other.has_false;
        self.numbers      = self.numbers.union(other.numbers);
        self.strings      = self.strings.union(other.strings);
        self.tables       = self.tables.union(other.tables);
        self.functions    = self.functions.union(other.functions);
        self
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
#[derive(Clone, PartialEq)]
pub enum T<'a> {
    Dynamic,                            // ?
    Nil,                                // nil
    Boolean,                            // boolean
    True,                               // true
    False,                              // false
    Number,                             // number
    Integer,                            // integer
    SomeInteger(i32),                   // integer in {...}
    SomeIntegers(Cow<'a, HashSet<i32>>), // integer in {...}
    String,                             // string
    SomeString(Cow<'a, Str>),           // string in {...}
    SomeStrings(Cow<'a, HashSet<Str>>), // string in {...}
    Table,                              // table
    SomeRecord(Cow<'a, HashMap<Str, Ty>>), // { a = t, b = u, ... }
    SomeTuple(Cow<'a, Vec<Ty>>),        // { t1, t2, t3, ... } XXX Cow<'a, [Ty]> overflows?
    SomeArray(Cow<'a, Ty>),             // { t } XXX conflicting syntax
    SomeMap(Cow<'a, Ty>, Cow<'a, Ty>),  // { [t] = t }
    Function,                           // function
    SomeFunction(Cow<'a, [Function]>),  // (t...) -> (...) & ...
    //TVar(TVar),                       // type variable
    Union(Cow<'a, Union>),              // union types A | B | ...
}

impl<'a> T<'a> {
    pub fn from(kind: &K) -> T<'a> {
        match *kind {
            K::Dynamic           => T::Dynamic,
            K::Nil               => T::Nil,
            K::Boolean           => T::Boolean,
            K::BooleanLit(true)  => T::True,
            K::BooleanLit(false) => T::False,
            K::Number            => T::Number,
            K::Integer           => T::Integer,
            K::IntegerLit(v)     => T::SomeInteger(v),
            K::String            => T::String,
            K::StringLit(ref s)  => T::SomeString(Cow::Owned(s.to_owned())),
            K::Table             => T::Table,
            K::Function          => T::Function,

            K::Union(ref kinds) => {
                assert!(!kinds.is_empty());
                let mut ty = T::from(&kinds[0]);
                for kind in &kinds[1..] {
                    ty = ty.union(T::from(kind));
                }
                ty
            }
        }
    }

    pub fn is_integral(&self) -> bool {
        match *self {
            T::Dynamic | T::Integer | T::SomeInteger(..) | T::SomeIntegers(..) => true,
            T::Union(ref u) => u.flags().is_integral(),
            _ => false,
        }
    }

    pub fn is_numeric(&self) -> bool {
        match *self {
            T::Dynamic | T::Number | T::Integer | T::SomeInteger(..) | T::SomeIntegers(..) => true,
            T::Union(ref u) => u.flags().is_numeric(),
            _ => false,
        }
    }

    pub fn is_stringy(&self) -> bool {
        match *self {
            T::Dynamic | T::Number | T::Integer | T::SomeInteger(..) | T::SomeIntegers(..) |
                T::String | T::SomeString(..) | T::SomeStrings(..) => true,
            T::Union(ref u) => u.flags().is_stringy(),
            _ => false,
        }
    }

    pub fn is_tabular(&self) -> bool {
        match *self {
            T::Dynamic | T::String | T::SomeString(..) | T::SomeStrings(..) |
                T::Table | T::SomeRecord(..) | T::SomeTuple(..) | T::SomeArray(..) | T::SomeMap(..) => true,
            T::Union(ref u) => u.flags().is_tabular(),
            _ => false,
        }
    }

    pub fn is_callable(&self) -> bool {
        match *self {
            T::Dynamic | T::Function | T::SomeFunction(..) => true,
            T::Union(ref u) => u.flags().is_callable(),
            _ => false,
        }
    }

    pub fn into_send(self) -> T<'static> {
        match self {
            T::Dynamic  => T::Dynamic,
            T::Nil      => T::Nil,
            T::Boolean  => T::Boolean,
            T::True     => T::True,
            T::False    => T::False,
            T::Number   => T::Number,
            T::Integer  => T::Integer,
            T::String   => T::String,
            T::Table    => T::Table,
            T::Function => T::Function,

            T::SomeInteger(v) => T::SomeInteger(v),
            T::SomeIntegers(set) => T::SomeIntegers(Cow::Owned(set.into_owned())),

            T::SomeString(s) => T::SomeString(Cow::Owned(s.into_owned())),
            T::SomeStrings(set) => T::SomeStrings(Cow::Owned(set.into_owned())),

            T::SomeRecord(fields) => T::SomeRecord(Cow::Owned(fields.into_owned())),
            T::SomeTuple(fields) => T::SomeTuple(Cow::Owned(fields.into_owned())),
            T::SomeArray(t) => T::SomeArray(Cow::Owned(t.into_owned())),
            T::SomeMap(k, v) => T::SomeMap(Cow::Owned(k.into_owned()), Cow::Owned(v.into_owned())),

            T::SomeFunction(f) => T::SomeFunction(Cow::Owned(f.into_owned())),

            T::Union(u) => T::Union(Cow::Owned(u.into_owned())),
        }
    }
}

impl<'a, 'b> Unionable<T<'b>> for T<'a> {
    type Output = T<'static>;

    fn union(self, other: T<'b>) -> T<'static> {
        match (self, other) {
            // dynamic eclipses everything else
            (T::Dynamic, _) => T::Dynamic,
            (_, T::Dynamic) => T::Dynamic,

            // A | A == A
            (T::Nil,      T::Nil)      => T::Nil,
            (T::Boolean,  T::Boolean)  => T::Boolean,
            (T::Number,   T::Number)   => T::Number,
            (T::String,   T::String)   => T::String,
            (T::Table,    T::Table)    => T::Table,
            (T::Function, T::Function) => T::Function,

            // for everything else, convert to the "common" format
            (lhs, rhs) => Union::from(lhs).union(Union::from(rhs)).simplify(),
        }
    }
}

impl<'a> fmt::Debug for T<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            T::Dynamic           => write!(f, "?"),
            T::Nil               => write!(f, "nil"),
            T::Boolean           => write!(f, "boolean"),
            T::True              => write!(f, "true"),
            T::False             => write!(f, "false"),
            T::Number            => write!(f, "number"),
            T::Integer           => write!(f, "integer"),
            T::SomeInteger(v)    => write!(f, "{:?}", v),
            T::String            => write!(f, "string"),
            T::SomeString(ref s) => write!(f, "{:?}", s),
            T::Table             => write!(f, "table"),
            T::Function          => write!(f, "function"),

            T::SomeIntegers(ref set) => {
                try!(write!(f, "("));
                let mut first = true;
                for v in set.iter() {
                    if first { first = false; } else { try!(write!(f, "|")); }
                    try!(write!(f, "{:?}", *v));
                }
                write!(f, ")")
            }

            T::SomeStrings(ref set) => {
                try!(write!(f, "("));
                let mut first = true;
                for s in set.iter() {
                    if first { first = false; } else { try!(write!(f, "|")); }
                    try!(write!(f, "{:?}", *s));
                }
                write!(f, ")")
            }

            T::SomeRecord(ref fields) => {
                try!(write!(f, "{{"));
                let mut first = true;
                for (name, t) in fields.iter() {
                    if first { first = false; } else { try!(write!(f, ", ")); }
                    try!(write!(f, "{:?} = {:?}", *name, *t));
                }
                write!(f, "}}")
            }

            T::SomeTuple(ref fields) => {
                try!(write!(f, "{{"));
                let mut first = true;
                for t in fields.iter() {
                    if first { first = false; } else { try!(write!(f, ", ")); }
                    try!(write!(f, "{:?}", *t));
                }
                write!(f, "}}")
            }

            T::SomeArray(ref t) => write!(f, "{{{:?}}}", *t),
            T::SomeMap(ref k, ref v) => write!(f, "{{[{:?}] = {:?}}}", *k, *v),

            T::SomeFunction(ref fty) => fmt::Debug::fmt(fty, f),

            T::Union(ref u) => fmt::Debug::fmt(u, f),
        }
    }
}

impl<'a> From<T<'a>> for Union { fn from(x: T<'a>) -> Union { Union::from(x) } }

impl<'a> From<K> for T<'a> { fn from(x: K) -> T<'a> { T::from(&x) } }

pub type Ty = Box<T<'static>>;

impl<'a, 'b> Unionable<Box<T<'b>>> for Box<T<'a>> {
    type Output = Ty;

    fn union(self, other: Box<T<'b>>) -> Ty {
        Box::new((*self).union(*other))
    }
}

impl From<Kind> for Ty { fn from(x: Kind) -> Ty { Box::new(From::from(*x)) } }

