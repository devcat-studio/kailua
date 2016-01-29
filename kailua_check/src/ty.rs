use std::fmt;
use std::hash::Hash;
use std::borrow::{Cow, ToOwned};
use std::collections::HashSet;
use kailua_syntax::{K, Kind, Str};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Builtin {
    Require,        // (fixed string) -> table & sideeffect
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
    pub fn is_numeric(&self) -> bool {
        (*self & T_NUMERIC != T_NONE) && (*self & !T_NUMERIC == T_NONE)
    }

    pub fn is_stringy(&self) -> bool {
        (*self & T_STRINGY != T_NONE) && (*self & !T_STRINGY == T_NONE)
    }

    pub fn is_tabular(&self) -> bool {
        (*self & T_TABULAR != T_NONE) && (*self & !T_TABULAR == T_NONE)
    }

    pub fn is_callable(&self) -> bool {
        (*self & T_CALLABLE != T_NONE) && (*self & !T_CALLABLE == T_NONE)
    }
}

pub mod flags {
    pub use super::{T_NONE, T_DYNAMIC, T_NIL, T_TRUE, T_FALSE, T_BOOLEAN,
                    T_NONINTEGER, T_INTEGER, T_NUMBER, T_STRING, T_TABLE, T_FUNCTION,
                    T_NUMERIC, T_STRINGY, T_TABULAR, T_CALLABLE};
}

#[derive(Clone, PartialEq)]
pub enum Numbers {
    None,
    SomeInt(HashSet<i32>),
    Int,
    All,
}

impl Numbers {
    pub fn union(self, other: Numbers) -> Numbers {
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

#[derive(Clone, PartialEq)]
pub enum Strings {
    None,
    Some(HashSet<Str>),
    All,
}

impl Strings {
    pub fn union(self, other: Strings) -> Strings {
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

// "true" types. constructed out of `T` "simple" types.
#[derive(Clone, PartialEq)]
pub struct Union {
    pub has_dynamic: bool, // XXX
    pub has_nil: bool,
    pub has_true: bool,
    pub has_false: bool,
    pub has_table: bool,
    pub has_function: bool,
    pub numbers: Numbers,
    pub strings: Strings,
}

impl Union {
    pub fn from<'a>(ty: T<'a>) -> Union {
        let mut u = Union {
            has_dynamic:  false,
            has_nil:      false,
            has_true:     false,
            has_false:    false,
            has_table:    false,
            has_function: false,
            numbers:      Numbers::None,
            strings:      Strings::None,
        };

        fn singleton<X: Hash + Eq>(x: X) -> HashSet<X> {
            let mut set = HashSet::with_capacity(1);
            set.insert(x);
            set
        }

        match ty {
            T::Dynamic           => { u.has_dynamic = true; }
            T::Nil               => { u.has_nil = true; }
            T::Boolean           => { u.has_true = true; u.has_false = true; }
            T::True              => { u.has_true = true; }
            T::False             => { u.has_false = true; }
            T::Number            => { u.numbers = Numbers::All; }
            T::Integer           => { u.numbers = Numbers::Int; }
            T::SomeInteger(v)    => { u.numbers = Numbers::SomeInt(singleton(v)); }
            T::SomeIntegers(set) => { u.numbers = Numbers::SomeInt(set.into_owned()); }
            T::String            => { u.strings = Strings::All; }
            T::SomeString(s)     => { u.strings = Strings::Some(singleton(s.into_owned())); }
            T::SomeStrings(set)  => { u.strings = Strings::Some(set.into_owned()); }
            T::Table             => { u.has_table = true; }
            T::Function          => { u.has_function = true; }
            T::Union(u)          => return u.into_owned() // ignore `u` above
        }

        u
    }

    pub fn flags(&self) -> Flags {
        let mut flags = T_NONE;
        if self.has_dynamic  { flags = flags | T_DYNAMIC; }
        if self.has_nil      { flags = flags | T_NIL; }
        if self.has_true     { flags = flags | T_TRUE; }
        if self.has_false    { flags = flags | T_FALSE; }
        if self.has_table    { flags = flags | T_TABLE; }
        if self.has_function { flags = flags | T_FUNCTION; }
        match self.numbers {
            Numbers::None => {}
            Numbers::SomeInt(..) | Numbers::Int => { flags = flags | T_INTEGER; }
            Numbers::All => { flags = flags | T_NUMBER; }
        }
        match self.strings {
            Strings::None => {}
            Strings::Some(..) | Strings::All => { flags = flags | T_STRING; }
        }
        flags
    }

    pub fn union(mut self, other: Union) -> Union {
        self.has_dynamic  |= other.has_dynamic;
        self.has_nil      |= other.has_nil;
        self.has_true     |= other.has_true;
        self.has_false    |= other.has_false;
        self.has_table    |= other.has_table;
        self.has_function |= other.has_function;
        self.numbers       = self.numbers.union(other.numbers);
        self.strings       = self.strings.union(other.strings);
        self
    }

    pub fn visit<'a, E, F>(&'a self, mut f: F) -> Result<(), E>
            where F: FnMut(T<'a>) -> Result<(), E> {
        if self.has_dynamic { try!(f(T::Dynamic)); }
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
        if self.has_table { try!(f(T::Table)); }
        if self.has_function { try!(f(T::Function)); }
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
    Dynamic,                // ?
    Nil,                    // nil
    Boolean,                // boolean
    True,                   // true
    False,                  // false
    Number,                 // number
    Integer,                // integer
    SomeInteger(i32),       // integer in {...}
    SomeIntegers(Cow<'a, HashSet<i32>>), // integer in {...}
    String,                 // string
    SomeString(Cow<'a, Str>), // string in {...}
    SomeStrings(Cow<'a, HashSet<Str>>), // string in {...}
    Table,                  // table
    Function,               // function
    Union(Cow<'a, Union>),  // union types A | B | ...
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

    pub fn union<'b>(self, other: T<'b>) -> T<'static> {
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

    pub fn into_send(self) -> T<'static> {
        match self {
            T::Dynamic        => T::Dynamic,
            T::Nil            => T::Nil,
            T::Boolean        => T::Boolean,
            T::True           => T::True,
            T::False          => T::False,
            T::Number         => T::Number,
            T::Integer        => T::Integer,
            T::SomeInteger(v) => T::SomeInteger(v),
            T::String         => T::String,
            T::SomeString(s)  => T::SomeString(Cow::Owned(s.into_owned())),
            T::Table          => T::Table,
            T::Function       => T::Function,

            T::SomeIntegers(set) => T::SomeIntegers(Cow::Owned(set.into_owned())),
            T::SomeStrings(set) => T::SomeStrings(Cow::Owned(set.into_owned())),
            T::Union(u) => T::Union(Cow::Owned(u.into_owned())),
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
                    if first {
                        first = false;
                    } else {
                        try!(write!(f, "|"));
                    }
                    try!(write!(f, "{:?}", *v));
                }
                write!(f, ")")
            }

            T::SomeStrings(ref set) => {
                try!(write!(f, "("));
                let mut first = true;
                for s in set.iter() {
                    if first {
                        first = false;
                    } else {
                        try!(write!(f, "|"));
                    }
                    try!(write!(f, "{:?}", *s));
                }
                write!(f, ")")
            }

            T::Union(ref u) => fmt::Debug::fmt(u, f),
        }
    }
}

impl<'a> From<T<'a>> for Union { fn from(x: T<'a>) -> Union { Union::from(x) } }

impl<'a> From<K> for T<'a> { fn from(x: K) -> T<'a> { T::from(&x) } }

pub type Ty = Box<T<'static>>;

impl From<Kind> for Ty { fn from(x: Kind) -> Ty { Box::new(From::from(*x)) } }

