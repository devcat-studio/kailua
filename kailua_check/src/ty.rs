use std::fmt;
use kailua_syntax::{K, Kind};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Builtin {
    Require,        // (fixed string) -> table & sideeffect
}

#[derive(Clone, PartialEq)]
pub struct Union {
    pub has_dynamic: bool, // XXX
    pub has_nil: bool,
    pub has_boolean: bool,
    pub has_number: bool,
    pub has_string: bool,
    pub has_table: bool,
    pub has_function: bool,
}

impl Union {
    pub fn from(ty: T) -> Union {
        let u = Union {
            has_dynamic:  false,
            has_nil:      false,
            has_boolean:  false,
            has_number:   false,
            has_string:   false,
            has_table:    false,
            has_function: false,
        };
        match ty {
            T::Dynamic  => Union { has_dynamic:  true, ..u },
            T::Nil      => Union { has_nil:      true, ..u },
            T::Boolean  => Union { has_boolean:  true, ..u },
            T::Number   => Union { has_number:   true, ..u },
            T::String   => Union { has_string:   true, ..u },
            T::Table    => Union { has_table:    true, ..u },
            T::Function => Union { has_function: true, ..u },
            T::Union(u) => u,
        }
    }

    pub fn union(mut self, other: Union) -> Union {
        self.has_dynamic  |= other.has_dynamic;
        self.has_nil      |= other.has_nil;
        self.has_boolean  |= other.has_boolean;
        self.has_number   |= other.has_number;
        self.has_string   |= other.has_string;
        self.has_table    |= other.has_table;
        self.has_function |= other.has_function;
        self
    }
}

impl fmt::Debug for Union {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "("));

        let mut first = true;
        macro_rules! pipe {
            () => (if first { first = false; } else { try!(write!(f, "|")); })
        }
        if self.has_dynamic  { pipe!(); try!(fmt::Debug::fmt(&T::Dynamic,  f)); }
        if self.has_nil      { pipe!(); try!(fmt::Debug::fmt(&T::Nil,      f)); }
        if self.has_boolean  { pipe!(); try!(fmt::Debug::fmt(&T::Boolean,  f)); }
        if self.has_number   { pipe!(); try!(fmt::Debug::fmt(&T::Number,   f)); }
        if self.has_string   { pipe!(); try!(fmt::Debug::fmt(&T::String,   f)); }
        if self.has_table    { pipe!(); try!(fmt::Debug::fmt(&T::Table,    f)); }
        if self.has_function { pipe!(); try!(fmt::Debug::fmt(&T::Function, f)); }

        let _ = first;
        write!(f, ")")
    }
}

#[derive(Clone, PartialEq)]
pub enum T {
    Dynamic,        // ?
    Nil,            // nil
    Boolean,        // boolean
    Number,         // number
    String,         // string
    Table,          // table
    Function,       // function
    Union(Union),   // union types A | B | ...
}

impl T {
    pub fn from(kind: &K) -> T {
        match *kind {
            K::Dynamic  => T::Dynamic,
            K::Nil      => T::Nil,
            K::Boolean  => T::Boolean,
            K::Number   => T::Number,
            K::String   => T::String,
            K::Table    => T::Table,
            K::Function => T::Function,
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

    pub fn union(self, other: T) -> T {
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
            (lhs, rhs) => T::Union(Union::from(lhs).union(Union::from(rhs))),
        }
    }
}

impl fmt::Debug for T {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            T::Dynamic  => write!(f, "?"),
            T::Nil      => write!(f, "nil"),
            T::Boolean  => write!(f, "boolean"),
            T::Number   => write!(f, "number"),
            T::String   => write!(f, "string"),
            T::Table    => write!(f, "table"),
            T::Function => write!(f, "function"),
            T::Union(ref u) => fmt::Debug::fmt(u, f),
        }
    }
}

impl From<T> for Union { fn from(x: T) -> Union { Union::from(x) } }

impl From<K> for T { fn from(x: K) -> T { T::from(&x) } }

pub type Ty = Box<T>;

impl From<Kind> for Ty { fn from(x: Kind) -> Ty { Box::new(From::from(*x)) } }

