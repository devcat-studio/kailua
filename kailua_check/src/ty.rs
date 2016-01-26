use std::fmt;
use kailua_syntax::{K, Kind};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Builtin {
    Require,        // (fixed string) -> table & sideeffect
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
        }
    }
}

impl From<K> for T { fn from(x: K) -> T { T::from(&x) } }

pub type Ty = Box<T>;

impl From<Kind> for Ty { fn from(x: Kind) -> Ty { Box::new(From::from(*x)) } }

