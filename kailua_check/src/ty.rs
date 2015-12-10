use std::fmt;
use kailua_syntax::{K, Kind};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Builtin {
    Require,        // (fixed string) -> table & sideeffect
}

#[derive(Clone, PartialEq)]
pub enum T {
    Dynamic,        // ?
}

impl T {
    pub fn from(kind: &K) -> T {
        match *kind {
            K::Dynamic => T::Dynamic,
        }
    }
}

impl fmt::Debug for T {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            T::Dynamic => write!(f, "?"),
        }
    }
}

impl From<K> for T { fn from(x: K) -> T { T::from(&x) } }

pub type Ty = Box<T>;

impl From<Kind> for Ty { fn from(x: Kind) -> Ty { Box::new(From::from(*x)) } }

