use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Builtin {
    Require,        // (fixed string) -> table & sideeffect
}

#[derive(Clone, PartialEq)]
pub enum T {
    Dynamic,        // ?
}

impl fmt::Debug for T {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            T::Dynamic => write!(f, "?"),
        }
    }
}

pub type Ty = Box<T>;

