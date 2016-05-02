use std::fmt;
use std::collections::HashSet;

use kailua_syntax::Str;
use diag::CheckResult;
use super::{TypeContext, Lattice};
use super::{error_not_sub, error_not_eq};

#[derive(Clone)]
pub enum Numbers {
    One(i32),
    Some(HashSet<i32>),
    Int,
    All,
}

impl Lattice for Numbers {
    type Output = Numbers;

    fn do_union(&self, other: &Numbers, _: &mut TypeContext) -> Numbers {
        match (self, other) {
            (&Numbers::All, _) => Numbers::All,
            (_, &Numbers::All) => Numbers::All,

            (&Numbers::Int, _) => Numbers::Int,
            (_, &Numbers::Int) => Numbers::Int,

            (&Numbers::Some(ref a), &Numbers::Some(ref b)) => {
                let mut ab = a.clone();
                ab.extend(b.iter().cloned());
                Numbers::Some(ab)
            }
            (&Numbers::Some(ref a), &Numbers::One(b)) => {
                let mut ab = a.clone();
                ab.insert(b);
                Numbers::Some(ab)
            }
            (&Numbers::One(a), &Numbers::Some(ref b)) => {
                let mut ab = b.clone();
                ab.insert(a);
                Numbers::Some(ab)
            }
            (&Numbers::One(a), &Numbers::One(b)) => {
                if a == b {
                    Numbers::One(a)
                } else {
                    let mut ab = HashSet::new();
                    ab.insert(a);
                    ab.insert(b);
                    Numbers::Some(ab)
                }
            }
        }
    }

    fn do_assert_sub(&self, other: &Self, _: &mut TypeContext) -> CheckResult<()> {
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

    fn do_assert_eq(&self, other: &Self, _ctx: &mut TypeContext) -> CheckResult<()> {
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

impl Lattice for Strings {
    type Output = Strings;

    fn do_union(&self, other: &Strings, _: &mut TypeContext) -> Strings {
        match (self, other) {
            (&Strings::All, _) => Strings::All,
            (_, &Strings::All) => Strings::All,

            (&Strings::Some(ref a), &Strings::Some(ref b)) => {
                let mut ab = a.clone();
                ab.extend(b.iter().cloned());
                Strings::Some(ab)
            }
            (&Strings::Some(ref a), &Strings::One(ref b)) => {
                let mut ab = a.clone();
                ab.insert(b.clone());
                Strings::Some(ab)
            }
            (&Strings::One(ref a), &Strings::Some(ref b)) => {
                let mut ab = b.clone();
                ab.insert(a.clone());
                Strings::Some(ab)
            }
            (&Strings::One(ref a), &Strings::One(ref b)) => {
                if a == b {
                    Strings::One(a.clone())
                } else {
                    let mut ab = HashSet::new();
                    ab.insert(a.clone());
                    ab.insert(b.clone());
                    Strings::Some(ab)
                }
            }
        }
    }

    fn do_assert_sub(&self, other: &Self, _: &mut TypeContext) -> CheckResult<()> {
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

    fn do_assert_eq(&self, other: &Self, _ctx: &mut TypeContext) -> CheckResult<()> {
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

