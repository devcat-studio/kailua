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

    fn union(self, other: Numbers, _: &mut TypeContext) -> Option<Numbers> {
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

    fn intersect(self, other: Numbers, _: &mut TypeContext) -> Option<Numbers> {
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

    fn assert_sub(&self, other: &Self, _: &mut TypeContext) -> CheckResult<()> {
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

    fn assert_eq(&self, other: &Self, _ctx: &mut TypeContext) -> CheckResult<()> {
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
    pub fn assert_sup_str(&self, other: &Str, _: &mut TypeContext) -> CheckResult<()> {
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

    fn union(self, other: Strings, _: &mut TypeContext) -> Option<Strings> {
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

    fn intersect(self, other: Strings, _: &mut TypeContext) -> Option<Strings> {
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

    fn assert_sub(&self, other: &Self, _: &mut TypeContext) -> CheckResult<()> {
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

    fn assert_eq(&self, other: &Self, _ctx: &mut TypeContext) -> CheckResult<()> {
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

