use std::fmt;
use std::cmp;
use std::borrow::ToOwned;
use std::collections::HashMap;

use kailua_syntax::Str;
use diag::CheckResult;
use super::{T, Ty, Slot, TypeContext, Lattice, Strings};
use super::{error_not_sub, error_not_eq};
use super::flags::*;

#[derive(Clone)]
pub enum Tables {
    Empty,
    Record(HashMap<Str, Box<Slot>>),
    Tuple(Vec<Box<Slot>>),
    Array(Box<Slot>),
    Map(Ty, Box<Slot>),
    All,
}

impl Tables {
    pub fn lift_to_map(self, ctx: &mut TypeContext) -> Tables {
        match self {
            Tables::Empty => Tables::Empty,
            Tables::Record(fields) => {
                let mut value = Slot::just(T::None);
                for (_, ty) in fields { value = value.union(&*ty, ctx); }
                Tables::Map(Box::new(T::string()), Box::new(value))
            },
            Tables::Tuple(fields) => {
                let mut value = Slot::just(T::None);
                for ty in fields { value = value.union(&*ty, ctx); }
                Tables::Map(Box::new(T::integer()), Box::new(value))
            },
            Tables::Array(value) => Tables::Map(Box::new(T::integer()), value),
            Tables::Map(key, value) => Tables::Map(key, value),
            Tables::All => Tables::All,
        }
    }

    pub fn insert(self, key: Option<T<'static>>, value: T<'static>,
                  ctx: &mut TypeContext) -> Tables {
        // a single string key is special
        if let Some(ref key) = key {
            if key.flags() == T_STRING {
                if let Some(&Strings::One(ref s)) = key.has_strings() {
                    match self {
                        Tables::Empty => {
                            let mut fields = HashMap::new();
                            fields.insert(s.to_owned(), Box::new(Slot::just(value)));
                            return Tables::Record(fields);
                        }
                        Tables::Record(mut fields) => {
                            // should override a duplicate field if any
                            fields.insert(s.to_owned(), Box::new(Slot::just(value)));
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
            (None, Tables::Empty) => Tables::Array(Box::new(Slot::just(value))),
            (None, Tables::Array(t)) =>
                Tables::Array(Box::new(Slot::just(value).union(&*t, ctx))),
            (None, Tables::Tuple(mut fields)) => {
                fields.push(Box::new(Slot::just(value)));
                Tables::Tuple(fields)
            },

            (Some(key), Tables::Empty) =>
                Tables::Map(Box::new(key), Box::new(Slot::just(value))),

            // fall back to the map when in doubt
            (key, tab) => match tab.lift_to_map(ctx) {
                Tables::Map(key_, value_) => {
                    let key = key.unwrap_or(T::integer()).union(&*key_, ctx);
                    let value = Slot::just(value).union(&*value_, ctx);
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

                let norm_kv = |(k, v): (Str, Box<Slot>)| {
                    let v = v.normalize();
                    let flags = v.borrow().unlift().flags();
                    if flags == T_NONE { None } else { Some((k, v)) }
                };
                let fields: HashMap<_, _> = fields.into_iter().filter_map(norm_kv).collect();
                if fields.is_empty() {
                    Some(Tables::Empty)
                } else {
                    Some(Tables::Record(fields))
                }
            },

            Tables::Tuple(fields) => {
                if fields.is_empty() { return Some(Tables::Empty); }

                let norm = |v: Box<Slot>| {
                    let v = v.normalize();
                    let flags = v.borrow().unlift().flags();
                    if flags == T_NONE { None } else { Some(v) }
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

    fn union(&self, other: &Tables, ctx: &mut TypeContext) -> Option<Tables> {
        let tab = match (self, other) {
            (&Tables::All, _) => Tables::All,
            (_, &Tables::All) => Tables::All,

            (&Tables::Record(ref fields1), &Tables::Record(ref fields2)) => {
                let mut fields1 = fields1.clone();
                let mut fields = HashMap::new();
                for (k, v2) in fields2 {
                    let k = k.clone();
                    if let Some(v1) = fields1.remove(&k) {
                        fields.insert(k, v1.union(v2, ctx));
                    } else {
                        fields.insert(k, Box::new(Slot::just(T::Nil).union(&v2, ctx)));
                    }
                }
                for (k, v1) in fields1 {
                    fields.insert(k.clone(), Box::new(Slot::just(T::Nil).union(&v1, ctx)));
                }
                Tables::Record(fields)
            },

            (&Tables::Record(ref fields), &Tables::Empty) |
            (&Tables::Empty, &Tables::Record(ref fields)) => {
                let add_nil = |(k,s): (&Str,&Box<Slot>)|
                    (k.clone(), Box::new(Slot::just(T::Nil).union(s, ctx)));
                Tables::Record(fields.iter().map(add_nil).collect())
            },

            (&Tables::Tuple(ref fields1), &Tables::Tuple(ref fields2)) => {
                let mut fields = Vec::with_capacity(cmp::max(fields1.len(), fields2.len()));
                for (lty, rty) in fields1.iter().zip(fields2.iter()) {
                    fields.push(lty.union(rty, ctx));
                }
                let excess = if fields1.len() < fields2.len() {
                    &fields2[fields1.len()..]
                } else if fields1.len() > fields2.len() {
                    &fields1[fields2.len()..]
                } else {
                    &[][..]
                };
                fields.extend(excess.iter().map(|ty| Box::new(Slot::just(T::Nil).union(&ty, ctx))));
                Tables::Tuple(fields)
            },

            (&Tables::Tuple(ref fields), &Tables::Empty) |
            (&Tables::Empty, &Tables::Tuple(ref fields)) => {
                let add_nil = |s: &Box<Slot>| Box::new(Slot::just(T::Nil).union(s, ctx));
                Tables::Tuple(fields.iter().map(add_nil).collect())
            },

            // records and tuples are considered disjoint to
            // other table types (including each other)
            (&Tables::Record(..), _) | (&Tables::Tuple(..), _) => Tables::All,
            (_, &Tables::Record(..)) | (_, &Tables::Tuple(..)) => Tables::All,

            (&Tables::Empty, tab) => tab.clone(),
            (tab, &Tables::Empty) => tab.clone(),

            (&Tables::Array(ref value1), &Tables::Array(ref value2)) =>
                Tables::Array(value1.union(value2, ctx)),

            (&Tables::Map(ref key1, ref value1), &Tables::Map(ref key2, ref value2)) =>
                Tables::Map(key1.union(key2, ctx), value1.union(value2, ctx)),

            (&Tables::Array(ref value1), &Tables::Map(ref key2, ref value2)) =>
                Tables::Map(Box::new((**key2).union(&T::integer(), ctx)),
                            value1.union(value2, ctx)),
            (&Tables::Map(ref key1, ref value1), &Tables::Array(ref value2)) =>
                Tables::Map(Box::new((**key1).union(&T::integer(), ctx)),
                            value1.union(value2, ctx)),
        };

        Some(tab)
    }

    fn assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        let ok = match (self, other) {
            (&Tables::Empty, _) => true,
            (_, &Tables::Empty) => false,

            (&Tables::All, _) => false,
            (_, &Tables::All) => true,

            (&Tables::Record(ref a), &Tables::Record(ref b)) => {
                for (k, av) in a {
                    if let Some(ref bv) = b.get(k) {
                        try!((**av).assert_sub(bv, ctx));
                    } else {
                        return error_not_sub(self, other);
                    }
                }
                true
            },

            (&Tables::Tuple(ref fields1), &Tables::Tuple(ref fields2)) => {
                for (ty1, ty2) in fields1.iter().zip(fields2.iter()) {
                    try!(ty1.assert_sub(ty2, ctx));
                }
                fields1.len() <= fields2.len()
            },

            // records and tuples are considered disjoint to
            // other table types (including each other)
            (&Tables::Record(..), _) | (&Tables::Tuple(..), _) => false,
            (_, &Tables::Record(..)) | (_, &Tables::Tuple(..)) => false,

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

    fn assert_eq(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
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
                for (k, _) in b {
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

