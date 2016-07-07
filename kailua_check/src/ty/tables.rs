use std::fmt;
use std::borrow::ToOwned;
use std::collections::BTreeMap;

use kailua_syntax::Str;
use diag::CheckResult;
use super::{T, Ty, Slot, SlotWithNil, TypeContext, Lattice};
use super::{error_not_sub, error_not_eq};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Key {
    Int(i32),
    Str(Str),
}

impl Key {
    pub fn is_int(&self) -> bool { if let Key::Int(_) = *self { true } else { false } }
    pub fn is_str(&self) -> bool { if let Key::Str(_) = *self { true } else { false } }

    pub fn into_type(self) -> T<'static> {
        match self {
            Key::Int(v) => T::int(v),
            Key::Str(s) => T::str(s),
        }
    }
}

impl From<i32> for Key { fn from(v: i32) -> Key { Key::Int(v) } }
impl From<Str> for Key { fn from(s: Str) -> Key { Key::Str(s) } }
impl<'a> From<&'a Str> for Key { fn from(s: &Str) -> Key { Key::Str(s.to_owned()) } }

impl fmt::Debug for Key {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Key::Int(ref v) => fmt::Debug::fmt(v, f),
            Key::Str(ref s) => fmt::Debug::fmt(s, f),
        }
    }
}

#[derive(Clone)]
pub enum Tables {
    Empty,

    // tuples and records
    Fields(BTreeMap<Key, Slot>),

    // does not appear naturally (indistinguishable from Map from integers)
    // determined only from the function usages, e.g. table.insert
    Array(SlotWithNil), // shared (non-linear) slots only

    Map(Ty, SlotWithNil), // shared (non-linear) slots only

    // ---

    All,
}

// note: implicitly removes nil as well
fn lift_fields_to_map(fields: &BTreeMap<Key, Slot>, ctx: &mut TypeContext)
                    -> (T<'static>, SlotWithNil) {
    let mut hasint = false;
    let mut hasstr = false;
    let mut value = Slot::just(T::None);
    for (key, ty) in fields {
        if key.is_int() { hasint = true; } else { hasstr = true; }
        value = value.union(ty, ctx);
    }
    assert!(!value.flex().is_linear(), "Slot::union should have destroyed Currently slots");
    let key = match (hasint, hasstr) {
        (false, false) => T::None,
        (false, true) => T::string(),
        (true, false) => T::integer(),
        (true, true) => T::integer() | T::string(),
    };
    (key, SlotWithNil::from_slot(value))
}

impl Tables {
    pub fn lift_to_map(self, ctx: &mut TypeContext) -> Tables {
        match self {
            Tables::Fields(fields) => {
                let (key, value) = lift_fields_to_map(&fields, ctx);
                Tables::Map(Box::new(key), value)
            },
            Tables::Array(value) => Tables::Map(Box::new(T::integer()), value),
            tab => tab,
        }
    }

    // used by table constructors
    // for missing keys the caller should count the number of prior missing keys and put
    // appropriate literal types: `{a=1, 2, [2]=3, 4, b=5}` => `{a=1, [1]=2, [2]=3, [2]=4, b=5}`
    pub fn insert(self, key: T<'static>, value: T<'static>, ctx: &mut TypeContext) -> Tables {
        let litkey =
            if let Some(key) = key.as_integer() {
                Some(key.into())
            } else if let Some(key) = key.as_string() {
                Some(key.into())
            } else {
                None
            };

        match (litkey, self) {
            (Some(litkey), Tables::Empty) => {
                // promote Empty to Fields
                let mut fields = BTreeMap::new();
                fields.insert(litkey, Slot::just(value));
                Tables::Fields(fields)
            }

            (None, Tables::Empty) => {
                // promote Empty to Map
                Tables::Map(Box::new(key), SlotWithNil::from(value))
            }

            (Some(litkey), Tables::Fields(mut fields)) => {
                // should override a duplicate field if any
                fields.insert(litkey, Slot::just(value));
                Tables::Fields(fields)
            }

            // fall back to the map when in doubt
            (_, tab) => match tab.lift_to_map(ctx) {
                Tables::Map(key_, value_) => {
                    let key = key.union(&key_, ctx);
                    let value = Slot::just(value).union(value_.as_slot_without_nil(), ctx);
                    let value = SlotWithNil::from_slot(value);
                    Tables::Map(Box::new(key), value)
                },
                tab => tab,
            }
        }
    }
}

impl Lattice for Tables {
    type Output = Tables;

    fn do_union(&self, other: &Tables, ctx: &mut TypeContext) -> Tables {
        match (self, other) {
            (&Tables::All, _) => Tables::All,
            (_, &Tables::All) => Tables::All,

            (&Tables::Fields(ref fields1), &Tables::Fields(ref fields2)) => {
                let mut fields1 = fields1.clone();
                let mut fields = BTreeMap::new();
                for (k, v2) in fields2 {
                    let k = k.clone();
                    if let Some(v1) = fields1.remove(&k) {
                        fields.insert(k, v1.union(v2, ctx));
                    } else {
                        fields.insert(k, Slot::just(T::Nil).union(v2, ctx));
                    }
                }
                for (k, v1) in fields1 {
                    fields.insert(k.clone(), Slot::just(T::Nil).union(&v1, ctx));
                }
                Tables::Fields(fields)
            },

            (&Tables::Fields(ref fields), &Tables::Empty) |
            (&Tables::Empty, &Tables::Fields(ref fields)) => {
                let add_nil = |(k,s): (&Key, &Slot)| (k.clone(), Slot::just(T::Nil).union(s, ctx));
                Tables::Fields(fields.iter().map(add_nil).collect())
            },

            (&Tables::Fields(ref fields), &Tables::Array(ref value)) |
            (&Tables::Array(ref value), &Tables::Fields(ref fields)) => {
                let (fkey, fvalue) = lift_fields_to_map(fields, ctx);
                Tables::Map(Box::new(fkey.union(&T::integer(), ctx)), fvalue.union(value, ctx))
            },

            (&Tables::Fields(ref fields), &Tables::Map(ref key, ref value)) |
            (&Tables::Map(ref key, ref value), &Tables::Fields(ref fields)) => {
                let (fkey, fvalue) = lift_fields_to_map(fields, ctx);
                Tables::Map(Box::new(fkey.union(key, ctx)), fvalue.union(value, ctx))
            },

            (&Tables::Empty, tab) => tab.clone(),
            (tab, &Tables::Empty) => tab.clone(),

            (&Tables::Array(ref value1), &Tables::Array(ref value2)) =>
                Tables::Array(value1.union(value2, ctx)),

            (&Tables::Map(ref key1, ref value1), &Tables::Map(ref key2, ref value2)) =>
                Tables::Map(Box::new(key1.union(key2, ctx)), value1.union(value2, ctx)),

            (&Tables::Array(ref value1), &Tables::Map(ref key2, ref value2)) =>
                Tables::Map(Box::new(key2.union(&T::integer(), ctx)), value1.union(value2, ctx)),
            (&Tables::Map(ref key1, ref value1), &Tables::Array(ref value2)) =>
                Tables::Map(Box::new(key1.union(&T::integer(), ctx)), value1.union(value2, ctx)),
        }
    }

    fn do_assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        let ok = match (self, other) {
            (&Tables::Empty, _) => true,
            (_, &Tables::Empty) => false,

            (_, &Tables::All) => true,
            (&Tables::All, _) => false,

            (&Tables::Fields(ref a), &Tables::Fields(ref b)) => {
                for (k, av) in a {
                    if let Some(ref bv) = b.get(k) {
                        try!(av.assert_sub(*bv, ctx));
                    } else {
                        return error_not_sub(self, other);
                    }
                }
                true
            },

            (&Tables::Fields(ref fields), &Tables::Map(ref key, ref value)) => {
                for (k, v) in fields {
                    try!(k.clone().into_type().assert_sub(key, ctx));
                    try!(v.assert_sub(value.as_slot_without_nil(), ctx));
                }
                true
            },

            (&Tables::Fields(..), _) => false,
            (_, &Tables::Fields(..)) => false,

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

    fn do_assert_eq(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        let ok = match (self, other) {
            (&Tables::All, &Tables::All) => true,
            (&Tables::Empty, &Tables::Empty) => true,
            (&Tables::Array(ref a), &Tables::Array(ref b)) => return a.assert_eq(b, ctx),
            (&Tables::Map(ref ak, ref av), &Tables::Map(ref bk, ref bv)) => {
                try!(ak.assert_eq(bk, ctx));
                try!(av.assert_eq(bv, ctx));
                true
            }
            (&Tables::Fields(ref a), &Tables::Fields(ref b)) => {
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

            (&Tables::Fields(ref a), &Tables::Fields(ref b)) => *a == *b,
            (&Tables::Fields(ref a), &Tables::Empty) => a.is_empty(),
            (&Tables::Empty, &Tables::Fields(ref b)) => b.is_empty(),

            (_, _) => false,
        }
    }
}

impl fmt::Debug for Tables {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Tables::All => write!(f, "table"),
            Tables::Empty => write!(f, "{{}}"),
            Tables::Fields(ref fields) => {
                try!(write!(f, "{{"));
                let mut first = true;
                // try consecutive initial integers first
                let mut nextlen = 1;
                while let Some(t) = fields.get(&Key::Int(nextlen)) {
                    if first { first = false; } else { try!(write!(f, ", ")); }
                    try!(write!(f, "{:?}", *t));
                    if nextlen >= 0x10000 { break; } // too much
                    nextlen += 1;
                }
                // print other keys
                for (name, t) in fields.iter() {
                    match *name {
                        Key::Int(v) if 1 <= v && v < nextlen => break, // strip duplicates
                        _ => {}
                    }
                    if first { first = false; } else { try!(write!(f, ", ")); }
                    try!(write!(f, "{:?} = {:?}", *name, *t));
                }
                // put an additional comma if there is a single numeric field (i.e. `{var T,}`)
                if nextlen == 2 && fields.len() == 1 {
                    try!(write!(f, ","));
                }
                write!(f, "}}")
            }
            Tables::Array(ref t) => write!(f, "{{{:?}}}", *t),
            Tables::Map(ref k, ref v) => write!(f, "{{[{:?}] = {:?}}}", *k, *v),
        }
    }
}

