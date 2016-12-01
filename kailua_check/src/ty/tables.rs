use std::fmt;
use std::borrow::Cow;
use std::collections::BTreeMap;

use kailua_syntax::Str;
use diag::{CheckResult, unquotable_name};
use super::{T, Ty, Slot, TypeContext, Lattice, Display};
use super::{error_not_sub, error_not_eq};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Key {
    Int(i32),
    Str(Str),
}

impl Key {
    pub fn is_int(&self) -> bool { if let Key::Int(_) = *self { true } else { false } }
    pub fn is_str(&self) -> bool { if let Key::Str(_) = *self { true } else { false } }

    pub fn to_type<'a>(&'a self) -> T<'a> {
        match *self {
            Key::Int(v) => T::Int(v),
            Key::Str(ref s) => T::Str(Cow::Borrowed(s)),
        }
    }
}

impl From<i32> for Key { fn from(v: i32) -> Key { Key::Int(v) } }
impl From<Str> for Key { fn from(s: Str) -> Key { Key::Str(s) } }
impl<'a> From<&'a Str> for Key { fn from(s: &Str) -> Key { Key::Str(s.to_owned()) } }

impl PartialEq<Str> for Key {
    fn eq(&self, other: &Str) -> bool {
        if let Key::Str(ref s) = *self { *s == *other } else { false }
    }
}

impl<'a> PartialEq<&'a [u8]> for Key {
    fn eq(&self, other: &&'a [u8]) -> bool {
        if let Key::Str(ref s) = *self { **s == **other } else { false }
    }
}

impl PartialEq<i32> for Key {
    fn eq(&self, other: &i32) -> bool {
        if let Key::Int(ref v) = *self { *v == *other } else { false }
    }
}

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Key::Int(ref v) => write!(f, "{:?}", *v),
            Key::Str(ref s) if unquotable_name(s) => write!(f, "{:-?}", *s),
            Key::Str(ref s) => write!(f, "`{:-?}`", *s),
        }
    }
}

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
    Array(Slot), // shared (non-linear) slots only, value implicitly unioned with nil

    Map(Ty, Slot), // shared (non-linear) slots only, value implicitly unioned with nil

    // ---

    All,
}

fn lift_fields_to_map(fields: &BTreeMap<Key, Slot>, ctx: &mut TypeContext)
                    -> (T<'static>, Slot) {
    let mut hasint = false;
    let mut hasstr = false;
    let mut value = Slot::just(Ty::new(T::None));
    for (key, ty) in fields {
        if key.is_int() { hasint = true; } else { hasstr = true; }
        value = value.union(ty, ctx);
    }
    assert!(!value.flex().is_linear(), "Slot::union should have destroyed Currently slots");
    let key = match (hasint, hasstr) {
        (false, false) => T::None,
        (false, true) => T::String,
        (true, false) => T::Integer,
        (true, true) => T::Integer | T::String,
    };
    (key, value)
}

impl Tables {
    fn fmt_generic<WriteTy, WriteSlot>(&self, f: &mut fmt::Formatter,
                                       mut write_ty: WriteTy,
                                       mut write_slot: WriteSlot) -> fmt::Result
            where WriteTy: FnMut(&T, &mut fmt::Formatter) -> fmt::Result,
                  WriteSlot: FnMut(&Slot, &mut fmt::Formatter, bool) -> fmt::Result {
        match *self {
            Tables::All => write!(f, "table"),

            Tables::Empty => write!(f, "{{}}"),

            Tables::Fields(ref fields) => {
                write!(f, "{{")?;
                let mut first = true;
                // try consecutive initial integers first
                let mut nextlen = 1;
                while let Some(t) = fields.get(&Key::Int(nextlen)) {
                    if first { first = false; } else { write!(f, ", ")?; }
                    write_slot(t, f, false)?;
                    if nextlen >= 0x10000 { break; } // too much
                    nextlen += 1;
                }
                // print other keys
                for (name, t) in fields.iter() {
                    match *name {
                        Key::Int(v) if 1 <= v && v < nextlen => continue, // strip duplicates
                        _ => {}
                    }
                    if first { first = false; } else { write!(f, ", ")?; }
                    write!(f, "{} = ", name)?;
                    write_slot(t, f, false)?;
                }
                write!(f, "}}")?;
                Ok(())
            }

            Tables::Array(ref t) => {
                write!(f, "vector<")?;
                write_slot(t, f, true)?;
                write!(f, ">")?;
                Ok(())
            }

            Tables::Map(ref k, ref v) => {
                write!(f, "map<")?;
                write_ty(k, f)?;
                write!(f, ", ")?;
                write_slot(v, f, true)?;
                write!(f, ">")?;
                Ok(())
            }
        }
    }
}

impl Lattice for Tables {
    type Output = Tables;

    fn union(&self, other: &Tables, ctx: &mut TypeContext) -> Tables {
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
                        fields.insert(k, Slot::just(Ty::silent_nil()).union(v2, ctx));
                    }
                }
                for (k, v1) in fields1 {
                    fields.insert(k.clone(), v1.union(&Slot::just(Ty::silent_nil()), ctx));
                }
                Tables::Fields(fields)
            },

            (&Tables::Fields(ref fields), &Tables::Empty) |
            (&Tables::Empty, &Tables::Fields(ref fields)) => {
                let nil = Slot::just(Ty::silent_nil());
                Tables::Fields(fields.iter().map(|(k,s)| (k.clone(), s.union(&nil, ctx))).collect())
            },

            (&Tables::Fields(ref fields), &Tables::Array(ref value)) |
            (&Tables::Array(ref value), &Tables::Fields(ref fields)) => {
                let (fkey, fvalue) = lift_fields_to_map(fields, ctx);
                Tables::Map(Ty::new(fkey.union(&T::Integer, ctx)), fvalue.union(value, ctx))
            },

            (&Tables::Fields(ref fields), &Tables::Map(ref key, ref value)) |
            (&Tables::Map(ref key, ref value), &Tables::Fields(ref fields)) => {
                let (fkey, fvalue) = lift_fields_to_map(fields, ctx);
                Tables::Map(Ty::new(fkey.union(&**key, ctx)), fvalue.union(value, ctx))
            },

            (&Tables::Empty, tab) => tab.clone(),
            (tab, &Tables::Empty) => tab.clone(),

            (&Tables::Array(ref value1), &Tables::Array(ref value2)) =>
                Tables::Array(value1.union(value2, ctx)),

            (&Tables::Map(ref key1, ref value1), &Tables::Map(ref key2, ref value2)) =>
                Tables::Map(key1.union(key2, ctx), value1.union(value2, ctx)),

            (&Tables::Array(ref value1), &Tables::Map(ref key2, ref value2)) =>
                Tables::Map(Ty::new((**key2).union(&T::Integer, ctx)),
                            value1.union(value2, ctx)),
            (&Tables::Map(ref key1, ref value1), &Tables::Array(ref value2)) =>
                Tables::Map(Ty::new((**key1).union(&T::Integer, ctx)),
                            value1.union(value2, ctx)),
        }
    }

    fn assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        let ok = match (self, other) {
            (&Tables::Empty, _) => true,
            (_, &Tables::Empty) => false,

            (_, &Tables::All) => true,
            (&Tables::All, _) => false,

            (&Tables::Fields(ref a), &Tables::Fields(ref b)) => {
                for (k, av) in a {
                    if let Some(ref bv) = b.get(k) {
                        av.assert_sub(*bv, ctx)?;
                    } else {
                        return error_not_sub(self, other);
                    }
                }
                true
            },

            (&Tables::Fields(ref fields), &Tables::Map(ref key, ref value)) => {
                for (k, v) in fields {
                    k.to_type().assert_sub(&**key, ctx)?;
                    v.assert_sub(&value.clone().with_nil(), ctx)?;
                }
                true
            },

            (&Tables::Fields(ref fields), &Tables::Array(ref value)) => {
                // the fields should have consecutive integer keys
                for (idx, (k, v)) in fields.iter().enumerate() {
                    k.to_type().assert_sub(&T::Int(idx as i32 + 1), ctx)?;
                    v.assert_sub(&value.clone().with_nil(), ctx)?;
                }
                true
            },

            (_, &Tables::Fields(..)) => false,

            (&Tables::Array(ref value1), &Tables::Array(ref value2)) => {
                value1.assert_sub(value2, ctx)?;
                true
            },

            (&Tables::Map(ref key1, ref value1), &Tables::Map(ref key2, ref value2)) => {
                key1.assert_sub(key2, ctx)?;
                value1.assert_sub(value2, ctx)?;
                true
            },

            (&Tables::Array(ref value1), &Tables::Map(ref key2, ref value2)) => {
                T::Integer.assert_sub(&**key2, ctx)?;
                value1.assert_sub(value2, ctx)?;
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
                ak.assert_eq(bk, ctx)?;
                av.assert_eq(bv, ctx)?;
                true
            }
            (&Tables::Fields(ref a), &Tables::Fields(ref b)) => {
                for (k, va) in a {
                    if let Some(vb) = b.get(k) {
                        va.assert_eq(vb, ctx)?;
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

impl Display for Tables {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, ctx: &TypeContext) -> fmt::Result {
        self.fmt_generic(
            f,
            |t, f| fmt::Display::fmt(&t.display(ctx), f),
            |s, f, without_nil| {
                let s = s.display(ctx);
                if without_nil { write!(f, "{:#}", s) } else { write!(f, "{}", s) }
            }
        )
    }
}

impl fmt::Debug for Tables {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_generic(
            f,
            |t, f| fmt::Debug::fmt(t, f),
            |s, f, without_nil| {
                if without_nil { write!(f, "{:#?}", s) } else { write!(f, "{:?}", s) }
            }
        )
    }
}

