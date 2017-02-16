use std::fmt;
use std::borrow::Cow;
use std::collections::BTreeMap;

use kailua_syntax::Str;
use diag::{CheckResult, unquotable_name};
use super::{T, Ty, Slot, TypeContext, Lattice, Display, RVar};
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

    // tuples and records are internally treated equally;
    // the extension is represented as a row variable,
    // which can be empty (RVar::empty()) in which case no other field is allowed
    Fields(BTreeMap<Key, Slot>, RVar),

    // does not appear naturally (indistinguishable from Map from integers)
    // determined only from the function usages, e.g. table.insert
    Array(Slot), // shared (non-linear) slots only, value implicitly unioned with nil

    // key should not contain nil (will be discarded)
    Map(Ty, Slot), // shared (non-linear) slots only, value implicitly unioned with nil

    // ---

    All,
}

impl Tables {
    fn fmt_generic<WriteTy, WriteSlot>(&self, f: &mut fmt::Formatter,
                                       mut write_ty: WriteTy,
                                       mut write_slot: WriteSlot,
                                       expose_rvar: bool) -> fmt::Result
            where WriteTy: FnMut(&Ty, &mut fmt::Formatter) -> fmt::Result,
                  WriteSlot: FnMut(&Slot, &mut fmt::Formatter, bool) -> fmt::Result {
        match *self {
            Tables::All => write!(f, "table"),

            Tables::Empty => write!(f, "{{}}"),

            Tables::Fields(ref fields, ref rvar) => {
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

                // print the extension if any
                if expose_rvar {
                    if first { first = false; } else { write!(f, ", ")?; }
                    write!(f, "...{}", rvar.to_usize())?;
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
    fn assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        let ok = match (self, other) {
            (&Tables::Empty, _) => true,
            (_, &Tables::Empty) => false,

            (_, &Tables::All) => true,
            (&Tables::All, _) => false,

            (&Tables::Fields(ref a, ref ar), &Tables::Fields(ref b, ref br)) => {
                for (k, av) in a {
                    if let Some(ref bv) = b.get(k) {
                        av.assert_sub(*bv, ctx)?;
                    } else {
                        return error_not_sub(self, other);
                    }
                }
                // TODO do something with ar and br
                true
            },

            (&Tables::Fields(ref fields, ref rvar), &Tables::Map(ref key, ref value)) => {
                // since an extensible row can result in soundness error,
                // we disable the extensibility once we need subtyping
                let rvar = rvar.ensure(ctx);
                ctx.assert_rvar_closed(rvar)?;

                for (k, v) in fields {
                    k.to_type().assert_sub(&**key, ctx)?;
                    v.assert_sub(&value.clone().with_nil(), ctx)?;
                }
                true
            },

            (&Tables::Fields(ref fields, ref rvar), &Tables::Array(ref value)) => {
                let rvar = rvar.ensure(ctx);
                ctx.assert_rvar_closed(rvar)?;

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
            (&Tables::Fields(ref a, ref ar), &Tables::Fields(ref b, ref br)) => {
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
                // TODO do something with ar and br
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

            (&Tables::Fields(ref a, ref ar), &Tables::Fields(ref b, ref br)) =>
                *a == *b && ar == br,
            (&Tables::Fields(ref a, ref ar), &Tables::Empty) =>
                a.is_empty() && *ar == RVar::empty(),
            (&Tables::Empty, &Tables::Fields(ref b, ref br)) =>
                b.is_empty() && *br == RVar::empty(),

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
            },
            false
        )
    }
}

impl fmt::Debug for Tables {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_generic(f, |t, f| fmt::Debug::fmt(t, f), |s, f, _| fmt::Debug::fmt(s, f), true)
    }
}

