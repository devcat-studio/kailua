use std::fmt;
use std::i32;
use std::borrow::Cow;
use std::collections::BTreeMap;

use kailua_syntax::Str;
use diag::{Origin, TypeReport, TypeResult};
use super::{Display, DisplayState, T, Ty, Slot, TypeContext, Union, Lattice, RVar};

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
            Key::Int(ref v) => write!(f, "{}", v),
            Key::Str(ref s) if s.quote_required() => write!(f, "`{:-}`", s),
            Key::Str(ref s) => write!(f, "{:-}", s),
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
    // tuples and records are internally treated equally, and stored as a row variable
    // also includes an empty table
    Fields(RVar),

    // does not appear naturally (indistinguishable from Map from integers)
    // determined only from the function usages, e.g. table.insert
    Array(Slot), // shared (non-linear) slots only, value implicitly unioned with nil

    // key should not contain nil (will be discarded)
    Map(Ty, Slot), // shared (non-linear) slots only, value implicitly unioned with nil

    // ---

    All,
}

impl Tables {
    pub fn generalize(self, ctx: &mut TypeContext) -> Tables {
        match self {
            Tables::Fields(r) => Tables::Fields(ctx.copy_rvar(r)),
            Tables::Array(v) => Tables::Array(v.generalize(ctx)),
            Tables::Map(k, v) => {
                let k = k.generalize(ctx);
                let v = v.generalize(ctx);
                Tables::Map(k, v)
            },
            Tables::All => Tables::All,
        }
    }

    fn fmt_generic<WriteTy, WriteSlot>(&self, f: &mut fmt::Formatter,
                                       st: Option<&DisplayState>,
                                       mut write_ty: WriteTy,
                                       mut write_slot: WriteSlot,
                                       expose_rvar: bool) -> fmt::Result
            where WriteTy: FnMut(&Ty, &mut fmt::Formatter) -> fmt::Result,
                  WriteSlot: FnMut(&Slot, &mut fmt::Formatter, bool) -> fmt::Result {
        match *self {
            Tables::All => write!(f, "table"),

            Tables::Fields(ref rvar) => {
                if let Some(st) = st {
                    if st.is_rvar_seen(rvar.clone()) {
                        return write!(f, "<...>");
                    }
                }

                let ret = (|| {
                    let mut fields = BTreeMap::new();
                    let mut morefields = 0;

                    // do not try to copy too many fields
                    const MAX_FIELDS: usize = 0x100;

                    // keys have to be sorted in the output, but row variables may have been
                    // instantiated in an arbitrary order, so we need to collect and sort them
                    // when ctx is available. otherwise we just print ctx out.
                    let rvar = if let Some(st) = st {
                        st.context.list_rvar_fields(rvar.clone(), &mut |k, v| {
                            if fields.len() < MAX_FIELDS {
                                fields.insert(k.clone(), v.clone());
                            } else {
                                morefields += 1;
                            }
                            Ok(())
                        }).expect("list_rvar_fields exited early while we haven't break")
                    } else {
                        rvar.clone()
                    };

                    write!(f, "{{")?;
                    let mut first = true;

                    // try consecutive initial integers first
                    let mut nextlen = 1;
                    while let Some(t) = fields.get(&Key::Int(nextlen)) {
                        if first { first = false; } else { write!(f, ", ")?; }
                        write_slot(t, f, false)?;
                        nextlen += 1;
                    }

                    // print other keys
                    for (name, t) in fields.iter() {
                        match *name {
                            Key::Int(v) if 1 <= v && v < nextlen => continue, // strip duplicates
                            _ => {}
                        }
                        if first { first = false; } else { write!(f, ", ")?; }
                        write!(f, "{}: ", name)?;
                        write_slot(t, f, false)?;
                    }

                    // print the number of omitted fields if any
                    if morefields > 0 {
                        if first { first = false; } else { write!(f, ", ")?; }
                        write!(f, "<{} fields omitted>", morefields)?;
                    }

                    // print the extension if any
                    if rvar != RVar::empty() {
                        if !first { write!(f, ", ")?; }
                        write!(f, "...")?;
                        if expose_rvar {
                            if rvar == RVar::any() {
                                write!(f, "?")?;
                            } else {
                                write!(f, "{}", rvar.to_usize())?;
                            }
                        }
                    }

                    write!(f, "}}")?;
                    Ok(())
                })();

                if let Some(st) = st {
                    st.unmark_rvar(rvar.clone());
                }

                ret
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

impl Union for Tables {
    type Output = Tables;

    fn union(&self, other: &Tables, explicit: bool, ctx: &mut TypeContext) -> TypeResult<Tables> {
        (|| {
            match (self, other) {
                (_, &Tables::All) | (&Tables::All, _) => Ok(Tables::All),

                // for the same kind of tables, they should be equal to each other
                (&Tables::Array(ref a), &Tables::Array(ref b)) => {
                    a.assert_eq(b, ctx)?;
                    Ok(Tables::Array(a.clone()))
                },
                (&Tables::Map(ref ak, ref av), &Tables::Map(ref bk, ref bv)) => {
                    ak.assert_eq(bk, ctx)?;
                    av.assert_eq(bv, ctx)?;
                    Ok(Tables::Map(ak.clone(), av.clone()))
                },
                (&Tables::Fields(ref ar), &Tables::Fields(ref br)) => {
                    ar.assert_eq(&br, ctx)?;
                    Ok(Tables::Fields(ar.clone()))
                },

                // for the records and non-records, records should be a subtype of non-records
                // (and should be no longer extensible)
                (lhs @ &Tables::Fields(_), rhs) => {
                    lhs.assert_sub(rhs, ctx)?;
                    Ok(rhs.clone())
                },
                (lhs, rhs @ &Tables::Fields(_)) => {
                    rhs.assert_sub(lhs, ctx)?;
                    Ok(lhs.clone())
                },

                (_, _) => Err(ctx.gen_report()),
            }
        })().map_err(|r: TypeReport| r.cannot_union(Origin::Tables, self, other, explicit, ctx))
    }
}

impl Lattice for Tables {
    fn assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> TypeResult<()> {
        (|| {
            let ok = match (self, other) {
                (_, &Tables::All) => true,
                (&Tables::All, _) => false,

                (&Tables::Fields(ref ar), &Tables::Fields(ref br)) => {
                    return ar.assert_sub(&br, ctx);
                },

                (&Tables::Fields(ref rvar), &Tables::Map(ref key, ref value)) => {
                    // subtyping should hold for existing fields
                    for (k, v) in ctx.get_rvar_fields(rvar.clone()) {
                        k.to_type().assert_sub(&**key, ctx)?;
                        v.assert_sub(&value.clone().with_nil(), ctx)?;
                    }

                    // since an extensible row can result in soundness error,
                    // we disable the extensibility once we need subtyping
                    return ctx.assert_rvar_closed(rvar.clone());
                },

                (&Tables::Fields(ref rvar), &Tables::Array(ref value)) => {
                    // the fields should have consecutive integer keys.
                    // since the fields listing is unordered, we check them by
                    // counting the number of integer keys and determining min and max key.
                    //
                    // this obviously assumes that we have no duplicate keys throughout the chain,
                    // which is the checker's responsibility.
                    let mut min = i32::MAX;
                    let mut max = i32::MIN;
                    let mut count = 0;
                    for (k, v) in ctx.get_rvar_fields(rvar.clone()) {
                        if let Key::Int(k) = k {
                            count += 1;
                            if min > k { min = k; }
                            if max < k { max = k; }
                            v.assert_sub(&value.clone().with_nil(), ctx)?;
                        } else {
                            // regenerate an appropriate error
                            k.to_type().assert_sub(&T::Integer, ctx)?;
                            panic!("non-integral {:?} is typed as integral {:?}", k, k.to_type());
                        }
                    }

                    ctx.assert_rvar_closed(rvar.clone())?;
                    count == 0 || (min == 1 && max == count)
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

            if ok { Ok(()) } else { Err(ctx.gen_report()) }
        })().map_err(|r: TypeReport| r.not_sub(Origin::Tables, self, other, ctx))
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TypeContext) -> TypeResult<()> {
        (|| {
            let ok = match (self, other) {
                (&Tables::All, &Tables::All) => true,
                (&Tables::Array(ref a), &Tables::Array(ref b)) => return a.assert_eq(b, ctx),
                (&Tables::Map(ref ak, ref av), &Tables::Map(ref bk, ref bv)) => {
                    ak.assert_eq(bk, ctx)?;
                    av.assert_eq(bv, ctx)?;
                    true
                }
                (&Tables::Fields(ref ar), &Tables::Fields(ref br)) => return ar.assert_eq(&br, ctx),
                (_, _) => false,
            };

            if ok { Ok(()) } else { Err(ctx.gen_report()) }
        })().map_err(|r: TypeReport| r.not_eq(Origin::Tables, self, other, ctx))
    }
}

impl PartialEq for Tables {
    fn eq(&self, other: &Tables) -> bool {
        match (self, other) {
            (&Tables::All, &Tables::All) => true,
            (&Tables::Array(ref a), &Tables::Array(ref b)) => *a == *b,
            (&Tables::Map(ref ak, ref av), &Tables::Map(ref bk, ref bv)) =>
                *ak == *bk && *av == *bv,
            (&Tables::Fields(ref ar), &Tables::Fields(ref br)) => *ar == *br,
            (_, _) => false,
        }
    }
}

impl Display for Tables {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        self.fmt_generic(
            f, Some(st),
            |t, f| fmt::Display::fmt(&t.display(st), f),
            |s, f, without_nil| {
                let s = s.display(st);
                if without_nil { write!(f, "{:#}", s) } else { write!(f, "{}", s) }
            },
            false
        )
    }
}

impl fmt::Debug for Tables {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_generic(
            f, None,
            |t, f| fmt::Debug::fmt(t, f),
            |s, f, _| fmt::Debug::fmt(s, f),
            true
        )
    }
}

