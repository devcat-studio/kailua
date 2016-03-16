use std::fmt;
use std::borrow::ToOwned;
use std::collections::HashMap;

use kailua_syntax::Str;
use diag::CheckResult;
use super::{T, Ty, Union, TVarContext, Lattice, Numbers, Strings};
use super::{error_not_sub, error_not_eq};
use super::flags::*;

#[derive(Clone)]
pub enum Tables {
    Empty,
    Record(HashMap<Str, Ty>),
    Tuple(Vec<Ty>),
    Array(Ty),
    Map(Ty, Ty),
    All,
}

impl Tables {
    pub fn lift_to_map(self, ctx: &mut TVarContext) -> Tables {
        match self {
            Tables::Empty => Tables::Empty,
            Tables::Record(fields) => {
                let mut value = T::None;
                for (_, ty) in fields { value = value.union(*ty, ctx); }
                Tables::Map(Box::new(T::string()), Box::new(value))
            },
            Tables::Tuple(fields) => {
                let mut value = T::None;
                for ty in fields { value = value.union(*ty, ctx); }
                Tables::Map(Box::new(T::integer()), Box::new(value))
            },
            Tables::Array(value) => Tables::Map(Box::new(T::integer()), value),
            Tables::Map(key, value) => Tables::Map(key, value),
            Tables::All => Tables::All,
        }
    }

    pub fn insert(self, key: Option<T<'static>>, value: T<'static>,
                  ctx: &mut TVarContext) -> Tables {
        // a single string key is special
        if let Some(ref key) = key {
            if key.flags() == T_STRING {
                if let Some(&Strings::One(ref s)) = key.has_strings() {
                    match self {
                        Tables::Empty => {
                            let mut fields = HashMap::new();
                            fields.insert(s.to_owned(), Box::new(value));
                            return Tables::Record(fields);
                        }
                        Tables::Record(mut fields) => {
                            // should override a duplicate field if any
                            fields.insert(s.to_owned(), Box::new(value));
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
            (None, Tables::Empty) => Tables::Array(Box::new(value)),
            (None, Tables::Array(t)) =>
                Tables::Array(Box::new(value.union(*t, ctx))),
            (None, Tables::Tuple(mut fields)) => {
                fields.push(Box::new(value));
                Tables::Tuple(fields)
            },

            (Some(key), Tables::Empty) => Tables::Map(Box::new(key), Box::new(value)),

            // fall back to the map when in doubt
            (key, tab) => match tab.lift_to_map(ctx) {
                Tables::Map(key_, value_) => {
                    let key = key.unwrap_or(T::integer()).union(*key_, ctx);
                    let value = value.union(*value_, ctx);
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

                let norm_kv = |(k, v): (Str, Ty)| {
                    let v = v.normalize();
                    if v.is_none() { None } else { Some((k, v)) }
                };
                let fields: HashMap<Str, Ty> = fields.into_iter().filter_map(norm_kv).collect();
                if fields.is_empty() {
                    Some(Tables::Empty)
                } else {
                    Some(Tables::Record(fields))
                }
            },

            Tables::Tuple(fields) => {
                if fields.is_empty() { return Some(Tables::Empty); }

                let norm = |v: Ty| {
                    let v = v.normalize();
                    if v.is_none() { None } else { Some(v) }
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

    fn union(self, other: Tables, ctx: &mut TVarContext) -> Option<Tables> {
        fn union_rec_tup(rec: HashMap<Str, Ty>, mut tup: Vec<Ty>,
                         ctx: &mut TVarContext) -> Tables {
            let mut uty = tup.pop().unwrap();
            for ty in tup {
                uty = uty.union(ty, ctx);
            }
            for (_, ty) in rec {
                uty = uty.union(ty, ctx);
            }
            Tables::Map(Box::new(T::integer().union(T::string(), ctx)), uty)
        }

        fn union_rec_map(fields: HashMap<Str, Ty>, key: Ty, value: Ty,
                         ctx: &mut TVarContext) -> Tables {
            let mut uty = value;
            for (_, ty) in fields {
                uty = uty.union(ty, ctx);
            }
            Tables::Map(key.union(Box::new(T::string()), ctx), uty)
        }

        fn union_tup_map(fields: Vec<Ty>, key: Ty, value: Ty, ctx: &mut TVarContext) -> Tables {
            let mut uty = value;
            for ty in fields {
                uty = uty.union(ty, ctx);
            }
            Tables::Map(key.union(Box::new(T::integer()), ctx), uty)
        }

        let tab = match (self, other) {
            (Tables::Empty, tab) => tab,
            (tab, Tables::Empty) => tab,

            (Tables::All, _) => Tables::All,
            (_, Tables::All) => Tables::All,

            (Tables::Record(mut fields1), Tables::Record(fields2)) => {
                for (k, v2) in fields2 {
                    if let Some(v1) = fields1.remove(&k) {
                        fields1.insert(k, v1.union(v2, ctx));
                    } else {
                        fields1.insert(k, v2);
                    }
                }
                Tables::Record(fields1)
            },

            (Tables::Record(fields1), Tables::Tuple(fields2)) =>
                union_rec_tup(fields1, fields2, ctx),
            (Tables::Tuple(fields1), Tables::Record(fields2)) =>
                union_rec_tup(fields2, fields1, ctx),

            (Tables::Record(fields), Tables::Array(value)) =>
                union_rec_map(fields, Box::new(T::integer()), value, ctx),
            (Tables::Array(value), Tables::Record(fields)) =>
                union_rec_map(fields, Box::new(T::integer()), value, ctx),

            (Tables::Record(fields), Tables::Map(key, value)) =>
                union_rec_map(fields, key, value, ctx),
            (Tables::Map(key, value), Tables::Record(fields)) =>
                union_rec_map(fields, key, value, ctx),

            (Tables::Tuple(mut fields1), Tables::Tuple(mut fields2)) => {
                if fields1.len() < fields2.len() {
                    fields1.resize(fields2.len(), Box::new(T::Nil));
                } else if fields1.len() > fields2.len() {
                    fields2.resize(fields1.len(), Box::new(T::Nil));
                }
                let tys = fields1.into_iter().zip(fields2.into_iter());
                Tables::Tuple(tys.map(|(lty, rty)| lty.union(rty, ctx)).collect())
            },

            (Tables::Tuple(fields), Tables::Array(value)) =>
                union_tup_map(fields, Box::new(T::integer()), value, ctx),
            (Tables::Array(value), Tables::Tuple(fields)) =>
                union_tup_map(fields, Box::new(T::integer()), value, ctx),

            (Tables::Tuple(fields), Tables::Map(key, value)) =>
                union_tup_map(fields, key, value, ctx),
            (Tables::Map(key, value), Tables::Tuple(fields)) =>
                union_tup_map(fields, key, value, ctx),

            (Tables::Array(value1), Tables::Array(value2)) =>
                Tables::Array(value1.union(value2, ctx)),

            (Tables::Map(key1, value1), Tables::Map(key2, value2)) =>
                Tables::Map(key1.union(key2, ctx), value1.union(value2, ctx)),

            (Tables::Array(value1), Tables::Map(key2, value2)) =>
                Tables::Map(key2.union(Box::new(T::integer()), ctx), value1.union(value2, ctx)),
            (Tables::Map(key1, value1), Tables::Array(value2)) =>
                Tables::Map(key1.union(Box::new(T::integer()), ctx), value1.union(value2, ctx)),
        };

        Some(tab)
    }

    fn intersect(self, other: Tables, ctx: &mut TVarContext) -> Option<Tables> {
        fn intersect_tup_arr(fields: Vec<Ty>, value: Ty,
                             ctx: &mut TVarContext) -> Tables {
            let mut newfields = Vec::new();
            for ty in fields {
                let v = ty.intersect(value.clone(), ctx);
                if v.is_none() { return Tables::Empty; }
                newfields.push(v);
            }
            Tables::Tuple(newfields)
        }

        fn intersect_rec_map(fields: HashMap<Str, Ty>, key: Ty, value: Ty,
                             ctx: &mut TVarContext) -> Tables {
            fn merge<F: Fn(&Str) -> bool>(fields: HashMap<Str, Ty>, value: Ty,
                                          ctx: &mut TVarContext, cond: F) -> Tables {
                let mut newfields = HashMap::new();
                for (k, ty) in fields {
                    if cond(&k) { 
                        let v = ty.intersect(value.clone(), ctx);
                        if !v.is_none() { newfields.insert(k, v); }
                    }
                }
                Tables::Record(newfields)
            }

            let key = Union::from(*key);
            if key.has_dynamic {
                merge(fields, value, ctx, |_| true)
            } else {
                match key.strings {
                    None => Tables::Empty,
                    Some(Strings::One(ref s)) => merge(fields, value, ctx, |k| *s == *k),
                    Some(Strings::Some(ref set)) => merge(fields, value, ctx, |k| set.contains(k)),
                    Some(Strings::All) => merge(fields, value, ctx, |_| true),
                }
            }
        }

        fn intersect_tup_map(fields: Vec<Ty>, key: Ty, value: Ty,
                             ctx: &mut TVarContext) -> Tables {
            fn merge<F: Fn(i32) -> bool>(fields: Vec<Ty>, value: Ty,
                                         ctx: &mut TVarContext, cond: F) -> Tables {
                let mut newfields = Vec::new();
                for (k, ty) in fields.into_iter().enumerate() {
                    if cond(k as i32) { 
                        let v = ty.intersect(value.clone(), ctx);
                        if !v.is_none() { newfields.push(v); }
                    } else {
                        newfields.push(Box::new(T::None));
                    }
                }
                Tables::Tuple(newfields)
            };

            let key = Union::from(*key);
            if key.has_dynamic {
                merge(fields, value, ctx, |_| true)
            } else {
                match key.numbers {
                    None => Tables::Empty,
                    Some(Numbers::One(v)) => merge(fields, value, ctx, |k| k == v),
                    Some(Numbers::Some(ref set)) => merge(fields, value, ctx, |k| set.contains(&k)),
                    Some(Numbers::Int) | Some(Numbers::All) => merge(fields, value, ctx, |_| true),
                }
            }
        }

        fn intersect_arr_map(elem: Ty, key: Ty, value: Ty,
                             ctx: &mut TVarContext) -> Tables {
            let key = Union::from(*key);
            if key.has_dynamic {
                Tables::Array(elem.intersect(value, ctx))
            } else {
                match key.numbers {
                    None | Some(Numbers::One(..)) | Some(Numbers::Some(..)) => Tables::Empty,
                    Some(Numbers::Int) | Some(Numbers::All) =>
                        Tables::Array(elem.intersect(value, ctx)),
                }
            }
        }

        let tab = match (self, other) {
            (Tables::Empty, _) => Tables::Empty,
            (_, Tables::Empty) => Tables::Empty,

            (Tables::All, tab) => tab,
            (tab, Tables::All) => tab,

            (Tables::Record(mut fields1), Tables::Record(fields2)) => {
                let mut fields = HashMap::new();
                for (k, v2) in fields2 {
                    if let Some(v1) = fields1.remove(&k) {
                        let v = v1.intersect(v2, ctx);
                        if !v.is_none() { fields.insert(k, v); }
                    }
                }
                Tables::Record(fields)
            },

            (Tables::Record(_), Tables::Tuple(_)) => Tables::Empty,
            (Tables::Tuple(_), Tables::Record(_)) => Tables::Empty,

            (Tables::Record(_), Tables::Array(_)) => Tables::Empty,
            (Tables::Array(_), Tables::Record(_)) => Tables::Empty,

            (Tables::Record(fields), Tables::Map(key, value)) =>
                intersect_rec_map(fields, key, value, ctx),
            (Tables::Map(key, value), Tables::Record(fields)) =>
                intersect_rec_map(fields, key, value, ctx),

            (Tables::Tuple(fields1), Tables::Tuple(fields2)) => {
                let mut fields = Vec::new();
                for (ty1, ty2) in fields1.into_iter().zip(fields2.into_iter()) {
                    let ty = ty1.intersect(ty2, ctx);
                    if ty.is_none() { return Some(Tables::Empty); }
                    fields.push(ty);
                }
                Tables::Tuple(fields)
            },

            (Tables::Tuple(fields), Tables::Array(value)) =>
                intersect_tup_arr(fields, value, ctx),
            (Tables::Array(value), Tables::Tuple(fields)) =>
                intersect_tup_arr(fields, value, ctx),

            (Tables::Tuple(fields), Tables::Map(key, value)) =>
                intersect_tup_map(fields, key, value, ctx),
            (Tables::Map(key, value), Tables::Tuple(fields)) =>
                intersect_tup_map(fields, key, value, ctx),

            (Tables::Array(value1), Tables::Array(value2)) =>
                Tables::Array(value1.intersect(value2, ctx)),

            (Tables::Map(key1, value1), Tables::Map(key2, value2)) =>
                Tables::Map(key1.intersect(key2, ctx), value1.intersect(value2, ctx)),

            (Tables::Array(value1), Tables::Map(key2, value2)) =>
                intersect_arr_map(value1, key2, value2, ctx),
            (Tables::Map(key1, value1), Tables::Array(value2)) =>
                intersect_arr_map(value2, key1, value1, ctx),
        };

        Some(tab)
    }

    fn assert_sub(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        let ok = match (self, other) {
            (&Tables::Empty, _) => true,
            (_, &Tables::Empty) => false,

            (&Tables::All, _) => false,
            (_, &Tables::All) => true,

            (&Tables::Record(ref a), &Tables::Record(ref b)) => {
                for (k, av) in a {
                    let none = T::None;
                    let bv = b.get(k).map_or(&none, |t| &*t);
                    try!((**av).assert_sub(bv, ctx));
                }
                true
            },

            (&Tables::Record(..), &Tables::Tuple(..)) => false,
            (&Tables::Tuple(..), &Tables::Record(..)) => false,

            (&Tables::Record(..), &Tables::Array(..)) => false,
            (&Tables::Array(..), &Tables::Record(..)) => false,

            (&Tables::Record(ref fields), &Tables::Map(ref key, ref value)) => {
                let mut ok = true;
                for (k, ty) in fields {
                    if let Some(str) = key.has_strings() {
                        try!(str.assert_sup_str(k, ctx));
                    } else {
                        ok = false;
                        break;
                    }
                    try!(ty.assert_sub(value, ctx));
                }
                ok
            },
            (&Tables::Map(..), &Tables::Record(..)) => false,

            (&Tables::Tuple(ref fields1), &Tables::Tuple(ref fields2)) => {
                for (ty1, ty2) in fields1.iter().zip(fields2.iter()) {
                    try!(ty1.assert_sub(ty2, ctx));
                }
                fields1.len() <= fields2.len()
            },

            (&Tables::Tuple(ref fields), &Tables::Array(ref value)) => {
                for ty in fields { try!(ty.assert_sub(value, ctx)); }
                true
            },
            (&Tables::Array(..), &Tables::Tuple(..)) => false,

            (&Tables::Tuple(ref fields), &Tables::Map(ref key, ref value)) => {
                for (i, ty) in fields.iter().enumerate() {
                    try!(T::int(i as i32).assert_sub(key, ctx));
                    try!(ty.assert_sub(value, ctx));
                }
                true
            },
            (&Tables::Map(..), &Tables::Tuple(..)) => false,

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

    fn assert_eq(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
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

