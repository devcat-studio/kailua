use std::fmt;
use std::ops;
use std::borrow::Cow;
use std::collections::BTreeMap;

use kailua_syntax::{K, SlotKind, Str, M};
use diag::CheckResult;
use super::{F, Slot, SlotWithNil, TypeContext, NoTypeContext, TypeResolver, Lattice, TySeq};
use super::{Numbers, Strings, Key, Tables, Function, Functions, Union, TVar, Builtin};
use super::{error_not_sub, error_not_eq};
use super::flags::*;

// basic value types, also used for enumeration and construction
#[derive(Clone)]
pub enum T<'a> {
    Dynamic,                            // WHATEVER
    All,                                // any (top)
    None,                               // (bottom)
    Nil,                                // nil
    Boolean,                            // boolean
    True,                               // true
    False,                              // false
    Thread,                             // thread
    UserData,                           // userdata
    Numbers(Cow<'a, Numbers>),          // number, ...
    Strings(Cow<'a, Strings>),          // string, ...
    Tables(Cow<'a, Tables>),            // table, ...
    Functions(Cow<'a, Functions>),      // function, ...
    TVar(TVar),                         // type variable
    Builtin(Builtin, Box<T<'a>>),       // builtin types (cannot be nested)
    Union(Cow<'a, Union>),              // union types A | B | ...
}

impl<'a> T<'a> {
    pub fn number()          -> T<'a> { T::Numbers(Cow::Owned(Numbers::All)) }
    pub fn integer()         -> T<'a> { T::Numbers(Cow::Owned(Numbers::Int)) }
    pub fn int(v: i32)       -> T<'a> { T::Numbers(Cow::Owned(Numbers::One(v))) }
    pub fn string()          -> T<'a> { T::Strings(Cow::Owned(Strings::All)) }
    pub fn str(s: Str)       -> T<'a> { T::Strings(Cow::Owned(Strings::One(s))) }
    pub fn table()           -> T<'a> { T::Tables(Cow::Owned(Tables::All)) }
    pub fn empty_table()     -> T<'a> { T::Tables(Cow::Owned(Tables::Empty)) }
    pub fn function()        -> T<'a> { T::Functions(Cow::Owned(Functions::All)) }
    pub fn func(f: Function) -> T<'a> { T::Functions(Cow::Owned(Functions::Simple(f))) }

    pub fn ints<I: IntoIterator<Item=i32>>(i: I) -> T<'a> {
        T::Numbers(Cow::Owned(Numbers::Some(i.into_iter().collect())))
    }
    pub fn strs<I: IntoIterator<Item=Str>>(i: I) -> T<'a> {
        T::Strings(Cow::Owned(Strings::Some(i.into_iter().collect())))
    }
    pub fn tuple<'b, I: IntoIterator<Item=Slot>>(i: I) -> T<'a> {
        let i = i.into_iter().enumerate();
        let fields = i.map(|(i,v)| ((i as i32 + 1).into(), v));
        T::Tables(Cow::Owned(Tables::Fields(fields.collect())))
    }
    pub fn record<'b, I: IntoIterator<Item=(Str,Slot)>>(i: I) -> T<'a> {
        let i = i.into_iter();
        let fields = i.map(|(k,v)| (k.into(), v));
        T::Tables(Cow::Owned(Tables::Fields(fields.collect())))
    }
    pub fn array(v: Slot) -> T<'a> {
        T::Tables(Cow::Owned(Tables::Array(SlotWithNil::from_slot(v))))
    }
    pub fn map(k: T, v: Slot) -> T<'a> {
        T::Tables(Cow::Owned(Tables::Map(Box::new(k.into_send()), SlotWithNil::from_slot(v))))
    }

    pub fn from(kind: &K, resolv: &mut TypeResolver) -> CheckResult<T<'a>> {
        let slot_from_slotkind = |slotkind: &SlotKind,
                                  resolv: &mut TypeResolver| -> CheckResult<Slot> {
            let ty = try!(T::from(&slotkind.kind.base, resolv));
            let flex = match slotkind.modf {
                M::None => F::Just, // XXX
                M::Var => F::Var,
                M::Const => F::Const,
            };
            Ok(Slot::new(flex, ty))
        };

        match *kind {
            K::Dynamic           => Ok(T::Dynamic),
            K::Any               => Ok(T::All),
            K::Nil               => Ok(T::Nil),
            K::Boolean           => Ok(T::Boolean),
            K::BooleanLit(true)  => Ok(T::True),
            K::BooleanLit(false) => Ok(T::False),
            K::Number            => Ok(T::Numbers(Cow::Owned(Numbers::All))),
            K::Integer           => Ok(T::Numbers(Cow::Owned(Numbers::Int))),
            K::IntegerLit(v)     => Ok(T::Numbers(Cow::Owned(Numbers::One(v)))),
            K::String            => Ok(T::Strings(Cow::Owned(Strings::All))),
            K::StringLit(ref s)  => Ok(T::Strings(Cow::Owned(Strings::One(s.to_owned())))),
            K::Table             => Ok(T::Tables(Cow::Owned(Tables::All))),
            K::EmptyTable        => Ok(T::Tables(Cow::Owned(Tables::Empty))),
            K::Function          => Ok(T::Functions(Cow::Owned(Functions::All))),
            K::Thread            => Ok(T::Thread),
            K::UserData          => Ok(T::UserData),
            K::Named(ref name)   => resolv.ty_from_name(name),
            K::Error(..)         => Err(format!("error type not yet supported in checker")),

            K::Record(ref fields) => {
                let mut newfields = BTreeMap::new();
                for &(ref name, ref slotkind) in fields {
                    let slot = try!(slot_from_slotkind(&slotkind.base, resolv));
                    newfields.insert(name.base.clone().into(), slot);
                }
                Ok(T::Tables(Cow::Owned(Tables::Fields(newfields))))
            }

            K::Tuple(ref fields) => {
                let mut newfields = BTreeMap::new();
                for (i, slotkind) in fields.iter().enumerate() {
                    let key = Key::Int(i as i32 + 1);
                    let slot = try!(slot_from_slotkind(slotkind, resolv));
                    newfields.insert(key, slot);
                }
                Ok(T::Tables(Cow::Owned(Tables::Fields(newfields))))
            },

            K::Array(ref v) => {
                let slot = SlotWithNil::from_slot(try!(slot_from_slotkind(v, resolv)));
                Ok(T::Tables(Cow::Owned(Tables::Array(slot))))
            },

            K::Map(ref k, ref v) => {
                let slot = SlotWithNil::from_slot(try!(slot_from_slotkind(v, resolv)));
                Ok(T::Tables(Cow::Owned(Tables::Map(Box::new(try!(T::from(k, resolv))), slot))))
            },

            K::Func(ref funcs) => {
                let mut ftys = Vec::new();
                for func in funcs {
                    ftys.push(Function {
                        args: try!(TySeq::from_kind_seq(&func.args, resolv)),
                        returns: try!(TySeq::from_kind_seq(&func.returns, resolv)),
                    });
                }
                if ftys.len() == 1 {
                    Ok(T::Functions(Cow::Owned(Functions::Simple(ftys.pop().unwrap()))))
                } else {
                    Ok(T::Functions(Cow::Owned(Functions::Multi(ftys))))
                }
            }

            K::Union(ref kinds) => {
                assert!(!kinds.is_empty());
                let mut ty = try!(T::from(&kinds[0], resolv));
                for kind in &kinds[1..] {
                    ty = ty | try!(T::from(kind, resolv));
                }
                Ok(ty)
            }
        }
    }

    pub fn flags(&self) -> Flags {
        match *self {
            T::Dynamic  => T_ALL | T_DYNAMIC,
            T::All      => T_ALL,
            T::None     => T_NONE,
            T::Nil      => T_NIL,
            T::Boolean  => T_BOOLEAN,
            T::True     => T_TRUE,
            T::False    => T_FALSE,
            T::Thread   => T_THREAD,
            T::UserData => T_USERDATA,

            T::Numbers(ref num) => match &**num {
                &Numbers::One(..) | &Numbers::Some(..) | &Numbers::Int => T_INTEGER,
                &Numbers::All => T_NUMBER,
            },
            T::Strings(..) => T_STRING,
            T::Tables(..) => T_TABLE,
            T::Functions(..) => T_FUNCTION,

            T::TVar(..) => T_NONE,
            T::Builtin(_, ref t) => t.flags(),
            T::Union(ref u) => u.flags(),
        }
    }

    pub fn to_ref<'b: 'a>(&'b self) -> T<'b> {
        match *self {
            T::Dynamic  => T::Dynamic,
            T::All      => T::All,
            T::None     => T::None,
            T::Nil      => T::Nil,
            T::Boolean  => T::Boolean,
            T::True     => T::True,
            T::False    => T::False,
            T::Thread   => T::Thread,
            T::UserData => T::UserData,

            T::Numbers(ref num) => T::Numbers(Cow::Borrowed(&**num)),
            T::Strings(ref str) => T::Strings(Cow::Borrowed(&**str)),
            T::Tables(ref tab) => T::Tables(Cow::Borrowed(&**tab)),
            T::Functions(ref func) => T::Functions(Cow::Borrowed(&**func)),
            T::TVar(v) => T::TVar(v),
            T::Builtin(b, ref t) => T::Builtin(b, Box::new(t.to_ref())),
            T::Union(ref u) => T::Union(Cow::Borrowed(&**u)),
        }
    }

    // used for value slots in array and mapping types
    pub fn to_ref_without_nil<'b: 'a>(&'b self) -> T<'b> {
        match *self {
            T::Nil => T::None,
            T::Union(ref u) if u.simple.contains(U_NIL) => {
                let mut u = u.clone().into_owned();
                u.simple.remove(U_NIL);
                u.simplify()
            },
            _ => self.to_ref(),
        }
    }

    // used for value slots in array and mapping types
    pub fn without_nil(self) -> T<'a> {
        match self {
            T::Nil => T::None,
            T::Union(u) => {
                if u.simple.contains(U_NIL) {
                    let mut u = u.into_owned();
                    u.simple.remove(U_NIL);
                    u.simplify()
                } else {
                    T::Union(u)
                }
            },
            t => t,
        }
    }

    pub fn is_dynamic(&self)  -> bool { self.flags().is_dynamic() }
    pub fn is_integral(&self) -> bool { self.flags().is_integral() }
    pub fn is_numeric(&self)  -> bool { self.flags().is_numeric() }
    pub fn is_stringy(&self)  -> bool { self.flags().is_stringy() }
    pub fn is_tabular(&self)  -> bool { self.flags().is_tabular() }
    pub fn is_callable(&self) -> bool { self.flags().is_callable() }
    pub fn is_truthy(&self)   -> bool { self.flags().is_truthy() }
    pub fn is_falsy(&self)    -> bool { self.flags().is_falsy() }

    // XXX for now
    pub fn is_referential(&self) -> bool { self.flags().is_tabular() }

    pub fn get_numbers(&self) -> Option<&Numbers> {
        match *self {
            T::Numbers(ref num) => Some(num),
            T::Builtin(_, ref t) => t.get_numbers(),
            T::Union(ref u) => u.numbers.as_ref(),
            _ => None,
        }
    }

    pub fn get_strings(&self) -> Option<&Strings> {
        match *self {
            T::Strings(ref str) => Some(str),
            T::Builtin(_, ref t) => t.get_strings(),
            T::Union(ref u) => u.strings.as_ref(),
            _ => None,
        }
    }

    pub fn get_tables(&self) -> Option<&Tables> {
        match *self {
            T::Tables(ref tab) => Some(tab),
            T::Builtin(_, ref t) => t.get_tables(),
            T::Union(ref u) => u.tables.as_ref(),
            _ => None,
        }
    }

    pub fn get_functions(&self) -> Option<&Functions> {
        match *self {
            T::Functions(ref func) => Some(func),
            T::Builtin(_, ref t) => t.get_functions(),
            T::Union(ref u) => u.functions.as_ref(),
            _ => None,
        }
    }

    pub fn get_tvar(&self) -> Option<TVar> {
        match *self {
            T::TVar(tv) => Some(tv),
            T::Builtin(_, ref t) => t.get_tvar(),
            T::Union(ref u) => u.tvar,
            _ => None,
        }
    }

    pub fn split_tvar(&self) -> (Option<TVar>, Option<T<'a>>) {
        match *self {
            T::TVar(tv) => (Some(tv), None),
            T::Union(ref u) => {
                if let Some(tv) = u.tvar {
                    let mut u = u.clone().into_owned();
                    u.tvar = None;
                    (Some(tv), Some(u.simplify()))
                } else {
                    (None, Some(T::Union(u.clone())))
                }
            },
            _ => (None, Some(self.clone())),
        }
    }

    pub fn builtin(&self) -> Option<Builtin> {
        match *self { T::Builtin(b, _) => Some(b), _ => None }
    }

    pub fn as_base(&self) -> &T<'a> {
        match self { &T::Builtin(_, ref t) => &*t, t => t }
    }

    pub fn into_base(self) -> T<'a> {
        match self { T::Builtin(_, t) => *t, t => t }
    }

    pub fn as_string(&self) -> Option<&Str> {
        // unlike flags, type variable should not be present
        let strings = match *self {
            T::Strings(ref str) => str.as_ref(),
            T::Builtin(_, ref t) => return t.as_string(),
            T::Union(ref u) if u.flags() == T_STRING && u.tvar.is_none() =>
                u.strings.as_ref().unwrap(),
            _ => return None,
        };
        match *strings {
            Strings::One(ref s) => Some(s),
            Strings::Some(ref set) if set.len() == 1 => Some(set.iter().next().unwrap()),
            _ => None,
        }
    }

    pub fn as_integer(&self) -> Option<i32> {
        // unlike flags, type variable should not be present
        let numbers = match *self {
            T::Numbers(ref num) => num.as_ref(),
            T::Builtin(_, ref t) => return t.as_integer(),
            T::Union(ref u) if u.flags() == T_INTEGER && u.tvar.is_none() =>
                u.numbers.as_ref().unwrap(),
            _ => return None,
        };
        match *numbers {
            Numbers::One(v) => Some(v),
            Numbers::Some(ref set) if set.len() == 1 => Some(*set.iter().next().unwrap()),
            _ => None,
        }
    }

    pub fn into_send(self) -> T<'static> {
        match self {
            T::Dynamic    => T::Dynamic,
            T::All        => T::All,
            T::None       => T::None,
            T::Nil        => T::Nil,
            T::Boolean    => T::Boolean,
            T::True       => T::True,
            T::False      => T::False,
            T::Thread     => T::Thread,
            T::UserData   => T::UserData,

            T::Numbers(num)    => T::Numbers(Cow::Owned(num.into_owned())),
            T::Strings(str)    => T::Strings(Cow::Owned(str.into_owned())),
            T::Tables(tab)     => T::Tables(Cow::Owned(tab.into_owned())),
            T::Functions(func) => T::Functions(Cow::Owned(func.into_owned())),
            T::TVar(tv)        => T::TVar(tv),

            T::Builtin(b, t) => T::Builtin(b, Box::new(t.into_send())),
            T::Union(u) => T::Union(Cow::Owned(u.into_owned())),
        }
    }

    pub fn filter_by_flags(self, flags: Flags, ctx: &mut TypeContext) -> CheckResult<T<'a>> {
        fn flags_to_ubound(flags: Flags) -> T<'static> {
            if flags.contains(T_DYNAMIC) {
                T::Dynamic
            } else {
                let mut t = T::None;
                if flags.contains(T_NIL)        { t = t | T::Nil; }
                if flags.contains(T_TRUE)       { t = t | T::True; }
                if flags.contains(T_FALSE)      { t = t | T::False; }
                if flags.contains(T_NONINTEGER) { t = t | T::number(); }
                if flags.contains(T_INTEGER)    { t = t | T::integer(); }
                if flags.contains(T_STRING)     { t = t | T::string(); }
                if flags.contains(T_TABLE)      { t = t | T::table(); }
                if flags.contains(T_FUNCTION)   { t = t | T::function(); }
                if flags.contains(T_THREAD)     { t = t | T::Thread; }
                if flags.contains(T_USERDATA)   { t = t | T::UserData; }
                t
            }
        }

        fn narrow_numbers<'a>(num: Cow<'a, Numbers>, flags: Flags) -> Option<Cow<'a, Numbers>> {
            let is_all = match num.as_ref() { &Numbers::All => true, _ => false };
            match (flags & T_NUMBER, is_all) {
                (T_NONINTEGER, false) => None,
                (T_INTEGER, true) => Some(Cow::Owned(Numbers::Int)),
                (T_NONE, _) => None,
                (_, _) => Some(num),
            }
        }

        fn narrow_tvar(tvar: TVar, flags: Flags, ctx: &mut TypeContext) -> CheckResult<TVar> {
            let ubound = flags_to_ubound(flags);

            // make a type variable i such that i <: ubound and i <: tvar
            let i = ctx.gen_tvar();
            try!(ctx.assert_tvar_sub_tvar(tvar, i));
            try!(ctx.assert_tvar_sub(tvar, &ubound));

            Ok(i)
        }

        let flags_or_none = |bit, t| if flags.contains(bit) { t } else { T::None };

        match self {
            T::Dynamic => Ok(T::Dynamic),
            T::None => Ok(T::None),
            T::All => Ok(flags_to_ubound(flags)),
            T::Boolean => match flags & T_BOOLEAN {
                T_BOOLEAN => Ok(T::Boolean),
                T_TRUE => Ok(T::True),
                T_FALSE => Ok(T::False),
                _ => Ok(T::None),
            },
            T::Numbers(num) => {
                if let Some(num) = narrow_numbers(num, flags) {
                    Ok(T::Numbers(num))
                } else {
                    Ok(T::None)
                }
            },

            T::Nil             => Ok(flags_or_none(T_NIL,      T::Nil)),
            T::True            => Ok(flags_or_none(T_TRUE,     T::True)),
            T::False           => Ok(flags_or_none(T_FALSE,    T::False)),
            T::Thread          => Ok(flags_or_none(T_THREAD,   T::Thread)),
            T::UserData        => Ok(flags_or_none(T_USERDATA, T::UserData)),
            T::Strings(str)    => Ok(flags_or_none(T_STRING,   T::Strings(str))),
            T::Tables(tab)     => Ok(flags_or_none(T_TABLE,    T::Tables(tab))),
            T::Functions(func) => Ok(flags_or_none(T_FUNCTION, T::Functions(func))),

            T::TVar(tv) => {
                Ok(T::TVar(try!(narrow_tvar(tv, flags, ctx))))
            },
            T::Builtin(b, t) => {
                Ok(T::Builtin(b, Box::new(try!((*t).filter_by_flags(flags, ctx)))))
            },
            T::Union(u) => {
                // compile a list of flags to remove, and only alter if there is any removal
                let removed = !flags & u.flags();
                if removed.is_empty() { return Ok(T::Union(u)); }

                let mut u = u.into_owned();
                let removed_simple = SimpleUnion::from_bits_truncate(removed.bits());
                if !removed_simple.is_empty() { u.simple &= !removed_simple; }
                if removed.intersects(T_NUMBER) {
                    let num = Cow::Owned(u.numbers.unwrap());
                    u.numbers = narrow_numbers(num, flags).map(|num| num.into_owned());
                }
                if removed.contains(T_STRING)   { u.strings   = None; }
                if removed.contains(T_TABLE)    { u.tables    = None; }
                if removed.contains(T_FUNCTION) { u.functions = None; }
                Ok(u.simplify())
            },
        }
    }
}

impl<'a, 'b> Lattice<T<'b>> for T<'a> {
    type Output = T<'static>;

    fn do_union(&self, other: &T<'b>, ctx: &mut TypeContext) -> T<'static> {
        match (self, other) {
            // built-in types are destructured first unless they point to the same builtin
            (&T::Builtin(lb, ref lhs), &T::Builtin(rb, ref rhs)) if lb == rb =>
                T::Builtin(lb, Box::new(lhs.union(rhs, ctx))),
            (&T::Builtin(_, ref lhs), &T::Builtin(_, ref rhs)) => lhs.union(rhs, ctx),
            (&T::Builtin(_, ref lhs), rhs) => lhs.union(rhs, ctx),
            (lhs, &T::Builtin(_, ref rhs)) => lhs.union(rhs, ctx),

            // dynamic eclipses everything else
            (&T::Dynamic, _) => T::Dynamic,
            (_, &T::Dynamic) => T::Dynamic,

            // top eclipses everything else except for dynamic
            (&T::All, _) => T::All,
            (_, &T::All) => T::All,

            (&T::None, ty) => ty.clone().into_send(),
            (ty, &T::None) => ty.clone().into_send(),

            (&T::Nil,      &T::Nil)      => T::Nil,
            (&T::Boolean,  &T::Boolean)  => T::Boolean,
            (&T::Boolean,  &T::True)     => T::Boolean,
            (&T::Boolean,  &T::False)    => T::Boolean,
            (&T::True,     &T::Boolean)  => T::Boolean,
            (&T::False,    &T::Boolean)  => T::Boolean,
            (&T::True,     &T::True)     => T::True,
            (&T::True,     &T::False)    => T::Boolean,
            (&T::False,    &T::True)     => T::Boolean,
            (&T::False,    &T::False)    => T::False,
            (&T::Thread,   &T::Thread)   => T::Thread,
            (&T::UserData, &T::UserData) => T::UserData,

            (&T::Numbers(ref a), &T::Numbers(ref b)) =>
                T::Numbers(Cow::Owned(a.union(b, ctx))),
            (&T::Strings(ref a), &T::Strings(ref b)) =>
                T::Strings(Cow::Owned(a.union(b, ctx))),
            (&T::Tables(ref a), &T::Tables(ref b)) =>
                T::Tables(Cow::Owned(a.union(b, ctx))),
            (&T::Functions(ref a), &T::Functions(ref b)) =>
                T::Functions(Cow::Owned(a.union(b, ctx))),
            (&T::TVar(ref a), &T::TVar(ref b)) =>
                T::TVar(a.union(b, ctx)),

            (a, b) => Union::from(&a).union(&Union::from(&b), ctx).simplify(),
        }
    }

    fn do_assert_sub(&self, other: &T<'b>, ctx: &mut TypeContext) -> CheckResult<()> {
        debug!("asserting a constraint {:?} <: {:?}", *self, *other);

        let ok = match (self, other) {
            // built-in types are destructured first
            (&T::Builtin(_, ref lhs), &T::Builtin(_, ref rhs)) => return lhs.assert_sub(rhs, ctx),
            (&T::Builtin(_, ref lhs), rhs) => return lhs.assert_sub(rhs, ctx),
            (lhs, &T::Builtin(_, ref rhs)) => return lhs.assert_sub(rhs, ctx),

            (&T::Dynamic, _) => true,
            (_, &T::Dynamic) => true,

            (_, &T::All) => true,

            (&T::None, _) => true,
            (_, &T::None) => false,

            (&T::Nil,      &T::Nil)      => true,
            (&T::Boolean,  &T::Boolean)  => true,
            (&T::True,     &T::Boolean)  => true,
            (&T::True,     &T::True)     => true,
            (&T::False,    &T::Boolean)  => true,
            (&T::False,    &T::False)    => true,
            (&T::Thread,   &T::Thread)   => true,
            (&T::UserData, &T::UserData) => true,

            (&T::Numbers(ref a),   &T::Numbers(ref b))   => return a.assert_sub(b, ctx),
            (&T::Strings(ref a),   &T::Strings(ref b))   => return a.assert_sub(b, ctx),
            (&T::Tables(ref a),    &T::Tables(ref b))    => return a.assert_sub(b, ctx),
            (&T::Functions(ref a), &T::Functions(ref b)) => return a.assert_sub(b, ctx),

            (&T::Union(ref a), &T::Union(ref b)) => return a.assert_sub(b, ctx),
            (&T::Union(ref a), &T::TVar(b)) if a.tvar.is_none() => {
                // do NOT try to split `T|U <: x` into `T <: x AND U <: x` if possible
                return ctx.assert_tvar_sup(b, self);
            },
            (&T::Union(ref a), b) => {
                // a1 \/ a2 <: b === a1 <: b AND a2 <: b
                return a.visit(|i| i.assert_sub(b, ctx));
            },

            // a <: b1 \/ b2 === a <: b1 OR a <: b2
            (&T::Nil,      &T::Union(ref b)) => b.simple.contains(U_NIL),
            (&T::Boolean,  &T::Union(ref b)) => b.simple.contains(U_BOOLEAN),
            (&T::True,     &T::Union(ref b)) => b.simple.contains(U_TRUE),
            (&T::False,    &T::Union(ref b)) => b.simple.contains(U_FALSE),
            (&T::Thread,   &T::Union(ref b)) => b.simple.contains(U_THREAD),
            (&T::UserData, &T::Union(ref b)) => b.simple.contains(U_USERDATA),

            (&T::Numbers(ref a), &T::Union(ref b)) => {
                if let Some(ref num) = b.numbers { return a.assert_sub(num, ctx); }
                false
            },
            (&T::Strings(ref a), &T::Union(ref b)) => {
                if let Some(ref str) = b.strings { return a.assert_sub(str, ctx); }
                false
            },
            (&T::Tables(ref a), &T::Union(ref b)) => {
                if let Some(ref tab) = b.tables { return a.assert_sub(tab, ctx); }
                false
            },
            (&T::Functions(ref a), &T::Union(ref b)) => {
                if let Some(ref func) = b.functions { return a.assert_sub(func, ctx); }
                false
            },
            // XXX a <: T \/ b === a <: T OR a <: b
            (&T::TVar(_a), &T::Union(ref b)) if b.tvar.is_some() => false,

            (&T::TVar(a), &T::TVar(b)) => return a.assert_sub(&b, ctx),
            (a, &T::TVar(b)) => return ctx.assert_tvar_sup(b, a),
            (&T::TVar(a), b) => return ctx.assert_tvar_sub(a, b),

            (_, _) => false,
        };

        if ok { Ok(()) } else { error_not_sub(self, other) }
    }

    fn do_assert_eq(&self, other: &T<'b>, ctx: &mut TypeContext) -> CheckResult<()> {
        debug!("asserting a constraint {:?} = {:?}", *self, *other);

        let ok = match (self, other) {
            // built-in types are destructured first
            (&T::Builtin(_, ref lhs), &T::Builtin(_, ref rhs)) => return lhs.assert_eq(rhs, ctx),
            (&T::Builtin(_, ref lhs), rhs) => return lhs.assert_eq(rhs, ctx),
            (lhs, &T::Builtin(_, ref rhs)) => return lhs.assert_eq(rhs, ctx),

            (&T::Dynamic, _) => true,
            (_, &T::Dynamic) => true,

            (&T::All, _) => true,
            (_, &T::All) => true,

            (&T::None, _) => true,
            (_, &T::None) => false,

            (&T::Nil,      &T::Nil)      => true,
            (&T::Boolean,  &T::Boolean)  => true,
            (&T::True,     &T::True)     => true,
            (&T::False,    &T::False)    => true,
            (&T::Thread,   &T::Thread)   => true,
            (&T::UserData, &T::UserData) => true,

            (&T::Numbers(ref a),   &T::Numbers(ref b))   => return a.assert_eq(b, ctx),
            (&T::Strings(ref a),   &T::Strings(ref b))   => return a.assert_eq(b, ctx),
            (&T::Tables(ref a),    &T::Tables(ref b))    => return a.assert_eq(b, ctx),
            (&T::Functions(ref a), &T::Functions(ref b)) => return a.assert_eq(b, ctx),

            (&T::TVar(a), &T::TVar(b)) => return a.assert_eq(&b, ctx),
            (a, &T::TVar(b)) => return ctx.assert_tvar_eq(b, a),
            (&T::TVar(a), b) => return ctx.assert_tvar_eq(a, b),

            (&T::Union(ref a), &T::Union(ref b)) => return a.assert_eq(b, ctx),
            (&T::Union(ref _a), _b) => unimplemented!(), // XXX for now
            (_a, &T::Union(ref _b)) => unimplemented!(), // XXX for now

            (_, _) => false,
        };

        if ok { Ok(()) } else { error_not_eq(self, other) }
    }
}

impl<'a, 'b> ops::BitOr<T<'b>> for T<'a> {
    type Output = T<'static>;
    fn bitor(self, rhs: T<'b>) -> T<'static> {
        self.union(&rhs, &mut NoTypeContext)
    }
}

// not intended to be complete equality, but enough for testing
impl<'a, 'b> PartialEq<T<'b>> for T<'a> {
    fn eq(&self, other: &T<'b>) -> bool {
        match (self, other) {
            (&T::Dynamic,  &T::Dynamic)  => true,
            (&T::All,      &T::All)      => true,
            (&T::None,     &T::None)     => true,
            (&T::Nil,      &T::Nil)      => true,
            (&T::Boolean,  &T::Boolean)  => true,
            (&T::True,     &T::True)     => true,
            (&T::False,    &T::False)    => true,
            (&T::Thread,   &T::Thread)   => true,
            (&T::UserData, &T::UserData) => true,

            (&T::Numbers(ref a),   &T::Numbers(ref b))   => *a == *b,
            (&T::Strings(ref a),   &T::Strings(ref b))   => *a == *b,
            (&T::Tables(ref a),    &T::Tables(ref b))    => *a == *b,
            (&T::Functions(ref a), &T::Functions(ref b)) => *a == *b,
            (&T::TVar(a),          &T::TVar(b))          => a == b,
            (&T::Builtin(ba, _),   &T::Builtin(bb, _))   => ba == bb, // XXX lifetime issues?
            (&T::Union(ref a),     &T::Union(ref b))     => a == b,

            (_, _) => false,
        }
    }
}

impl<'a> fmt::Debug for T<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            T::Dynamic  => write!(f, "WHATEVER"),
            T::All      => write!(f, "any"),
            T::None     => write!(f, "<bottom>"),
            T::Nil      => write!(f, "nil"),
            T::Boolean  => write!(f, "boolean"),
            T::True     => write!(f, "true"),
            T::False    => write!(f, "false"),
            T::Thread   => write!(f, "thread"),
            T::UserData => write!(f, "userdata"),

            T::Numbers(ref num)    => fmt::Debug::fmt(num, f),
            T::Strings(ref str)    => fmt::Debug::fmt(str, f),
            T::Tables(ref tab)     => fmt::Debug::fmt(tab, f),
            T::Functions(ref func) => fmt::Debug::fmt(func, f),
            T::TVar(tv)            => write!(f, "<#{}>", tv.0),
            T::Builtin(b, ref t)   => write!(f, "{:?} (= {})", *t, b.name()),
            T::Union(ref u)        => fmt::Debug::fmt(u, f),
        }
    }
}

impl<'a> From<T<'a>> for Union {
    fn from(x: T<'a>) -> Union { Union::from(&x) }
}

pub type Ty = Box<T<'static>>;

#[cfg(test)] 
#[allow(unused_variables, dead_code)]
mod tests {
    use kailua_diag::NoReport;
    use kailua_syntax::Str;
    use std::rc::Rc;
    use ty::{Lattice, TypeContext, NoTypeContext, F, Slot, Mark};
    use env::Context;
    use super::*;

    macro_rules! hash {
        ($($k:ident = $v:expr),*) => (vec![$((s(stringify!($k)), $v)),*])
    }

    fn s(x: &str) -> Str { Str::from(x.as_bytes().to_owned()) }
    fn just(t: T) -> Slot { Slot::new(F::Just, t.into_send()) }
    fn var(t: T) -> Slot { Slot::new(F::Var, t.into_send()) }
    fn cnst(t: T) -> Slot { Slot::new(F::Const, t.into_send()) }
    fn curr(t: T) -> Slot { Slot::new(F::Currently, t.into_send()) }
    fn varcnst(t: T) -> Slot { Slot::new(F::VarOrConst(Mark::any()), t.into_send()) }
    fn varcurr(t: T) -> Slot { Slot::new(F::VarOrCurrently(Mark::any()), t.into_send()) }

    #[test]
    fn test_lattice() {
        macro_rules! check {
            ($l:expr, $r:expr; $u:expr) => ({
                let left = $l;
                let right = $r;
                let union = $u;
                let mut ctx = Context::new(Rc::new(NoReport));
                let actualunion = left.union(&right, &mut ctx);
                if actualunion != union {
                    panic!("{:?} | {:?} = expected {:?}, actual {:?}",
                           left, right, union, actualunion);
                }
                (left, right, actualunion)
            })
        }

        // dynamic & top vs. everything else
        check!(T::Dynamic, T::Dynamic; T::Dynamic);
        check!(T::Dynamic, T::integer(); T::Dynamic);
        check!(T::tuple(vec![var(T::integer()), curr(T::Boolean)]), T::Dynamic; T::Dynamic);
        check!(T::All, T::Boolean; T::All);
        check!(T::Dynamic, T::All; T::Dynamic);
        check!(T::All, T::All; T::All);

        // integer literals
        check!(T::integer(), T::number(); T::number());
        check!(T::number(), T::integer(); T::number());
        check!(T::number(), T::number(); T::number());
        check!(T::integer(), T::integer(); T::integer());
        check!(T::int(3), T::int(3); T::int(3));
        check!(T::int(3), T::number(); T::number());
        check!(T::integer(), T::int(3); T::integer());
        check!(T::int(3), T::int(4); T::ints(vec![3, 4]));
        check!(T::ints(vec![3, 4]), T::int(3); T::ints(vec![3, 4]));
        check!(T::int(5), T::ints(vec![3, 4]); T::ints(vec![3, 4, 5]));
        check!(T::ints(vec![3, 4]), T::ints(vec![5, 4, 7]); T::ints(vec![3, 4, 5, 7]));
        check!(T::ints(vec![3, 4, 5]), T::ints(vec![2, 3, 4]); T::ints(vec![2, 3, 4, 5]));

        // string literals
        check!(T::string(), T::str(s("hello")); T::string());
        check!(T::str(s("hello")), T::string(); T::string());
        check!(T::str(s("hello")), T::str(s("hello")); T::str(s("hello")));
        check!(T::str(s("hello")), T::str(s("goodbye"));
               T::strs(vec![s("hello"), s("goodbye")]));
        check!(T::str(s("hello")), T::strs(vec![s("goodbye")]);
               T::strs(vec![s("hello"), s("goodbye")]));
        check!(T::strs(vec![s("hello"), s("goodbye")]), T::str(s("goodbye"));
               T::strs(vec![s("hello"), s("goodbye")]));
        check!(T::strs(vec![s("hello"), s("goodbye")]),
               T::strs(vec![s("what"), s("goodbye")]);
               T::strs(vec![s("hello"), s("goodbye"), s("what")]));
        check!(T::strs(vec![s("a"), s("b"), s("c")]),
               T::strs(vec![s("b"), s("c"), s("d")]);
               T::strs(vec![s("a"), s("b"), s("c"), s("d")]));

        // tables
        check!(T::table(), T::array(just(T::integer())); T::table());
        check!(T::table(), T::array(var(T::integer())); T::table());
        check!(T::table(), T::array(curr(T::integer())); T::table());
        check!(T::array(just(T::integer())), T::array(just(T::integer()));
               T::array(just(T::integer())));
        check!(T::array(var(T::integer())), T::array(var(T::integer()));
               T::array(varcnst(T::integer())));
        check!(T::array(cnst(T::integer())), T::array(cnst(T::integer()));
               T::array(cnst(T::integer())));
        check!(T::array(just(T::int(3))), T::array(just(T::int(4)));
               T::array(just(T::ints(vec![3, 4]))));
        check!(T::array(cnst(T::int(3))), T::array(cnst(T::int(4)));
               T::array(cnst(T::ints(vec![3, 4]))));
        check!(T::array(var(T::int(3))), T::array(var(T::int(4)));
               T::array(varcnst(T::ints(vec![3, 4]))));
        check!(T::array(var(T::int(3))), T::array(just(T::int(4)));
               T::array(varcnst(T::ints(vec![3, 4]))));
        check!(T::tuple(vec![just(T::integer()), just(T::string())]),
               T::tuple(vec![just(T::number()), just(T::Dynamic), just(T::Boolean)]);
               T::tuple(vec![just(T::number()), just(T::Dynamic), just(T::Boolean | T::Nil)]));
        check!(T::tuple(vec![just(T::integer()), just(T::string())]),
               T::tuple(vec![just(T::number()), just(T::Boolean), just(T::Dynamic)]);
               T::tuple(vec![just(T::number()), just(T::string() | T::Boolean),
                             just(T::Dynamic)]));
        { // self-modifying unions
            let (lhs, rhs, _) = check!(
                T::tuple(vec![var(T::integer()), curr(T::string())]),
                T::tuple(vec![cnst(T::string()), just(T::number()), var(T::Boolean)]);
                T::tuple(vec![cnst(T::integer() | T::string()),
                              varcnst(T::string() | T::number()),
                              varcnst(T::Boolean | T::Nil)]));
            assert_eq!(lhs, T::tuple(vec![var(T::integer()), var(T::string())]));
            assert_eq!(rhs, T::tuple(vec![cnst(T::string()), just(T::number()), var(T::Boolean)]));

            let (lhs, rhs, _) = check!(
                T::tuple(vec![cnst(T::integer())]),
                T::tuple(vec![cnst(T::number()), curr(T::string())]);
                T::tuple(vec![cnst(T::number()), varcnst(T::string() | T::Nil)]));
            assert_eq!(lhs, T::tuple(vec![cnst(T::integer())]));
            assert_eq!(rhs, T::tuple(vec![cnst(T::number()), var(T::string())]));

            let (lhs, _, _) = check!(
                T::tuple(vec![just(T::integer()), var(T::string()), curr(T::Boolean)]),
                T::empty_table();
                T::tuple(vec![just(T::integer() | T::Nil), varcnst(T::string() | T::Nil),
                              varcnst(T::Boolean | T::Nil)]));
            assert_eq!(lhs, T::tuple(vec![just(T::integer()), var(T::string()), var(T::Boolean)]));
        }
        check!(T::record(hash![foo=just(T::integer()), bar=just(T::string())]),
               T::record(hash![quux=just(T::Boolean)]);
               T::record(hash![foo=just(T::integer() | T::Nil), bar=just(T::string() | T::Nil),
                               quux=just(T::Boolean | T::Nil)]));
        check!(T::record(hash![foo=just(T::int(3)), bar=just(T::string())]),
               T::record(hash![foo=just(T::int(4))]);
               T::record(hash![foo=just(T::ints(vec![3, 4])), bar=just(T::string() | T::Nil)]));
        check!(T::record(hash![foo=just(T::integer()), bar=just(T::number()),
                                    quux=just(T::array(just(T::Dynamic)))]),
               T::record(hash![foo=just(T::number()), bar=just(T::string()),
                                    quux=just(T::array(just(T::Boolean)))]);
               T::record(hash![foo=just(T::number()), bar=just(T::number() | T::string()),
                                    quux=just(T::array(just(T::Dynamic)))]));
        check!(T::record(hash![foo=just(T::int(3)), bar=just(T::number())]),
               T::map(T::string(), just(T::integer()));
               T::map(T::string(), just(T::number())));
        check!(T::array(just(T::integer())), T::tuple(vec![just(T::string())]);
               T::map(T::integer(), just(T::integer() | T::string())));
        check!(T::map(T::str(s("wat")), just(T::integer())),
               T::map(T::string(), just(T::int(42)));
               T::map(T::string(), just(T::integer())));
        check!(T::array(just(T::number())), T::map(T::Dynamic, just(T::integer()));
               T::map(T::Dynamic, just(T::number())));
        check!(T::empty_table(), T::array(just(T::integer()));
               T::array(just(T::integer())));

        // others
        check!(T::Thread, T::Thread; T::Thread);
        check!(T::UserData, T::UserData; T::UserData);
        check!(T::All, T::UserData; T::All);
        check!(T::Thread, T::Dynamic; T::Dynamic);

        // general unions
        check!(T::True, T::False; T::Boolean);
        check!(T::int(3) | T::Nil, T::int(4) | T::Nil;
               T::ints(vec![3, 4]) | T::Nil);
        check!(T::int(3) | T::UserData | T::Nil, T::Nil | T::Thread | T::int(4);
               T::Thread | T::ints(vec![3, 4]) | T::UserData | T::Nil);
        check!(T::ints(vec![3, 5]) | T::Nil, T::int(4) | T::string();
               T::string() | T::ints(vec![3, 4, 5]) | T::Nil);
        check!(T::int(3) | T::string(), T::str(s("wat")) | T::int(4);
               T::ints(vec![3, 4]) | T::string());
        assert_eq!(T::map(T::string(), just(T::integer())),
                   T::map(T::string(), just(T::integer() | T::Nil)));
    }

    #[test]
    fn test_sub() {
        assert_eq!(T::record(hash![foo=just(T::int(3)), bar=just(T::integer())]).assert_sub(
                       &T::map(T::str(s("foo")) | T::str(s("bar")), just(T::number())),
                       &mut NoTypeContext),
                   Ok(()));

        let mut ctx = Context::new(Rc::new(NoReport));

        {
            let v1 = ctx.gen_tvar();
            // v1 <: integer
            assert_eq!(T::TVar(v1).assert_sub(&T::integer(), &mut ctx), Ok(()));
            // v1 <: integer
            assert_eq!(T::TVar(v1).assert_sub(&T::integer(), &mut ctx), Ok(()));
            // v1 <: integer AND v1 <: string (!)
            assert!(T::TVar(v1).assert_sub(&T::string(), &mut ctx).is_err());
        }

        {
            let v1 = ctx.gen_tvar();
            let v2 = ctx.gen_tvar();
            // v1 <: v2
            assert_eq!(T::TVar(v1).assert_sub(&T::TVar(v2), &mut ctx), Ok(()));
            // v1 <: v2 <: string
            assert_eq!(T::TVar(v2).assert_sub(&T::string(), &mut ctx), Ok(()));
            // v1 <: v2 <: string AND v1 <: integer (!)
            assert!(T::TVar(v1).assert_sub(&T::integer(), &mut ctx).is_err());
        }

        {
            let v1 = ctx.gen_tvar();
            let v2 = ctx.gen_tvar();
            let t1 = T::record(hash![a=just(T::integer()), b=just(T::TVar(v1))]);
            let t2 = T::record(hash![a=just(T::TVar(v2)), b=just(T::string()), c=just(T::Boolean)]);
            // {a=just integer, b=just v1} <: {a=just v2, b=just string, c=just boolean}
            assert_eq!(t1.assert_sub(&t2, &mut ctx), Ok(()));
            // ... AND v1 <: string
            assert_eq!(T::TVar(v1).assert_sub(&T::string(), &mut ctx), Ok(()));
            // ... AND v1 <: string AND v2 :> integer
            assert_eq!(T::integer().assert_sub(&T::TVar(v2), &mut ctx), Ok(()));
            // {a=just integer, b=just v1} = {a=just v2, b=just string, c=just boolean} (!)
            assert!(t1.assert_eq(&t2, &mut ctx).is_err());
        }
    }
}

