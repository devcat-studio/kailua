use std::fmt;
use std::borrow::Cow;

use diag::CheckResult;
use super::{T, TVarContext, Lattice, Flags, Numbers, Strings, Tables, Functions, TVar};
use super::{error_not_sub, error_not_eq};
use super::flags::*;

// expanded value types for unions
#[derive(Clone, PartialEq)]
pub struct Union {
    pub has_dynamic: bool, // XXX
    pub has_nil: bool,
    pub has_true: bool,
    pub has_false: bool,
    pub numbers: Option<Numbers>,
    pub strings: Option<Strings>,
    pub tables: Option<Tables>,
    pub functions: Option<Functions>,
    pub tvar: Option<TVar>,
}

impl Union {
    pub fn from<'a>(ty: T<'a>) -> Union {
        let mut u = Union {
            has_dynamic: false, has_nil: false, has_true: false, has_false: false,
            numbers: None, strings: None, tables: None, functions: None, tvar: None,
        };

        match ty {
            T::Dynamic => { u.has_dynamic = true; }
            T::None    => {}
            T::Nil     => { u.has_nil = true; }
            T::Boolean => { u.has_true = true; u.has_false = true; }
            T::True    => { u.has_true = true; }
            T::False   => { u.has_false = true; }

            T::Numbers(num)    => { u.numbers = Some(num.into_owned()); }
            T::Strings(str)    => { u.strings = Some(str.into_owned()); }
            T::Tables(tab)     => { u.tables = Some(tab.into_owned()); }
            T::Functions(func) => { u.functions = Some(func.into_owned()); }
            T::TVar(tv)        => { u.tvar = Some(tv); }

            T::Union(u) => return u.into_owned() // ignore `u` above
        }

        u
    }

    pub fn flags(&self) -> Flags {
        let mut flags = T_NONE;
        if self.has_dynamic  { flags = flags | T_DYNAMIC; }
        if self.has_nil      { flags = flags | T_NIL; }
        if self.has_true     { flags = flags | T_TRUE; }
        if self.has_false    { flags = flags | T_FALSE; }
        match self.numbers {
            None => {}
            Some(Numbers::All) => { flags = flags | T_NUMBER; }
            Some(_)  => { flags = flags | T_INTEGER; }
        }
        if self.strings.is_some()   { flags = flags | T_STRING; }
        if self.tables.is_some()    { flags = flags | T_TABLE; }
        if self.functions.is_some() { flags = flags | T_FUNCTION; }
        flags
    }

    pub fn visit<'a, E, F>(&'a self, mut f: F) -> Result<(), E>
            where F: FnMut(T<'a>) -> Result<(), E> {
        // dynamic type eschews every other types
        if self.has_dynamic { return f(T::Dynamic); }

        if self.has_nil { try!(f(T::Nil)); }
        if self.has_true {
            if self.has_false { try!(f(T::Boolean)); } else { try!(f(T::True)); }
        } else if self.has_false {
            try!(f(T::False));
        }
        if let Some(ref num) = self.numbers { try!(f(T::Numbers(Cow::Borrowed(num)))) }
        if let Some(ref str) = self.strings { try!(f(T::Strings(Cow::Borrowed(str)))) }
        if let Some(ref tab) = self.tables { try!(f(T::Tables(Cow::Borrowed(tab)))) }
        if let Some(ref func) = self.functions { try!(f(T::Functions(Cow::Borrowed(func)))) }
        if let Some(tvar) = self.tvar { try!(f(T::TVar(tvar))) }
        Ok(())
    }

    pub fn simplify(self) -> T<'static> {
        let single = {
            let mut single = None;
            let ret = self.visit(|ty| {
                if single.is_some() { return Err(()); }
                single = Some(ty);
                Ok(())
            });
            if ret.is_ok() {
                Some(single.unwrap_or(T::None).into_send())
            } else {
                None
            }
        };
        single.unwrap_or_else(|| T::Union(Cow::Owned(self)))
    }
}

impl Lattice for Union {
    type Output = Union;

    fn normalize(mut self) -> Self {
        self.numbers   = self.numbers.normalize();
        self.strings   = self.strings.normalize();
        self.tables    = self.tables.normalize();
        self.functions = self.functions.normalize();
        self
    }

    fn union(mut self, other: Union, ctx: &mut TVarContext) -> Union {
        self.has_dynamic |= other.has_dynamic;
        self.has_nil     |= other.has_nil;
        self.has_true    |= other.has_true;
        self.has_false   |= other.has_false;

        self.numbers   = self.numbers.union(other.numbers, ctx);
        self.strings   = self.strings.union(other.strings, ctx);
        self.tables    = self.tables.union(other.tables, ctx);
        self.functions = self.functions.union(other.functions, ctx);

        self.tvar = match (self.tvar, other.tvar) {
            (Some(a), Some(b)) => Some(a.union(b, ctx)),
            (a, b) => a.or(b),
        };

        self
    }

    fn intersect(mut self, other: Union, ctx: &mut TVarContext) -> Union {
        self.has_dynamic &= other.has_dynamic;
        self.has_nil     &= other.has_nil;
        self.has_true    &= other.has_true;
        self.has_false   &= other.has_false;

        self.numbers   = self.numbers.intersect(other.numbers, ctx);
        self.strings   = self.strings.intersect(other.strings, ctx);
        self.tables    = self.tables.intersect(other.tables, ctx);
        self.functions = self.functions.intersect(other.functions, ctx);

        self.tvar = match (self.tvar, other.tvar) {
            (Some(a), Some(b)) => Some(a.intersect(b, ctx)),
            (_, _) => None,
        };

        self
    }

    fn assert_sub(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        // exit early if either side is dynamic
        if self.has_dynamic || other.has_dynamic { return Ok(()); }

        if (self.has_nil && !other.has_nil) || (self.has_true && !other.has_true) ||
                                               (self.has_false && !other.has_false) {
            return error_not_sub(self, other);
        }

        try!(self.numbers.assert_sub(&other.numbers, &mut ()));
        try!(self.strings.assert_sub(&other.strings, &mut ()));

        // XXX err on unions with possible overlapping instantiation for now
        let count = if self.tables.is_some() { 1 } else { 0 } +
                    if self.functions.is_some() { 1 } else { 0 } +
                    if self.tvar.is_some() { 1 } else { 0 };
        if count > 1 { unimplemented!() }

        let count = if other.tables.is_some() { 1 } else { 0 } +
                    if other.functions.is_some() { 1 } else { 0 } +
                    if other.tvar.is_some() { 1 } else { 0 };
        if count > 1 { unimplemented!() }

        try!(self.tables.assert_sub(&other.tables, ctx));
        try!(self.functions.assert_sub(&other.functions, ctx));

        match (self.tvar, other.tvar) {
            (Some(a), Some(b)) => a.assert_sub(&b, ctx),
            (Some(_), None) => error_not_sub(self, other),
            (None, _) => Ok(()),
        }
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        // exit early if either side is dynamic
        if self.has_dynamic || other.has_dynamic { return Ok(()); }

        match (self.tvar, self.flags(), other.tvar, other.flags()) {
            (Some(a), T_NONE, Some(b), T_NONE) =>
                return ctx.assert_tvar_eq_tvar(a, b),
            (Some(a), T_NONE, _, _) =>
                return ctx.assert_tvar_eq(a, &T::Union(Cow::Borrowed(other))),
            (_, _, Some(b), T_NONE) =>
                return ctx.assert_tvar_eq(b, &T::Union(Cow::Borrowed(self))),
            (Some(_), _, _, _) | (_, _, Some(_), _) =>
                // XXX if we have a type variable in the union,
                // the type variable essentially eschews all differences between two input types
                // and there is no error condition except for conflicting instantiation.
                unimplemented!(),
            (None, _, None, _) => {}
        }

        if self.has_nil != other.has_nil || self.has_true != other.has_true ||
                                            self.has_false != other.has_false {
            return error_not_eq(self, other);
        }

        try!(self.numbers.assert_eq(&other.numbers, &mut ()));
        try!(self.strings.assert_eq(&other.strings, &mut ()));
        try!(self.tables.assert_eq(&other.tables, ctx));
        try!(self.functions.assert_eq(&other.functions, ctx));
        Ok(())
    }
}

impl fmt::Debug for Union {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "("));
        let mut first = true;
        try!(self.visit(|ty| {
            if first {
                first = false;
            } else {
                try!(write!(f, "|"));
            }
            fmt::Debug::fmt(&ty, f)
        }));
        write!(f, ")")
    }
}

