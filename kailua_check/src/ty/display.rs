use std::fmt;
use std::cell::RefCell;
use std::collections::HashSet;

use kailua_env::Spanned;
use kailua_diag::{Locale, Localize, Localized};
use super::{TypeContext, S, TVar, RVar};

pub struct DisplayState<'a> {
    pub locale: Locale,
    pub context: &'a TypeContext,
    slots_seen: RefCell<HashSet<*const S>>,
    tvars_seen: RefCell<HashSet<u32>>,
    rvars_seen: RefCell<HashSet<u32>>,
}

impl<'a> DisplayState<'a> {
    pub fn new(locale: Locale, ctx: &'a TypeContext) -> DisplayState<'a> {
        DisplayState {
            locale: locale,
            context: ctx,
            slots_seen: RefCell::new(HashSet::new()),
            tvars_seen: RefCell::new(HashSet::new()),
            rvars_seen: RefCell::new(HashSet::new()),
        }
    }

    pub fn is_slot_seen(&self, slot: &S) -> bool {
        !self.slots_seen.borrow_mut().insert(slot as *const S)
    }

    pub fn is_tvar_seen(&self, tvar: TVar) -> bool {
        !self.tvars_seen.borrow_mut().insert(tvar.0)
    }

    pub fn is_rvar_seen(&self, rvar: RVar) -> bool {
        !self.rvars_seen.borrow_mut().insert(rvar.to_u32())
    }
}

// human-readable description of various types requiring the type context.
// expected to implement fmt::Display.
pub trait Display: fmt::Debug + Sized {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result;

    fn display<'b, C>(&'b self, ctx: C) -> Displayed<'b, Self, C> {
        Displayed { base: self, ctx: ctx }
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        self.base.fmt_displayed(f, st)
    }
}

impl<T: Display + ?Sized> Display for Box<T> {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        (**self).fmt_displayed(f, st)
    }
}

impl<'a, T: Display + ?Sized> Display for &'a T {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        (**self).fmt_displayed(f, st)
    }
}

impl<'a, T: Display + ?Sized> Display for &'a mut T {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        (**self).fmt_displayed(f, st)
    }
}

impl Display for String {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, _st: &DisplayState) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl<'a> Display for &'a str {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, _st: &DisplayState) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

// a `fmt_displayed` wrapper for a particular context (`&DisplayState` or `&TypeContext`)
pub struct Displayed<'b, T: Display + 'b, C> {
    base: &'b T,
    ctx: C,
}

impl<'b, 'c, 'd, T: Display + 'b> fmt::Display for Displayed<'b, T, &'c DisplayState<'d>> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.base.fmt_displayed(f, self.ctx)
    }
}

impl<'b, 'c, T: Display + 'b> Displayed<'b, T, &'c TypeContext> {
    // a shortcut for `Localized::new` (hard to provide for Localize itself due to trait object)
    pub fn localized<'a>(&'a self,
                         locale: Locale) -> Localized<'a, Displayed<'b, T, &'c TypeContext>> {
        Localized::new(self, locale)
    }
}

impl<'b, 'c, T: Display + 'b> Localize for Displayed<'b, T, &'c TypeContext> {
    fn fmt_localized(&self, f: &mut fmt::Formatter, locale: Locale) -> fmt::Result {
        self.base.fmt_displayed(f, &DisplayState::new(locale, self.ctx))
    }
}

impl<'b, T: Display + fmt::Debug + 'b, C> fmt::Debug for Displayed<'b, T, C> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self.base, f)
    }
}

