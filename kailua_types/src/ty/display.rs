use std::fmt;
use std::cell::RefCell;
use std::collections::{HashSet, HashMap};

use kailua_env::{Span, Spanned};
use kailua_diag::{Locale, Localize, Localized};
use kailua_syntax::Name;
use super::{TypeContext, S, TVar, RVar};

/// A display state to catch recursive types and handle display hints.
pub struct DisplayState<'a> {
    /// A message locale.
    pub locale: Locale,
    /// An associated type context.
    pub context: &'a TypeContext,

    disambiguators: RefCell<HashMap<Name, HashMap<Span, usize>>>,
    slots_seen: RefCell<HashSet<*const S>>,
    tvars_seen: RefCell<HashSet<u32>>,
    rvars_seen: RefCell<HashSet<u32>>,

    // rvars are one of the biggest source of type display bloats,
    // so they are controlled via {:.<maxlevel>} (0 means that no records are printed)
    max_rvar_level: Option<usize>,
}

impl<'a> DisplayState<'a> {
    pub fn new(f: &fmt::Formatter, locale: Locale, ctx: &'a TypeContext) -> DisplayState<'a> {
        DisplayState {
            locale: locale,
            context: ctx,
            disambiguators: RefCell::new(HashMap::new()),
            slots_seen: RefCell::new(HashSet::new()),
            tvars_seen: RefCell::new(HashSet::new()),
            rvars_seen: RefCell::new(HashSet::new()),
            max_rvar_level: f.precision(),
        }
    }

    pub fn is_top_level(&self) -> bool {
        self.rvars_seen.borrow().is_empty()
    }

    pub fn can_recurse(&self) -> bool {
        self.max_rvar_level.map_or(true, |maxlevel| self.rvars_seen.borrow().len() < maxlevel)
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

    pub fn unmark_slot(&self, slot: &S) {
        self.slots_seen.borrow_mut().remove(&(slot as *const S));
    }

    pub fn unmark_tvar(&self, tvar: TVar) {
        self.tvars_seen.borrow_mut().remove(&tvar.0);
    }

    pub fn unmark_rvar(&self, rvar: RVar) {
        self.rvars_seen.borrow_mut().remove(&rvar.to_u32());
    }

    pub fn disambiguator(&self, name: &Spanned<Name>) -> Disambiguator {
        if name.span.is_dummy() {
            return Disambiguator(None);
        }

        let mut disambig = self.disambiguators.borrow_mut();
        let mut spans = disambig.entry(name.base.clone()).or_insert_with(|| HashMap::new());
        let next = spans.len();
        let value = *spans.entry(name.span).or_insert(next);
        Disambiguator(Some(value))
    }
}

/// The disambiguator for same names with different origins.
///
/// If we have three same names to print, the first printed name will print as is,
/// and second and third names will be suffixed with `#1` and `#2`.
/// The origin is determined by a span, so it always prints `#?` when the span is a dummy.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Disambiguator(Option<usize>);

impl fmt::Display for Disambiguator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            Some(0) => Ok(()),
            Some(v) => write!(f, "#{}", v),
            None => write!(f, "#?"),
        }
    }
}

impl fmt::Debug for Disambiguator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(v) = self.0 {
            write!(f, "Disambiguator({})", v)
        } else {
            write!(f, "Disambiguator(?)")
        }
    }
}

/// A display name associated to the type or a portion of the type (in the `Unioned`).
#[derive(Clone, Debug)]
pub enum DisplayName {
    /// The type originates from a type name.
    ///
    /// This is always displayed, but an option can be used to ignore it on the top-level.
    /// Currently the option is enabled for printing a type from the hover help.
    Type(Spanned<Name>),

    /// The type originates from a variable.
    ///
    /// This is a weak hint, as this is only useful when the containing type is complex enough
    /// that *any* hint will be useful. Type names will override this hint.
    Var(Spanned<Name>),
}

impl DisplayName {
    pub fn can_override(&self, prev: &DisplayName) -> bool {
        match (self, prev) {
            // since variables are optional but types are not, we should keep types
            (&DisplayName::Var(_), &DisplayName::Type(_)) => false,
            // otherwise it is a genuine update (e.g. `--# type Y = X` will need updates)
            (_, _) => true,
        }
    }
}

impl PartialEq for DisplayName {
    fn eq(&self, other: &DisplayName) -> bool {
        match (self, other) {
            (&DisplayName::Type(ref lhs), &DisplayName::Type(ref rhs)) |
            (&DisplayName::Var(ref lhs), &DisplayName::Var(ref rhs)) => {
                lhs.base == rhs.base && lhs.span == rhs.span
            },
            (_, _) => false,
        }
    }
}

impl Display for DisplayName {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        match *self {
            DisplayName::Type(ref name) => {
                write!(f, "{:+}{}", name.base, st.disambiguator(name))
            },
            DisplayName::Var(ref name) => {
                match &st.locale[..] {
                    "ko" => write!(f, "<변수")?,
                    _    => write!(f, "<variable")?,
                }
                write!(f, " {:+}{}>", name.base, st.disambiguator(name))
            },
        }
    }
}

/// Human-readable description of various types requiring the type context.
///
/// Expected to implement `std::fmt::Display`.
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

/// A `fmt_displayed` wrapper for a particular context (`&DisplayState` or `&TypeContext`).
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
        let st = DisplayState::new(f, locale, self.ctx);
        self.base.fmt_displayed(f, &st)
    }
}

impl<'b, T: Display + fmt::Debug + 'b, C> fmt::Debug for Displayed<'b, T, C> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self.base, f)
    }
}

