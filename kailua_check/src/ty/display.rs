use std::fmt;

use kailua_env::Spanned;
use kailua_diag::{Locale, Localize, Localized};
use super::TypeContext;

// human-readable description of various types requiring the type context.
// expected to implement fmt::Display.
pub trait Display: fmt::Debug + Sized {
    fn fmt_displayed(&self, f: &mut fmt::Formatter,
                     locale: Locale, ctx: &TypeContext) -> fmt::Result;

    fn display<'b, 'c>(&'b self, ctx: &'c TypeContext) -> Displayed<'b, 'c, Self> {
        Displayed { base: self, ctx: ctx }
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt_displayed(&self, f: &mut fmt::Formatter,
                     locale: Locale, ctx: &TypeContext) -> fmt::Result {
        self.base.fmt_displayed(f, locale, ctx)
    }
}

impl<T: Display + ?Sized> Display for Box<T> {
    fn fmt_displayed(&self, f: &mut fmt::Formatter,
                     locale: Locale, ctx: &TypeContext) -> fmt::Result {
        (**self).fmt_displayed(f, locale, ctx)
    }
}

impl<'a, T: Display + ?Sized> Display for &'a T {
    fn fmt_displayed(&self, f: &mut fmt::Formatter,
                     locale: Locale, ctx: &TypeContext) -> fmt::Result {
        (**self).fmt_displayed(f, locale, ctx)
    }
}

impl<'a, T: Display + ?Sized> Display for &'a mut T {
    fn fmt_displayed(&self, f: &mut fmt::Formatter,
                     locale: Locale, ctx: &TypeContext) -> fmt::Result {
        (**self).fmt_displayed(f, locale, ctx)
    }
}

impl Display for String {
    fn fmt_displayed(&self, f: &mut fmt::Formatter,
                     _locale: Locale, _ctx: &TypeContext) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl<'a> Display for &'a str {
    fn fmt_displayed(&self, f: &mut fmt::Formatter,
                     _locale: Locale, _ctx: &TypeContext) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

pub struct Displayed<'b, 'c, T: Display + 'b> {
    base: &'b T,
    ctx: &'c TypeContext,
}

impl<'b, 'c, T: Display + 'b> Displayed<'b, 'c, T> {
    // a shortcut for `Localized::new` (hard to provide for Localize itself due to trait object)
    pub fn localized<'a>(&'a self, locale: Locale) -> Localized<'a, Displayed<'b, 'c, T>> {
        Localized::new(self, locale)
    }
}

impl<'b, 'c, T: Display + 'b> Localize for Displayed<'b, 'c, T> {
    fn fmt_localized(&self, f: &mut fmt::Formatter, locale: Locale) -> fmt::Result {
        self.base.fmt_displayed(f, locale, self.ctx)
    }
}

impl<'b, 'c, T: Display + fmt::Debug + 'b> fmt::Debug for Displayed<'b, 'c, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self.base, f)
    }
}

