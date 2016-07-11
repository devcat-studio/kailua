use std::fmt;
use std::env;

pub trait Localize {
    fn fmt_localized(&self, f: &mut fmt::Formatter, lang: &str) -> fmt::Result;
}

impl<'a> Localize for &'a Localize {
    fn fmt_localized(&self, f: &mut fmt::Formatter, lang: &str) -> fmt::Result {
        (**self).fmt_localized(f, lang)
    }
}

impl<T: fmt::Display> Localize for T {
    fn fmt_localized(&self, f: &mut fmt::Formatter, _lang: &str) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

pub struct Localized<'b, 'c, T: Localize + ?Sized + 'b> {
    base: &'b T,
    lang: &'c str,
}

impl<'b, 'c, T: Localize + ?Sized + 'b> Localized<'b, 'c, T> {
    pub fn new(base: &'b T, lang: &'c str) -> Localized<'b, 'c, T> {
        Localized { base: base, lang: lang }
    }
}

impl<'b, 'c, T: Localize + ?Sized + 'b> fmt::Display for Localized<'b, 'c, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.base.fmt_localized(f, self.lang)
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! define_msg_internal {
    (@as_item $i:item) => ($i);

    (@gen_match $f:ident, $l:ident; $($lang:pat => $format:tt),*; $tail:tt) => (
        match $l {
            $($lang => define_msg_internal!(@gen_arm $f; $format; $tail),)*
        }
    );

    (@gen_arm $f:ident; $format:tt; ($($tail:tt)*)) => (
        write!($f, $format $($tail)*)
    );

    (
        $name:ident
        ($($vis:tt)*)
        {
            constr: [ $($constr:tt)* ],
            params: [ $($params:tt)* ],
            $($_generics:tt)*
        },
        $({
            $($fname:ident: $ftype:ty),* $(,)*
        })*:
        $($lang:pat => $format:tt),* $(,)*
    ) => (
        // since `$($constr)*` and `$($params)*` have to be expanded lazily,
        // we need to coerce the AST to delay the expansion

        define_msg_internal! { @as_item
            $($vis)* struct $name<$($params)*> {
                $($(pub $fname: $ftype,)*)*
            }
        }

        define_msg_internal! { @as_item
            impl<$($constr)*> $crate::Localize for $name<$($params)*> {
                fn fmt_localized(&self, f: &mut ::std::fmt::Formatter,
                                 lang: &str) -> ::std::fmt::Result {
                    // "tt bundling" as in http://stackoverflow.com/a/37754096
                    define_msg_internal!(@gen_match f, lang;
                        $($lang => $format),*;
                        ($($(, $fname = $crate::Localized::new(&self.$fname, lang))*)*))
                }
            }
        }
    );
}

#[macro_export]
macro_rules! define_msg {
    (pub $name:ident $($t:tt)*) => (
        parse_generics_shim! {
            { constr, params, ltimes, tnames },
            then define_msg_internal!($name (pub)),
            $($t)*
        }
    );

    ($name:ident $($t:tt)*) => (
        parse_generics_shim! {
            { constr, params, ltimes, tnames },
            then define_msg_internal!($name ()),
            $($t)*
        }
    );
}

// XXX won't work well in Windows
fn get_locale_string() -> Option<String> {
    if let Ok(s) = env::var("LC_ALL") {
        if !s.is_empty() { return Some(s); }
    }
    if let Ok(s) = env::var("LC_MESSAGES") {
        if !s.is_empty() { return Some(s); }
    }
    env::var("LANG").ok()
}

pub fn get_message_language() -> Option<String> {
    if let Some(locale) = get_locale_string() {
        let locale = locale.to_lowercase();
        // either `xx`, `xx-<trail>` or `xx_<trail>`; not `/path/to/locale` or `xyz`
        if locale.len() >= 2 && locale.chars().take(2).all(|c| 'a' <= c && c <= 'z')
                             && locale[2..].chars().next().map_or(true, |c| !c.is_alphabetic()) {
            Some(locale)
        } else {
            None
        }
    } else {
        None
    }
}

