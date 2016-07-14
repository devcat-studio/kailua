use std::fmt;
use std::env;

pub trait Localize: fmt::Debug {
    fn fmt_localized(&self, f: &mut fmt::Formatter, lang: &str) -> fmt::Result;
}

impl<'a> Localize for &'a Localize {
    fn fmt_localized(&self, f: &mut fmt::Formatter, lang: &str) -> fmt::Result {
        (**self).fmt_localized(f, lang)
    }
}

impl<T: fmt::Display + fmt::Debug> Localize for T {
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
        ($($prefix:tt)*)
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
            #[derive(Debug)]
            $($prefix)* struct $name<$($constr)*> {
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
    ($(#[$meta:meta])* pub $name:ident $($t:tt)*) => (
        parse_generics_shim! {
            { constr, params, ltimes, tnames },
            then define_msg_internal!($name ($(#[$meta])* pub)),
            $($t)*
        }
    );

    ($(#[$meta:meta])* $name:ident $($t:tt)*) => (
        parse_generics_shim! {
            { constr, params, ltimes, tnames },
            then define_msg_internal!($name ($(#[$meta])*)),
            $($t)*
        }
    );
}

// XXX won't work well in Windows
fn get_locale_string_from_env() -> Option<String> {
    if let Ok(s) = env::var("LC_ALL") {
        if !s.is_empty() { return Some(s); }
    }
    if let Ok(s) = env::var("LC_MESSAGES") {
        if !s.is_empty() { return Some(s); }
    }
    // per POSIX, allow an empty string here; in Windows the empty envvar is forbidden
    env::var("LANG").ok()
}

#[cfg(windows)]
fn get_locale_string() -> Option<String> {
    // allow the user to override via envvar
    if let Some(s) = get_locale_string_from_env() {
        return Some(s);
    }

    use std::ffi::OsString;
    use std::os::windows::ffi::OsStringExt;
    use winapi;
    use kernel32;

    // what we need is the UI language, as opposed to the locale
    let langid = unsafe { kernel32::GetUserDefaultUILanguage() };
    let lcid = langid as winapi::LCID; // default sort order = 0 (XXX not yet in winapi?)
    const LOCALE_SISO639LANGNAME: winapi::LCTYPE = 0x59; // XXX not yet in winapi??
    let mut buf = [0; 16];
    let bufsz = unsafe {
        kernel32::GetLocaleInfoW(lcid, LOCALE_SISO639LANGNAME,
                                 buf.as_mut_ptr(), buf.len() as winapi::c_int)
    };
    if bufsz > 0 {
        let os: OsString = OsStringExt::from_wide(&buf[..(bufsz - 1) as usize]);
        return os.into_string().ok();
    }

    None
}

#[cfg(not(windows))]
fn get_locale_string() -> Option<String> {
    get_locale_string_from_env()
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

