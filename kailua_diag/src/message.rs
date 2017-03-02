use std::fmt;
use std::ops;
use std::str;
use std::env;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Locale {
    lang: [u8; 8],
}

impl Locale {
    pub fn new(locale: &str) -> Option<Locale> {
        // should be 2--8 bytes long
        if locale.len() < 2 || locale.len() > 8 {
            return None;
        }

        // should be letters (normalized to lowercase), `-` or `_` (normalized to `-`)
        let mut lang = [0u8; 8];
        lang[..locale.len()].copy_from_slice(locale.as_bytes());
        for c in &mut lang[..locale.len()] {
            match *c {
                b'a'...b'z' | b'-' => {},
                b'A'...b'Z' => *c += 32,
                b'_' => *c = b'-',
                _ => return None,
            }
        }

        // first two letters should be letters
        if !(b'a' <= lang[0] && lang[0] <= b'z' && b'a' <= lang[1] && lang[1] <= b'z') {
            return None;
        }

        Some(Locale { lang: lang })
    }

    pub fn dummy() -> Locale {
        Locale { lang: *b"xx\0\0\0\0\0\0" }
    }
}

impl<'a> From<&'a str> for Locale {
    fn from(s: &'a str) -> Locale {
        Locale::new(s).expect("invalid locale")
    }
}

impl ops::Deref for Locale {
    type Target = str;

    fn deref(&self) -> &str {
        str::from_utf8(&self.lang).expect("locale is not UTF-8").trim_right_matches('\0')
    }
}

impl fmt::Debug for Locale {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<Locale {}>", &self[..])
    }
}

#[test]
fn test_locale_names() {
    assert!(Locale::new("").is_none());
    assert!(Locale::new("e").is_none());
    assert!(Locale::new("en").is_some());
    assert!(Locale::new("ko").is_some());
    assert!(Locale::new("ko-KR").is_some());
    assert_eq!(Locale::from("ko-KR"), Locale::from("ko_kr"));
    assert_ne!(Locale::from("ko-KR"), Locale::from("ko"));
    assert_ne!(Locale::from("kor"), Locale::from("ko"));
    assert_eq!(Locale::from("KO"), Locale::from("ko"));
    assert!(Locale::new("ko-KR-x-qqq").is_none());
}

pub trait Localize: fmt::Debug {
    fn fmt_localized(&self, f: &mut fmt::Formatter, locale: Locale) -> fmt::Result;
}

impl<'a> Localize for &'a Localize {
    fn fmt_localized(&self, f: &mut fmt::Formatter, locale: Locale) -> fmt::Result {
        (**self).fmt_localized(f, locale)
    }
}

impl<T: fmt::Display + fmt::Debug> Localize for T {
    fn fmt_localized(&self, f: &mut fmt::Formatter, _locale: Locale) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

pub struct Localized<'b, T: Localize + ?Sized + 'b> {
    base: &'b T,
    locale: Locale,
}

impl<'b, T: Localize + ?Sized + 'b> Localized<'b, T> {
    pub fn new(base: &'b T, locale: Locale) -> Localized<'b, T> {
        Localized { base: base, locale: locale }
    }
}

impl<'b, T: Localize + ?Sized + 'b> fmt::Display for Localized<'b, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.base.fmt_localized(f, self.locale)
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! define_msg_internal {
    (@as_item $i:item) => ($i);

    (@gen_match $f:ident, $l:ident; $($locale:pat => $format:tt),*; $tail:tt) => (
        match &$l[..] {
            $($locale => define_msg_internal!(@gen_arm $f; $format; $tail),)*
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
        $($locale:pat => $format:tt),* $(,)*
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
                                 locale: ::kailua_diag::Locale) -> ::std::fmt::Result {
                    // "tt bundling" as in http://stackoverflow.com/a/37754096
                    define_msg_internal!(@gen_match f, locale;
                        $($locale => $format),*;
                        ($($(, $fname = $crate::Localized::new(&self.$fname, locale))*)*))
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

pub fn get_message_locale() -> Option<Locale> {
    get_locale_string().and_then(|locale| Locale::new(&locale))
}

