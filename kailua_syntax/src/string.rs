//! Common string-like types and routines.

use std::fmt;
use std::ops;
use lex::Keyword;

fn is_unquotable(s: &[u8]) -> bool {
    fn is_first(c: u8) -> bool {
        match c { b'_' | b'a'...b'z' | b'A'...b'Z' => true, _ => false }
    }

    fn is_next(c: u8) -> bool {
        match c { b'_' | b'a'...b'z' | b'A'...b'Z' | b'0'...b'9' => true, _ => false }
    }

    !s.is_empty() && is_first(s[0])
                  && s[1..].iter().all(|&c| is_next(c))
                  && Keyword::from(s, true).is_none()
}

fn format_ascii_vec(f: &mut fmt::Formatter, s: &[u8]) -> fmt::Result {
    for &c in s {
        match c {
            b'\t' => write!(f, "\\t")?,
            b'\n' => write!(f, "\\n")?,
            b'\r' => write!(f, "\\r")?,
            b'"' | b'\'' | b'`' | b'\\' => write!(f, "\\{}", c as char)?,
            b'\x20'...b'\x7e' => write!(f, "{}", c as char)?,
            _ => write!(f, "\\x{:02x}", c)?,
        }
    }
    Ok(())
}

/// A name from the source code.
///
/// This may include non-identifier bytes if constructed from a quoted name in the meta block.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(Box<[u8]>);

impl Name {
    /// Returns true if the name is not an ordinary identifier
    /// for all known versions of Lua and Kailua.
    pub fn quote_required(&self) -> bool { !is_unquotable(&self.0) }

    /// Extracts the owned slice.
    pub fn into_bytes(self) -> Box<[u8]> { self.0 }
}

impl From<Box<[u8]>> for Name {
    fn from(n: Box<[u8]>) -> Name { Name(n) }
}

impl From<Vec<u8>> for Name {
    fn from(n: Vec<u8>) -> Name { Name(n.into_boxed_slice()) }
}

impl ops::Deref for Name {
    type Target = [u8];
    fn deref(&self) -> &[u8] { &self.0[..] }
}

/// Same to the `Debug` implementation.
impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

/// A name can be formatted in three ways:
///
/// * `{}` will always print backquotes. Suitable for error messages.
/// * `{:-}` won't print any backquotes.
/// * `{:+}` will print backquotes when required (see also `Name::quote_required`).
///
/// In any case, non-printable bytes are escaped as like `\r` or `\xab`;
/// any quotes (`'`, `"` or `` ` ``) are also escaped.
/// This makes switching quotes possible: `"{:-}"` will equal to the `Str` output.
impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let quote = if f.sign_plus() { self.quote_required() } else { !f.sign_minus() };
        if quote { write!(f, "`")?; }
        format_ascii_vec(f, &self.0)?;
        if quote { write!(f, "`")?; }
        Ok(())
    }
}

/// A string (or sometimes a desugared name) from the source code.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Str(Box<[u8]>);

impl Str {
    /// Returns true if the string is not an ordinary identifier
    /// for all known versions of Lua and Kailua.
    pub fn quote_required(&self) -> bool { !is_unquotable(&self.0) }

    /// Extracts the owned slice.
    pub fn into_bytes(self) -> Box<[u8]> { self.0 }
}

impl From<Box<[u8]>> for Str {
    fn from(s: Box<[u8]>) -> Str { Str(s) }
}

impl From<Vec<u8>> for Str {
    fn from(s: Vec<u8>) -> Str { Str(s.into_boxed_slice()) }
}

impl ops::Deref for Str {
    type Target = [u8];
    fn deref(&self) -> &[u8] { &self.0[..] }
}

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

/// A string can be formatted in three ways:
///
/// * `{}` will always print double quotes. Suitable for error messages.
/// * `{:-}` won't print any double quotes.
/// * `{:+}` will print double quotes when required (see also `Str::quote_required`).
///
/// In any case, non-printable bytes are escaped as like `\r` or `\xab`;
/// any quotes (`'`, `"` or `` ` ``) are also escaped.
/// This makes switching quotes possible: `` `{:-}` `` will equal to the `Name` output.
impl fmt::Debug for Str {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let quote = if f.sign_plus() { self.quote_required() } else { !f.sign_minus() };
        if quote { write!(f, "\"")?; }
        format_ascii_vec(f, &self.0)?;
        if quote { write!(f, "\"")?; }
        Ok(())
    }
}

impl<'a> From<&'a [u8]> for Name {
    fn from(s: &'a [u8]) -> Name { Name(s.to_owned().into_boxed_slice()) }
}

impl<'a> From<&'a [u8]> for Str {
    fn from(s: &'a [u8]) -> Str { Str(s.to_owned().into_boxed_slice()) }
}

impl<'a> From<&'a Name> for Name {
    fn from(n: &'a Name) -> Name { n.clone() }
}

impl<'a> From<&'a Str> for Str {
    fn from(s: &'a Str) -> Str { s.clone() }
}

impl From<Str> for Name {
    fn from(Str(s): Str) -> Name { Name(s) }
}

impl From<Name> for Str {
    fn from(Name(n): Name) -> Str { Str(n) }
}

