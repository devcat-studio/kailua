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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(Box<[u8]>);

impl Name {
    pub fn quote_required(&self) -> bool { !is_unquotable(&self.0) }
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

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let quote = if f.sign_plus() { self.quote_required() } else { !f.sign_minus() };
        if quote { write!(f, "`")?; }
        format_ascii_vec(f, &self.0)?;
        if quote { write!(f, "`")?; }
        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Str(Box<[u8]>);

impl Str {
    pub fn quote_required(&self) -> bool { !is_unquotable(&self.0) }
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

