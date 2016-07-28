use kailua_syntax::Keyword;

pub type Error = String;

pub type CheckResult<T> = Result<T, Error>;

pub fn unquotable_name(s: &[u8]) -> bool {
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

