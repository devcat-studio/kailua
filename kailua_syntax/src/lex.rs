use std::str;
use std::u64;
use std::fmt;

use message as m;
use kailua_diag as diag;
use kailua_env::{SourceData, Pos, Span, Spanned, WithLoc};
use kailua_env::SourceData::{U8, U16, EOF};
use kailua_diag::{Report, Reporter, Localize, Localized};

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Error, // dummy token
    Comment,
    Punct(Punct),
    Keyword(Keyword),
    Num(f64),
    Name(Vec<u8>),
    Str(Vec<u8>),
    EOF,
}

impl Localize for Tok {
    fn fmt_localized(&self, f: &mut fmt::Formatter, lang: &str) -> fmt::Result {
        match (lang, self) {
            ("ko", &Tok::Error)      => write!(f, "잘못된 문자"),
            (_,    &Tok::Error)      => write!(f, "an invalid character"),
            ("ko", &Tok::Comment)    => write!(f, "주석"),
            (_,    &Tok::Comment)    => write!(f, "a comment"),
            (_,    &Tok::Punct(p))   => write!(f, "{}", Localized::new(&p, lang)),
            (_,    &Tok::Keyword(w)) => write!(f, "{}", Localized::new(&w, lang)),
            ("ko", &Tok::Num(_))     => write!(f, "숫자"),
            (_,    &Tok::Num(_))     => write!(f, "a number"),
            ("ko", &Tok::Name(_))    => write!(f, "이름"),
            (_,    &Tok::Name(_))    => write!(f, "a name"),
            ("ko", &Tok::Str(_))     => write!(f, "문자열 리터럴"),
            (_,    &Tok::Str(_))     => write!(f, "a string literal"),
            ("ko", &Tok::EOF)        => write!(f, "파일의 끝"),
            (_,    &Tok::EOF)        => write!(f, "the end of file"),
        }
    }
}

impl<'a> Localize for &'a Tok {
    fn fmt_localized(&self, f: &mut fmt::Formatter, lang: &str) -> fmt::Result {
        (**self).fmt_localized(f, lang)
    }
}

macro_rules! define_puncts {
    ($ty:ident |$lang:ident|: $($i:ident $t:expr,)*) => (
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        pub enum $ty { $($i,)* }

        impl Localize for $ty {
            fn fmt_localized(&self, f: &mut fmt::Formatter, $lang: &str) -> fmt::Result {
                let text = match *self { $($ty::$i => $t,)* };
                fmt::Display::fmt(text, f)
            }
        }
    );
}

define_puncts! { Punct |lang|:
    Plus        "`+`",
    Dash        "`-`",
    Star        "`*`",
    Slash       "`/`",
    Percent     "`%`",
    Caret       "`^`",
    Hash        "`#`",
    EqEq        "`==`",
    TildeEq     "`~=`",
    LtEq        "`<=`",
    GtEq        "`>=`",
    Lt          "`<`",
    Gt          "`>`",
    Eq          "`=`",
    Amp         "`&`",  // reserved for 5.3+, never generated
    Tilde       "`~`",  // reserved for 5.3+, never generated
    Pipe        "`|`",  // reserved for 5.3+, only generated in meta
    LtLt        "`<<`", // reserved for 5.3+, never generated
    GtGt        "`>>`", // reserved for 5.3+, never generated
    SlashSlash  "`//`", // reserved for 5.3+, never generated
    LParen      "`(`",
    RParen      "`)`",
    LBrace      "`{`",
    RBrace      "`}`",
    LBracket    "`[`",
    RBracket    "`]`",
    Semicolon   "`;`",
    Colon       "`:`",
    ColonColon  "`::`", // reserved for 5.2+, never generated
    Comma       "`,`",
    Dot         "`.`",
    DotDot      "`..`",
    DotDotDot   "`...`",

    // Kailua extensions
    DashDashHash    "`--#`",
    DashDashV       "`--v`",
    DashDashColon   "`--:`",
    DashDashGt      "`-->`",
    Ques            "`?`",
    Bang            "`!`",
    Newline         match lang { "ko" => "개행문자", _ => "a newline" },
}

macro_rules! define_keywords {
    ($ty:ident: everywhere { $($i:ident $t:expr,)* } meta_only { $($mi:ident $mt:expr,)* }) => (
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        pub enum $ty { $($i,)* $($mi,)* }

        impl $ty {
            pub fn from(s: &[u8], in_meta: bool) -> Option<Keyword> {
                match (in_meta, s) {
                    $((_, $t) => Some(Keyword::$i),)*
                    $((true, $mt) => Some(Keyword::$mi),)*
                    (_, _) => None,
                }
            }

            pub fn name(&self) -> &'static [u8] {
                match *self { $($ty::$i => $t,)* $($ty::$mi => $mt,)* }
            }
        }

        impl Localize for $ty {
            fn fmt_localized(&self, f: &mut fmt::Formatter, lang: &str) -> fmt::Result {
                let name = str::from_utf8(self.name()).unwrap();
                match lang {
                    "ko" => write!(f, "예약어 `{}`", name),
                    _ => write!(f, "a keyword `{}`", name),
                }
            }
        }
    );
}

define_keywords! { Keyword:
    everywhere {
        And         b"and",
        Break       b"break",
        Do          b"do",
        Else        b"else",
        Elseif      b"elseif",
        End         b"end",
        False       b"false",
        For         b"for",
        Function    b"function",
        // Goto should be here, but see below
        If          b"if",
        In          b"in",
        Local       b"local",
        Nil         b"nil",
        Not         b"not",
        Or          b"or",
        Repeat      b"repeat",
        Return      b"return",
        Then        b"then",
        True        b"true",
        Until       b"until",
        While       b"while",
    }

    meta_only { // Kailua extensions
        Assume      b"assume",
        Const       b"const",
        Global      b"global",
        Map         b"map",
        Module      b"module",
        Once        b"once",
        Open        b"open",
        Type        b"type",
        Var         b"var",
        Vector      b"vector",

        // this is _not_ a Kailua extension, but reserved to ensure that
        // the name should be escaped even in a metablock of the Lua 5.1 mode.
        // it is technically treated as like a keyword elsewhere.
        Goto        b"goto",        // reserved for 5.2+
    }
}

pub struct Lexer<'a> {
    bytes: &'a mut Iterator<Item=Spanned<SourceData>>,
    pos: Pos,
    last_pos: Pos,
    last_data: SourceData,
    lookahead: bool,
    meta: bool,
    meta_span: Span,
    eof: bool,
    report: &'a Report,
}

fn is_digit(c: SourceData) -> bool {
    match c { U8(b'0'...b'9') => true, _ => false }
}

fn normalize_data(c: SourceData) -> SourceData {
    // normalize ASCII letters to U8, so that we can easily check against them
    match c {
        U8(v) => U8(v),
        U16(v) => if v < 0x80 { U8(v as u8) } else { U16(v) },
        EOF => EOF,
    }
}

impl<'a> Lexer<'a> {
    pub fn new(bytes: &'a mut Iterator<Item=Spanned<SourceData>>,
               report: &'a Report) -> Lexer<'a> {
        let first = bytes.next().expect("Lexer should have got at least one token");
        Lexer {
            bytes: bytes,
            pos: first.span.end(),
            last_pos: first.span.begin(),
            last_data: normalize_data(first.base),
            lookahead: true,
            meta: false,
            meta_span: Span::dummy(),
            eof: false,
            report: report,
        }
    }

    fn pos(&self) -> Pos {
        if self.lookahead {
            self.last_pos
        } else {
            self.pos
        }
    }

    fn read(&mut self) -> SourceData {
        if self.lookahead {
            self.lookahead = false;
            self.last_data
        } else {
            self.last_pos = self.pos;
            if let Some(Spanned { span, base: c }) = self.bytes.next() {
                let c = normalize_data(c);
                self.pos = span.end();
                self.last_data = c;
                c
            } else {
                EOF
            }
        }
    }

    fn unread(&mut self, last: SourceData) {
        assert!(!self.lookahead, "only one lookahead byte is supported");
        assert!(!self.last_pos.is_dummy());
        assert_eq!(self.last_data, last);
        self.lookahead = true;
    }

    fn try<Cond>(&mut self, mut cond: Cond) -> Option<SourceData>
            where Cond: FnMut(SourceData) -> bool {
        let c = self.read();
        if c != EOF {
            if cond(c) {
                Some(c)
            } else {
                self.unread(c);
                None
            }
        } else {
            None
        }
    }

    fn scan_while<Cond, F>(&mut self, mut cond: Cond, mut f: F)
            where Cond: FnMut(SourceData) -> bool, F: FnMut(SourceData) {
        loop {
            let c = self.read();
            if c == EOF {
                break;
            }
            if !cond(c) {
                self.unread(c);
                break;
            }
            f(c);
        }
    }

    // here comes a fun part! we know that c is almost correct UTF-16,
    // but for the purpose of reporting and everything else
    // we need to transcode it into a byte sequence, preferably UTF-8.
    // this also means that, when we see a bad surrogate, we should bail out...
    fn translate_u16<F>(&mut self, lastpos: Pos, c: u16, mut f: F) -> diag::Result<()>
            where F: FnMut(u8) {
        match c {
            // high surrogate
            0xd800...0xdbff => {
                if let U16(c2 @ 0xdc00...0xdfff) = self.read() {
                    // std::char::encode_utf8 is not yet stable ;(
                    let c = 0x10000 + ((c as u32 & 0x3ff << 10) | (c2 as u32 & 0x3ff));
                    f(0b1111_0000 | (c >> 18 & 0x07) as u8);
                    f(0b1000_0000 | (c >> 12 & 0x3f) as u8);
                    f(0b1000_0000 | (c >>  6 & 0x3f) as u8);
                    f(0b1000_0000 | (c       & 0x3f) as u8);
                } else {
                    try!(self.report.error(lastpos..self.pos(), m::BadSurrogate {}).done());
                    // emit U+FEFF
                    f(0xeb);
                    f(0xbb);
                    f(0xbf);
                }
            }

            // low surrogate (invalid at this position)
            0xdc00...0xdfff => {
                try!(self.report.error(lastpos..self.pos(), m::BadSurrogate {}).done());
                // emit U+FEFF
                f(0xeb);
                f(0xbb);
                f(0xbf);
            }

            0x0000...0x007f => {
                f(c as u8);
            }

            0x0080...0x07ff => {
                f(0b1100_0000 | (c >> 6 & 0x1f) as u8);
                f(0b1000_0000 | (c      & 0x3f) as u8);
            }

            _ => {
                f(0b1110_0000 | (c >> 12 & 0x0f) as u8);
                f(0b1000_0000 | (c >>  6 & 0x3f) as u8);
                f(0b1000_0000 | (c       & 0x3f) as u8);
            }
        }

        Ok(())
    }

    fn count_equals(&mut self) -> i32 {
        let mut v = 0;
        self.scan_while(|c| c == U8(b'='), |_| v += 1);
        v
    }

    // assumes that the first `[` is already read and
    // the next character in the lookahead is either `=` or `[`.
    //
    // returns true only if the long bracket was successfully scanned.
    // unclosed_open diag can be set to None to indicate that this is not an error condition.
    fn scan_long_bracket<F>(&mut self, begin: Pos, mut f: F,
                            unclosed_open: Option<&Localize>,
                            premature_eof: &Localize,
                            long_bracket_start: &Localize,
                            no_newline_in_meta: &Localize) -> diag::Result<bool>
            where F: FnMut(u8) {
        let opening_level = self.count_equals();
        match self.read() {
            U8(b'[') => {}
            c => {
                self.unread(c);
                if let Some(unclosed_open) = unclosed_open {
                    try!(self.report.error(begin..self.pos(), unclosed_open).done());
                }
                return Ok(false);
            }
        }
        loop {
            let lastpos = self.pos();
            match self.read() {
                U8(b']') => {
                    let closing_level = self.count_equals();
                    match self.read() {
                        U8(b']') if opening_level == closing_level => break,
                        c @ U8(_) | c @ U16(_) => {
                            // reconstruct previously read bytes
                            f(b']');
                            for _ in 0..closing_level { f(b'='); }
                            self.unread(c); // may be the start of closing bracket
                        },
                        EOF => {
                            try!(self.report.error(self.pos(), premature_eof)
                                            .note(begin, long_bracket_start)
                                            .done());
                            return Ok(false);
                        }
                    }
                },
                c @ U8(b'\r') | c @ U8(b'\n') if self.meta => {
                    // the meta block should be closed later, so we need to unread
                    self.unread(c);

                    // do not include newlines in the report span, however
                    try!(self.report.error(begin..lastpos, no_newline_in_meta)
                                    .note(self.meta_span, m::MetaStart {})
                                    .done());
                    return Ok(false);
                },
                U8(c) => f(c),
                U16(c) => try!(self.translate_u16(lastpos, c, &mut f)),
                EOF => {
                    try!(self.report.error(self.pos(), premature_eof)
                                    .note(begin, long_bracket_start)
                                    .done());
                    return Ok(false);
                }
            }
        }
        Ok(true)
    }

    // assumes that the first quote is already read
    fn scan_quoted_string<F>(&mut self, begin: Pos, quote: u8, mut f: F) -> diag::Result<()>
            where F: FnMut(u8) {
        loop {
            let lastpos = self.pos();
            match self.read() {
                U8(b'\\') => match self.read() {
                    U8(b'a')  => f(b'\x07'),
                    U8(b'b')  => f(b'\x08'),
                    U8(b'f')  => f(b'\x0c'),
                    U8(b'n')  => f(b'\n'),
                    U8(b'r')  => f(b'\r'),
                    U8(b't')  => f(b'\t'),
                    U8(b'v')  => f(b'\x0b'),
                    U8(b'\\') => f(b'\\'),
                    U8(b'\'') => f(b'\''),
                    U8(b'"')  => f(b'"'),
                    U8(b'\r') | U8(b'\n') => f(b'\n'),
                    U8(c) if c == quote => {
                        f(c) // to account for `foo\`foo` in the Kailua block
                    },
                    U8(d @ b'0'...b'9') => { // up to three digits
                        let mut n = d - b'0';
                        if let Some(d) = self.try(is_digit) {
                            n = n * 10 + (d.u8() - b'0');
                            if let Some(d) = self.try(is_digit) {
                                let n_ = n as u32 * 10 + (d.u8() - b'0') as u32;
                                if n_ < 256 {
                                    n = n_ as u8;
                                } else {
                                    self.unread(d);
                                }
                            }
                        }
                        f(n)
                    },
                    U8(_) | U16(_) => {
                        try!(self.report.error(lastpos..self.pos(),
                                               m::UnrecognizedEscapeInString {})
                                        .done());
                        // skip this character
                    },
                    EOF => {
                        self.unread(EOF); // should translate to Tok::EOF in the caller
                        try!(self.report.error(self.pos(), m::PrematureEofInString {})
                                        .note(begin, m::StringStart {})
                                        .done());
                        break;
                    },
                },
                U8(b'\r') | U8(b'\n') => {
                    try!(self.report.error(self.pos(), m::UnescapedNewlineInString {})
                                    .note(begin, m::StringStart {})
                                    .done());
                    // return without adding a newline, it's more likely that the quote is missing
                    break;
                },
                U8(c) if c == quote => break,
                U8(c) => f(c),
                U16(c) => try!(self.translate_u16(lastpos, c, &mut f)),
                EOF => {
                    self.unread(EOF); // should translate to Tok::EOF in the caller
                    try!(self.report.error(self.pos(), m::PrematureEofInString {})
                                    .note(begin, m::StringStart {})
                                    .done());
                    break;
                },
            }
        }
        Ok(())
    }

    pub fn next_token(&mut self) -> diag::Result<Option<Spanned<Tok>>> {
        loop {
            // skip any whitespace
            if self.meta {
                // need to check for newline in the meta block
                self.scan_while(|c| c == U8(b' ') || c == U8(b'\t'), |_| {});
            } else {
                self.scan_while(|c| c == U8(b' ') || c == U8(b'\t') ||
                                    c == U8(b'\r') || c == U8(b'\n'),
                                |_| {});
            }

            let begin = self.pos();

            macro_rules! tok {
                (@token Comment)          => (Tok::Comment);
                (@token Keyword($e:expr)) => (Tok::Keyword($e));
                (@token Name($e:expr))    => (Tok::Name($e));
                (@token Num($e:expr))     => (Tok::Num($e));
                (@token Str($e:expr))     => (Tok::Str($e));
                (@token $i:ident)         => (Tok::Punct(Punct::$i));

                (meta: $($t:tt)*) => ({
                    let span = Span::new(begin, self.pos());
                    self.meta = true;
                    self.meta_span = span;
                    Ok(Some(tok!(@token $($t)*).with_loc(span)))
                });
                ($($t:tt)*) => (
                    Ok(Some(tok!(@token $($t)*).with_loc(Span::new(begin, self.pos()))))
                );
            }

            match self.read() {
                // names
                U8(c @ b'A'...b'Z') | U8(c @ b'a'...b'z') | U8(c @ b'_') => {
                    let mut name = vec![c];
                    self.scan_while(
                        |c| match c {
                            U8(b'A'...b'Z') | U8(b'a'...b'z') | U8(b'0'...b'9') | U8(b'_') => true,
                            _ => false,
                        },
                        |c| name.push(c.u8()));

                    if let Some(keyword) = Keyword::from(&name, self.meta) {
                        return tok!(Keyword(keyword));
                    } else {
                        return tok!(Name(name));
                    }
                }

                // numbers
                U8(c @ b'0'...b'9') => {
                    if c == b'0' && self.try(|c| c == U8(b'x')).is_some() {
                        // hexadecimal
                        let mut num = Vec::new();
                        self.scan_while(
                            |c| match c {
                                U8(b'A'...b'F') | U8(b'a'...b'f') | U8(b'0'...b'9') => true,
                                _ => false,
                            },
                            |c| num.push(c.u8()));

                        let s = str::from_utf8(&num).unwrap();
                        if s.len() <= 16 {
                            let v = u64::from_str_radix(s, 16).unwrap();
                            return tok!(Num(v as f64));
                        } else {
                            // uh, this is possible when `0x` is followed by 17+ hex digits.
                            // it is still a valid number however,
                            // so we take the initial 16 digits (64 bits) and scale accordingly.
                            // it should be noted that, while 64 bits are enough to fit to
                            // f64's mantissa, it takes ALL digits to correctly round that.
                            // we don't seriously use such numbers in the checker though,
                            // so we won't care.
                            let v = u64::from_str_radix(&s[..16], 16).unwrap();
                            let shift = 4 * (s.len() - 16);
                            return tok!(Num(v as f64 * (shift as f64).exp2()));
                        }
                    } else {
                        let mut num = vec![c];
                        self.scan_while(is_digit, |c| num.push(c.u8()));
                        if let Some(c) = self.try(|c| c == U8(b'.')) {
                            num.push(c.u8());
                            self.scan_while(is_digit, |c| num.push(c.u8()));
                        }
                        if let Some(c) = self.try(|c| c == U8(b'e') || c == U8(b'E')) {
                            num.push(c.u8());
                            if let Some(c) = self.try(|c| c == U8(b'-')) {
                                num.push(c.u8());
                            }
                            self.scan_while(is_digit, |c| num.push(c.u8()));
                        }

                        if let Ok(s) = str::from_utf8(&num) {
                            if let Ok(v) = s.parse::<f64>() {
                                return tok!(Num(v));
                            }
                        }

                        try!(self.report.error(begin..self.pos(), m::InvalidNumber {}).done());
                        // continue reading other tokens
                    }
                }

                // strings
                U8(q @ b'\'') | U8(q @ b'"') => {
                    let mut s = Vec::new();
                    try!(self.scan_quoted_string(begin, q, |c| s.push(c)));
                    return tok!(Str(s));
                }

                U8(b'[') => {
                    let c = self.read();
                    self.unread(c);
                    if c == U8(b'=') || c == U8(b'[') {
                        let mut s = Vec::new();
                        try!(self.scan_long_bracket(
                                begin, |c| s.push(c),
                                Some(&m::UnclosedOpeningLongString {}),
                                &m::PrematureEofInLongString {},
                                &m::LongStringStart {},
                                &m::NoNewlineInLongStringInMeta {}));
                        return tok!(Str(s));
                    }
                    return tok!(LBracket);
                }

                U8(b'-') => match self.read() {
                    U8(b'-') => {
                        match self.read() {
                            U8(b'[') => {
                                if let Some(c) = self.try(|c| c == U8(b'[') || c == U8(b'=')) {
                                    // long comment
                                    self.unread(c);
                                    let was_long = try!(self.scan_long_bracket(
                                            begin, |_| {},
                                            None,
                                            &m::PrematureEofInLongComment {},
                                            &m::LongCommentStart {},
                                            &m::NoNewlineInLongCommentInMeta {}));
                                    if !was_long {
                                        // this turned out to be just a simple short comment
                                        self.scan_while(|c| c != U8(b'\r') && c != U8(b'\n'),
                                                        |_| {});
                                    }
                                    return tok!(Comment);
                                }
                            }

                            // Kailua extensions
                            // meta comment inside meta comment is tokenized but does not nest 
                            // and thus is going to cause a parser error (intentional).
                            U8(b'#') => return tok!(meta: DashDashHash),
                            U8(b':') => return tok!(meta: DashDashColon),
                            U8(b'>') => return tok!(meta: DashDashGt),
                            U8(b'v') => return tok!(meta: DashDashV),

                            c @ U8(_) | c @ U16(_) => { self.unread(c); }
                            EOF => {}
                        }

                        // short comment
                        self.scan_while(|c| c != U8(b'\r') && c != U8(b'\n'), |_| {});
                        // do NOT read an excess newline, may be the end of meta block
                        return tok!(Comment);
                    }

                    c @ U8(_) | c @ U16(_) => { self.unread(c); return tok!(Dash); }
                    EOF => { return tok!(Dash); }
                },

                U8(b'+') => return tok!(Plus),
                U8(b'*') => return tok!(Star),
                U8(b'/') => return tok!(Slash),
                U8(b'%') => return tok!(Percent),
                U8(b'^') => return tok!(Caret),
                U8(b'#') => return tok!(Hash),
                U8(b'=') => {
                    if let Some(_) = self.try(|c| c == U8(b'=')) { return tok!(EqEq); }
                    return tok!(Eq);
                },
                U8(b'~') => {
                    if let Some(_) = self.try(|c| c == U8(b'=')) { return tok!(TildeEq); }
                    try!(self.report.error(begin..self.pos(), m::UnexpectedChar {}).done());
                    // continue reading other tokens
                },
                U8(b'<') => {
                    if let Some(_) = self.try(|c| c == U8(b'=')) { return tok!(LtEq); }
                    return tok!(Lt);
                },
                U8(b'>') => {
                    if let Some(_) = self.try(|c| c == U8(b'=')) { return tok!(GtEq); }
                    return tok!(Gt);
                },
                U8(b'(') => return tok!(LParen),
                U8(b')') => return tok!(RParen),
                U8(b'{') => return tok!(LBrace),
                U8(b'}') => return tok!(RBrace),
                U8(b']') => return tok!(RBracket),
                U8(b';') => return tok!(Semicolon),
                U8(b':') => return tok!(Colon),
                U8(b',') => return tok!(Comma),
                U8(b'.') => {
                    if let Some(_) = self.try(|c| c == U8(b'.')) {
                        if let Some(_) = self.try(|c| c == U8(b'.')) { return tok!(DotDotDot); }
                        return tok!(DotDot);
                    }
                    return tok!(Dot);
                },

                // Kailua extensions
                U8(q @ b'`') if self.meta => {
                    let mut s = Vec::new();
                    try!(self.scan_quoted_string(begin, q, |c| s.push(c)));
                    return tok!(Name(s));
                }
                U8(b'\r') | U8(b'\n') if self.meta => {
                    self.meta = false;
                    return tok!(Newline);
                },
                U8(b'?') if self.meta => return tok!(Ques),
                U8(b'!') if self.meta => return tok!(Bang),
                U8(b'|') if self.meta => return tok!(Pipe),

                U8(_) | U16(_) => {
                    try!(self.report.error(begin..self.pos(), m::UnexpectedChar {}).done());
                },
                EOF => {
                    if self.meta { // the last line should be closed by the (dummy) Newline token
                        self.meta = false;
                        return tok!(Newline);
                    } else if !self.eof {
                        self.eof = true;
                        return Ok(Some(Tok::EOF.with_loc(self.pos())));
                    } else {
                        return Ok(None);
                    }
                },
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Spanned<Tok>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(tok) => tok,
            Err(_) => Some(Tok::Error.with_loc(self.pos())),
        }
    }
}

