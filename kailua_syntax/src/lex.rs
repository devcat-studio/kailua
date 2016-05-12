use std::str;
use std::u64;
use std::fmt;

use kailua_diag::{SourceBytes, Pos, Span, Spanned, WithLoc, Report};

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Error, // dummy token
    Punct(Punct),
    Keyword(Keyword),
    Num(f64),
    Name(Vec<u8>),
    Str(Vec<u8>),
    EOF, // only used in the parser
}

macro_rules! tt_to_expr { ($e:expr) => ($e) }
macro_rules! define_tokens {
    ($ty:ident: $($t:tt $i:ident,)*) => (
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        pub enum $ty { $($i,)* }
        impl fmt::Display for $ty {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let text = match *self { $($ty::$i => tt_to_expr!($t),)* };
                fmt::Display::fmt(text, f)
            }
        }
    );
}

define_tokens! { Punct:
    "`+`"   Plus,
    "`-`"   Dash,
    "`*`"   Star,
    "`/`"   Slash,
    "`%`"   Percent,
    "`^`"   Caret,
    "`#`"   Hash,
    "`==`"  EqEq,
    "`~=`"  TildeEq,
    "`<=`"  LtEq,
    "`>=`"  GtEq,
    "`<`"   Lt,
    "`>`"   Gt,
    "`=`"   Eq,
    "`(`"   LParen,
    "`)`"   RParen,
    "`{`"   LBrace,
    "`}`"   RBrace,
    "`[`"   LBracket,
    "`]`"   RBracket,
    "`;`"   Semicolon,
    "`:`"   Colon,
    "`,`"   Comma,
    "`.`"   Dot,
    "`..`"  DotDot,
    "`...`" DotDotDot,

    // Kailua extensions
    "`--#`" DashDashHash,
    "`--v`" DashDashV,
    "`--:`" DashDashColon,
    "`-->`" DashDashGt,
    "`?`"   Ques,
    "`|`"   Pipe,
    "`&`"   Amp,
    "`->`"  DashGt,
    "a newline" Newline,
}

define_tokens! { Keyword:
    "a keyword `and`"       And,
    "a keyword `break`"     Break,
    "a keyword `do`"        Do,
    "a keyword `else`"      Else,
    "a keyword `elseif`"    Elseif,
    "a keyword `end`"       End,
    "a keyword `false`"     False,
    "a keyword `for`"       For,
    "a keyword `function`"  Function,
    "a keyword `if`"        If,
    "a keyword `in`"        In,
    "a keyword `local`"     Local,
    "a keyword `nil`"       Nil,
    "a keyword `not`"       Not,
    "a keyword `or`"        Or,
    "a keyword `repeat`"    Repeat,
    "a keyword `return`"    Return,
    "a keyword `then`"      Then,
    "a keyword `true`"      True,
    "a keyword `until`"     Until,
    "a keyword `while`"     While,

    // Kailua extensions
    "a keyword `assume`"    Assume,
    "a keyword `const`"     Const,
    "a keyword `global`"    Global,
    "a keyword `module`"    Module,
    "a keyword `once`"      Once,
    "a keyword `open`"      Open,
    "a keyword `var`"       Var,
}

impl Keyword {
    pub fn from(s: &[u8], allow_meta: bool) -> Option<Keyword> {
        match (allow_meta, s) {
            (_, b"and")      => Some(Keyword::And),
            (_, b"break")    => Some(Keyword::Break),
            (_, b"do")       => Some(Keyword::Do),
            (_, b"else")     => Some(Keyword::Else),
            (_, b"elseif")   => Some(Keyword::Elseif),
            (_, b"end")      => Some(Keyword::End),
            (_, b"false")    => Some(Keyword::False),
            (_, b"for")      => Some(Keyword::For),
            (_, b"function") => Some(Keyword::Function),
            (_, b"if")       => Some(Keyword::If),
            (_, b"in")       => Some(Keyword::In),
            (_, b"local")    => Some(Keyword::Local),
            (_, b"nil")      => Some(Keyword::Nil),
            (_, b"not")      => Some(Keyword::Not),
            (_, b"or")       => Some(Keyword::Or),
            (_, b"repeat")   => Some(Keyword::Repeat),
            (_, b"return")   => Some(Keyword::Return),
            (_, b"then")     => Some(Keyword::Then),
            (_, b"true")     => Some(Keyword::True),
            (_, b"until")    => Some(Keyword::Until),
            (_, b"while")    => Some(Keyword::While),

            (true, b"assume") => Some(Keyword::Assume),
            (true, b"const")  => Some(Keyword::Const),
            (true, b"global") => Some(Keyword::Global),
            (true, b"module") => Some(Keyword::Module),
            (true, b"once")   => Some(Keyword::Once),
            (true, b"open")   => Some(Keyword::Open),
            (true, b"var")    => Some(Keyword::Var),

            (_, _) => None,
        }
    }
}

pub struct Lexer<'a, R> {
    bytes: SourceBytes<'a>,
    last_pos: Pos,
    last_byte: u8,
    lookahead: bool,
    meta: bool,
    report: R,
}

pub type Error = &'static str;

fn is_digit(c: u8) -> bool { b'0' <= c && c <= b'9' }

impl<'a, R: Report<Error=Error>> Lexer<'a, R> {
    pub fn new(bytes: SourceBytes<'a>, report: R) -> Lexer<'a, R> {
        Lexer {
            bytes: bytes,
            last_pos: Pos::dummy(),
            last_byte: b'\0',
            lookahead: false,
            meta: false,
            report: report,
        }
    }

    fn pos(&self) -> Pos {
        if self.lookahead { self.last_pos } else { self.bytes.pos() }
    }

    fn read(&mut self) -> Option<u8> {
        if self.lookahead {
            self.lookahead = false;
            Some(self.last_byte)
        } else {
            self.last_pos = self.bytes.pos();
            if let Some(c) = self.bytes.next() {
                self.last_byte = c;
                Some(c)
            } else {
                None
            }
        }
    }

    fn unread(&mut self, last: u8) {
        assert!(!self.lookahead, "only one lookahead byte is supported");
        assert!(!self.last_pos.is_dummy());
        assert_eq!(self.last_byte, last);
        self.lookahead = true;
    }

    fn try<Cond>(&mut self, mut cond: Cond) -> Option<u8>
            where Cond: FnMut(u8) -> bool {
        if let Some(c) = self.read() {
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
            where Cond: FnMut(u8) -> bool, F: FnMut(u8) {
        while let Some(c) = self.read() {
            if !cond(c) {
                self.unread(c);
                break;
            }
            f(c);
        }
    }

    fn count_equals(&mut self) -> i32 {
        let mut v = 0;
        self.scan_while(|c| c == b'=', |_| v += 1);
        v
    }

    // assumes that the first `[` is already read and
    // the next character in the lookahead is either `=` or `[`.
    fn scan_long_bracket<F>(&mut self, begin: Pos, mut f: F) -> Result<(), R::Error>
            where F: FnMut(u8) {
        let opening_level = self.count_equals();
        match self.read() {
            Some(b'[') => {}
            Some(c) => {
                self.unread(c);
                return self.report.fatal(begin..self.pos(), "Unexpected start of long bracket");
            }
            None => {
                return Err("unexpected EOF in long bracket");
            }
        }
        loop {
            match self.read() {
                Some(b']') => {
                    let closing_level = self.count_equals();
                    match self.read() {
                        Some(b']') if opening_level == closing_level => break,
                        Some(c) => {
                            // reconstruct previously read bytes
                            f(b']');
                            for _ in 0..closing_level { f(b'='); }
                            self.unread(c); // may be the start of closing bracket
                        },
                        None => return Err("unexpected EOF in long bracket"),
                    }
                },
                Some(b'\r') | Some(b'\n') if self.meta => {
                    return Err("newline disallowed in long bracket inside metablock")
                },
                Some(c) => f(c),
                None => {
                    return self.report.error(begin..self.pos(), "Unexpected EOF in long bracket");
                }
            }
        }
        Ok(())
    }

    // assumes that the first quote is already read
    fn scan_quoted_string<F>(&mut self, quote: u8, mut f: F) -> Result<(), R::Error>
            where F: FnMut(u8) {
        loop {
            match self.read() {
                Some(b'\\') => match self.read() {
                    Some(b'a')  => f(b'\x07'),
                    Some(b'b')  => f(b'\x08'),
                    Some(b'f')  => f(b'\x0c'),
                    Some(b'n')  => f(b'\n'),
                    Some(b'r')  => f(b'\r'),
                    Some(b't')  => f(b'\t'),
                    Some(b'v')  => f(b'\x0b'),
                    Some(b'\\') => f(b'\\'),
                    Some(b'\'') => f(b'\''),
                    Some(b'"')  => f(b'"'),
                    Some(b'\n') => f(b'\n'),
                    Some(c) if c == quote => f(c), // to account for `foo\`foo` in the Kailua block
                    Some(d @ b'0'...b'9') => { // up to three digits
                        let mut n = d - b'0';
                        if let Some(d) = self.try(is_digit) {
                            n = n * 10 + (d - b'0');
                            if let Some(d) = self.try(is_digit) {
                                let n_ = n as u32 * 10 + (d - b'0') as u32;
                                if n_ < 256 {
                                    n = n_ as u8;
                                } else {
                                    self.unread(d);
                                }
                            }
                        }
                        f(n)
                    },
                    Some(_) => return Err("unexpected escape sequence in string"),
                    None => return Err("unexpected EOF in string"),
                },
                Some(c) if c == quote => break,
                Some(c) => f(c),
                None => return Err("unexpected EOF in string"),
            }
        }
        Ok(())
    }

    pub fn next_token(&mut self) -> Result<Option<Spanned<Tok>>, R::Error> {
        loop {
            // skip any whitespace
            if self.meta {
                // need to check for newline in the meta block
                self.scan_while(|c| c == b' ' || c == b'\t', |_| {});
            } else {
                self.scan_while(|c| c == b' ' || c == b'\t' || c == b'\r' || c == b'\n', |_| {});
            }

            let begin = self.pos();

            macro_rules! tok {
                (@token Keyword($e:expr)) => (Tok::Keyword($e));
                (@token Name($e:expr))    => (Tok::Name($e));
                (@token Num($e:expr))     => (Tok::Num($e));
                (@token Str($e:expr))     => (Tok::Str($e));
                (@token $i:ident)         => (Tok::Punct(Punct::$i));

                ($($t:tt)*) => (
                    Ok(Some(tok!(@token $($t)*).with_loc(Span::new(begin, self.pos()))))
                );
            }

            match self.read() {
                // names
                Some(c @ b'A'...b'Z') | Some(c @ b'a'...b'z') | Some(c @ b'_') => {
                    let mut name = vec![c];
                    self.scan_while(
                        |c| match c { b'A'...b'Z' | b'a'...b'z' | b'0'...b'9' | b'_' => true,
                                      _ => false },
                        |c| name.push(c));

                    if let Some(keyword) = Keyword::from(&name, self.meta) {
                        return tok!(Keyword(keyword));
                    } else {
                        return tok!(Name(name));
                    }
                }

                // numbers
                Some(c @ b'0'...b'9') => {
                    if c == b'0' && self.try(|c| c == b'x').is_some() {
                        // hexadecimal
                        let mut num = Vec::new();
                        self.scan_while(
                            |c| match c { b'A'...b'F' | b'a'...b'f' | b'0'...b'9' => true,
                                          _ => false },
                            |c| num.push(c));

                        if let Ok(s) = str::from_utf8(&num) {
                            if let Ok(v) = u64::from_str_radix(s, 16) {
                                return tok!(Num(v as f64));
                            }
                        }
                    } else {
                        let mut num = vec![c];
                        self.scan_while(is_digit, |c| num.push(c));
                        if let Some(c) = self.try(|c| c == b'.') {
                            num.push(c);
                            self.scan_while(is_digit, |c| num.push(c));
                        }
                        if let Some(c) = self.try(|c| c == b'e' || c == b'E') {
                            num.push(c);
                            if let Some(c) = self.try(|c| c == b'-') {
                                num.push(c);
                            }
                            self.scan_while(is_digit, |c| num.push(c));
                        }

                        if let Ok(s) = str::from_utf8(&num) {
                            if let Ok(v) = s.parse::<f64>() {
                                return tok!(Num(v));
                            }
                        }
                    }

                    return Err("invalid number");
                }

                // strings
                Some(q @ b'\'') | Some(q @ b'"') => {
                    let mut s = Vec::new();
                    try!(self.scan_quoted_string(q, |c| s.push(c)));
                    return tok!(Str(s));
                }

                Some(b'[') => {
                    if let Some(c) = self.read() {
                        self.unread(c);
                        if c == b'=' || c == b'[' {
                            let mut s = Vec::new();
                            try!(self.scan_long_bracket(begin, |c| s.push(c)));
                            return tok!(Str(s));
                        }
                    }
                    return tok!(LBracket);
                }

                Some(b'-') => match self.read() {
                    Some(b'-') => {
                        match self.read() {
                            Some(b'[') => {
                                if let Some(c) = self.try(|c| c == b'[' || c == b'=') {
                                    // long comment
                                    self.unread(c);
                                    try!(self.scan_long_bracket(begin, |_| {}));
                                    continue;
                                }
                            }

                            // Kailua extensions
                            // meta comment inside meta comment is tokenized but does not nest 
                            // and thus is going to cause a parser error (intentional).
                            Some(b'#') => { self.meta = true; return tok!(DashDashHash); }
                            Some(b':') => { self.meta = true; return tok!(DashDashColon); }
                            Some(b'>') => { self.meta = true; return tok!(DashDashGt); }
                            Some(b'v') => { self.meta = true; return tok!(DashDashV); }

                            Some(c) => { self.unread(c); }
                            None => {}
                        }

                        // short comment
                        self.scan_while(|c| c != b'\r' && c != b'\n', |_| {});
                        // do NOT read an excess newline, may be the end of meta block
                        continue;
                    }

                    // Kailua extensions
                    Some(b'>') if self.meta => return tok!(DashGt),

                    Some(c) => { self.unread(c); return tok!(Dash); }
                    None => { return tok!(Dash); }
                },

                Some(b'+') => return tok!(Plus),
                Some(b'*') => return tok!(Star),
                Some(b'/') => return tok!(Slash),
                Some(b'%') => return tok!(Percent),
                Some(b'^') => return tok!(Caret),
                Some(b'#') => return tok!(Hash),
                Some(b'=') => {
                    if let Some(_) = self.try(|c| c == b'=') { return tok!(EqEq); }
                    return tok!(Eq);
                },
                Some(b'~') => {
                    if let Some(_) = self.try(|c| c == b'=') { return tok!(TildeEq); }
                    return Err("unexpected character");
                },
                Some(b'<') => {
                    if let Some(_) = self.try(|c| c == b'=') { return tok!(LtEq); }
                    return tok!(Lt);
                },
                Some(b'>') => {
                    if let Some(_) = self.try(|c| c == b'=') { return tok!(GtEq); }
                    return tok!(Gt);
                },
                Some(b'(') => return tok!(LParen),
                Some(b')') => return tok!(RParen),
                Some(b'{') => return tok!(LBrace),
                Some(b'}') => return tok!(RBrace),
                Some(b']') => return tok!(RBracket),
                Some(b';') => return tok!(Semicolon),
                Some(b':') => return tok!(Colon),
                Some(b',') => return tok!(Comma),
                Some(b'.') => {
                    if let Some(_) = self.try(|c| c == b'.') {
                        if let Some(_) = self.try(|c| c == b'.') { return tok!(DotDotDot); }
                        return tok!(DotDot);
                    }
                    return tok!(Dot);
                },

                // Kailua extensions
                Some(q @ b'`') if self.meta => {
                    let mut s = Vec::new();
                    try!(self.scan_quoted_string(q, |c| s.push(c)));
                    return tok!(Name(s));
                }
                Some(b'\r') | Some(b'\n') if self.meta => {
                    self.meta = false;
                    return tok!(Newline);
                },
                Some(b'?') if self.meta => return tok!(Ques),
                Some(b'|') if self.meta => return tok!(Pipe),
                Some(b'&') if self.meta => return tok!(Amp),

                Some(_) => return Err("unexpected character"),
                None => {
                    if self.meta { // the last line should be closed by the (dummy) Newline token
                        self.meta = false;
                        return tok!(Newline);
                    } else {
                        return Ok(None)
                    }
                },
            }
        }
    }
}

impl<'a, R: Report<Error=Error>> Iterator for Lexer<'a, R> {
    type Item = Spanned<Tok>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Some(tok)) => Some(tok),
            Ok(None) => None,
            Err(_) => Some(Tok::Error.with_loc(Span::dummy())),
        }
    }
}

