//! Lexical analysis.

use std::str;
use std::fmt;

use kailua_diag::{Locale, Localize, Localized};
use string::{Name, Str};

/// A token.
#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    /// A token which is distinct from all other tokens.
    ///
    /// The lexer emits this token on an error.
    Error,

    /// A comment token. The parser should ignore this.
    ///
    /// The shebang line (the first line starting with `#`) is also considered as a comment.
    Comment,

    /// A punctuation.
    Punct(Punct),

    /// A keyword.
    Keyword(Keyword),

    /// A number.
    Num(f64),

    /// A name (either an identifier or a quoted name in the meta block).
    Name(Name),

    /// A string (either `"string"` or `[[string]]`).
    Str(Str),

    /// The end of file.
    ///
    /// A valid stream of tokens is expected to have only one EOF token at the end.
    EOF,
}

impl Localize for Tok {
    fn fmt_localized(&self, f: &mut fmt::Formatter, locale: Locale) -> fmt::Result {
        match (&locale[..], self) {
            ("ko", &Tok::Error)      => write!(f, "잘못된 문자"),
            (_,    &Tok::Error)      => write!(f, "an invalid character"),
            ("ko", &Tok::Comment)    => write!(f, "주석"),
            (_,    &Tok::Comment)    => write!(f, "a comment"),
            (_,    &Tok::Punct(p))   => write!(f, "{}", Localized::new(&p, locale)),
            (_,    &Tok::Keyword(w)) => write!(f, "{}", Localized::new(&w, locale)),
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
    fn fmt_localized(&self, f: &mut fmt::Formatter, locale: Locale) -> fmt::Result {
        (**self).fmt_localized(f, locale)
    }
}

macro_rules! define_puncts {
    ($ty:ident |$locale:ident|: $($i:ident $t:expr, #[$m:meta])*) => (
        /// A punctuation.
        ///
        /// This includes Kailua-specific punctuations,
        /// which are only generated in the meta block (marked as [M] below).
        /// Some of them are also only generated after a particular Lua version
        /// (marked as [5.x+] below).
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        pub enum $ty { $(#[$m] $i,)* }

        impl Localize for $ty {
            fn fmt_localized(&self, f: &mut fmt::Formatter, $locale: Locale) -> fmt::Result {
                let text = match *self { $($ty::$i => $t,)* };
                fmt::Display::fmt(text, f)
            }
        }
    );
}

define_puncts! { Punct |locale|:
    Plus        "`+`",      /// `+`.
    Dash        "`-`",      /// `-`.
    Star        "`*`",      /// `*`.
    Slash       "`/`",      /// `/`.
    Percent     "`%`",      /// `%`.
    Caret       "`^`",      /// `^`.
    Hash        "`#`",      /// `#`.
    EqEq        "`==`",     /// `==`.
    TildeEq     "`~=`",     /// `~=`.
    LtEq        "`<=`",     /// `<=`.
    GtEq        "`>=`",     /// `>=`.
    Lt          "`<`",      /// `<`.
    Gt          "`>`",      /// `>`.
    Eq          "`=`",      /// `=`.
    Amp         "`&`",      /// `&`. [5.3+]
    Tilde       "`~`",      /// `~`. [5.3+]
    Pipe        "`|`",      /// `|`. [5.3+ or M]
    LtLt        "`<<`",     /// `<<`. [5.3+]
    GtGt        "`>>`",     /// `>>`. [5.3+]
    SlashSlash  "`//`",     /// `//`. [5.3+]
    LParen      "`(`",      /// `(`.
    RParen      "`)`",      /// `)`.
    LBrace      "`{`",      /// `{`.
    RBrace      "`}`",      /// `}`.
    LBracket    "`[`",      /// `[`.
    RBracket    "`]`",      /// `]`.
    Semicolon   "`;`",      /// `;`.
    Colon       "`:`",      /// `:`.
    ColonColon  "`::`",     /// `::`. [5.2+]
    Comma       "`,`",      /// `,`.
    Dot         "`.`",      /// `.`.
    DotDot      "`..`",     /// `..`.
    DotDotDot   "`...`",    /// `...`.

    // Kailua extensions
    DashDashHash    "`--#`",    /// `--#`. [M]
    DashDashV       "`--v`",    /// `--v`. [M]
    DashDashColon   "`--:`",    /// `--:`. [M]
    DashDashGt      "`-->`",    /// `-->`. [M]
    Ques            "`?`",      /// `?`. [M]
    Bang            "`!`",      /// `!`. [M]
    Newline         match &locale[..] { "ko" => "개행문자", _ => "a newline" },
                    /// A newline. Only generated at the end of the meta block.
}

macro_rules! define_keywords {
    ($ty:ident: everywhere { $($i:ident $t:expr, #[$m:meta])* }
                meta_only { $($mi:ident $mt:expr, #[$mm:meta])* }) => (
        /// A keyword.
        ///
        /// This includes Kailua-specific keywords,
        /// which are only generated in the meta block (marked as [M] below).
        /// Some of them are also only generated after a particular Lua version
        /// (marked as [5.x+] below).
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        pub enum $ty { $(#[$m] $i,)* $(#[$mm] $mi,)* }

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
    );
}

define_keywords! { Keyword:
    everywhere {
        And         b"and",         /// `and`.
        Break       b"break",       /// `break`.
        Do          b"do",          /// `do`.
        Else        b"else",        /// `else`.
        Elseif      b"elseif",      /// `elseif`.
        End         b"end",         /// `end`.
        False       b"false",       /// `false`.
        For         b"for",         /// `for`.
        Function    b"function",    /// `function`.
        Goto        b"goto",        /// `goto`. [5.2+; a normal identifier in Lua 5.1]
        If          b"if",          /// `if`.
        In          b"in",          /// `in`.
        Local       b"local",       /// `local`.
        Nil         b"nil",         /// `nil`.
        Not         b"not",         /// `not`.
        Or          b"or",          /// `or`.
        Repeat      b"repeat",      /// `repeat`.
        Return      b"return",      /// `return`.
        Then        b"then",        /// `then`.
        True        b"true",        /// `true`.
        Until       b"until",       /// `until`.
        While       b"while",       /// `while`.
    }

    meta_only { // Kailua extensions
        Assume      b"assume",      /// `assume`. [M]
        Class       b"class",       /// `class`. [M]
        Const       b"const",       /// `const`. [M]
        Global      b"global",      /// `global`. [M]
        Map         b"map",         /// `map`. [M]
        Method      b"method",      /// `method`. [M]
        Module      b"module",      /// `module`. [M]
        Once        b"once",        /// `once`. [M]
        Open        b"open",        /// `open`. [M]
        Static      b"static",      /// `static`. [M]
        Type        b"type",        /// `type`. [M]
        Var         b"var",         /// `var`. [M]
        Vector      b"vector",      /// `vector`. [M]
    }
}

impl From<Keyword> for Str {
    fn from(kw: Keyword) -> Str {
        kw.name().into()
    }
}

impl From<Keyword> for Name {
    fn from(kw: Keyword) -> Name {
        kw.name().into()
    }
}

impl Localize for Keyword {
    fn fmt_localized(&self, f: &mut fmt::Formatter, locale: Locale) -> fmt::Result {
        let name = str::from_utf8(self.name()).unwrap();
        match &locale[..] {
            "ko" => write!(f, "예약어 `{}`", name),
            _ => write!(f, "a keyword `{}`", name),
        }
    }
}

mod lexer;
mod nesting;

pub use self::lexer::Lexer;
pub use self::nesting::{Nest, NestedToken, NestingCategory, NestingSerial};

