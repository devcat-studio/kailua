use std::mem;
use std::ptr;
use std::panic::{self, AssertUnwindSafe};
use source::VSSource;
use report::VSReport;
use kailua_diag::{Span, Spanned, Report};
use kailua_syntax::{Tok, Punct, Keyword, Lexer};

macro_rules! define_token_type {
    (
        const { $($ck:ident = $cv:expr,)* }
        data { $($dk:ident = $dv:expr,)* }
        punct { $($pk:ident = $pv:expr,)* }
        keyword { $($kk:ident = $kv:expr,)* }
    ) => (
        #[repr(u8)]
        #[derive(Copy, Clone, Debug, PartialEq)]
        pub enum VSTokenType {
            Dead = 0xff,
            $($ck = $cv,)*
            $($dk = $dv,)*
            $($pk = $pv,)*
            $($kk = $kv,)*
        }

        impl VSTokenType {
            pub fn from(tok: &Tok) -> VSTokenType {
                match *tok {
                    $(Tok::$ck => VSTokenType::$ck,)*
                    $(Tok::$dk(..) => VSTokenType::$dk,)*
                    $(Tok::Punct(Punct::$pk) => VSTokenType::$pk,)*
                    $(Tok::Keyword(Keyword::$kk) => VSTokenType::$kk,)*
                }
            }
        }
    )
}

define_token_type! {
    const {
        Error = 0x00,
        Comment = 0x01,
        EOF = 0x07,
    }

    data {
        Num = 0x04,
        Name = 0x05,
        Str = 0x06,
    }

    punct {
        Plus            = 0x40,
        Dash            = 0x41,
        Star            = 0x42,
        Slash           = 0x43,
        Percent         = 0x44,
        Caret           = 0x45,
        Hash            = 0x46,
        EqEq            = 0x47,
        TildeEq         = 0x48,
        LtEq            = 0x49,
        GtEq            = 0x4a,
        Lt              = 0x4b,
        Gt              = 0x4c,
        Eq              = 0x4d,
        LParen          = 0x4e,
        RParen          = 0x4f,
        LBrace          = 0x50,
        RBrace          = 0x51,
        LBracket        = 0x52,
        RBracket        = 0x53,
        Semicolon       = 0x54,
        Colon           = 0x55,
        Comma           = 0x56,
        Dot             = 0x57,
        DotDot          = 0x58,
        DotDotDot       = 0x59,
        DashDashHash    = 0x5a,
        DashDashV       = 0x5b,
        DashDashColon   = 0x5c,
        DashDashGt      = 0x5d,
        Ques            = 0x5e,
        Pipe            = 0x5f,
        Amp             = 0x60,
        DashGt          = 0x61,
        Newline         = 0x3f, // technically NOT a punctuation
    }

    keyword {
        And         = 0x80,
        Break       = 0x81,
        Do          = 0x82,
        Else        = 0x83,
        Elseif      = 0x84,
        End         = 0x85,
        False       = 0x86,
        For         = 0x87,
        Function    = 0x88,
        If          = 0x89,
        In          = 0x8a,
        Local       = 0x8b,
        Nil         = 0x8c,
        Not         = 0x8d,
        Or          = 0x8e,
        Repeat      = 0x8f,
        Return      = 0x90,
        Then        = 0x91,
        True        = 0x92,
        Until       = 0x93,
        While       = 0x94,
        Assume      = 0x95,
        Const       = 0x96,
        Global      = 0x97,
        Module      = 0x98,
        Once        = 0x99,
        Open        = 0x9a,
        Type        = 0x9b,
        Var         = 0x9c,
    }
}

pub struct VSTokenStream {
    tokens: Vec<Spanned<Tok>>,
    cursor: usize,
}

impl VSTokenStream {
    pub fn new(source: &VSSource, span: Span, report: &Report) -> Option<Box<VSTokenStream>> {
        let source = source.source().lock().unwrap();
        if let Some(iter) = source.iter_from_span(span) {
            let lexer = Lexer::new(iter, report);
            let tokens: Vec<_> = lexer.collect();
            assert!(!tokens.is_empty());
            Some(Box::new(VSTokenStream { tokens: tokens, cursor: 0 }))
        } else {
            None
        }
    }

    pub fn next(&mut self, span: &mut Span) -> VSTokenType {
        let token = &self.tokens[self.cursor];
        if self.cursor + 1 < self.tokens.len() {
            self.cursor += 1;
        }
        *span = token.span;
        VSTokenType::from(&token.base)
    }
}

#[no_mangle]
pub extern "C" fn kailua_token_stream_new(src: *const VSSource,
                                          span: *const Span,
                                          report: *const VSReport) -> *mut VSTokenStream {
    if src.is_null() { return ptr::null_mut(); }
    if span.is_null() { return ptr::null_mut(); }
    if report.is_null() { return ptr::null_mut(); }

    let src: &VSSource = unsafe { mem::transmute(src) };
    let span = unsafe { *span };
    let report: &VSReport = unsafe { mem::transmute(report) };

    let src = AssertUnwindSafe(src);
    let span = AssertUnwindSafe(span);
    let report = AssertUnwindSafe(report);
    panic::catch_unwind(move || {
        let stream = VSTokenStream::new(&src, span.0, &report.proxy());
        unsafe { mem::transmute(stream) }
    }).unwrap_or(ptr::null_mut())
}

#[no_mangle]
pub extern "C" fn kailua_token_stream_next(stream: *mut VSTokenStream,
                                           span: *mut Span) -> VSTokenType {
    if stream.is_null() { return VSTokenType::Dead; }
    if span.is_null() { return VSTokenType::Dead; }

    let stream: &mut VSTokenStream = unsafe { mem::transmute(stream) };
    let span = unsafe { span.as_mut().unwrap() };

    let mut stream = AssertUnwindSafe(stream);
    let span = AssertUnwindSafe(span);
    panic::catch_unwind(move || {
        stream.next(span.0)
    }).unwrap_or(VSTokenType::Dead)
}

#[no_mangle]
pub extern "C" fn kailua_token_stream_free(stream: *mut VSTokenStream) {
    if stream.is_null() { return; }
    let stream: Box<VSTokenStream> = unsafe { mem::transmute(stream) };

    let stream = AssertUnwindSafe(stream); // XXX use Unique when it is stabilized
    let _ = panic::catch_unwind(move || {
        drop(stream);
    }); // cannot do much beyond this
}

