use std::mem;
use std::ptr;
use std::sync::Arc;
use std::panic::{self, AssertUnwindSafe};
use source::VSSource;
use report::VSReport;
use kailua_env::Span;
use kailua_diag::Report;
use kailua_syntax::{Tok, Punct, Keyword, Lexer, Nest, NestedToken};

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
        Amp             = 0x4e,
        Tilde           = 0x4f,
        Pipe            = 0x50,
        LtLt            = 0x51,
        GtGt            = 0x52,
        SlashSlash      = 0x53,
        LParen          = 0x54,
        RParen          = 0x55,
        LBrace          = 0x56,
        RBrace          = 0x57,
        LBracket        = 0x58,
        RBracket        = 0x59,
        Semicolon       = 0x5a,
        Colon           = 0x5b,
        ColonColon      = 0x5c,
        Comma           = 0x5d,
        Dot             = 0x5e,
        DotDot          = 0x5f,
        DotDotDot       = 0x60,
        DashDashHash    = 0x61,
        DashDashV       = 0x62,
        DashDashColon   = 0x63,
        DashDashGt      = 0x64,
        Ques            = 0x65,
        Bang            = 0x66,
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
        Goto        = 0x89,
        If          = 0x8a,
        In          = 0x8b,
        Local       = 0x8c,
        Nil         = 0x8d,
        Not         = 0x8e,
        Or          = 0x8f,
        Repeat      = 0x90,
        Return      = 0x91,
        Then        = 0x92,
        True        = 0x93,
        Until       = 0x94,
        While       = 0x95,
        Assume      = 0x96,
        Const       = 0x97,
        Global      = 0x98,
        Map         = 0x99,
        Module      = 0x9a,
        Once        = 0x9b,
        Open        = 0x9c,
        Type        = 0x9d,
        Var         = 0x9e,
        Vector      = 0x9f,
    }
}

pub const VS_TOKEN_NESTING_IS_EXPR: u8 = 0;
pub const VS_TOKEN_NESTING_IS_META: u8 = 1;

#[repr(C)]
pub struct VSTokenNesting {
    serial: u32,
    depth: u16,
    category: u8,
}

pub struct VSTokenStream {
    tokens: Vec<NestedToken>,
    cursor: usize,
}

impl VSTokenStream {
    pub fn new(source: &VSSource, span: Span, report: &Report) -> Option<Box<VSTokenStream>> {
        let source = source.source().write().unwrap();
        if let Some(mut iter) = source.iter_from_span(span) {
            let mut lexer = Lexer::new(&mut iter, report);
            let nest = Nest::new(&mut lexer);
            let tokens: Vec<_> = nest.collect();
            assert!(!tokens.is_empty()); // should include EOF
            Some(Box::new(VSTokenStream { tokens: tokens, cursor: 0 }))
        } else {
            None
        }
    }

    pub fn into_tokens(self) -> Vec<NestedToken> {
        self.tokens
    }

    pub fn next(&mut self, span: &mut Span, nesting: &mut VSTokenNesting) -> VSTokenType {
        let token = &self.tokens[self.cursor];
        if self.cursor + 1 < self.tokens.len() {
            self.cursor += 1;
        }
        *span = token.tok.span;
        *nesting = VSTokenNesting {
            serial: token.serial.to_usize() as u32,
            depth: token.depth,
            category: token.category as u8,
        };
        VSTokenType::from(&token.tok.base)
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
    let report: &Arc<VSReport> = unsafe { mem::transmute(&report) };

    let src = AssertUnwindSafe(src);
    let span = AssertUnwindSafe(span);
    let report = AssertUnwindSafe(report);
    panic::catch_unwind(move || {
        let stream = VSTokenStream::new(&src, span.0, &report.0);
        unsafe { mem::transmute(stream) }
    }).unwrap_or(ptr::null_mut())
}

#[no_mangle]
pub extern "C" fn kailua_token_stream_next(stream: *mut VSTokenStream,
                                           span: *mut Span,
                                           nesting: *mut VSTokenNesting) -> VSTokenType {
    if stream.is_null() { return VSTokenType::Dead; }
    if span.is_null() { return VSTokenType::Dead; }
    if nesting.is_null() { return VSTokenType::Dead; }

    let stream: &mut VSTokenStream = unsafe { mem::transmute(stream) };
    let span = unsafe { span.as_mut().unwrap() };
    let nesting = unsafe { nesting.as_mut().unwrap() };

    let mut stream = AssertUnwindSafe(stream);
    let span = AssertUnwindSafe(span);
    let nesting = AssertUnwindSafe(nesting);
    panic::catch_unwind(move || {
        stream.next(span.0, nesting.0)
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

