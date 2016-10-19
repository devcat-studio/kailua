#[macro_use] extern crate parse_generics_shim;
#[macro_use] extern crate kailua_diag;
#[macro_use] extern crate custom_derive;
#[macro_use] extern crate newtype_derive;
#[macro_use] extern crate log;

use kailua_diag::{Source, Span, Spanned, Report};

pub use lex::{Tok, Punct, Keyword, Lexer};
pub use ast::{Name, Str, Seq, Var, TypeSpec, Sig, Ex, Exp, UnOp, BinOp, NameScope, SelfParam};
pub use ast::{St, Stmt, Block, M, K, Kind, FuncKind, SlotKind, Attr};
pub use parser::Parser;

mod message;
mod lex;
mod ast;
mod parser;

pub fn parse_chunk(source: &Source, span: Span,
                   report: &Report) -> kailua_diag::Result<Spanned<Block>> {
    if let Some(mut iter) = source.iter_from_span(span) {
        let lexer = lex::Lexer::new(&mut iter, &report);
        let parser = parser::Parser::new(lexer, &report);
        parser.into_chunk()
    } else {
        use kailua_diag::Reporter;
        report.fatal(span, message::NoFileForSpan {}).done()
    }
}

