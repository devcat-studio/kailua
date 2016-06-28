extern crate kailua_diag;
#[macro_use] extern crate custom_derive;
#[macro_use] extern crate newtype_derive;

use kailua_diag::{Source, Span, Spanned, Report};

pub use ast::{Name, Str, Seq, Var, TypeSpec, Sig, Ex, Exp, UnOp, BinOp, NameScope, SelfParam};
pub use ast::{St, Stmt, Block, M, K, Kind, FuncKind, SlotKind};

mod lex;
mod ast;
mod parser;

pub fn parse_chunk(source: &Source, span: Span,
                   report: &Report) -> kailua_diag::Result<Spanned<Block>> {
    let lexer = lex::Lexer::new(source.iter_bytes_from_span(span), &report);
    let parser = parser::Parser::new(lexer, &report);
    parser.into_chunk()
}

