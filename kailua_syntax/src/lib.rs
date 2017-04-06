#[macro_use] extern crate parse_generics_shim;
#[macro_use] extern crate kailua_diag;
#[macro_use] extern crate log;
extern crate kailua_env;

use kailua_env::{Source, Span};
use kailua_diag::Report;

pub use lang::{Language, Lua, Kailua};
pub use lex::{Tok, Punct, Keyword, Lexer};
pub use lex::{Nest, NestedToken, NestingCategory, NestingSerial};
pub use ast::{Name, NameRef, Str, Seq, Var, TypeSpec, Varargs, Returns};
pub use ast::{Sig, Ex, Exp, UnOp, BinOp, SelfParam};
pub use ast::{TypeScope, St, Stmt, Block, M, MM, K, Kind, FuncKind, SlotKind, Attr, Args, Chunk};
pub use parser::Parser;

mod lang;
mod message;
mod lex;
mod ast;
mod parser;

pub fn parse_chunk(source: &Source, span: Span, report: &Report) -> kailua_diag::Result<Chunk> {
    if let Some(mut iter) = source.iter_from_span(span) {
        let mut lexer = lex::Lexer::new(&mut iter, &report);
        let mut nest = lex::Nest::new(&mut lexer);
        let parser = parser::Parser::new(&mut nest, &report);
        parser.into_chunk()
    } else {
        use kailua_diag::Reporter;
        report.fatal(span, message::NoFileForSpan {}).done()
    }
}

