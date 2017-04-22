#[macro_use] extern crate parse_generics_shim;
#[macro_use] extern crate kailua_diag;
#[macro_use] extern crate log;
extern crate kailua_env;

use kailua_env::{Source, Span};
use kailua_diag::Report;

pub use string::{Str, Name};
pub use lex::{Lexer, Nest, Tok, NestedToken};
pub use ast::Chunk;
pub use parser::Parser;

pub mod lang;
mod message;
pub mod lex;
pub mod string;
pub mod ast;
mod parser;

pub fn parse_chunk(source: &Source, span: Span, report: &Report) -> kailua_diag::Result<Chunk> {
    if let Some(mut iter) = source.iter_from_span(span) {
        let mut lexer = Lexer::new(&mut iter, &report);
        let mut nest = Nest::new(&mut lexer);
        let parser = Parser::new(&mut nest, &report);
        parser.into_chunk()
    } else {
        use kailua_diag::Reporter;
        report.fatal(span, message::NoFileForSpan {}).done()
    }
}

