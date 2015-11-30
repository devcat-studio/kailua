extern crate lalrpop_util;
#[macro_use] extern crate custom_derive;
#[macro_use] extern crate newtype_derive;

pub use lex::Error;
pub use ast::{Name, Str, Var, Params, E, Exp, UnOp, BinOp, S, Stmt, Block};

mod lex;
mod ast;
mod parser;

pub fn parse_block(s: &[u8]) -> Result<Block, Error> {
    let lexer = lex::Lexer::new(s.iter().cloned());
    match parser::parse_Block(lexer) {
        Ok(ast) => Ok(ast),
        Err(_) => Err("parse error"), // for now
    }
}

#[test]
fn test_parse() {
    fn test(s: &str) -> String {
        match parse_block(s.as_bytes()) {
            Ok(ast) => format!("{:?}", ast),
            Err(e) => String::from(e),
        }
    }

    assert_eq!(test("do break; end; break"), "[Do([Break]), Break]");
}

