#[macro_use] extern crate custom_derive;
#[macro_use] extern crate newtype_derive;

pub use lex::Error;
pub use ast::{Name, Str, Var, Params, E, Exp, UnOp, BinOp, FuncScope, SelfParam, S, Stmt, Block};

mod lex;
mod ast;
mod parser;

pub fn parse_chunk(s: &[u8]) -> Result<Block, Error> {
    let lexer = lex::Lexer::new(s.iter().cloned());
    parser::Parser::new(lexer).into_chunk()
}

#[test]
fn test_parse() {
    fn test(s: &str) -> String {
        match parse_chunk(s.as_bytes()) {
            Ok(ast) => format!("{:?}", ast),
            Err(_) => String::from("parse error"),
        }
    }

    assert_eq!(test("do break; end; break"), "[Do([Break]), Break]");
    assert_eq!(test("function r(p) --[[...]] end"), "[FuncDecl(Global, `r`, [`p`], [])]");
    assert_eq!(test("local function r(p,...)\n\nend"), "[FuncDecl(Local, `r`, [`p`, ...], [])]");
    assert_eq!(test("local a, b"), "[Local([`a`, `b`], [])]");
    assert_eq!(test("f()"), "[Void(Call(`f`, []))]");
    assert_eq!(test("f(3)"), "[Void(Call(`f`, [3]))]");
    assert_eq!(test("f(3+4)"), "[Void(Call(`f`, [(3 + 4)]))]");
    assert_eq!(test("f(3+4-5)"), "[Void(Call(`f`, [((3 + 4) - 5)]))]");
    assert_eq!(test("f(3+4*5)"), "[Void(Call(`f`, [(3 + (4 * 5))]))]");
    assert_eq!(test("f((3+4)*5)"), "[Void(Call(`f`, [((3 + 4) * 5)]))]");
    assert_eq!(test("f(2^3^4)"), "[Void(Call(`f`, [(2 ^ (3 ^ 4))]))]");
    assert_eq!(test("f'oo'"), "[Void(Call(`f`, [\"oo\"]))]");
    assert_eq!(test("f{a=1,[3.1]=4e5;[=[[[]]]=],}"),
               "[Void(Call(`f`, [Table([(Some(`a`), 1), \
                                        (Some(3.1), 400000), \
                                        (None, \"[[]]\")])]))]");
    assert_eq!(test("f{a=a, a}"), "[Void(Call(`f`, [Table([(Some(`a`), `a`), (None, `a`)])]))]");
    assert_eq!(test("--[a]"), "[]");
}

