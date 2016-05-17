extern crate kailua_diag;
#[macro_use] extern crate custom_derive;
#[macro_use] extern crate newtype_derive;

use kailua_diag::Spanned;

pub use lex::Error;
pub use ast::{Name, Str, Var, TypeSpec, Sig, Ex, Exp, UnOp, BinOp, FuncScope, SelfParam};
pub use ast::{St, Stmt, Block, M, K, Kind, FuncKind, SlotKind};

mod lex;
mod ast;
mod parser;

pub fn parse_chunk(s: &[u8]) -> Result<Spanned<Block>, Error> {
    parse_chunk_with_path(s, "<none>")
}

pub fn parse_chunk_with_path(s: &[u8], path: &str) -> Result<Spanned<Block>, Error> {
    let mut source = kailua_diag::Source::new();
    let span = source.add_string(path, s);
    let report = kailua_diag::ConsoleReport::new(&source);
    let lexer = lex::Lexer::new(source.iter_bytes_from_span(span), &report);
    let parser = parser::Parser::new(lexer, &report);
    let chunk = try!(parser.into_chunk());
    if report.max_kind_seen() >= Some(kailua_diag::Kind::Error) {
        Err("parsing error")
    } else {
        Ok(chunk)
    }
}

#[test]
fn test_parse() {
    extern crate regex;

    let span_pattern = regex::Regex::new(r"@(?:_|[0-9a-f]+(?:-[0-9a-f]+)?)").unwrap();
    let strip_span = |s: &str| span_pattern.replace_all(s, "");
    assert_eq!(strip_span("[X@1, Y@3a-4f0]@_"), "[X, Y]");

    let test = |s: &str| {
        println!("{:?}", s);
        match parse_chunk(s.as_bytes()) {
            Ok(ast) => strip_span(&format!("{:?}", ast)),
            Err(e) => { println!("{:?}", e); String::from("parse error") },
        }
    };

    assert_eq!(test(""), "[]");
    assert_eq!(test("\n"), "[]");
    assert_eq!(test("\n\n"), "[]");
    assert_eq!(test("do break; end; break"), "[Do([Break]), Break]");
    assert_eq!(test("function r(p) --[[...]] end"),
               "[FuncDecl(Global, `r`, [`p`] -> _, [])]");
    assert_eq!(test("local function r(p,...)\n\nend"),
               "[FuncDecl(Local, `r`, [`p`, ...: _] -> _, [])]");
    assert_eq!(test("local a, b"), "[Local([`a`, `b`], [])]");
    assert_eq!(test("local a --: integer\n, b --: var ?"),
               "[Local([`a`: _ Integer, `b`: Var Dynamic], [])]");
    assert_eq!(test("local a, --: const table\nb"),
               "[Local([`a`: Const Table, `b`], [])]");
    assert_eq!(test("local function r(p --: integer\n)\n\nend"),
               "[FuncDecl(Local, `r`, [`p`: _ Integer] -> _, [])]");
    assert_eq!(test("local function r(p, q) --: integer --> string\n\nend"),
               "[FuncDecl(Local, `r`, [`p`, `q`: _ Integer] -> String, [])]");
    assert_eq!(test("f()"), "[Void(`f`())]");
    assert_eq!(test("f(3)"), "[Void(`f`(3))]");
    assert_eq!(test("f(3+4)"), "[Void(`f`((3 + 4)))]");
    assert_eq!(test("f(3+4-5)"), "[Void(`f`(((3 + 4) - 5)))]");
    assert_eq!(test("f(3+4*5)"), "[Void(`f`((3 + (4 * 5))))]");
    assert_eq!(test("f((3+4)*5)"), "[Void(`f`(((3 + 4) * 5)))]");
    assert_eq!(test("f(2^3^4)"), "[Void(`f`((2 ^ (3 ^ 4))))]");
    assert_eq!(test("f'oo'"), "[Void(`f`(\"oo\"))]");
    assert_eq!(test("f{a=1,[3.1]=4e5;[=[[[]]]=],}"),
               "[Void(`f`(Table([(Some(\"a\"), 1), \
                                 (Some(3.1), 400000), \
                                 (None, \"[[]]\")])))]");
    assert_eq!(test("f{a=a, a}"), "[Void(`f`(Table([(Some(\"a\"), `a`), (None, `a`)])))]");
    assert_eq!(test("f{a=a; a;}"), "[Void(`f`(Table([(Some(\"a\"), `a`), (None, `a`)])))]");
    assert_eq!(test("--[a]\ndo end--]]"), "[Do([])]");
    assert_eq!(test("--[[a]\ndo end--]]"), "[]");
    assert_eq!(test("--#\ndo end"), "[Do([])]");
    assert_eq!(test("--#\n"), "[]");
    assert_eq!(test("--#"), "[]");
    assert_eq!(test("--#\n--#"), "[]");
    assert_eq!(test("--#\n--#\n--#"), "[]");
    assert_eq!(test("--#\n--#\ndo end"), "[Do([])]");
    assert_eq!(test("--#\n\n--#"), "[]");
    assert_eq!(test("--#\n\n--#\n\n--#"), "[]");
    assert_eq!(test("--# --foo\ndo end"), "[Do([])]");
    assert_eq!(test("--# --[[foo]]\ndo end"), "[Do([])]");
    assert_eq!(test("--# --[[foo\n--# --foo]]\ndo end"), "parse error");
    assert_eq!(test("--# assume a: string"), "[KailuaAssume(`a`, _, String, None)]");
    assert_eq!(test("--# assume a: string\n--#"), "[KailuaAssume(`a`, _, String, None)]");
    assert_eq!(test("--# assume a:
                     --#   string"), "[KailuaAssume(`a`, _, String, None)]");
    assert_eq!(test("--# assume a:
                     --# assume b: string"), "parse error");
    assert_eq!(test("--# assume a: {x=string}
                     --# assume b: {y=string}"),
               "[KailuaAssume(`a`, _, Record([\"x\": _ String]), None), \
                 KailuaAssume(`b`, _, Record([\"y\": _ String]), None)]");
    assert_eq!(test("--# assume assume: ?"), "parse error");
    assert_eq!(test("--# assume `assume`: ?"), "[KailuaAssume(`assume`, _, Dynamic, None)]");
    assert_eq!(test("--# `assume` `assume`: ?"), "parse error");
    assert_eq!(test("local x --: {b=var string, a=integer, c=const {d=const {}}}"),
               "[Local([`x`: _ Record([\"b\": Var String, \"a\": _ Integer, \
                                       \"c\": Const Record([\"d\": Const EmptyTable])])], [])]");
    assert_eq!(test("local x --: function"), "[Local([`x`: _ Function], [])]");
    assert_eq!(test("local x --: function()"), "[Local([`x`: _ Func([() -> ()])], [])]");
    assert_eq!(test("local x --: function()->()"), "[Local([`x`: _ Func([() -> ()])], [])]");
    assert_eq!(test("local x --: function () & (integer, boolean...)->string?"),
               "[Local([`x`: _ Func([() -> (), \
                                     (Integer, Boolean...) -> Union([String, Nil])])], [])]");
    assert_eq!(test("local x --: function (boolean...) | string?"),
               "[Local([`x`: _ Union([Func([(Boolean...) -> ()]), Union([String, Nil])])], [])]");
    assert_eq!(test("local x --: `function`"), "[Local([`x`: _ Function], [])]");
    assert_eq!(test("local x --: `function`()"), "parse error");
    assert_eq!(test("local x --: (integer, string)"), "parse error");
    assert_eq!(test("local x --: (integer)"), "[Local([`x`: _ Integer], [])]");
    assert_eq!(test("local x --: (integer)?"), "[Local([`x`: _ Union([Integer, Nil])], [])]");
    assert_eq!(test("local x --:
                             --: (integer)?"), "[Local([`x`: _ Union([Integer, Nil])], [])]");
    assert_eq!(test("local x --: (
                             --:   integer
                             --: )?"), "[Local([`x`: _ Union([Integer, Nil])], [])]");
    assert_eq!(test("local x --: (
                             --:   integer"), "parse error");
    assert_eq!(test("local x --: {}"), "[Local([`x`: _ EmptyTable], [])]");
    assert_eq!(test("local x --: {a = const function (), b = var string,
                             --:  c = const function (string) -> integer &
                             --:                     (string, integer) -> number}?"),
               "[Local([`x`: _ Union([Record([\"a\": Const Func([() -> ()]), \
                                              \"b\": Var String, \
                                              \"c\": Const Func([(String) -> Integer, \
                                                                 (String, Integer) -> Number])\
                                             ]), Nil])], [])]");
    assert_eq!(test("local x --: {const function (); var string;
                             --:  const function (string) -> integer &
                             --:                 (string, integer) -> number;
                             --: }?"),
               "[Local([`x`: _ Union([Tuple([Const Func([() -> ()]), \
                                             Var String, \
                                             Const Func([(String) -> Integer, \
                                                         (String, Integer) -> Number])\
                                            ]), Nil])], [])]");
    assert_eq!(test("local x --: {?}"), "[Local([`x`: _ Array(_ Dynamic)], [])]");
    assert_eq!(test("local x --: {?,}"), "[Local([`x`: _ Tuple([_ Dynamic])], [])]");
    assert_eq!(test("local x --: {?,?}"), "[Local([`x`: _ Tuple([_ Dynamic, _ Dynamic])], [])]");
    assert_eq!(test("local x --: {?;?;}"), "[Local([`x`: _ Tuple([_ Dynamic, _ Dynamic])], [])]");
    assert_eq!(test("local x --: {var integer}"), "[Local([`x`: _ Array(Var Integer)], [])]");
    assert_eq!(test("local x --: {[string] = const integer}"),
               "[Local([`x`: _ Map(String, Const Integer)], [])]");
    assert_eq!(test("local x --: {[string] = integer|boolean}"),
               "[Local([`x`: _ Map(String, _ Union([Integer, Boolean]))], [])]");
    assert_eq!(test("local x --: {[string|boolean] = integer|boolean}"),
               "[Local([`x`: _ Map(Union([String, Boolean]), _ Union([Integer, Boolean]))], [])]");
    assert_eq!(test("local x --: {[string?] = integer?}"),
               "[Local([`x`: _ Map(Union([String, Nil]), _ Union([Integer, Nil]))], [])]");
    assert_eq!(test("local x --: {[integer] = const {var {[string] = {integer, integer}?}}}"),
               "[Local([`x`: _ Map(Integer, \
                                   Const Array(Var Map(String, \
                                                       _ Union([Tuple([_ Integer, _ Integer]), \
                                                                Nil]))))], [])]");
    assert_eq!(test("--v ()
                     function foo() end"),
               "[FuncDecl(Global, `foo`, [], [])]"); // note that the return is specified (not `_`)
    assert_eq!(test("--v (a: integer)
                     function foo() end"), "parse error");
    assert_eq!(test("--v (a: integer)
                     function foo(b) end"), "parse error");
    assert_eq!(test("--v ()
                     function foo(b) end"), "parse error");
    assert_eq!(test("--v (a: integer, b: integer)
                     function foo(b, a) end"), "parse error");
    assert_eq!(test("--v (a: integer)
                     local function foo(a) end"),
               "[FuncDecl(Local, `foo`, [`a`: _ Integer], [])]");
    assert_eq!(test("(--v (a: const integer,
                      --v  ...)
                      --v -> string
                      function(a, ...) end)()"),
               "[Void(Func([`a`: Const Integer, ...: _] -> String, [])())]");
    assert_eq!(test("--v ()
                     function foo() --> string
                     end"), "parse error");
    assert_eq!(test("--v ()
                     function foo(a) --: integer --> string
                     end"), "parse error");
    assert_eq!(test("--v ()
                     function foo(a, --: integer
                                  b) --> string
                     end"), "parse error");
    assert_eq!(test("--v ()"), "parse error");
    assert_eq!(test("--v ()
                     local v = 42"), "parse error");
    assert_eq!(test("--v ()
                     --# assume x: integer"), "parse error");
    assert_eq!(test("--v ()
                     for i = 1, 3 do end"), "parse error");
    assert_eq!(test("function foo(a, --: integer
                                  b, ...) --: string
                     end"),
               "[FuncDecl(Global, `foo`, [`a`: _ Integer, `b`, ...: String] -> _, [])]");
    assert_eq!(test("--v (a: integer, b: boolean, ...: string)
                     function foo(a, b, ...) --: string
                     end"), "parse error");
    assert_eq!(test("--v (a: integer, b: boolean)
                     function foo(a, b, ...)
                     end"), "parse error");
    assert_eq!(test("--v (a: integer, b: boolean, ...: string)
                     function foo(a, b)
                     end"), "parse error");
    assert_eq!(test("--v (a: integer, b: boolean, ...: string)
                     function foo(a, b, ...)
                     end"),
               "[FuncDecl(Global, `foo`, [`a`: _ Integer, `b`: _ Boolean, ...: String], [])]");
    assert_eq!(test("--v (...: string)
                     function foo(...)
                     end"),
               "[FuncDecl(Global, `foo`, [...: String], [])]");
    assert_eq!(test("--v (...: string)
                     local function foo(...)
                     end"),
               "[FuncDecl(Local, `foo`, [...: String], [])]");
    assert_eq!(test("--# assume a: { x
                     --#             y"), "parse error");
}

