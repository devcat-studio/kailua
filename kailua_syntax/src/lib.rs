extern crate kailua_diag;
#[macro_use] extern crate custom_derive;
#[macro_use] extern crate newtype_derive;

use kailua_diag::Spanned;

pub use ast::{Name, Str, Seq, Var, TypeSpec, Sig, Ex, Exp, UnOp, BinOp, NameScope, SelfParam};
pub use ast::{St, Stmt, Block, M, K, Kind, FuncKind, SlotKind};

mod lex;
mod ast;
mod parser;

pub fn parse_chunk(source: &kailua_diag::Source, span: kailua_diag::Span,
                   report: &kailua_diag::Report) -> kailua_diag::Result<Spanned<Block>> {
    let lexer = lex::Lexer::new(source.iter_bytes_from_span(span), &report);
    let parser = parser::Parser::new(lexer, &report);
    parser.into_chunk()
}

#[test]
#[allow(unused_mut)]
fn test_parse() {
    extern crate regex;
    use std::cell::RefCell;
    use std::io::{stderr, Write};
    use std::collections::HashMap;
    use kailua_diag::Report;

    let span_pattern = regex::Regex::new(r"@(?:_|[0-9a-f]+(?:-[0-9a-f]+)?)").unwrap();
    let strip_span = |s: &str| span_pattern.replace_all(s, "");
    assert_eq!(strip_span("[X@1, Y@3a-4f0]@_"), "[X, Y]");

    let parse = |code: &str, path: &str| {
        let mut source = kailua_diag::Source::new();
        let filespan = source.add_string(path, code.as_bytes());
        let report = kailua_diag::CollectedReport::new();
        let chunk = match parse_chunk(&source, filespan, &report) {
            Ok(chunk) => strip_span(&format!("{:?}", chunk)),
            Err(_) => String::from("error"),
        };
        let reports: Vec<_> = {
            let file = source.file_from_span(filespan).unwrap();
            report.into_reports().into_iter().map(|(kind, span, msg)| {
                let lines = file.lines_from_span(span).map(|(begin, _, end)| (begin + 1, end + 1));
                (kind, span, lines, msg)
            }).collect()
        };
        (RefCell::new(source), chunk, reports)
    };

    let check = |code: &str, path: &str,
                 expectedchunk: &str, expectedreports: &[(Option<(usize, usize)>, &str)]| {
        let (source, chunk, reports) = parse(code, path);
        let mut reportset = HashMap::new();
        for &(lines, msg) in expectedreports {
            *reportset.entry((lines, msg.to_owned())).or_insert(0isize) += 1;
        }
        for &(kind, _span, lines, ref msg) in &reports {
            let key = (lines, format!("[{:?}] {}", kind, msg));
            if let Some(value) = reportset.get_mut(&key) { *value -= 1; }
        }
        if chunk != expectedchunk || reportset.values().any(|&v| v > 0) {
            // lazily print the reports
            let mut err = stderr();
            let _ = writeln!(err, "");
            let _ = writeln!(err, "{:-<60}", "EXPECTED ");
            let _ = writeln!(err, "{:?}", expectedchunk);
            if !expectedreports.is_empty() {
                let _ = writeln!(err, "");
                for &(lines, msg) in expectedreports {
                    if let Some((begin, end)) = lines {
                        if begin == end {
                            let _ = write!(err, "{}:{}:_: ", path, begin);
                        } else {
                            let _ = write!(err, "{}:{}:_: {}:_ ", path, begin, end);
                        }
                    }
                    let _ = writeln!(err, "{}", msg);
                }
            }
            let _ = writeln!(err, "{:-<60}", "ACTUAL ");
            let _ = writeln!(err, "{:?}", chunk);
            if !reports.is_empty() {
                let _ = writeln!(err, "");
                let display = kailua_diag::ConsoleReport::new(&source);
                for &(kind, span, _lines, ref msg) in &reports {
                    let _ = display.add_span(kind, span, msg.clone());
                }
            }
            let _ = writeln!(err, "{:-<60}", "");
            false
        } else {
            true
        }
    };

    macro_rules! test {
        (@expr $e:expr) => ($e);

        (@reports $out:expr) => ({});
        (@reports $out:expr;) => ({});
        (@reports $out:expr; $line:tt: $msg:tt $($t:tt)*) => ({
            $out.push((Some((test!(@expr $line), test!(@expr $line))), test!(@expr $msg)));
            test!(@reports $out $($t)*)
        });
        (@reports $out:expr; $begin:tt-$end:tt: $msg:tt $($t:tt)*) => ({
            $out.push((Some((test!(@expr $begin), test!(@expr $end))), test!(@expr $msg)));
            test!(@reports $out $($t)*)
        });
        (@reports $out:expr; $msg:tt $($t:tt)*) => ({
            $out.push((None, test!(@expr $msg)));
            test!(@reports $out $($t)*)
        });

        ($code:expr; $ast:tt $($t:tt)*) => ({
            let path = format!("<string at {}:{}>", file!(), line!());
            let mut reports = Vec::new();
            test!(@reports reports $($t)*);
            if !check($code, &path, test!(@expr $ast), &reports) { panic!("check failed"); }
        });
    }

    test!("";
          "[]");

    test!("\n";
          "[]");

    test!("\n\n";
          "[]");

    test!("do break; end; break";
          "[Do([Break]), Break]");

    test!("function r(p) --[[...]] end";
          "[FuncDecl(Global, `r`, [`p`] -> _, [])]");

    test!("local function r(p,...)\n\nend";
          "[FuncDecl(Local, `r`, [`p`, ...: _] -> _, [])]");

    test!("local a, b";
          "[Local([`a`, `b`], [])]");

    test!("local a --: integer\n, b --: var ?";
          "[Local([`a`: _ Integer, `b`: Var Dynamic], [])]");

    test!("local a, --: const table\nb";
          "[Local([`a`: Const Table, `b`], [])]");

    test!("local function r(p --: integer\n)\n\nend";
          "[FuncDecl(Local, `r`, [`p`: _ Integer] -> _, [])]");

    test!("local function r(p, q) --: integer --> string\n\nend";
          "[FuncDecl(Local, `r`, [`p`, `q`: _ Integer] -> String, [])]");

    test!("f()";
          "[Void(`f`())]");

    test!("f(3)";
          "[Void(`f`(3))]");

    test!("f(3+4)";
          "[Void(`f`((3 + 4)))]");

    test!("f(3+4-5)";
          "[Void(`f`(((3 + 4) - 5)))]");

    test!("f(3+4*5)";
          "[Void(`f`((3 + (4 * 5))))]");

    test!("f((3+4)*5)";
          "[Void(`f`(((3 + 4) * 5)))]");

    test!("f(2^3^4)";
          "[Void(`f`((2 ^ (3 ^ 4))))]");

    test!("f'oo'";
          "[Void(`f`(\"oo\"))]");

    test!("f{a=1,[3.1]=4e5;[=[[[]]]=],}";
          "[Void(`f`(Table([(Some(\"a\"), 1), \
                            (Some(3.1), 400000), \
                            (None, \"[[]]\")])))]");

    test!("f{a=a, a}";
          "[Void(`f`(Table([(Some(\"a\"), `a`), (None, `a`)])))]");

    test!("f{a=a; a;}";
          "[Void(`f`(Table([(Some(\"a\"), `a`), (None, `a`)])))]");

    test!("--[a]\ndo end--]]";
          "[Do([])]");

    test!("--[[a]\ndo end--]]";
          "[]");

    test!("--#\ndo end";
          "[Do([])]");

    test!("--#\n";
          "[]");

    test!("--#";
          "[]");

    test!("--#\n--#";
          "[]");

    test!("--#\n--#\n--#";
          "[]");

    test!("--#\n--#\ndo end";
          "[Do([])]");

    test!("--#\n\n--#";
          "[]");

    test!("--#\n\n--#\n\n--#";
          "[]");

    test!("--# --foo\ndo end";
          "[Do([])]");

    test!("--# --[[foo]]\ndo end";
          "[Do([])]");

    test!("--# --[[foo\n--# --foo]]\ndo end";
          "error";
          1: "[Fatal] A newline is disallowed in a long string inside the meta block";
          1: "[Note] The meta block started here");

    test!("--# assume a: string";
          "[KailuaAssume(Local, `a`, _, String, None)]");

    test!("--# assume a: string\n--#";
          "[KailuaAssume(Local, `a`, _, String, None)]");

    test!("--# assume a:
           --#   string";
          "[KailuaAssume(Local, `a`, _, String, None)]");

    test!("--# assume global a:
           --#   string";
          "[KailuaAssume(Global, `a`, _, String, None)]");

    test!("--# assume global global";
          "error";
          1: "[Fatal] Expected a name, got a keyword `global`");

    test!("--# assume a:
           --# assume b: string";
          "error";
          2: "[Error] Expected a single type, got a keyword `assume`");

    test!("--# assume global a: {x=string}
           --# assume b: {y=string}";
          "[KailuaAssume(Global, `a`, _, Record([\"x\": _ String]), None), \
            KailuaAssume(Local, `b`, _, Record([\"y\": _ String]), None)]");

    test!("--# assume assume: ?";
          "error";
          1: "[Fatal] Expected a name, got a keyword `assume`");

    test!("--# assume `assume`: ?";
          "[KailuaAssume(Local, `assume`, _, Dynamic, None)]");

    test!("--# `assume` `assume`: ?";
          "error";
          1: "[Error] Expected a newline, got a name");

    test!("--# assume a: ? = \"foo\"
           --# assume b: ?";
          "[KailuaAssume(Local, `a`, _, Dynamic, Some(\"foo\")), \
            KailuaAssume(Local, `b`, _, Dynamic, None)]");

    test!("local x --: {b=var string, a=integer, c=const {d=const {}}}";
          "[Local([`x`: _ Record([\"b\": Var String, \"a\": _ Integer, \
                                  \"c\": Const Record([\"d\": Const EmptyTable])])], [])]");

    test!("local x --: function";
          "[Local([`x`: _ Function], [])]");

    test!("local x --: function()";
          "[Local([`x`: _ Func([() -> ()])], [])]");

    test!("local x --: function()->()";
          "[Local([`x`: _ Func([() -> ()])], [])]");

    test!("local x --: function () & (integer, boolean...)->string?";
          "[Local([`x`: _ Func([() -> (), \
                                (Integer, Boolean...) -> Union([String, Nil])])], [])]");

    test!("local x --: function (boolean...) | string?";
          "[Local([`x`: _ Union([Func([(Boolean...) -> ()]), Union([String, Nil])])], [])]");

    test!("local x --: `function`";
          "[Local([`x`: _ Function], [])]");

    test!("local x --: `function`()
           local";
          "error";
          1: "[Error] Expected a newline, got `(`";
          2: "[Fatal] Expected a name or `function` after `local`"); // recoverable

    test!("local x --: (integer, string)";
          "error";
          1: "[Error] Expected a single type, not type sequence");

    test!("local x --: (integer)";
          "[Local([`x`: _ Integer], [])]");

    test!("local x --: (integer)?";
          "[Local([`x`: _ Union([Integer, Nil])], [])]");

    test!("local x --:
                   --: (integer)?";
          "[Local([`x`: _ Union([Integer, Nil])], [])]");

    test!("local x --: (
                   --:   integer
                   --: )?";
          "[Local([`x`: _ Union([Integer, Nil])], [])]");

    test!("local x --: (
                   --:   integer";
          "error";
          2: "[Fatal] Expected `)`, got a newline");

    test!("local x --: {}";
          "[Local([`x`: _ EmptyTable], [])]");

    test!("local x --: {a = const function (), b = var string,
                   --:  c = const function (string) -> integer &
                   --:                     (string, integer) -> number}?";
          "[Local([`x`: _ Union([Record([\"a\": Const Func([() -> ()]), \
                                         \"b\": Var String, \
                                         \"c\": Const Func([(String) -> Integer, \
                                                            (String, Integer) -> Number])\
                                        ]), Nil])], [])]");

    test!("local x --: {const function (); var string;
                   --:  const function (string) -> integer &
                   --:                 (string, integer) -> number;
                   --: }?";
          "[Local([`x`: _ Union([Tuple([Const Func([() -> ()]), \
                                        Var String, \
                                        Const Func([(String) -> Integer, \
                                                    (String, Integer) -> Number])\
                                       ]), Nil])], [])]");

    test!("local x --: {?}";
          "[Local([`x`: _ Array(_ Dynamic)], [])]");

    test!("local x --: {?,}";
          "[Local([`x`: _ Tuple([_ Dynamic])], [])]");

    test!("local x --: {?,?}";
          "[Local([`x`: _ Tuple([_ Dynamic, _ Dynamic])], [])]");

    test!("local x --: {?;?;}";
          "[Local([`x`: _ Tuple([_ Dynamic, _ Dynamic])], [])]");

    test!("local x --: {var integer}";
          "[Local([`x`: _ Array(Var Integer)], [])]");

    test!("local x --: {[string] = const integer}";
          "[Local([`x`: _ Map(String, Const Integer)], [])]");

    test!("local x --: {[string] = integer|boolean}";
          "[Local([`x`: _ Map(String, _ Union([Integer, Boolean]))], [])]");

    test!("local x --: {[string|boolean] = integer|boolean}";
          "[Local([`x`: _ Map(Union([String, Boolean]), _ Union([Integer, Boolean]))], [])]");

    test!("local x --: {[string?] = integer?}";
          "[Local([`x`: _ Map(Union([String, Nil]), _ Union([Integer, Nil]))], [])]");

    test!("local x --: {[integer] = const {var {[string] = {integer, integer}?}}}";
          "[Local([`x`: _ Map(Integer, \
                              Const Array(Var Map(String, \
                                                  _ Union([Tuple([_ Integer, _ Integer]), \
                                                           Nil]))))], [])]");

    test!("--v ()
           function foo() end";
          "[FuncDecl(Global, `foo`, [], [])]"); // note that the return is specified (not `_`)

    test!("--v (a: integer,
           --v  b: string)
           function foo() end";
          "error";
          1-2: "[Error] Excess arguments in the function specification");

    test!("--v (a: integer,
           --v  b: string)
           function foo(a) end";
          "error";
          2: "[Error] Excess arguments in the function specification");

    test!("--v (a: integer)
           function foo(b) end";
          "error";
          2: "[Error] Mismatching argument name in the function specification";
          1: "[Note] The corresponding argument was here");

    test!("--v ()
           function foo(a,
                        b) end";
          "error";
          2-3: "[Error] Excess arguments in the function declaration");

    test!("--v (a: integer)
           function foo(a,
                        b) end";
          "error";
          3: "[Error] Excess arguments in the function declaration");

    test!("--v (a: integer, b: integer)
           function foo(b, a) end";
          "error";
          2: "[Error] Mismatching argument name in the function specification";
          1: "[Note] The corresponding argument was here";
          2: "[Error] Mismatching argument name in the function specification";
          1: "[Note] The corresponding argument was here");

    test!("--v (a: integer)
           local function foo(a) end";
          "[FuncDecl(Local, `foo`, [`a`: _ Integer], [])]");

    test!("(--v (a: const integer,
            --v  ...)
            --v -> string
            function(a, ...) end)()";
          "[Void(Func([`a`: Const Integer, ...: _] -> String, [])())]");

    test!("--v ()
           function foo() --> string
           end";
          "error";
          2: "[Error] Inline return type specification cannot appear \
                      with the function specification";
          1: "[Note] The function specification appeared here");

    test!("--v (a: integer)
           function foo(a) --: integer --> string
           end";
          "error";
          2: "[Error] Inline argument type specification cannot appear \
                      with the function specification";
          1: "[Note] The function specification appeared here";
          2: "[Error] Inline return type specification cannot appear \
                      with the function specification";
          1: "[Note] The function specification appeared here");

    test!("--v (a: integer, b: boolean)
           function foo(a, --: integer
                        b) --> string
           end";
          "error";
          2: "[Error] Inline argument type specification cannot appear \
                      with the function specification";
          1: "[Note] The function specification appeared here";
          3: "[Error] Inline return type specification cannot appear \
                      with the function specification";
          1: "[Note] The function specification appeared here");

    test!("--v ()";
          "error";
          1: "[Error] No function declaration after the function specification");

    test!("--v ()
           local v = 42";
          "error";
          1: "[Error] No function declaration after the function specification");

    test!("--v ()
           local v = 42
           --v ()";
          "error";
          1: "[Error] No function declaration after the function specification";
          3: "[Error] No function declaration after the function specification");

    test!("--v ()
           --# assume x: integer";
          "error";
          1: "[Error] No function declaration after the function specification");

    test!("--v ()
           for i = 1, 3 do end";
          "error";
          1: "[Error] No function declaration after the function specification");

    test!("f(--v ()
             g())";
          "error";
          1: "[Error] No function literal after the function specification");

    test!("function foo(a, --: integer
                        b, ...) --: string
           end";
          "[FuncDecl(Global, `foo`, [`a`: _ Integer, `b`, ...: String] -> _, [])]");

    test!("--v (a: integer, b: boolean, ...: string)
           function foo(a, b, ...) --: string
           end";
          "error";
          2: "[Error] Inline variadic argument type specification \
                      cannot appear with the function specification";
          1: "[Note] The corresponding argument in the function specification was here");

    test!("--v (a: integer, b: boolean)
           function foo(a, b, ...)
           end";
          "error";
          2: "[Error] Variadic arguments appear in the function \
                      but not in the function specification");

    test!("--v (a: integer, b: boolean, ...: string)
           function foo(a, b)
           end";
          "error";
          1: "[Error] Variadic arguments appear in the function \
                      specification but not in the function itself");

    test!("--v (a: integer, b: boolean, ...: string)
           function foo(a, b, ...)
           end";
          "[FuncDecl(Global, `foo`, [`a`: _ Integer, `b`: _ Boolean, ...: String], [])]");

    test!("--v (...: string)
           function foo(...)
           end";
          "[FuncDecl(Global, `foo`, [...: String], [])]");

    test!("--v (...: string)
           local function foo(...)
           end";
          "[FuncDecl(Local, `foo`, [...: String], [])]");

    test!("--# assume a: { integer, string
           --#                      boolean";
          "error";
          2: "[Fatal] expected `,`, `;` or `}`, got a name");

    test!("f([==== [";
          "error";
          1: "[Fatal] Opening long bracket should end with `]`");

    test!("f([====";
          "error";
          1: "[Fatal] Opening long bracket should end with `]`");

    test!("f([====[";
          "error";
          1: "[Fatal] Premature end of file in a long string";
          1: "[Note] The long string started here");

    test!("f([====[foo]";
          "error";
          1: "[Fatal] Premature end of file in a long string";
          1: "[Note] The long string started here");

    test!("--# assume p: [[xxx
           --# yyy]]";
          "error";
          1: "[Fatal] A newline is disallowed in a long string inside the meta block";
          1: "[Note] The meta block started here");

    test!("--# assume p: --[[xxx
           --# yyy]]";
          "error";
          1: "[Fatal] A newline is disallowed in a long string inside the meta block";
          1: "[Note] The meta block started here");

    test!("--# assume p: --[[xxx
                             yyy]]";
          "error";
          1: "[Fatal] A newline is disallowed in a long string inside the meta block";
          1: "[Note] The meta block started here");

    test!("f('foo\\xyz')";
          "error";
          1: "[Error] Unrecognized escape sequence in a string");

    test!("f('foo\\xyz', 'bar\\zyx')";
          "error";
          1: "[Error] Unrecognized escape sequence in a string";
          1: "[Error] Unrecognized escape sequence in a string");

    test!("f('foo\\";
          "error";
          1: "[Fatal] Premature end of file in a string";
          1: "[Note] The string started here");

    test!("f('foo";
          "error";
          1: "[Fatal] Premature end of file in a string";
          1: "[Note] The string started here");

    test!("f(0x12345678901234567)";
          "[Void(`f`(20988295476557332000))]"); // relies on std's correct f64 rounding

    test!("f(3e";
          "error";
          1: "[Fatal] Invalid number");

    test!("f(3e+";
          "error";
          1: "[Fatal] Invalid number");

    test!("f(~3)";
          "error";
          1: "[Fatal] Unexpected character");

    test!("f(@3)";
          "error";
          1: "[Fatal] Unexpected character");

    test!("--# assume p: integer f";
          "error";
          1: "[Error] Expected a newline, got a name");

    test!("for a of x";
          "error";
          1: "[Fatal] Expected `=`, `,`, `in` or `--:` after `for NAME`");

    test!("local (x, y) = (1, 2)";
          "error";
          1: "[Fatal] Expected a name or `function` after `local`");

    test!("function p(#";
          "error";
          1: "[Fatal] Expected a name, `)` or `...`");

    test!("function p(...) --: var integer
           end";
          "error";
          1: "[Error] Variadic argument specifier cannot have modifiers");

    test!("f({x#})";
          "error";
          1: "[Fatal] expected `,`, `;` or `}`, got `#`");

    test!("f(x.0)";
          "error";
          1: "[Fatal] Expected a name after `<expression> .`, got a number");

    test!("f(x:g - 1)";
          "error";
          1: "[Fatal] Expected argument(s) after `<expression> : <name>`, got `-`");

    test!("f(2, *3)";
          "error";
          1: "[Fatal] Expected an expression, got `*`");

    test!("f(2 + *3)";
          "error";
          1: "[Fatal] Expected an expression, got `*`");

    test!("f(2 .. *3)";
          "error";
          1: "[Fatal] Expected an expression, got `*`");

    test!("f(#*3)";
          "error";
          1: "[Fatal] Expected an expression, got `*`");

    test!("a, *b = 5";
          "error";
          1: "[Fatal] Expected a variable, got `*`");

    test!("--# assume x: #";
          "error";
          1: "[Error] Expected a single type, got `#`");

    test!("--# assume x: (...)";
          "error";
          1: "[Error] `...` should be preceded with a kind in the ordinary kinds";
          1: "[Error] Variadic argument can only be used as a function argument");

    test!("--# assume x: (string...)";
          "error";
          1: "[Error] Variadic argument can only be used as a function argument");

    test!("--# assume x: (integer, ...)";
          "error";
          1: "[Error] `...` should be preceded with a kind in the ordinary kinds";
          1: "[Error] Variadic argument can only be used as a function argument");

    test!("--# assume x: (integer, string)";
          "error";
          1: "[Error] Expected a single type, not type sequence");

    test!("--# assume x: (integer, string...)";
          "error";
          1: "[Error] Variadic argument can only be used as a function argument");

    test!("--# assume x: (integer, #)";
          "error";
          1: "[Fatal] Expected a kind, got `#`");

    test!("--# assume x: function () -> #";
          "error";
          1: "[Error] Expected a single type or type sequence, got `#`");

    test!("--# assume x: whatever";
          "[KailuaAssume(Local, `x`, _, `whatever`, None)]");

    test!("--# assume x: {x = integer #}";
          "error";
          1: "[Fatal] expected `,`, `;` or `}`, got `#`");

    test!("--# assume x: {integer #}";
          "error";
          1: "[Fatal] expected `,`, `;` or `}`, got `#`");

    test!("--# assume x: {x = integer, x = string}";
          "error";
          1: "[Error] Duplicate record field `x` in the type specification";
          1: "[Note] The first duplicate appeared here");

    test!("--# assume x: {x = integer,
           --#            x = string,
           --#            y = table,
           --#            x = boolean,
           --#            y = number}";
          "error";
          2: "[Error] Duplicate record field `x` in the type specification";
          1: "[Note] The first duplicate appeared here";
          4: "[Error] Duplicate record field `x` in the type specification";
          1: "[Note] The first duplicate appeared here";
          5: "[Error] Duplicate record field `y` in the type specification";
          3: "[Note] The first duplicate appeared here");

    test!("--# assume x: ? = hello
           --# assume y: \"bo\\gus\"
           f(";
          "error";
          1: "[Error] Expected a string after `assume <name> : <kind> =`, got a name";
          2: "[Error] Unrecognized escape sequence in a string";
          3: "[Fatal] Expected `)`, got the end of file");

    test!("--# assume x: integer | #";
          "error";
          1: "[Fatal] Expected a type, got `#`");

    test!("--# assume x: integer | () | nil
           f(";
          "error";
          1: "[Error] A sequence of types cannot be inside a union";
          2: "[Fatal] Expected `)`, got the end of file");

    test!("--# assume x: integer | (string,
           --#                      boolean) | nil
           f(";
          "error";
          1-2: "[Error] A sequence of types cannot be inside a union";
          3: "[Fatal] Expected `)`, got the end of file");

    test!("--# open lua51";
          "[KailuaOpen(`lua51`)]");

    test!("--# open
           --# open your heart";
          "error";
          2: "[Fatal] Expected a name, got a keyword `open`");

    test!("--# type int = integer
           --# assume x: {int}";
          "[KailuaType(`int`, Integer), \
            KailuaAssume(Local, `x`, _, Array(_ `int`), None)]");

    test!("--# type any = integer
           --# assume x: {any}";
          "error";
          1: "[Error] Cannot redefine a builtin type");

    test!("--# type int =
           --# assume x: {int}";
          "error";
          2: "[Error] Expected a single type, got a keyword `assume`";
          2: "[Error] Expected a newline, got a name");

    test!("--# type x = error";
          "[KailuaType(`x`, Error)]");

    test!("--# type x = error 'whatever'";
          "[KailuaType(`x`, Error(\"whatever\"))]");

    test!("--# type x = error | 'whatever'";
          "[KailuaType(`x`, Union([Error, String(\"whatever\")]))]");
}

