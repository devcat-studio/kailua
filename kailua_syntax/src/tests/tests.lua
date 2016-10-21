-- Basic tests for the Kailua parser.

--8<-- empty-1
--! []
--8<-- empty-2

--! []
--8<-- empty-3


--! []
--8<-- do-and-break
do break; end; break
--! [Do([Break]), Break]

--8<-- do-recover
do
    @ --@< Error: Unexpected character
    p()
end
q()
--! [Do([Void(`p`())]), Void(`q`())]

--8<-- while
while a do b() end c()
--! [While(`a`, [Void(`b`())]), Void(`c`())]

--8<-- while-recover-1
while @ --@< Error: Unexpected character
    x()
end --@< Error: Expected a keyword `do`, got a keyword `end`
f()
--! [While(Oops, []), Void(`f`())]

--8<-- while-recover-2
while @ do --@< Error: Unexpected character
           --@^ Error: Expected an expression, got a keyword `do`
    x()
end
f()
--! [While(Oops, [Void(`x`())]), Void(`f`())]

--8<-- while-recover-3
while do --@< Error: Expected an expression, got a keyword `do`
    x()
end
f()
--! [While(Oops, [Void(`x`())]), Void(`f`())]

--8<-- while-recover-4
while a do
    @ --@< Error: Unexpected character
end
f()
--! [While(`a`, []), Void(`f`())]

--8<-- top-level-end
f() end g() --@< Error: Expected a statement, got a keyword `end`
--! [Void(`f`()), Oops, Void(`g`())]

--8<-- top-level-closing-paren
f()) g() --@< Error: Expected a statement, got `)`
--! [Void(`f`()), Oops, Void(`g`())]

--8<-- paren-recover
f((a@)+(@b)*c) --@< Error: Unexpected character
               --@^ Error: Unexpected character
--! [Void(`f`((`a` + (`b` * `c`))))]

--8<-- func
function r(p) --[[...]] end
--! [FuncDecl(Global, `r`, [`p`] --> _, [])]

--8<-- local-func
local function r(p,...)

end
--! [FuncDecl(Local, `r`, [`p`, ...: _] --> _, [])]

--8<-- func-in-table
function a.b.c(p) --[[...]] end
--! [MethodDecl([`a`, `b`, `c`], None, [`p`] --> _, [])]

--8<-- method
function a.b:c(p) --[[...]] end
--! [MethodDecl([`a`, `b`, `c`], Some(self), [`p`] --> _, [])]

--8<-- local
local a, b
--! [Local([`a`, `b`], [])]

--8<-- local-comma-1
local a --: integer
    , b --: WHATEVER
--! [Local([`a`: _ Integer, `b`: _ Dynamic], [])]

--8<-- local-comma-2
local a, --: const table
      b
--! [Local([`a`: Const Table, `b`], [])]

--8<-- local-assign-1
local a = --: integer
          f()
--! [Local([`a`: _ Integer], [`f`()])]

--8<-- local-assign-2
local a --: integer
      = f()
--! [Local([`a`: _ Integer], [`f`()])]

--8<-- local-type-in-same-line
local a = f() --: integer
--! [Local([`a`: _ Integer], [`f`()])]

--8<-- local-type-in-same-line-2
local a, b = f() --: integer, string
--! [Local([`a`: _ Integer, `b`: _ String], [`f`()])]

--8<-- local-type-in-same-line-3
local a, b, c = f() --: integer, string, const {}
--! [Local([`a`: _ Integer, `b`: _ String, `c`: Const EmptyTable], [`f`()])]

--8<-- local-duplicate-types-in-same-line-1
local a, b, c --: const {}
              = f() --: integer, string, const {}
--@^ Error: The type specification cannot appear both at variable names and after the `local` declaration
f()
--! [Local([`a`, `b`, `c`: Const EmptyTable], [`f`()]), Void(`f`())]

--8<-- local-duplicate-types-in-same-line-2
local a, --: integer
      b, c = f() --: integer, string, const {}
--@^ Error: The type specification cannot appear both at variable names and after the `local` declaration
--! [Local([`a`: _ Integer, `b`, `c`], [`f`()])]

--8<-- local-duplicate-types-in-same-line-3
local a, --: integer
      b, --: string
      c = f() --: const {}
--@^ Error: The type specification cannot appear both at variable names and after the `local` declaration
--! [Local([`a`: _ Integer, `b`: _ String, `c`], [`f`()])]

--8<-- local-less-types-in-same-line-1
local a, b,
      c --@< Error: Excess type specifications in the variable names
      = f() --: integer, string
--! [Local([`a`, `b`, `c`], [`f`()])]

--8<-- local-less-types-in-same-line-2
local a,
      b,
      c,
      d --@^-< Error: Excess type specifications in the variable names
      = f() --: integer, string
--! [Local([`a`, `b`, `c`, `d`], [`f`()])]

--8<-- local-more-types-in-same-line-1
local a, b = f() --: integer,
                 --: string,
                 --: const {} --@< Error: Excess type specifications after the `local` declaration
--! [Local([`a`, `b`], [`f`()])]

--8<-- local-more-types-in-same-line-2
local a = f() --: integer,
              --: string,
              --: const {} --@^-< Error: Excess type specifications after the `local` declaration
--! [Local([`a`], [`f`()])]

--8<-- assign-1-1
a = f()
--! [Assign([`a`], [`f`()])]

--8<-- assign-1-3
a = f(), g(), h()
--! [Assign([`a`], [`f`(), `g`(), `h`()])]

--8<-- assign-2-1
a, b = f()
--! [Assign([`a`, `b`], [`f`()])]

--8<-- assign-2-3
a, b = f(), g(), h()
--! [Assign([`a`, `b`], [`f`(), `g`(), `h`()])]

--8<-- assign-index-name-1
ab.cde = f(), g(), h()
--! [Assign([(`ab`)["cde"]], [`f`(), `g`(), `h`()])]

--8<-- assign-index-name-2
ab.cde.fg = f(), g(), h()
--! [Assign([(`ab`["cde"])["fg"]], [`f`(), `g`(), `h`()])]

--8<-- assign-index-name-3
a, b.cde = f(), g(), h()
--! [Assign([`a`, (`b`)["cde"]], [`f`(), `g`(), `h`()])]

--8<-- assign-index-name-4
a, b.cde.fg = f(), g(), h()
--! [Assign([`a`, (`b`["cde"])["fg"]], [`f`(), `g`(), `h`()])]

--8<-- assign-index-exp-1
a[a*a] = f(), g(), h()
--! [Assign([(`a`)[(`a` * `a`)]], [`f`(), `g`(), `h`()])]

--8<-- assign-index-exp-2
a, b[a*a] = f(), g(), h()
--! [Assign([`a`, (`b`)[(`a` * `a`)]], [`f`(), `g`(), `h`()])]

--8<-- assign-index-func-exp-1
x().y[a*a] = f(), g(), h()
--! [Assign([(`x`()["y"])[(`a` * `a`)]], [`f`(), `g`(), `h`()])]

--8<-- assign-index-func-exp-2
a, x().y[a*a] = f(), g(), h()
--! [Assign([`a`, (`x`()["y"])[(`a` * `a`)]], [`f`(), `g`(), `h`()])]

--8<-- assign-type
a --: integer
  = f()
--! [Assign([`a`: _ Integer], [`f`()])]

--8<-- assign-type-2
a, --: integer
b  --: const string
= f(),
  g()
--! [Assign([`a`: _ Integer, `b`: Const String], [`f`(), `g`()])]

--8<-- assign-type-3
a,
b  --: const string
= f(),
  g()
--! [Assign([`a`, `b`: Const String], [`f`(), `g`()])]

--8<-- assign-type-in-same-line
a = f() --: integer
--! [Assign([`a`: _ Integer], [`f`()])]

--8<-- assign-type-in-same-line-2
a, b = f() --: integer, string
--! [Assign([`a`: _ Integer, `b`: _ String], [`f`()])]

--8<-- assign-type-in-same-line-3
a, b, c = f() --: integer, string, const {}
--! [Assign([`a`: _ Integer, `b`: _ String, `c`: Const EmptyTable], [`f`()])]

--8<-- assign-duplicate-types-in-same-line-1
a, b, c --: const {}
        = f() --: integer, string, const {}
--@^ Error: The type specification cannot appear both at the left hand side and after the assignment
--! [Assign([`a`, `b`, `c`: Const EmptyTable], [`f`()])]

--8<-- assign-duplicate-types-in-same-line-2
a, --: integer
b, c = f() --: integer, string, const {}
--@^ Error: The type specification cannot appear both at the left hand side and after the assignment
--! [Assign([`a`: _ Integer, `b`, `c`], [`f`()])]

--8<-- assign-duplicate-types-in-same-line-3
a, --: integer
b, --: string
c = f() --: const {}
--@^ Error: The type specification cannot appear both at the left hand side and after the assignment
--! [Assign([`a`: _ Integer, `b`: _ String, `c`], [`f`()])]

--8<-- assign-less-types-in-same-line-1
a, b,
c --@< Error: Excess type specifications in the left hand side
= f() --: integer, string
--! [Assign([`a`, `b`, `c`], [`f`()])]

--8<-- assign-less-types-in-same-line-2
a,
b,
c,
d --@^-< Error: Excess type specifications in the left hand side
= f() --: integer, string
--! [Assign([`a`, `b`, `c`, `d`], [`f`()])]

--8<-- assign-more-types-in-same-line-1
a, b = f() --: integer,
           --: string,
           --: const {} --@< Error: Excess type specifications after the assignment
--! [Assign([`a`, `b`], [`f`()])]

--8<-- assign-more-types-in-same-line-2
a = f() --: integer,
        --: string,
        --: const {} --@^-< Error: Excess type specifications after the assignment
--! [Assign([`a`], [`f`()])]

--8<-- func-argtype
local function r(p --: integer
                )

end
--! [FuncDecl(Local, `r`, [`p`: _ Integer] --> _, [])]

--8<-- func-argtype-rettype
local function r(p, q) --: integer --> string

end
--! [FuncDecl(Local, `r`, [`p`, `q`: _ Integer] --> String, [])]

--8<-- funccall
f()
--! [Void(`f`())]

--8<-- funccall-1
f(3)
--! [Void(`f`(3))]

--8<-- funccall-op-1
f(3+4)
--! [Void(`f`((3 + 4)))]

--8<-- funccall-op-2
f(3+4-5)
--! [Void(`f`(((3 + 4) - 5)))]

--8<-- funccall-op-3
f(3+4*5)
--! [Void(`f`((3 + (4 * 5))))]

--8<-- funccall-op-4
f((3+4)*5)
--! [Void(`f`(((3 + 4) * 5)))]

--8<-- funccall-op-5
f(2^3^4)
--! [Void(`f`((2 ^ (3 ^ 4))))]

--8<-- funccall-string
f'oo'
--! [Void(`f`("oo"))]

--8<-- funccall-table
f{a=1,[3.1]=4e5;[=[[[]]]=],}
--! [Void(`f`(Table([(Some("a"), 1), \
--!                  (Some(3.1), 400000), \
--!                  (None, "[[]]")])))]

--8<-- funccall-table-1
f{a=a, a}
--! [Void(`f`(Table([(Some("a"), `a`), (None, `a`)])))]

--8<-- funccall-table-2
f{a=a; a;}
--! [Void(`f`(Table([(Some("a"), `a`), (None, `a`)])))]

--8<-- comment-bracket-1
--[a]
do end--]]
--! [Do([])]

--8<-- comment-bracket-2
--[[a]
do end--]]
--! []

--8<-- directive-empty
--#
do end
--! [Do([])]

--8<-- directive-empty-1
--#
--! []
--8<-- directive-empty-1-noeol
--#
--! []

--&

--8<-- directive-empty-2-noeol
--#
--#
--! []

--&

--8<-- directive-empty-3-noeol
--#
--#
--#
--! []

--&

--8<-- directive-empty-2
--#
--#
do end
--! [Do([])]

--8<-- directive-empty-1-1-noeol
--#

--#
--! []

--&

--8<-- directive-empty-1-1-1-noeol
--#

--#

--#
--! []

--&

--8<-- directive-comment
--# --foo
do end
--! [Do([])]

--8<-- directive-long-comment
--# --[[foo]]
do end
--! [Do([])]

--8<-- directive-long-comment-multiline
--# --[[foo --@< Error: A newline is disallowed in a long comment inside the meta block
--# --foo]] --@^ Note: The meta block started here
do end
--! [Do([])]

--8<-- assume
--# assume a: string
--! [KailuaAssume(Local, `a`, _, String)]

--8<-- assume-and-empty
--# assume a: string
--#
--! [KailuaAssume(Local, `a`, _, String)]

--8<-- assume-multiline
--# assume a:
--#   string
--! [KailuaAssume(Local, `a`, _, String)]

--8<-- assume-global-multiline
--# assume global a:
--#   string
--! [KailuaAssume(Global, `a`, _, String)]

--8<-- assume-global-global
--# assume global global --@< Fatal: Expected a name, got a keyword `global`
--! error

--8<-- assume-incomplete
--# assume a:
--# assume b: string --@< Error: Expected a single type, got a keyword `assume`
--! [KailuaAssume(Local, `a`, _, Oops)]

--8<-- assume-global-local
--# assume global a: {x=string}
--# assume b: {y=string}
--! [KailuaAssume(Global, `a`, _, Record(["x": _ String])), \
--!  KailuaAssume(Local, `b`, _, Record(["y": _ String]))]

--8<-- assume-assume
--# assume assume: WHATEVER --@< Fatal: Expected a name, got a keyword `assume`
--! error

--8<-- assume-quoted-assume
--# assume `assume`: WHATEVER
--! [KailuaAssume(Local, `assume`, _, Dynamic)]

--8<-- quoted-assume-assume
--# `assume` `assume`: WHATEVER --@< Error: Expected a newline, got a name
--! []

--8<-- assume-builtin-rejected
--# assume a: WHATEVER = "foo" --@< Error: Expected a newline, got `=`
--# assume b: WHATEVER
--! [KailuaAssume(Local, `a`, _, Dynamic)]

--8<-- kind-table
local x --: {b=string, a=integer, c=const {d=const {}}}
--! [Local([`x`: _ Record(["b": _ String, "a": _ Integer, \
--!                        "c": Const Record(["d": Const EmptyTable])])], [])]

--8<-- kind-func
local x --: function
--! [Local([`x`: _ Function], [])]

--8<-- kind-func-or-string-opt
local x --: function | string?
--! [Local([`x`: _ Union([Function, String?])], [])]

--8<-- kind-func-or-string-paren-opt
local x --: (function | string)?
--! [Local([`x`: _ Union([Function, String])?], [])]

--8<-- kind-func-0
local x --: function()
--! [Local([`x`: _ Func(() --> ())], [])]

--8<-- kind-func-0-explicit
local x --: function()-->()
--! [Local([`x`: _ Func(() --> ())], [])]

--8<-- kind-func-2
local x --: function(integer, boolean...)-->string?
--! [Local([`x`: _ Func((Integer, Boolean...) --> String?)], [])]

--8<-- kind-func-2-seq
local x --: function(integer, boolean...)-->(string?, WHATEVER...)
--! [Local([`x`: _ Func((Integer, Boolean...) --> (String?, Dynamic...))], [])]

--8<-- kind-func-seq-without-parens
local x --: function () --> integer... --@< Error: Expected a newline, got `...`
--! [Local([`x`: _ Func(() --> Integer)], [])]

--8<-- kind-func-or-without-parens
local x --: function (boolean...) | string? --@< Error: Expected a newline, got `|`
--! [Local([`x`: _ Func((Boolean...) --> ())], [])]

--8<-- kind-func-or
local x --: (function (boolean...)) | string?
--! [Local([`x`: _ Union([Func((Boolean...) --> ()), String?])], [])]

--8<-- kind-any-func
local x --: `function`
--! [Local([`x`: _ Function], [])]

--8<-- kind-any-func-recover
local x --: `function`() --@< Error: Expected a newline, got `(`
local                    --@< Fatal: Expected a name or `function` after `local`, got the end of file
--! error

--&

--8<-- kind-thread
local x --: thread
--! [Local([`x`: _ Thread], [])]

--8<-- kind-userdata
local x --: userdata
--! [Local([`x`: _ UserData], [])]

--8<-- kind-seq-outside-func
local x --: (integer, string) --@< Error: Expected a single type, not type sequence
--! [Local([`x`: _ Oops], [])]

--8<-- kind-paren
local x --: (integer)
--! [Local([`x`: _ Integer], [])]

--8<-- kind-paren-opt
local x --: (integer)?
--! [Local([`x`: _ Integer?], [])]

--8<-- kind-paren-opt-multiline-1
local x --:
        --: (integer)?
--! [Local([`x`: _ Integer?], [])]

--8<-- kind-paren-opt-multiline-2
local x --: (
        --:   integer
        --: )?
--! [Local([`x`: _ Integer?], [])]

--8<-- kind-paren-multiline-recover
local x --: (
        --:   integer --@< Error: Expected `)`, got a newline
--! [Local([`x`: _ Oops], [])]

--&

--8<-- kind-table-empty
local x --: {}
--! [Local([`x`: _ EmptyTable], [])]

--8<-- kind-rec
local x --: {a = const function (), b = string,
        --:  c = const function (string, integer) --> number}?
--! [Local([`x`: _ Record(["a": Const Func(() --> ()), \
--!                        "b": _ String, \
--!                        "c": Const Func((String, Integer) --> Number)\
--!                       ])?], [])]

--8<-- kind-tuple
local x --: {const function (); string;
        --:  const function (string, integer) --> number;
        --: }?
--! [Local([`x`: _ Tuple([Const Func(() --> ()), \
--!                       _ String, \
--!                       Const Func((String, Integer) --> Number)\
--!                      ])?], [])]

--8<-- kind-tuple-1
local x --: {WHATEVER}
--! [Local([`x`: _ Tuple([_ Dynamic])], [])]

--8<-- kind-tuple-1-comma
local x --: {WHATEVER,}
--! [Local([`x`: _ Tuple([_ Dynamic])], [])]

--8<-- kind-tuple-2-comma
local x --: {WHATEVER,WHATEVER}
--! [Local([`x`: _ Tuple([_ Dynamic, _ Dynamic])], [])]

--8<-- kind-tuple-2-semicolon
local x --: {WHATEVER;WHATEVER;}
--! [Local([`x`: _ Tuple([_ Dynamic, _ Dynamic])], [])]

--8<-- kind-array
local x --: vector<integer>
local y --: vector<const integer>
--! [Local([`x`: _ Array(_ Integer)], []), \
--!  Local([`y`: _ Array(Const Integer)], [])]

--8<-- kind-array-recover-1
local x --: vector<integer, string> --@< Error: `vector` type needs a single type parameter
local y --: vector<integer>
--! [Local([`x`: _ Oops], []), \
--!  Local([`y`: _ Array(_ Integer)], [])]

--8<-- kind-array-recover-2
-- XXX excess >/>> expectation error is hard to fix without a special nesting
local x --: vector<integer, , string> --@< Error: Expected a single type, got `,`
                                      --@^-< Error: Expected `>` or `>>`, got a newline
                                      --@^^ Error: `vector` type needs a single type parameter
local y --: vector<boolean,> --@< Error: Expected a single type, got `>`
                             --@^-< Error: Expected `>` or `>>`, got a newline
                             --@^^ Error: `vector` type needs a single type parameter
local z --: vector<integer>
local w --: vector<> --@< Error: Expected a single type, got `>`
                     --@^-< Error: Expected `>` or `>>`, got a newline
--! [Local([`x`: _ Oops], []), \
--!  Local([`y`: _ Oops], []), \
--!  Local([`z`: _ Array(_ Integer)], []), \
--!  Local([`w`: _ Array(_ Oops)], [])]

--8<-- kind-array-recover-3
local x --: vector<integer, --@<-v Error: Expected a single type, got a newline
                            --@^-< Error: Expected `>` or `>>`, got a newline
                            --@^^ Error: `vector` type needs a single type parameter
local y --: vector<boolean  --@<-v Error: Expected `>` or `>>`, got a newline
local z --: vector<integer>
local w --: vector          --@<-v Error: Expected a list of type parameters, got a newline
--! [Local([`x`: _ Oops], []), \
--!  Local([`y`: _ Array(_ Boolean)], []), \
--!  Local([`z`: _ Array(_ Integer)], []), \
--!  Local([`w`: _ Oops], [])]

--8<-- kind-array-reserved
local x --: `vector` --@< Error: The type name `vector` is reserved and cannot be used
local y --: `vector`<const integer> --@< Error: The type name `vector` is reserved and cannot be used
                                    --@^ Error: Expected a newline, got `<`
--! [Local([`x`: _ Oops], []), \
--!  Local([`y`: _ Oops], [])]

--8<-- kind-map
local x --: map<string, const integer>
--! [Local([`x`: _ Map(String, Const Integer)], [])]

--8<-- kind-map-or-1
local x --: map<string, integer|boolean>
--! [Local([`x`: _ Map(String, _ Union([Integer, Boolean]))], [])]

--8<-- kind-map-or-2
local x --: map<string|boolean, integer|boolean>
--! [Local([`x`: _ Map(Union([String, Boolean]), _ Union([Integer, Boolean]))], [])]

--8<-- kind-map-opt
local x --: map<string?, integer?>
--! [Local([`x`: _ Map(String?, _ Integer?)], [])]

--8<-- kind-map-recover-1
local x --: map<integer, string, boolean> --@< Error: `map` type needs two type parameters
local y --: map<integer, string>
local z --: map<integer> --@< Error: `map` type needs two type parameters
--! [Local([`x`: _ Oops], []), \
--!  Local([`y`: _ Map(Integer, _ String)], []), \
--!  Local([`z`: _ Oops], [])]

--8<-- kind-map-recover-2
-- XXX excess >/>> expectation error is hard to fix without a special nesting
local x --: map<integer, , string> --@< Error: Expected a single type, got `,`
                                   --@^-< Error: Expected `>` or `>>`, got a newline
local y --: map<boolean,> --@< Error: Expected a single type, got `>`
                          --@^-< Error: Expected `>` or `>>`, got a newline
local z --: map<integer, string>
--! [Local([`x`: _ Map(Integer, _ Oops)], []), \
--!  Local([`y`: _ Map(Boolean, _ Oops)], []), \
--!  Local([`z`: _ Map(Integer, _ String)], [])]

--8<-- kind-map-recover-3
local x --: map<const integer, string> --@< Error: The first type parameter of `map` type cannot have modifiers
--! [Local([`x`: _ Map(Integer, _ String)], [])]

--8<-- kind-nested-table
local x --: map<integer, const vector<map<string, {integer, integer}?>>>
--! [Local([`x`: _ Map(Integer, \
--!                    Const Array(_ Map(String, \
--!                                      _ Tuple([_ Integer, _ Integer])?)))], [])]

--8<-- kind-attr-1
local x --: [builtin] string
--! [Local([`x`: _ [`builtin`] String], [])]

--8<-- kind-attr-2
local x --: [builtin] function(any)
--! [Local([`x`: _ [`builtin`] Func((Any) --> ())], [])]

--8<-- kind-attr-paren
local x --: ([builtin] (string))
--! [Local([`x`: _ [`builtin`] String], [])]

--8<-- kind-attr-empty
local x --: [] string --@< Error: Expected a name, got `]`
--! [Local([`x`: _ String], [])]

--8<-- kind-attr-wrong
local x --: [built-in] string --@< Error: Expected `]`, got `-`
local y --: [built_in] string
--! [Local([`x`: _ String], []), Local([`y`: _ [`built_in`] String], [])]

--8<-- kind-attr-wrong-recover
local x --: [built-in(function(i) return _G[i] end] string --@< Error: Expected `]`, got `-`
local y --: [built_in] string
--! [Local([`x`: _ String], []), Local([`y`: _ [`built_in`] String], [])]

--8<-- kind-attr-keyword
local x --: [type] function(any)
--! [Local([`x`: _ [`type`] Func((Any) --> ())], [])]

--8<-- kind-attr-dup
local x --: [builtin] [builtin] string --@< Error: Expected a single type, got `[`
--! [Local([`x`: _ Oops], [])]

--8<-- kind-attr-seq
local x --: function() --> [builtin] (string, string)
--@^ Error: Cannot attach the type attribute (like [name]) to the type sequence
--! [Local([`x`: _ Func(() --> (String, String))], [])]

--8<-- funcspec
--v function()
function foo() end
--! [FuncDecl(Global, `foo`, [], [])]
-- note that the return is specified (not `_`)

--8<-- funcspec-more-arity-1
--v function(a: integer,
--v          b: string) --@^-< Error: Excess arguments in the function specification
function foo() end
--! [FuncDecl(Global, `foo`, [`a`: _ Integer, `b`: _ String], [])]

--8<-- funcspec-more-arity-2
--v function(a: integer,
--v          b: string) --@< Error: Excess arguments in the function specification
function foo(a) end
--! [FuncDecl(Global, `foo`, [`a`: _ Integer, `b`: _ String], [])]

--8<-- funcspec-wrong-name
--v function(a: integer)
function foo(b) end --@< Error: Mismatching argument name in the function specification
                    --@^^ Note: The corresponding argument was here
--! [FuncDecl(Global, `foo`, [`a`: _ Integer], [])]

--8<-- funcspec-less-arity-1
--v function()
function foo(a,
             b) end --@^-< Error: Excess arguments in the function declaration
--! [FuncDecl(Global, `foo`, [], [])]

--8<-- funcspec-less-arity-2
--v function(a: integer)
function foo(a,
             b) end --@< Error: Excess arguments in the function declaration
--! [FuncDecl(Global, `foo`, [`a`: _ Integer], [])]

--8<-- funcspec-swapped-name
--v function(a: integer, b: integer)
function foo(b, a) end --@< Error: Mismatching argument name in the function specification
                       --@^^ Note: The corresponding argument was here
                       --@^^ Error: Mismatching argument name in the function specification
                       --@^^^^ Note: The corresponding argument was here
--! [FuncDecl(Global, `foo`, [`a`: _ Integer, `b`: _ Integer], [])]

--8<-- funcspec-1
--v function(a: integer)
local function foo(a) end
--! [FuncDecl(Local, `foo`, [`a`: _ Integer], [])]

--8<-- funcspec-1-seq-1
--v function(a: integer) --> (WHATEVER...)
local function foo(a) end
--! [FuncDecl(Local, `foo`, [`a`: _ Integer] --> [Dynamic...], [])]

--8<-- funcspec-1-seq-2
--v function(a: integer) --> (string, WHATEVER...)
local function foo(a) end
--! [FuncDecl(Local, `foo`, [`a`: _ Integer] --> [String, Dynamic...], [])]

--8<-- funcspec-inline
(--v function(a: const integer,
 --v          ...)
 --v         --> string
 function(a, ...) end)()
--! [Void(Func([`a`: Const Integer, ...: _] --> String, [])())]

--8<-- funcspec-attr-1
--v [no_check] function()
function foo() end
--! [FuncDecl(Global, `foo`, [`no_check`] [], [])]

--8<-- funcspec-attr-2
--v [no_check]
--v function()
function foo() end
--! [FuncDecl(Global, `foo`, [`no_check`] [], [])]

--8<-- funcspec-attr-multi-1
--v [no_check] [self_destruct] function()
function foo() end
--! [FuncDecl(Global, `foo`, [`no_check`] [`self_destruct`] [], [])]

--8<-- funcspec-attr-multi-2
--v [no_check]
--v [self_destruct]
--v function()
function foo() end
--! [FuncDecl(Global, `foo`, [`no_check`] [`self_destruct`] [], [])]

--8<-- funcspec-attr-only
--v [no_check]
function foo() end
--! [FuncDecl(Global, `foo`, [`no_check`] [] --> _, [])]
-- note the trailing `_`, which indicates that there was no pre-signature

--8<-- funcspec-attr-incomplete
--v [no_check --@<-v Error: Expected `]`, got a newline
function foo() end
--! [FuncDecl(Global, `foo`, [] --> _, [])]

--8<-- funcspec-no-empty
--v --@<-v Error: Expected a keyword `function`, got a newline
function foo() end
--! error

--8<-- funcspec-no-bare-parens
--v () --@< Error: Expected a keyword `function`, got `(`
function foo() end
--! error

--8<-- rettype-none
function foo() --> ()
end
--! [FuncDecl(Global, `foo`, [], [])]

--8<-- rettype-string
function foo() --> string
end
--! [FuncDecl(Global, `foo`, [] --> String, [])]

--8<-- rettype-string-parens
function foo() --> ((((string))))
end
--! [FuncDecl(Global, `foo`, [] --> String, [])]

--8<-- rettype-seq-1
function foo() --> (WHATEVER...)
end
--! [FuncDecl(Global, `foo`, [] --> [Dynamic...], [])]

--8<-- rettype-seq-2
function foo() --> (string, WHATEVER...)
end
--! [FuncDecl(Global, `foo`, [] --> [String, Dynamic...], [])]

--8<-- funcspec-and-rettype
--v function()
function foo() --> string
    --@^ Error: Inline return type specification cannot appear with the function specification
end --@^^^ Note: The function specification appeared here
--! [FuncDecl(Global, `foo`, [], [])]

--8<-- funcspec-and-argtype-rettype-1
--v function(a: integer)
function foo(a) --: integer --> string
    --@^ Error: Inline argument type specification cannot appear with the function specification
end --@^^^ Note: The function specification appeared here
    --@^^^ Error: Inline return type specification cannot appear with the function specification
    --@^^^^^ Note: The function specification appeared here
--! [FuncDecl(Global, `foo`, [`a`: _ Integer], [])]

--8<-- funcspec-and-argtype-rettype-2
--v function(a: integer, b: boolean)
function foo(a, --: integer --@< Error: Inline argument type specification cannot appear with the function specification
                            --@^^ Note: The function specification appeared here
             b) --> string  --@< Error: Inline return type specification cannot appear with the function specification
                            --@^^^^ Note: The function specification appeared here
end
--! [FuncDecl(Global, `foo`, [`a`: _ Integer, `b`: _ Boolean], [])]

--8<-- funcspec-before-nothing
--v function() --@< Error: No function declaration after the function specification
--! []

--8<-- funcspec-before-local
--v function() --@< Error: No function declaration after the function specification
local v = 42
--! [Local([`v`], [42])]

--8<-- funcspec-before-local-recover
--v function() --@< Error: No function declaration after the function specification
local v = 42
--v function() --@< Error: No function declaration after the function specification
--! [Local([`v`], [42])]

--8<-- funcspec-before-assume
--v function() --@< Error: No function declaration after the function specification
--# assume x: integer
--! [KailuaAssume(Local, `x`, _, Integer)]

--8<-- funcspec-before-for
--v function() --@< Error: No function declaration after the function specification
for i = 1, 3 do end
--! [For(`i`, 1, 3, None, [])]

--8<-- funcspec-before-expr-inline
f(--v function() --@< Error: No function literal after the function specification
  g())
--! [Void(`f`(`g`()))]

--8<-- argtype-inline
function foo(a, --: integer
             b, ...) --: string
end
--! [FuncDecl(Global, `foo`, [`a`: _ Integer, `b`, ...: String] --> _, [])]

--8<-- funcspec-and-argtype
--v function(a: integer, b: boolean, ...: string)
function foo(a, b, ...) --: string
    --@^ Error: Inline variadic argument type specification cannot appear with the function specification
    --@^^^ Note: The corresponding argument in the function specification was here
end
--! [FuncDecl(Global, `foo`, [`a`: _ Integer, `b`: _ Boolean, ...: String], [])]

--8<-- funcspec-less-varargs
--v function(a: integer, b: boolean)
function foo(a, b, ...)
    --@^ Error: Variadic arguments appear in the function but not in the function specification
end
--! [FuncDecl(Global, `foo`, [`a`: _ Integer, `b`: _ Boolean], [])]

--8<-- funcspec-more-varargs
--@v Error: Variadic arguments appear in the function specification but not in the function itself
--v function(a: integer, b: boolean, ...: string)
function foo(a, b)
end
--! [FuncDecl(Global, `foo`, [`a`: _ Integer, `b`: _ Boolean, ...: String], [])]

--8<-- funcspec-varargs
--v function(a: integer, b: boolean, ...: string)
function foo(a, b, ...)
end
--! [FuncDecl(Global, `foo`, [`a`: _ Integer, `b`: _ Boolean, ...: String], [])]

--8<-- funcspec-varargs-0
--v function(...: string)
function foo(...)
end
--! [FuncDecl(Global, `foo`, [...: String], [])]

--8<-- local-funcspec-varargs-0
--v function(...: string)
local function foo(...)
end
--! [FuncDecl(Local, `foo`, [...: String], [])]

--8<-- funcspec-method-no-self-1
--v function() --@< Error: The first argument in the function specification for a method is not `self`
function foo:bar() end
--! [MethodDecl([`foo`, `bar`], Some(self), [], [])]

--8<-- funcspec-method-no-self-2
--@v-vv Error: The first argument in the function specification for a method is not `self`
--v function(x: integer,
--v          y: string)
function foo:bar(x, y) end
--! [MethodDecl([`foo`, `bar`], Some(self), [`x`: _ Integer, `y`: _ String], [])]

--8<-- funcspec-method-self
--v function(self)
function foo:bar() end
--! [MethodDecl([`foo`, `bar`], Some(self), [], [])]

--8<-- funcspec-method-self-typed
--v function(self: table)
function foo:bar() end
--! [MethodDecl([`foo`, `bar`], Some(self: _ Table), [], [])]

--8<-- funcspec-method-self-typed-with-modf
--v function(self: const table)
function foo:bar() end
--! [MethodDecl([`foo`, `bar`], Some(self: Const Table), [], [])]

--8<-- funcspec-non-method-self-1
--v function(self, x: integer) --@< Error: Arguments in the function specification are missing their types
                               --@^ Error: Excess arguments in the function specification
function foo.bar(x) end --@< Error: Mismatching argument name in the function specification
                        --@^^^ Note: The corresponding argument was here
--! [MethodDecl([`foo`, `bar`], None, [`self`, `x`: _ Integer], [])]

--8<-- funcspec-non-method-self-2
--v function(self, x: integer) --@< Error: Arguments in the function specification are missing their types
function foo.bar(self, x) end
--! [MethodDecl([`foo`, `bar`], None, [`self`, `x`: _ Integer], [])]

--8<-- funcspec-non-method-self-typed-1
--v function(self: table, x: integer) --@< Error: Excess arguments in the function specification
function foo.bar(x) end --@< Error: Mismatching argument name in the function specification
                        --@^^ Note: The corresponding argument was here
--! [MethodDecl([`foo`, `bar`], None, [`self`: _ Table, `x`: _ Integer], [])]

--8<-- funcspec-non-method-self-typed-2
--v function(self: table, x: integer)
function foo.bar(self, x) end
--! [MethodDecl([`foo`, `bar`], None, [`self`: _ Table, `x`: _ Integer], [])]

--8<-- assume-multiline-recover
--# assume a: { integer, string
--#                      boolean --@< Error: Expected `,`, `;` or `}`, got a name
--! [KailuaAssume(Local, `a`, _, Tuple([_ Integer, _ String]))]

--8<-- long-string-incomplete-1
f([==== [ --@< Error: Opening long bracket in a string should end with `[`
          --@^ Error: Expected `)`, got `[`
--! [Void(`f`(""))]

--&
) -- highlighting fix

--8<-- long-string-incomplete-2
f([==== --@< Error: Opening long bracket in a string should end with `[`
        --@^ Error: Expected `)`, got the end of file
--! [Void(`f`(""))]

--&
) -- highlighting fix

--8<-- long-string-incomplete-3
f([====[ --@< Error: Premature end of file in a long string
         --@^ Note: The long string started here
         --@^^ Error: Expected `)`, got the end of file
--! [Void(`f`(""))]

--&
]====]) -- highlighting fix

--8<-- long-string-incomplete-4
f([====[foo] --@< Error: Premature end of file in a long string
             --@^ Note: The long string started here
             --@^^ Error: Expected `)`, got the end of file
--! [Void(`f`("foo"))]

--&
]====]) -- highlighting fix

--8<-- long-comment-incomplete-1
-- unlike long-string-incomplete-1 this is not an error!
f(--[==== [
)
--! [Void(`f`())]

--8<-- long-comment-incomplete-1-eof
--[==== [
--! []

--8<-- long-comment-incomplete-2
-- unlike long-string-incomplete-2 this is not an error!
f(--[====
)
--! [Void(`f`())]

--8<-- long-comment-incomplete-2-eof
--[====
--! []

--8<-- long-comment-incomplete-3
--@vvv Error: Premature end of file in a long comment
--@v Note: The long comment started here
f(--[====[
) --@< Error: Expected `)`, got the end of file
--! [Void(`f`())]

--&
]====] -- highlighting fix

--8<-- long-comment-incomplete-4
--@vvv Error: Premature end of file in a long comment
--@v Note: The long comment started here
f(--[====[foo]
) --@< Error: Expected `)`, got the end of file
--! [Void(`f`())]

--&
]====] -- highlighting fix

--8<-- meta-eof
--@v Error: Expected a single type, got a newline
--# assume p:
--&
--! [KailuaAssume(Local, `p`, _, Oops)]

--8<-- meta-long-string
--# assume p: [[xxx   --@< Error: A newline is disallowed in a long string inside the meta block
--#             yyy]] --@^ Note: The meta block started here
                      --@^ Error: Expected a newline, got a name
f()
--! [KailuaAssume(Local, `p`, _, String("xxx   ")), Void(`f`())]

--8<-- meta-long-comment-1
--# assume p: --[[xxx   --@< Error: A newline is disallowed in a long comment inside the meta block
--#               yyy]] --@^ Note: The meta block started here
                        --@^ Error: Expected a newline, got `]`
f()
--! [KailuaAssume(Local, `p`, _, `yyy`), Void(`f`())]

--8<-- meta-long-comment-2
--# assume p: --[[xxx   --@< Error: A newline is disallowed in a long comment inside the meta block
                  yyy]] --@^ Note: The meta block started here
                        --@^^-^ Error: Expected a single type, got a newline
                        --@^^ Error: Expected `=`, got `]`
--! error

--8<-- string-multi-line
f('foo\
bar')
--! [Void(`f`("foo\nbar"))]

--8<-- string-multi-line-recover
f('foo)
) --@< Error: Unescaped newline in a string
  --@^^ Note: The string started here
g()
--! [Void(`f`("foo)")), Void(`g`())]

--&
') -- highlighting fix

--8<-- string-wrong-escape
f('foo\xyz') --@< Error: Unrecognized escape sequence in a string
--! [Void(`f`("fooyz"))]

--8<-- string-wrong-escape-recover
f('foo\xyz', 'bar\zyx') --@< Error: Unrecognized escape sequence in a string
                        --@^ Error: Unrecognized escape sequence in a string
--! [Void(`f`("fooyz", "baryx"))]

--8<-- string-incomplete-escape
f('foo\ --@< Error: Premature end of file in a string
        --@^ Note: The string started here
        --@^^ Error: Expected `)`, got the end of file
--! [Void(`f`("foo"))]

--&
') -- highlighting fix

--8<-- string-incomplete
--@vvv Error: Premature end of file in a string
--@vv Note: The string started here
--@v Error: Expected `)`, got the end of file
f('foo
--&
--! [Void(`f`("foo"))]

') -- highlighting fix

--8<-- number-long
f(0x12345678901234567)
--! [Void(`f`(20988295476557332000))]
-- relies on std's correct f64 rounding

--8<-- number-incomplete-exp-1
f(3e --@< Error: Invalid number
     --@^ Error: Expected `)`, got the end of file
--! [Void(`f`())]

--&
) -- highlighting fix

--8<-- number-incomplete-exp-1-recover
f(3e) --@< Error: Invalid number
--! [Void(`f`())]

--8<-- number-incomplete-exp-2
f(3e+ --@< Error: Invalid number
      --@^ Error: Expected `)`, got `+`
--! [Void(`f`())]

--&
) -- highlighting fix

--8<-- invalid-char-1
f(~3) --@< Error: Unexpected character
--! [Void(`f`(3))]

--8<-- invalid-char-2
f(@3) --@< Error: Unexpected character
--! [Void(`f`(3))]

--8<-- assume-excess
--# assume p: integer f --@< Error: Expected a newline, got a name
f()
--# assume q: integer g --@< Error: Expected a newline, got a name
--! [KailuaAssume(Local, `p`, _, Integer), Void(`f`()), KailuaAssume(Local, `q`, _, Integer)]

--8<-- for-of
for a of x --@< Error: Expected `=`, `,`, `in` or `--:` after `for NAME`, got a name
--! [Oops]

--8<-- local-seq
local (x, y) = (1, 2) --@< Fatal: Expected a name or `function` after `local`, got `(`
--! error

--8<-- func-args-invalid-char
function p(# --@< Fatal: Expected an argument name, `)` or `...`, got `#`
--! error

--&
) -- highlighting fix

--8<-- argtype-slot
function p(...) --: const integer --@< Error: Variadic argument specifier cannot have modifiers
end
--! [FuncDecl(Global, `p`, [...: Integer] --> _, [])]

--8<-- table-invalid-char
f({x#}) --@< Error: Expected `,`, `;` or `}`, got `#`
g({y#}) --@< Error: Expected `,`, `;` or `}`, got `#`
--! [Void(`f`(Table([(None, `x`)]))), \
--!  Void(`g`(Table([(None, `y`)])))]

--8<-- index-with-number
f(x.0) --@< Fatal: Expected a name after `<expression> .`, got a number
--! error

--8<-- methodcall-no-args
f(x:g - 1) --@< Fatal: Expected argument(s) after `<expression> : <name>`, got `-`
--! error

--8<-- funccall-invalid-char
f(2, *3) --@< Error: Expected an expression, got `*`
--! [Void(`f`(2))]

--8<-- op-invalid-char-1
f(2 + *3) --@< Fatal: Expected an expression, got `*`
--! error

--8<-- op-invalid-char-2
f(2 .. *3) --@< Fatal: Expected an expression, got `*`
--! error

--8<-- op-invalid-char-3
f(#*3) --@< Fatal: Expected an expression, got `*`
--! error

--8<-- lval-invalid-char
a, *b = 5 --@< Fatal: Expected a left-hand-side expression, got `*`
--! error

--8<-- assume-invalid-char
--# assume x: #foo --@< Error: Expected a single type, got `#`
f()
--! [KailuaAssume(Local, `x`, _, Oops), Void(`f`())]

--8<-- assume-seq-varargs-1
--# assume x: (...) --@< Error: `...` should be preceded with a kind in the ordinary kinds
                    --@^ Error: Expected a single type, not type sequence
--! [KailuaAssume(Local, `x`, _, Oops)]

--8<-- assume-seq-1
--# assume x: (string...) --@< Error: Expected a single type, not type sequence
--! [KailuaAssume(Local, `x`, _, Oops)]

--8<-- assume-seq-varargs-2
--# assume x: (integer, ...) --@< Error: `...` should be preceded with a kind in the ordinary kinds
                             --@^ Error: Expected a single type, not type sequence
--! [KailuaAssume(Local, `x`, _, Oops)]

--8<-- assume-seq-2
--# assume x: (integer, string) --@< Error: Expected a single type, not type sequence
--! [KailuaAssume(Local, `x`, _, Oops)]

--8<-- assume-seq-varargs-3
--# assume x: (integer, string...) --@< Error: Expected a single type, not type sequence
--! [KailuaAssume(Local, `x`, _, Oops)]

--8<-- assume-seq-invalid-char
--# assume x: (integer, #) --@< Fatal: Expected a type, got `#`
--! error

--8<-- assume-func-invalid-char
--# assume x: function () --> #foo --@< Error: Expected a single type or type sequence, got `#`
                                   --@^ Error: Expected a newline, got a name
--! [KailuaAssume(Local, `x`, _, Func(() --> Oops))]

--8<-- assume-named
--# assume x: whatever
--! [KailuaAssume(Local, `x`, _, `whatever`)]

--8<-- assume-rec-invalid-char
--# assume x: {x = integer #} --@< Error: Expected `,`, `;` or `}`, got `#`
--# assume y: {y = integer #} --@< Error: Expected `,`, `;` or `}`, got `#`
--! [KailuaAssume(Local, `x`, _, Record(["x": _ Integer])), \
--!  KailuaAssume(Local, `y`, _, Record(["y": _ Integer]))]

--8<-- assume-tuple-invalid-char
--# assume x: {integer, integer #} --@< Error: Expected `,`, `;` or `}`, got `#`
--# assume y: {integer, integer #} --@< Error: Expected `,`, `;` or `}`, got `#`
--! [KailuaAssume(Local, `x`, _, Tuple([_ Integer, _ Integer])), \
--!  KailuaAssume(Local, `y`, _, Tuple([_ Integer, _ Integer]))]

--8<-- assume-rec-duplicate-name
--# assume x: {x = integer, x = string} --@< Error: Duplicate record field `x` in the type specification
                                        --@^ Note: The first duplicate appeared here
--! [KailuaAssume(Local, `x`, _, Record(["x": _ Integer, "x": _ String]))]

--8<-- assume-rec-duplicate-name-recover
--# assume x: {x = integer,
--#            x = string,  --@< Error: Duplicate record field `x` in the type specification
--#                         --@^^ Note: The first duplicate appeared here
--#            y = table,
--#            x = boolean, --@< Error: Duplicate record field `x` in the type specification
--#                         --@^^^^^ Note: The first duplicate appeared here
--#            y = number}  --@< Error: Duplicate record field `y` in the type specification
                            --@^^^^ Note: The first duplicate appeared here
--! [KailuaAssume(Local, `x`, _, Record(["x": _ Integer, \
--!                                      "x": _ String, \
--!                                      "y": _ Table, \
--!                                      "x": _ Boolean, \
--!                                      "y": _ Number]))]

--8<-- assume-builtin-and-literal-recover
--# assume x: WHATEVER = hello --@< Error: Expected a newline, got `=`
--# assume y: "bo\gus"         --@< Error: Unrecognized escape sequence in a string
f(                             --@< Error: Expected `)`, got the end of file
--! [KailuaAssume(Local, `x`, _, Dynamic), Void(`f`())]

--&
) -- highlighting fix

--8<-- assume-or-invalid-char
--# assume x: integer | # --@< Fatal: Expected a type, got `#`
--! error

--8<-- assume-or-seq-1
--# assume x: integer | () | nil --@< Error: A sequence of types cannot be inside a union
f(                               --@< Error: Expected `)`, got the end of file
--! [KailuaAssume(Local, `x`, _, Union([Integer, Oops, Nil])), Void(`f`())]

--&
) -- highlighting fix

--8<-- assume-or-seq-2
--# assume x: integer | (string,
--#                      boolean) | nil --@^-< Error: A sequence of types cannot be inside a union
f(                                      --@< Error: Expected `)`, got the end of file
--! [KailuaAssume(Local, `x`, _, Union([Integer, Oops, Nil])), Void(`f`())]

--&
) -- highlighting fix

--8<-- open-lua51
--# open lua51
--! [KailuaOpen(`lua51`)]

--8<-- open-incomplete
--# open
--# open your heart --@< Fatal: Expected a name, got a keyword `open`
--! error

--8<-- alias
--# type int = integer
--# assume x: vector<int>
--! [KailuaType(`int`, Integer), \
--!  KailuaAssume(Local, `x`, _, Array(_ `int`))]

--8<-- alias-builtin
--# type any = integer --@< Error: Cannot redefine a builtin type
--# assume x: vector<any>
--! [KailuaType(`any`, Integer), KailuaAssume(Local, `x`, _, Array(_ Any))]

--8<-- alias-incomplete
--# type int =
--# assume x: vector<int> --@< Error: Expected a single type, got a keyword `assume`
--! [KailuaType(`int`, Oops)]

--8<-- kind-error
--# type x = error
--! [KailuaType(`x`, Error)]

--8<-- kind-error-with-message
--# type x = error 'whatever'
--! [KailuaType(`x`, Error("whatever"))]

--8<-- kind-error-or-string
--# type x = error | 'whatever'
--! [KailuaType(`x`, Union([Error, String("whatever")]))]

--8<-- lua51-goto-as-a-name
--# open lua51
goto = 42
--! [KailuaOpen(`lua51`), Assign([`goto`], [42])]

--8<-- lua51-goto-as-a-name-in-meta-1
--# open lua51
--# type goto = integer --@< Fatal: Expected a name, got a keyword `goto`
--! error

--8<-- lua51-goto-as-a-name-in-meta-2
--# open lua51
--# type `goto` = integer
--! [KailuaOpen(`lua51`), KailuaType(`goto`, Integer)]

--8<-- type-spec-recover-negative-span
local a = {} --: var { var { } } --@< Error: Expected a single type, got a keyword `var`
local b --: var { var { } }      --@< Error: Expected a single type, got a keyword `var`
--! [Local([`a`: _ Oops], [Table([])]), Local([`b`: _ Oops], [])]

