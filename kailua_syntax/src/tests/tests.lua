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

--8<-- func
function r(p) --[[...]] end
--! [FuncDecl(Global, `r`, [`p`] -> _, [])]

--8<-- local-func
local function r(p,...)

end
--! [FuncDecl(Local, `r`, [`p`, ...: _] -> _, [])]

--8<-- local
local a, b
--! [Local([`a`, `b`], [])]

--8<-- local-comma-1
local a --: integer
    , b --: var WHATEVER
--! [Local([`a`: _ Integer, `b`: Var Dynamic], [])]

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
local a, b = f() --: integer, var string
--! [Local([`a`: _ Integer, `b`: Var String], [`f`()])]

--8<-- local-type-in-same-line-3
local a, b, c = f() --: integer, var string, const {}
--! [Local([`a`: _ Integer, `b`: Var String, `c`: Const EmptyTable], [`f`()])]

--8<-- local-duplicate-types-in-same-line-1
local a, b, c --: const {}
              = f() --: integer, var string, const {}
--@^ Error: The type specification cannot appear both at variable names and after the `local` declaration
--! error

--8<-- local-duplicate-types-in-same-line-2
local a, --: integer
      b, c = f() --: integer, var string, const {}
--@^ Error: The type specification cannot appear both at variable names and after the `local` declaration
--! error

--8<-- local-duplicate-types-in-same-line-3
local a, --: integer
      b, --: var string
      c = f() --: const {}
--@^ Error: The type specification cannot appear both at variable names and after the `local` declaration
--! error

--8<-- local-less-types-in-same-line-1
local a, b,
      c --@< Error: Excess type specifications in the variable names
      = f() --: integer, var string
--! error

--8<-- local-less-types-in-same-line-2
local a,
      b,
      c,
      d --@^-< Error: Excess type specifications in the variable names
      = f() --: integer, var string
--! error

--8<-- local-more-types-in-same-line-1
local a, b = f() --: integer,
                 --: var string,
                 --: const {} --@< Error: Excess type specifications after the `local` declaration
--! error

--8<-- local-more-types-in-same-line-2
local a = f() --: integer,
              --: var string,
              --: const {} --@^-< Error: Excess type specifications after the `local` declaration
--! error

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
a, b = f() --: integer, var string
--! [Assign([`a`: _ Integer, `b`: Var String], [`f`()])]

--8<-- assign-type-in-same-line-3
a, b, c = f() --: integer, var string, const {}
--! [Assign([`a`: _ Integer, `b`: Var String, `c`: Const EmptyTable], [`f`()])]

--8<-- assign-duplicate-types-in-same-line-1
a, b, c --: const {}
        = f() --: integer, var string, const {}
--@^ Error: The type specification cannot appear both at the left hand side and after the assignment
--! error

--8<-- assign-duplicate-types-in-same-line-2
a, --: integer
b, c = f() --: integer, var string, const {}
--@^ Error: The type specification cannot appear both at the left hand side and after the assignment
--! error

--8<-- assign-duplicate-types-in-same-line-3
a, --: integer
b, --: var string
c = f() --: const {}
--@^ Error: The type specification cannot appear both at the left hand side and after the assignment
--! error

--8<-- assign-less-types-in-same-line-1
a, b,
c --@< Error: Excess type specifications in the left hand side
= f() --: integer, var string
--! error

--8<-- assign-less-types-in-same-line-2
a,
b,
c,
d --@^-< Error: Excess type specifications in the left hand side
= f() --: integer, var string
--! error

--8<-- assign-more-types-in-same-line-1
a, b = f() --: integer,
           --: var string,
           --: const {} --@< Error: Excess type specifications after the assignment
--! error

--8<-- assign-more-types-in-same-line-2
a = f() --: integer,
        --: var string,
        --: const {} --@^-< Error: Excess type specifications after the assignment
--! error

--8<-- func-argtype
local function r(p --: integer
                )

end
--! [FuncDecl(Local, `r`, [`p`: _ Integer] -> _, [])]

--8<-- func-argtype-rettype
local function r(p, q) --: integer --> string

end
--! [FuncDecl(Local, `r`, [`p`, `q`: _ Integer] -> String, [])]

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
--# --[[foo --@< Fatal: A newline is disallowed in a long string inside the meta block
--# --foo]] --@^ Note: The meta block started here
do end
--! error

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
                     --@^ Error: Expected a newline, got a name
--! error

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
--! error

--8<-- assume-builtin-rejected
--# assume a: WHATEVER = "foo" --@< Error: Expected a newline, got `=`
--# assume b: WHATEVER
--! error

--8<-- kind-table
local x --: {b=var string, a=integer, c=const {d=const {}}}
--! [Local([`x`: _ Record(["b": Var String, "a": _ Integer, \
--!                        "c": Const Record(["d": Const EmptyTable])])], [])]

--8<-- kind-func
local x --: function
--! [Local([`x`: _ Function], [])]

--8<-- kind-func-or-string-or-nil
local x --: function | string?
--! [Local([`x`: _ Union([Function, Union([String, Nil])])], [])]

--8<-- kind-func-0
local x --: function()
--! [Local([`x`: _ Func([() -> ()])], [])]

--8<-- kind-func-0-explicit
local x --: function()->()
--! [Local([`x`: _ Func([() -> ()])], [])]

--8<-- kind-func-0-and-2
local x --: function () & (integer, boolean...)->string?
--! [Local([`x`: _ Func([() -> (), \
--!                      (Integer, Boolean...) -> Union([String, Nil])])], [])]

--8<-- kind-func-0-and-2-seq
local x --: function () -> (integer...) & (integer, boolean...)->(string?, WHATEVER...)
--! [Local([`x`: _ Func([() -> (Integer...), \
--!                      (Integer, Boolean...) -> (Union([String, Nil]), Dynamic...)\
--!                     ])], [])]

--8<-- kind-func-seq-without-parens
local x --: function () -> integer... --@< Error: Expected a newline, got `...`
--! error

--8<-- kind-func-or-without-parens
local x --: function (boolean...) | string? --@< Error: Expected a newline, got `|`
--! error

--8<-- kind-func-or
local x --: (function (boolean...)) | string?
--! [Local([`x`: _ Union([Func([(Boolean...) -> ()]), Union([String, Nil])])], [])]

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
--! error

--8<-- kind-paren
local x --: (integer)
--! [Local([`x`: _ Integer], [])]

--8<-- kind-paren-or-nil
local x --: (integer)?
--! [Local([`x`: _ Union([Integer, Nil])], [])]

--8<-- kind-paren-or-nil-multiline-1
local x --:
        --: (integer)?
--! [Local([`x`: _ Union([Integer, Nil])], [])]

--8<-- kind-paren-or-nil-multiline-2
local x --: (
        --:   integer
        --: )?
--! [Local([`x`: _ Union([Integer, Nil])], [])]

--8<-- kind-paren-multiline-recover
local x --: (
        --:   integer --@< Fatal: Expected `)`, got a newline
--! error

--&

--8<-- kind-table-empty
local x --: {}
--! [Local([`x`: _ EmptyTable], [])]

--8<-- kind-rec
local x --: {a = const function (), b = var string,
        --:  c = const function (string) -> integer &
        --:                     (string, integer) -> number}?
--! [Local([`x`: _ Union([Record(["a": Const Func([() -> ()]), \
--!                               "b": Var String, \
--!                               "c": Const Func([(String) -> Integer, \
--!                                                (String, Integer) -> Number])\
--!                              ]), Nil])], [])]

--8<-- kind-tuple
local x --: {const function (); var string;
        --:  const function (string) -> integer &
        --:                 (string, integer) -> number;
        --: }?
--! [Local([`x`: _ Union([Tuple([Const Func([() -> ()]), \
--!                              Var String, \
--!                              Const Func([(String) -> Integer, \
--!                                          (String, Integer) -> Number])\
--!                             ]), Nil])], [])]

--8<-- kind-tuple-1
local x --: {WHATEVER}
--! [Local([`x`: _ Array(_ Dynamic)], [])]

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
local x --: {var integer}
--! [Local([`x`: _ Array(Var Integer)], [])]

--8<-- kind-map
local x --: {[string] = const integer}
--! [Local([`x`: _ Map(String, Const Integer)], [])]

--8<-- kind-map-or-1
local x --: {[string] = integer|boolean}
--! [Local([`x`: _ Map(String, _ Union([Integer, Boolean]))], [])]

--8<-- kind-map-or-2
local x --: {[string|boolean] = integer|boolean}
--! [Local([`x`: _ Map(Union([String, Boolean]), _ Union([Integer, Boolean]))], [])]

--8<-- kind-map-or-nil
local x --: {[string?] = integer?}
--! [Local([`x`: _ Map(Union([String, Nil]), _ Union([Integer, Nil]))], [])]

--8<-- kind-nested-table
local x --: {[integer] = const {var {[string] = {integer, integer}?}}}
--! [Local([`x`: _ Map(Integer, \
--!                    Const Array(Var Map(String, \
--!                                        _ Union([Tuple([_ Integer, _ Integer]), \
--!                                                 Nil]))))], [])]

--8<-- kind-builtin-1
local x --: [builtin] string
--! [Local([`x`: _ [`builtin`] String], [])]

--8<-- kind-builtin-2
local x --: [builtin] function(any)
--! [Local([`x`: _ [`builtin`] Func([(Any) -> ()])], [])]

--8<-- kind-builtin-paren
local x --: ([builtin] (string))
--! [Local([`x`: _ [`builtin`] String], [])]

--8<-- kind-builtin-empty
local x --: [] string --@< Fatal: Expected a name, got `]`
--! error

--8<-- kind-builtin-wrong
local x --: [built-in] string --@< Fatal: Expected `]`, got `-`
--! error

--8<-- kind-builtin-keyword
local x --: [type] function(any)
--! [Local([`x`: _ [`type`] Func([(Any) -> ()])], [])]

--8<-- kind-builtin-dup
local x --: [builtin] [builtin] string --@< Error: Expected a single type, got `[`
                                       --@^ Error: Expected a newline, got a name
--! error

--8<-- kind-builtin-seq
local x --: function() -> [builtin] (string, string)
--@^ Error: Cannot attach the built-in type specification (like [name]) to the type sequence
--! error

--8<-- funcspec
--v ()
function foo() end
--! [FuncDecl(Global, `foo`, [], [])]
-- note that the return is specified (not `_`)

--8<-- funcspec-more-arity-1
--v (a: integer,
--v  b: string) --@^-< Error: Excess arguments in the function specification
function foo() end
--! error

--8<-- funcspec-more-arity-2
--v (a: integer,
--v  b: string) --@< Error: Excess arguments in the function specification
function foo(a) end
--! error

--8<-- funcspec-wrong-name
--v (a: integer)
function foo(b) end --@< Error: Mismatching argument name in the function specification
                    --@^^ Note: The corresponding argument was here
--! error

--8<-- funcspec-less-arity-1
--v ()
function foo(a,
             b) end --@^-< Error: Excess arguments in the function declaration
--! error

--8<-- funcspec-less-arity-2
--v (a: integer)
function foo(a,
             b) end --@< Error: Excess arguments in the function declaration
--! error

--8<-- funcspec-swapped-name
--v (a: integer, b: integer)
function foo(b, a) end --@< Error: Mismatching argument name in the function specification
                       --@^^ Note: The corresponding argument was here
                       --@^^ Error: Mismatching argument name in the function specification
                       --@^^^^ Note: The corresponding argument was here
--! error

--8<-- funcspec-1
--v (a: integer)
local function foo(a) end
--! [FuncDecl(Local, `foo`, [`a`: _ Integer], [])]

--8<-- funcspec-1-seq-1
--v (a: integer) -> (WHATEVER...)
local function foo(a) end
--! [FuncDecl(Local, `foo`, [`a`: _ Integer] -> [Dynamic...], [])]

--8<-- funcspec-1-seq-2
--v (a: integer) -> (string, WHATEVER...)
local function foo(a) end
--! [FuncDecl(Local, `foo`, [`a`: _ Integer] -> [String, Dynamic...], [])]

--8<-- funcspec-inline
(--v (a: const integer,
 --v  ...)
 --v -> string
 function(a, ...) end)()
--! [Void(Func([`a`: Const Integer, ...: _] -> String, [])())]

--8<-- rettype-none
function foo() --> ()
end
--! [FuncDecl(Global, `foo`, [], [])]

--8<-- rettype-string
function foo() --> string
end
--! [FuncDecl(Global, `foo`, [] -> String, [])]

--8<-- rettype-string-parens
function foo() --> ((((string))))
end
--! [FuncDecl(Global, `foo`, [] -> String, [])]

--8<-- rettype-seq-1
function foo() --> (WHATEVER...)
end
--! [FuncDecl(Global, `foo`, [] -> [Dynamic...], [])]

--8<-- rettype-seq-2
function foo() --> (string, WHATEVER...)
end
--! [FuncDecl(Global, `foo`, [] -> [String, Dynamic...], [])]

--8<-- funcspec-and-rettype
--v ()
function foo() --> string
    --@^ Error: Inline return type specification cannot appear with the function specification
end --@^^^ Note: The function specification appeared here
--! error

--8<-- funcspec-and-argtype-rettype-1
--v (a: integer)
function foo(a) --: integer --> string
    --@^ Error: Inline argument type specification cannot appear with the function specification
end --@^^^ Note: The function specification appeared here
    --@^^^ Error: Inline return type specification cannot appear with the function specification
    --@^^^^^ Note: The function specification appeared here
--! error

--8<-- funcspec-and-argtype-rettype-2
--v (a: integer, b: boolean)
function foo(a, --: integer --@< Error: Inline argument type specification cannot appear with the function specification
                            --@^^ Note: The function specification appeared here
             b) --> string  --@< Error: Inline return type specification cannot appear with the function specification
                            --@^^^^ Note: The function specification appeared here
end
--! error

--8<-- funcspec-before-nothing
--v () --@< Error: No function declaration after the function specification
--! error

--8<-- funcspec-before-local
--v () --@< Error: No function declaration after the function specification
local v = 42
--! error

--8<-- funcspec-before-local-recover
--v () --@< Error: No function declaration after the function specification
local v = 42
--v () --@< Error: No function declaration after the function specification
--! error

--8<-- funcspec-before-assume
--v () --@< Error: No function declaration after the function specification
--# assume x: integer
--! error

--8<-- funcspec-before-for
--v () --@< Error: No function declaration after the function specification
for i = 1, 3 do end
--! error

--8<-- funcspec-before-expr-inline
f(--v () --@< Error: No function literal after the function specification
  g())
--! error

--8<-- argtype-inline
function foo(a, --: integer
             b, ...) --: string
end
--! [FuncDecl(Global, `foo`, [`a`: _ Integer, `b`, ...: String] -> _, [])]

--8<-- funcspec-and-argtype
--v (a: integer, b: boolean, ...: string)
function foo(a, b, ...) --: string --@< Error: Inline variadic argument type specification cannot appear with the function specification
                                   --@^^ Note: The corresponding argument in the function specification was here
end
--! error

--8<-- funcspec-less-varargs
--v (a: integer, b: boolean)
function foo(a, b, ...) --@< Error: Variadic arguments appear in the function but not in the function specification
end
--! error

--8<-- funcspec-more-varargs
--v (a: integer, b: boolean, ...: string) --@< Error: Variadic arguments appear in the function specification but not in the function itself
function foo(a, b)
end
--! error

--8<-- funcspec-varargs
--v (a: integer, b: boolean, ...: string)
function foo(a, b, ...)
end
--! [FuncDecl(Global, `foo`, [`a`: _ Integer, `b`: _ Boolean, ...: String], [])]

--8<-- funcspec-varargs-0
--v (...: string)
function foo(...)
end
--! [FuncDecl(Global, `foo`, [...: String], [])]

--8<-- local-funcspec-varargs-0
--v (...: string)
local function foo(...)
end
--! [FuncDecl(Local, `foo`, [...: String], [])]

--8<-- assume-multiline-recover
--# assume a: { integer, string
--#                      boolean --@< Fatal: Expected `,`, `;` or `}`, got a name
--! error

--8<-- long-string-incomplete-1
f([==== [ --@< Fatal: Opening long bracket should end with `[`
--! error

--&
) -- highlighting fix

--8<-- long-string-incomplete-2
f([==== --@< Fatal: Opening long bracket should end with `[`
--! error

--&
) -- highlighting fix

--8<-- long-string-incomplete-3
f([====[ --@< Fatal: Premature end of file in a long string
         --@^ Note: The long string started here
--! error

--&
]====]) -- highlighting fix

--8<-- long-string-incomplete-4
f([====[foo] --@< Fatal: Premature end of file in a long string
             --@^ Note: The long string started here
--! error

--&
]====]) -- highlighting fix

--8<-- meta-long-string
--# assume p: [[xxx   --@< Fatal: A newline is disallowed in a long string inside the meta block
--#             yyy]] --@^ Note: The meta block started here
--! error

--8<-- meta-long-comment-1
--# assume p: --[[xxx   --@< Fatal: A newline is disallowed in a long string inside the meta block
--#               yyy]] --@^ Note: The meta block started here
--! error

--8<-- meta-long-comment-2
--# assume p: --[[xxx   --@< Fatal: A newline is disallowed in a long string inside the meta block
                  yyy]] --@^ Note: The meta block started here
--! error

--8<-- string-wrong-escape
f('foo\xyz') --@< Error: Unrecognized escape sequence in a string
--! error

--8<-- string-wrong-escape-recover
f('foo\xyz', 'bar\zyx') --@< Error: Unrecognized escape sequence in a string
                        --@^ Error: Unrecognized escape sequence in a string
--! error

--8<-- string-incomplete-escape
f('foo\ --@< Fatal: Premature end of file in a string
        --@^ Note: The string started here
--! error

--&
') -- highlighting fix

--8<-- string-incomplete
f('foo --@< Fatal: Premature end of file in a string
       --@^ Note: The string started here
--! error

--&
') -- highlighting fix

--8<-- number-long
f(0x12345678901234567)
--! [Void(`f`(20988295476557332000))]
-- relies on std's correct f64 rounding

--8<-- number-incomplete-exp-1
f(3e --@< Fatal: Invalid number
--! error

--&
) -- highlighting fix

--8<-- number-incomplete-exp-2
f(3e+ --@< Fatal: Invalid number
--! error

--&
) -- highlighting fix

--8<-- invalid-char-1
f(~3) --@< Fatal: Unexpected character
--! error

--8<-- invalid-char-2
f(@3) --@< Fatal: Unexpected character
--! error

--8<-- assume-excess
--# assume p: integer f --@< Error: Expected a newline, got a name
--! error

--8<-- for-of
for a of x --@< Fatal: Expected `=`, `,`, `in` or `--:` after `for NAME`, got a name
--! error

--8<-- local-seq
local (x, y) = (1, 2) --@< Fatal: Expected a name or `function` after `local`, got `(`
--! error

--8<-- func-args-invalid-char
function p(# --@< Fatal: Expected an argument name, `)` or `...`, got `#`
--! error

--&
) -- highlighting fix

--8<-- argtype-slot
function p(...) --: var integer --@< Error: Variadic argument specifier cannot have modifiers
end
--! error

--8<-- table-invalid-char
f({x#}) --@< Fatal: Expected `,`, `;` or `}`, got `#`
--! error

--8<-- index-with-number
f(x.0) --@< Fatal: Expected a name after `<expression> .`, got a number
--! error

--8<-- methodcall-no-args
f(x:g - 1) --@< Fatal: Expected argument(s) after `<expression> : <name>`, got `-`
--! error

--8<-- funccall-invalid-char
f(2, *3) --@< Fatal: Expected an expression, got `*`
--! error

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
--# assume x: # --@< Error: Expected a single type, got `#`
--! error

--8<-- assume-seq-varargs-1
--# assume x: (...) --@< Error: `...` should be preceded with a kind in the ordinary kinds
                    --@^ Error: Expected a single type, not type sequence
--! error

--8<-- assume-seq-1
--# assume x: (string...) --@< Error: Expected a single type, not type sequence
--! error

--8<-- assume-seq-varargs-2
--# assume x: (integer, ...) --@< Error: `...` should be preceded with a kind in the ordinary kinds
                             --@^ Error: Expected a single type, not type sequence
--! error

--8<-- assume-seq-2
--# assume x: (integer, string) --@< Error: Expected a single type, not type sequence
--! error

--8<-- assume-seq-varargs-3
--# assume x: (integer, string...) --@< Error: Expected a single type, not type sequence
--! error

--8<-- assume-seq-invalid-char
--# assume x: (integer, #) --@< Fatal: Expected a type, got `#`
--! error

--8<-- assume-func-invalid-char
--# assume x: function () -> # --@< Error: Expected a single type or type sequence, got `#`
--! error

--8<-- assume-named
--# assume x: whatever
--! [KailuaAssume(Local, `x`, _, `whatever`)]

--8<-- assume-rec-invalid-char
--# assume x: {x = integer #} --@< Fatal: Expected `,`, `;` or `}`, got `#`
--! error

--8<-- assume-tuple-invalid-char
--# assume x: {integer #} --@< Fatal: Expected `,`, `;` or `}`, got `#`
--! error

--8<-- assume-rec-duplicate-name
--# assume x: {x = integer, x = string} --@< Error: Duplicate record field `x` in the type specification
                                        --@^ Note: The first duplicate appeared here
--! error

--8<-- assume-rec-duplicate-name-recover
--# assume x: {x = integer,
--#            x = string,  --@< Error: Duplicate record field `x` in the type specification
--#                         --@^^ Note: The first duplicate appeared here
--#            y = table,
--#            x = boolean, --@< Error: Duplicate record field `x` in the type specification
--#                         --@^^^^^ Note: The first duplicate appeared here
--#            y = number}  --@< Error: Duplicate record field `y` in the type specification
                            --@^^^^ Note: The first duplicate appeared here
--! error

--8<-- assume-builtin-and-literal-recover
--# assume x: WHATEVER = hello --@< Error: Expected a newline, got `=`
--# assume y: "bo\gus"         --@< Error: Unrecognized escape sequence in a string
f(                             --@< Fatal: Expected `)`, got the end of file
--! error

--&
) -- highlighting fix

--8<-- assume-or-invalid-char
--# assume x: integer | # --@< Fatal: Expected a type, got `#`
--! error

--8<-- assume-or-seq-1
--# assume x: integer | () | nil --@< Error: A sequence of types cannot be inside a union
f(                               --@< Fatal: Expected `)`, got the end of file
--! error

--&
) -- highlighting fix

--8<-- assume-or-seq-2
--# assume x: integer | (string,
--#                      boolean) | nil --@^-< Error: A sequence of types cannot be inside a union
f(                                      --@< Fatal: Expected `)`, got the end of file
--! error

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
--# assume x: {int}
--! [KailuaType(`int`, Integer), \
--!  KailuaAssume(Local, `x`, _, Array(_ `int`))]

--8<-- alias-builtin
--# type any = integer --@< Error: Cannot redefine a builtin type
--# assume x: {any}
--! error

--8<-- alias-incomplete
--# type int =
--# assume x: {int} --@< Error: Expected a single type, got a keyword `assume`
                    --@^ Error: Expected a newline, got a name
--! error

--8<-- kind-error
--# type x = error
--! [KailuaType(`x`, Error)]

--8<-- kind-error-with-message
--# type x = error 'whatever'
--! [KailuaType(`x`, Error("whatever"))]

--8<-- kind-error-or-string
--# type x = error | 'whatever'
--! [KailuaType(`x`, Union([Error, String("whatever")]))]

