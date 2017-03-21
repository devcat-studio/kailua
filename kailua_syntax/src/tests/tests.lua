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
--! [Do([Void(`p`_())]), Void(`q`_())]

--8<-- if
if a then
    local p
    p()
elseif b then
    local q = 0
else
    p()
end
--! [If((`a`_ => [Local([`p`$1], [])$1, Void(`p`$1())]), \
--!     (`b`_ => [Local([`q`$2], [0])$2]), \
--!     [Void(`p`_())])]

--8<-- while
while a do b() end c()
--! [While(`a`_, [Void(`b`_())]), Void(`c`_())]

--8<-- while-recover-1
while @ --@< Error: Unexpected character
    x()
end --@< Error: Expected a keyword `do`, got a keyword `end`
f()
--! [While(Oops, []), Void(`f`_())]

--8<-- while-recover-2
while @ do --@< Error: Unexpected character
           --@^ Error: Expected an expression, got a keyword `do`
    x()
end
f()
--! [While(Oops, [Void(`x`_())]), Void(`f`_())]

--8<-- while-recover-3
while do --@< Error: Expected an expression, got a keyword `do`
    x()
end
f()
--! [While(Oops, [Void(`x`_())]), Void(`f`_())]

--8<-- while-recover-4
while a do
    @ --@< Error: Unexpected character
end
f()
--! [While(`a`_, []), Void(`f`_())]

--8<-- top-level-end
f() end g() --@< Error: Expected a statement, got a keyword `end`
--! [Void(`f`_()), Oops, Void(`g`_())]

--8<-- top-level-closing-paren
f()) g() --@< Error: Expected a statement, got `)`
--! [Void(`f`_()), Oops, Void(`g`_())]

--8<-- paren-recover
f((a@)+(@b)*c) --@< Error: Unexpected character
               --@^ Error: Unexpected character
--! [Void(`f`_(((`a`_) + ((`b`_) * `c`_))))]

--8<-- func
function r(p) --[[...]] end
--! [FuncDecl(`r`_, [`p`$1] --> _, $1[])]

--8<-- local-func
local function r(p,...)

end
--! [FuncDecl(`r`$2, [`p`$1, ...: _] --> _, $1[])$2]

--8<-- local-func-without-sibling-scope-1
local r
function r(p,...)

end
--! [Local([`r`$1], [])$1, FuncDecl(`r`$1, [`p`$2, ...: _] --> _, $2[])]

--8<-- local-func-without-sibling-scope-2
local r
do
    local s
    function r(p,...)

    end
end
--! [Local([`r`$1], [])$1, Do([Local([`s`$2], [])$2, \
--!                            FuncDecl(`r`$1, [`p`$3, ...: _] --> _, $3[])])]

--8<-- func-in-table
function a.b.c(p) --[[...]] end
--! [MethodDecl((`a`_.`b`.`c`), None, [`p`$1] --> _, $1[])]

--8<-- method
function a.b:c(p) --[[...]] end
--! [MethodDecl((`a`_.`b`.`c`), Some(self=`self`$1), [`p`$1] --> _, $1[])]

--8<-- local
local a, b
--! [Local([`a`$1, `b`$1], [])$1]

--8<-- local-comma-1
local a --: integer
    , b --: WHATEVER
--! [Local([`a`$1: _ Integer, `b`$1: _ Dynamic], [])$1]

--8<-- local-comma-2
local a, --: const table
      b
--! [Local([`a`$1: Const Table, `b`$1], [])$1]

--8<-- local-assign-1
local a = --: integer
          f()
--! [Local([`a`$1: _ Integer], [`f`_()])$1]

--8<-- local-assign-2
local a --: integer
      = f()
--! [Local([`a`$1: _ Integer], [`f`_()])$1]

--8<-- local-type-in-same-line
local a = f() --: integer
--! [Local([`a`$1: _ Integer], [`f`_()])$1]

--8<-- local-type-in-same-line-2
local a, b = f() --: integer, string
--! [Local([`a`$1: _ Integer, `b`$1: _ String], [`f`_()])$1]

--8<-- local-type-in-same-line-3
local a, b, c = f() --: integer, string, const {}
--! [Local([`a`$1: _ Integer, `b`$1: _ String, `c`$1: Const EmptyTable], [`f`_()])$1]

--8<-- local-duplicate-types-in-same-line-1
local a, b, c --: const {}
              = f() --: integer, string, const {}
--@^ Error: The type specification cannot appear both at variable names and after the `local` declaration
f()
--! [Local([`a`$1, `b`$1, `c`$1: Const EmptyTable], [`f`_()])$1, Void(`f`_())]

--8<-- local-duplicate-types-in-same-line-2
local a, --: integer
      b, c = f() --: integer, string, const {}
--@^ Error: The type specification cannot appear both at variable names and after the `local` declaration
--! [Local([`a`$1: _ Integer, `b`$1, `c`$1], [`f`_()])$1]

--8<-- local-duplicate-types-in-same-line-3
local a, --: integer
      b, --: string
      c = f() --: const {}
--@^ Error: The type specification cannot appear both at variable names and after the `local` declaration
--! [Local([`a`$1: _ Integer, `b`$1: _ String, `c`$1], [`f`_()])$1]

--8<-- local-less-types-in-same-line-1
local a, b,
      c --@< Error: Excess type specifications in the variable names
      = f() --: integer, string
--! [Local([`a`$1, `b`$1, `c`$1], [`f`_()])$1]

--8<-- local-less-types-in-same-line-2
local a,
      b,
      c,
      d --@^-< Error: Excess type specifications in the variable names
      = f() --: integer, string
--! [Local([`a`$1, `b`$1, `c`$1, `d`$1], [`f`_()])$1]

--8<-- local-more-types-in-same-line-1
local a, b = f() --: integer,
                 --: string,
                 --: const {} --@< Error: Excess type specifications after the `local` declaration
--! [Local([`a`$1, `b`$1], [`f`_()])$1]

--8<-- local-more-types-in-same-line-2
local a = f() --: integer,
              --: string,
              --: const {} --@^-< Error: Excess type specifications after the `local` declaration
--! [Local([`a`$1], [`f`_()])$1]

--8<-- assign-1-1
a = f()
--! [Assign([`a`_], [`f`_()])]

--8<-- assign-1-3
a = f(), g(), h()
--! [Assign([`a`_], [`f`_(), `g`_(), `h`_()])]

--8<-- assign-2-1
a, b = f()
--! [Assign([`a`_, `b`_], [`f`_()])]

--8<-- assign-2-3
a, b = f(), g(), h()
--! [Assign([`a`_, `b`_], [`f`_(), `g`_(), `h`_()])]

--8<-- assign-index-name-1
ab.cde = f(), g(), h()
--! [Assign([`ab`_.`cde`], [`f`_(), `g`_(), `h`_()])]

--8<-- assign-index-name-2
ab.cde.fg = f(), g(), h()
--! [Assign([`ab`_.`cde`.`fg`], [`f`_(), `g`_(), `h`_()])]

--8<-- assign-index-name-3
a, b.cde = f(), g(), h()
--! [Assign([`a`_, `b`_.`cde`], [`f`_(), `g`_(), `h`_()])]

--8<-- assign-index-name-4
a, b.cde.fg = f(), g(), h()
--! [Assign([`a`_, `b`_.`cde`.`fg`], [`f`_(), `g`_(), `h`_()])]

--8<-- assign-index-exp-1
a[a*a] = f(), g(), h()
--! [Assign([`a`_[(`a`_ * `a`_)]], [`f`_(), `g`_(), `h`_()])]

--8<-- assign-index-exp-2
a, b[a*a] = f(), g(), h()
--! [Assign([`a`_, `b`_[(`a`_ * `a`_)]], [`f`_(), `g`_(), `h`_()])]

--8<-- assign-index-func-exp-1
x().y[a*a] = f(), g(), h()
--! [Assign([`x`_().`y`[(`a`_ * `a`_)]], [`f`_(), `g`_(), `h`_()])]

--8<-- assign-index-func-exp-2
a, x().y[a*a] = f(), g(), h()
--! [Assign([`a`_, `x`_().`y`[(`a`_ * `a`_)]], [`f`_(), `g`_(), `h`_()])]

--8<-- assign-type
a --: integer
  = f()
--! [Assign([`a`_: _ Integer], [`f`_()])]

--8<-- assign-type-2
a, --: integer
b  --: const string
= f(),
  g()
--! [Assign([`a`_: _ Integer, `b`_: Const String], [`f`_(), `g`_()])]

--8<-- assign-type-3
a,
b  --: const string
= f(),
  g()
--! [Assign([`a`_, `b`_: Const String], [`f`_(), `g`_()])]

--8<-- assign-type-in-same-line
a = f() --: integer
--! [Assign([`a`_: _ Integer], [`f`_()])]

--8<-- assign-type-in-same-line-2
a, b = f() --: integer, string
--! [Assign([`a`_: _ Integer, `b`_: _ String], [`f`_()])]

--8<-- assign-type-in-same-line-3
a, b, c = f() --: integer, string, const {}
--! [Assign([`a`_: _ Integer, `b`_: _ String, `c`_: Const EmptyTable], [`f`_()])]

--8<-- assign-duplicate-types-in-same-line-1
a, b, c --: const {}
        = f() --: integer, string, const {}
--@^ Error: The type specification cannot appear both at the left hand side and after the assignment
--! [Assign([`a`_, `b`_, `c`_: Const EmptyTable], [`f`_()])]

--8<-- assign-duplicate-types-in-same-line-2
a, --: integer
b, c = f() --: integer, string, const {}
--@^ Error: The type specification cannot appear both at the left hand side and after the assignment
--! [Assign([`a`_: _ Integer, `b`_, `c`_], [`f`_()])]

--8<-- assign-duplicate-types-in-same-line-3
a, --: integer
b, --: string
c = f() --: const {}
--@^ Error: The type specification cannot appear both at the left hand side and after the assignment
--! [Assign([`a`_: _ Integer, `b`_: _ String, `c`_], [`f`_()])]

--8<-- assign-less-types-in-same-line-1
a, b,
c --@< Error: Excess type specifications in the left hand side
= f() --: integer, string
--! [Assign([`a`_, `b`_, `c`_], [`f`_()])]

--8<-- assign-less-types-in-same-line-2
a,
b,
c,
d --@^-< Error: Excess type specifications in the left hand side
= f() --: integer, string
--! [Assign([`a`_, `b`_, `c`_, `d`_], [`f`_()])]

--8<-- assign-more-types-in-same-line-1
a, b = f() --: integer,
           --: string,
           --: const {} --@< Error: Excess type specifications after the assignment
--! [Assign([`a`_, `b`_], [`f`_()])]

--8<-- assign-more-types-in-same-line-2
a = f() --: integer,
        --: string,
        --: const {} --@^-< Error: Excess type specifications after the assignment
--! [Assign([`a`_], [`f`_()])]

--8<-- assign-type-index-1
a.x, b.y = 42, 54 --: integer, integer
--! [Assign([`a`_.`x`: _ Integer, `b`_.`y`: _ Integer], [42, 54])]

--8<-- assign-type-index-2
a.x, b, c['z'] = 42, 54 --: integer, integer
--@^ Error: Excess type specifications in the left hand side
--! [Assign([`a`_.`x`, `b`_, `c`_["z"]], [42, 54])]

--8<-- func-argtype
local function r(p --: integer
                )

end
--! [FuncDecl(`r`$2, [`p`$1: _ Integer] --> _, $1[])$2]

--8<-- func-argtype-rettype
local function r(p, q) --: integer --> string

end
--! [FuncDecl(`r`$2, [`p`$1, `q`$1: _ Integer] --> String, $1[])$2]

--8<-- funccall
f()
--! [Void(`f`_())]

--8<-- funccall-1
f(3)
--! [Void(`f`_(3))]

--8<-- funccall-op-1
f(3+4)
f(3+4-5)
f(3+4*5)
f((3+4)*5)
--! [Void(`f`_((3 + 4))), \
--!  Void(`f`_(((3 + 4) - 5))), \
--!  Void(`f`_((3 + (4 * 5)))), \
--!  Void(`f`_((((3 + 4)) * 5)))]

--8<-- funccall-op-2
f(2^3^4)
f(-2^4)
f(-2^-4)
f(- -3)
--! [Void(`f`_((2 ^ (3 ^ 4)))), \
--!  Void(`f`_((- (2 ^ 4)))), \
--!  Void(`f`_((- (2 ^ (- 4))))), \
--!  Void(`f`_((- (- 3))))]

--8<-- funccall-string
f'oo'
--! [Void(`f`_"oo")]

--8<-- funccall-string-string
f'oo''bar'
f'oo'[[bar]]
f[=[oo]=]"bar"
f[=[oo]=][[bar]]
--! [Void(`f`_"oo""bar"), Void(`f`_"oo""bar"), Void(`f`_"oo""bar"), Void(`f`_"oo""bar")]

--8<-- funccall-table
f{a=1,[3.1]=4e5;[=[[[]]]=],}
--! [Void(`f`_{["a"] = 1, [3.1] = 400000, "[[]]"})]

--8<-- funccall-table-1
f{a=a, a}
--! [Void(`f`_{["a"] = `a`_, `a`_})]

--8<-- funccall-table-2
f{a=a; a;}
--! [Void(`f`_{["a"] = `a`_, `a`_})]

--8<-- funccall-table-3
f{a=a; a();}
--! [Void(`f`_{["a"] = `a`_, `a`_()})]

--8<-- funccall-table-empty
f{}
f({})
--! [Void(`f`_{}), Void(`f`_({}))]

--8<-- funccall-table-string
f"oo"{ba=r}
f{o=o}[[bar]]
--! [Void(`f`_"oo"{["ba"] = `r`_}), Void(`f`_{["o"] = `o`_}"bar")]

--8<-- funccall-index
f.a()
--! [Void(`f`_.`a`())]

--8<-- methodcall
f:a(1)
--! [Void((`f`_:`a`)(1))]

--8<-- methodcall-index
f.a:b(1, 2)
--! [Void((`f`_.`a`:`b`)(1, 2))]

--8<-- desugared-call
a = r"string":sub(3)
a = r{a=4}.a
--! [Assign([`a`_], [(`r`_"string":`sub`)(3)]), \
--!  Assign([`a`_], [`r`_{["a"] = 4}.`a`])]

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
local a
--# assume a: string
b = a
--# assume b: string
local c = a .. b
--! [Local([`a`$1], [])$1, \
--!  KailuaAssume(`a`$2, `a`$1, _, String)$2, \
--!  Assign([`b`_], [`a`$2]), \
--!  KailuaAssume(`b`$3, `b`_, _, String)$3, \
--!  Local([`c`$4], [(`a`$2 .. `b`$3)])$4]

--8<-- assume-scope-1
local a
do
    local b = a
    --# assume a: string
    local c = a
end
local d = a
--! [Local([`a`$1], [])$1, \
--!  Do([Local([`b`$2], [`a`$1])$2, \
--!      KailuaAssume(`a`$3, `a`$1, _, String)$3, \
--!      Local([`c`$4], [`a`$3])$4]), \
--!  Local([`d`$5], [`a`$1])$5]

--8<-- assume-scope-2
do
    local b = a
    --# assume a: string
    local c = a
end
local d = a
--! [Do([Local([`b`$1], [`a`_])$1, \
--!      KailuaAssume(`a`$2, `a`_, _, String)$2, \
--!      Local([`c`$3], [`a`$2])$3]), \
--!  Local([`d`$4], [`a`_])$4]

--8<-- assume-global-1
local a
--# assume global a: string --@< Error: `--# assume` directive tried to set a global variable `a`, but it was shadowed by a local variable of the same name
--# assume global b: string
local c = a .. b
--! [Local([`a`$1], [])$1, \
--!  KailuaAssume(`a`_, `a`_, _, String), \
--!  KailuaAssume(`b`_, `b`_, _, String), \
--!  Local([`c`$2], [(`a`$1 .. `b`_)])$2]

--8<-- assume-global-2
do
    local a
    --# assume global a: string
    --@^ Error: `--# assume global` should be in the top-level scope
    --@^^ Error: `--# assume` directive tried to set a global variable `a`, but it was shadowed by a local variable of the same name
    --# assume global b: string
    --@^ Error: `--# assume global` should be in the top-level scope
    local c = a .. b
end
--! [Do([Local([`a`$1], [])$1, \
--!      KailuaAssume(`a`_, `a`_, _, String), \
--!      KailuaAssume(`b`_, `b`_, _, String), \
--!      Local([`c`$2], [(`a`$1 .. `b`_)])$2])]

--8<-- assume-and-empty
--# assume a: string
--#
--! [KailuaAssume(`a`$1, `a`_, _, String)$1]

--8<-- assume-multiline
--# assume a:
--#   string
--! [KailuaAssume(`a`$1, `a`_, _, String)$1]

--8<-- assume-global-multiline
--# assume global a:
--#   string
--! [KailuaAssume(`a`_, `a`_, _, String)]

--8<-- assume-global-global
--# assume global global --@< Error: Expected a name, got a keyword `global`
f()
--! [Oops, Void(`f`_())]

--8<-- assume-incomplete
--# assume a:
--# assume b: string --@< Error: Expected a single type, got a keyword `assume`
--! [KailuaAssume(`a`$1, `a`_, _, Oops)$1, \
--!  KailuaAssume(`b`$2, `b`_, _, String)$2]

--8<-- assume-assume
--# assume assume: WHATEVER --@< Error: Expected a name, got a keyword `assume`
f()
--! [Oops, Void(`f`_())]

--8<-- assume-quoted-assume
--# assume `assume`: WHATEVER
--! [KailuaAssume(`assume`$1, `assume`_, _, Dynamic)$1]

--8<-- quoted-assume-assume
--# `assume` `assume`: WHATEVER --@< Error: Expected a newline, got a name
--! []

--8<-- assume-builtin-rejected
--# assume a: WHATEVER = "foo" --@< Error: Expected a newline, got `=`
--# assume b: WHATEVER
--! [KailuaAssume(`a`$1, `a`_, _, Dynamic)$1]

--8<-- assume-static
--# assume static a: WHATEVER --@< Error: `--# assume static` can only be used to set fields in class prototypes
--! [KailuaAssume(`a`$1, `a`_, _, Dynamic)$1]

--8<-- assume-field-1
--# assume a.b: WHATEVER
--# assume a.b.c: WHATEVER
--# assume a.b.c
--#           .d: WHATEVER
--# assume a.b.c.
--#            d.e: const WHATEVER
--! [KailuaAssumeField(false, (`a`_.`b`), _, Dynamic), \
--!  KailuaAssumeField(false, (`a`_.`b`.`c`), _, Dynamic), \
--!  KailuaAssumeField(false, (`a`_.`b`.`c`.`d`), _, Dynamic), \
--!  KailuaAssumeField(false, (`a`_.`b`.`c`.`d`.`e`), Const, Dynamic)]

--8<-- assume-field-2
local a
--# assume a: WHATEVER
--# assume a.b: WHATEVER
--# assume a.b.c: WHATEVER
--# assume a.b.c
--#           .d: WHATEVER
--# assume a.b.c.
--#            d.e: const WHATEVER
--! [Local([`a`$1], [])$1, \
--!  KailuaAssume(`a`$2, `a`$1, _, Dynamic)$2, \
--!  KailuaAssumeField(false, (`a`$2.`b`), _, Dynamic), \
--!  KailuaAssumeField(false, (`a`$2.`b`.`c`), _, Dynamic), \
--!  KailuaAssumeField(false, (`a`$2.`b`.`c`.`d`), _, Dynamic), \
--!  KailuaAssumeField(false, (`a`$2.`b`.`c`.`d`.`e`), Const, Dynamic)]

--8<-- assume-field-static-1
--# assume static a.b: WHATEVER
--# -- everything below should be an error in the checker
--# assume static a.b.c: WHATEVER
--# assume static a.b.c
--#                .d: WHATEVER
--# assume static a.b.c.
--#                 d.e: const WHATEVER
--! [KailuaAssumeField(true, (`a`_.`b`), _, Dynamic), \
--!  KailuaAssumeField(true, (`a`_.`b`.`c`), _, Dynamic), \
--!  KailuaAssumeField(true, (`a`_.`b`.`c`.`d`), _, Dynamic), \
--!  KailuaAssumeField(true, (`a`_.`b`.`c`.`d`.`e`), Const, Dynamic)]

--8<-- assume-field-static-2
local a
--# assume a: WHATEVER
--# assume static a.b: WHATEVER
--# -- everything below should be an error in the checker
--# assume static a.b.c: WHATEVER
--# assume static a.b.c
--#                  .d: WHATEVER
--# assume static a.b.c.
--#                   d.e: const WHATEVER
--! [Local([`a`$1], [])$1, \
--!  KailuaAssume(`a`$2, `a`$1, _, Dynamic)$2, \
--!  KailuaAssumeField(true, (`a`$2.`b`), _, Dynamic), \
--!  KailuaAssumeField(true, (`a`$2.`b`.`c`), _, Dynamic), \
--!  KailuaAssumeField(true, (`a`$2.`b`.`c`.`d`), _, Dynamic), \
--!  KailuaAssumeField(true, (`a`$2.`b`.`c`.`d`.`e`), Const, Dynamic)]

--8<-- assume-field-scope-1
local a
do
    --# assume a.b: WHATEVER
    --# assume a.b.c: const WHATEVER
    local b = a
end
local c = a
--! [Local([`a`$1], [])$1, \
--!  Do([KailuaAssumeField(false, (`a`$1.`b`), _, Dynamic), \
--!      KailuaAssumeField(false, (`a`$1.`b`.`c`), Const, Dynamic), \
--!      Local([`b`$2], [`a`$1])$2]), \
--!  Local([`c`$3], [`a`$1])$3]

--8<-- assume-field-scope-2
do
    --# assume a.b: WHATEVER --@< Error: `--# assume` for fields in a global variable `a` should be in the top-level scope
    --# assume a.b.c: const WHATEVER --@< Error: `--# assume` for fields in a global variable `a` should be in the top-level scope
    local b = a
end
local c = a
--! [Do([KailuaAssumeField(false, (`a`_.`b`), _, Dynamic), \
--!      KailuaAssumeField(false, (`a`_.`b`.`c`), Const, Dynamic), \
--!      Local([`b`$1], [`a`_])$1]), \
--!  Local([`c`$2], [`a`_])$2]

--8<-- assume-field-global
--# assume global a.b: WHATEVER --@< Error: `global` is redundant here because a field gets `--# assume`d in place
--# assume global a.b.c: WHATEVER --@< Error: `global` is redundant here because a field gets `--# assume`d in place
--! [KailuaAssumeField(false, (`a`_.`b`), _, Dynamic), \
--!  KailuaAssumeField(false, (`a`_.`b`.`c`), _, Dynamic)]

--8<-- kind-int
local x --: int
--! [Local([`x`$1: _ Integer], [])$1]

--8<-- kind-bool
local x --: bool
--! [Local([`x`$1: _ Boolean], [])$1]

--8<-- kind-table
local x --: {b:string, a:integer, c:const {const {}}}
--! [Local([`x`$1: _ Record(["b": _ String, "a": _ Integer, \
--!                          "c": Const Tuple([Const EmptyTable])])], [])$1]

--8<-- kind-table-old-recover
local x --: {b=string, a=integer, c=const {const {}}} --@< Error: Expected `,`, `;` or `}`, got `=`
--! [Local([`x`$1: _ Tuple([_ `b`])], [])$1]

--8<-- kind-func
local x --: function
--! [Local([`x`$1: _ Function], [])$1]

--8<-- kind-func-or-string-opt
local x --: function | string?
--! [Local([`x`$1: _ Union([Function, String?])], [])$1]

--8<-- kind-func-or-string-paren-opt
local x --: (function | string)?
--! [Local([`x`$1: _ Union([Function, String])?], [])$1]

--8<-- kind-func-0
local x --: function()
--! [Local([`x`$1: _ Func(() --> ())], [])$1]

--8<-- kind-func-0-explicit
local x --: function()-->()
--! [Local([`x`$1: _ Func(() --> ())], [])$1]

--8<-- kind-func-2
local x --: function(integer, boolean...)-->string?
--! [Local([`x`$1: _ Func((Integer, Boolean...) --> String?)], [])$1]

--8<-- kind-func-2-seq
local x --: function(integer, boolean...)-->(string?, WHATEVER...)
--! [Local([`x`$1: _ Func((Integer, Boolean...) --> (String?, Dynamic...))], [])$1]

--8<-- kind-func-seq-without-parens
local x --: function () --> integer... --@< Error: Expected a newline, got `...`
--! [Local([`x`$1: _ Func(() --> Integer)], [])$1]

--8<-- kind-func-or-without-parens
local x --: function (boolean...) | string? --@< Error: Expected a newline, got `|`
--! [Local([`x`$1: _ Func((Boolean...) --> ())], [])$1]

--8<-- kind-func-or
local x --: (function (boolean...)) | string?
--! [Local([`x`$1: _ Union([Func((Boolean...) --> ()), String?])], [])$1]

--8<-- kind-func-recover
local x --: function (integer, @) --> string --@< Error: Unexpected character
                                             --@^ Error: Expected a type, got `)`
--! [Local([`x`$1: _ Func((Integer, Oops) --> String)], [])$1]

--8<-- kind-any-func
local x --: `function`
--! [Local([`x`$1: _ Function], [])$1]

--8<-- kind-any-func-recover-1
local x --: `function`() --@< Error: Expected a newline, got `(`
local                    --@< Error: Expected a name or `function` after `local`, got the end of file
--&
--! [Local([`x`$1: _ Function], [])$1, Oops]

--8<-- kind-any-func-recover-2
local x --: `function`() --@< Error: Expected a newline, got `(`
local
do end --@< Error: Expected a name or `function` after `local`, got a keyword `do`
--! [Local([`x`$1: _ Function], [])$1, Oops, Do([])]

--8<-- kind-thread
local x --: thread
--! [Local([`x`$1: _ Thread], [])$1]

--8<-- kind-userdata
local x --: userdata
--! [Local([`x`$1: _ UserData], [])$1]

--8<-- kind-seq-outside-func
local x --: (integer, string) --@< Error: Expected a single type, not type sequence
--! [Local([`x`$1: _ Oops], [])$1]

--8<-- kind-paren
local x --: (integer)
--! [Local([`x`$1: _ Integer], [])$1]

--8<-- kind-paren-opt
local x --: (integer)?
--! [Local([`x`$1: _ Integer?], [])$1]

--8<-- kind-paren-opt-multiline-1
local x --:
        --: (integer)?
--! [Local([`x`$1: _ Integer?], [])$1]

--8<-- kind-paren-opt-multiline-2
local x --: (
        --:   integer
        --: )?
--! [Local([`x`$1: _ Integer?], [])$1]

--8<-- kind-paren-multiline-recover
local x --: (
        --:   integer --@< Error: Expected `)`, got a newline
--! [Local([`x`$1: _ Oops], [])$1]

--&

--8<-- kind-table-empty
local x --: {}
--! [Local([`x`$1: _ EmptyTable], [])$1]

--8<-- kind-rec
local x --: {a: const function (), b: string,
        --:  c: const function (string, integer) --> number}?
--! [Local([`x`$1: _ Record(["a": Const Func(() --> ()), \
--!                          "b": _ String, \
--!                          "c": Const Func((String, Integer) --> Number)\
--!                         ])?], [])$1]

--8<-- kind-rec-extensible
local x --: {a: string, ...}
local y --: {b: integer;
        --:  a: string;
        --:  ...
        --: }
local z --: {...}
--! [Local([`x`$1: _ Record(["a": _ String, ...])], [])$1, \
--!  Local([`y`$2: _ Record(["b": _ Integer, "a": _ String, ...])], [])$2, \
--!  Local([`z`$3: _ Record([...])], [])$3]

--8<-- kind-rec-extensible-recover-1
local x --: {a: string, ... --@<-v Error: Expected `}`, got a newline
local y --: {b: string, ...}
--! [Local([`x`$1: _ Record(["a": _ String, ...])], [])$1, \
--!  Local([`y`$2: _ Record(["b": _ String, ...])], [])$2]

--8<-- kind-rec-extensible-recover-2
local x --: {a: string, ... | nil --@< Error: Expected `}`, got `|`
local y --: {b: string, ...}
--! [Local([`x`$1: _ Record(["a": _ String, ...])], [])$1, \
--!  Local([`y`$2: _ Record(["b": _ String, ...])], [])$2]

--8<-- kind-tuple
local x --: {const function (); string;
        --:  const function (string, integer) --> number;
        --: }?
--! [Local([`x`$1: _ Tuple([Const Func(() --> ()), \
--!                         _ String, \
--!                         Const Func((String, Integer) --> Number)\
--!                        ])?], [])$1]

--8<-- kind-tuple-1
local x --: {WHATEVER}
--! [Local([`x`$1: _ Tuple([_ Dynamic])], [])$1]

--8<-- kind-tuple-1-comma
local x --: {WHATEVER,}
--! [Local([`x`$1: _ Tuple([_ Dynamic])], [])$1]

--8<-- kind-tuple-2-comma
local x --: {WHATEVER,WHATEVER}
--! [Local([`x`$1: _ Tuple([_ Dynamic, _ Dynamic])], [])$1]

--8<-- kind-tuple-2-semicolon
local x --: {WHATEVER;WHATEVER;}
--! [Local([`x`$1: _ Tuple([_ Dynamic, _ Dynamic])], [])$1]

--8<-- kind-array
local x --: vector<integer>
local y --: vector<const integer>
--! [Local([`x`$1: _ Array(_ Integer)], [])$1, \
--!  Local([`y`$2: _ Array(Const Integer)], [])$2]

--8<-- kind-array-recover-1
local x --: vector<integer, string> --@< Error: `vector` type needs a single type parameter
local y --: vector<integer>
--! [Local([`x`$1: _ Oops], [])$1, \
--!  Local([`y`$2: _ Array(_ Integer)], [])$2]

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
--! [Local([`x`$1: _ Oops], [])$1, \
--!  Local([`y`$2: _ Oops], [])$2, \
--!  Local([`z`$3: _ Array(_ Integer)], [])$3, \
--!  Local([`w`$4: _ Array(_ Oops)], [])$4]

--8<-- kind-array-recover-3
local x --: vector<integer, --@<-v Error: Expected a single type, got a newline
                            --@^-< Error: Expected `>` or `>>`, got a newline
                            --@^^ Error: `vector` type needs a single type parameter
local y --: vector<boolean  --@<-v Error: Expected `>` or `>>`, got a newline
local z --: vector<integer>
local w --: vector          --@<-v Error: Expected a list of type parameters, got a newline
--! [Local([`x`$1: _ Oops], [])$1, \
--!  Local([`y`$2: _ Array(_ Boolean)], [])$2, \
--!  Local([`z`$3: _ Array(_ Integer)], [])$3, \
--!  Local([`w`$4: _ Oops], [])$4]

--8<-- kind-array-reserved
local x --: `vector` --@< Error: The type name `vector` is reserved and cannot be used
local y --: `vector`<const integer> --@< Error: The type name `vector` is reserved and cannot be used
                                    --@^ Error: Expected a newline, got `<`
--! [Local([`x`$1: _ Oops], [])$1, \
--!  Local([`y`$2: _ Oops], [])$2]

--8<-- kind-map
local x --: map<string, const integer>
--! [Local([`x`$1: _ Map(String, Const Integer)], [])$1]

--8<-- kind-map-or-1
local x --: map<string, integer|boolean>
--! [Local([`x`$1: _ Map(String, _ Union([Integer, Boolean]))], [])$1]

--8<-- kind-map-or-2
local x --: map<string|boolean, integer|boolean>
--! [Local([`x`$1: _ Map(Union([String, Boolean]), _ Union([Integer, Boolean]))], [])$1]

--8<-- kind-map-opt
local x --: map<string?, integer?>
--! [Local([`x`$1: _ Map(String?, _ Integer?)], [])$1]

--8<-- kind-map-recover-1
local x --: map<integer, string, boolean> --@< Error: `map` type needs two type parameters
local y --: map<integer, string>
local z --: map<integer> --@< Error: `map` type needs two type parameters
--! [Local([`x`$1: _ Oops], [])$1, \
--!  Local([`y`$2: _ Map(Integer, _ String)], [])$2, \
--!  Local([`z`$3: _ Oops], [])$3]

--8<-- kind-map-recover-2
-- XXX excess >/>> expectation error is hard to fix without a special nesting
local x --: map<integer, , string> --@< Error: Expected a single type, got `,`
                                   --@^-< Error: Expected `>` or `>>`, got a newline
local y --: map<boolean,> --@< Error: Expected a single type, got `>`
                          --@^-< Error: Expected `>` or `>>`, got a newline
local z --: map<integer, string>
--! [Local([`x`$1: _ Map(Integer, _ Oops)], [])$1, \
--!  Local([`y`$2: _ Map(Boolean, _ Oops)], [])$2, \
--!  Local([`z`$3: _ Map(Integer, _ String)], [])$3]

--8<-- kind-map-recover-3
local x --: map<const integer, string> --@< Error: The first type parameter of `map` type cannot have modifiers
--! [Local([`x`$1: _ Map(Integer, _ String)], [])$1]

--8<-- kind-nested-table
local x --: map<integer, const vector<map<string, {integer, integer}?>>>
--! [Local([`x`$1: _ Map(Integer, \
--!                      Const Array(_ Map(String, \
--!                                        _ Tuple([_ Integer, _ Integer])?)))], [])$1]

--8<-- kind-attr-1
local x --: [builtin] string
--! [Local([`x`$1: _ [`builtin`] String], [])$1]

--8<-- kind-attr-2
local x --: [builtin] function(any)
--! [Local([`x`$1: _ [`builtin`] Func((Any) --> ())], [])$1]

--8<-- kind-attr-paren
local x --: ([builtin] (string))
--! [Local([`x`$1: _ [`builtin`] String], [])$1]

--8<-- kind-attr-empty
local x --: [] string --@< Error: Expected a name, got `]`
--! [Local([`x`$1: _ String], [])$1]

--8<-- kind-attr-wrong
local x --: [built-in] string --@< Error: Expected `]`, got `-`
local y --: [built_in] string
--! [Local([`x`$1: _ String], [])$1, \
--!  Local([`y`$2: _ [`built_in`] String], [])$2]

--8<-- kind-attr-wrong-recover
local x --: [built-in(function(i) return _G[i] end] string --@< Error: Expected `]`, got `-`
local y --: [built_in] string
--! [Local([`x`$1: _ String], [])$1, \
--!  Local([`y`$2: _ [`built_in`] String], [])$2]

--8<-- kind-attr-keyword
local x --: [type] function(any)
--! [Local([`x`$1: _ [`type`] Func((Any) --> ())], [])$1]

--8<-- kind-attr-dup
local x --: [builtin] [builtin] string --@< Error: Expected a single type, got `[`
--! [Local([`x`$1: _ Oops], [])$1]

--8<-- kind-attr-seq
local x --: function() --> [builtin] (string, string)
--@^ Error: Cannot attach the type attribute (like [name]) to the type sequence
--! [Local([`x`$1: _ Func(() --> (String, String))], [])$1]

--8<-- kind-or-recover
function foo()
    local x --: integer | --@<-v Error: Expected a type, got a newline
end
--! [FuncDecl(`foo`_, [] --> _, $1[Local([`x`$2: _ Union([Integer])], [])$2])]

--8<-- funcspec
--v function()
function foo() end
--! [FuncDecl(`foo`_, [], $1[])]
-- note that the return is specified (not `_`)

--8<-- funcspec-more-arity-1
--v function(a: integer,
--v          b: string) --@^-< Error: Excess arguments in the function specification
function foo() end
--! [FuncDecl(`foo`_, [`a`$1: _ Integer, `b`$1: _ String], $1[])]

--8<-- funcspec-more-arity-2
--v function(a: integer,
--v          b: string) --@< Error: Excess arguments in the function specification
function foo(a) end
--! [FuncDecl(`foo`_, [`a`$1: _ Integer, `b`$1: _ String], $1[])]

--8<-- funcspec-wrong-name
--v function(a: integer)
function foo(b) end --@< Error: Mismatching argument name in the function specification
                    --@^^ Note: The corresponding argument was here
--! [FuncDecl(`foo`_, [`a`$1: _ Integer], $1[])]

--8<-- funcspec-less-arity-1
--v function()
function foo(a,
             b) end --@^-< Error: Excess arguments in the function declaration
--! [FuncDecl(`foo`_, [], $1[])]

--8<-- funcspec-less-arity-2
--v function(a: integer)
function foo(a,
             b) end --@< Error: Excess arguments in the function declaration
--! [FuncDecl(`foo`_, [`a`$1: _ Integer], $1[])]

--8<-- funcspec-swapped-name
--v function(a: integer, b: integer)
function foo(b, a) end --@< Error: Mismatching argument name in the function specification
                       --@^^ Note: The corresponding argument was here
                       --@^^ Error: Mismatching argument name in the function specification
                       --@^^^^ Note: The corresponding argument was here
--! [FuncDecl(`foo`_, [`a`$1: _ Integer, `b`$1: _ Integer], $1[])]

--8<-- funcspec-1
--v function(a: integer)
local function foo(a) end
--! [FuncDecl(`foo`$2, [`a`$1: _ Integer], $1[])$2]

--8<-- funcspec-1-seq-1
--v function(a: integer) --> (WHATEVER...)
local function foo(a) end
--! [FuncDecl(`foo`$2, [`a`$1: _ Integer] --> [Dynamic...], $1[])$2]

--8<-- funcspec-1-seq-2
--v function(a: integer) --> (string, WHATEVER...)
local function foo(a) end
--! [FuncDecl(`foo`$2, [`a`$1: _ Integer] --> [String, Dynamic...], $1[])$2]

--8<-- funcspec-inline
(--v function(a: const integer,
 --v          ...)
 --v         --> string
 function(a, ...) end)()
--! [Void((Func([`a`$1: Const Integer, ...: _] --> String, $1[]))())]

--8<-- funcspec-attr-1
--v [no_check] function()
function foo() end
--! [FuncDecl(`foo`_, [`no_check`] [], $1[])]

--8<-- funcspec-attr-2
--v [no_check]
--v function()
function foo() end
--! [FuncDecl(`foo`_, [`no_check`] [], $1[])]

--8<-- funcspec-attr-multi-1
--v [no_check] [self_destruct] function()
function foo() end
--! [FuncDecl(`foo`_, [`no_check`] [`self_destruct`] [], $1[])]

--8<-- funcspec-attr-multi-2
--v [no_check]
--v [self_destruct]
--v function()
function foo() end
--! [FuncDecl(`foo`_, [`no_check`] [`self_destruct`] [], $1[])]

--8<-- funcspec-attr-only
--v [no_check]
function foo() end
--! [FuncDecl(`foo`_, [`no_check`] [] --> _, $1[])]
-- note the trailing `_`, which indicates that there was no pre-signature

--8<-- funcspec-attr-incomplete
--v [no_check --@<-v Error: Expected `]`, got a newline
function foo() end
--! [FuncDecl(`foo`_, [] --> _, $1[])]

--8<-- funcspec-no-empty
--v --@<-v Error: Expected a keyword `function`, got a newline
function foo() end
--! [FuncDecl(`foo`_, [] --> _, $1[])]

--8<-- funcspec-no-bare-parens
--v () --@< Error: Expected a keyword `function`, got `(`
function foo() end
--! [FuncDecl(`foo`_, [] --> _, $1[])]

--8<-- rettype-none
function foo() --> ()
end
--! [FuncDecl(`foo`_, [], $1[])]

--8<-- rettype-string
function foo() --> string
end
--! [FuncDecl(`foo`_, [] --> String, $1[])]

--8<-- rettype-string-parens
function foo() --> ((((string))))
end
--! [FuncDecl(`foo`_, [] --> String, $1[])]

--8<-- rettype-seq-1
function foo() --> (WHATEVER...)
end
--! [FuncDecl(`foo`_, [] --> [Dynamic...], $1[])]

--8<-- rettype-seq-2
function foo() --> (string, WHATEVER...)
end
--! [FuncDecl(`foo`_, [] --> [String, Dynamic...], $1[])]

--8<-- rettype-recover
--@v-vv Error: Expected a single type or type sequence, got a newline
function foo() -->
end
--! [FuncDecl(`foo`_, [] --> Oops, $1[])]

--8<-- funcspec-and-rettype
--v function()
function foo() --> string
    --@^ Error: Inline return type specification cannot appear with the function specification
end --@^^^ Note: The function specification appeared here
--! [FuncDecl(`foo`_, [], $1[])]

--8<-- funcspec-and-argtype-rettype-1
--v function(a: integer)
function foo(a) --: integer --> string
    --@^ Error: Inline argument type specification cannot appear with the function specification
end --@^^^ Note: The function specification appeared here
    --@^^^ Error: Inline return type specification cannot appear with the function specification
    --@^^^^^ Note: The function specification appeared here
--! [FuncDecl(`foo`_, [`a`$1: _ Integer], $1[])]

--8<-- funcspec-and-argtype-rettype-2
--v function(a: integer, b: boolean)
function foo(a, --: integer --@< Error: Inline argument type specification cannot appear with the function specification
                            --@^^ Note: The function specification appeared here
             b) --> string  --@< Error: Inline return type specification cannot appear with the function specification
                            --@^^^^ Note: The function specification appeared here
end
--! [FuncDecl(`foo`_, [`a`$1: _ Integer, `b`$1: _ Boolean], $1[])]

--8<-- funcspec-before-nothing
--v function() --@< Error: No function declaration after the function specification
--! []

--8<-- funcspec-before-local
--v function() --@< Error: No function declaration after the function specification
local v = 42
--! [Local([`v`$1], [42])$1]

--8<-- funcspec-before-local-recover
--v function() --@< Error: No function declaration after the function specification
local v = 42
--v function() --@< Error: No function declaration after the function specification
--! [Local([`v`$1], [42])$1]

--8<-- funcspec-before-assume
--v function() --@< Error: No function declaration after the function specification
--# assume x: integer
--! [KailuaAssume(`x`$1, `x`_, _, Integer)$1]

--8<-- funcspec-before-for
--v function() --@< Error: No function declaration after the function specification
for i = 1, 3 do end
--! [For(`i`$1, 1, 3, None, $1[])]

--8<-- funcspec-before-expr-inline
f(--v function() --@< Error: No function literal after the function specification
  g())
--! [Void(`f`_(`g`_()))]

--8<-- argtype-inline
function foo(a, --: integer
             b, ...) --: string
end
--! [FuncDecl(`foo`_, [`a`$1: _ Integer, `b`$1, ...: String] --> _, $1[])]

--8<-- funcspec-and-argtype
--v function(a: integer, b: boolean, ...: string)
function foo(a, b, ...) --: string
    --@^ Error: Inline variadic argument type specification cannot appear with the function specification
    --@^^^ Note: The corresponding argument in the function specification was here
end
--! [FuncDecl(`foo`_, [`a`$1: _ Integer, `b`$1: _ Boolean, ...: String], $1[])]

--8<-- funcspec-less-varargs
--v function(a: integer, b: boolean)
function foo(a, b, ...)
    --@^ Error: Variadic arguments appear in the function but not in the function specification
end
--! [FuncDecl(`foo`_, [`a`$1: _ Integer, `b`$1: _ Boolean], $1[])]

--8<-- funcspec-more-varargs
--@v Error: Variadic arguments appear in the function specification but not in the function itself
--v function(a: integer, b: boolean, ...: string)
function foo(a, b)
end
--! [FuncDecl(`foo`_, [`a`$1: _ Integer, `b`$1: _ Boolean, ...: String], $1[])]

--8<-- funcspec-varargs
--v function(a: integer, b: boolean, ...: string)
function foo(a, b, ...)
end
--! [FuncDecl(`foo`_, [`a`$1: _ Integer, `b`$1: _ Boolean, ...: String], $1[])]

--8<-- funcspec-varargs-0
--v function(...: string)
function foo(...)
end
--! [FuncDecl(`foo`_, [...: String], $1[])]

--8<-- local-funcspec-varargs-0
--v function(...: string)
local function foo(...)
end
--! [FuncDecl(`foo`$2, [...: String], $1[])$2]

--8<-- funcspec-method-no-self-1
--v function() --@< Error: The first argument in the function specification for a method is not `self`
function foo:bar() end
--! [MethodDecl((`foo`_.`bar`), Some(self=`self`$1), [], $1[])]

--8<-- funcspec-method-no-self-2
--@v-vv Error: The first argument in the function specification for a method is not `self`
--v function(x: integer,
--v          y: string)
function foo:bar(x, y) end
--! [MethodDecl((`foo`_.`bar`), Some(self=`self`$1), [`x`$1: _ Integer, `y`$1: _ String], $1[])]

--8<-- funcspec-method-self
--v function(self)
function foo:bar() end
--! [MethodDecl((`foo`_.`bar`), Some(self=`self`$1), [], $1[])]

--8<-- funcspec-method-self-typed
--v function(self: table)
function foo:bar() end
--! [MethodDecl((`foo`_.`bar`), Some(self=`self`$1: _ Table), [], $1[])]

--8<-- funcspec-method-self-typed-with-modf
--v function(self: const table)
function foo:bar() end
--! [MethodDecl((`foo`_.`bar`), Some(self=`self`$1: Const Table), [], $1[])]

--8<-- funcspec-non-method-self-1
--v function(self, x: integer) --@< Error: Arguments in the function specification are missing their types
                               --@^ Error: Excess arguments in the function specification
function foo.bar(x) end --@< Error: Mismatching argument name in the function specification
                        --@^^^ Note: The corresponding argument was here
--! [MethodDecl((`foo`_.`bar`), None, [`self`$1, `x`$1: _ Integer], $1[])]

--8<-- funcspec-non-method-self-2
--v function(self, x: integer) --@< Error: Arguments in the function specification are missing their types
function foo.bar(self, x) end
--! [MethodDecl((`foo`_.`bar`), None, [`self`$1, `x`$1: _ Integer], $1[])]

--8<-- funcspec-non-method-self-typed-1
--v function(self: table, x: integer) --@< Error: Excess arguments in the function specification
function foo.bar(x) end --@< Error: Mismatching argument name in the function specification
                        --@^^ Note: The corresponding argument was here
--! [MethodDecl((`foo`_.`bar`), None, [`self`$1: _ Table, `x`$1: _ Integer], $1[])]

--8<-- funcspec-non-method-self-typed-2
--v function(self: table, x: integer)
function foo.bar(self, x) end
--! [MethodDecl((`foo`_.`bar`), None, [`self`$1: _ Table, `x`$1: _ Integer], $1[])]

--8<-- assume-multiline-recover
--# assume a: { integer, string
--#                      boolean --@< Error: Expected `,`, `;` or `}`, got a name
--! [KailuaAssume(`a`$1, `a`_, _, Tuple([_ Integer, _ String]))$1]

--8<-- long-string-incomplete-1
f([==== [ --@< Error: Opening long bracket in a string should end with `[`
          --@^ Error: Expected `)`, got `[`
--! [Void(`f`_(""))]

--&
) -- highlighting fix

--8<-- long-string-incomplete-2
f([==== --@< Error: Opening long bracket in a string should end with `[`
        --@^ Error: Expected `)`, got the end of file
--! [Void(`f`_(""))]

--&
) -- highlighting fix

--8<-- long-string-incomplete-3
f([====[ --@< Error: Premature end of file in a long string
         --@^ Note: The long string started here
         --@^^ Error: Expected `)`, got the end of file
--! [Void(`f`_(""))]

--&
]====]) -- highlighting fix

--8<-- long-string-incomplete-4
f([====[foo] --@< Error: Premature end of file in a long string
             --@^ Note: The long string started here
             --@^^ Error: Expected `)`, got the end of file
--! [Void(`f`_("foo"))]

--&
]====]) -- highlighting fix

--8<-- long-comment-incomplete-1
-- unlike long-string-incomplete-1 this is not an error!
f(--[==== [
)
--! [Void(`f`_())]

--8<-- long-comment-incomplete-1-eof
--[==== [
--! []

--8<-- long-comment-incomplete-2
-- unlike long-string-incomplete-2 this is not an error!
f(--[====
)
--! [Void(`f`_())]

--8<-- long-comment-incomplete-2-eof
--[====
--! []

--8<-- long-comment-incomplete-3
--@vvv Error: Premature end of file in a long comment
--@v Note: The long comment started here
f(--[====[
) --@< Error: Expected `)`, got the end of file
--! [Void(`f`_())]

--&
]====] -- highlighting fix

--8<-- long-comment-incomplete-4
--@vvv Error: Premature end of file in a long comment
--@v Note: The long comment started here
f(--[====[foo]
) --@< Error: Expected `)`, got the end of file
--! [Void(`f`_())]

--&
]====] -- highlighting fix

--8<-- meta-eof
--@v Error: Expected a single type, got a newline
--# assume p:
--&
--! [KailuaAssume(`p`$1, `p`_, _, Oops)$1]

--8<-- meta-long-string
--# assume p: [[xxx   --@< Error: A newline is disallowed in a long string inside the meta block
--#             yyy]] --@^ Note: The meta block started here
                      --@^ Error: Expected a newline, got a name
f()
--! [KailuaAssume(`p`$1, `p`_, _, String("xxx   "))$1, Void(`f`_())]

--8<-- meta-long-comment-1
--# assume p: --[[xxx   --@< Error: A newline is disallowed in a long comment inside the meta block
--#               yyy]] --@^ Note: The meta block started here
                        --@^ Error: Expected a newline, got `]`
f()
--! [KailuaAssume(`p`$1, `p`_, _, `yyy`)$1, Void(`f`_())]

--8<-- meta-long-comment-2
--# assume p: --[[xxx   --@< Error: A newline is disallowed in a long comment inside the meta block
                  yyy]] --@^ Note: The meta block started here
                        --@^^-^ Error: Expected a single type, got a newline
                        --@^^ Error: Only function calls are allowed as statement-level expressions
                        --@^^^ Error: Expected a statement, got `]`
                        --@^^^^ Error: Expected a statement, got `]`
--! [KailuaAssume(`p`$1, `p`_, _, Oops)$1, Void(`yyy`_), Oops, Oops]

--8<-- string-multi-line
f('foo\
bar')
--! [Void(`f`_("foo\nbar"))]

--8<-- string-multi-line-recover
f('foo)
) --@< Error: Unescaped newline in a string
  --@^^ Note: The string started here
g()
--! [Void(`f`_("foo)")), Void(`g`_())]

--&
') -- highlighting fix

--8<-- string-wrong-escape
f('foo\xyz') --@< Error: Unrecognized escape sequence in a string
--! [Void(`f`_("fooyz"))]

--8<-- string-wrong-escape-recover
f('foo\xyz', 'bar\zyx') --@< Error: Unrecognized escape sequence in a string
                        --@^ Error: Unrecognized escape sequence in a string
--! [Void(`f`_("fooyz", "baryx"))]

--8<-- string-incomplete-escape
f('foo\ --@< Error: Premature end of file in a string
        --@^ Note: The string started here
        --@^^ Error: Expected `)`, got the end of file
--! [Void(`f`_("foo"))]

--&
') -- highlighting fix

--8<-- string-incomplete
--@vvv Error: Premature end of file in a string
--@vv Note: The string started here
--@v Error: Expected `)`, got the end of file
f('foo
--&
--! [Void(`f`_("foo"))]

') -- highlighting fix

--8<-- number-long
f(0x12345678901234567)
--! [Void(`f`_(20988295476557332000))]
-- relies on std's correct f64 rounding

--8<-- number-incomplete-exp-1
f(3e --@< Error: Invalid number
     --@^ Error: Expected `)`, got the end of file
--! [Void(`f`_())]

--&
) -- highlighting fix

--8<-- number-incomplete-exp-1-recover
f(3e) --@< Error: Invalid number
--! [Void(`f`_())]

--8<-- number-incomplete-exp-2
f(3e+ --@< Error: Invalid number
      --@^ Error: Expected `)`, got `+`
--! [Void(`f`_())]

--&
) -- highlighting fix

--8<-- invalid-char-1
f(~3) --@< Error: Unexpected character
--! [Void(`f`_(3))]

--8<-- invalid-char-2
f(@3) --@< Error: Unexpected character
--! [Void(`f`_(3))]

--8<-- assume-excess
--# assume p: integer f --@< Error: Expected a newline, got a name
f()
--# assume q: integer g --@< Error: Expected a newline, got a name
--! [KailuaAssume(`p`$1, `p`_, _, Integer)$1, Void(`f`_()), \
--!  KailuaAssume(`q`$2, `q`_, _, Integer)$2]

--8<-- for-of
for a of x --@< Error: Expected `=`, `,`, `in` or `--:` after `for NAME`, got a name
--! [Oops]

--8<-- for-in
for a, b, c in pairs({}) do end
--! [ForIn([`a`$1, `b`$1, `c`$1], [`pairs`_({})], $1[])]

--8<-- local-seq
local (x, y) = (1, 2) --@< Error: Expected a name or `function` after `local`, got `(`
                      --@^ Error: Expected `)`, got `,`
                      --@^^ Error: Only function calls are allowed as statement-level expressions
                      --@^^^ Error: Expected a statement, got `=`
                      --@^^^^ Error: Expected `)`, got `,`
                      --@^^^^^ Error: Only function calls are allowed as statement-level expressions
f()
--! [Oops, Void((Oops)), Oops, Void((Oops)), Void(`f`_())]

--8<-- func-args-invalid-char
function p(# --@< Error: Expected an argument name, `)` or `...`, got `#`
             --@^ Error: Expected a keyword `end`, got the end of file
--! [FuncDecl(`p`_, [] --> _, $1[])]

--&
) -- highlighting fix

--8<-- func-args-recover
function p(a, *) --@< Error: Expected a name, got `*`
    print(a)
end
p(4)
--! [FuncDecl(`p`_, [`a`$1] --> _, $1[Void(`print`_(`a`$1))]), Void(`p`_(4))]

--8<-- func-no-args-recover
function p
    print(a) --@< Error: Expected `(` after `function` or `function <name>`, got a name
end
p(4)
--! [Oops, Void(`p`_(4))]

--8<-- argtype-slot
function p(...) --: const integer --@< Error: Variadic argument specifier cannot have modifiers
end
--! [FuncDecl(`p`_, [...: Integer] --> _, $1[])]

--8<-- table-recover-invalid-char
f({x@})  --@< Error: Unexpected character
g({y@})  --@< Error: Unexpected character
h({z,@}) --@< Error: Unexpected character
i({@})   --@< Error: Unexpected character
--! [Void(`f`_({`x`_})), \
--!  Void(`g`_({`y`_})), \
--!  Void(`h`_({`z`_})), \
--!  Void(`i`_({}))]

--8<-- table-recover-invalid-exp
-- in this case `#` is a valid unary operator so the recovery procedure is a bit different
f({x#}) --@< Error: Expected `,`, `;` or `}`, got `#`
g({y#}) --@< Error: Expected `,`, `;` or `}`, got `#`
h({z,#}) --@< Error: Expected an expression, got `}`
i({#}) --@< Error: Expected an expression, got `}`
--! [Void(`f`_({`x`_})), \
--!  Void(`g`_({`y`_})), \
--!  Void(`h`_({`z`_, (# Oops)})), \
--!  Void(`i`_({(# Oops)}))]

--8<-- table-recover-key-1
f({[a]=b, [}) --@< Error: Expected an expression, got `}`
              --@^ Error: Expected `]`, got `}`
              --@^^ Error: Expected `=`, got `}`
f({[a]=})     --@< Error: Expected an expression, got `}`
f({[a=})      --@< Error: Expected `]`, got `=`
              --@^ Error: Expected `=`, got `}`
f({[a]})      --@< Error: Expected `=`, got `}`
f({[a})       --@< Error: Expected `]`, got `}`
              --@^ Error: Expected `=`, got `}`
f({[})        --@< Error: Expected an expression, got `}`
              --@^ Error: Expected `]`, got `}`
              --@^^ Error: Expected `=`, got `}`
--! [Void(`f`_({[`a`_] = `b`_, [Oops] = Oops})), \
--!  Void(`f`_({[`a`_] = Oops})), \
--!  Void(`f`_({[Oops] = Oops})), \
--!  Void(`f`_({[`a`_] = Oops})), \
--!  Void(`f`_({[Oops] = Oops})), \
--!  Void(`f`_({[Oops] = Oops}))]

--8<-- table-recover-key-2
f({[a]=b, [) --@< Error: Expected an expression, got `)`
             --@^ Error: Expected `]`, got `)`
             --@^^ Error: Expected `=`, got `)`
             --@^^^ Error: Expected `,`, `;` or `}`, got `)`
f({[a]=)     --@< Error: Expected an expression, got `)`
             --@^ Error: Expected `,`, `;` or `}`, got `)`
f({[a=)      --@< Error: Expected `]`, got `=`
             --@^ Error: Expected `=`, got `)`
             --@^^ Error: Expected `,`, `;` or `}`, got `)`
f({[a])      --@< Error: Expected `=`, got `)`
             --@^ Error: Expected `,`, `;` or `}`, got `)`
f({[a)       --@< Error: Expected `]`, got `)`
             --@^ Error: Expected `=`, got `)`
             --@^^ Error: Expected `,`, `;` or `}`, got `)`
f({[)        --@< Error: Expected an expression, got `)`
             --@^ Error: Expected `]`, got `)`
             --@^^ Error: Expected `=`, got `)`
             --@^^^ Error: Expected `,`, `;` or `}`, got `)`
f({)         --@< Error: Expected an expression, got `)`
             --@^ Error: Expected `,`, `;` or `}`, got `)`
--! [Void(`f`_({[`a`_] = `b`_, [Oops] = Oops})), \
--!  Void(`f`_({[`a`_] = Oops})), \
--!  Void(`f`_({[Oops] = Oops})), \
--!  Void(`f`_({[`a`_] = Oops})), \
--!  Void(`f`_({[Oops] = Oops})), \
--!  Void(`f`_({[Oops] = Oops})), \
--!  Void(`f`_({Oops}))]

--&
})]})]})]})]})]})]}) -- highlighting fix

--8<-- table-recover-key-3
f({[
f({[ --@< Error: Expected an expression, got the end of file
     --@^ Error: Expected `]`, got the end of file
     --@^^ Error: Expected `=`, got the end of file
     --@^^^ Error: Expected `,`, `;` or `}`, got the end of file
     --@^^^^ Error: Expected `)`, got the end of file
     --@^^^^^ Error: Expected `]`, got the end of file
     --@^^^^^^ Error: Expected `=`, got the end of file
     --@^^^^^^^ Error: Expected `,`, `;` or `}`, got the end of file
     --@^^^^^^^^ Error: Expected `)`, got the end of file
--! [Void(`f`_({[Oops] = Oops}))]

--&
]})]}) -- highlighting fix

--8<-- table-recover-key-4
f({[a
f({[a --@< Error: Expected `]`, got a name
      --@^ Error: Expected `=`, got the end of file
      --@^^ Error: Expected `,`, `;` or `}`, got the end of file
      --@^^^ Error: Expected `)`, got the end of file
--! [Void(`f`_({[Oops] = Oops}))]

--&
]})]}) -- highlighting fix

--8<-- index-with-number
f(x.0) --@< Error: Expected a name after `<expression> .`, got a number
       --@^ Error: Expected `)`, got a number
--! [Void(`f`_(`x`_))]

--8<-- methodcall-no-args
f(x:g - 1) --@< Error: Expected argument(s) after `<expression> : <name>`, got `-`
--! [Void(`f`_((`x`_.`g` - 1)))]

--8<-- funccall-invalid-char
f(2, *3) --@< Error: Expected an expression, got `*`
         --@^ Error: Expected `)`, got `*`
--! [Void(`f`_(2, Oops))]

--8<-- op-invalid-char-1
f(2 + *3) --@< Error: Expected an expression, got `*`
--! [Void(`f`_(((2 + Oops) * 3)))]

--8<-- op-invalid-char-2
f(2 .. *3) --@< Error: Expected an expression, got `*`
--! [Void(`f`_(((2 .. Oops) * 3)))]

--8<-- op-invalid-char-3
f(#*3) --@< Error: Expected an expression, got `*`
--! [Void(`f`_(((# Oops) * 3)))]

--8<-- lval-invalid-char
a, *b = 5 --@< Error: Expected a left-hand-side expression, got `*`
          --@^ Error: Expected a statement, got `*`
f()
--! [Oops, Oops, Assign([`b`_], [5]), Void(`f`_())]

--8<-- assume-invalid-char
--# assume x: #foo --@< Error: Expected a single type, got `#`
f()
--! [KailuaAssume(`x`$1, `x`_, _, Oops)$1, Void(`f`_())]

--8<-- assume-seq-varargs-1
--# assume x: (...) --@< Error: `...` should be preceded with a kind in the ordinary kinds
                    --@^ Error: Expected a single type, not type sequence
f()
--! [KailuaAssume(`x`$1, `x`_, _, Oops)$1, Void(`f`_())]

--8<-- assume-seq-1
--# assume x: (string...) --@< Error: Expected a single type, not type sequence
f()
--! [KailuaAssume(`x`$1, `x`_, _, Oops)$1, Void(`f`_())]

--8<-- assume-seq-varargs-2
--# assume x: (integer, ...) --@< Error: `...` should be preceded with a kind in the ordinary kinds
                             --@^ Error: Expected a single type, not type sequence
f()
--! [KailuaAssume(`x`$1, `x`_, _, Oops)$1, Void(`f`_())]

--8<-- assume-seq-2
--# assume x: (integer, string) --@< Error: Expected a single type, not type sequence
f()
--! [KailuaAssume(`x`$1, `x`_, _, Oops)$1, Void(`f`_())]

--8<-- assume-seq-varargs-3
--# assume x: (integer, string...) --@< Error: Expected a single type, not type sequence
f()
--! [KailuaAssume(`x`$1, `x`_, _, Oops)$1, Void(`f`_())]

--8<-- assume-seq-invalid-char
--# assume x: (integer, #) --@< Error: Expected a type, got `#`
                           --@^ Error: Expected `)`, got `#`
f()
--! [KailuaAssume(`x`$1, `x`_, _, Oops)$1, Void(`f`_())]

--8<-- assume-func-invalid-char
--# assume x: function () --> #foo --@< Error: Expected a single type or type sequence, got `#`
                                   --@^ Error: Expected a newline, got `#`
--! [KailuaAssume(`x`$1, `x`_, _, Func(() --> Oops))$1]

--8<-- assume-named
--# assume x: whatever
--! [KailuaAssume(`x`$1, `x`_, _, `whatever`)$1]

--8<-- assume-rec-invalid-char
--# assume x: {x: integer #} --@< Error: Expected `,`, `;` or `}`, got `#`
--# assume y: {y: integer #} --@< Error: Expected `,`, `;` or `}`, got `#`
--! [KailuaAssume(`x`$1, `x`_, _, Record(["x": _ Integer]))$1, \
--!  KailuaAssume(`y`$2, `y`_, _, Record(["y": _ Integer]))$2]

--8<-- assume-tuple-invalid-char
--# assume x: {integer, integer #} --@< Error: Expected `,`, `;` or `}`, got `#`
--# assume y: {integer, integer #} --@< Error: Expected `,`, `;` or `}`, got `#`
--! [KailuaAssume(`x`$1, `x`_, _, Tuple([_ Integer, _ Integer]))$1, \
--!  KailuaAssume(`y`$2, `y`_, _, Tuple([_ Integer, _ Integer]))$2]

--8<-- assume-rec-duplicate-name
--# assume x: {x: integer, x: string} --@< Error: Duplicate record field `x` in the type specification
                                      --@^ Note: The first duplicate appeared here
--! [KailuaAssume(`x`$1, `x`_, _, Record(["x": _ Integer, "x": _ String]))$1]

--8<-- assume-rec-duplicate-name-recover
--# assume x: {x: integer,
--#            x: string,  --@< Error: Duplicate record field `x` in the type specification
--#                        --@^^ Note: The first duplicate appeared here
--#            y: table,
--#            x: boolean, --@< Error: Duplicate record field `x` in the type specification
--#                        --@^^^^^ Note: The first duplicate appeared here
--#            y: number}  --@< Error: Duplicate record field `y` in the type specification
                           --@^^^^ Note: The first duplicate appeared here
--! [KailuaAssume(`x`$1, `x`_, _, Record(["x": _ Integer, \
--!                                       "x": _ String, \
--!                                       "y": _ Table, \
--!                                       "x": _ Boolean, \
--!                                       "y": _ Number]))$1]

--8<-- assume-builtin-and-literal-recover
--# assume x: WHATEVER = hello --@< Error: Expected a newline, got `=`
--# assume y: "bo\gus"         --@< Error: Unrecognized escape sequence in a string
f(                             --@< Error: Expected `)`, got the end of file
--! [KailuaAssume(`x`$1, `x`_, _, Dynamic)$1, Void(`f`_())]

--&
) -- highlighting fix

--8<-- assume-or-invalid-char
--# assume x: integer | # --@< Error: Expected a type, got `#`
                          --@^ Error: Expected a newline, got `#`
f()
--! [KailuaAssume(`x`$1, `x`_, _, Union([Integer]))$1, Void(`f`_())]

--8<-- assume-or-seq-1
--# assume x: integer | () | nil --@< Error: A sequence of types cannot be inside a union
f(                               --@< Error: Expected `)`, got the end of file
--! [KailuaAssume(`x`$1, `x`_, _, Union([Integer, Oops, Nil]))$1, Void(`f`_())]

--&
) -- highlighting fix

--8<-- assume-or-seq-2
--# assume x: integer | (string,
--#                      boolean) | nil --@^-< Error: A sequence of types cannot be inside a union
f(                                      --@< Error: Expected `)`, got the end of file
--! [KailuaAssume(`x`$1, `x`_, _, Union([Integer, Oops, Nil]))$1, Void(`f`_())]

--&
) -- highlighting fix

--8<-- open-lua51
--# open lua51
--! [KailuaOpen(`lua51`)]

--8<-- open-incomplete
--# open
--# open your heart --@< Error: Expected a name, got a keyword `open`
f()
--! [Oops, Void(`f`_())]

--8<-- alias
--# type Int = integer
--# type local integral = integer
--# type global Integer = integer
--# assume x: vector<Int>
--! [KailuaType(Exported, `Int`, Integer), \
--!  KailuaType(Local, `integral`, Integer), \
--!  KailuaType(Global, `Integer`, Integer), \
--!  KailuaAssume(`x`$1, `x`_, _, Array(_ `Int`))$1]

--8<-- alias-builtin
--# type any = integer --@< Error: Cannot redefine a builtin type
--# assume x: vector<any>
--! [KailuaType(Exported, `any`, Integer), KailuaAssume(`x`$1, `x`_, _, Array(_ Any))$1]

--8<-- alias-incomplete
--# type Int =
--# assume x: vector<Int> --@< Error: Expected a single type, got a keyword `assume`
--! [KailuaType(Exported, `Int`, Oops), KailuaAssume(`x`$1, `x`_, _, Array(_ `Int`))$1]

--8<-- alias-export-in-local-scope
do
    --# type Int = integer --@< Error: `--# type` with an exported type should be in the top-level scope
end
--! [Do([KailuaType(Exported, `Int`, Integer)])]

--8<-- alias-global-in-local-scope
do
    --# type global Int = integer --@< Error: `--# type global` should be in the top-level scope
end
--! [Do([KailuaType(Global, `Int`, Integer)])]

--8<-- alias-local-in-local-scope
do
    --# type local Int = integer
end
--! [Do([KailuaType(Local, `Int`, Integer)])]

--8<-- kind-error
--# type x = error
--! [KailuaType(Exported, `x`, Error)]

--8<-- kind-error-with-message
--# type x = error 'whatever'
--! [KailuaType(Exported, `x`, Error("whatever"))]

--8<-- kind-error-or-string
--# type x = error | 'whatever'
--! [KailuaType(Exported, `x`, Union([Error, String("whatever")]))]

--8<-- lua51-goto-as-a-name
--# open lua51
goto = 42 --@< Warning: The use of a keyword `goto` is discouraged as it was a name in Lua 5.1 but it became a keyword since Lua 5.2
--! [KailuaOpen(`lua51`), Assign([`goto`_], [42])]

--8<-- lua51-goto-as-a-name-in-meta-1
--# open lua51
--# type goto = integer --@< Warning: The use of a keyword `goto` is discouraged as it was a name in Lua 5.1 but it became a keyword since Lua 5.2
f()
--! [KailuaOpen(`lua51`), KailuaType(Exported, `goto`, Integer), Void(`f`_())]

--8<-- lua51-goto-as-a-name-in-meta-2
--# open lua51
--# type `goto` = integer
--! [KailuaOpen(`lua51`), KailuaType(Exported, `goto`, Integer)]

--8<-- type-spec-recover-negative-span
local a = {} --: var { var { } } --@< Error: Expected a single type, got a keyword `var`
local b --: var { var { } }      --@< Error: Expected a single type, got a keyword `var`
--! [Local([`a`$1: _ Oops], [{}])$1, Local([`b`$2: _ Oops], [])$2]

--8<-- non-prefix-expr-at-top-level-1
f --@< Error: Only function calls are allowed as statement-level expressions
--&
--! [Void(`f`_)]

--8<-- non-prefix-expr-at-top-level-2
f.a --@< Error: Only function calls are allowed as statement-level expressions
--&
--! [Void(`f`_.`a`)]

--8<-- non-prefix-expr-at-top-level-3
f[3] --@< Error: Only function calls are allowed as statement-level expressions
--&
--! [Void(`f`_[3])]

--8<-- non-prefix-expr-at-top-level-4
"string" --@< Error: Only function calls are allowed as statement-level expressions
f()
--! [Void("string"), Void(`f`_())]

--8<-- non-prefix-expr-at-top-level-5
42 --@< Error: Only function calls are allowed as statement-level expressions
f()
--! [Void(42), Void(`f`_())]

--8<-- non-prefix-expr-at-top-level-6
f(a).b --@< Error: Only function calls are allowed as statement-level expressions
--&
--! [Void(`f`_(`a`_).`b`)]

--8<-- non-prefix-expr-at-top-level-7
f(a).b --@< Error: Only function calls are allowed as statement-level expressions
g()
--! [Void(`f`_(`a`_).`b`), Void(`g`_())]

--8<-- non-prefix-expr-at-top-level-8
3 + 4 --@< Error: Only function calls are allowed as statement-level expressions
f()
--! [Void((3 + 4)), Void(`f`_())]

--8<-- non-prefix-expr-at-top-level-9
{} --@< Error: Only function calls are allowed as statement-level expressions
f()
--! [Void({}), Void(`f`_())]

--8<-- expr-seq-at-top-level-0
a, --@< Error: Expected a left-hand-side expression, got the end of file
--&
--! []

--8<-- expr-seq-at-top-level-1
a, b --@< Error: Expected `=`, got the end of file
--&
--! [Assign([`a`_, `b`_], _)]

--8<-- expr-seq-at-top-level-2
a, b.c
f() --@< Error: Expected `=`, got a name
--! [Assign([`a`_, `b`_.`c`], _), Void(`f`_())]

--8<-- expr-seq-at-top-level-3
a, b.c
do end --@< Error: Expected `=`, got a keyword `do`
--! [Assign([`a`_, `b`_.`c`], _), Do([])]

--8<-- expr-seq-at-top-level-4
a(), b --@< Error: Expected a statement, got `,`
       --@^ Error: Only function calls are allowed as statement-level expressions
do end
--! [Void(`a`_()), Oops, Void(`b`_), Do([])]

--8<-- index-name-recover-eof-1
f. --@< Error: Expected a name after `<expression> .`, got the end of file
   --@^ Error: Only function calls are allowed as statement-level expressions
--&
--! [Void(`f`_)]

--8<-- index-name-recover-eof-2
f: --@< Error: Expected a name after `<expression> :`, got the end of file
   --@^ Error: Only function calls are allowed as statement-level expressions
--&
--! [Void(`f`_)]

--8<-- index-name-recover-eof-3
f:g --@< Error: Expected argument(s) after `<expression> : <name>`, got the end of file
    --@^ Error: Only function calls are allowed as statement-level expressions
--&
--! [Void(`f`_.`g`)]

--8<-- index-name-recover-keyword
f.
for i = 1, 5 do end --@< Error: Expected a name after `<expression> .`, got a keyword `for`
                    --@^^ Error: Only function calls are allowed as statement-level expressions
f:
repeat until false --@< Error: Expected a name after `<expression> :`, got a keyword `repeat`
                   --@^^ Error: Only function calls are allowed as statement-level expressions
f:g
do end --@< Error: Expected argument(s) after `<expression> : <name>`, got a keyword `do`
       --@^^ Error: Only function calls are allowed as statement-level expressions
--! [Void(`f`_), For(`i`$1, 1, 5, None, $1[]), \
--!  Void(`f`_), Repeat([], false), \
--!  Void(`f`_.`g`), Do([])]

--8<-- index-name-recover-meta
f.
--# assume f: WHATEVER --@< Error: Expected a name after `<expression> .`, got `--#`
                       --@^^ Error: Only function calls are allowed as statement-level expressions
f:
--# assume f: WHATEVER --@< Error: Expected a name after `<expression> :`, got `--#`
                       --@^^ Error: Only function calls are allowed as statement-level expressions
f:g
--# assume f: WHATEVER --@< Error: Expected argument(s) after `<expression> : <name>`, got `--#`
                       --@^^ Error: Only function calls are allowed as statement-level expressions
--! [Void(`f`_), KailuaAssume(`f`$1, `f`_, _, Dynamic)$1, \
--!  Void(`f`$1), KailuaAssume(`f`$2, `f`$1, _, Dynamic)$2, \
--!  Void(`f`$2.`g`), KailuaAssume(`f`$3, `f`$2, _, Dynamic)$3]

--8<-- index-name-recover-do-end
do
    f.
end --@< Error: Expected a name after `<expression> .`, got a keyword `end`
    --@^^ Error: Only function calls are allowed as statement-level expressions
do
    f:
end --@< Error: Expected a name after `<expression> :`, got a keyword `end`
    --@^^ Error: Only function calls are allowed as statement-level expressions
do
    f:g
end --@< Error: Expected argument(s) after `<expression> : <name>`, got a keyword `end`
    --@^^ Error: Only function calls are allowed as statement-level expressions
g()
--! [Do([Void(`f`_)]), \
--!  Do([Void(`f`_)]), \
--!  Do([Void(`f`_.`g`)]), \
--!  Void(`g`_())]

--8<-- index-exp-recover-eof
f[ --@< Error: Expected an expression, got the end of file
   --@^ Error: Expected `]`, got the end of file
   --@^^ Error: Only function calls are allowed as statement-level expressions
--&
--! [Void(`f`_[Oops])]

--8<-- index-exp-recover-keyword
f[
do end --@< Error: Expected an expression, got a keyword `do`
       --@^ Error: Expected `]`, got a keyword `do`
       --@^^^ Error: Only function calls are allowed as statement-level expressions
--! [Void(`f`_[Oops]), Do([])]

--8<-- index-exp-recover-do-end
do
    f[
end --@< Error: Expected an expression, got a keyword `end`
    --@^ Error: Expected `]`, got a keyword `end`
    --@^^^ Error: Only function calls are allowed as statement-level expressions
g()
--! [Do([Void(`f`_[Oops])]), Void(`g`_())]

--8<-- multibyte-invalid-chars
-- should result in only one error
do아햏햏end --@< Error: Unexpected character
--! [Do([])]

