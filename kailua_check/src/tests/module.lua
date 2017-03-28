-- Delayed type checking (`module`) tests for the Kailua type checker.

--8<-- no-module
local M = {}

--v function(n: integer)
function M.foo(n)
    if n > 0 then M.bar(n - 1) end
    --@^ Error: Missing key "bar" in `{foo: <not initialized>, ...}`
end

--v function(n: integer)
function M.bar(n)
    if n > 0 then M.foo(n - 1) end
end

--! error

--8<-- module
local M = {} --: module

--v function(n: integer)
function M.foo(n)
    if n > 0 then M.bar(n - 1) end
end

--v function(n: integer)
function M.bar(n)
    if n > 0 then M.foo(n - 1) end
end

--! ok

--8<-- module-implicit-sig
local M = {} --: module

--@v-vv Error: Delayed type checking via `module` requires the return type to be present
function M.foo()
end

--! error

--8<-- module-explicit-1
local M = {} --: module {...}

--v function(n: integer)
function M.foo(n)
    if n > 0 then M.bar(n - 1) end
end

--v function(n: integer)
function M.bar(n)
    if n > 0 then M.foo(n - 1) end
end

--! ok

--8<-- module-explicit-2
local M = {} --: module {}

--v function()
function M.foo() end --@< Error: Missing key "foo" in `<initializing> {}`

--! error

--8<-- module-table
local M = {} --: module table --@< Error: Cannot assign `{...}` into `<initializing> table`
                              --@^ Note: The other type originates here

--v function()
function M.foo() end --@< Error: Cannot index `<initializing> table` without further type information; specify more detailed type, or use `--# assume` as a last resort

--! error

--8<-- module-integer
local M = 42 --: module

--v function()
function M.foo() end --@< Error: Tried to index a non-table type `<initializing> integer`

--! error

--8<-- module-non-func-decl
local M = {} --: module

-- special treatments for modules are only applied for declarations
M.a = function() M.b() end --: function()
--@^ Error: Cannot index `<initializing> {a: <not initialized>, ...}` with `"b"`
M.b = function() M.a() end --: function()
--! error

--8<-- module-field-assign
local M = {} --: module
M.a = 42
M.b = {1, 2, 3}
--! ok

--8<-- module-reflexive-dependency
local M = {} --: module

--v function()
function M.a() M.a() end

--! ok

--8<-- module-transitive-dependency
local M = {} --: module

--v function()
function M.c() M.d() end
--@^ Error: Cannot index `<initializing> {a: function() --> (), b: function() --> (), c: function() --> (), ...}` with `"d"`

--v function()
function M.b() M.c() end

--v function()
function M.a() M.b() end

--! error

--8<-- module-mutual-dependency-1
local A = {} --: module
local B = {} --: module

--v function()
function A.x() B.y() end

--v function()
function B.y() A.x() end

--! ok

--8<-- module-mutual-dependency-2
local A = {} --: module
local B = {} --: module

--v function()
function A.x() B.x() end
--@^ Error: Cannot index `<initializing> {y: function() --> (), ...}` with `"x"`

--v function()
function B.y() A.y() end
--@^ Error: Cannot index `<initializing> {x: function() --> (), ...}` with `"y"`

--! error

--8<-- module-dup-1
local M = {} --: module

--v function(n: integer)
function M.foo(n)
end

--v function(n: string)
function M.foo(n) --@< Error: Cannot assign `function(n: string) --> ()` into `function(n: integer) --> ()`
                   --@^^-vv Note: The other type originates here
    M.foo(#n)
end

--! error

--8<-- module-dup-2
local M = {} --: module

--v function(n: integer)
function M.foo(n)
    local x = n * 'string' --@< Error: Cannot apply * operator to `integer` and `"string"`
                           --@^ Cause: `"string"` is not a subtype of `number`
end

--v function(n: integer)
function M.foo(n)
    local x = n * 'another string' --@< Error: Cannot apply * operator to `integer` and `"another string"`
                                   --@^ Cause: `"another string"` is not a subtype of `number`
end

--! error

--8<-- module-field
local A, B = {}, {}

A.a = {} --: module
B.b = {} --: module

--v function()
function A.a.x() B.b.y() end

--v function()
function B.b.y() A.a.x() end

--! ok

--8<-- module-field-explicit
local A, B = {}, {}

A.a = {} --: module {...}
B.b = {} --: module {...}

--v function()
function A.a.x() B.b.y() end

--v function()
function B.b.y() A.a.x() end

--! ok

--8<-- module-field-nested
local A = {} --: module

A.B = {} --: module

--v function()
function A.x() A.B.y() end

--v function()
function A.B.y() A.x() end

--! ok

--8<-- module-field-nested-explicit
local A = {} --: module {...}

A.B = {} --: module {...}

--v function()
function A.x() A.B.y() end

--v function()
function A.B.y() A.x() end

--! ok

--8<-- module-func-args-1
--v function(t: module {...})
function foo(t)
    --v function()
    function t.a() t.b() end
    --v function()
    function t.b() t.a() end
end

-- `module` in the function signature is strictly local
local bar = foo --: function(t: {a: function(), b: function(), ...})
--! ok

--8<-- module-func-args-2
local M = {} --: module

--v function(n: integer)
function M.a(n) M.b(n - 1) end

--v function(n: integer)
function M.b(n) M.a(n + 1) end

--v function(t: {a: function(integer), b: function(integer), ...})
function foo(t)
    t.a(4)
    t.b(5)
end

foo(M)

--! ok

--8<-- module-partial-init-1
local M = {} --: module

--v function()
function M.a() M.b() end
--@^ Error: Cannot index `<initializing> {a: function() --> (), ...}` with `"b"`

M.a()
--! error

--8<-- module-partial-init-2
local M = {} --: module

--v function()
function M.a() M.b() end

M.a() -- this will error, but this is within the guarantee (since M.b is ultimately nilable)

--v function()
function M.b() M.a() end
--! ok

--8<-- module-partial-init-3
do
    M = {} --: module

    --v function()
    function M.a() end
end
--! ok

--8<-- module-scope-1
do
    M = {} --: module
    --v function()
    function M.a() M.b() end
    --v function()
    function M.b() M.a() end
end

--v function()
function M.c() M.d() end
--@^ Error: Cannot index `{a: function() --> (), b: function() --> (), c: <not initialized>, ...}` with `"d"`
--v function()
function M.d() M.c() end

--! error

--8<-- module-scope-2
do
    M = {} --: module
    --v function()
    function M.a() M.b() end
    --v function()
    function M.b() M.c() end
    --@^ Error: Cannot index `<initializing> {a: function() --> (), b: function() --> (), ...}` with `"c"`
end

--v function()
function M.c() M.d() end
--@^ Error: Cannot index `{a: function() --> (), b: function() --> (), c: <not initialized>, ...}` with `"d"`
--v function()
function M.d() M.a() end

--! error

--8<-- module-conditional-scope
local M = {} --: module
local b = false --: boolean

if b then
    --v function()
    function M.a() M.c() end
else
    --v function()
    function M.b() M.c() end
end

--v function()
function M.c()
    if not b then M.a() else M.b() end
end

--! ok

--8<-- module-function-scope
local M = {} --: module

function foo()
    --v function()
    function M.x() M.y() end
end

function bar()
    --v function()
    function M.y() M.x() end
end

foo()
bar()

--! ok

--8<-- module-assign-1
local M = {} --: module

--v function()
function M.a() M.b() end

M = {a = function() end}

--v function()
function M.b() M.a() end

--! ok

--8<-- module-assign-2
local M = {} --: module

--v function()
function M.a() M.b() end

M = {} --@< Error: Cannot assign `{...}` into `<initializing> {a: function() --> (), ...}`
       --@^ Note: The other type originates here

--v function()
function M.b() M.a() end

--! error

--8<-- module-mutual-sharing-1
local N = {}
local M = {} --: module
N = M -- this is fine

--v function()
function N.a() N.b() end --@< Error: Missing key "b" in `{a: <not initialized>, ...}`

--v function()
function N.b() N.a() end
--! error

--8<-- module-mutual-sharing-2
local N = {}
local M = {} --: module
N = M

--v function()
function M.a() M.b() end

--v function()
function N.b() N.a() end

M.b()
N.a()
--! ok

--8<-- module-nested-decl-1
local M = {} --: module

--v function()
function M.a()
    --v function()
    function M.c()
        M.b()
    end

    --v function()
    function M.b()
        M.a()
    end
end

M.b() --@< Error: Cannot index `<initializing> {a: function() --> (), ...}` with `"b"`
--! error

--8<-- module-nested-decl-2
local M = {} --: module

--v function()
function M.a()
    --v function()
    function M.b()
        --v function()
        function M.c()
            M.a()
        end
    end
end
--! ok

--8<-- module-nested-decl-3
local M = {} --: module

--v function()
function M.a()
    --v function()
    function M.b()
        --v function()
        function M.c()
            M.d()
            --@^ Error: Cannot index `<initializing> {a: function() --> (), b: function() --> (), c: function() --> (), ...}` with `"d"`
        end
    end
end
--! error

--8<-- module-nested-decl-4
local M = {} --: module

--v function()
function M.a()
    --v function()
    function M.b()
        -- this is checked *after* M.d is processed
        --v function()
        function M.c()
            M.d()
        end
    end

    --v function()
    function M.d()
        M.c()
    end
end
--! ok

--8<-- module-decl-name-scope-local
local M = {} --: module

--v function() --> integer
function M.a()
    return x --@< Error: Global or local variable `x` is not defined
end

local x = 42
--! error

--8<-- module-decl-name-scope-global
local M = {} --: module

--v function() --> integer
function M.a()
    return x
end

x = 42
--! ok

--8<-- module-decl-type-scope-local
local M = {} --: module

--v function() --> integer
function M.a()
    local x = 42 --: X --@< Error: Type `X` is not defined
    return x
end

--# type local X = integer
--! error

--8<-- module-decl-type-scope-global
local M = {} --: module

--v function() --> integer
function M.a()
    local x = 42 --: X
    return x
end

--# type global X = integer
--! ok

