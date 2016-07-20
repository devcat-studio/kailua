-- Basic tests for the Kailua type checker.

--8<-- funccall-nil
local p
p() --@< Error: Tried to call a non-function `nil`
--! error

--8<-- funccall-func
local function p() end
p()
--! ok

--8<-- funccall-var-outside-of-scope-1
local c
--@v-vvv Warning: These `if` case(s) are never executed
if c then --@< Note: This condition always evaluates to a falsey value
    local p
end
p() --@< Error: Global or local variable `p` is not defined
--! error

--8<-- funccall-var-outside-of-scope-2
local c = true
if c then --@< Warning: This condition always evaluates to a truthy value
    local p
end
p() --@< Error: Global or local variable `p` is not defined
--! error

--8<-- funccall-var-outside-of-scope-3
local c = false
--@v-vvv Warning: These `if` case(s) are never executed
if c then --@< Note: This condition always evaluates to a falsey value
    local p
end
p() --@< Error: Global or local variable `p` is not defined
--! error

--8<-- funccall-var-outside-of-scope-4
local c --: boolean
--@v-vvv Warning: These `if` case(s) are never executed
if c then --@< Note: This condition always evaluates to a falsey value
    local p
end
p() --@< Error: Global or local variable `p` is not defined
--! error

-->8-- funccall-integer-or-nil
local c, p
if c then p = 4 end
p()
--! error

--8<-- funccall-undefined
p() --@< Error: Global or local variable `p` is not defined
--! error

--8<-- funccall-dynamic
--# assume p: WHATEVER
p()
--! ok

--8<-- methodcall-dynamic
--# assume s: WHATEVER
local p = s:find('hello')
--! ok

--8<-- add-number-integer
--# assume p: number
local x = p + 3
--! ok

--8<-- add-number-string
--# assume p: number
local x = p + 'foo' --@< Error: `"foo"` is not a subtype of `number`
--! error

--8<-- add-number-func
local function p() end
local x = p + 'foo' --@< Error: `function() -> ()` is not a subtype of `number`
--! error

--8<-- lt-number-integer-1
--# assume p: number
local x = p < 3
--! ok

--8<-- lt-number-integer-2
--# assume p: integer
local x = 2.17 < p
--! ok

--8<-- lt-integer-number-1
--# assume p: integer
local x = p < 3.14
--! ok

--8<-- lt-integer-number-2
--# assume p: number
local x = 2 < p
--! ok

--8<-- lt-string-string
--# assume p: string
local x = p < 'string'
--! ok

--8<-- lt-string-number
--# assume p: string
local x = p < 3.14 --@< Error: Operands `string` and `number` to < operator should be both numbers or both strings
--! error

--8<-- lt-string-or-number
--# assume p: string|number
local x = p < 3.14 --@< Error: Operand `(number|string)` to < operator should be either numbers or strings but not both
--! error

--8<-- lt-string-or-number-both
--# assume p: 'hello'|number
--# assume q: string|integer
local x = p < q --@< Error: Operand `(number|"hello")` to < operator should be either numbers or strings but not both
                --@^ Error: Operand `(integer|string)` to < operator should be either numbers or strings but not both
--! error

--8<-- lt-func-number
local function p() end
local x = p < 3.14 --@< Error: Cannot apply < operator to `function() -> ()` and `number`
--! error

--8<-- unknown-type
--# assume p: unknown_type --@< Error: Type `unknown_type` is not defined
--! error

--8<-- add-integer-integer
local p = 3 + 4
--! ok

--8<-- add-integer-string
local p = 3 + 'foo' --@< Error: `"foo"` is not a subtype of `number`
--! error

--8<-- add-boolean-integer
local p = true + 7 --@< Error: `true` is not a subtype of `number`
--! error

--8<-- index-empty-with-integer
local p = ({})[3] --@< Error: Cannot index `{}` with `3`
--! error

--8<-- index-intrec-with-integer
local p = ({[3] = 4})[3]
--! ok

--8<-- index-intrec-with-integer-no-key
local p = ({[2] = 4})[3] --@< Error: Cannot index `{2 = 4}` with `3`
--! error

--8<-- index-map-with-integer
local t = {[3] = 4, [8] = 5} --: var {[integer] = integer}
local p = t[3]
--! ok

--8<-- index-map-with-integer-type
local t = {[3] = 4, [8] = 5} --: var {[integer] = integer}
local p = t[3] + 3 --@< Error: `(nil|integer)` is not a subtype of `number`
--! error

--8<-- index-map-with-integer-no-key
local t = {[2] = 4, [8] = 5} --: var {[integer] = integer}
local p = t[3]
--! ok

--8<-- index-map-with-integer-no-subtype
local t = {[2] = 4, [8] = 5} --: var {[integer] = integer}
local p = t.string --@< Error: Cannot index `var {[integer] = integer}` with `"string"`
--! error

--8<-- index-empty-with-name
local p = ({}).a --@< Error: Cannot index `{}` with `"a"`
--! error

--8<-- index-rec-with-name
local p = ({a = 4}).a
--! ok

--8<-- index-rec-with-name-no-key
local p = ({a = 4}).b --@< Error: Cannot index `{a = 4}` with `"b"`
--! error

--8<-- index-rec-with-string
--# assume x: string
local p = ({a = 4})[x]
--@^ Error: Cannot index `{a = 4}` with index `string` that cannot be resolved ahead of time
--! error

--8<-- index-rec-with-weird-string
--# assume x: 'not ice'
local p = ({['not ice'] = 4})[x]
--! ok

--8<-- index-rec-with-weird-string-error
--# assume x: 'ice'
local p = ({['not ice'] = 4})[x] --@< Error: Cannot index `{`not ice` = 4}` with `"ice"`
--! error

--8<-- methodcall-empty
local p = ({}):hello() --@< Error: Cannot index `{}` with `"hello"`
--! error

--8<-- methodcall-rec-1
local x = {hello = function(a) end}
local p = x:hello()
--! ok

--8<-- methodcall-rec-2
local x = {hello = --v (a: table, b: integer)
                   function(a, b) end}
local p = x:hello() --@ Error: `nil` is not a subtype of `integer`
-- XXX missing span
--! error

--8<-- methodcall-rec-3
local x = {hello = --v (a: table, b: integer)
                   function(a, b) end}
local p = x:hello(4)
--! ok

--8<-- methodcall-rec-4
local x = {hello = --v (a: table, b: integer)
                   function(a, b) end}
local p = x:hello('string') --@< Error: `"string"` is not a subtype of `integer`
--! error

--8<-- methodcall-rec-5
local x = {hello = function() end}
local p = x:hello() --@< Error: `{hello = function() -> ()}` is not a subtype of `nil`
-- XXX error is bad, should mention about the method call
--! error

--8<-- methodcall-integer
local s = (42):char() --@< Error: Tried to index a non-table type `42`
--! error

--8<-- index-func
local p = (function() end)[3] --@< Error: Tried to index a non-table type `function() -> ()`
--! error

--8<-- currently-nil-to-string
local f
f = 'hello?'
--! ok

--8<-- currently-func-to-func
local f = function() end
f = function() return 54 end
--! ok

--8<-- currently-func-to-table
local f = function() end
f = {54, 49}
--! ok

--8<-- assume-rec
local f = function() end
--# assume f: {index = integer}
local p = f.index
--! ok

--8<-- assume-table
local f = function() end
--# assume f: table
local p = f.index --@< Error: Cannot index `table` without downcasting
--! error

--8<-- conjunctive-lhs-1
local a = ('string' and 53) + 42
--! ok

--8<-- conjunctive-lhs-1
local a = (53 and 'string') + 42 --@< Error: `"string"` is not a subtype of `number`
--! error

--8<-- conjunctive-rhs-1
local a = (nil and 'string') + 42 --@< Error: `nil` is not a subtype of `number`
--! error

--8<-- conjunctive-rhs-2
local a = (nil and 53) + 42 --@< Error: `nil` is not a subtype of `number`
--! error

--8<-- disjunctive-lhs-1
local a = (53 or 'string') + 42
--! ok

--8<-- disjunctive-lhs-2
local a = (53 or nil) + 42
--! ok

--8<-- disjunctive-rhs-1
local a = (nil or 'string') + 42 --@< Error: `"string"` is not a subtype of `number`
--! error

--8<-- disjunctive-rhs-2
local a = (nil or 53) + 42
--! ok

--8<-- conjunctive-lhs-dynamic
--# assume p: WHATEVER
local a = (p and 'foo') + 54
--! ok

--8<-- conjunctive-rhs-dynamic
--# assume p: WHATEVER
local a = ('foo' and p) + 54
--! ok

--8<-- disjunctive-lhs-dynamic
--# assume p: WHATEVER
local a = (p or 'foo') + 54
--! ok

--8<-- disjunctive-rhs-dynamic
--# assume p: WHATEVER
local a = ('foo' or p) + 54
--! ok

--8<-- cat-string-or-number
--# assume p: string | number
local q = p .. 3
--! ok

--8<-- add-string-or-number
--# assume p: string | number
local q = p + 3 --@< Error: `(number|string)` is not a subtype of `number`
--! error

--8<-- cat-string-or-boolean
--# assume p: string | boolean
local q = p .. 3 --@< Error: `(boolean|string)` is not a subtype of `(number|string)`
--! error

--8<-- cat-string-lit-1
--# assume p: 'a'
--# assume q: 'b'
--# assume r: var 'ab'
r = p .. q
--! ok

--8<-- cat-string-lit-2
--# assume r: var 'ab'
r = 'a' .. 'b'
--! ok

--8<-- cat-string-lit-3
--# assume r: var 'ab'
local p = 'a' --: var string
r = p .. 'b' --@< Error: Cannot assign `string` into `var "ab"`
             --@^ Note: The other type originates here
--! error

--8<-- var-integer-literal
local x
--# assume x: var 3
x = 3
--! ok

--8<-- var-integer-literals-1
local x
--# assume x: var 3 | 4
x = 3
--! ok

--8<-- var-integer-literals-2
local x
--# assume x: var 4 | 5
x = 3 --@< Error: Cannot assign `3` into `var (4|5)`
      --@^ Note: The other type originates here
--! error

--8<-- add-sub-mul-mod-integer-integer
local x, y, z
--# assume x: var integer
--# assume y: var integer
--# assume z: var integer
z = x + y
z = x - y
z = x * y
z = x % y
--! ok

--8<-- div-integer-integer
local x, y, z
--# assume x: var integer
--# assume y: var integer
--# assume z: var integer
z = x / y --@< Error: Cannot assign `number` into `var integer`
          --@^ Note: The other type originates here
--! error

--8<-- add-integer-integer-is-integer
local p
--# assume p: var integer
p = 3 + 4
--! ok

--8<-- add-number-integer-is-not-integer
local p
--# assume p: var integer
p = 3.1 + 4 --@< Error: Cannot assign `number` into `var integer`
            --@^ Note: The other type originates here
--! error

--8<-- add-dynamic-integer
local p, q
--# assume p: WHATEVER
--# assume q: var integer
q = p + 3
--! ok

--8<-- add-dynamic-number-is-not-integer
local p, q
--# assume p: WHATEVER
--# assume q: var integer
q = p + 3.5 --@< Error: Cannot assign `number` into `var integer`
            --@^ Note: The other type originates here
--! error

--8<-- add-dynamic-number-is-number
local p, q
--# assume p: WHATEVER
--# assume q: var number
q = p + 3.5
--! ok

--8<-- add-dynamic-dynamic-1
local p, q
--# assume p: WHATEVER
--# assume q: var number
q = p + p
--! ok

--8<-- add-dynamic-dynamic-2
local p, q
--# assume p: WHATEVER
--# assume q: var integer
q = p + p --@< Error: Cannot assign `number` into `var integer`
          --@^ Note: The other type originates here
--! error

--8<-- assume-currently-integer
local a = true
a = 'string'
--# assume a: integer
a = a + 3.1
--! ok

--8<-- len-table
local a = 3 + #{1, 2, 3}
--! ok

--8<-- len-string
local a = 3 + #'heck'
--! ok

--8<-- len-integer
local a = 3 + #4 --@< Error: `4` is not a subtype of `(string|table)`
--! error

--8<-- for
--# assume a: var integer
for i = 1, 9 do a = i end
--! ok

--8<-- for-step
--# assume a: var integer
for i = 1, 9, 2 do a = i end
--! ok

--8<-- for-non-integer
--# assume a: var integer
for i = 1.1, 9 do
    a = i --@< Error: Cannot assign `number` into `var integer`
          --@^ Note: The other type originates here
end
--! error

--8<-- for-non-integer-step
--# assume a: var integer
for i = 1, 9, 2.1 do
    a = i --@< Error: Cannot assign `number` into `var integer`
          --@^ Note: The other type originates here
end
--! error

--8<-- func-varargs
function a(...)
    return ...
end
--! ok

--8<-- func-no-varargs
function a()
    return ... --@< Error: Variadic arguments do not exist in the innermost function
end
--! error

--8<-- func-nested-varargs
function a(...)
    return function()
        return ... --@< Error: Variadic arguments do not exist in the innermost function
    end
end
--! error

--8<-- index-rec-with-name-1
local a = { x = 3, y = 'foo' }
--# assume b: var integer
b = a.x
--! ok

--8<-- index-rec-with-name-2
local a = { x = 3, y = 'foo' }
--# assume b: var string
b = a.y
--! ok

--8<-- index-rec-with-wrong-name-1
local a = { x = 3, y = 'foo' }
local b = a.z + 1 -- z should be nil
--@^ Error: Cannot index `{x = 3, y = "foo"}` with `"z"`
--! error

--8<-- index-rec-with-wrong-name-2
local a = { x = 3, y = 'foo' }
local b = a.z .. 'bar' -- ditto
--@^ Error: Cannot index `{x = 3, y = "foo"}` with `"z"`
--! error

--8<-- table-update
local a = {}
a[1] = 1
a[2] = 2
a[3] = 3
--! ok

--8<-- func-number-arg-explicit
-- XXX can't propagate the number constraints upwards!
function p(a) --: number
    return a + 3
end
local x = p(4.5)
--! ok

--8<-- func-number-arg-implicit
function p(a) return a + 3 end
local x = p('what') --@< Error: `"what"` is not a subtype of `<unknown type>`
--! error

--8<-- capture-and-return-1
local a = 'string'
a = 42
function p() return a end
local b = p() + 54
--! ok

--8<-- capture-and-return-2
local a = 'string'
a = 42
function p() return a end
a = p() * 54
--! ok

-->8-- capture-and-weaken
-- XXX do not yet work
local a = 'string'
a = 42
function p() return a end
a = true -- a is now fixed to Var
--! error

--8<-- func-argtype-rettype-1
function p(x) --: string --> string
    return x
end
local a = p('foo') .. 'bar'
--! ok

--8<-- func-argtype-implicit-rettype
function p(x) --: string
    return x
end
local a = p('foo') .. 'bar'
--! ok

--8<-- func-argtype-rettype-2
function p(x) --: string --> string
    return x
end
local a = p('foo') + 3 --@< Error: `string` is not a subtype of `number`
--! error

--8<-- table-update-with-integer
local a = {} --: {}
a[1] = 42
a[2] = 54
--! ok

--8<-- var-table-update-with-integer
local a = {} --: var {} -- cannot be changed!
a[1] = 42
--@^ Error: Cannot adapt the table type `var {}` into `{<unknown type>,}`
--@^^ Note: The table had to be adapted in order to index it with `1`
a[2] = 54
--@^ Error: Cannot adapt the table type `var {}` into `{2 = <unknown type>}`
--@^^ Note: The table had to be adapted in order to index it with `2`
--! error

--8<-- table-update-with-name
local a = {} --: {}
a.what = 42
--! ok

--8<-- table-update-with-index-and-name
local a = {} --: {}
a[1] = 42
a.what = 54
--! ok

--8<-- array-update-with-index-and-name
local a = {} --: {number}
a[1] = 42
a.what = 54 -- {number} coerced to {[integer|string] = number} in the slot
--! ok

--8<-- var-array
local a = {} --: var {number}
--! ok

--8<-- var-array-update-with-integer-and-name
local a = {} --: var {number}
a[1] = 42
a.what = 54
--@^ Error: Cannot adapt the table type `var {var number}` into `{[integer] = var number}`
--@^^ Note: The table had to be adapted in order to index it with `"what"`
--! error

--8<-- var-map-update-and-index
local a = {} --: var {[number] = number}
a[1] = 42
a[3] = 54
a[1] = nil
local z = a[3] --: var number?
--! ok

--8<-- var-map-update-and-index-subtype
local a = {} --: var {[number] = number}
a[1] = 42
a[3] = 54
a[1] = nil
local z = a[3] --: var integer?
--@^ Error: Cannot assign `var (nil|number)` into `var (nil|integer)`
--@^^ Note: The other type originates here
--! error

--8<-- var-map-update-and-index-wrong-key
local a = {} --: var {[number] = number}
a[1] = 42
a.string = 54 --@< Error: Cannot index `var {[number] = var number}` with `"string"`
--! error

--8<-- var-map-update-and-index-without-nil
local a = {} --: var {[number] = number}
a[1] = 42
a[3] = 54
a[1] = nil
local z = a[3] --: var integer
--@^ Error: Cannot assign `var (nil|number)` into `var integer`
--@^^ Note: The other type originates here
--! error

--8<-- const-map-init
local a = {} --: const {[number] = number}
--! ok

--8<-- const-map-update
local a = {} --: const {[number] = number}
a[1] = 42
--@^ Error: Cannot adapt the table type `const {[number] = number}` into `{[number] = number}`
--@^^ Note: The table had to be adapted in order to index it with `1`
--! error

--8<-- var-any-update
local a --: var any
a = {}
a = 'hello'
a = 42
--! ok

--8<-- var-any-update-and-add
local a --: var any
a = 42
local b = a + 5 --@< Error: `any` is not a subtype of `number`
--! error

--8<-- var-typed-error-1
local a = 3 --: var number
a = 'foo'
--@^ Error: Cannot assign `"foo"` into `var number`
--@^^ Note: The other type originates here
a = 4
a = 'bar'
--@^ Error: Cannot assign `"bar"` into `var number`
--@^^ Note: The other type originates here
--! error

--8<-- var-typed-error-2
local a = 'foo' --: var number
--@^ Error: Cannot assign `"foo"` into `var number`
--@^^ Note: The other type originates here
a = 3
a = 'bar'
--@^ Error: Cannot assign `"bar"` into `var number`
--@^^ Note: The other type originates here
a = 4
--! error

--8<-- var-typed-error-lazy-1
local a --: var number
a = 3
a = 'foo'
--@^ Error: Cannot assign `"foo"` into `var number`
--@^^ Note: The other type originates here
a = 4
a = 'bar'
--@^ Error: Cannot assign `"bar"` into `var number`
--@^^ Note: The other type originates here
--! error

--8<-- var-typed-error-lazy-2
local a --: var number
a = 'foo'
--@^ Error: Cannot assign `"foo"` into `var number`
--@^^ Note: The other type originates here
a = 3
a = 'bar'
--@^ Error: Cannot assign `"bar"` into `var number`
--@^^ Note: The other type originates here
a = 4
--! error

--8<-- const-typed-error-1
local a = 3 --: const number
a = 'foo'
--@^ Error: Cannot assign `"foo"` into `const number`
--@^^ Note: The other type originates here
a = 4
--@^ Error: Cannot assign `4` into `const number`
--@^^ Note: The other type originates here
--! error

--8<-- const-typed-error-2
local a = 'foo' --: const number
--@^ Error: Cannot assign `"foo"` into `const number`
--@^^ Note: The other type originates here
a = 3
--@^ Error: Cannot assign `3` into `const number`
--@^^ Note: The other type originates here
a = 'bar'
--@^ Error: Cannot assign `"bar"` into `const number`
--@^^ Note: The other type originates here
--! error

--8<-- const-typed-error-lazy-1
local a --: const number
a = 3
a = 'foo'
--@^ Error: Cannot assign `"foo"` into `const number`
--@^^ Note: The other type originates here
a = 4
--@^ Error: Cannot assign `4` into `const number`
--@^^ Note: The other type originates here
--! error

--8<-- const-typed-error-lazy-2
local a --: const number
a = 'foo'
--@^ Error: Cannot assign `"foo"` into `const number`
--@^^ Note: The other type originates here
a = 3
--@^ Error: Cannot assign `3` into `const number`
--@^^ Note: The other type originates here
a = 'bar'
--@^ Error: Cannot assign `"bar"` into `const number`
--@^^ Note: The other type originates here
--! error

--8<-- local-init-does-not-copy-modf
local x = 42 --: const integer
local y = x
y = 54
--! ok

--8<-- func-returns-rec-1
--v () -> {a=integer}
local function p() return {a=4} end
local x = p().a + 5
--! ok

--8<-- func-returns-rec-2
--v () -> {a=integer}
local function p() return {a=4} end
local x = p().a.b --@< Error: Tried to index a non-table type `integer`
--! error

--8<-- func-returns-rec-2-span
--v () -> {a=integer}
local function p() return {a=4} end
local x = p().a
local y = x.b --@< Error: Tried to index a non-table type `integer`
--! error

--8<-- func-implicit-returns-rec
local function p() return {a=4} end
local x = p().a + 5
--! ok

--8<-- func-returns-seq
--v () -> (integer, integer)
local function p()
    return 3, 4
end
--! ok

--8<-- func-returns-seq-error
--v () -> (integer, string)
local function p()
    return 3, 4 --@< Error: `4` is not a subtype of `string`
end
--! error

--8<-- func-returns-seq-with-nil
--v () -> (integer, nil)
local function p()
    return 3
end
--! ok

--8<-- func-returns-with-nil
--v () -> integer
local function p()
    return 3, nil
end
--! ok

--8<-- func-returns-seq-union
--v (n: boolean) -> (string|nil, string|nil)
local function p(n)
    if n then return 'string' else return nil, 'error' end
end
--! ok

--8<-- assign-from-seq-1
local function p()
    return 3, 4, 5
end
local a, b = p() --: var integer, var integer
--! ok

--8<-- assign-from-seq-2
local function p()
    return 3, 4, 'string'
end
local a, b = p() --: var integer, var integer
--! ok

--8<-- assign-from-seq-3
local function p()
    return 3, 'string', 5
end
local a, b = p() --: var integer, var integer
--@^ Error: Cannot assign `"string"` into `var integer`
--@^^ Note: The other type originates here
--! error

--8<-- assign-from-seq-with-nil-1
local function p()
    return 3, 4
end
local a, b, c = p() --: var integer, var integer, var nil
--! ok

--8<-- assign-from-seq-with-nil-2
local function p()
    return 3, 4, nil
end
local a, b = p() --: var integer, var integer
--! ok

--8<-- assign-from-seq-union-1
local function p(n)
    if n then return 'string' else return nil, 'error' end
end
local a, b = p(false) --: var string|nil, var string|nil
--! ok

--8<-- assign-from-seq-union-2
local function p(n)
    if n then return 'string' else return nil, 'error' end
end
local a, b = p(false) --: var string, var string|nil
--@^ Error: Cannot assign `(nil|"string")` into `var string`
--@^^ Note: The other type originates here
--! error

--8<-- assign-from-seq-union-3
local function p(n)
    if n then return 'string' else return nil, 'error' end
end
local a, b = p(false) --: var string|nil, var string
--@^ Error: Cannot assign `(nil|"error")` into `var string`
--@^^ Note: The other type originates here
--! error

--8<-- table-from-seq
local function p()
    return 1, 2, 3
end
local a = {p(), p()} --: var {integer, integer, integer, integer}
local b = {foo = p()} --: var {foo = integer}
local c = {p(), bar = p()} --: var {[integer|string] = integer}
--! ok

--8<-- funccall-from-seq
local function p()
    return 1, 'string', false
end
--v (a: number, b: integer, c: number, d: integer, e: string, f: boolean)
local function q(a,b,c,d,e,f) end
q(3.14, p(), -42, p())
--! ok

--8<-- func-varargs-type-1
--v (...: integer)
local function p(...) end
p(1, 2, 3)
--! ok

--8<-- func-varargs-type-2
--v (...: integer)
local function p(...) end
p(1, false, 3) --@< Error: `false` is not a subtype of `(nil|integer)`
--! error

--8<-- func-varargs-type-with-nil
--v (...: integer)
local function p(...) end
p(1, 2, 3, nil, nil, nil)
--! ok

--8<-- type
--# type known_type = number
--# assume p: known_type
local q = p - 5
--! ok

--8<-- type-local
do
    --# type known_type = number
end
--# assume p: known_type --@< Error: Type `known_type` is not defined
--! error

--8<-- type-no-shadowing
--# type known_type = integer --@< Note: The type was originally defined here
do
    --# type known_type = number --@< Error: A type named `known_type` is already defined
end
--! error

--8<-- type-no-recursive
--# type known_type = known_type --@< Error: Type `known_type` is not defined
--! error

--8<-- type-transitive
--# type some_type = integer
--# type another_type = some_type
--# assume p: another_type
local q = p * 3
--! ok

--8<-- type-unknown-type
--# type some_type = another_type --@< Error: Type `another_type` is not defined
--! error

--8<-- type-transitive-out-of-order
--# type some_type = another_type --@< Error: Type `another_type` is not defined
--# type another_type = integer -- this is intentional
--! error

--8<-- lua51-no-implicit
print('hello') --@< Error: Global or local variable `print` is not defined
--! error

--8<-- lua51-print
--# open lua51
print('hello')
--! ok

--8<-- assert-truthy
--# open lua51
--# assume p: integer|nil
assert(p)
print(p + 5)
--! ok

--8<-- assert-disjunctive
--# open lua51
--# assume p: integer|nil
--# assume q: integer|nil
assert(p or q)
print(p + 5) --@< Error: `(nil|integer)` is not a subtype of `number`
--! error

--8<-- assert-conjunctive
--# open lua51
--# assume p: integer|nil
--# assume q: integer|nil
assert(p and q)
print(p + q)
--! ok

--8<-- assert-conjunctive-partial-1
--# open lua51
--# assume p: integer|nil
--# assume q: integer|nil
assert(p and not q)
print(p + 5)
--! ok

--8<-- assert-conjunctive-partial-2
--# open lua51
--# assume p: integer|nil
--# assume q: integer|nil
assert(p and not q)
print(q + 5) --@< Error: `nil` is not a subtype of `number`
--! error

--8<-- assert-conjunctive-partial-dynamic
--# open lua51
--# assume p: WHATEVER
--# assume q: WHATEVER
assert(p and not q)
print(p + 5)
print(q + 5) -- should not alter dynamic types
--! ok

--8<-- assert-number-type-1
--# open lua51
--# assume p: integer|string
assert(type(p) == 'number')
print(p + 5)
--! ok

--8<-- assert-number-type-2
--# open lua51
--# assume p: integer|string
assert('number' == type(p))
print(p + 5)
--! ok

--8<-- assert-integer-type
--# open lua51
--# assume p: integer|string
assert(type(p) == 'integer') -- no such type in Lua 5.1
--@^ Error: The literal cannot appear as a return type name for `type`
--! error

--8<-- assert-same-type
--# open lua51
assert(type(13) == type('string')) -- no-op
--! ok

--8<-- assert-not-1
--# open lua51
--# assume assert_not: const [assert_not] function(any)
--# assume p: integer|nil
--# assume q: integer|nil
assert_not(p or not q) -- i.e. assert(not p and q)
print(q + 5)
--! ok

--8<-- assert-not-2
--# open lua51
--# assume assert_not: const [assert_not] function(any)
--# assume p: integer|nil
--# assume q: integer|nil
assert_not(p or not q) -- i.e. assert(not p and q)
print(p + 5) --@< Error: `nil` is not a subtype of `number`
--! error

--8<-- assert-type
--# open lua51
--# assume assert_type: const [assert_type] function(any, string)
--# assume p: integer|string
assert_type(p, 'integer')
print(p + 5)
--! ok

--8<-- assign-identical
--# assume x: WHATEVER
x = x
--! ok

--8<-- call-identical
--# assume x: WHATEVER
x(x)
--! ok

--8<-- index-identical
--# assume x: WHATEVER
x[x]()
--! ok

--8<-- if-assign-identical
--# assume x: WHATEVER
if x then x = 42 end
--! ok

--8<-- while-assign-identical
--# assume x: WHATEVER
while x do x = 42 end
--! ok

--8<-- for-assign-identical
--# assume x: WHATEVER
for i = 1, x do x = 42 end
--! ok

--8<-- for-identical
--# assume x: WHATEVER
for i = x, x do end
--! ok

--8<-- table-identical
--# assume x: 'hello'
local p = {[x] = x}
--! ok

--8<-- assign-table-to-table
--# assume x: var table
--# assume y: var table
x = y
--! ok

--8<-- assign-function-to-function
--# assume x: var function
--# assume y: var function
x = y
--! ok

--8<-- assign-var-subtype-1
--# assume p: var number
--# assume q: var integer
p = q
--! ok

--8<-- assign-var-subtype-2
--# assume p: var integer
--# assume q: var 1|2
p = q
--! ok

--8<-- assign-var-suptype-1
--# assume p: var integer
--# assume q: var number
p = q
--@^ Error: Cannot assign `var number` into `var integer`
--@^^ Note: The other type originates here
--! error

--8<-- assign-var-suptype-2
--# assume p: var 1|2
--# assume q: var integer
p = q
--@^ Error: Cannot assign `var integer` into `var (1|2)`
--@^^ Note: The other type originates here
--! error

--8<-- assign-var-map-eqtype
--# assume p: var { [string] = var number }
--# assume q: var number
p.x = q
--! ok

--8<-- assign-var-map-subtype
--# assume p: var { [string] = var number }
--# assume q: var integer
p.x = q
--! ok

--8<-- assign-var-map-suptype
--# assume p: var { [string] = var integer }
--# assume q: var number
p.x = q
--@^ Error: Cannot assign `var number` into `var (nil|integer)`
--@^^ Note: The other type originates here
--! error

--8<-- require-unknown
--# open lua51
x = require 'a' --@< Warning: Cannot resolve the module name given to `require`
--! ok

--8<-- require-unknown-returns-1
--# open lua51
x = require 'a' --@< Warning: Cannot resolve the module name given to `require`
print(x + 4) --@< Error: `any` is not a subtype of `number`
--! error

--8<-- require-unknown-returns-2
--# open lua51
--# assume x: var integer
x = require 'A' --@< Warning: Cannot resolve the module name given to `require`
                --@^ Error: Cannot assign `any` into `var integer`
                --@^^ Note: The other type originates here

--& a
return 42

--! error

--8<-- require-returns-integer
--# open lua51
--# assume x: var integer
x = require 'a'

--& a
return 42

--! ok

--8<-- require-returns-false
--# open lua51
require 'a'
--@^ Error: Returning `false` from the module disables Lua's protection against recursive `require` calls and is heavily discouraged
-- XXX the span should be ideally at `return`

--& a
return false -- false triggers a Lua bug, so it is prohibited

--! error

--8<-- require-returns-whatever
--# open lua51
local a = require 'a'
a(a*a[a])

--& a
--# assume x: WHATEVER
return x

--! ok

--8<-- require-returns-string
--# open lua51
--# assume x: var string
x = require 'a'

--& a
local function p() return 'hello' end
return p()

--! ok

--8<-- require-returns-func
--# open lua51
print((require 'a')() + 5)

--& a
local function p() return 42 end
return p

--! ok

--8<-- require-returns-not-fully-resolved
--# open lua51
require 'a'
--@^ Error: The module has returned a type `(nil|<unknown type>)` that is not yet fully resolved
-- XXX the span should be ideally at `return`

--& a
local function p(...) return ... end
return p() -- this doesn't (yet) resolve fully (no generics)

--! error

--8<-- require-nested
--# open lua51
require 'a'

--& a
require 'b'

--& b
return true

--! ok

--8<-- require-nested-idempotent
--# open lua51
require 'a'
require 'b'
require 'a'

--& a
require 'b'

--& b
return true

--! ok

--8<-- require-recursive
--# open lua51
require 'a' --@< Note: The module was previously `require`d here

--& a
require 'b'

--& b
require 'a' --@< Error: Recursive `require` was requested

--! error

--8<-- require-disconnected
--# open lua51

--& a
require 'b'

--& b
require 'a'

-- check is dynamic
--! ok

--8<-- require-name-expr
--# open lua51
--# assume x: var number
x = require('a' .. 'b')

--& ab
return 42

--! ok

--8<-- index-assign-typed
local p = {x = 5, y = 6} --: var {x=number, y=number}
p.x = 'string' --@< Error: Cannot assign `"string"` into `var number`
               --@^ Note: The other type originates here
--! error

--8<-- index-assign-whatever
--# assume p: WHATEVER
p[1] = 42
--! ok

--8<-- for-in-simple-iter-1
--# assume func: const function() -> number?
for x in func do
    local a = x * 3
end
--! ok

--8<-- for-in-simple-iter-2
--# assume func: const function() -> number    -- technically infinite loop
for x in func do
    local a = x * 3
end
--! ok

--8<-- for-in-simple-iter-3
--# assume func: const function() -> number|string
for x in func do
    local a = x * 3 --@< Error: `(number|string)` is not a subtype of `number`
end
--! error

--8<-- for-in-stateful-iter-1
--# assume func: const function({const integer}, integer?) -> integer?
--# assume state: const {const integer}
--# assume first: const integer?
for x in func, state, first do
    local a = x * 3
end
--! ok

--8<-- for-in-stateful-iter-2
--# assume func: const function({const integer}, integer|string?) -> integer?
--# assume state: const {const integer}
--# assume first: const integer|string     -- the first value is silently discard
for x in func, state, first do
    local a = x * 3
end
--! ok

--8<-- for-in-multi-1
--# assume func: const function({const integer}, integer|string?) -> (integer?, string)
--# assume state: const {const integer}
--# assume first: const integer?
for x, y in func, state, first do
    local a = x * 3
    local b = y .. 'a'
end
--! ok

--8<-- for-in-multi-2
--# assume func: const function({const integer}, integer|string?) -> (integer?, string?)
--# assume state: const {const integer}
--# assume first: const integer?
for x, y in func, state, first do
    local a = x * 3
    local b = y .. 'a'    -- y can be nil
    --@^ Error: `(nil|string)` is not a subtype of `(number|string)`
end
--! error

--8<-- for-in-non-func
--@v Error: The iterator given to `for`-`in` statement returned a non-function `"hello"`
for x in 'hello' do
end
--! error

--8<-- for-in-non-func-recover
--@v Error: The iterator given to `for`-`in` statement returned a non-function `"hello"`
for x in 'hello' do
    x()
    y() --@< Error: Global or local variable `y` is not defined
end
--! error

--8<-- lua51-ipairs-integer-array
--# open lua51
--# assume p: var {var integer}
for x, y in ipairs(p) do
    local a = x * 3
    local b = y * 4
end
--! ok

--8<-- lua51-ipairs-string-array-1
--# open lua51
--# assume p: var {var string}
for x, y in ipairs(p) do
    local a = x * 3
    local b = y .. 'a'
end
--! ok

--8<-- lua51-ipairs-string-array-2
--# open lua51
--# assume p: var {var string}
for x, y in ipairs(p) do
    local b = y * 4 --@< Error: `string` is not a subtype of `number`
end
--! error

--8<-- lua51-ipairs-no-map
--# open lua51
--# assume p: var {[integer] = var string}
for x, y in ipairs(p) do
    --@^ Error: `{[integer] = var string}` is not a subtype of `{const WHATEVER}`
    -- XXX WHATEVER is temporary
end
--! error

--8<-- lua51-ipairs-no-table
--# open lua51
--# assume p: var table
for x, y in ipairs(p) do
    --@^ Error: `table` is not a subtype of `{const WHATEVER}`
    -- XXX WHATEVER is temporary
end
--! error

--8<-- lua51-ipairs-no-non-table
--# open lua51
--# assume p: var string
for x, y in ipairs(p) do
    --@^ Error: `string` is not a subtype of `{const WHATEVER}`
    -- XXX WHATEVER is temporary
end
--! error

--8<-- lua51-pairs-integer-array
--# open lua51
--# assume p: var {var integer}
for x, y in pairs(p) do
    local a = x * 3
    local b = y * 4
end
--! ok

--8<-- lua51-pairs-string-array-1
--# open lua51
--# assume p: var {var string}
for x, y in pairs(p) do
    local a = x * 3
    local b = y .. 'a'
end
--! ok

--8<-- lua51-pairs-string-array-2
--# open lua51
--# assume p: var {var string}
for x, y in pairs(p) do
    local b = y * 4 --@< Error: `string` is not a subtype of `number`
end
--! error

--8<-- lua51-pairs-integer-integer-map
--# open lua51
--# assume p: var {[integer] = var integer}
for x, y in pairs(p) do
    local a = x * 3
    local b = y * 4
end
--! ok

--8<-- lua51-pairs-integer-string-map-1
--# open lua51
--# assume p: var {[integer] = var string}
for x, y in pairs(p) do
    local a = x * 3
    local b = y .. 'a'
end
--! ok

--8<-- lua51-pairs-integer-string-map-2
--# open lua51
--# assume p: var {[integer] = var string}
for x, y in pairs(p) do
    local b = y * 4 --@< Error: `string` is not a subtype of `number`
end
--! error

--8<-- lua51-pairs-string-integer-map-1
--# open lua51
--# assume p: var {[string] = var integer}
for x, y in pairs(p) do
    local a = x .. 'a'
    local b = y * 4
end
--! ok

--8<-- lua51-pairs-string-integer-map-2
--# open lua51
--# assume p: var {[string] = var integer}
for x, y in pairs(p) do
    local a = x * 3 --@< Error: `string` is not a subtype of `number`
end
--! error

--8<-- lua51-pairs-table
--# open lua51
--# assume p: var table
for x, y in pairs(p) do
end
--! ok

--8<-- lua51-pairs-table-any
--# open lua51
--# assume p: var table
for x, y in pairs(p) do
    print(p .. 3) --@< Error: `table` is not a subtype of `(number|string)`
end
--! error

--8<-- lua51-pairs-no-non-table
--# open lua51
--# assume p: var string
for x, y in pairs(p) do --@< Error: `string` is not a subtype of `table`
end
--! error

--8<-- lua51-update-package-cpath
--# open lua51
package.cpath = '?.lua'
--! ok

--8<-- redefine-global
p = 42 --: integer
p = 54 --: integer --@< Error: Cannot redefine the type of a global variable `p`
p = 63 --: integer --@< Error: Cannot redefine the type of a global variable `p`
--! error

--8<-- redefine-global-in-single-stmt-1
p, p = 42, 54 --: integer, integer
--@^ Error: Cannot redefine the type of a global variable `p`
--! error

--8<-- redefine-global-in-single-stmt-2
p, p, p = 42, 54, 63 --: integer, integer, integer
--@^ Error: Cannot redefine the type of a global variable `p`
--@^^ Error: Cannot redefine the type of a global variable `p`
--! error

--8<-- redefine-local
local p = 42 --: integer
local p = 54 --: integer
local p = 'string' --: string
--! ok

--8<-- unknown-builtin
--# --@v Warning: `sudo` is an unknown built-in name and ignored
--# assume sudo: [sudo] {
--#     --@v Warning: `make_sandwich` is an unknown built-in name and ignored
--#     make_sandwich = const [make_sandwich] function(boolean)
--# }
--! ok

--8<-- builtin-with-subtyping-1
--# assume x: var [`internal subtype`] number
--# assume y: var number
x = y
--@^ Error: Cannot assign `var number` into `var [internal subtype] number`
--@^^ Note: The other type originates here
--! error

--8<-- builtin-with-subtyping-2
--# assume x: var [`internal subtype`] number
--# assume y: var number
y = x
--! ok

--8<-- builtin-without-subtyping-1
--# assume x: var [`internal no_subtype`] number
--# assume y: var number
x = y
--! ok

--8<-- builtin-without-subtyping
--# assume x: var [`internal no_subtype`] number
--# assume y: var number
y = x
--! ok

--8<-- lua51-package-path-literal
--# open lua51
package.path = '?.lua'
package.cpath = '?.so'
--! ok

--8<-- lua51-package-path-non-literal
--# open lua51
--# assume x: string
package.path = x
--@^ Warning: Cannot infer the values assigned to the `package_path` built-in variable; subsequent `require` may be unable to find the module path
package.cpath = x
--@^ Warning: Cannot infer the values assigned to the `package_cpath` built-in variable; subsequent `require` may be unable to find the module path
--! ok

--8<-- package-path-non-literal-local-1
-- while this is very contrived, this and subsequent tests check the ability to
-- declare *and* assign special variables altogether.
--# assume x: string
local p = x --: var [package_path] string
--@^ Warning: Cannot infer the values assigned to the `package_path` built-in variable; subsequent `require` may be unable to find the module path
--! ok

--8<-- package-path-non-literal-local-2
--# assume x: string
local p = '' --: var [package_path] string
local q = 0 --: var integer
p, q = x, 42
--@^ Warning: Cannot infer the values assigned to the `package_path` built-in variable; subsequent `require` may be unable to find the module path
--! ok

--8<-- package-path-non-literal-global-1
--# assume x: string
p = x --: var [package_path] string
--@^ Warning: Cannot infer the values assigned to the `package_path` built-in variable; subsequent `require` may be unable to find the module path
--! ok

--8<-- package-path-non-literal-global-2
--# assume x: string
p, q = x, x --: var [package_path] string, var [package_cpath] string
--@^ Warning: Cannot infer the values assigned to the `package_path` built-in variable; subsequent `require` may be unable to find the module path
--@^^ Warning: Cannot infer the values assigned to the `package_cpath` built-in variable; subsequent `require` may be unable to find the module path
--! ok

--8<-- if-false-warning-1
--@v-vvvvv Warning: These `if` case(s) are never executed
if false then --@< Note: This condition always evaluates to a falsey value
    local a
    local b
    local c
end
--! ok

--8<-- if-false-warning-2
--@v-vv Warning: These `if` case(s) are never executed
if false then --@< Note: This condition always evaluates to a falsey value
    local a
else
    local b
    local c
end
--! ok

--8<-- if-false-warning-3
--# assume x: boolean
if x then
    local a
--@v-vv Warning: These `if` case(s) are never executed
elseif false then --@< Note: This condition always evaluates to a falsey value
    local b
--@v-vv Warning: These `if` case(s) are never executed
elseif false then --@< Note: This condition always evaluates to a falsey value
    local c
else
    local d
end
--! ok

--8<-- if-true-warning-1
if true then --@< Warning: This condition always evaluates to a truthy value
    local a
    local b
    local c
end
--! ok

--8<-- if-true-warning-2
--# assume x: boolean
if x then
    local a
elseif true then --@< Warning: This condition always evaluates to a truthy value
    local b
end
--! ok

--8<-- if-true-warning-3
--# assume x: boolean
if true then --@< Note: This condition always evaluates to a truthy value
    local a
elseif x then --@<-vvvv Warning: These `if` case(s) are never executed
    local b
else
    local c
end
--! ok

--8<-- if-true-warning-4
--# assume x: boolean
if x then
    local a
elseif true then --@< Note: This condition always evaluates to a truthy value
    local b
elseif true then --@<-vvvvvv Warning: These `if` case(s) are never executed
    local c
elseif false then
    local d
else
    local e
end
--! ok

--8<-- unassigned-local-var-1
local p --: var string
--! ok

--8<-- unassigned-local-var-2
local p --: var string
local function f(x) end
f(p)
--@^ Error: The variable is not yet initialized
--@1 Note: The variable was not implicitly initialized to `nil` as its type is `var string`
f(p)
--@^ Error: The variable is not yet initialized
--@1 Note: The variable was not implicitly initialized to `nil` as its type is `var string`
p = 'string'
f(p) -- no longer an error
--! error

--8<-- unassigned-local-var-nil-1
local p --: var string?
--! ok

--8<-- unassigned-local-var-nil-2
local p --: var string?
local function f(x) end
f(p)
p = 'string'
f(p)
p = nil
f(p)
--! ok

--8<-- unassigned-global-var
q, p = 42 --: var integer, var string
--@^ Error: Cannot assign `nil` into `var string`
--! error

--8<-- table-update-nested-1
local a = {}
a.b = {}
a.b.c = {}
a.b.c.d = {}
--! ok

--8<-- table-update-nested-2
local a = { b = {} }
a.b.c = {}
a.b.c.d = {}
--! ok

--8<-- table-update-nested-3
local a = { b = { c = {} } }
a.b.c.d = {}
--! ok

--8<-- method-decl
local p = {}
function p.a() end
p.a()
--! ok

--8<-- method-decl-nested
local p = {a = {}}
function p.a.b() end
p.a.b()
--! ok

--8<-- method-decl-self
local p = {}
function p:a() end
p:a()
--! ok

--8<-- method-decl-self-nested
local p = {a = {}}
function p.a:b() end
p.a:b()
--! ok

--8<-- method-decl-nontable
local p = 42
function p.a() end --@< Error: Tried to index a non-table type `42`
p.a()              --@< Error: Tried to index a non-table type `42`
--! error

--8<-- method-decl-nontable-nested
local p = {a = 42}
function p.a.b() end --@< Error: Tried to index a non-table type `42`
p.a.b()              --@< Error: Tried to index a non-table type `42`
--! error

--8<-- method-decl-const
local p = {} --: const {}
function p.a() end
--@^ Error: Cannot adapt the table type `const {}` into `{a = <unknown type>}`
--@^^ Note: The table had to be adapted in order to index it with `"a"`
p.a() --@< Error: Cannot index `const {}` with `"a"`
--! error

--8<-- method-decl-const-nested
local p = { a = {} } --: const {a = const {}}
function p.a.b() end
--@^ Error: Cannot adapt the table type `const {}` into `{b = <unknown type>}`
--@^^ Note: The table had to be adapted in order to index it with `"b"`
p.a.b() --@< Error: Cannot index `const {}` with `"b"`
--! error

