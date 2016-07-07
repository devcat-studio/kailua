-- Basic tests for the Kailua type checker.

--8<-- funccall-nil
local p
p()
--! error

--8<-- funccall-func
local function p() end
p()
--! ok

--8<-- funccall-var-outside-of-scope-1
local c
if c then local p end
p() --@< Error: Global or local variable `p` is not defined
--! error

--8<-- funccall-var-outside-of-scope-2
local c = true
if c then local p end
p() --@< Error: Global or local variable `p` is not defined
--! error

--8<-- funccall-var-outside-of-scope-3
local c = false
if c then local p end
p() --@< Error: Global or local variable `p` is not defined
--! error

--8<-- funccall-var-outside-of-scope-4
local c --: boolean
if c then local p end
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

--8<-- unknown-type
--# assume p: unknown_type
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
local p = ({})[3]
--! error

--8<-- index-map-with-integer
local p = ({[3] = 4})[3]
--! ok

--8<-- index-map-with-integer-no-key
local p = ({[2] = 4})[3]
--! error

--8<-- index-empty-with-name
local p = ({}).a
--! error

--8<-- index-rec-with-name
local p = ({a = 4}).a
--! ok

--8<-- index-rec-with-name-no-key
local p = ({a = 4}).b
--! error

--8<-- methodcall-empty
local p = ({}):hello() -- XXX
--! ok

--8<-- methodcall-func
local p = (function() end)[3] -- XXX
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
local p = f.index
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
x = 3
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
z = x / y
--! error

--8<-- add-integer-integer-is-integer
local p
--# assume p: var integer
p = 3 + 4
--! ok

--8<-- add-number-integer-is-not-integer
local p
--# assume p: var integer
p = 3.1 + 4
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
q = p + 3.5
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
q = p + p
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
for i = 1.1, 9 do a = i end
--! error

--8<-- for-non-integer-step
--# assume a: var integer
for i = 1, 9, 2.1 do a = i end
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
--! error

--8<-- index-rec-with-wrong-name-2
local a = { x = 3, y = 'foo' }
local b = a.z .. 'bar' -- ditto
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
local x = p('what')
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
local a --: {}
      = {} -- XXX parser bug
a[1] = 42
a[2] = 54
--! ok

--8<-- var-table-update-with-integer
local a --: var {} -- cannot be changed!
      = {} -- XXX parser bug
a[1] = 42
a[2] = 54
--! error

--8<-- table-update-with-name
local a --: {}
      = {} -- XXX parser bug
a.what = 42
--! ok

--8<-- table-update-with-index-and-name
local a --: {}
      = {} -- XXX parser bug
a[1] = 42
a.what = 54
--! ok

--8<-- array-update-with-index-and-name
local a --: {number}
      = {} -- XXX parser bug
a[1] = 42
a.what = 54 -- {number} coerced to {[integer|string] = number} in the slot
--! ok

--8<-- var-array
local a --: var {number}
      = {} -- XXX parser bug
--! ok

--8<-- var-array-update-with-integer-and-name
local a --: var {number}
      = {} -- XXX parser bug
a[1] = 42
a.what = 54
--! error

--8<-- var-map-update-and-index
local a --: var {[number] = number}
      = {} -- XXX parser bug
a[1] = 42
a[3] = 54
a[1] = nil
local z --: var number?
      = a[3] -- XXX parser bug
--! ok

--8<-- var-map-update-and-index-subtype
local a --: var {[number] = number}
      = {} -- XXX parser bug
a[1] = 42
a[3] = 54
a[1] = nil
local z --: var integer?
      = a[3] -- XXX parser bug
--! error

--8<-- var-map-update-and-index-without-nil
local a --: var {[number] = number}
      = {} -- XXX parser bug
a[1] = 42
a[3] = 54
a[1] = nil
local z --: var integer
      = a[3] -- XXX parser bug
--! error

--8<-- const-map-update
local a --: const {[number] = number}
      = {} -- XXX parser bug
a[1] = 42
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

--8<-- func-returns-rec-1
--v () -> {a=integer}
local function p() return {a=4} end
local x = p().a + 5
--! ok

--8<-- func-returns-rec-2
--v () -> {a=integer}
local function p() return {a=4} end
local x = p().a.b
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
    return 3, 4
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
local a, --: var integer
      b  --: var integer
      = p()
--! ok

--8<-- assign-from-seq-2
local function p()
    return 3, 4, 'string'
end
local a, --: var integer
      b  --: var integer
      = p()
--! ok

--8<-- assign-from-seq-3
local function p()
    return 3, 'string', 5
end
local a, --: var integer
      b  --: var integer
      = p()
--! error

--8<-- assign-from-seq-with-nil-1
local function p()
    return 3, 4
end
local a, --: var integer
      b, --: var integer
      c  --: var nil
      = p()
--! ok

--8<-- assign-from-seq-with-nil-2
local function p()
    return 3, 4, nil
end
local a, --: var integer
      b  --: var integer
      = p()
--! ok

--8<-- assign-from-seq-union-1
local function p(n)
    if n then return 'string' else return nil, 'error' end
end
local a, --: var string|nil
      b  --: var string|nil
      = p(false)
--! ok

--8<-- assign-from-seq-union-2
local function p(n)
    if n then return 'string' else return nil, 'error' end
end
local a, --: var string
      b  --: var string|nil
      = p(false)
--! error

--8<-- assign-from-seq-union-3
local function p(n)
    if n then return 'string' else return nil, 'error' end
end
local a, --: var string|nil
      b  --: var string
      = p(false)
--! error

--8<-- table-from-seq
local function p()
    return 1, 2, 3
end
local a --: var {integer, integer, integer, integer}
      = {p(), p()}
local b --: var {foo = integer}
      = {foo = p()}
local c --: var {[integer|string] = integer}
      = {p(), bar = p()}
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
p(1, false, 3)
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
--# assume p: known_type
--! error

--8<-- type-no-shadowing
--# type known_type = integer
do
    --# type known_type = number
end
--! error

--8<-- type-no-recursive
--# type known_type = known_type
--! error

--8<-- type-transitive
--# type some_type = integer
--# type another_type = some_type
--# assume p: another_type
local q = p * 3
--! ok

--8<-- type-unknown-type
--# type some_type = another_type
--! error

--8<-- type-transitive-out-of-order
--# type some_type = another_type
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
--! error

--8<-- assert-same-type
--# open lua51
assert(type(13) == type('string')) -- no-op
--! ok

--8<-- assert-not-1
--# open lua51
--# assume assert_not: const function(any) = 'assert-not'
--# assume p: integer|nil
--# assume q: integer|nil
assert_not(p or not q) -- i.e. assert(not p and q)
print(q + 5)
--! ok

--8<-- assert-not-2
--# open lua51
--# assume assert_not: const function(any) = 'assert-not'
--# assume p: integer|nil
--# assume q: integer|nil
assert_not(p or not q) -- i.e. assert(not p and q)
print(p + 5) --@< Error: `nil` is not a subtype of `number`
--! error

--8<-- assert-type
--# open lua51
--# assume assert_type: const function(any, string) = 'assert-type'
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

--8<-- require-unknown
--# open lua51
x = require 'a' --@< Warning: Cannot resolve the module name given to `require`
--! ok

--8<-- require-unknown-returns-1
--# open lua51
x = require 'a' --@< Warning: Cannot resolve the module name given to `require`
print(x + 4) --@< Error: `<unknown type>` is not a subtype of `integer`
--! error

--8<-- require-unknown-returns-2
--# open lua51
--# assume x: var integer
x = require 'A' --@< Warning: Cannot resolve the module name given to `require`

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

--& a
return false -- false triggers a Lua bug, so it is prohibited

--! error

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
require 'a'

--& a
require 'b'

--& b
require 'a'

--! error

--8<-- require-disconnected
--# open lua51

--& a
require 'b'

--& b
require 'a'

-- check is dynamic
--! ok

--8<-- index-assign-typed
local p --: var {x=number, y=number}
      = {x = 5, y = 6}
p.x = 'string'
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
for x in 'hello' do --@< Error: The iterator returned a non-function "hello"
end
--! error

--8<-- for-in-non-func-recover
for x in 'hello' do --@< Error: The iterator returned a non-function "hello"
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
end
--! error

--8<-- lua51-ipairs-no-table
--# open lua51
--# assume p: var table
for x, y in ipairs(p) do
end
--! error

--8<-- lua51-ipairs-no-non-table
--# open lua51
--# assume p: var string
for x, y in ipairs(p) do
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

--8<-- lua51-ipairs-table
--# open lua51
--# assume p: var table
for x, y in pairs(p) do
end
--! ok

--8<-- lua51-ipairs-table-any
--# open lua51
--# assume p: var table
for x, y in pairs(p) do
    print(p .. 3) --@< Error: `table` is not a subtype of `(number|string)`
end
--! error

--8<-- lua51-ipairs-no-non-table
--# open lua51
--# assume p: var string
for x, y in pairs(p) do
end
--! error

