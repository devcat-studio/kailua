-- Tests specific to Lua 5.1 library support in the Kailua type checker.

--8<-- lua51-print
--# open lua51
print('hello')
--! ok

--8<-- lua51-duplicate-open
--# open lua51
--# open lua51
print('hello')
--! ok

--8<-- lua51-no-implicit
print('hello') --@< Error: Global or local variable `print` is not defined
--! error

--8<-- lua51-assert-truthy
--# open lua51
--# assume p: integer?
assert(p)
print(p + 5)
--! ok

--8<-- lua51-assert-disjunctive
--# open lua51
--# assume p: integer?
--# assume q: integer?
assert(p or q)
print(p + 5) --@< Error: Cannot apply + operator to `integer?` and `5`
             --@^ Cause: `integer?` is not a subtype of `number`
--! error

--8<-- lua51-assert-conjunctive
--# open lua51
--# assume p: integer?
--# assume q: integer?
assert(p and q)
print(p + q)
--! ok

--8<-- lua51-assert-conjunctive-partial-1
--# open lua51
--# assume p: integer?
--# assume q: integer?
assert(p and not q)
print(p + 5)
--! ok

--8<-- lua51-assert-conjunctive-partial-2
--# open lua51
--# assume p: integer?
--# assume q: integer?
assert(p and not q)
print(q + 5) --@< Error: Cannot apply + operator to `nil` and `5`
             --@^ Cause: `nil` is not a subtype of `number`
--! error

--8<-- lua51-assert-conjunctive-partial-dynamic
--# open lua51
--# assume p: WHATEVER
--# assume q: WHATEVER
assert(p and not q)
print(p + 5)
print(q + 5) -- should not alter dynamic types
--! ok

--8<-- lua51-assert-number-type-1
--# open lua51
--# assume p: integer|string
assert(type(p) == 'number')
print(p + 5)
--! ok

--8<-- lua51-assert-number-type-2
--# open lua51
--# assume p: integer|string
assert('number' == type(p))
print(p + 5)
--! ok

--8<-- lua51-assert-integer-type
--# open lua51
--# assume p: integer|string
assert(type(p) == 'integer') -- no such type in Lua 5.1
--@^ Error: The literal cannot appear as a return type name for `type`
--! error

--8<-- lua51-assert-same-type
--# open lua51
assert(type(13) == type('string')) -- no-op
--! ok

--8<-- lua51-assert-not-1
--# open lua51
--# assume assert_not: const [assert_not] function(any)
--# assume p: integer?
--# assume q: integer?
assert_not(p or not q) -- i.e. assert(not p and q)
print(q + 5)
--! ok

--8<-- lua51-assert-not-2
--# open lua51
--# assume assert_not: const [assert_not] function(any)
--# assume p: integer?
--# assume q: integer?
assert_not(p or not q) -- i.e. assert(not p and q)
print(p + 5) --@< Error: Cannot apply + operator to `nil` and `5`
             --@^ Cause: `nil` is not a subtype of `number`
--! error

--8<-- lua51-assert-type
--# open lua51
--# assume assert_type: const [assert_type] function(any, string)
--# assume p: integer|string
assert_type(p, 'integer')
print(p + 5)
--! ok

--8<-- lua51-ipairs-integer-array
--# open lua51
--# assume p: vector<integer>
for x, y in ipairs(p) do
    local a = x * 3
    local b = y * 4
end
--! ok

--8<-- lua51-ipairs-string-array-1
--# open lua51
--# assume p: vector<string>
for x, y in ipairs(p) do
    local a = x * 3
    local b = y .. 'a'
end
--! ok

--8<-- lua51-ipairs-string-array-2
--# open lua51
--# assume p: vector<string>
for x, y in ipairs(p) do
    local b = y * 4 --@< Error: Cannot apply * operator to `string` and `4`
                    --@^ Cause: `string` is not a subtype of `number`
end
--! error

--8<-- lua51-ipairs-no-map
--# open lua51
--# assume p: map<integer, string>
for x, y in ipairs(p) do
    --@^ Error: The type `[generic_pairs] function(vector<const WHATEVER>) --> (function(vector<const WHATEVER>, integer) --> (integer?, any), vector<const WHATEVER>, integer)` cannot be called
    --@^^ Cause: First function argument `map<integer, string>` is not a subtype of `vector<const WHATEVER>`
    -- XXX WHATEVER is temporary
end
--! error

--8<-- lua51-ipairs-no-table
--# open lua51
--# assume p: table
for x, y in ipairs(p) do
    --@^ Error: The type `[generic_pairs] function(vector<const WHATEVER>) --> (function(vector<const WHATEVER>, integer) --> (integer?, any), vector<const WHATEVER>, integer)` cannot be called
    --@^^ Cause: First function argument `table` is not a subtype of `vector<const WHATEVER>`
    -- XXX WHATEVER is temporary
end
--! error

--8<-- lua51-ipairs-no-non-table
--# open lua51
--# assume p: string
for x, y in ipairs(p) do
    --@^ Error: The type `[generic_pairs] function(vector<const WHATEVER>) --> (function(vector<const WHATEVER>, integer) --> (integer?, any), vector<const WHATEVER>, integer)` cannot be called
    --@^^ Cause: First function argument `string` is not a subtype of `vector<const WHATEVER>`
    -- XXX WHATEVER is temporary
end
--! error

--8<-- lua51-pairs-integer-array
--# open lua51
--# assume p: vector<integer>
for x, y in pairs(p) do
    local a = x * 3
    local b = y * 4
end
--! ok

--8<-- lua51-pairs-string-array-1
--# open lua51
--# assume p: vector<string>
for x, y in pairs(p) do
    local a = x * 3
    local b = y .. 'a'
end
--! ok

--8<-- lua51-pairs-string-array-2
--# open lua51
--# assume p: vector<string>
for x, y in pairs(p) do
    local b = y * 4 --@< Error: Cannot apply * operator to `string` and `4`
                    --@^ Cause: `string` is not a subtype of `number`
end
--! error

--8<-- lua51-pairs-integer-integer-map
--# open lua51
--# assume p: map<integer, integer>
for x, y in pairs(p) do
    local a = x * 3
    local b = y * 4
end
--! ok

--8<-- lua51-pairs-integer-string-map-1
--# open lua51
--# assume p: map<integer, string>
for x, y in pairs(p) do
    local a = x * 3
    local b = y .. 'a'
end
--! ok

--8<-- lua51-pairs-integer-string-map-2
--# open lua51
--# assume p: map<integer, string>
for x, y in pairs(p) do
    local b = y * 4 --@< Error: Cannot apply * operator to `string` and `4`
                    --@^ Cause: `string` is not a subtype of `number`
end
--! error

--8<-- lua51-pairs-string-integer-map-1
--# open lua51
--# assume p: map<string, integer>
for x, y in pairs(p) do
    local a = x .. 'a'
    local b = y * 4
end
--! ok

--8<-- lua51-pairs-string-integer-map-2
--# open lua51
--# assume p: map<string, integer>
for x, y in pairs(p) do
    local a = x * 3 --@< Error: Cannot apply * operator to `string` and `3`
                    --@^ Cause: `string` is not a subtype of `number`
end
--! error

--8<-- lua51-pairs-table
--# open lua51
--# assume p: table
for x, y in pairs(p) do
end
--! ok

--8<-- lua51-pairs-table-any
--# open lua51
--# assume p: table
for x, y in pairs(p) do
    print(p .. 3) --@< Error: Cannot apply .. operator to `table` and `3`
                  --@^ Cause: `table` is not a subtype of `(number|string)`
end
--! error

--8<-- lua51-pairs-no-non-table
--# open lua51
--# assume p: string
for x, y in pairs(p) do
    --@^ Error: The type `[generic_pairs] function(table) --> (function(table, any) --> (any?, any), table, any)` cannot be called
    --@^^ Cause: First function argument `string` is not a subtype of `table`
end
--! error

--8<-- lua51-update-package-cpath
--# open lua51
package.cpath = '?.lua'
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

--8<-- lua51-string-meta
--# open lua51
local x = ('f'):byte() --: integer
local x, y, z = ('XY'):byte() --: integer, integer, integer
local x, y, z = ('XY'):byte(10, 15) --: integer, integer, integer
local x = ('foo'):find('o') --: integer
local x = ('%s%s'):format('x', 'y') --: string
local x = ('xyzzy'):len() --: integer
local x = ('xyZzy'):lower() --: string
local x = ('xyZzy'):upper() --: string
local x = ('*'):rep(80) --: string
local x = ('abracadabra'):reverse() --: string
local x = ('notice'):sub(4) --: string
local x = ('notice'):sub(1, 3) --: string
local x = ('notice'):sub(10, 10) --: string
--! ok

-->8-- lua51-assert-string-type-and-meta
--# open lua51
local function f(s)
    assert(type(s) == 'string')
    return s:lower()
end

f('string')
-- in theory f(42) and others may work, since assertion is a runtime type check
-- and the compile time type checker may remain sound without any further check.
-- in practice the current constraint solver is a bit quirky and will reject them.
-- both are not what we want in the long term, clearly. TODO

--! ok

-->8-- lua51-lt-operand-inferred-from-funcarg
--# open lua51
local function negative(x)
    assert(type(x) == 'number') -- won't destroy the type varible
    return x < 0
end
--! ok

--8<-- lua51-string-add-method
--# open lua51
--v function(self: string) --> string
function string:trim()
    return self:gsub('^%s+', ''):gsub('%s+$', '')
end
--! ok

--8<-- lua51-table-insert-1
--# open lua51
local x = {} --: vector<integer>
table.insert(x, 42)
table.insert(x, 54)
--! ok

-->8-- lua51-table-insert-2
--# open lua51
local x = {} --: vector<integer>
table.insert(x, 42)
table.insert(x, 'not an integer') -- should error, but current definition is rather weak
--! error

--8<-- lua51-table-maxn
--# open lua51
local x = {1, 2, 3} --: vector<integer>
local y = {'foo', 'bar'} --: vector<string>
local n = table.maxn(x) --: integer
local m = table.maxn(y) --: integer
--! ok

