-- Tests for displaying Kailua types.

--8<-- display-named
--# type X = string
local x = 42 --: X
--@^ Error: Cannot assign `42` into `X`
--@^^ Note: The other type originates here
--! error

--8<-- display-named-nil-1
--# type X = string
local x = 42 --: X?
--@^ Error: Cannot assign `42` into `X?`
--@^^ Note: The other type originates here
--! error

--8<-- display-named-nil-2
--# type X = string?
local x = 42 --: X?
--@^ Error: Cannot assign `42` into `X`
--@^^ Note: The other type originates here
--! error

--8<-- display-named-no-nil-1
--# type X = string
local x = 42 --: X!
--@^ Error: Cannot assign `42` into `X!`
--@^^ Note: The other type originates here
--! error

--8<-- display-named-no-nil-2
--# type X = string!
local x = 42 --: X!
--@^ Error: Cannot assign `42` into `X`
--@^^ Note: The other type originates here
--! error

--8<-- display-named-no-nil-3
--# type X = string?
local x = 42 --: X!
--@^ Error: Cannot assign `42` into `X!`
--@^^ Note: The other type originates here
--! error

--8<-- display-union-maximally-disjoint
local a = true --: true | 42 | 'foobar' | thread | userdata | (function()) | {string}
local b = a --: nil
--@^ Error: Cannot assign `(true|thread|userdata|42|"foobar"|{string}|function() --> ())` into `nil`
--@^^ Note: The other type originates here
--! error

--8<-- display-union-nil
--# type X = string?
--# type Y = integer
local x = 42.5 --: X | Y
--@^ Error: Cannot assign `number` into `(string|Y)?`
--@^^ Note: The other type originates here
-- XXX this is not entirely intentional but stems from the current implementation strategy:
-- ? or ! are separate from unions, so unions cannot have hints including them.
-- in the future unions can be made more flexible and have any kind of hints.
--! error

--8<-- display-union-same
--# type X = string
local x = 42 --: X | X | X
--@^ Error: Cannot assign `42` into `X`
--@^^ Note: The other type originates here
--! error

--8<-- display-union-duplicates
--# type X = string
--# type Y = string
local x = 42 --: X | Y
--@^ Error: Cannot assign `42` into `string`
--@^^ Note: The other type originates here
--! error

--8<-- display-union-disjoint-1
--# type X = 1|2|3
--# type Y = 4|5
local x = 42 --: X | Y
--@^ Error: Cannot assign `42` into `(X|Y)`
--@^^ Note: The other type originates here
--! error

--8<-- display-union-disjoint-2
--# type X = integer|string
--# type Y = string|boolean
local x = 42.5 --: X | Y
--@^ Error: Cannot assign `number` into `(X|Y)`
--@^^ Note: The other type originates here
--! error

--8<-- display-union-disjoint-unnamed
--# type X = string|boolean
local x = 42.5 --: integer | X
--@^ Error: Cannot assign `number` into `(integer|X)`
--@^^ Note: The other type originates here
--! error

--8<-- display-union-overlap
--# type X = 1|2|3
--# type Y = 3|4|5
local x = 42 --: X | Y
--@^ Error: Cannot assign `42` into `(X|Y)`
--@^^ Note: The other type originates here
--! error

--8<-- display-union-includes-named-1
--# type X = 1|2|3
--# type Y = 1|2
local x = 42 --: X | Y
--@^ Error: Cannot assign `42` into `(X|Y)`
--@^^ Note: The other type originates here
--! error

--8<-- display-union-includes-named-2
--# type X = 1|2|3
--# type Y = integer
local x = 42.5 --: X | Y
--@^ Error: Cannot assign `number` into `Y`
--@^^ Note: The other type originates here
--! error

--8<-- display-union-includes-unnamed
--# type X = 1|2|3
local x = 42 --: 3 | X
--@^ Error: Cannot assign `42` into `X`
--@^^ Note: The other type originates here
--! error

--8<-- display-rec-recursive-1
local x = {}
x.x = x
x.y = x
x.z = x
local a = x --: integer
--@^ Error: Cannot assign `{x: <variable x>, y: <variable x>, z: <variable x>, ...}` into `integer`
--@^^ Note: The other type originates here
--! error

--8<-- display-rec-recursive-2
local x = {}
x.x = x
x.y = x
x.z = x
local y = {}
y.x = y
y.y = x
-- this error is actually quite wrong, because it occurred in midst of recursive rvar relation...
x = y
--@^ Error: Cannot assign `{x: <variable y>, y: <variable x>, ...}` into `{x: <variable y>, y: <variable x>, z: <variable x>, ...}`
--@^^ Note: The other type originates here
--! error

--8<-- display-rec-recursive-3
local u = {}
u.u = u
u.u = 0
--@^ Error: Cannot assign `0` into `{u: <...>, ...}`
--@^^ Note: The other type originates here
--! error

--8<-- display-same-names
local x = {}
do
    --# type local X = integer
    x.a = 42 --: X
end
do
    --# type local X = string
    --# type local Y = vector<X>
    x.b = 'foo' --: X
    x.c = {x.b} --: Y
end
do
    --# type local X = boolean
    --# type local Y = {X}
    x.d = true --: X
    x.e = {x.d} --: Y
end
local y = x.x --@< Error: Missing key "x" in `{a: X, b: X#1, c: Y, d: X#2, e: Y#1, ...}`
--! error

