-- Gideros class system support tests for the Kailua type checker.

--8<-- gideros-assume-class
--# class system gideros
--# assume global class(gideros) Hello
local x --: Hello
--! ok

--8<-- gideros-assume-class-parent-1
--# class system gideros
--# assume global class(gideros) Greeting
--# assume global class(gideros) Hello: Greeting
local x --: Hello
local y = x --: Greeting
--! ok

--8<-- gideros-assume-class-parent-2
--# class system gideros
--# assume global class(gideros) Greeting
--# assume global class(gideros) Hello: Greeting
local x --: Greeting
local y = x --: Hello --@< Error: Cannot assign `Greeting` into `Hello`
                      --@^ Note: The other type originates here
--! error

--8<-- gideros-assume-class-parent-self
--# class system gideros
--# assume global class(gideros) Hello: Hello --@< Error: Type `Hello` is not defined
local x --: Hello
--! error

--8<-- gideros-assume-class-parent-missing
--# class system gideros
--# assume global class(gideros) Hello: Greeting --@< Error: Type `Greeting` is not defined
local x --: Hello
--! error

--8<-- gideros-assume-class-other-class-system
--# class system gideros
--# assume global class Foo
--# assume global class(gideros) Hello: Foo --@< Error: The class cannot inherit from a class using a different class system
--! error

--8<-- gideros-make-class
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class()
--! ok

--8<-- gideros-make-class-no-check
--# class system gideros
--v [NO_CHECK] [make_class(gideros)] function() --> table
function class()
    return {}
end
Hello = class()
--! ok

--8<-- gideros-make-class-unnamed
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
local x = class() + 3
--@^ Error: Cannot apply + operator to `<initializing> <prototype for <unnamed class #1.0>>` and `3`
--@^^ Cause: `<prototype for <unnamed class #1.0>>` is not a subtype of `number`
--! error

--8<-- gideros-make-class-named
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class()
local x = Hello + 3
--@^ Error: Cannot apply + operator to `<initializing> <prototype for Hello>` and `3`
--@^^ Cause: `<prototype for Hello>` is not a subtype of `number`
--! error

--8<-- gideros-make-class-named-local
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
local function f()
    local Hello = class()
    local x = Hello + 3
    --@^ Error: Cannot apply + operator to `<initializing> <prototype for Hello>` and `3`
    --@^^ Cause: `<prototype for Hello>` is not a subtype of `number`
end
--! error

--8<-- gideros-make-class-renamed
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class() --@< Note: The class was previously named here
Goodbye = Hello --@< Warning: A new name for the previously named class is ignored
local x = Goodbye + 3
--@^ Error: Cannot apply + operator to `<initializing> <prototype for Hello>` and `3`
--@^^ Cause: `<prototype for Hello>` is not a subtype of `number`
--! error

--8<-- gideros-make-class-renamed-2
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class() --@< Note: The class was previously named here
local x
x = Hello --@< Warning: A new name for the previously named class is ignored
local x = x + 3
--@^ Error: Cannot apply + operator to `<initializing> <prototype for Hello>` and `3`
--@^^ Cause: `<prototype for Hello>` is not a subtype of `number`
--! error

--8<-- gideros-make-class-parent-1
--# class system gideros
--# assume `class`: [make_class(gideros)] function(any) --> table
A = class()
B = class(A)
C = class(A)
D = class(B)
--! ok

--8<-- gideros-make-class-parent-2
--# class system gideros
--# assume `class`: [make_class(gideros)] function(any) --> table
--# open `internal kailua_test`
A = class()
B = class('string') --@< Error: The non-class type `"string"` cannot be a parent class
local x = nil --: A
C = class(x) --@< Error: The non-class type `A` cannot be a parent class
D = class(kailua_test.gen_tvar()) --@< Error: The non-class type `<unknown type>` cannot be a parent class
--! error

--8<-- gideros-make-class-global-name-collision-with-local
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
do
    --# type local Hello = string --@< Note: The type was originally defined here
    Hello = class()               --@< Error: A type `Hello` is already defined
end
--! error

--8<-- gideros-make-class-other-class-system
--# class system gideros
--# assume global class Foo
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class(Foo) --@< Error: The class cannot inherit from a class using a different class system
--! error

--8<-- gideros-class-init
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table

Hello = class()
--v method()
function Hello:init()
end

local h = Hello.new() --: Hello
--! ok

--8<-- gideros-class-init-no-self
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table

Hello = class()

--v function()
function Hello.init() --@< Error: The type `function() --> ()` of the constructor (`init` method) doesn't have a correct type for the first argument
end

local h = Hello.new() --: Hello
--! error

--8<-- gideros-class-init-bad-self
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table

Hello = class()

--v function(self: map<string, integer>)
function Hello.init(self) --@< Error: The type `function(self: map<string, integer>) --> ()` of the constructor (`init` method) doesn't have a correct type for the first argument
    self.foo = 42
end

local h = Hello.new() --: Hello
--! error

--8<-- gideros-class-init-no-check
--# class system gideros
--v [NO_CHECK] [make_class(gideros)] function() --> table
function class()
    return {}
end

Hello = class()
--v method()
function Hello:init()
end

local h = Hello.new() --: Hello
--! ok

--8<-- gideros-class-init-bad-arity
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table

Hello = class()
--v method(x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
end

local h = Hello.new(3, 4, 5, 6)
--@^ Error: The type `function(x: integer, y: integer, z: integer) --> Hello` cannot be called
--@^^ Cause: Cannot give more than 3 argument(s) to the function
--@^^^ Note: The other type originates here
--! error

--8<-- gideros-class-init-fields
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table

do
    Hello = class()
    --v method(x: integer, y: integer, z: integer)
    function Hello:init(x, y, z)
        self.x = x
        self.y = y
        self.z = z
    end
end

local h = Hello.new(3, 4, 5)
local a = h.x + h.y + h.z --: integer
--! ok

--8<-- gideros-class-init-fields-2
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table

do
    Hello = class()
    --v method(x: integer, y: integer, z: integer)
    function Hello:init(x, y, z)
        self.x = x --: integer|string
        self.y = y
        self.z = z
        self.x = 'string'
    end
end

local h = Hello.new(3, 4, 5)
local a = h.y + h.z --: integer
local b = h.x .. 'hello' --: string
local c = h.x + 42 --@< Error: Cannot apply + operator to `(integer|string)` and `42`
                   --@^ Cause: `(integer|string)` is not a subtype of `number`
--! error

--8<-- gideros-class-init-fields-3
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table

do
    Hello = class()
    --v method(x: integer, y: integer, z: integer)
    function Hello:init(x, y, z)
        self.x = x
        self.y = y
        self.z = z
        self.x = 'string' --@< Error: Cannot assign `"string"` into `integer`
                          --@^ Note: The other type originates here
    end
end

local h = Hello.new(3, 4, 5)
local a = h.y + h.z --: integer
local b = h.x .. 'hello' --: string
local c = h.x + 42 --: integer
--! error

--8<-- gideros-class-init-self-assign
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class()
--v method()
function Hello:init()
    self = self
end
--! ok

--8<-- gideros-class-new-assign
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class()
--v method(x: integer, y: integer, z: integer)
function Hello:new(x, y, z) --@< Error: `new` method is reserved and cannot be defined
end
--! error

--8<-- gideros-class-missing-ctor
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class()
local x = Hello.new() --@< Error: The `new` method cannot be called with no constructor (`init` method) defined
--! error

--8<-- gideros-class-method-bad-arity-1
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table

Hello = class()

--v method()
function Hello:init() end

--v method(x: integer, y: integer)
function Hello:foo(x, y) end

local h = Hello.new()
h:foo(3) --@< Error: The type `function(self: Hello, x: integer, y: integer) --> ()` cannot be called
         --@^ Cause: Third method argument cannot be omitted because its type is `integer`
         --@^^ Note: The other type originates here
--! error

--8<-- gideros-class-method-bad-arity-2
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class()

--v method()
function Hello:init() end

--v method(x: integer, y: integer)
function Hello:foo(x, y) end

local h = Hello.new()
h:foo(3, 4, 5) --@< Error: The type `function(self: Hello, x: integer, y: integer) --> ()` cannot be called
               --@^ Cause: Cannot give more than 3 argument(s) including `self` to the method
               --@^^ Note: The other type originates here
--! error

--8<-- gideros-class-method-bad-arity-3
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class()

--v method()
function Hello:init() end

--v method(x: integer, y: integer)
function Hello:foo(x, y) end

local h = Hello.new()
h.foo() --@< Error: The type `function(self: Hello, x: integer, y: integer) --> ()` cannot be called
        --@^ Cause: First function argument cannot be omitted because its type is `Hello`
        --@^^ Note: The other type originates here
--! error

--8<-- gideros-class-fields-after-ctor
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class()

--v method(x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x
    self.y = y
    self.z = z
end

--v method() --> integer
function Hello:sum()
    return self.x + self.y + self.z
end

local h = Hello.new(3, 4, 5)
local a = h:sum() --: integer
--! ok

--8<-- gideros-class-fields-assign-after-ctor-1
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class()

--v method(x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x
    self.y = y
    self.z = z
end

--v method() --> integer
function Hello:sum()
    return self.x + self.y + self.z
end

--v method()
function Hello:add()
    self.x = self.x + self:sum()
    self.y = self.y + self:sum()
    self.z = self.z + self:sum()
end

local h = Hello.new(3, 4, 5)
h:add()
--! ok

--8<-- gideros-class-fields-assign-after-ctor-2
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class()

--v method(x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x
    self.y = y
    self.z = z
end

--v method(n: integer)
function Hello:add(n)
    self.x = self.x + n
    self.y = self.y + n
    self.z = self.z + n
end

local h = Hello.new(3, 4, 5)
h:add(8)
--! ok

--8<-- gideros-class-fields-assign-after-ctor-3
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class()

--v method(x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x
    self.y = y
    self.z = z
end

--v method()
function Hello:stringify()
    -- this is invalid, the type cannot be no longer changed
    self.x = '' .. self.x --@< Error: Cannot assign `string` into `integer`
                          --@^ Note: The other type originates here
    self.y = '' .. self.y --@< Error: Cannot assign `string` into `integer`
                          --@^ Note: The other type originates here
    self.z = '' .. self.z --@< Error: Cannot assign `string` into `integer`
                          --@^ Note: The other type originates here
end

local h = Hello.new(3, 4, 5)
h:stringify()
--! error

--8<-- gideros-class-fields-bad-assign-after-ctor
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class()

--v method(x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x
    self.y = y
    self.z = z
end

--v method()
function Hello:summarize()
    self.sum = self.x + self.y + self.z
end

local h = Hello.new(3, 4, 5)
h:summarize()
local x = h.sum --: integer --@< Error: Cannot index `Hello` with `"sum"`

--! error

--8<-- gideros-class-methodcall-in-ctor
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class()

--v method(x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    local n = self:sum() -- as Hello is implicitly delay-checked this is fine
    self.x = x + n
    self.y = y + n
    self.z = z + n
end

--v method() --> integer
function Hello:sum()
    return self.x + self.y + self.z
end

local h = Hello.new(3, 4, 5)
local s = h:sum() --: integer

-- test error recovery
--@v Error: Cannot index `Hello` with `"average"`
local t = h:average() --: integer

--! error

--8<-- gideros-class-parent-simple
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
A = class()

--v method()
function A:reset()
    self.x = 0 --: integer
end

--v method() --> integer
function A:f()
    return self.x
end

B = class(A)

--v method(x: integer)
function B:init(x)
    self.x = x
end

local b = B.new(42) --: B
local x = b:f() --: integer

--! ok

--8<-- gideros-class-parent-transitive
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
A = class()

--v method()
function A:reset()
    self.x = 0 --: integer
end

--v method() --> integer
function A:f()
    return self.x
end

B = class(A)

--v method() --> integer
function B:g()
    return self:f() * 2
end

C = class(B)

--v method(x: integer)
function C:init(x)
    self.x = x
end

local b = B.new(42) --: B --@< Error: The `new` method cannot be called with no constructor (`init` method) defined

local c = C.new(42) --: C
local x = c:f() * c:g() --: integer

local b = c --: B
local x = b:g() --: integer

local a = b --: A
local x = a:g() --: integer --@< Error: Cannot index `A` with `"g"`

--! error

--8<-- gideros-class-parent-not-shared
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
A = class()

--v method() --> integer
function A:f()
    return 42
end

B = class(A)

--v method(x: integer)
function B:init(x)
    self.x = x
end

--v method() --> integer
function B:g()
    return self:f() * self.x
end

C = class(A)

--v method(x: string)
function C:init(x)
    self.x = x
end

--v method() --> string
function C:g()
    return self:f() .. self.x
end

local b = B.new(42) --: B
local c = C.new('string') --: C
local bb = b:g() --: integer
local cc = c:g() --: string

local c = C.new(42) --: C --@< Error: The type `function(x: string) --> C` cannot be called
                          --@^ Cause: First function argument `42` is not a subtype of `string`
                          --@^^ Note: The other type originates here
local b = B.new('string') --: B --@< Error: The type `function(x: integer) --> B` cannot be called
                                --@^ Cause: First function argument `"string"` is not a subtype of `integer`
                                --@^^ Note: The other type originates here

--! error

--8<-- gideros-class-prototype-override-by-assume-1
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
A = class()
B = class(A)

--# assume static A.a: number
--# assume static B.a: number

--# assume static A.b: number
--# assume static B.b: integer --@< Error: Tried to override a field `b` in a parent class but `integer` is not a subtype of `number` when being inside the mutable class
                               --@^^ Note: Previous definition of the field type here

--# assume static A.c: integer
--# assume static B.c: number --@< Error: Tried to override a field `c` in a parent class but `number` is not a subtype of `integer` when being inside the mutable class
                              --@^^ Note: Previous definition of the field type here

--! error

--8<-- gideros-class-prototype-override-by-assume-2
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
A = class()
B = class(A)

--# assume static A.a: const number
--# assume static B.a: const number

--# assume static A.b: const number
--# assume static B.b: const integer

--# assume static A.c: const integer
--# assume static B.c: const number --@< Error: Tried to override a field `c` in a parent class but `const number` is not a subtype of `const integer` when being inside the mutable class
                                    --@^^ Note: Previous definition of the field type here

--! error

--8<-- gideros-class-prototype-override-by-assign-1
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
A = class()
B = class(A)

A.a = 42 --: number
B.a = 54 --: number

A.b = 42 --: number
B.b = 54 --: integer --@< Error: Cannot assign `integer` into `number`
                     --@^ Note: The other type originates here
                     --@^^ Cause: `number` does not equal to `integer`
                     --@^^^ Note: The other type originates here

A.c = 42 --: integer
B.c = 54 --: number --@< Error: Cannot assign `number` into `integer`
                    --@^ Note: The other type originates here
                    --@^^ Cause: `integer` does not equal to `number`
                    --@^^^ Note: The other type originates here

--! error

--8<-- gideros-class-prototype-override-by-assign-2
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
A = class()
B = class(A)

A.a = 42 --: const number
B.a = 54 --: const number

A.b = 42 --: const number
B.b = 54 --: const integer
--! ok

-->8-- gideros-class-prototype-override-by-assign-2x
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
A = class()
B = class(A)

-- TODO this should not be possible, but skipped due to the constraint solver bug
A.c = 42 --: const integer
B.c = 54 --: const number --@< Error: Cannot assign `const number` into `const integer`
                          --@^ Note: The other type originates here
                          --@^^ Cause: `number` is not a subtype of `integer`
                          --@^^^ Note: The other type originates here

--! error

--8<-- gideros-class-instance-override
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
A = class()
B = class(A)

--v method()
function A:foo()
    self.a = 42.195
    self.b = 42.195
    self.c = 42
end

--v method()
function B:bar()
    self.a = 42.195
    self.b = 42
    self.c = 42.195 --@< Error: Cannot assign `number` into `integer`
                    --@^ Note: The other type originates here
end

--! error

--8<-- gideros-lua51-class-global-naming
--# open lua51
require 'a'
local hh = Hello.new() --: Hello

--& a
--# class system gideros
--# assume global `class`: [make_class(gideros)] function() --> table
Hello = class()
--v method()
function Hello:init() end
local h = Hello.new() --: Hello

--! ok

--8<-- gideros-lua51-class-local-naming
--# open lua51
require 'a'
local hh = Hello.new() --: Hello
--@^ Error: Global or local variable `Hello` is not defined
--@^^ Error: Type `Hello` is not defined

--& a
--# class system gideros
--# assume global `class`: [make_class(gideros)] function() --> table
local Hello = class()
--v method()
function Hello:init() end
local h = Hello.new() --: Hello

--! error

--8<-- gideros-class-call-ctor
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
Hello = class()

--v method()
function Hello:init() end

local h = Hello.new()
h:init() --@< Error: The constructor (`init` method) should not be accessed through instances
         --@^ Error: Cannot index `Hello` with `"init"`
Hello.init(h) -- fine!

--! error

--8<-- gideros-class-overriding-ctor
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
A = class()
B = class(A)

--v method(a: string)
function A:init(a) end

-- the naming difference does not matter
--v method(b: string)
function B:init(b) end

C = class(B)
D = class(C)

--v method(d: string)
function D:init(d) end

local a = A.new('alpha') --: A
local b = B.new('beta') --: B
local c = C.new('gamma') --: C -- makes use of B.init
local d = D.new('delta') --: D

--! ok

--8<-- gideros-class-overriding-parent-prototype
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
A = class()
B = class(A)

--v method()
function B:x() end

-- currently disallowed
--v method()
function A:x() end --@< Error: Cannot create a field with the key `x` already defined in ancestor classes

--! error

--8<-- gideros-class-overriding-parent-instance
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
A = class()
B = class(A)

--v method()
function B:x() end

--v method()
function A:f()
    self.x = 42 --@< Error: Cannot create a field with the key `x` already defined in ancestor classes
end

--! error

--8<-- gideros-class-overriding-current-prototype
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
A = class()

--v method()
function A:x() end

--v method()
function A:f()
    self.x = 42 --@< Error: Cannot assign `integer` into `function(self: A) --> ()`
                --@^ Note: The other type originates here
                --@^^ Cause: `function(self: A) --> ()` does not equal to `integer`
                --@^^^ Note: The other type originates here
end

--! error

--8<-- gideros-class-overriding-current-instance
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
do
    A = class()

    --v method()
    function A:f()
        self.x = 42
    end

    -- by this point the instance field is set
end

A.x = 'string' --@< Error: Cannot create a class field with the key `x` already defined in instances
--! error

--8<-- gideros-class-default-parent
--# class system gideros
--# assume `class`: [make_class(gideros)] function() --> table
A = class()
B = class()
C = class(A)

--# assume a: A
--# assume b: B
--# assume c: C
a = b
a = c
--! ok

--8<-- gideros-assume-class-default-parent
--# class system gideros
--# assume global class(gideros) A
--# assume global class(gideros) B --@< Error: There should be a single class without a parent in the `gideros` class system
--# assume global class(gideros) C: A
--! error

