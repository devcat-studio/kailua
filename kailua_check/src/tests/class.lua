-- Class tests for the Kailua type checker.

--8<-- make-class
--# assume `class`: [make_class] function() --> table
Hello = class()
--! ok

--8<-- make-class-unnamed
--# assume `class`: [make_class] function() --> table
local x = class() + 3
--@^ Error: Cannot apply + operator to `<prototype for unnamed class #0>` and `3`
--@^^ Cause: `<prototype for unnamed class #0>` is not a subtype of `number`
--! error

--8<-- make-class-named
--# assume `class`: [make_class] function() --> table
Hello = class()
local x = Hello + 3
--@^ Error: Cannot apply + operator to `<prototype for Hello>` and `3`
--@^^ Cause: `<prototype for Hello>` is not a subtype of `number`
--! error

--8<-- make-class-named-local
--# assume `class`: [make_class] function() --> table
local function f()
    local Hello = class()
    local x = Hello + 3
    --@^ Error: Cannot apply + operator to `<prototype for Hello>` and `3`
    --@^^ Cause: `<prototype for Hello>` is not a subtype of `number`
end
--! error

--8<-- make-class-renamed
--# assume `class`: [make_class] function() --> table
Hello = class() --@< Note: The class was previously named here
Goodbye = Hello --@< Warning: A new name for the previously named class is ignored
local x = Goodbye + 3
--@^ Error: Cannot apply + operator to `<prototype for Hello>` and `3`
--@^^ Cause: `<prototype for Hello>` is not a subtype of `number`
--! error

--8<-- make-class-renamed-2
--# assume `class`: [make_class] function() --> table
Hello = class() --@< Note: The class was previously named here
local x
x = Hello --@< Warning: A new name for the previously named class is ignored
local x = x + 3
--@^ Error: Cannot apply + operator to `<prototype for Hello>` and `3`
--@^^ Cause: `<prototype for Hello>` is not a subtype of `number`
--! error

--8<-- make-class-global-name-collision-with-local
--# assume `class`: [make_class] function() --> table
do
    --# type local Hello = string --@< Note: The type was originally defined here
    Hello = class()               --@< Error: A type `Hello` is already defined
end
--! error

--8<-- class-init
--# assume `class`: [make_class] function() --> table
Hello = class()
function Hello:init()
end
local h = Hello.new() --: Hello
--! ok

--8<-- class-init-bad-arity
--# assume `class`: [make_class] function() --> table
Hello = class()
--v method(x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
end
local h = Hello.new(3, 4, 5, 6)
--@^ Error: The type `function(x: integer, y: integer, z: integer) --> Hello` cannot be called
--@^^ Cause: Cannot give more than 3 argument(s) to the function
--@^^^ Note: The other type originates here
--! error

--8<-- class-init-fields
--# assume `class`: [make_class] function() --> table
Hello = class()
--v method(x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x
    self.y = y
    self.z = z
end
local h = Hello.new(3, 4, 5)
local a = h.x + h.y + h.z --: integer
--! ok

--8<-- class-init-fields-2
--# assume `class`: [make_class] function() --> table
Hello = class()
--v method(x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x --: integer|string
    self.y = y
    self.z = z
    self.x = 'string'
end
local h = Hello.new(3, 4, 5)
local a = h.y + h.z --: integer
local b = h.x .. 'hello' --: string
local c = h.x + 42 --@< Error: Cannot apply + operator to `(integer|string)` and `42`
                   --@^ Cause: `(integer|string)` is not a subtype of `number`
--! error

--8<-- class-init-fields-3
--# assume `class`: [make_class] function() --> table
Hello = class()
--v method(x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x
    self.y = y
    self.z = z
    self.x = 'string' --@< Error: Cannot assign `"string"` into `integer`
                      --@^ Note: The other type originates here
end
local h = Hello.new(3, 4, 5)
local a = h.y + h.z --: integer
local b = h.x .. 'hello' --: string
local c = h.x + 42 --: integer
--! error

--8<-- class-init-self-assign
--# assume `class`: [make_class] function() --> table
Hello = class()
function Hello:init()
    --@v Error: `self` variable cannot be assigned in a constructor (`init` method)
    self = 'foo'
end
--! error

--8<-- class-new-assign
--# assume `class`: [make_class] function() --> table
Hello = class()
--v method(x: integer, y: integer, z: integer)
function Hello:new(x, y, z) --@< Error: `new` method is reserved and cannot be defined
end
--! error

--8<-- class-method-bad-arity-1
--# assume `class`: [make_class] function() --> table
Hello = class()

function Hello:init() end

--v method(x: integer, y: integer)
function Hello:foo(x, y) end

local h = Hello.new()
h:foo(3) --@< Error: The type `function(self: Hello, x: integer, y: integer) --> ()` cannot be called
         --@^ Cause: Third method argument cannot be omitted because its type is `integer`
         --@^^ Note: The other type originates here
--! error

--8<-- class-method-bad-arity-2
--# assume `class`: [make_class] function() --> table
Hello = class()

function Hello:init() end

--v method(x: integer, y: integer)
function Hello:foo(x, y) end

local h = Hello.new()
h:foo(3, 4, 5) --@< Error: The type `function(self: Hello, x: integer, y: integer) --> ()` cannot be called
               --@^ Cause: Cannot give more than 3 argument(s) including `self` to the method
               --@^^ Note: The other type originates here
--! error

--8<-- class-method-bad-arity-3
--# assume `class`: [make_class] function() --> table
Hello = class()

function Hello:init() end

--v method(x: integer, y: integer)
function Hello:foo(x, y) end

local h = Hello.new()
h.foo() --@< Error: The type `function(self: Hello, x: integer, y: integer) --> ()` cannot be called
        --@^ Cause: First function argument cannot be omitted because its type is `Hello`
        --@^^ Note: The other type originates here
--! error

--8<-- class-fields-after-ctor
--# assume `class`: [make_class] function() --> table
Hello = class()

--v method(x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x
    self.y = y
    self.z = z
end

function Hello:sum()
    return self.x + self.y + self.z
end

local h = Hello.new(3, 4, 5)
local a = h:sum() --: integer
--! ok

--8<-- class-fields-assign-after-ctor-1
--# assume `class`: [make_class] function() --> table
Hello = class()

--v method(x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x
    self.y = y
    self.z = z
end

function Hello:sum()
    return self.x + self.y + self.z
end

function Hello:add()
    self.x = self.x + self:sum()
    self.y = self.y + self:sum()
    self.z = self.z + self:sum()
end

local h = Hello.new(3, 4, 5)
h:add()
--! ok

--8<-- class-fields-assign-after-ctor-2
--# assume `class`: [make_class] function() --> table
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

--8<-- class-fields-assign-after-ctor-3
--# assume `class`: [make_class] function() --> table
Hello = class()

--v method(x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x
    self.y = y
    self.z = z
end

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

--8<-- class-fields-bad-assign-after-ctor
--# assume `class`: [make_class] function() --> table
Hello = class()

--v method(x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x
    self.y = y
    self.z = z
end

function Hello:summarize()
    self.sum = self.x + self.y + self.z --@< Error: Cannot add a new field to the class instance outside of the constructor
end

local h = Hello.new(3, 4, 5)
h:summarize()
local x = h.sum --: integer --@< Error: Cannot index `Hello` with `"sum"`
--! error

--8<-- class-no-methodcall-in-ctor
--# assume `class`: [make_class] function() --> table
Hello = class()

--v method(x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    --@v Error: Cannot index `[internal constructible] Hello` with `"sum"`
    local n = self:sum()
    self.x = x + n
    self.y = y + n
    self.z = z + n
end

function Hello:sum()
    return self.x + self.y + self.z
end

local h = Hello.new(3, 4, 5)
local s = h:sum() --: integer

-- test error recovery
--@v Error: Cannot index `Hello` with `"average"`
local t = h:average() --: integer

--! error

--8<-- lua51-class-global-naming
--# open lua51
require 'a'
local hh = Hello.new() --: Hello

--& a
--# assume global `class`: [make_class] function() --> table
Hello = class()
function Hello:init() end
local h = Hello.new() --: Hello

--! ok

--8<-- lua51-class-local-naming
--# open lua51
require 'a'
local hh = Hello.new() --: Hello
--@^ Error: Global or local variable `Hello` is not defined
--@^^ Error: Type `Hello` is not defined

--& a
--# assume global `class`: [make_class] function() --> table
local Hello = class()
function Hello:init() end
local h = Hello.new() --: Hello

--! error

--8<-- class-call-ctor
--# assume `class`: [make_class] function() --> table
Hello = class()

function Hello:init() end

local h = Hello.new()
h:init()      --@< Error: The constructor (`init` method) is only internally called and should not be called outside
Hello.init(h) --@< Error: The constructor (`init` method) is only internally called and should not be called outside

--! error

--8<-- assume-field-class-local
--# assume `class`: [make_class] function() --> table
local Hello = class()
function Hello:init() end
--# assume Hello.f: function(Hello) --> string
--# assume static Hello.g: function() --> string

local h = Hello.new()
local x = h:f() --: string
local y = h.g() --: string
--! ok

--8<-- assume-field-class-local-export
--# assume global `class`: [make_class] function() --> table
--# assume global `require`: [require] function(string) --> any

local Hello = require 'x' --@< Warning: A new name for the previously named class is ignored
local h = Hello.new()
local x = h:f() --: string
local y = h.g() --: string

--& x
local Hello = class() --@< Note: The class was previously named here
function Hello:init() end
--# assume Hello.f: function(Hello) --> string
--# assume static Hello.g: function() --> string
return Hello

--! ok

--8<-- assume-field-class-global
--# assume `class`: [make_class] function() --> table
Hello = class()
function Hello:init() end
--# assume Hello.f: function(Hello) --> string
--# assume static Hello.g: function() --> string

local h = Hello.new()
local x = h:f() --: string
local y = h.g() --: string
--! ok

--8<-- assume-field-class-global-export
--# assume global `class`: [make_class] function() --> table
--# assume global `require`: [require] function(string) --> any

require 'x'
local h = Hello.new()
local x = h:f() --: string
local y = h.g() --: string

--& x
Hello = class()
function Hello:init() end
--# assume Hello.f: function(Hello) --> string
--# assume static Hello.g: function() --> string

--! ok

--8<-- assume-field-class-nested
--# assume `class`: [make_class] function() --> table
Hello = class()
function Hello:init() end
--# assume Hello.f.x: function(Hello) --> string
--@^ Error: Cannot apply `--# assume` directive to the inside of fields in the class prototype of `Hello`
--# assume static Hello.g.y: function() --> string
--@^ Error: Cannot apply `--# assume` directive to the inside of fields in the class prototype of `Hello`

local h = Hello.new()
local x = h:f() --: string --@< Error: Cannot index `Hello` with `"f"`
local y = h.g() --: string --@< Error: Cannot index `Hello` with `"g"`
--! error

--8<-- assume-field-class-fields
--# assume `class`: [make_class] function() --> table
Hello = class()
function Hello:init() end
--# assume Hello.x: integer
--# assume static Hello.y: string

--v method(n: integer)
function Hello:set(n)
    self.x = n
    self.y = n .. '' --@< Error: Cannot add a new field to the class instance outside of the constructor
end

local h = Hello.new()
h:set(42)
local x = h.x --: integer
local y = h.y --: string
local xx = Hello.x --: integer --@< Error: Cannot index `<prototype for Hello>` with `"x"`
local yy = Hello.y --: string
--! error

--8<-- assume-field-class-method
--# assume `class`: [make_class] function() --> table
local Hello = class()
function Hello:init() end
--# assume Hello.f: method() --> string
--# assume Hello.g: method(s: string, base: integer?) --> number

local h = Hello.new()
local x = h:f() --: string
local y = h.g(h, "string") --: number
--! ok

--8<-- assume-field-class-method-nested
--# assume `class`: [make_class] function() --> table
local Hello = class()
function Hello:init() end
--# assume Hello.f.g: method() --> string
--@^ Error: Cannot apply `--# assume` directive to the inside of fields in the class prototype of `Hello`

local h = Hello.new()
local x = h:f() --: string --@< Error: Cannot index `Hello` with `"f"`
local y = h.g() --: string --@< Error: Cannot index `Hello` with `"g"`
--! error

--8<-- class-no-module
--# assume `class`: [make_class] function() --> table
local Hello = class()

--v method(n: integer)
function Hello:init(n)
    self.n = n
end

--v method()
function Hello:foo()
    if self.n > 0 then
        self.n = self.n - 1
        self:bar() --@< Error: Cannot index `Hello` with `"bar"`
    end
end

--v method()
function Hello:bar()
    if self.n > 0 then
        self.n = self.n - 1
        self:foo()
    end
end

--! error

--8<-- class-module
--# assume `class`: [make_class] function() --> table
local Hello = class() --: module

--v method(n: integer)
function Hello:init(n)
    self.n = n
end

--v method()
function Hello:foo()
    if self.n > 0 then
        self.n = self.n - 1
        self:bar()
    end
end

--v method()
function Hello:bar()
    if self.n > 0 then
        self.n = self.n - 1
        self:foo()
    end
end

--! ok

--8<-- class-module-partial-init
--# assume `class`: [make_class] function() --> table
local Hello = class() --: module

--v method(n: integer)
function Hello:init(n)
    self.n = n
end

local h = Hello.new(42)

--v method()
function Hello:foo()
    if self.n > 0 then
        self.n = self.n - 1
        self:bar()
    end
end

--v method()
function Hello:bar()
    if self.n > 0 then
        self.n = self.n - 1
        self:foo()
    end
end

h:foo()

--! ok

