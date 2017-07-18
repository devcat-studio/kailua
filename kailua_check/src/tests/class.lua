-- Class tests for the Kailua type checker.

--8<-- assume-class-local
local Hello
--# assume class Hello
local x --: Hello
--! ok

--8<-- assume-class-global
--# assume global class Hello
local x --: Hello
--! ok

--8<-- assume-class-unknown-class-system
--# assume global class(unknown) Hello --@< Error: `unknown` class system hasn't been defined
--! error

--8<-- assume-class-undefined-class-system
-- the `gideros` class system actually exists but should be explicitly defined
--# assume global class(gideros) Hello --@< Error: `gideros` class system hasn't been defined
--! error

--8<-- assume-class-duplicate-name
--# type Hello = integer
--# assume global class Hello --@< Error: A type `Hello` is already defined
                              --@^^ Note: The type was originally defined here
local x --: Hello
local y = x + 3 --: integer
--! error

--8<-- class-field-static
--# assume global class Hello
Hello.a = 42
local x = Hello.a + 5 --: integer
--! ok

--8<-- class-field-instance
--# assume global class Hello
--v method()
function Hello:f()
    self.a = 42
end
--v method()
function Hello:g()
    local a = self.a + 5 --: integer
    self.a = a
end
--! ok

--8<-- assume-field-class-local
local Hello
--# assume class Hello
--# assume Hello.f: function(Hello) --> string
--# assume static Hello.g: function() --> string

--# assume h: Hello
local x = h:f() --: string
local y = h.g() --: string
--! ok

--8<-- assume-field-class-local-export
--# assume global `require`: [require] function(string) --> any

local Hello = require 'x' --@< Warning: A new name for the previously named class is ignored
--# assume h: Hello
local x = h:f() --: string
local y = h.g() --: string

--& x
local Hello
--# assume class Hello --@< Note: The class was previously named here
--# assume Hello.f: function(Hello) --> string
--# assume static Hello.g: function() --> string
return Hello

--! ok

--8<-- assume-field-class-global
--# assume global class Hello
--# assume Hello.f: function(Hello) --> string
--# assume static Hello.g: function() --> string

--# assume h: Hello
local x = h:f() --: string
local y = h.g() --: string
--! ok

--8<-- assume-field-class-global-export
--# assume global `require`: [require] function(string) --> any

require 'x'
--# assume h: Hello
local x = h:f() --: string
local y = h.g() --: string

--& x
--# assume global class Hello
--# assume Hello.f: function(Hello) --> string
--# assume static Hello.g: function() --> string

--! ok

--8<-- assume-field-class-nested
--# assume global class Hello
--# assume Hello.f.x: function(Hello) --> string
--@^ Error: Cannot apply `--# assume` directive to the inside of fields in the class prototype of `Hello`
--# assume static Hello.g.y: function() --> string
--@^ Error: Cannot apply `--# assume` directive to the inside of fields in the class prototype of `Hello`

--# assume h: Hello
local x = h:f() --: string --@< Error: Cannot index `Hello` with `"f"`
local y = h.g() --: string --@< Error: Cannot index `Hello` with `"g"`
--! error

--8<-- assume-field-class-fields
--# assume global class Hello
--# assume Hello.x: integer
--# assume static Hello.y: string

--v method(n: integer)
function Hello:set(n)
    self.x = n
    self.y = n .. ''
end

--# assume h: Hello
h:set(42)
local x = h.x --: integer
local y = h.y --: string
local xx = Hello.x --: integer --@< Error: Cannot index `<initializing> <prototype for Hello>` with `"x"`
local yy = Hello.y --: string
--! error

--8<-- assume-field-class-method
--# assume global class Hello
--# assume Hello.f: method() --> string
--# assume Hello.g: method(s: string, base: integer?) --> number

--# assume h: Hello
local x = h:f() --: string
local y = h.g(h, "string") --: number
--! ok

--8<-- assume-field-class-method-nested
--# assume global class Hello
--# assume Hello.f.g: method() --> string
--@^ Error: Cannot apply `--# assume` directive to the inside of fields in the class prototype of `Hello`

--# assume h: Hello
local x = h:f() --: string --@< Error: Cannot index `Hello` with `"f"`
local y = h.g() --: string --@< Error: Cannot index `Hello` with `"g"`
--! error

--8<-- class-implicit-module
--# assume global class Hello
--# assume Hello.n: integer

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

--8<-- class-implicit-module-partial-init
--# assume global class Hello
--# assume Hello.n: integer

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

--# assume h: Hello
h:foo()

--! ok

--8<-- class-nil-field-1
--# assume global class Hello
--# assume static Hello.p: integer
--# assume static Hello.q: integer!
--# assume static Hello.r: integer?

local u = Hello.p --: string --@< Error: Cannot assign `integer` into `string`
                             --@^ Note: The other type originates here
local v = Hello.q --: string --@< Error: Cannot assign `integer!` into `string`
                             --@^ Note: The other type originates here
local w = Hello.r --: string --@< Error: Cannot assign `integer?` into `string`
                             --@^ Note: The other type originates here
--! error

--8<-- class-nil-field
--# assume global class Hello
--# assume Hello.p: integer
--# assume Hello.q: integer!
--# assume Hello.r: integer?

--v method()
function Hello:f()
    local u = self.p --: string --@< Error: Cannot assign `integer` into `string`
                                --@^ Note: The other type originates here
    local v = self.q --: string --@< Error: Cannot assign `integer!` into `string`
                                --@^ Note: The other type originates here
    local w = self.r --: string --@< Error: Cannot assign `integer?` into `string`
                                --@^ Note: The other type originates here
end
--! error

--8<-- make-class-no-class-system
--# assume `class`: [make_class] function() --> table
--@^ Error: The type attribute `make_class` requires exactly 1 value(s)
--# assume `class`: [make_class()] function() --> table
--@^ Error: The type attribute `make_class` requires exactly 1 value(s)
--! error

--8<-- make-class-unknown-class-system
-- the `gideros` class system actually exists but should be explicitly defined
--# assume `class`: [make_class(gideros)] function() --> table
--@^ Error: `gideros` class system hasn't been defined
--! error

