-- Class tests for the Kailua type checker.

--8<-- make-class
--# assume `class`: [make_class] function() -> table
Hello = class()
--! ok

--8<-- make-class-unnamed
--# assume `class`: [make_class] function() -> table
local x = class() + 3
--@^ Error: `<prototype for unnamed class #0>` is not a subtype of `number`
--! error

--8<-- make-class-named
--# assume `class`: [make_class] function() -> table
Hello = class()
local x = Hello + 3
--@^ Error: `<prototype for Hello>` is not a subtype of `number`
--! error

--8<-- make-class-named-local
--# assume `class`: [make_class] function() -> table
local function f()
    local Hello = class()
    local x = Hello + 3
    --@^ Error: `<prototype for Hello>` is not a subtype of `number`
end
--! error

--8<-- make-class-renamed
--# assume `class`: [make_class] function() -> table
Hello = class() --@< Note: The class was previously named here
Goodbye = Hello --@< Warning: A new name for the previously named class is ignored
local x = Goodbye + 3
--@^ Error: `<prototype for Hello>` is not a subtype of `number`
--! error

--8<-- make-class-renamed-2
--# assume `class`: [make_class] function() -> table
Hello = class() --@< Note: The class was previously named here
local x
x = Hello --@< Warning: A new name for the previously named class is ignored
local x = x + 3
--@^ Error: `<prototype for Hello>` is not a subtype of `number`
--! error

--8<-- make-class-global-name-collision-with-local
--# assume `class`: [make_class] function() -> table
do
    --# type Hello = string --@< Note: The type was originally defined here
    Hello = class()         --@< Error: A type named `Hello` is already defined
end
--! error

--8<-- class-init
--# assume `class`: [make_class] function() -> table
Hello = class()
function Hello:init()
end
local h = Hello.new() --: Hello
--! ok

--8<-- class-init-bad-arity
--# assume `class`: [make_class] function() -> table
Hello = class()
--v (x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
end
local h = Hello.new(3, 4, 5, 6)
--@^ Error: `6` is not a subtype of `nil`
--! error

--8<-- class-init-fields
--# assume `class`: [make_class] function() -> table
Hello = class()
--v (x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x
    self.y = y
    self.z = z
end
local h = Hello.new(3, 4, 5)
local a = h.x + h.y + h.z --: integer
--! ok

--8<-- class-init-fields-currently
--# assume `class`: [make_class] function() -> table
Hello = class()
--v (x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x
    self.y = y
    self.z = z + self.x
    self.x = 'string'
end
local h = Hello.new(3, 4, 5)
local a = h.y .. h.z --: integer
local b = h.x .. 'hello' --: string
--! ok

--8<-- class-init-self-assign
--# assume `class`: [make_class] function() -> table
Hello = class()
function Hello:init()
    --@v Error: `self` variable cannot be assigned in a constructor (`init` method)
    self = 'foo'
end
--! error

--8<-- class-new-assign
--# assume `class`: [make_class] function() -> table
Hello = class()
--v (x: integer, y: integer, z: integer)
function Hello:new(x, y, z) --@< Error: `new` method is reserved and cannot be defined
end
--! error

--8<-- class-fields-after-ctor
--# assume `class`: [make_class] function() -> table
Hello = class()

--v (x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x
    self.y = y
    self.z = z
end

function Hello:sum()
    return self.x + self.y + self.z
end

local h = Hello.new(3, 4, 5)
local a = h:sum() --: var integer
--! ok

--8<-- class-fields-assign-after-ctor-1
--# assume `class`: [make_class] function() -> table
Hello = class()

--v (x: integer, y: integer, z: integer)
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
--# assume `class`: [make_class] function() -> table
Hello = class()

--v (x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x
    self.y = y
    self.z = z
end

function Hello:add(n)
    self.x = self.x + n
    self.y = self.y + n
    self.z = self.z + n
end

local h = Hello.new(3, 4, 5)
h:add(8)
--! ok

-->8-- class-fields-assign-after-ctor-3
--# assume `class`: [make_class] function() -> table
Hello = class()

--v (x: integer, y: integer, z: integer)
function Hello:init(x, y, z)
    self.x = x
    self.y = y
    self.z = z
end

function Hello:stringify()
    -- this is invalid, the type cannot be no longer changed
    self.x = '' .. self.x
    self.y = '' .. self.y
    self.z = '' .. self.z
end

local h = Hello.new(3, 4, 5)
h:stringify()
--! error

--8<-- class-no-methodcall-in-ctor
--# assume `class`: [make_class] function() -> table
Hello = class()

--v (x: integer, y: integer, z: integer)
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
local s = h:sum() --: var integer

-- test error recovery
--@v Error: Cannot index `Hello` with `"average"`
local t = h:average() --: var integer

--! error

--8<-- lua51-class-global-naming
--# open lua51
require 'a'
local hh = Hello.new() --: Hello

--& a
--# assume global `class`: [make_class] function() -> table
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
--# assume global `class`: [make_class] function() -> table
local Hello = class()
function Hello:init() end
local h = Hello.new() --: Hello

--! error

