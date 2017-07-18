# Class System

Lua normally does not natively support classes and any other object-oriented features. Instead, it defers the difficult job of designing a class system to the users (or library authors). Consequently there are multiple **class systems** used in the wild, and whatever Kailua supports has to cover all of them.

This is close to impossible, so Kailua offers a customizable class system support. Currently the feature is in the early stage of development and not all possible features are implemented, mainly due to the authors' inability to analyze all of them. Analyses and suggestions for other class systems will be appreciated.

## Declaring the class system

The class systems available in the current session are identified with unique global names, and have to be explicitly declared.

```lua
--# class system gideros
```

Currently the name of each class system is fixed and should be chosen from the following list:

| Name | Description | Inheritance | `[make_class]` |
| ---- | ----------- | ----------- | -------------- |
| `gideros` | [Gideros class system](classes-gideros.html) | Single | Supported |

## Declaring a class

The class can be declared in two ways.

1. `--# assume class` directive can declare and assume the existing class.
2. If the class system has a support, a function with a `[make_class(<class system>)]` attribute will generate a new class whenever called.

### `--# assume class` directive

The `--# assume class` directive can declare a class without class system. This is useful for defining a simple class with no inheritance and special semantics.

```lua
-- defines a global name `Hello` to be a class prototype for
-- the newly defined global class `Hello`.

--# assume global class Hello
```

Since this is an ordinary directive (that is going to be ignored by Lua), a local class (without `global`) requires the name to be already a local name.

```lua
local Hello = {}
--# assume class Hello
```

The class system, if any, should be enclosed with parentheses. If the class system supports inhertiance the parent class can be also specified.

```lua
--# assume global class(gideros) Object
--# assume global class(gideros) Sprite: Object
```

In general different class systems (including those with no class system) do not mix.

### `[make_class]` attribute

A function with the `[make_class]` attribute is much closer to how ordinary Lua code defines a new class. This attribute is exclusive to class systems, and such a function can be `--# assume`d like this:

```lua
--# assume global `class`: [make_class(gideros)] function() --> table
```

Or can be explicitly declared with a function specification:

```lua
--v [NO_CHECK] -- because the gory detail of classes is hard to check
--v [make_class(gideros)]
--v function() --> table
function make_class()
    -- ...
end
```

In many cases calling without any arguments will define a new class without explicit parents, and calling with an argument of the parent class prototype will do the inheritance. The exact interpretation of those arguments is however up to the class system.

A new class prototype returned by the `[make_class]` function should be assigned to the variable as soon as possible. This is when the class receives its name:

```lua
local Hello = class() -- will also define a local type `Hello`

Sprite = class() -- will also define a global type `Sprite`
```

It is possible but not recommended to use a class without name (there are some backdoors). Such classes will be uniquely identified in the messages.

### Defining fields and methods

Once the class is declared, fields and methods can be freely added to them as long as it does not break the class system's own limitations:

```lua
--# assume global class Person

--v function(name: string) --> Person
function Person.new(name)
    local person = {}
    set_metaclass_for_person(person) -- up to your imagination
    --# assume person: Person

    person.name = name -- will define a new field
    return person
end

-- declares a new class field
Person.HELLO = 'Hello'

-- declares a new method (which is just a class field with a function receiving `self`)
--v method()
function Person:greet()
    print(self.HELLO .. ', ' .. self.name)
end

local person = Person.new('J. Random Hacker')
person:greet()
```

As one can observe:

* Without a class system, `--# assume` is essential to produce a new instance type. On the other hands, class systems will typically have their own ways to automatically derive a `new` method or similar from a constructor.

* The instance field can be created via assignments. Reading a missing field still errors, so if we have transmuted the `person` variable *after* setting a `name` field the checker will miss its existence.

* The method can be defined with the same syntax as functions, but it uses the `method` keyword and the `self` argument is omitted (and inferred).

`--# assume` also works for classes. The code above will look like the following if we can assume they already exist and we only want to add types:

```lua
--# assume global class Person
--# assume static Person.new: function(name: string) --> Person
--# assume static Person.HELLO: string
--# assume Person.greet: method()

local person = Person.new('J. Random Hacker')
person:greet()
```

This is same to ordinary `--# assume`, but please note several differences:

* Conceptually `Person.greet` should be a `static` function with the first argument typed as `Person`. The `method` keyword was used here for the convenience (it is not a true type).

* Fields of globally defined class can be only `--# assume`d in the topmost scope. This is because `--# assume` generally creates a copy of given name in the current local scope, but creating a new field will globally affect that class.

* Some fields can simply not be defined depending on the class system.

<!-- TODO: mention that the classes prototypes are automatically subject to delayed type checking (needs to explain this first) -->

