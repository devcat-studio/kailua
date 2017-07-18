# Gideros Support

The `gideros` class system mimics the behavior of [Gideros classes](http://docs.giderosmobile.com/classes_in_gideros.html). This class system was chosen because, besides from the existing requirements, it represents a typical instance of naively defined class system that can be problematic for the type checker.

A typical declaration for the `gideros` class system is as follows:

```lua
--# class system gideros
--# assume global class(gideros) Object
--# assume global Core: {}
--# assume Core.class: [make_class(gideros)] function(parent: table?) --> table
```

This defines the `Object` class and a `Core.class` class-generating function.

## Constructor

The `new` method is automatically created when the `init` method is created.

```lua
--# assume global class(gideros) Foo: Object

--v method(text: string)
function Foo:init(text)
    self.text = text
end

local x = Foo.new('string')
```

Note that the actual creation currently occurs at the first invocation of `new`, so the error may be delayed to the usage site.

## Inheritance

The Gideros class system supports the single inheritance and shares the general syntax and behavior described in the [earlier section](classes.html).

Fields in child classes simply shadows a previously defined field in a parent, which would break the subtyping (using a child class to the place expecting a parent class) if not restricted. **Therefore in Kailua fields cannot be normally overriden.** The exception is made for constructors (`init`), which cannot be explicitly called through instances anyway.

In Gideros every class is assumed to be a descendant of the `Object` class. **Kailua recognizes the first (and only) class defined without a parent as such a class and disallows multiple such classes.** The `Core.class` function will use `Object` as a parent if no other parent is specified. Since this implicit behavior is confusing otherwise, though, `--# assume class` should always specify the parent class even when it would be `Object`.

