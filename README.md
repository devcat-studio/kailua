# 🌴 Kailua

**Kailua** is an experimental type checker and integrated development environment (IDE)
for the [Lua] programming language (currently only Lua 5.1 is supported).

## Installation

*TBW*

## Usage

Kailua can be used as a standalone checker (`kailua-check`) or an IDE plugin.

In the latter case, you would need a separate configuration file
to specify where to start the checking.
In the case of [Visual Studio Code][VSCode],
the file `.vscode/kailua.json` should contain the following:

```lua
{
    "start_path": "<path to the entry point>"
}
```

Once you've set the entry point, you can write your first Kailua code:

```lua
--# open lua51
print('Hello, world!')
```

Play a bit with this code to see which errors Kailua can detect.

## Overview

### Special Comments

Kailua is a subset of valid Lua code---you don't need any transpilation or compilation.
The additional annotations are described in special comments:

* `--: <type>` describes the type(s) of preceding item(s).

  It is valid anywhere a new name can be possibly defined:
  `local` (either individual names or statement), `function` (arguments),
  `for` (individual names) and assignment (either individual names or statement).

  When used right after the name, you can put a comma or closing parenthesis
  *before* the type like this:

  ```lua
  function f(x, --: integer
             y, --: integer
             z) --: integer
      -- ...
  end
  ```

  For the common case of defining multiple names you can put types after the statement.
  In this case types are delimited by commas.

* `--> <type>` describes the type(s) of function returns.

  It is valid only after the closing parenthesis of function arguments.
  It is valid to put `--:` and `--v` in the same line.

* `--v function(<name>: <type> ...) [--> <type>]` describes a function type.

  It is valid before the `function` keyword (yes, also for anonymous functions).
  This is equivalent to `--:` and `-->`, but much more readable.

* `--# ...` is a special directive for the type checker.

  The most important directive is `--# open <name>`,
  which also implicitly specifies what language variant is currently in use.
  The only supported name so far is `lua51`, for the vanilla Lua 5.1.
  It is recommended to put it to the first non-comment line in the entry point.

  `--# type <name> = <type>` can be used to declare a type alias.
  The type alias is locally scoped (much like `local` but no shadowing).

  `--# assume [global] <name>: <type>` *overrides* the type for given name.
  The `global` keyword forces the global assignment,
  otherwise a new scope is created like `local` statements.
  It is useful for sidestepping the checker issue, but it is also highly unsafe.
  **Use at your own risk.**

  More directives are likely to come.

The equal kind of special comments can span multiple lines.

```lua
--# type Date = {
--#     hour = integer;
--#     min = integer;
--#     sec = integer;
--# }
```

### Types

The following basic types are recognized:

* `nil`, `boolean`, `number`, `string`, `function`, `userdata`, `thread`, `table`
  for primitive Lua types.

* `integer` for a check-time subset of `number`. (Primitive in Lua 5.3 or later)

* Boolean, integer and string literals are valid subtypes of
  `boolean`, `integer` and `string`, respectively.

* `vector<T>` for a table with consecutive integer keys.

* `map<Key, Value>` for a homogeneous associative table.

* `{ key = T, ... }` for records, whose keys are strings and fixed at the check time.
  You can use semicolons in place of commas.

* `{ T, ... }` for tuples, whose keys are consecutive integers.
  Otherwise they are similar to records.

* `function(Arg, ...)` or `function(Arg, ...) --> Ret` for functions.
  `Ret` can be multiple types, in which case you need parentheses
  (`function(vector<T>, integer) --> (integer, string)`).

* `T | T | ...` for union types.
  They are mostly useful for literal types (e.g. `"read" | "write" | "execute"`).
  Kailua has very limited support for checking other kinds of union types.

* `any` has no type information.
  `--# assume` is the only way to make it useful.

* `WHATEVER` (note the case) is a *hole* that the type checker always accepts.
  `map<integer, WHATEVER>` and `map<WHATEVER, string>` are compatible;
  `map<integer, WHATEVER>` and `map<string, string>` are not.
  As this thwarts the basic of type checking, **use at your own risk.**

The Kailua types are by default *not checked for `nil`*.
That is, you can assign `nil` to `integer` but you can also add two `integer`s;
the valid Kailua code can still result in an error therefore.
This restriction was required for making a practical type checker
without changing the source programming language.

You can opt in two other `nil`-handling modes if you need to make it explicit.
As they are (transitively) freely assignable, think them more as a documentation.

* `T?` also accepts `nil` but it is aware that it can contain `nil`.
  Two `integer?`s cannot be added.

* `T!` guarantees that it cannot contain `nil`.

Also, the table values are always `T` or `T?` (for the obvious reason).

Finally, types for the names and table values can optionally have a `const` prefix.
You cannot modify `const` types: `map<integer, const vector<string>>`.
You can still assign to them (otherwise this type won't be useful at all).

### Avoiding the type checker

As annotating everything is not practical,
Kailua supports two ways to avoid the type checking with more localized guarantees:

* `--v [no_check] function(...)` disables the type checking for the following function.

  Kailua essentially *believes* the specified function type,
  which can be no longer omitted.

* You can override what file to check by having `.kailua` files.

  When `require()` was used with a check-time string
  Kailua makes use of `package.path` and `package.cpath` set.
  For `package.path`, it will try `F.kailua` first before reading a file `F`.
  For `package.cpath`, it will always `F.kailua` as `F` would be probably binary.
  (Note that this will normally result in two extensions `.lua.kailua`
  unless you have a sole `?` in the search paths.)

  `.kailua` files would frequently use `--# assume`
  as you should *assume* that the original code has given types.

## Source Organization

Kailua is a [Rust] application, composed of multiple [crates][crates-and-modules]:

* [`kailua_env`](kailua_env/): Span information, source code management, scope mapping.

* [`kailua_diag`](kailua_diag/): Rudimentary localization and reporting facility.

* [`kailua_test`](kailua_test/): Testing harness for `kailua_syntax` and `kailua_check`.
  For those crates `cargo test` will include a battery of integration tests.

* [`kailua_syntax`](kailua_syntax/): Versatile Lua lexer and parser.
  Aware of Kailua extensions as well.

* [`kailua_check`](kailua_check/): Main type checker for Kailua.

* [`kailua_vs`](kailua_vs/): Working but now abandoned [Visual Studio][VS] IDE extension.
  Includes both [C#](kailua_vs/Source/) and [Rust](kailua_vs/src/) codes.

* [`kailua_vsc`](kailua_vsc/): [Visual Studio Code][VSCode] IDE extension in progress.

  * [`kailua_langsvr`](kailua_langsvr/): A standalone language server for
    [Visual Studio Code][VSCode].

  * [`kailua_langsvr_protocol`](kailua_langsvr_protocol/): A macro-heavy portion of
    `kailua_langsvr`, separated in order to reduce compilation time.

[Lua]: https://www.lua.org/
[Rust]: https://www.rust-lang.org/
[crates-and-modules]: https://doc.rust-lang.org/book/crates-and-modules.html
[VS]: https://www.visualstudio.com/
[VSCode]: https://code.visualstudio.com/

