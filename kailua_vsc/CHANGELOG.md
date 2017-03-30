## 0.0.16

* Table constructors can be inferred as a vector, a map or `table` when the sufficient type hint is given. In particular it is now possible to create a vector or a map with `local x = {[k] = v} --: map<..., ...>` declaration. `#` also hints its operand.

* Fixed an invalid resolution of types in the declarations affected by `--: module`. This has a side effect that type scopes are more forgiving than ever (e.g. the declarations can refer to the types declared later in the same scope), but this is not intended and can be fixed later.

* Greatly improved the behavior of field completion; it now tries to use the previous output, then retry with the current output when the previous output cannot be used.

* Added support for hovering type informations.

## 0.0.15

* Made `require "foo.bar"` check *both* `foo/bar.lua` and `foo.bar.lua` (forgot to process both, oops).

## 0.0.14

* Delayed type checking via the `module` keyword is now supported. Traditionally Kailua didn't support mutually recursive functions, mainly because it checks everything in the lexical order---which is inevitable for dynamically typed languages like Lua. `module` acts as an escape hatch: when some variable or field is declared as `--: module` or `--: module <type>`, every function or method declaration through it is collected but not immediately type-checked. Instead, they are checked at the end of the scope where `--: module` appears, at the time declarations are guaranteed to exist.

    * Assignments are not affected by the `module` keyword. `function M.a() end` is eligible for the delayed type checking but `M.a = function() end` is not. This may change in the future.

    * Due to the delayed type checking, declarations to `--: module` variable or field should be fully specified much like the `[NO_CHECK]` attribute.

    * In spite of the delayed type checking, every local name in the declaration is resolved at its initial position, not at the end of the scope. Global names *are* resolved at the end of the scope.

    * It is possible to declare mutually recursive functions throughout *different* `--: module` variables or fields. They should be declared as `--: module` in the same scope to be correctly checked, however.

* Function signatures that never return (i.e. diverging) are now supported. A special return type `!` can be used to declare that the function should not return.

    * Diverging function calls inside an expression are unconditional warnings, because they are almost always accidental.

* Modifier-only specifications like `--: const` are now allowed (a side effect of having `--: module`). `local x = 3 --: const` is now identical to `local x = 3 --: const integer`.

* Improved warnings on wrong or duplicate attributes. (Triggered from a prior renaming from `[no_check]` to `[NO_CHECK]`.)

* `require "foo.bar"` now correctly opens `foo/bar.lua` (and so on) instead of `foo.bar.lua`.

* Fixed a parser crash on the incomplete functions or records with named arguments or fields.

* Syntax highlighting has been greatly improved to catch up with Kailua-specific syntaxes.

## 0.0.13

* The function type declaration has been changed. Previously `--v function(self, ...)` (without a type for `self`) is allowed for methods; this is now `--v method(...)` and no type for `self` can be declared. As before, `--v function(...)` should be used for functions and `--v method(...)` should be used for methods; since the only difference between function and method declaration is the implied type for `self`, you can freely use a function declaration if you *do* need to specify the type for `self`.

* `--# assume C.f: method(...) --> ...` is now accepted as a special form equivalent to `--# assume static C.f: function(self: Self, ...) --> ...` where `Self` is an inferred self type. This draws a parallel to the method declaration syntax.

* Function types can now have optional argument names like `function(x: T, y: U)`. This is purely for the documentation purpose and does not affect the type checking. The names, if present, should be present for all non-variadic arguments, and should be unique to each other (though this only results in an error and doesn't stop the checking). Function specifications are automatically named, improving error messages.

* Renamed `[no_check]` to `[NO_CHECK]` to signify that this is not a normally desirable code (the same rationale applies to `WHATEVER`).

* In explicit types `{}` is (correctly) no longer extensible. `{...}` remains extensible. This change would make `{}` close to useless, which *is* intentional for consistency.

* A field completion now works correctly when the field name has been partially typed.

## 0.0.12

* Experimentally separated an inextensible record type (`{a: T, b: U}`) from an extensible record type (`{a: T, b: U, ...}`). The checker already had this distinction (for example, assigning an extensible record type to the mapping will make it inextensible, because from that point adding a non-conforming key will break the type check!) but this change makes this explicit. The bare expression `{}` still generates an extensible record type (the distinction is only for Kailua blocks), and not all error messages have been changed as this can be reverted later.

* Removed a remaining special treatment for `assert` (mistakenly left after the abandonment).

* Reworded and partially simplified several error messages.

* Fixed a couple of edge case bugs.

## 0.0.11

* Function arguments and returns are now checked for the arity mismatch. Previously omitted arguments and returns are treated as (noisy) nils that can be matched against other nils, but this is now forbidden. Use an explicit `T?` to allow the omission.

* Fixed a stack overflow when the recursive records are built and diagnostic needs to show those types.

* `pairs` and `ipairs` now works correctly against `WHATEVER` arguments.

## 0.0.10

* The record types in the function arguments and returns are no longer affected by later calls.

* Non-existent fields no longer count as nil when records need to be equal to or a subtype of other records. In the other words, you *can* supply additional fields to a function accepting a known record type, but you cannot omit non-`T?` fields in them. This change in particular affects function calls, disjunctive implicit unions (`a or b`) and assignments.

* `--# assume` has been (again) revised.

    * `--# assume global x.y: T` is gone (again), and `--# assume x.y: T` will always alter `x` in place: the change is visible wherever `x` is visible.

    * `--# assume C.x: T` when `C` is a class prototype is allowed. This actually sets a field to the class *instance* of `C`---that is, same to set `self.x` to given type in the constructor. One can use `--# assume static C.x: T` to set a field to the class *prototype*. For now, adding a method requires a verbosity like `--# assume static C.method: function(C, ...) -> ...`; this will be shortened in the future.

    * `--# assume x.y: T` where `x` is an explicitly nilable type (like `{}?`) is now correctly disallowed.

* Function calls can hint the return types of anonymous functions given as arguments. Previously this was only done with anonymous functions' arguments.

* `[no_check]` is now checked after function type hinting, so it can be used to omit hintable function types.

* Fields completion now correctly gives class and instance methods.

* Three keywords `static`, `class` and `method` have been added to the Kailua-only meta block. You need to use backquotes (like `` `class` ``) to use them as normal names.

## 0.0.9

* Method calls now correctly give hints to the arguments, so calls like `o:f(function(a, b, c) ... end)` will correctly infer the anonymous function type whenever possible.

* Record type syntax has been changed from `{a = T, b = U}` to `{a: T, b: U}`.

* Union of any table type `T` and records that is `T`'s subtype is now `T`. It was previously an error, and forbade codes like `x or {}` (with any table `x`) or `x or {3, 4, 5}` (with an integer vector `x`).

## 0.0.8

* Top-level functions defined in the internal definitions (as loaded by `--# open`) now return `T` instead of `T?`, even when nil is expected. There are some cases that the explicit check for nil is beneficial in general, though, and some of them can be changed later when using `T?` becomes easier.

* C++-like `int` and `bool` are now supported as aliases to `integer` and `boolean`. The longer names still remain as canonical because that's what Lua 5.2 and later uses.

* Some compromises in the built-in definitions.

    * `math.floor` and `math.ceil` now returns integers. (This is technically incorrect as they can return NaN for NaN.)

    * `string.match` and `string.find` now always returns strings. (This is technically incorrect as they can return integers when the pattern includes `()`; use `--# assume` if you want.)

## 0.0.7

* `--# type local` and `--# type global` has been added (they are respectively similar to `--# assume local` and `--# assume global` except that they don't allow shadowing). The old `--# type` is now solely used for exporting types, so that a `require` call can now import types (again, no shadowing is allowed). Redefinition of existing types is forbidden except for two cases, `--# type T = T` and `--# type global T = T` where `T` is a visible local type.

## 0.0.6

* No implicit function types are now disabled by default; the only allowed cases (for now, subject to expansion) are anonymous functions in the function call of the known argument types and the assignment to a slot with the known type.

* Associated diagnostics are now displayed altogether with the base diagnostic (as VS Code always destroy the diagnostics ordering).

* Reverted `--# assume` syntax, renaming `--# assume local` to `--# assume` (almost same to the previous semantics but always create a local binding). `--# assume global` works as is.

* Fixed a faulty error when `WHATEVER` etc. is used as comparison operands.

## 0.0.5

* Fixed various erratic behavior of record types.

* Fixed several parser errors that completly disable the language server.

* Correctly reports an error when the start path itself is missing.

* `--# assume` syntax has been changed; it now creates a new local binding _only when the local variable of the same name exists_, otherwise it creates a global variable. `--# assume local` and `--# assume global` can be used to force either case and error on others.

## 0.0.4

* `--# assume x.y: T` syntax is now supported.

* `true` and `false` now coerces to `boolean` unless explicitly typed.

* `true | false` no longer translates to `boolean`, it's now forbidden.

* Fixed a misleading error for bad binary operators.

## 0.0.3

* The error reporting for type errors has been vastly improved.

* Fixed a crash when a function never returns and hasn't explicitly set its return type.

## 0.0.2

* Fully variadic return types (e.g. from `string.match`) no longer count as always-true conditions.

## 0.0.1

* Language updates.

## 0.0.0

* Initial public release.
