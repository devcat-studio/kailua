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
