# 🌴 Kailua

<!--
this file is intended for VS Code extension and should not be edit alone.

checklist:
* remove "Standalone Checker" section and rename "Installation and Usage" to "Usage"
* remove "Source Organization" section
* convert all images into the data URIs
-->

<!--[한국어](https://github.com/devcat-studio/kailua/tree/master/README.ko.md) currently not reachable-->

**Kailua** is an experimental type checker and integrated development environment (IDE) for the [Lua] programming language (currently only Lua 5.1 is supported).

***THIS IS VERY EXPERIMENTAL PROJECT AND NO WARRANTY OR SUPPORT IS PROVIDED!***

## Usage

**Caution: Currently the extension only works in Windows due to the packaging issue.**

Kailua can be used as an IDE support for [Visual Studio Code][VSCode]. Install Kailua by typing `ext install kailua` from the Quick Launch (`Ctrl-P`).

You will see a warning that the configuration file is missing when you open a folder containing Lua codes. You need it for real-time checking.

![](data:image/png;base64,
iVBORw0KGgoAAAANSUhEUgAAAlsAAAAuCAMAAAAoaywYAAAAAXNSR0IArs4c6QAAAARnQU1BAACx
jwv8YQUAAAMAUExURQICAgMEAwQEBAUHBQYGBggICAgLCAoKCgwMDA4ODgwQDA8VDxkVDxAQEBIS
EhQUFBUVFhYWFhMaExgYGBoaGhsbHBwcHBwcHR0dHh4eHh4eHw8VMx8fICAgICAgISEhIiIiIiIi
IyMjJCQkJCQkJSUlJiYmJigoKCoqKi0tLS4uLjAwMDIyMjMzMzQ0NDY2Njs7Oz09PRMaWhAsQEUq
DH8Af25DE25bOkZGRkxMTE9PT09XW1FRUVdXV1tbVlpaWl5eXlpyeGJiYmZmZmhoaGpqam1tbW5u
bnBwcHNzc3V1dXZ2dnh4eHt7e319fX9/fw5jnBpqoSZypDJ5qT2ArUaGsUiHsUyJslGNtVWPtliR
t1uSuF2UuV+WunqSimKXu2SZvGeavGibvGudvm2ev3CgwHajwnmlw3unxH+pxbiVAL2eGsCiJMOo
MsOoNMWqOcasPsetQcu0VMy1V8y2Wc23Ws23XM66Y8+7ZdC9bNG+btK/ctPCd9PCeYCAgIKCgoSE
hIaGhoiIiIqKioyMjI6OjpCQkJKSkpSUlJaWlpiYmJqampycnJ+fn6CgoKOjo6SkpKenp6ioqKqq
qqysrK6urrCwsLKysrS0tLa2tri4uLq6ury8vL6+voCqxoWtyIeuyImvyZG0zJW2zaC+0qnD1arE
1a/H17HI2LTK2bjM2rvO27/R3bvw8NbGg9fIiNfIitjLkdnLktnMlNrOmNzQnd/Vqt/VrN/Wr+DW
sODXsuHYs+HauOPbvePcv/Dwu8HBwcLCwsTExMbGxsjIyMvLy8zMzM7OzsLT3sTU39DQ0NLS0tTU
1NbW1tjY2Nra2tzc3N7e3sbV4MjW4MvY4c3Z4s7a4tDc49Hc5NTe5Nfg5djg5t7k6OXexOXfxuXg
yebhy+bhzOfj0Ofj0+jk1Ojl1unm2ero3ero3uDg4OLi4uTk5Obm5uLn6ePn6uTo6uro4Ovp4uzq
5ezr5+jo6Orq6unq7Ovs7e3s6O3s6u3s7O3t7u7u7e7u7vDw8AAAABkAi2kAAAEAdFJOU///////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
/////////////////////////////wBT9wclAAAACXBIWXMAAA7DAAAOwwHHb6hkAAAAGXRFWHRT
b2Z0d2FyZQBwYWludC5uZXQgNC4wLjEzNANbegAAC2hJREFUeF7tmwt8U1cdxxFBYJN2pR0MHOWx
DhC5267PUDvTpQHahdIymE98fCZMrVOr022iQmkKoZS2sbSQ0BR1KIJOnOKDKWJD096Zxw2xE1c3
H4xuI2l0BRtWgteP/3POP682CZdC+dRxvh+ae57/8/+f88s55+bzYdx/41iOYDYF2ChpM6yJq/v9
e36CKc4NRoK2OJxrCNcWZ7Tg2uKMFuPuBPLyBA4nJeVXSl4ekRXRVl7efDTC4SQBFaOe+VRcoC2Q
1jw0wuEkARWjnnlUXOOItObmohEOJwmoGPXkziXiAm3Nn5c7C41wOElAxahnVi7sXHeOg21rzqwZ
aITDSQIqRj0zZs2BjQu0NS/3thw0wuEkARWjnpzbcucRbc2fd/v0bDTC4SQBFaOe7Om3w6EI2poz
69YsNAJUqACbcm4QUDHqybqVHIpUWzlcW5w0oGLUk5UT09YtaARA+aQFm3JuEFAx6rklTluZaARA
+aQFm3JuEFAx6snk2uKoAxWjnmukraKOUNi5HDNXilkin02OAppLic5twpQg6F1NsgHTQKfVEJ9l
WDsxEY/etZ08arwlNBvF5NZhKiVJ7alBMmMiChttmBPAkDISc6cVMxQ0lmCTTaBQH5QLaSIB1rLE
W0NzKUnmTCKoGMID3/+n8uK+8u79mE9OGm0dvvjXisre8NMVR5RfUjENAZsS9B6bXtNYh7lkbJFS
rx2bGnHvsCVIJF5bDR2rEsWURFvJKXMMc0TbuU2NtoZQ58BEXCo5qbSlgviYGam1Veiup7k47NBq
+OgjBBUDrD54av3qTx+4Cm19rb+3cuN55WjFsfMbqZiGgE0J7XYRU6lIN5v4tbsccfMsOmqHiEm1
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
SUVORK5CYII=)

You can either create `.vscode/kailua.json` by hand, or search "Kailua" from the Command Palette (`Ctrl-Shift-P`) to edit one.

![](data:image/png;base64,
iVBORw0KGgoAAAANSUhEUgAAAmoAAABJCAMAAAC6q/r8AAAAAXNSR0IArs4c6QAAAARnQU1BAACx
jwv8YQUAAAMAUExURQICAgICBQIEBQUCAgUDBAUFAgUFBQICCAIECgIGDAYGCQcIBgMICgMJDQYI
CQQKDQYMDQoEAwkHCAsKAwkIBQ4PAwwOBgkJCQoLDAgMDAwMCA0NDQQEEgQLEwQOGwsMEQ0NGQ4R
DQURGgoVGw4YGhEGAxUMBRMOCxkGBh4KBh0PDxQSBhQQCxgaBhAQEBMTFBEUExEWFhUVFRAYHBwW
FRwYFhkZGRsbHB0dHQYGIQYRJAYaKgseKxERIR8fIBsgGQwhLRQkLh0kIx0nKh4oJhsqKxYmMR0p
MSEPCCUTCSgWCygZDiETESQbEygbFjAQCSEhIiMjJCQkJSUpKSwsLDUsIDExMTU1NTA4NDw8PAY2
VQY3WAY4WQY7XgY8Xwk4VhAzSRA9WxM/XAY8YQY+ZRE2cBVAXRhDXwZAZwZDbAVFbwVGcQVHdQVI
dQVLeAVOfxdIaBdKbB9IYyFKZSZOaChPaSpRai1TbC9VbTBVbjRYcDhbczlccztddDxedUMWDVgd
EUs/LWJSO0NDQ0VFRUNPSUhISE9PT09eSFFRUVRUVFlZWV1dXUBieEZme0hofExrf1hoYGBgYGZm
ZmpqamxsbHFxcXl5eXx8fARRhQRUigRYjwRYkQRblgRclx5eiANgnwNkpgNnqANoqwNrsQNsswJv
twJvuAJwuQJ0vwJ1wgJ3xQJ4xgJ4yAF8zAF+0U5sgE98j1JvglVxhFl0hlx3iVh8j114iWR9jQGA
1AGC2AGE2gGI4QCK5gCM6QCO7QCR8QCT9QCV9wCW+myDkm6Ek2+FlHCGlHOIlnSJl3qOmn2QnIWF
hYuLi42NjZGRkZeXl5mZmZ+fn4OUoIWWoYmZo4uapIybpY6cppCep5GfqJSiqpekrJmlrZyor52o
sKCgoKWlpampqa6urqCrsaKssqSttKewtamxtquzuKyzuK20ubGxsbW1tbC2u7K4vLS6vbe8v7m5
ub6+vrm9wLzAwr/CxMDAwMDDxMLExsXGxsbHyMnKyszMzPDw8AAAAOnUwb8AAAEAdFJOU///////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
/////////////////////////////wBT9wclAAAACXBIWXMAAA7DAAAOwwHHb6hkAAAAGXRFWHRT
b2Z0d2FyZQBwYWludC5uZXQgNC4wLjEzNANbegAACG5JREFUeF7tnA10U2cZx1PrLqxHgSjaaelw
BdqhlY/K2mxTppssMk0snTp7pVZE6XQWlXYgdYtTN3AttZMN6urUlfmByKSClS+7Fi4IA4Y2Vhbs
gNpCYGvT3LXpWsKtx+f9yO1NmqRpd3NPzs7zOyXv837dcE5+53/f257W9D8EMQRUDTEIUwGCGEJA
tWW8RRBdGRGLq7Ysn7UIoi/5qmtMtWX5qBoSF/JV16hqYJqF9hBEZyyqa1w1C6qGxAWLJUg1CLVc
2iv4LoLoBDMqF2KNVUS1ZfmLVdUKEUQXVNUW81hjqlnycmgPVUP0gquWkxe4g/JUy0XVEH0JqJaL
qYbEF0w1xCAipRqqhuiMqhqmGhJfxpdqzi5eEE57Hg/qI0g0xpdqoWqhakjMvIlUA1A1JGZiSrXK
rlN0MVHr54OnNv7Xr/RUkQ581XoqCys9tY+2+pW+J9kqBAlDbKm2qbN/K2mdXU/1ny7c3LGpqq81
WLVK55aNPZ10MYKEI9az2pMeD2SW09PnZP0OZ7BqZOwlvJ0ikYn1rPb4PwdPg2pK/+bCwg2t/f7h
UNVO9/sVVA2JTGypVtkx+I8N0Dp7uro2FLZ6tlY5tapV9dWe6qvdhKmGRCGmVKvsoaKRs1pVX9eG
zs4fb4EbKVNty+BLj3b4a1s9Gzf1oGpIZGI9qzFAracGnVv7FU9nQLVCePTs8NRu9ij9pIcgEYj1
rIYgb5LxpRqCTBhMNcQgMNUQg8BUQwwCUw0xiDFTDUF0ghkVMdUQRF8iphqC6AumGmIQmGqIQWhS
7T7gC58rwL9EhMSFeTm5uRaK6b5Cqhr+KT8kLoBqzDRQjaeaiCBxQJtqTLXFfAZBdEWTanD/RNWQ
uIGphhgEntUQg9CmGn8C5TMIoiuYaohBRD+r1Uv14soD0jbefeCFgyvrpB28FxmH11H9+h7eiYFq
WTl29iLvRCLaFdd6/+7wNfIOkphoUi3MEyhRbZu0/8u8G1W148PAcVqCaqRpP0N7WnZ3K4rcwDsq
L19cwasINPTSC4ZBfVdULdEZM9VKm5tX8x4lsmpuXqiquV20p+HoGyfXrDhykvdUXKMWhtDoi6ha
4F1RtUQn+lmtXnpuJwSbWLS9RXrhGXF1y/4iUO3+ppbV4g6pTvxVk3Tod0VrXj1B1vIPvfyCcu2s
7Gj07fEODw8RQ1a1XVXkajL3kLyvmLTiT72KcqEcDPqXXzlT7CYLwUuy1eV1UFHdx0X3RdnreOWa
cnkdiS4X0S14H7kSe1fwjKhWdg4uUEIGkMRDm2qjn0DrpQOHye3z/iZJkpq/FaraDhiVng1Wrf3y
2vJLb4BqjkCqPXi2psx9nlRHeh+kIxWyq6TiSrvY6G8v2zewm6YaLOZbVdUGnhAf/k/FermNphr8
C90HBKkGF6j2HiEDSOIxVqrtamkuhWL5N39xUKoLVU382tPb1fspiZ4hsAHuYzUk1VTVCMwIbiNT
bq93faOvWlzTfZyrFtiqqvYKXVwMs1y10H0AeddrxDP4qvDCg4OL7UISjrHOatu2S7uKxNKmpj80
jVKt6I8tz+8aUY3lC/EEXjSqFb8oX1Xo5LHuNXSELuWLiFRMtcBW2hLVYLSkzecfVlUL3Re4FE81
xxB5RqADSOKhSbWwT6DbvtEEh7XfS8+s3K+qVrT/cB3pPn145/LnQlWTIVn2+LSqHZH/UvEinazx
7WUj3WU8nYJUk+GWCFsdcLBb9SpTre21hu9pUi1kHxCkmlxDOkhiMlaq1Yu/lJpLfyNJh1pU1b70
vCQdPiTVffsQHOCkHUFntVVud/naSwNUtfPn6Mm9rbus4jKdLHYNHS0pOXryodfPwJmLGTSi2qpL
bOs6+d9fPaEw1c6fW1Eju8TdvifI4tB9gFa14gvuctJDEpKxzmr14vI/SbseOCj9dqeqmlj6N9pd
vl068OyIauTu5RIdvcrQSXaP23t14GGY+H6v4nNRI8TiEwOKcsUhNsj0UTFYNdHxmjJwAu6ex64q
564w1Rp8Su95l1hyUXmZLA7ZB2hVE9ddVhTfqO/aIYmBNtV0+xko+45GjAQv3uddz6uxiPAm43pv
xECip5rh1MjtvELeakQ/qxlLuzLsb4eDP/KWRJNqBfyX8/gMguiKJtWoaotRNSQ+hEk18otUCKI3
YVKNTSCIvmCqIQaBqYYYBKYaYhCYaohBjEq1vJx5aamCYJooE98ZQtLU9yWbTNenknrytOkm09Tp
pslkiDBp2nW0hTEN74TFMDNpGrSkTppKV5FrBDZEJmm6mVdmMykFczKMJb/NnCyYTTDAxsg8qUZW
q9ARsgJ2aVYjgCCkpo1Ktbyc+Wmp4FoY+LZoCKm6qTYpYyF8VOmz3w81vE7O+MhN0PJPb8bs99A2
Y6HWoIwF110/a2HyjNk3TUqftQA+81kLyPSNC5Nhw7vZGkJ6TjavNCRlZbKL35BlnpJlNqVlmoWs
7DnvyDKnZSZlz4EvU0pWJlkI1chq05TsTOo/HTalZtNdSdmZb38XXY0Q0YRUqhozLaAaHNYg1iLI
NiZp8wgT3KzhdqvVelc2FDM+brXeayavd959h3DbEjObv9X6YdreeDdM0IoAq6x3vldI+ajVuuQT
ZPg2GLhDuP2TZuHWJR9iawgZ99p4pWXKIrvdvlSYaZsrCLfcY77BbrN/xnZz+qfnLFqaYpubYoPZ
TwlCOjR2+9zAakGYaV8K/6nAcMotdvsHbTcL821226J72IURZlpQqhXkk+eC+fPSwgICJjZf+SIv
4sDMz3+WV8gEgAxSTWOqwWENXAPZwsGNS1y+8xgv4sAHfvgDXiHjB+wZCbWAauBaHsgWDiZc4vH1
P/+V8pNf/4yPxIGP/egRXiETYcQ0i+X/OwapXJl3aMkAAAAASUVORK5CYII=)

The following content is required for `.vscode/kailua.json`, in case you are editing it by hand:

```json
{
    // Unlike a normal JSON, a comment or a stray comma is allowed.
    "start_path": "<path to the entry point>",

    // You can also put the following:
    //
    //"package_path": "<the value of `package.path`, determined from assignments if missing>",
    //"package_cpath": "<the value of `package.cpath`, determined from assignments if missing>",
}
```

You need to reload the current window (`Ctrl-R` or `Cmd-R`) to apply the configuration.

Once you've set the entry point, you can write your first Kailua code:

```lua
--# open lua51
print('Hello, world!')
```

Play a bit with this code to see which errors Kailua can detect.

### Supported IDE Functions

* Real-time syntax checking (for all files).

* Real-time type checking, starting from given start path.

* Auto-completions for names and fields.

* Help for function signatures.

* Help for types of most subexpressions on hover.

* Go to definition for local and global names.

* Mass renaming of local and global names.

## Kailua the Language

### Special Comments

Kailua is a subset of valid Lua code---you don't need any transpilation or compilation. The additional annotations are described in special comments:

* `--: <type>` describes the type(s) of preceding item(s).

  It is valid anywhere a new name can be possibly defined: `local` (either individual names or statement), `function` (arguments), `for` (individual names) and assignment (either individual names or statement).

  When used right after the name, for the ease of typing, you can put a comma or closing parenthesis *before* the type like this:

  ```lua
  function f(x, --: integer
             y, --: integer
             z) --: integer
      -- ...
  end
  ```

  For the common case of defining multiple names you can put types after the statement. In this case types are delimited by commas.

* `--> <type>` describes the type(s) of function returns.

  It is valid only after the closing parenthesis of function arguments. It is valid to put `--:` (for the last argument) and `-->` in the same line.

* `--v function(<name>: <type> ...) [--> <type>]` describes a function type.

  It is valid before the `function` keyword (yes, also for anonymous functions). This is equivalent to `--:` and `-->`, but much more readable. All names should be identical to the corresponding declarations. Variadic arguments can be written as `...: <type>` at the end of arguments. Multiple returns need parentheses.

  The general rule of the thumb is that all functions have to be typed with either `--v` or `--:/-->` unless it is obvious from the preceding context. This allows you to write a code like `f(function(a, b) ... end)`, but only when `f` is known to accept such a function.

* `--v method(<name>: <type> ...) [--> <type>]` describes a method type.

  It is same to `function`, but for declarations like `function A:b(...)`. Kailua tries to infer the type of `self`, and if it's not possible you should use `function A.b(self, ...)` and `--v function(...)` instead for the clarity.

* `--# ...` is a special directive for the type checker.

  The most important directive is `--# open <built-in library name>`, which loads the corresponding built-in names and also implicitly specifies what language variant is currently in use. The only supported name so far is `lua51`, for the vanilla Lua 5.1. It is recommended to put it to the first non-comment line in the entry point.

  `--# type [local | global] <name> = <type>` can be used to declare a type alias. There are three flavors of typa alises: `local` is locally scoped (much like `local` statements), `global` is globally scoped (much like `A = ...`), and no modifier indicates that the type is *exported* from the current file and they should be locally visible after `require`. Only local types can be in the inner scopes. Unlike variable names, inner type names should not overwrite outer names.

  `--# assume [global] <name>: <type>` *overrides* the type for given name. The `global` keyword forces the global assignment, otherwise a new scope is created like `local` statements. It is useful for sidestepping the checker issue, but it is also highly unsafe. **Use at your own risk.**

  More directives are likely to come.

The equal kind of special comments can span multiple lines.

```lua
--# type Date = {
--#     hour: integer;
--#     min: integer;
--#     sec: integer;
--# }
```

### Types

The following basic types are recognized:

* `nil`, `boolean` (or `bool`), `number`, `string`, `function`, `userdata`, `thread`, `table` for primitive Lua types.

* `integer` (or `int`) for a check-time integral subset of `number`. (In the future, in the Lua 5.3 mode or later, it will be also recognized as primitive.)

* `true` or `false`, integer and string literals are valid subtypes of `boolean`, `integer` and `string`, respectively.

* The table type is divided into four useful cases.

  Importantly, first two cases are not automatically inferred from the use and should be explicitly annotated like `local tab = {} --: vector<integer>`.

  * `vector<T>` for a table with consecutive integer keys.

  * `map<Key, Value>` for a homogeneous associative table.

  * `{ key1: T1, key2: T2 }` for records, whose keys are strings and fixed at the check time. You can use semicolons in place of commas.

    Explicitly declared records are "inextensible" by default, meaning that the list of fields is complete and cannot be altered. You can make it extensible by putting `...` at the end of fields; this allows a lazy initialization of records like `table.field = 'string'`. On the other hands, a normal Lua table is implicitly typed as extensible records, only made inextensible when required.

  * `{ T1, T2, T3 }` for tuples, whose keys are consecutive integers. Otherwise they are similar to records.

* `function(Arg, ...)` or `function(Arg, ...) --> Ret` for functions. `Ret` can be multiple types, in which case you need parentheses (`function(vector<T>, integer) --> (integer, string)`). Arguments can be named like `function(a: string, b: number)`.

* `T | T | ...` for union types. They are mostly useful for literal types (e.g. `"read" | "write" | "execute"`). Kailua has very limited support for checking other kinds of union types.

* `any` has no type information. `--# assume` is the only way to make it useful.

* `WHATEVER` (note the case) is a *hole* that the type checker always accepts. `map<integer, WHATEVER>` and `map<WHATEVER, string>` are compatible; `map<integer, WHATEVER>` and `map<string, string>` are not. As this thwarts the basic of type checking, **use at your own risk.**

The Kailua types are by default *not checked for `nil`*. That is, you can assign `nil` to `integer` but you can also add two `integer`s; the valid Kailua code can still result in a runtime error therefore. This restriction was required for making a practical type checker without changing the source programming language.

You can opt in two other `nil`-handling modes if you need to make it explicit. As they are (transitively) freely assignable, consider them more a machine-readable documentation.

* `T?` also accepts `nil` but it is aware that it can contain `nil`. Two `integer?`s cannot be added. It also allows for missing fields and missing arguments (they are not allowed otherwise): the type `{a: integer?, b: integer}` can contain either `{a = 42, b = 54}` or `{b = 54}`, but not `{a = 42}`.

* `T!` guarantees that it cannot contain `nil`.

Also, the table values are always `T` or `T?` (for the obvious reason).

Finally, types for the names and table values can optionally have a `const` prefix. You cannot modify the innard of `const` types: `map<integer, const vector<string>>`. You can still assign to them (otherwise this type won't be useful at all).

### Avoiding the type checker

As annotating everything is not practical, Kailua supports two ways to avoid the type checking with more localized guarantees:

* `--v [NO_CHECK] function(...)` disables the type checking for the following function.

  Kailua essentially *believes* the specified function type, which can be no longer omitted.

* You can override what file to check by having `.kailua` files.

  When `require()` was used with a check-time string Kailua makes use of `package.path` and `package.cpath` set. For `package.path`, it will try `F.kailua` first before reading a file `F`. For `package.cpath`, it will always `F.kailua` as `F` would be probably binary. (Note that this will normally result in two extensions `.lua.kailua` unless you have a sole `?` in the search paths.)

  `.kailua` files would frequently use `--# assume` as you should *assume* that the original code has given types.

<!-- -->

[Lua]: https://www.lua.org/
[Rust]: https://www.rust-lang.org/
[install Rust]: https://www.rust-lang.org/install.html
[crates-and-modules]: https://doc.rust-lang.org/book/crates-and-modules.html
[VS]: https://www.visualstudio.com/
[VSCode]: https://code.visualstudio.com/

