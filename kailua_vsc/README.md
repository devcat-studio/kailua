# 🌴 [Kailua][docs]

<!--
this file is intended for VS Code extension and should not be edit alone.

checklist:
* remove "Standalone Checker" section and rename "Installation and Usage" to "Usage"
* convert all images into the data URIs
* reword the non-Windows indications to refer to the repository
-->

**Kailua** is an experimental type checker and integrated development environment (IDE) for the [Lua] programming language (currently only Lua 5.1 is supported).

**The detailed documentation is available [here][docs].**

## Usage

Kailua can be used as an IDE support for [Visual Studio Code][VSCode]. Install Kailua by typing `ext install kailua` from the Quick Launch (`Ctrl-P`). **If you are not on Windows, you should also install the [standalone checker](https://github.com/devcat-studio/kailua/#standalone-checker) first.**

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
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)

The following content is required for `.vscode/kailua.json`, in case you are editing it by hand:

<!-- what Kailua really supports is not exactly JSON5, but probably it's closer than JSON. -->

```json5
{
    "start_path": "<path to the entry point>",

    "preload": {
        // This indicates that we are using Lua 5.1 and all built-in libraries of it.
        "open": ["lua51"],
    },
}
```

You need to reload the current window (`Ctrl-R` or `Cmd-R`) to apply the configuration.

Once you've set the entry point, you can write your first Kailua code (simple, eh?):

```lua
print('Hello, world!')
```

Play a bit with this code to see which errors Kailua can detect.

## Supported IDE Functions

* Real-time syntax checking (for all files).

* Real-time type checking, starting from given start path.

* Auto-completions for names and fields.

* Help for function signatures.

* Help for types of most subexpressions on hover.

* Go to definition for local and global names.

* Mass renaming of local and global names.

<!-- -->

[docs]: https://devcat-studio.github.io/kailua/
[Lua]: https://www.lua.org/
[Rust]: https://www.rust-lang.org/
[install Rust]: https://www.rust-lang.org/install.html
[crates-and-modules]: https://doc.rust-lang.org/book/crates-and-modules.html
[VS]: https://www.visualstudio.com/
[VSCode]: https://code.visualstudio.com/

