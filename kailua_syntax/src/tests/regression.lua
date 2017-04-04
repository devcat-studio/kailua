-- Regression tests arising from past bugs in the Kailua parser.

--8<-- regression-rettype-nesting
-- cargo-fuzz trophy case #2 (we probably need to special-case unexpected `-->` etcs, however)
--@vvvv Error: Expected a statement, got `-->`
--@vvv Error: Expected an expression, got a newline
--@vv Error: Expected a keyword `do`, got a newline
--@v Error: Expected a keyword `end`, got the end of file
--> while
--&
--! [Oops, While(Oops, [])]

--8<-- regression-integer-lit-0x
-- cargo-fuzz trophy case #3
local x = 0x 3 --@< Error: Invalid number
--&
--! [Local([`x`$1], [3])$1]

-->8-- regression-two-meta
-- cargo-fuzz trophy case #8
--v function
--: {f
=
--!

