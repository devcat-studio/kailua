-- Regression tests arising from past bugs in the Kailua parser.

--8<-- regression-rettype-nesting
-- cargo-fuzz trophy case #2
--> while --@< Error: Expected a statement, got `-->`
--&
--! [Oops]

--8<-- regression-integer-lit-0x
-- cargo-fuzz trophy case #3
local x = 0x 3 --@< Error: Invalid number
--&
--! [Local([`x`$1], [3])$1]

--8<-- regression-two-meta
-- cargo-fuzz trophy case #8
--v function --@<-v Error: Expected `(`, got a newline
--: {f       --@< Error: Expected a statement, got `--:`
=            --@< Error: Expected a statement, got `=`
--! [Oops, Oops]

