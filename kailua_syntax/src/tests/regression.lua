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

--8<-- regression-nested-meta
-- cargo-fuzz trophy case #10
--: --: --@< Error: Expected a statement, got `--:`
        --@^ Error: Expected a statement, got `--:`
        --@^^-^ Error: Expected a statement, got a newline
--! [Oops, Oops, Oops]

--8<-- regression-meta-inside-braces
-- cargo-fuzz trophy case #11
--v function --@<-v Error: Expected `(`, got a newline
{ --: } {a --@< Error: Expected an expression, got `--:`
           --@^ Error: Expected `,`, `;` or `}`, got `--:`
           --@^^ Error: Only function calls are allowed as statement-level expressions
           --@^^^ Error: Expected `,`, `;` or `}`, got a newline
           --@^^^^ Error: Only function calls are allowed as statement-level expressions
--&
--! [Void({Oops}), Void({`a`_})]

