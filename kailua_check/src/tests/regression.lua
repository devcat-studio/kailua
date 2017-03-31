-- Regression tests arising from past bugs in the Kailua type checker.

--8<-- regression-kind-map-func-array
-- used to cause Slot::assert_eq deadlock
local foo = {} --: map<string, function(vector<string>) --> ()>
--! ok

--8<-- regression-assign-multi-missing-init
-- cargo-fuzz trophy case #1
a,b; --@< Error: Expected `=`, got `;`
--! ok

