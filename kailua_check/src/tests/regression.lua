-- Regression tests arising from past bugs in the Kailua type checker.

--8<-- regression-kind-map-func-array
-- used to cause Slot::assert_eq deadlock
local foo = {} --: map<string, function(vector<string>) --> ()>
--! ok

--8<-- regression-assign-multi-missing-init
-- cargo-fuzz trophy case #1
a,b; --@< Error: Expected `=`, got `;`
--! ok

--8<-- regression-recovering-recursive-rec-assign
-- cargo-fuzz trophy case #5
u = {u = 0}
u = {u = u} --@< Error: Cannot assign `{u: <variable u>, ...}` into `{u: 0, ...}`
            --@^ Note: The other type originates here
--! error

--8<-- regression-recovering-recursive-tuple-assign
-- cargo-fuzz trophy case #6
u = {}
u = {u}
u = {0} --@< Error: Cannot assign `{0, ...}` into `{<variable u>, ...}`
        --@^ Note: The other type originates here
--! error

-->8-- regression-recursive-tuple-tuple
-- cargo-fuzz trophy case #7
u = {}
u = {u}
u = {{u}}
--! error

-->8-- regression-recursive-broken-invariant
-- cargo-fuzz trophy case #9
u = {}
u.u = e{}
u.u = #u.u
a = u.u - 1
--! error

