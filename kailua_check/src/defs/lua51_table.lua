-- definitions for Lua 5.1 table library

--# -- TODO lacks genericity (yet)
--# assume global `table`:
--#     {
--#         `concat` = function({const string|number}, string?, integer?, integer?) -> string;
--#         -- TODO ah fuck, needs overloading with function({var any}, integer, any)
--#         `insert` = function({var any}, any);
--#         `maxn` = function({const any}) -> integer;
--#         `remove` = function({var any}, integer?) -> any?;
--#         `sort` = function({var any}, (function(WHATEVER, WHATEVER) -> boolean)?);
--#     }

