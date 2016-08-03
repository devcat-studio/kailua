-- definitions for Lua 5.1 table library

--# -- TODO lacks genericity (yet)
--# assume global `table`:
--#     [currently] {
--#         `concat` = function({const string|number}, string?, integer?, integer?) -> string;
--#         -- TODO ah fuck, needs overloading with function({any}, integer, any)
--#         `insert` = function({any}, any);
--#         `maxn` = function({const any}) -> integer;
--#         `remove` = function({any}, integer?) -> any?;
--#         `sort` = function({any}, (function(WHATEVER, WHATEVER) -> boolean)?);
--#     }

