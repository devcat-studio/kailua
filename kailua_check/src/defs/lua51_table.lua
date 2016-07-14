-- definitions for Lua 5.1 table library

--# -- TODO lacks genericity (yet)
--# assume global `table`:
--#     var {
--#         `concat` = var function({const string|number}, string?, integer?, integer?) -> string;
--#         -- TODO ah fuck, needs overloading with function({var any}, integer, any)
--#         `insert` = var function({var any}, any);
--#         `maxn` = var function({const any}) -> integer;
--#         `remove` = var function({var any}, integer?) -> any?;
--#         `sort` = var function({var any}, (function(WHATEVER, WHATEVER) -> boolean)?);
--#     }

