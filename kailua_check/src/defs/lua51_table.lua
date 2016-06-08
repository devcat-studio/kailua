-- definitions for Lua 5.1 table library

--# -- TODO lacks genericity (yet)
--# assume global `table`:
--#     const {
--#         `concat` = const function({const string|number}, string?, integer?, integer?) -> string;
--#         -- TODO ah fuck, needs overloading with function({var any}, integer, any)
--#         `insert` = const function({var any}, any);
--#         `maxn` = const function({const any}) -> integer;
--#         `remove` = const function({var any}, integer?) -> any?;
--#         `sort` = const function({var any}, (function(?, ?) -> boolean)?);
--#     }

