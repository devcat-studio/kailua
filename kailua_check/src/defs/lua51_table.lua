-- definitions for Lua 5.1 table library

--# -- TODO lacks genericity (yet)
--# assume global `table`:
--#     {
--#         `concat`: function(table: vector<const string|number>, sep: string?,
--#                            i: integer?, j: integer?) --> string;
--#         -- TODO ah fuck, needs overloading with
--#         --      function(table: vector<WHATEVER>, pos: integer, value: any)
--#         `insert`: function(table: vector<WHATEVER>, value: any);
--#         `maxn`: function(table: vector<const any>) --> integer;
--#         `remove`: function(table: vector<WHATEVER>, pos: integer?) --> any;
--#         `sort`: function(table: vector<WHATEVER>,
--#                          comp: (function(WHATEVER, WHATEVER) --> boolean)?);
--#         ...
--#     }

