-- definitions for Lua 5.1 table library

--# -- TODO lacks genericity (yet)
--# assume global `table`:
--#     {
--#         `concat` = function(vector<const string|number>, string?,
--#                             integer?, integer?) --> string;
--#         -- TODO ah fuck, needs overloading with function(vector<any>, integer, any)
--#         `insert` = function(vector<any>, any);
--#         `maxn` = function(vector<const any>) --> integer;
--#         `remove` = function(vector<any>, integer?) --> any?;
--#         `sort` = function(vector<any>, (function(WHATEVER, WHATEVER) --> boolean)?);
--#     }

