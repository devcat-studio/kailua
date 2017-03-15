-- definitions for Lua 5.1 table library

--# -- TODO lacks genericity (yet)
--# assume global `table`:
--#     {
--#         `concat`: function(vector<const string|number>, string?,
--#                            integer?, integer?) --> string;
--#         -- TODO ah fuck, needs overloading with function(vector<WHATEVER>, integer, any)
--#         `insert`: function(vector<WHATEVER>, any);
--#         `maxn`: function(vector<const any>) --> integer;
--#         `remove`: function(vector<WHATEVER>, integer?) --> any;
--#         `sort`: function(vector<WHATEVER>, (function(WHATEVER, WHATEVER) --> boolean)?);
--#     }

