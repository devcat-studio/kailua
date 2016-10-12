-- definitions for Lua 5.1 string library

--# assume global `string`:
--#     -- [currently] should be first, it is specially handled only for `assume`
--#     [currently] ([string_meta] {
--#         `byte` = function(string, integer?, integer?) --> (integer...);
--#         `char` = function(integer...) --> string;
--#         `dump` = function(function) --> string;
--#         `find` = function(string, string, integer?, boolean?) -->
--#                           (integer?, integer?, string|integer...);
--#         `format` = function(string, any...) --> string;
--#         `gmatch` = function(string, string) --> function() --> string?;
--#         -- TODO have to constrain the function argument, but not easy
--#         `gsub` = function(string, string,
--#                           string | { [string] = string } | (function(WHATEVER...) --> string),
--#                           integer?) --> string;
--#         `len` = function(string) --> integer;
--#         `lower` = function(string) --> string;
--#         `match` = function(string, string, integer?) --> (string|integer...);
--#         `rep` = function(string, integer) --> string;
--#         `reverse` = function(string) --> string;
--#         `sub` = function(string, integer, integer?) --> string;
--#         `upper` = function(string) --> string;
--#     })

