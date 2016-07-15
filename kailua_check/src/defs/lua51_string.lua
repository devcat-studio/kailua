-- definitions for Lua 5.1 string library

--# assume global `string`:
--#     var [string_meta] {
--#         `byte` = var function(string, integer?, integer?) -> (integer...);
--#         `char` = var function(integer...) -> string;
--#         `dump` = var function(function) -> string;
--#         `find` = var function(string, string, integer?, boolean?) ->
--#                               (integer, integer, string|integer...);
--#         `format` = var function(string, any...) -> string;
--#         `gmatch` = var function(string, string) -> function() -> string?;
--#         -- TODO have to constrain the function argument, but not easy
--#         `gsub` = var function(string, string,
--#                               string | { [string] = string } | (function(WHATEVER...) -> string),
--#                               integer?) -> string;
--#         `len` = var function(string) -> integer;
--#         `lower` = var function(string) -> string;
--#         `match` = var function(string, string, integer?) -> (string|integer...);
--#         `rep` = var function(string, integer) -> string;
--#         `reverse` = var function(string) -> string;
--#         `sub` = var function(string, integer, integer?) -> string;
--#         `upper` = var function(string) -> string;
--#     }

