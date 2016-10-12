-- definitions for Lua 5.1 os library

--# type tm = {
--#     year = integer, month = integer, day = integer,
--#     hour = integer?, min = integer?, sec = integer?, isdst = boolean?
--# }
--#
--# assume global `os`:
--#     [currently] {
--#         `clock` = function() --> number;
--#         -- TODO it is very hard to recognize '*t'|'!*t' from other string types, ugh
--#         `date` = function(string?, tm?) --> string | {
--#             year = integer, month = integer, day = integer,
--#             hour = integer, min = integer, sec = integer,
--#             wday = integer, yday = integer, isdst = boolean
--#         };
--#         `difftime` = function(number, number) --> number;
--#         `execute` = function(string?) --> integer;
--#         -- TODO diverging function signature
--#         `exit` = function(integer?);
--#         `getenv` = function(string) --> string?;
--#         `remove` = function(string) --> (boolean?, string?);
--#         `rename` = function(string, string) --> (boolean?, string?);
--#         `setlocale` = function(string?, string?) --> string?;
--#         `time` = function(tm?) --> number;
--#         `tmpname` = function() --> string;
--#     }

