-- definitions for Lua 5.1 math library

--# assume global `math`:
--#     {
--#         `abs` = function(number) --> number;
--#         `acos` = function(number) --> number;
--#         `asin` = function(number) --> number;
--#         `atan` = function(number) --> number;
--#         `atan2` = function(number, number) --> number;
--#         `ceil` = function(number) --> integer;
--#         `cos` = function(number) --> number;
--#         `cosh` = function(number) --> number;
--#         `deg` = function(number) --> number;
--#         `exp` = function(number) --> number;
--#         `floor` = function(number) --> integer;
--#         `fmod` = function(number, number) --> number;
--#         `frexp` = function(number) --> (number, integer);
--#         `huge` = number;
--#         `ldexp` = function(number, integer) --> number;
--#         `log` = function(number) --> number;
--#         `log10` = function(number) --> number;
--#         -- TODO should really be `function(integer...) --> integer & function(number...) --> number`
--#         `max` = function(number...) --> number;
--#         -- TODO should really be `function(integer...) --> integer & function(number...) --> number`
--#         `min` = function(number...) --> number;
--#         `modf` = function(number) --> (integer, number);
--#         `pi` = number;
--#         `pow` = function(number, number) --> number;
--#         `rad` = function(number) --> number;
--#         -- TODO should really be `function() --> number & function(integer, integer?) --> integer`
--#         `random` = function(integer?, integer?) --> number;
--#         `randomseed` = function(integer);
--#         `sin` = function(number) --> number;
--#         `sinh` = function(number) --> number;
--#         `sqrt` = function(number) --> number;
--#         `tan` = function(number) --> number;
--#         `tanh` = function(number) --> number;
--#     }

