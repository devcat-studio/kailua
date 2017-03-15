-- definitions for Lua 5.1 basic library

--# -- TODO return generics
--# assume global `assert`:
--#     [assert] function(any, string?)
--#
--# assume global `collectgarbage`:
--#     function(string?, any?) --> any
--#
--# assume global `dofile`:
--#     [geval] function(string?) --> any
--#
--# -- TODO diverging function signature
--# assume global `error`:
--#     function(string, integer?)
--#
--# assume global `_G`:
--#     [genv] table
--#
--# assume global `getfenv`:
--#     function(function|integer?) --> table
--#
--# assume global `getmetatable`:
--#     function(any) --> table?
--#
--# assume global `ipairs`:
--#     [generic_pairs]
--#     function(vector<const WHATEVER>) -->
--#         (function(vector<const WHATEVER>, integer) --> (integer?, any),
--#          vector<const WHATEVER>, integer)
--#
--# -- TODO sequence conditional union: (function) | (nil, string)
--# assume global `load`:
--#     [geval] function(function() --> string?, string?) --> (function, string)
--#
--# -- TODO sequence conditional union: (function) | (nil, string)
--# assume global `loadfile`:
--#     [geval] function(string?) --> (function, string)
--#
--# -- TODO sequence conditional union: (function) | (nil, string)
--# assume global `loadstring`:
--#     [geval] function(string, string?) --> (function, string)
--#
--# -- TODO genericity
--# assume global `next`:
--#     function(table, any?) --> (integer, any)
--#
--# assume global `pairs`:
--#     [generic_pairs] function(table) --> (function(table, any) --> (any?, any), table, any)
--#
--# -- TODO `f` should be once function
--# -- TODO genericity
--# assume global `pcall`:
--#     function(function, any...) --> (boolean, any...)
--#
--# assume global `print`:
--#     function(any...)
--#
--# assume global `rawequal`:
--#     function(any, any) --> boolean
--#
--# assume global `rawget`:
--#     function(table, any) --> any
--#
--# assume global `rawset`:
--#     function(table, any, any) --> table
--#
--# -- TODO genericity
--# assume global `select`:
--#     function(number|'#', any...) --> (any...)
--#
--# assume global `setfenv`:
--#     function(function|integer?, table) --> function
--#
--# assume global `setmetatable`:
--#     function(table, any?) --> table
--#
--# assume global `tonumber`:
--#     function(any, integer?) --> number
--#
--# assume global `tostring`:
--#     function(any) --> string
--#
--# -- TODO enumerate all the possibility?
--# assume global `type`:
--#     [type] function(any) --> string
--#
--# -- TODO genericity
--# assume global `unpack`:
--#     function(table, integer?, integer?) --> (any...)
--#
--# assume global `_VERSION`:
--#     string
--#
--# -- TODO `f` and `err` should be once function
--# -- TODO genericity
--# assume global `xpcall`:
--#     function(function, function) --> (boolean, any...)
--#
--# assume global `coroutine`:
--#     {
--#         -- TODO genericity
--#         `create` = function(function) --> thread;
--#         `resume` = function(thread, any...) --> (boolean, any...);
--#         `running` = function() --> thread;
--#         `status` = function(thread) --> string;
--#         -- TODO genericity
--#         `wrap` = function(function) --> thread;
--#         `yield` = function(any...) --> (any...);
--#     }

