-- definitions for Lua 5.1 package library

--# assume global `module`:
--#     var [become_module] function(string, function(table)...)
--#
--# assume global `require`:
--#     var [require] function(string) -> any
--#
--# assume global `package`:
--#     var {
--#         `cpath` = var [package_cpath] string;
--#         `loaded` = var { [string] = var table };
--#         `loaders` = var { var function(string) -> (function|string|nil) };
--#         `loadlib` = var [geval] function(string, string);
--#         `path` = var [package_path] string;
--#         `preload` = var { var function(string) -> (function|string|nil) };
--#         -- TODO error type not yet supported (should it be a slot?)
--#         --`seeall` = error "package.seeall is discouraged, use _G instead";
--#     }

