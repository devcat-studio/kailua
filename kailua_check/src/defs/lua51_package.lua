-- definitions for Lua 5.1 package library

--# assume global `module`:
--#     [become_module] function(string, function(table)...)
--#
--# assume global `require`:
--#     [require] function(string) --> any
--#
--# assume global `package`:
--#     [currently] {
--#         `cpath` = [package_cpath] string;
--#         `loaded` = { [string] = table };
--#         `loaders` = { function(string) --> (function|string|nil) };
--#         `loadlib` = [geval] function(string, string);
--#         `path` = [package_path] string;
--#         `preload` = { function(string) --> (function|string|nil) };
--#         -- TODO error type not yet supported (should it be a slot?)
--#         --`seeall` = error "package.seeall is discouraged, use _G instead";
--#     }

