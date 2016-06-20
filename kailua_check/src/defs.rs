pub struct Def {
    pub name: &'static str,
    pub code: &'static [u8],
}

macro_rules! defs {
    ($($defs:ident, $def:ident = $name:expr, $path:expr;)*) => ($(
        const $def: Def = Def { name: $name, code: include_bytes!($path) };
        const $defs: &'static [Def] = &[$def];
    )*);
}

defs! {
    LUA51_BASE_DEFS,    LUA51_BASE_DEF    = "lua51_base",    "defs/lua51_base.lua";
    LUA51_PACKAGE_DEFS, LUA51_PACKAGE_DEF = "lua51_package", "defs/lua51_package.lua";
    LUA51_STRING_DEFS,  LUA51_STRING_DEF  = "lua51_string",  "defs/lua51_string.lua";
    LUA51_TABLE_DEFS,   LUA51_TABLE_DEF   = "lua51_table",   "defs/lua51_table.lua";
    LUA51_MATH_DEFS,    LUA51_MATH_DEF    = "lua51_math",    "defs/lua51_math.lua";
    LUA51_IO_DEFS,      LUA51_IO_DEF      = "lua51_io",      "defs/lua51_io.lua";
    LUA51_DEBUG_DEFS,   LUA51_DEBUG_DEF   = "lua51_debug",   "defs/lua51_debug.lua";
}

const LUA51_DEFS: &'static [Def] = &[
    LUA51_BASE_DEF,
    LUA51_PACKAGE_DEF,
    LUA51_STRING_DEF,
    LUA51_TABLE_DEF,
    LUA51_MATH_DEF,
    LUA51_IO_DEF,
    LUA51_DEBUG_DEF,
];

pub fn get_defs(name: &str) -> Option<&'static [Def]> {
    match name {
        "lua51"         => Some(LUA51_DEFS),
        "lua51_base"    => Some(LUA51_BASE_DEFS),
        "lua51_package" => Some(LUA51_PACKAGE_DEFS),
        "lua51_string"  => Some(LUA51_STRING_DEFS),
        "lua51_table"   => Some(LUA51_TABLE_DEFS),
        "lua51_math"    => Some(LUA51_MATH_DEFS),
        "lua51_io"      => Some(LUA51_IO_DEFS),
        "lua51_debug"   => Some(LUA51_DEBUG_DEFS),
        _ => None,
    }
}

