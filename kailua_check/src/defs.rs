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
}

const LUA51_DEFS: &'static [Def] = &[
    LUA51_BASE_DEF,
    LUA51_PACKAGE_DEF,
    LUA51_STRING_DEF,
    LUA51_TABLE_DEF,
]; // TODO

pub fn get_defs(name: &str) -> Option<&'static [Def]> {
    match name {
        "lua51"         => Some(LUA51_DEFS),
        "lua51_base"    => Some(LUA51_BASE_DEFS),
        "lua51_package" => Some(LUA51_PACKAGE_DEFS),
        "lua51_string"  => Some(LUA51_STRING_DEFS),
        "lua51_table"   => Some(LUA51_TABLE_DEFS),
        _ => None,
    }
}

