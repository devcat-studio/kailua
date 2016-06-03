pub struct Def {
    pub name: &'static str,
    pub code: &'static [u8],
}

const LUA51_BASE_DEF: Def =
    Def { name: "lua51_base", code: include_bytes!("defs/lua51_base.lua") };

const LUA51_BASE_DEFS: &'static [Def] = &[LUA51_BASE_DEF];

const LUA51_DEFS: &'static [Def] = &[LUA51_BASE_DEF]; // TODO

pub fn get_defs(name: &str) -> Option<&'static [Def]> {
    match name {
        "lua51" => Some(LUA51_DEFS),
        "lua51_base" => Some(LUA51_BASE_DEFS),
        _ => None,
    }
}

