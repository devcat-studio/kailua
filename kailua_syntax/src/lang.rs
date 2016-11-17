use std::fmt;
use kailua_diag::{Localize, Localized};

// a number describing a language version being used.
// this affects the parsing but not the lexing (well, at least currently).
//
// this is a combination of two bit fields 0xLLKK0000
// where 0xLL is the Lua version and 0xKK is the Kailua version (or 0x00 if disabled).
// lower 16 bits are reserved for the future usage and significant extensions.
// 0x00000000 is reserved for the absence of language information.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Language(u32);

impl Language {
    pub fn new(lua: Lua, kailua: Kailua) -> Language {
        Language((lua as u32) << 24 | (kailua as u32) << 16)
    }

    pub fn from_u32(v: u32) -> Option<Language> {
        let lua = v >> 24;
        let kailua = (v >> 16) & 0xff;
        let exts = v & 0xffff;
        match (Lua::from_u32(lua), Kailua::from_u32(kailua), exts) {
            (Some(_), Some(_), 0) => Some(Language(v)),
            (_, _, _) => None,
        }
    }

    pub fn to_u32(&self) -> u32 {
        self.0
    }

    pub fn lua(&self) -> Lua {
        Lua::from_u32(self.0 >> 24).unwrap()
    }

    pub fn kailua(&self) -> Option<Kailua> {
        let v = (self.0 >> 16) & 0xff;
        if v == 0 { None } else { Some(Kailua::from_u32(v).unwrap()) }
    }
}

impl fmt::Debug for Language {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let lua = self.lua();
        if let Some(kailua) = self.kailua() {
            write!(f, "<{} + {}>", lua.name(), kailua.name())
        } else {
            write!(f, "<{}>", lua.name())
        }
    }
}

impl Localize for Language {
    fn fmt_localized(&self, f: &mut fmt::Formatter, lang: &str) -> fmt::Result {
        let lua = self.lua();
        let lua_name = Localized::new(&lua, lang);
        if let Some(kailua) = self.kailua() {
            let kailua_name = Localized::new(&kailua, lang);
            match lang {
                "ko" => write!(f, "{} 확장을 사용하는 {}", kailua_name, lua_name),
                _ => write!(f, "{} with {} extension", lua_name, kailua_name),
            }
        } else {
            write!(f, "{}", lua_name)
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lua {
    Lua51 = 0x51,
    Lua52 = 0x52,
    Lua53 = 0x53,
}

impl Lua {
    pub fn from_u32(v: u32) -> Option<Lua> {
        match v {
            0x51 => Some(Lua::Lua51),
            0x52 => Some(Lua::Lua52),
            0x53 => Some(Lua::Lua53),
            _ => None,
        }
    }

    pub fn name(&self) -> &'static str {
        match *self {
            Lua::Lua51 => "Lua 5.1",
            Lua::Lua52 => "Lua 5.2",
            Lua::Lua53 => "Lua 5.3",
        }
    }
}

impl fmt::Debug for Lua {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>", self.name())
    }
}

impl Localize for Lua {
    fn fmt_localized(&self, f: &mut fmt::Formatter, _lang: &str) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kailua {
    // 0x00 is reserved for no extension
    Kailua01 = 0x01, // to be updated later
}

impl Kailua {
    pub fn from_u32(v: u32) -> Option<Kailua> {
        match v {
            0x01 => Some(Kailua::Kailua01),
            _ => None,
        }
    }

    pub fn name(&self) -> &'static str {
        match *self {
            Kailua::Kailua01 => "Kailua 0.1",
        }
    }
}

impl fmt::Debug for Kailua {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>", self.name())
    }
}

impl Localize for Kailua {
    fn fmt_localized(&self, f: &mut fmt::Formatter, _lang: &str) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

