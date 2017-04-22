//! Source language description.
//!
//! Right now Kailua only supports the entirety of Lua 5.1 and its own extension syntax,
//! but these types may allow other versions of Lua in the future.

use std::fmt;
use kailua_diag::{Locale, Localize, Localized};

/// Describes a language version being used.
///
/// Internally this is a combination of two bit fields `0xLLKK0000`,
/// where `0xLL` is the Lua version and `0xKK` is the Kailua version (or `0x00` if disabled).
/// Lower 16 bits are reserved for the future usage and significant extensions.
/// `0x00000000` is reserved for the absence of language information.
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
    fn fmt_localized(&self, f: &mut fmt::Formatter, locale: Locale) -> fmt::Result {
        let lua = self.lua();
        let lua_name = Localized::new(&lua, locale);
        if let Some(kailua) = self.kailua() {
            let kailua_name = Localized::new(&kailua, locale);
            match &locale[..] {
                "ko" => write!(f, "{} 확장을 사용하는 {}", kailua_name, lua_name),
                _ => write!(f, "{} with {} extension", lua_name, kailua_name),
            }
        } else {
            write!(f, "{}", lua_name)
        }
    }
}

/// Lua version.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lua {
    /// Lua 5.1.
    Lua51 = 0x51,

    /// Lua 5.2.
    Lua52 = 0x52,

    /// Lua 5.3.
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
    fn fmt_localized(&self, f: &mut fmt::Formatter, _locale: Locale) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// Kailua version.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kailua {
    // 0x00 is reserved for no extension

    /// Kaliua 1.0.x.
    ///
    /// Please note that they had been in flux because of heavy development
    /// and no strong compatibility is guaranteed.
    Kailua10 = 0x10,
}

impl Kailua {
    pub fn from_u32(v: u32) -> Option<Kailua> {
        match v {
            0x10 => Some(Kailua::Kailua10),
            _ => None,
        }
    }

    pub fn name(&self) -> &'static str {
        match *self {
            Kailua::Kailua10 => "Kailua 1.0.x",
        }
    }
}

impl fmt::Debug for Kailua {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>", self.name())
    }
}

impl Localize for Kailua {
    fn fmt_localized(&self, f: &mut fmt::Formatter, _locale: Locale) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

