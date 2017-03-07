use kailua_env::{Span, SourceData, WithLoc};
use kailua_diag::NoReport;
use kailua_syntax::{Chunk, Lexer, Nest, Parser};

pub struct Def {
    pub name: &'static str,
    pub code: &'static [u8],
}

impl Def {
    pub fn to_chunk(&self) -> Chunk {
        let span = Span::builtin(); // a special span independent of Source
        let mut iter = self.code.iter().map(|&c| SourceData::U8(c).with_loc(span))
                                       .chain(Some(SourceData::EOF.with_loc(span)));
        let no_report = NoReport;
        let mut lexer = Lexer::new(&mut iter, &no_report);
        let mut nest = Nest::new(&mut lexer);
        let parser = Parser::new(&mut nest, &no_report);
        match parser.into_chunk() {
            Ok(chunk) => chunk,
            Err(e) => panic!("failed to parse a built-in definition {:?}: {:?}", self.name, e),
        }
    }
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
    LUA51_OS_DEFS,      LUA51_OS_DEF      = "lua51_os",      "defs/lua51_os.lua";
    LUA51_DEBUG_DEFS,   LUA51_DEBUG_DEF   = "lua51_debug",   "defs/lua51_debug.lua";
    KAILUA_TEST_DEFS,   KAILUA_TEST_DEF   = "kailua_test",   "defs/kailua_test.lua";
}

const LUA51_DEFS: &'static [Def] = &[
    LUA51_BASE_DEF,
    LUA51_PACKAGE_DEF,
    LUA51_STRING_DEF,
    LUA51_TABLE_DEF,
    LUA51_MATH_DEF,
    LUA51_IO_DEF,
    LUA51_OS_DEF,
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
        "lua51_os"      => Some(LUA51_OS_DEFS),
        "lua51_debug"   => Some(LUA51_DEBUG_DEFS),

        // only internally used
        "internal kailua_test" => Some(KAILUA_TEST_DEFS),

        _ => None,
    }
}

