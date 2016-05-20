extern crate kailua_diag;
extern crate kailua_syntax;
extern crate kailua_check;

use std::str;
use std::env;
use std::path::Path;
use std::collections::HashSet;

use kailua_diag::{Span, Spanned, WithLoc, Source, ConsoleReport};
use kailua_syntax::{parse_chunk, Block};

fn parse(source: &mut Source, path: &Path) -> Result<Spanned<Block>, String> {
    let filespan = try!(source.add_file(path).map_err(|e| e.to_string()));
    let report = ConsoleReport::new(&source);
    parse_chunk(source, filespan, &report).map_err(|_| format!("parse error"))
}

fn parse_and_check(mainpath: &Path) -> Result<(), String> {
    struct Options<'a> {
        source: Source,
        mainpath: &'a Path,
        required: HashSet<Vec<u8>>,
    }
    impl<'a> kailua_check::Options for Options<'a> {
        fn require_block(&mut self, path: &[u8]) -> Result<Spanned<Block>, String> {
            // require only runs once per path
            if self.required.contains(path) {
                return Ok(Vec::new().with_loc(Span::dummy()));
            }
            self.required.insert(path.to_owned());

            const BUILTIN_MODS: &'static [&'static str] = &[
                "cjson",
                "url",
                "redis",
            ];

            if BUILTIN_MODS.iter().any(|&name| name.as_bytes() == path) {
                // dummy: they should not affect anything here
                Ok(Vec::new().with_loc(Span::dummy()))
            } else {
                println!("requiring {:?}", kailua_syntax::Str::from(path));

                let path = try!(str::from_utf8(path).map_err(|e| e.to_string()));
                let path = if path.ends_with(".lua") {
                    path.to_owned()
                } else {
                    format!("{}.lua", path)
                };
                let maindir = self.mainpath.parent().unwrap_or(&Path::new("."));
                let mut reqpath = maindir.join("..");
                reqpath.push(&path);
                parse(&mut self.source, &reqpath)
            }
        }
    }

    const BOOTSTRAP_CODE: &'static str = r#"
        --# assume `require`: ? = "require"
        --# assume `package`: ?
        --# assume `assert`: ?
        --# assume `type`: ?
        --# assume `tonumber`: ?
        --# assume `tostring`: ?
        --# assume `pairs`: ?
        --# assume `ipairs`: ?
        --# assume `pcall`: ?
        --# assume `xpcall`: ?
        --# assume `error`: ?
        --# assume `getmetatable`: ?
        --# assume `setmetatable`: ?
        --# assume `rawget`: ?
        --# assume `rawset`: ?
        --# assume `select`: ?
        --# assume `print`: ?
        --# assume `loadstring`: ?
        --# assume `pack`: ?
        --# assume `unpack`: ?
        --# assume `next`: ?
        --# assume `_G`: ? = "globals" -- not yet supported

        --# assume `string`: ?
        --# assume `math`: ?
        --# assume `table`: ?
        --# assume `io`: ?
        --# assume `os`: ?
        --# assume `debug`: ?
    "#;

    let mut source = Source::new();
    let bootstrapspan = source.add_string("<bootstrap>", BOOTSTRAP_CODE.as_bytes());
    let chunk = try!(parse(&mut source, &mainpath));
    let bootstrapchunk = parse_chunk(&source, bootstrapspan,
                                     &ConsoleReport::new(&source)).unwrap();
    let mut context = kailua_check::Context::new();
    let mut opts = Options { source: source, mainpath: mainpath, required: HashSet::new() };
    let mut checker = kailua_check::Checker::new(&mut context, &mut opts);
    try!(checker.visit(&bootstrapchunk));
    checker.visit(&chunk)
}

pub fn main() {
    for path in env::args().skip(1) {
        println!("--== {} ==--", path);
        if let Err(e) = parse_and_check(&Path::new(&path)) {
            println!("error: {}", e);
        }
        println!("");
    }
}


