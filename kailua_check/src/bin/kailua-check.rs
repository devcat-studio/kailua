extern crate kailua_syntax;
extern crate kailua_check;

use std::str;
use std::env;
use std::fs;
use std::path::Path;
use std::io::Read;
use std::collections::{HashMap, HashSet};

fn parse(path: &Path) -> Result<kailua_syntax::Block, String> {
    let mut f = try!(fs::File::open(path).map_err(|e| e.to_string()));
    let mut buf = Vec::new();
    try!(f.read_to_end(&mut buf).map_err(|e| e.to_string()));
    drop(f);

    // strip any BOM
    let mut offset = 0;
    if &buf[0..3] == b"\xef\xbb\xbf" {
        offset = 3;
    }

    kailua_syntax::parse_chunk(&buf[offset..]).map_err(|e| e.to_string())
}

fn parse_and_check(mainpath: &Path) -> Result<(), String> {
    struct Options<'a> {
        mainpath: &'a Path,
        required: HashSet<Vec<u8>>,
    }
    impl<'a> kailua_check::Options for Options<'a> {
        fn require_block(&mut self, path: &[u8]) -> Result<kailua_syntax::Block, String> {
            // require only runs once per path
            if self.required.contains(path) {
                return Ok(Vec::new());
            }
            self.required.insert(path.to_owned());

            const BUILTIN_MODS: &'static [&'static str] = &[
                "cjson",
                "url",
                "redis",
            ];

            if BUILTIN_MODS.iter().any(|&name| name.as_bytes() == path) {
                // dummy: they should not affect anything here
                Ok(Vec::new())
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
                parse(&reqpath)
            }
        }
    }

    let chunk = try!(parse(&mainpath));
    let mut globals = HashMap::new();
    let mut opts = Options { mainpath: mainpath, required: HashSet::new() };
    let mut env = kailua_check::Env::new(&mut globals, &mut opts);
    env.open_libs();
    env.visit(&chunk)
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


