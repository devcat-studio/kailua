extern crate kailua_syntax;
extern crate kailua_check;

use std::env;
use std::fs;
use std::io::Read;
use std::collections::HashMap;

fn parse_and_check(path: &str) -> Result<(), String> {
    let mut f = try!(fs::File::open(path).map_err(|e| e.to_string()));
    let mut buf = Vec::new();
    try!(f.read_to_end(&mut buf).map_err(|e| e.to_string()));
    drop(f);

    // strip any BOM
    let mut offset = 0;
    if &buf[0..3] == b"\xef\xbb\xbf" {
        offset = 3;
    }

    struct Options;
    impl kailua_check::Options for Options {
        fn require_block(&mut self, path: &[u8]) -> Option<kailua_syntax::Block> {
            None
        }
    }

    let chunk = try!(kailua_syntax::parse_chunk(&buf[offset..]).map_err(|e| e.to_string()));
    let mut globals = HashMap::new();
    let mut opts = Options;
    let mut env = kailua_check::Env::new(&mut globals, &mut opts);
    env.visit(&chunk)
}

pub fn main() {
    for path in env::args().skip(1) {
        println!("--== {} ==--", path);
        if let Err(e) = parse_and_check(&path) {
            println!("error: {}", e);
        }
        println!("");
    }
}


