extern crate kailua_diag;
extern crate kailua_syntax;

use std::env;
use std::cell::RefCell;
use std::rc::Rc;
use std::path::Path;

fn parse_and_dump(path: &str) -> Result<(), String> {
    let mut source = kailua_diag::Source::new();
    let filespan = try!(source.add_file(&Path::new(path)).map_err(|e| e.to_string()));
    let source = Rc::new(RefCell::new(source));
    let report = kailua_diag::ConsoleReport::new(source.clone());
    if let Ok(chunk) = kailua_syntax::parse_chunk(&source.borrow(), filespan, &report) {
        println!("{:?}", chunk);
    }
    Ok(())
}

pub fn main() {
    for path in env::args().skip(1) {
        println!("--== {} ==--", path);
        if let Err(e) = parse_and_dump(&path) {
            println!("error: {}", e);
        }
        println!("");
    }
}

