extern crate kailua_env;
extern crate kailua_diag;
extern crate kailua_syntax;

use std::env;
use std::cell::RefCell;
use std::rc::Rc;
use std::path::Path;
use kailua_env::{Source, SourceFile};
use kailua_diag::ConsoleReport;

fn parse_and_dump(path: &str) -> Result<(), String> {
    let mut source = Source::new();
    let file = try!(SourceFile::from_file(&Path::new(path)).map_err(|e| e.to_string()));
    let filespan = source.add(file);
    let source = Rc::new(RefCell::new(source));
    let report = ConsoleReport::new(source.clone());
    if let Ok(chunk) = kailua_syntax::parse_chunk(&source.borrow(), filespan, &report) {
        println!("{:?}{:?}", chunk.global_scope, chunk.block);
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

