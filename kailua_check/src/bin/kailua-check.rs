extern crate kailua_diag;
extern crate kailua_syntax;
extern crate kailua_check;
#[macro_use] extern crate log;
extern crate env_logger;

use std::str;
use std::env;
use std::cell::RefCell;
use std::path::Path;
use std::collections::HashSet;

use kailua_diag::{Span, Spanned, WithLoc, Source, Report, ConsoleReport};
use kailua_syntax::{parse_chunk, Block};

fn parse_and_check(mainpath: &Path) -> Result<(), String> {
    struct Options<'a> {
        source: &'a RefCell<Source>,
        mainpath: &'a Path,
        report: &'a Report,
        required: HashSet<Vec<u8>>,
    }

    impl<'a> kailua_check::Options for Options<'a> {
        fn source(&self) -> &RefCell<Source> { self.source }

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
                info!("requiring {:?}", kailua_syntax::Str::from(path));

                let path = try!(str::from_utf8(path).map_err(|e| e.to_string()));
                let path = if path.ends_with(".lua") {
                    path.to_owned()
                } else {
                    format!("{}.lua", path)
                };
                let maindir = self.mainpath.parent().unwrap_or(&Path::new("."));
                let mut reqpath = maindir.join("..");
                reqpath.push(&path);
                let span = try!(self.source.borrow_mut().add_file(&reqpath)
                                                        .map_err(|e| e.to_string()));
                parse_chunk(&self.source.borrow(), span, self.report)
                    .map_err(|_| format!("parse error"))
            }
        }
    }

    let mut source = Source::new();
    let filespan = try!(source.add_file(mainpath).map_err(|e| e.to_string()));
    let source = RefCell::new(source);
    let report = ConsoleReport::new(&source);
    let mut context = kailua_check::Context::new();
    let mut opts = Options { source: &source, mainpath: mainpath,
                             report: &report, required: HashSet::new() };
    kailua_check::check_from_span(&mut context, filespan, &mut opts, &report)
}

pub fn main() {
    env_logger::init().unwrap();
    for path in env::args().skip(1) {
        println!("--== {} ==--", path);
        if let Err(e) = parse_and_check(&Path::new(&path)) {
            println!("error: {}", e);
        } else {
            println!("done.");
        }
        println!("");
    }
}


