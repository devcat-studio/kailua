extern crate kailua_diag;
extern crate kailua_syntax;
extern crate kailua_check;
#[macro_use] extern crate log;
extern crate env_logger;

use std::str;
use std::io;
use std::env;
use std::cell::RefCell;
use std::path::{Path, PathBuf};

use kailua_diag::{Span, Spanned, WithLoc, Source, Report, ConsoleReport};
use kailua_syntax::{parse_chunk, Block};

fn parse_and_check(mainpath: &Path) -> Result<(), String> {
    struct Options<'a> {
        source: &'a RefCell<Source>,
        root: PathBuf,
        report: &'a Report,
    }

    impl<'a> kailua_check::Options for Options<'a> {
        fn source(&self) -> &RefCell<Source> { self.source }

        fn require_block(&mut self, path: &[u8]) -> Result<Spanned<Block>, String> {
            const BUILTIN_MODS: &'static [&'static str] = &[
                "cjson",
                "url",
                "redis",
            ];

            if BUILTIN_MODS.iter().any(|&name| name.as_bytes() == path) {
                // dummy: they should not affect anything here
                Ok(Vec::new().with_loc(Span::dummy()))
            } else {
                let path = try!(str::from_utf8(path).map_err(|e| e.to_string()));

                let try = |opts: &mut Options, path: &Path| {
                    let path = opts.root.join(path);
                    let span = match opts.source.borrow_mut().add_file(&path) {
                        Ok(span) => span,
                        Err(e) => {
                            if e.kind() == io::ErrorKind::NotFound { return Ok(None); }
                            return Err(e.to_string());
                        }
                    };
                    let chunk = try!(parse_chunk(&opts.source.borrow(), span,
                                                 opts.report).map_err(|_| format!("parse error")));
                    Ok(Some(chunk))
                };

                macro_rules! try_path {
                    ($e:expr) => ({
                        if let Some(chunk) = try!(try(self, Path::new(&$e))) { return Ok(chunk); }
                    })
                }

                if !path.ends_with(".lua") {
                    try_path!(format!("{}.lua.kailua", path));
                    try_path!(format!("{}.lua", path));
                }
                try_path!(format!("{}.kailua", path));
                try_path!(path);
                Err(format!("module not found"))
            }
        }
    }

    let mut source = Source::new();
    let filespan = try!(source.add_file(mainpath).map_err(|e| e.to_string()));
    let source = RefCell::new(source);
    let report = ConsoleReport::new(&source);
    let mut context = kailua_check::Context::new();
    let root = mainpath.parent().unwrap_or(&Path::new(".."));
    let mut opts = Options { source: &source, root: root.to_owned(), report: &report };
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


