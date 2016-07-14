extern crate kailua_diag;
extern crate kailua_syntax;
extern crate kailua_check;
#[macro_use] extern crate log;
extern crate env_logger;

use std::str;
use std::io;
use std::env;
use std::cell::RefCell;
use std::rc::Rc;
use std::path::{Path, PathBuf};

use kailua_diag::{Spanned, Source, Report, ConsoleReport};
use kailua_syntax::{parse_chunk, Block};

fn parse_and_check(mainpath: &Path) -> Result<(), String> {
    struct Options {
        source: Rc<RefCell<Source>>,
        root: PathBuf,
        report: Rc<Report>,
    }

    impl kailua_check::Options for Options {
        fn source(&self) -> &RefCell<Source> { &*self.source }

        fn require_block(&mut self, path: &[u8]) -> Result<Spanned<Block>, String> {
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
                                             &*opts.report).map_err(|_| format!("parse error")));
                Ok(Some(chunk))
            };

            macro_rules! try_path {
                ($e:expr) => ({
                    if let Some(chunk) = try!(try(self, Path::new(&$e))) { return Ok(chunk); }
                })
            }

            let try_dotlua = !path.ends_with(".lua");
            if try_dotlua {
                try_path!(format!("{}.lua.kailua", path));
            }
            // this has to be first, it turns out that people tries to use A.kailua against A.lua
            try_path!(format!("{}.kailua", path));
            if try_dotlua {
                try_path!(format!("{}.lua", path));
            }
            try_path!(path);
            Err(format!("module not found"))
        }
    }

    let mut source = Source::new();
    let filespan = try!(source.add_file(mainpath).map_err(|e| e.to_string()));
    let source = Rc::new(RefCell::new(source));
    let report = Rc::new(ConsoleReport::new(source.clone()));
    let mut context = kailua_check::Context::new(report.clone());
    let root = mainpath.parent().unwrap_or(&Path::new(".."));
    let mut opts = Options { source: source, root: root.to_owned(), report: report };
    kailua_check::check_from_span(&mut context, filespan, &mut opts)
}

pub fn main() {
    env_logger::init().unwrap();
    for path in env::args().skip(1) {
        println!("--== {} ==--", path);
        if let Err(e) = parse_and_check(&Path::new(&path)) {
            // hide the internal error message, which will be eventually removed
            println!("stopped due to prior errors.");
            info!("error while checking {}: {}", path, e);
        } else {
            println!("done.");
        }
        println!("");
    }
}

