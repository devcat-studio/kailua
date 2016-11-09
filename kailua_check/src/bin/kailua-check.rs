extern crate kailua_env;
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
use std::path::Path;

use kailua_env::{Source, SourceFile};
use kailua_diag::{Report, ConsoleReport, TrackMaxKind};
use kailua_syntax::{parse_chunk, Chunk};
use kailua_check::{FsSource, FsOptions, Context, check_from_chunk};

struct LocalFsSource {
    source: Rc<RefCell<Source>>,
    report: Rc<Report>,
}

impl FsSource for LocalFsSource {
    fn chunk_from_path(&self, resolved_path: &Path) -> Result<Option<Chunk>, String> {
        match SourceFile::from_file(resolved_path) {
            Ok(file) => {
                let mut source = self.source.borrow_mut();
                let span = source.add(file);
                if let Ok(chunk) = parse_chunk(&source, span, &*self.report) {
                    Ok(Some(chunk))
                } else {
                    Err(format!("parse error"))
                }
            }
            Err(e) => {
                if e.kind() == io::ErrorKind::NotFound {
                    Ok(None)
                } else {
                    Err(e.to_string())
                }
            }
        }
    }
}

fn parse_and_check(mainpath: &Path) -> Result<(), String> {
    let source = Rc::new(RefCell::new(Source::new()));
    let report = Rc::new(TrackMaxKind::new(ConsoleReport::new(source.clone())));
    let mut context = Context::new(report.clone());

    let fssource = LocalFsSource { source: source, report: report.clone() };
    let filechunk = try!(fssource.chunk_from_path(mainpath));
    let filechunk = try!(filechunk.ok_or_else(|| format!("cannot found the main path")));

    let root = mainpath.parent().unwrap_or(&Path::new(".."));
    let opts = Rc::new(RefCell::new(FsOptions::new(fssource, root.to_owned())));

    try!(check_from_chunk(&mut context, filechunk, opts));
    if report.can_continue() {
        Ok(())
    } else {
        Err("stopped due to prior errors".into())
    }
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

