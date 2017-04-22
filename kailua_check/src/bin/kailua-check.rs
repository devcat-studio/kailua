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

use kailua_env::{Spanned, Source, SourceFile, WithLoc};
use kailua_diag::{Stop, Report, ConsoleReport, TrackMaxKind};
use kailua_syntax::{parse_chunk, Chunk};
use kailua_check::check_from_chunk;
use kailua_check::env::Context;
use kailua_check::options::{FsSource, FsOptions};

struct LocalFsSource {
    source: Rc<RefCell<Source>>,
}

impl FsSource for LocalFsSource {
    fn chunk_from_path(&self, resolved_path: Spanned<&Path>,
                       report: &Report) -> Result<Option<Chunk>, Option<Stop>> {
        match SourceFile::from_file(&resolved_path) {
            Ok(file) => {
                let span = self.source.borrow_mut().add(file);
                if let Ok(chunk) = parse_chunk(&self.source.borrow(), span, report) {
                    Ok(Some(chunk))
                } else {
                    Err(Some(Stop)) // we have already reported parsing errors
                }
            }
            Err(e) => {
                if e.kind() == io::ErrorKind::NotFound {
                    Ok(None)
                } else {
                    Err(None)
                }
            }
        }
    }
}

fn parse_and_check(mainpath: &Path) -> Result<(), String> {
    let source = Rc::new(RefCell::new(Source::new()));
    let report = Rc::new(TrackMaxKind::new(ConsoleReport::new(source.clone())));
    let mut context = Context::new(report.clone());

    let fssource = LocalFsSource { source: source };
    let filechunk = fssource.chunk_from_path(mainpath.without_loc(), &report).map_err(|_| {
        format!("error while loading")
    })?;
    let filechunk = filechunk.ok_or_else(|| format!("cannot found the main path"))?;

    let root = mainpath.parent().unwrap_or(&Path::new(".."));
    let opts = Rc::new(RefCell::new(FsOptions::new(fssource, root.to_owned())));

    if check_from_chunk(&mut context, filechunk, opts).is_ok() && report.can_continue() {
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

