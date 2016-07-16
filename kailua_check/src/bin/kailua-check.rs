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

use kailua_diag::{Span, Spanned, Source, Report, ConsoleReport};
use kailua_syntax::{parse_chunk, Block};

struct Options {
    source: Rc<RefCell<Source>>,
    root: PathBuf,
    report: Rc<Report>,
    package_path: Vec<String>,
    package_cpath: Vec<String>,
}

impl Options {
    fn new(source: Rc<RefCell<Source>>, root: PathBuf, report: Rc<Report>) -> Options {
        Options {
            source: source,
            root: root,
            report: report,

            // by default, local files only
            package_path: vec!["?.lua".into()],
            package_cpath: vec![],
        }
    }

    fn search_file(&self, path: &str, search_paths: &[String],
                   suffix: &str) -> Result<Option<Span>, String> {
        for template in search_paths {
            let path = template.replace('?', &path) + suffix;
            let path = self.root.join(path);
            debug!("trying to load {:?}", path);

            match self.source.borrow_mut().add_file(&path) {
                Ok(span) => return Ok(Some(span)),
                Err(e) => {
                    if e.kind() == io::ErrorKind::NotFound { continue; }
                    return Err(e.to_string());
                }
            };
        }

        Ok(None)
    }
}

impl kailua_check::Options for Options {
    fn source(&self) -> &RefCell<Source> { &*self.source }

    fn set_package_path(&mut self, path: &[u8]) -> Result<(), String> {
        let path = try!(str::from_utf8(path).map_err(|e| e.to_string()));
        self.package_path = path.split(";").map(|s| s.to_owned()).collect();
        Ok(())
    }

    fn set_package_cpath(&mut self, path: &[u8]) -> Result<(), String> {
        let path = try!(str::from_utf8(path).map_err(|e| e.to_string()));
        self.package_cpath = path.split(";").map(|s| s.to_owned()).collect();
        Ok(())
    }

    fn require_block(&mut self, path: &[u8]) -> Result<Spanned<Block>, String> {
        let path = try!(str::from_utf8(path).map_err(|e| e.to_string()));

        let mut span = None;
        if span.is_none() { span = try!(self.search_file(&path, &self.package_path, ".kailua")); }
        if span.is_none() { span = try!(self.search_file(&path, &self.package_cpath, ".kailua")); }
        if span.is_none() { span = try!(self.search_file(&path, &self.package_path, "")); }
        if span.is_none() { span = try!(self.search_file(&path, &self.package_cpath, "")); }

        if let Some(span) = span {
            let chunk = try!(parse_chunk(&self.source.borrow(), span,
                                         &*self.report).map_err(|_| format!("parse error")));
            Ok(chunk)
        } else {
            Err(format!("module not found"))
        }
    }
}

fn parse_and_check(mainpath: &Path) -> Result<(), String> {
    let mut source = Source::new();
    let filespan = try!(source.add_file(mainpath).map_err(|e| e.to_string()));
    let source = Rc::new(RefCell::new(source));
    let report = Rc::new(ConsoleReport::new(source.clone()));
    let mut context = kailua_check::Context::new(report.clone());
    let root = mainpath.parent().unwrap_or(&Path::new(".."));
    let opts = Rc::new(RefCell::new(Options::new(source, root.to_owned(), report.clone())));
    try!(kailua_check::check_from_span(&mut context, filespan, opts));
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

