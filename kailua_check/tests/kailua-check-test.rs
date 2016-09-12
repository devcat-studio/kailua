#[macro_use] extern crate log;
extern crate env_logger;
extern crate kailua_test;
extern crate kailua_diag;
extern crate kailua_syntax;
extern crate kailua_check;

use std::str;
use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;
use kailua_diag::{Source, Span, Spanned, Report, TrackMaxKind};
use kailua_syntax::{Block, parse_chunk};
use kailua_check::{Options, Context, CheckResult, check_from_chunk};

struct Testing;

impl kailua_test::Testing for Testing {
    fn run(&self, source: Rc<RefCell<Source>>, span: Span, filespans: &HashMap<String, Span>,
           report: Rc<Report>) -> String {
        let chunk = match parse_chunk(&source.borrow(), span, &*report) {
            Ok(chunk) => chunk,
            Err(_) => return format!("parse error"),
        };

        struct Opts {
            source: Rc<RefCell<Source>>,
            filespans: HashMap<String, Span>,
            report: Rc<TrackMaxKind<Rc<Report>>>,
        }

        impl Options for Opts {
            fn source(&self) -> &RefCell<Source> { &*self.source }
            fn require_block(&mut self, path: &[u8]) -> CheckResult<Spanned<Block>> {
                let path = try!(str::from_utf8(path).map_err(|_| format!("bad require name")));
                let span = *try!(self.filespans.get(path).ok_or_else(|| format!("no such module")));
                parse_chunk(&self.source.borrow(), span, &*self.report)
                    .map_err(|_| format!("parse error"))
            }
        }

        let report = Rc::new(TrackMaxKind::new(report));
        let opts = Rc::new(RefCell::new(Opts { source: source, filespans: filespans.clone(),
                                               report: report.clone() }));
        match check_from_chunk(&mut Context::new(report.clone()), &chunk, opts) {
            Ok(()) => {
                if report.can_continue() {
                    format!("ok")
                } else {
                    info!("check failed due to prior errors");
                    format!("error")
                }
            },
            Err(e) => {
                info!("check failed: {:?}", e);
                format!("error")
            },
        }
    }
}

fn main() {
    env_logger::init().unwrap();
    kailua_test::Tester::new(Testing).scan("src/tests").done();
}

