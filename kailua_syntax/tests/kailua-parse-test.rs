extern crate env_logger;
extern crate regex;
extern crate kailua_test;
extern crate kailua_diag;
extern crate kailua_syntax;

use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;
use kailua_diag::{Source, Span, Report, TrackMaxKind};

struct Testing {
    span_pattern: regex::Regex,
}

impl Testing {
    fn new() -> Testing {
        let span_pattern = regex::Regex::new(r"@(?:_|\d+(?:/\d+(?:-\d+)?)?)").unwrap();
        assert_eq!(span_pattern.replace_all("[X@1, Y@3/40-978]@_", ""), "[X, Y]");
        Testing { span_pattern: span_pattern }
    }
}

impl kailua_test::Testing for Testing {
    fn run(&self, source: Rc<RefCell<Source>>, span: Span, _filespans: &HashMap<String, Span>,
           report: Rc<Report>) -> String {
        let report = TrackMaxKind::new(&*report);
        if let Ok(chunk) = kailua_syntax::parse_chunk(&source.borrow(), span, &report) {
            return self.span_pattern.replace_all(&format!("{:?}", chunk), "");
        }
        String::from("error")
    }
}

fn main() {
    env_logger::init().unwrap();
    kailua_test::Tester::new(Testing::new()).scan("src/tests").done();
}

