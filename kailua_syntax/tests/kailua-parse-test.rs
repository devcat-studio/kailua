extern crate regex;
extern crate kailua_test;
extern crate kailua_diag;
extern crate kailua_syntax;

use std::collections::HashMap;
use kailua_diag::{Source, Span, Report};

struct Testing {
    span_pattern: regex::Regex,
}

impl Testing {
    fn new() -> Testing {
        let span_pattern = regex::Regex::new(r"@(?:_|[0-9a-f]+(?:-[0-9a-f]+)?)").unwrap();
        assert_eq!(span_pattern.replace_all("[X@1, Y@3a-4f0]@_", ""), "[X, Y]");
        Testing { span_pattern: span_pattern }
    }
}

impl kailua_test::Testing for Testing {
    fn run(&self, source: &Source, span: Span, _filespans: &HashMap<String, Span>,
           report: &Report) -> String {
        match kailua_syntax::parse_chunk(source, span, report) {
            Ok(chunk) => self.span_pattern.replace_all(&format!("{:?}", chunk), ""),
            Err(_) => String::from("error"),
        }
    }
}

fn main() {
    kailua_test::Tester::new(Testing::new()).scan("src/tests").done();
}

