extern crate env_logger;
extern crate regex;
extern crate clap;
extern crate kailua_test;
extern crate kailua_env;
extern crate kailua_diag;
extern crate kailua_syntax;

use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;
use clap::{App, Arg, ArgMatches};
use kailua_env::{Source, Span};
use kailua_diag::{Report, Reporter, TrackMaxKind};

struct Testing {
    span_pattern: regex::Regex,
    scoped_id_pattern: regex::Regex,
    note_scopes: bool,
}

impl Testing {
    fn new() -> Testing {
        let span_pattern = regex::Regex::new(r"@(?:_|\d+(?:/\d+(?:-\d+)?)?)").unwrap();
        assert_eq!(span_pattern.replace_all("[X@1, Y@3/40-978]@_", ""), "[X, Y]");

        let scoped_id_pattern = regex::Regex::new(r"<(\d+)>").unwrap();

        Testing {
            span_pattern: span_pattern,
            scoped_id_pattern: scoped_id_pattern,
            note_scopes: false,
        }
    }
}

impl kailua_test::Testing for Testing {
    fn augment_args<'a, 'b: 'a>(&self, app: App<'a, 'b>) -> App<'a, 'b> {
        app.arg(
            Arg::with_name("note_scopes")
                .short("s")
                .long("note-scopes")
                .help("Displays a list of scopes and associated names as notes.\n\
                       Only useful when used with `--exact-diags`."))
    }

    fn collect_args<'a>(&mut self, matches: &ArgMatches<'a>) {
        self.note_scopes = matches.is_present("note_scopes");
    }

    fn run(&self, source: Rc<RefCell<Source>>, span: Span, _filespans: &HashMap<String, Span>,
           report: Rc<Report>) -> String {
        let report = TrackMaxKind::new(&*report);
        if let Ok(chunk) = kailua_syntax::parse_chunk(&source.borrow(), span, &report) {
            let s = format!("{:?}", chunk.block);
            if self.note_scopes {
                for scope in chunk.map.all_scopes() {
                    let mut msg = format!("scope {:?}", scope.base);
                    if let Some(parent) = chunk.map.parent_scope(scope.base) {
                        msg.push_str(&format!(" <- {:?}", parent));
                    }
                    msg.push_str(&format!(": {:?}",
                                          chunk.map.names(scope.base).collect::<Vec<_>>()));
                    report.note(scope.span, msg).done().unwrap();
                }
            }
            let s = self.span_pattern.replace_all(&s, "");
            let s = self.scoped_id_pattern.replace_all(&s, |caps: &regex::Captures| {
                let id = caps[1].parse().unwrap();
                if let Some((name, scope)) = chunk.map.find_id_with_index(id) {
                    format!("{:?}{:?}", name, scope)
                } else {
                    caps[0].to_owned()
                }
            });
            return s;
        }
        String::from("error")
    }
}

fn main() {
    env_logger::init().unwrap();
    kailua_test::Tester::new("kailua-parse-test", Testing::new()).scan("src/tests").done();
}

