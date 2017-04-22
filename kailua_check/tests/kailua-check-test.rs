#[macro_use] extern crate log;
extern crate env_logger;
extern crate clap;
extern crate kailua_test;
extern crate kailua_env;
extern crate kailua_diag;
extern crate kailua_syntax;
extern crate kailua_types;
extern crate kailua_check;

use std::str;
use std::usize;
use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;
use clap::{App, Arg, ArgMatches};
use kailua_env::{Source, Span, Spanned};
use kailua_diag::{Stop, Locale, Report, Reporter, TrackMaxKind};
use kailua_syntax::{Chunk, parse_chunk};
use kailua_types::ty::{TypeContext, Display};
use kailua_check::check_from_chunk;
use kailua_check::options::Options;
use kailua_check::env::Context;

struct Testing {
    note_spanned_infos: bool,
}

impl Testing {
    fn new() -> Testing {
        Testing { note_spanned_infos: false }
    }
}

impl kailua_test::Testing for Testing {
    fn augment_args<'a, 'b: 'a>(&self, app: App<'a, 'b>) -> App<'a, 'b> {
        app.arg(
            Arg::with_name("note_spanned_infos")
                .short("s")
                .long("note-spanned-infos")
                .help("Displays a list of spanned informations.\n\
                       Only useful when used with `--exact-diags`."))
    }

    fn collect_args<'a>(&mut self, matches: &ArgMatches<'a>) {
        self.note_spanned_infos = matches.is_present("note_spanned_infos");
    }

    fn run(&self, source: Rc<RefCell<Source>>, span: Span, filespans: &HashMap<String, Span>,
           report: Rc<Report>) -> String {
        let chunk = match parse_chunk(&source.borrow(), span, &*report) {
            Ok(chunk) => chunk,
            Err(_) => return format!("parse error"),
        };

        struct Opts {
            source: Rc<RefCell<Source>>,
            filespans: HashMap<String, Span>,
        }

        impl Options for Opts {
            fn require_chunk(&mut self, path: Spanned<&[u8]>,
                             report: &Report) -> Result<Chunk, Option<Stop>> {
                let path = str::from_utf8(&path).map_err(|_| None)?;
                let span = *self.filespans.get(path).ok_or(None)?;
                parse_chunk(&self.source.borrow(), span, report).map_err(|_| None)
            }
        }

        let report = Rc::new(TrackMaxKind::new(report));
        let opts = Rc::new(RefCell::new(Opts { source: source, filespans: filespans.clone() }));
        let mut context = Context::new(report.clone());
        let ret = check_from_chunk(&mut context, chunk, opts);

        // spanned information is available even on error
        if self.note_spanned_infos {
            let mut slots: Vec<_> = context.spanned_slots().iter().collect();
            slots.sort_by_key(|slot| {
                (slot.span.unit(), slot.span.end().to_usize(),
                 usize::MAX - slot.span.begin().to_usize())
            });
            for slot in slots {
                let msg = format!("slot: {}",
                                  slot.display(context.types() as &TypeContext)
                                      .localized(Locale::dummy()));
                report.info(slot.span, &msg).done().unwrap();
            }
        }

        match ret {
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
    kailua_test::Tester::new("kailua-check-test", Testing::new())
        .feature("no_implicit_func_sig", cfg!(feature = "no_implicit_func_sig"))
        .feature("warn_on_useless_conds", cfg!(feature = "warn_on_useless_conds"))
        .feature("warn_on_dead_code", cfg!(feature = "warn_on_dead_code"))
        .scan("src/tests")
        .done();
}

