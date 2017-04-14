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
use kailua_syntax::{Lexer, Nest, NestedToken, Parser, Chunk, TokenAux};

fn lex_and_parse_chunk(source: &Source, span: Span,
                       report: &Report) -> kailua_diag::Result<(Vec<NestedToken>, Chunk)> {
    if let Some(mut iter) = source.iter_from_span(span) {
        let mut lexer = Lexer::new(&mut iter, &report);
        let tokens: Vec<_> = Nest::new(&mut lexer).collect();
        let chunk = {
            let mut tokens_iter = tokens.iter().cloned();
            let parser = Parser::new(&mut tokens_iter, &report);
            parser.into_chunk()?
        };
        Ok((tokens, chunk))
    } else {
        panic!("couldn't lex and parse chunk");
    }
}

struct Testing {
    span_pattern: regex::Regex,
    scoped_id_pattern: regex::Regex,
    note_scopes: bool,
    note_token_aux: bool,
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
            note_token_aux: false,
        }
    }
}

impl kailua_test::Testing for Testing {
    fn augment_args<'a, 'b: 'a>(&self, app: App<'a, 'b>) -> App<'a, 'b> {
        app.arg(
            Arg::with_name("note_scopes")
                .short("s")
                .long("note-scopes")
                .help("Displays a list of scopes and associated names as reports.\n\
                       Only useful when used with `--exact-diags`.")
        ).arg(
            Arg::with_name("note_token_aux")
                .short("x")
                .long("note-token-aux")
                .help("Displays auxiliary informations generated for each token after parsing.\n\
                       Only useful when used with `--exact-diags`.")
        )
    }

    fn collect_args<'a>(&mut self, matches: &ArgMatches<'a>) {
        self.note_scopes = matches.is_present("note_scopes");
        self.note_token_aux = matches.is_present("note_token_aux");
    }

    fn run(&self, source: Rc<RefCell<Source>>, span: Span, _filespans: &HashMap<String, Span>,
           report: Rc<Report>) -> String {
        let report = TrackMaxKind::new(&*report);
        if let Ok((tokens, chunk)) = lex_and_parse_chunk(&source.borrow(), span, &report) {
            assert_eq!(tokens.len(), chunk.token_aux.len());
            let s = format!("{:?}", chunk.block);
            if self.note_scopes {
                for scope in chunk.map.all_scopes() {
                    let mut msg = format!("scope {:?}", scope.base);
                    if let Some(parent) = chunk.map.parent_scope(scope.base) {
                        msg.push_str(&format!(" <- {:?}", parent));
                    }
                    msg.push_str(&format!(": {:?}",
                                          chunk.map.names(scope.base).collect::<Vec<_>>()));
                    report.info(scope.span, msg).done().unwrap();
                }
            }
            if self.note_token_aux {
                for (tok, aux) in tokens.iter().zip(chunk.token_aux.iter()) {
                    match *aux {
                        TokenAux::None => {}
                        TokenAux::LocalVarName(ref id) => {
                            let def = chunk.local_names.get(id).expect("unregistered scoped id");
                            let msg = format!("LocalVarName #{} ({:?})", id.to_usize(), def.kind);
                            report.info(tok.tok.span, msg).done().unwrap();
                        }
                        TokenAux::GlobalVarName => {
                            report.info(tok.tok.span, "GlobalVarName").done().unwrap();
                        }
                    }
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

