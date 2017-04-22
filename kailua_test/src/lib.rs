//! Test harness for Kailua.
//!
//! Most integration tests in Kailua are composed of the input, the expected output and
//! expected reports to `kailua_diag::Report`. This cannot be easily done with unit tests,
//! so this library simplifies the creation of such test harness for individual applications.
//!
//! A crate can add the following section to its `Cargo.toml`...
//!
//! ```toml
//! [[test]]
//! name = "integration-test"
//! harness = false
//! ```
//!
//! ...and put the following to the corresponding source file.
//!
//! ```rust
//! extern crate kailua_env;
//! extern crate kailua_diag;
//! extern crate kailua_test;
//!
//! use std::cell::RefCell;
//! use std::rc::Rc;
//! use std::collections::HashMap;
//! use kailua_env::{Source, Span};
//! use kailua_diag::Report;
//!
//! struct Testing;
//! impl kailua_test::Testing for Testing {
//!     fn run(&self, source: Rc<RefCell<Source>>, span: Span,
//!            filespans: &HashMap<String, Span>, report: Rc<Report>) -> String {
//!         format!("Expected test output")
//!     }
//! }
//!
//! fn main() {
//! #   return; // we only check if this compiles
//!     kailua_test::Tester::new("integration-test", Testing).scan("src/tests").done();
//! }
//! ```
//!
//! This will scan all `*.lua` files in `src/tests` and
//! test all them according to the command-line options.
//! It is possible to add more methods to `Testing` to further customize the test application.
//!
//! # Test Format
//!
//! ```lua
//! this part is ignored, put some descriptions here.
//!
//!
//! --8<-- test-name-1 -- options options ...
//! --@v Error: this error should occur in the following line
//! local x = 'some testing code' --@< Note: this note should occur in this exact line
//! --@^ Warning: this warning should occur in the preceding line
//!
//! --@v-vvv Fatal: this fatal error should span following three lines
//! do
//!     return
//! end
//!
//! --! expected output here; long line can be concatenated \
//! --!     like this, where preceding whitespaces are ignored in this line.
//!
//!
//! -->8-- test-name-2
//! this test is ignored unless `-f` is given.
//!
//!
//! --8<-- test-name-3
//! local a = require 'x'
//!
//! --& x
//! -- it is possible to use multiple input files.
//! return 42
//!
//! --&
//! -- duplicate input file names are invalid, except for the empty file name.
//! -- this means that the preceding file (named or not) should be trimmed following whitespaces,
//! -- required by parser tests which tend to be sensitive to the final whitespaces.
//! -- the code block itself gets ignored.
//! ```
//!
//! More on report formats:
//!
//! * The explicit line can be also given (`--@3-8 ...`);
//!   the line number is 1-based and renumbered for each code block.
//!
//! * The line number can be also missing,
//!   in which case the report should not have any associated span.
//!
//! * All report `Kind`s except for `Kind::Info` can be used.
//!   The `info` report is normally used for debugging informations,
//!   so it cannot be suppressed nor checked against.
//!
//! Supported test options:
//!
//! * `exact` means that the test should not tolerate additional reports
//!   (normally only enabled when `-e` is given).
//!
//! * `feature:FEATURE` enables the test only when a particular feature is enabled.
//!   There can be multiple feature options, all of them are required to enable the test.
//!   Note that this is different from Cargo features;
//!   you should manually set the flag with `Tester::feature` in the `main` function.
//!
//! * `feature:!FEATURE` enables the test only when a particular feature is disabled.
//!
//! # Test invocation
//!
//! `cargo test` (or if you have multiple test binaries, `cargo test --test NAME`)
//! will do all the jobs. The process will exit on any error.
//!
//! The test binary itself can be given options.
//! Note that this will conflict with Cargo's own options,
//! so they should be in principle given after `--`: `cargo test -- --option-for-tester`.
//! Since this is annoying you can also replace preceding hyphens with pluses:
//! `cargo test ++option-for-tester` will be same and easier to run.
//!
//! Supported options:
//!
//! * `-v`, `--verbose`: Displays all test outputs even when the test passes.
//!
//! * `-e`, `--exact-diags`: Fails the test when additional reports exist.
//!   This is same to put the `exact` option to all tests.
//!
//! * `-h`, `--highlight-mismatch`: Highlights any mismatch in the reports or output.
//!
//! * `-l LOCALE`, `--message-locale LOCALE`: Let reports use given locale.
//!   Likely to fail most tests as the reports are compared by their localized texts.
//!
//! * `-p`, `--stop-on-panic`: Stops on the first panic. Also enables `RUST_BACKTRACE=1`.
//!
//! * `-pp`: Same to `-p` but will enable `RUST_BACKTRACE=full` instead.
//!
//! * `-f`, `--force`: Run the explicitly ignored (`-->8--`) tests.
//!
//! * `-ff`: Same to `-f` but will also run tests which are ignored by feature options.
//!
//! * The implementation can supply its own options by adding `Testing::augment_args` method.
//!
//! The remaining argument, if any, is a regular expression for filtering test names.

extern crate regex;
#[macro_use] extern crate lazy_static;
extern crate term;
extern crate kailua_env;
extern crate kailua_diag;
#[macro_use] extern crate log;
extern crate clap;

use std::str;
use std::fmt;
use std::env;
use std::fs;
use std::mem;
use std::panic;
use std::process;
use std::any::Any;
use std::ascii::AsciiExt;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::Rc;
use std::fs::File;
use std::error::Error;
use std::collections::{HashMap, HashSet};
use regex::Regex;
use term::StderrTerminal;
use clap::{App, Arg, ArgMatches};
use kailua_env::{Source, SourceFile, Span};
use kailua_diag::{Locale, Kind, Report, CollectedReport};

/// A customizable portion of the tester.
pub trait Testing {
    /// Runs the test with given input, producing an output and (optionally) reports.
    ///
    /// `source` holds all inputs. `span` is the span for the (unnamed) main file;
    /// `filespans` are for everything else.
    fn run(&self, source: Rc<RefCell<Source>>, span: Span, filespans: &HashMap<String, Span>,
           report: Rc<Report>) -> String;

    /// Checks if the actual output and expected output matches.
    /// By default it is a simple string equivalence.
    fn check_output(&self, actual: &str, expected: &str) -> bool { actual == expected }

    /// Alters the command-line parser (built with `clap`).
    /// Normally used to add additional options, and does nothing by default.
    fn augment_args<'a, 'b: 'a>(&self, app: App<'a, 'b>) -> App<'a, 'b> { app }

    /// Collects the parsed command-line options. Does nothing by default.
    fn collect_args<'a>(&mut self, _matches: &ArgMatches<'a>) { }
}

#[derive(Debug)]
struct TestError {
    desc: String,
    cause: Option<Box<Error>>,
}

impl TestError {
    fn new(desc: String) -> TestError {
        TestError { desc: desc, cause: None }
    }

    fn new_with_error<T: Error + 'static>(desc: String, cause: Box<T>) -> TestError {
        TestError { desc: desc, cause: Some(cause as Box<Error>) }
    }
}

impl Error for TestError {
    fn description(&self) -> &str { &self.desc }
    fn cause(&self) -> Option<&Error> { self.cause.as_ref().map(|err| &**err) }
}

impl fmt::Display for TestError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.desc)
    }
}

impl<T: fmt::Debug + Error + 'static> From<Box<T>> for TestError {
    fn from(err: Box<T>) -> TestError {
        let desc = format!("{:?}", err);
        TestError::new_with_error(desc, err)
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
struct Expected<'a> {
    // the path is None when it is a main file (the tester will fill the temporary name)
    pos: Option<(Option<Cow<'a, str>>, usize, usize)>,
    kind: Kind,
    msg: Cow<'a, str>,
}

impl<'a> Expected<'a> {
    pub fn into_send(self) -> Expected<'static> {
        Expected {
            pos: self.pos.map(|(p,s,e)| (p.map(|p| p.into_owned().into()), s, e)),
            kind: self.kind,
            msg: self.msg.into_owned().into(),
        }
    }
}

fn split_line<'a>(s: &'a str, file: Option<&'a str>,
                  lineno: usize) -> Result<(&'a str, Option<Expected<'a>>), TestError> {
    use regex::Regex;

    lazy_static! {
        static ref LINE_PATTERN: Regex =
            Regex::new(r"(?xs)
                         ^ (?P<line> .*?)
                           --@ (?: (?P<line1> \d+ | < | \^+ | v+)
                                   (?: - (?P<line2> \d+ | < | \^+ | v+) )?
                               )?
                               \s+ (?P<kind> \w+): (?P<msg> .*)
                         $").unwrap();
    }

    fn kind_from_str(s: &str) -> Option<Kind> {
        match &s.to_ascii_lowercase()[..] {
            "note" => Some(Kind::Note),
            "warn" | "warning" => Some(Kind::Warning),
            "cause" | "because" => Some(Kind::Cause),
            "error" => Some(Kind::Error),
            "fatal" => Some(Kind::Fatal),
            _ => None,
        }
    }

    let err = || TestError::new(format!("invalid test spec at line {}", lineno));
    let erre = |e| TestError::new_with_error(format!("invalid test spec at line {}", lineno),
                                             Box::new(e));

    assert!(lineno > 0);
    if let Some(m) = LINE_PATTERN.captures(s) {
        let line = m.name("line").unwrap().as_str();

        let parse_lineno = |s: &str| {
            if s.starts_with("<") {
                Ok(lineno)
            } else if s.starts_with("^") {
                let lineno = lineno.checked_sub(s.len()).ok_or_else(|| err())?;
                if lineno == 0 { return Err(err()); }
                Ok(lineno)
            } else if s.starts_with("v") {
                lineno.checked_add(s.len()).ok_or_else(|| err())
            } else {
                s.parse().map_err(|e| erre(e))
            }
        };

        let pos = if let Some(line1) = m.name("line1") {
            let line1 = parse_lineno(line1.as_str())?;
            let line2 = if let Some(line2) = m.name("line2") {
                parse_lineno(line2.as_str())?
            } else {
                line1
            };
            if line1 > line2 || line1 == 0 { return Err(err()); }
            Some((line1, line2))
        } else {
            None
        };
        let pos = pos.map(|(s,e)| (file.map(|f| f.into()), s, e));
        let kind = kind_from_str(m.name("kind").unwrap().as_str()).ok_or_else(|| err())?;
        let msg = m.name("msg").unwrap().as_str().trim();
        Ok((line, Some(Expected { pos: pos, kind: kind, msg: msg.into() })))
    } else if s.contains("--@") {
        Err(err()) // invalid syntax present
    } else {
        Ok((s, None))
    }
}

#[test]
fn test_split_line() {
    let path = "foo.lua";
    let split_line = |line, lineno| split_line(line, Some(path), lineno).map_err(|_| ());
    let make_pos = |s, e| Some((Some(path.into()), s, e));

    assert_eq!(split_line("hello\r\n", 42),
               Ok(("hello\r\n", None)));
    assert_eq!(split_line("hello --@\n", 42),
               Err(()));
    assert_eq!(split_line("hello --@ hufffffff\n", 42),
               Err(()));
    assert_eq!(split_line("hello --@ Error: whatever\tffff\t ", 42),
               Ok(("hello ", Some(Expected { pos: None, kind: Kind::Error,
                                             msg: "whatever\tffff".into() }))));
    assert_eq!(split_line("hello --@ eRROR: whatever\tffff\t ", 42),
               Ok(("hello ", Some(Expected { pos: None, kind: Kind::Error,
                                             msg: "whatever\tffff".into() }))));
    assert_eq!(split_line("hello --@ log: whatever\tffff\t ", 42),
               Err(()));
    assert_eq!(split_line("hello --@ Error: --@ Error: whatever ", 42),
               Ok(("hello ", Some(Expected { pos: None, kind: Kind::Error,
                                             msg: "--@ Error: whatever".into() }))));
    assert_eq!(split_line("hello --@ 3 Error: whatever ", 42),
               Err(()));
    assert_eq!(split_line("hello --@ some Error: whatever ", 42),
               Err(()));
    assert_eq!(split_line("hello --@< Error: whatever ", 42),
               Ok(("hello ", Some(Expected { pos: make_pos(42, 42), kind: Kind::Error,
                                             msg: "whatever".into() }))));
    assert_eq!(split_line("hello --@^^^^ Error: whatever ", 42),
               Ok(("hello ", Some(Expected { pos: make_pos(38, 38), kind: Kind::Error,
                                             msg: "whatever".into() }))));
    assert_eq!(split_line("hello --@^ Error: whatever ", 2),
               Ok(("hello ", Some(Expected { pos: make_pos(1, 1), kind: Kind::Error,
                                             msg: "whatever".into() }))));
    assert_eq!(split_line("hello --@^^ Error: whatever ", 2),
               Err(()));
    assert_eq!(split_line("hello --@vvv Error: whatever ", 42),
               Ok(("hello ", Some(Expected { pos: make_pos(45, 45), kind: Kind::Error,
                                             msg: "whatever".into() }))));
    assert_eq!(split_line("hello --@7 Error: whatever ", 42),
               Ok(("hello ", Some(Expected { pos: make_pos(7, 7), kind: Kind::Error,
                                             msg: "whatever".into() }))));
    assert_eq!(split_line("hello --@7-9 Error: whatever ", 42),
               Ok(("hello ", Some(Expected { pos: make_pos(7, 9), kind: Kind::Error,
                                             msg: "whatever".into() }))));
    assert_eq!(split_line("hello --@7-7 Error: whatever ", 42),
               Ok(("hello ", Some(Expected { pos: make_pos(7, 7), kind: Kind::Error,
                                             msg: "whatever".into() }))));
    assert_eq!(split_line("hello --@7-6 Error: whatever ", 42),
               Err(()));
    assert_eq!(split_line("hello --@999999999999999999999999 Error: whatever ", 42),
               Err(()));
    assert_eq!(split_line("hello --@7-999999999999999999999999 Error: whatever ", 42),
               Err(()));
    assert_eq!(split_line("hello --@^-v Error: whatever ", 42),
               Ok(("hello ", Some(Expected { pos: make_pos(41, 43), kind: Kind::Error,
                                             msg: "whatever".into() }))));
    assert_eq!(split_line("hello --@<-vvvv Error: whatever ", 42),
               Ok(("hello ", Some(Expected { pos: make_pos(42, 46), kind: Kind::Error,
                                             msg: "whatever".into() }))));
    assert_eq!(split_line("hello --@^^^-^^ Error: whatever ", 42),
               Ok(("hello ", Some(Expected { pos: make_pos(39, 40), kind: Kind::Error,
                                             msg: "whatever".into() }))));
    assert_eq!(split_line("hello --@<-< Error: whatever ", 42),
               Ok(("hello ", Some(Expected { pos: make_pos(42, 42), kind: Kind::Error,
                                             msg: "whatever".into() }))));
    assert_eq!(split_line("hello --@v-< Error: whatever ", 42),
               Err(()));
    assert_eq!(split_line("hello --@8-vv Error: whatever ", 42),
               Ok(("hello ", Some(Expected { pos: make_pos(8, 44), kind: Kind::Error,
                                             msg: "whatever".into() }))));
    assert_eq!(split_line("hello --@<-45 Error: whatever ", 42),
               Ok(("hello ", Some(Expected { pos: make_pos(42, 45), kind: Kind::Error,
                                             msg: "whatever".into() }))));
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Test {
    file: PathBuf,
    first_line: usize, // the first line after the divider
    name: String, // can be an empty string
    input: Vec<String>,
    files: HashMap<String, Vec<String>>,
    output: Vec<String>,
    reports: Vec<Expected<'static>>,
    ignored: bool,
    exact: bool,
    features: HashSet<String>,
}

fn extract_tests(path: &Path) -> Result<Vec<Test>, TestError> {
    let f = File::open(path).map_err(Box::new)?;
    let f = BufReader::new(f);

    let mut tests = Vec::new();

    let mut test: Option<Test> = None;
    let mut current_file: Option<String> = None;
    let mut current_lines = Vec::new();
    let mut output_cont = false; // did the last output line end with `\`?

    macro_rules! flush {
        ($lineno:expr) => ({
            if output_cont {
                return Err(TestError::new(format!("found a trailing `\\` in the output \
                                                   at line {}", $lineno - 1)));
            }
            if let Some(mut test) = test {
                if !(test.reports.is_empty() && test.output.is_empty()) {
                    if let Some(file) = current_file {
                        if !file.is_empty() { test.files.insert(file, current_lines); }
                    } else {
                        test.input = current_lines;
                    }
                    tests.push(test);
                }
            }
        })
    }

    let mut next_lineno = 1;
    for (lineno, line) in f.lines().enumerate() {
        let line = line.map_err(Box::new)?;
        let lineno = lineno + 1;
        next_lineno = lineno + 1;

        let premature_err = || TestError::new(format!("found test spec before the first \
                                                       `--8<--` at line {}", lineno));

        // try to look at the divider (`-*--8<---* <test name> ---* <options>`)
        if let Some(pos) = line.find("--8<--").or_else(|| line.find("-->8--")) {
            if line[..pos].chars().all(|c| c == '-') {
                // we've found a divider, flush the current test
                flush!(lineno);

                let dash_or_space = |c: char| c == '-' || c.is_whitespace();

                let ignored = &line[pos..pos+6] == "-->8--";
                let line = line[pos+6..].trim_left_matches(&dash_or_space);
                let (name, options) = if let Some(pos) = line.find("--") {
                    (line[..pos].trim_right(), line[pos..].trim_matches(&dash_or_space))
                } else {
                    (line.trim_right_matches(&dash_or_space), "")
                };
                let options: Vec<_> = options.split_whitespace().collect();

                test = Some(Test {
                    file: path.to_owned(),
                    first_line: next_lineno,
                    name: name.to_owned(),
                    input: Vec::new(),
                    files: HashMap::new(),
                    output: Vec::new(),
                    reports: Vec::new(),
                    ignored: ignored,
                    exact: options.contains(&"exact"),
                    features: options.iter().filter_map(|s| {
                        if s.starts_with("feature:") { Some(s[8..].to_owned()) } else { None }
                    }).collect(),
                });
                current_file = None;
                current_lines = Vec::new();
                continue;
            }
        }

        // `--! output` specifies the desired output
        // `--! output \` is same, but the trailing \ and the subsequent newline is stripped
        if let Some(pos) = line.find("--!") {
            let test = test.as_mut().ok_or_else(|| premature_err())?;

            let mut next = line[pos+3..].trim_left();
            let mut next_cont = false;
            if next.ends_with("\\") {
                next = &next[..next.len()-1];
                next_cont = true;
            }
            if output_cont {
                test.output.first_mut().unwrap().push_str(next);
            } else {
                test.output.push(next.to_owned());
            }
            output_cont = next_cont;
            current_lines.push(line[..pos].to_owned());
            continue;
        }

        // `--! ... \` should be followed by other `--!`
        if output_cont {
            return Err(TestError::new(format!("found a trailing `\\` in the output \
                                               at line {}", lineno - 1)));
        }

        // `--& filename` specifies an additional input file
        if line.starts_with("--&") {
            let test = test.as_mut().ok_or_else(|| premature_err())?;

            let filename = line[3..].trim();
            if filename.is_empty() {
                // special case: parser tests are very dependent of final spacing,
                // so if one use `--&` without a name we will use it as a signal to
                // strip the last input and ignore the subsequent file block.
                while let Some(line) = current_lines.pop() {
                    let line = line.trim_right().to_owned();
                    if !line.is_empty() {
                        current_lines.push(line);
                        break;
                    }
                }
            } else if test.files.contains_key(filename) {
                // otherwise check for the duplicates.
                return Err(TestError::new(format!("duplicate file name at line {}", lineno)));
            }

            if let Some(file) = current_file {
                if !file.is_empty() { test.files.insert(file, current_lines); }
            } else {
                test.input = current_lines;
            }
            current_file = Some(filename.to_owned());
            current_lines = Vec::new();
            continue;
        }

        // otherwise we are looking at `--@` report specs (if any)
        let (line, spec) = split_line(&line, current_file.as_ref().map(|s| &s[..]),
                                      current_lines.len() + 1)?;
        current_lines.push(line.to_owned());
        if let Some(spec) = spec {
            let test = test.as_mut().ok_or_else(|| premature_err())?;
            test.reports.push(spec.into_send());
        }
    }

    flush!(next_lineno);
    Ok(tests)
}

struct TestLog {
    test: Test,
    source: Source,
    delta_only: bool, // if true, `collected` contains calculated differences only
    panicked: bool,
    output: String,
    output_mismatch: bool,
    collected: Vec<(Kind, Span, String, bool /*mismatch*/)>,
    reports_aux: Vec<bool /*mismatch*/>, // corresponds to each report in `test.reports`
}

/// The tester. It is parameterized over test-specific `Testing` implementations.
#[must_use]
pub struct Tester<T> {
    testing: T,
    features: HashSet<String>,
    filter: Option<Regex>,
    term: Box<StderrTerminal>,
    verbose: bool,
    exact_diags: bool,
    highlight_mismatch: bool,
    message_locale: Locale,
    stop_on_panic: bool,
    force: bool,
    ignore_features: bool,
    displayed_logs: Vec<TestLog>,
    num_tested: usize,
    num_passed: usize,
    num_ignored: usize,
}

const MAIN_PATH: &'static str = "<test main>";

enum TestResult { Passed, Failed, Panicked, Ignored }

impl<T: Testing> Tester<T> {
    /// Creates a new tester with given `Testing` implementation,
    /// and parses command-line options (while calling `Testing::augment_args` and
    /// `Testing::collect_args` methods if any).
    ///
    /// The name is used for command-line helps and otherwise irrelevant.
    pub fn new(name: &str, mut testing: T) -> Tester<T> {
        // `cargo test` will pass args to every tester, so we need to avoid hyphens as a prefix
        let args = env::args().map(|s| {
            let pluses = s.len() - s.trim_left_matches('+').len();
            format!("{:->pluses$}{}", "", &s[pluses..], pluses = pluses)
        });

        let app = App::new(name)
            .arg(Arg::with_name("verbose")
                .short("v")
                .long("verbose")
                .help("Displays all test outputs regardless of the result.\n\
                       Note that unexpected \"info\" reports are always displayed,\n\
                       as it is commonly an output of other flags."))
            .arg(Arg::with_name("exact_diags")
                .short("e")
                .long("exact-diags")
                .help("Requires the exact output for reports.\n\
                       Without this flag the excess reports are ignored."))
            .arg(Arg::with_name("highlight_mismatch")
                .short("h")
                .long("highlight-mismatch")
                .help("Highlights any mismatch in the reports or output."))
            .arg(Arg::with_name("message_locale")
                .short("l")
                .long("message-locale")
                .takes_value(true)
                .help("Switches to given language/locale for reports.\n\
                       Likely to fail most tests."))
            .arg(Arg::with_name("stop_on_panic")
                .short("p")
                .long("stop-on-panic")
                .multiple(true)
                .help("Stops on the first panic. Also enables `RUST_BACKTRACE=1`."))
            .arg(Arg::with_name("force_level")
                .short("f")
                .long("force")
                .multiple(true)
                .help("Run the ignored tests. \
                       Will also run feature-blocked tests when given twice."))
            .arg(Arg::with_name("filter"));
        let app = testing.augment_args(app);
        let matches = app.get_matches_from(args);

        let filter = match matches.value_of("filter") {
            Some(s) => Some(Regex::new(s).expect("pattern should be a valid regex")),
            None => None,
        };
        let verbose = matches.is_present("verbose");
        let exact_diags = matches.is_present("exact_diags");
        let highlight_mismatch = matches.is_present("highlight_mismatch");
        let message_locale = matches.value_of("message_locale").unwrap_or("en");
        let message_locale = Locale::new(message_locale).expect("unrecognized message locale");
        let stop_on_panic = matches.occurrences_of("stop_on_panic");
        let force_level = matches.occurrences_of("force_level");
        testing.collect_args(&matches);

        // for the convenience
        if stop_on_panic > 1 {
            env::set_var("RUST_BACKTRACE", "full");
        } else if stop_on_panic > 0 {
            env::set_var("RUST_BACKTRACE", "1");
        }

        let term = term::stderr().unwrap();
        Tester {
            testing: testing, features: HashSet::new(), filter: filter, term: term,
            verbose: verbose, exact_diags: exact_diags, highlight_mismatch: highlight_mismatch,
            message_locale: message_locale, stop_on_panic: stop_on_panic > 0,
            force: force_level > 0, ignore_features: force_level > 1,
            displayed_logs: Vec::new(), num_tested: 0, num_passed: 0, num_ignored: 0,
        }
    }

    /// Adds a feature (as recognized by `feature:*` test options) and enables or disables that.
    pub fn feature(mut self, name: &str, value: bool) -> Tester<T> {
        info!("feature {} is turned {}", name, if value { "on" } else { "off" });
        if value {
            self.features.insert(name.to_owned());
        }
        self
    }

    /// Scans a given directory for test files and tests any of them in the current setting.
    pub fn scan<P: AsRef<Path>>(mut self, dir: P) -> Tester<T> {
        for f in fs::read_dir(dir).expect("failed to read the test directory") {
            let path = f.expect("failed to read the test directory").path();
            if path.extension().map_or(false, |ext| ext != "lua") { continue; }

            let tests = extract_tests(&path).expect(&format!("failed to parse the test \
                                                              specification file {}",
                                                              path.display()));
            info!("extracted {} test(s) from {}", tests.len(), path.display());
            let mut file_noted = false;
            for test in tests {
                if let Some(ref filter) = self.filter {
                    if !filter.is_match(&test.name) { continue; }
                }
                if !file_noted {
                    self.note_file(&path);
                    file_noted = true;
                }
                let mut ignored = test.ignored && !self.force;
                if !test.features.is_empty() && !self.ignore_features {
                    ignored |= !test.features.iter().all(|feat| {
                        if feat.starts_with("!") {
                            !self.features.contains(&feat[1..])
                        } else {
                            self.features.contains(feat)
                        }
                    });
                }
                if ignored {
                    self.num_ignored += 1;
                    self.note_test(&test, TestResult::Ignored);
                } else {
                    self.test(test);
                }
            }
        }

        self
    }

    /// Finishes the test by printing summaries and any failed test outputs.
    /// Additionally terminates the current process with exit code 1 on any error.
    pub fn done(mut self) {
        let _ = writeln!(self.term, "");
        let _ = writeln!(self.term, "{} passed, {} ignored, {} failed",
                         self.num_passed, self.num_ignored, self.num_tested - self.num_passed);
        for log in mem::replace(&mut self.displayed_logs, Vec::new()) {
            self.note_test_output(log);
        }
        let _ = writeln!(self.term, "");

        if self.num_passed < self.num_tested {
            process::exit(1);
        }
    }

    fn string_from_panic(&self, err: Box<Any + Send>) -> String {
        if let Some(s) = err.downcast_ref::<String>() {
            s.to_string()
        } else if let Some(s) = err.downcast_ref::<&'static str>() {
            s.to_string()
        } else {
            format!("<unknown error>")
        }
    }

    fn test(&mut self, test: Test) {
        // prepare the source
        let mut source = Source::new();
        let input = SourceFile::from_u8(MAIN_PATH.to_owned(), test.input.join("\n").into_bytes());
        let inputspan = source.add(input);
        let mut filespans = HashMap::new();
        for (file, text) in test.files.iter() {
            let srcfile = SourceFile::from_u8(file.to_owned(), text.join("\n").into_bytes());
            filespans.insert(file.to_owned(), source.add(srcfile));
        }

        let source = Rc::new(RefCell::new(source));
        let collected = Rc::new(CollectedReport::new(self.message_locale));
        let output = {
            let testing = panic::AssertUnwindSafe(&mut self.testing);
            let source = panic::AssertUnwindSafe(source.clone());
            let report = panic::AssertUnwindSafe(collected.clone());
            if !self.stop_on_panic {
                panic::set_hook(Box::new(|_| {})); // suppress the default panicking message
            }
            let output = panic::catch_unwind(move || {
                testing.run(source.0, inputspan, &filespans, report.0)
            });
            panic::take_hook();
            output
        };
        let source = match Rc::try_unwrap(source) {
            Ok(src) => src.into_inner(),
            Err(_) => panic!("Testing::run should not own Source"),
        };
        let collected = match Rc::try_unwrap(collected) {
            Ok(rep) => rep.into_reports(),
            Err(_) => panic!("Testing::run should not own Report"),
        };
        self.num_tested += 1;

        // fail on a mismatching output
        let output_mismatch = if let Ok(ref output) = output {
            let expected_output = test.output.join("\n");
            !self.testing.check_output(output, &expected_output)
        } else {
            true
        };

        let translate_span = |source: &Source, span: Span| {
            source.get_file(span.unit()).and_then(|file| {
                file.lines_from_span(span).map(|(begin, _, end)| {
                    (file.path().to_owned(), begin + 1, end + 1)
                })
            })
        };

        // check if test.reports are all included in collected reports (multiset inclusion)
        // do not check if collected reports have some others, though (unless exact_diags is set)

        let mut reportset = HashMap::new(); // # of expected reports - # of collected reports
        let exact = self.exact_diags || test.exact;

        let reports_aux: Vec<_> = test.reports.iter().map(|expected| {
            let pos = expected.pos.as_ref().map(|&(ref p, s, e)| {
                (p.as_ref().map_or(MAIN_PATH, |p| &p[..]).to_owned(), s, e)
            });
            let key = (pos, expected.kind, expected.msg.clone().into_owned());
            *reportset.entry(key.clone()).or_insert(0isize) += 1;
            key
        }).collect();

        // first pass: construct a key (suitable for reportset) and update the counter
        let collected: Vec<_> = collected.into_iter().map(|(kind, span, msg)| {
            let key = (translate_span(&source, span), kind, msg);
            if let Some(value) = reportset.get_mut(&key) {
                *value -= 1;
            }
            let (loc, _, msg) = key; // move back
            (loc, kind, span, msg)
        }).collect();

        // second pass: check the counter and mark mismatches
        let mut report_mismatch = false;
        let collected: Vec<_> = collected.into_iter().map(|(loc, kind, span, msg)| {
            let key = (loc, kind, msg);

            // assume that a missing key has a negative counter
            let count = reportset.get(&key).map_or(-1, |&v| v);
            report_mismatch |= count > 0 || (exact && count < 0);

            let (_, _, msg) = key; // move back
            (kind, span, msg, count != 0)
        }).collect();

        // third pass: calculate mismatches for `test.reports` as well
        // (needed to catch reports that should occur but didn't)
        let reports_aux: Vec<_> = reports_aux.into_iter().map(|key| {
            let count = *reportset.get(&key).unwrap();
            report_mismatch |= count > 0 || (exact && count < 0);
            count != 0
        }).collect();

        if !output_mismatch && !report_mismatch {
            self.note_test(&test, TestResult::Passed);
            self.num_passed += 1;

            // we still need to display the remaining logs if -v is set or
            // unexpected reports in the collected are present
            let mut collected = collected;
            let has_delta = collected.iter().any(|&(_, _, _, mismatch)| mismatch);
            let delta_only = !self.verbose && !self.highlight_mismatch;
            if delta_only {
                // when highlight_mismatch is true, we want to see the context
                collected.retain(|&(_, _, _, mismatch)| mismatch);
            }
            if has_delta {
                self.displayed_logs.push(TestLog {
                    test: test, source: source, delta_only: delta_only, panicked: false,
                    output: output.unwrap(), output_mismatch: output_mismatch,
                    collected: collected, reports_aux: reports_aux,
                });
            }
        } else {
            let (panicked, output) = match output {
                Ok(s) => {
                    self.note_test(&test, TestResult::Failed);
                    (false, s)
                },
                Err(e) => {
                    self.note_test(&test, TestResult::Panicked);
                    if self.stop_on_panic {
                        // we need to print the line above before resuming the panic process
                        panic::resume_unwind(e);
                    } else {
                        (true, self.string_from_panic(e))
                    }
                },
            };
            self.displayed_logs.push(TestLog {
                test: test, source: source, delta_only: false, panicked: panicked,
                output: output, output_mismatch: output_mismatch,
                collected: collected, reports_aux: reports_aux,
            });
        }
    }

    fn note_file(&mut self, path: &Path) {
        let _ = writeln!(self.term, "");
        let _ = self.term.fg(term::color::BRIGHT_MAGENTA);
        let _ = write!(self.term, "          {}", path.display());
        let _ = self.term.reset();
        let _ = writeln!(self.term, "");
    }

    fn note_test(&mut self, test: &Test, result: TestResult) {
        let (fg, bg, text) = match result {
            TestResult::Passed => (term::color::BRIGHT_GREEN, None, "PASSED"),
            TestResult::Failed => (term::color::BRIGHT_RED, None, "FAILED"),
            TestResult::Panicked => (term::color::BLACK, Some(term::color::RED), "PANIC"),
            TestResult::Ignored => (term::color::BRIGHT_BLACK, None, "IGNORE"),
        };
        if let Some(bg) = bg {
            let _ = write!(self.term, "  ");
            let _ = self.term.fg(fg);
            let _ = self.term.bg(bg);
            let _ = write!(self.term, "{}", text);
            let _ = self.term.reset();
            let _ = write!(self.term, "{:1$}  ", "", 6 - text.len());
        } else {
            let _ = self.term.fg(fg);
            let _ = write!(self.term, "  {:<6}  ", text);
            let _ = self.term.reset();
        }
        let _ = self.term.fg(term::color::BRIGHT_WHITE);
        if !test.name.is_empty() {
            let _ = write!(self.term, "{}", test.name);
        } else {
            let _ = write!(self.term, "<anonymous test at line {}>", test.first_line);
        }
        let _ = self.term.reset();
        let _ = writeln!(self.term, "");
    }

    fn mark_mismatch(&mut self, mismatch: bool) {
        if self.highlight_mismatch && mismatch {
            let _ = self.term.bg(term::color::BRIGHT_BLACK);
        }
    }

    fn note_test_output(&mut self, log: TestLog) {
        let _ = writeln!(self.term, "");
        let _ = self.term.fg(term::color::BRIGHT_MAGENTA);
        let _ = write!(self.term, "{} ", log.test.file.display());
        let _ = self.term.fg(term::color::BRIGHT_WHITE);
        if !log.test.name.is_empty() {
            let _ = write!(self.term, "{} ", log.test.name);
        }
        let _ = self.term.reset();
        let _ = writeln!(self.term, "(at line {})", log.test.first_line);

        let _ = self.term.fg(term::color::BRIGHT_BLACK);
        if !log.delta_only {
            let _ = writeln!(self.term, "{:-<60}", "EXPECTED ");
            self.mark_mismatch(log.output_mismatch);
            let _ = self.term.fg(term::color::BRIGHT_WHITE);
            for (i, line) in log.test.output.into_iter().enumerate() {
                if i > 0 {
                    let _ = writeln!(self.term, "");
                }
                let _ = write!(self.term, "{}", line);
            }
            let _ = self.term.reset();
            let _ = writeln!(self.term, "");
            if !log.test.reports.is_empty() {
                let _ = writeln!(self.term, "");
                let reports = log.test.reports.into_iter().zip(log.reports_aux.into_iter());
                for (expected, mismatch) in reports {
                    self.mark_mismatch(mismatch);

                    if let Some((path, begin, end)) = expected.pos {
                        let path = path.as_ref().map_or(MAIN_PATH, |p| p.as_ref());
                        if begin == end {
                            let _ = write!(self.term, "{}:{}:_: ", path, begin);
                        } else {
                            let _ = write!(self.term, "{}:{}:_: {}:_ ", path, begin, end);
                        }
                    }

                    let (dim, bright) = expected.kind.colors();
                    let _ = self.term.fg(dim);
                    let _ = write!(self.term, "[");
                    let _ = self.term.fg(bright);
                    let _ = write!(self.term, "{:?}", expected.kind);
                    let _ = self.term.fg(dim);
                    let _ = write!(self.term, "] ");
                    let _ = self.term.fg(term::color::BRIGHT_WHITE);
                    let _ = write!(self.term, "{}", expected.msg);
                    let _ = self.term.reset();
                    let _ = writeln!(self.term, "");
                }
            }

            let _ = self.term.reset();
            let _ = self.term.fg(term::color::BRIGHT_BLACK);
            let _ = writeln!(self.term, "{:-<60}", "ACTUAL ");
            if log.panicked {
                let _ = self.term.bg(term::color::RED);
                let _ = self.term.fg(term::color::BLACK);
                let _ = write!(self.term, "PANICKED");
                let _ = self.term.reset();
                let _ = self.term.fg(term::color::BRIGHT_RED);
                let _ = write!(self.term, " ");
            } else {
                self.mark_mismatch(log.output_mismatch);
                let _ = self.term.fg(term::color::BRIGHT_WHITE);
            }
            let _ = write!(self.term, "{}", log.output);
            let _ = self.term.reset();
            let _ = writeln!(self.term, "");
        } else {
            let _ = writeln!(self.term, "{:-<60}", "ACTUAL (DIFF FROM EXPECTED) ");
        }

        let _ = self.term.reset();
        if !log.collected.is_empty() {
            if !log.delta_only {
                let _ = writeln!(self.term, "");
            }
            let source = Rc::new(RefCell::new(log.source));
            let display = kailua_diag::ConsoleReport::new(source);
            for (kind, span, msg, mismatch) in log.collected {
                self.mark_mismatch(mismatch);
                let _ = display.add_span(kind, span, &msg);
            }
        }

        let _ = self.term.reset();
        let _ = self.term.fg(term::color::BRIGHT_BLACK);
        let _ = writeln!(self.term, "{:-<60}", "");
        let _ = self.term.reset();
    }
}

