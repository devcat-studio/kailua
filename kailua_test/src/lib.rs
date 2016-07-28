extern crate regex;
#[macro_use] extern crate lazy_static;
extern crate term;
extern crate kailua_diag;
#[macro_use] extern crate log;

use std::str;
use std::fmt;
use std::env;
use std::fs;
use std::mem;
use std::process;
use std::ascii::AsciiExt;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::Rc;
use std::fs::File;
use std::error::Error;
use std::collections::HashMap;
use regex::Regex;
use term::StderrTerminal;
use kailua_diag::{Source, Kind, Span, Report, CollectedReport};

pub trait Testing {
    fn run(&self, source: Rc<RefCell<Source>>, span: Span, filespans: &HashMap<String, Span>,
           report: Rc<Report>) -> String;

    fn check_output(&self, actual: &str, expected: &str) -> bool { actual == expected }
}

#[derive(Debug)]
pub struct TestError {
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
pub struct Expected<'a> {
    // the path is None when it is a main file (the tester will fill the temporary name)
    pub pos: Option<(Option<Cow<'a, str>>, usize, usize)>,
    pub kind: Kind,
    pub msg: Cow<'a, str>,
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
        let line = m.name("line").unwrap();

        let parse_lineno = |s: &str| {
            if s.starts_with("<") {
                Ok(lineno)
            } else if s.starts_with("^") {
                let lineno = try!(lineno.checked_sub(s.len()).ok_or_else(|| err()));
                if lineno == 0 { return Err(err()); }
                Ok(lineno)
            } else if s.starts_with("v") {
                lineno.checked_add(s.len()).ok_or_else(|| err())
            } else {
                s.parse().map_err(|e| erre(e))
            }
        };

        let pos = if let Some(line1) = m.name("line1") {
            let line1 = try!(parse_lineno(line1));
            let line2 = if let Some(line2) = m.name("line2") {
                try!(parse_lineno(line2))
            } else {
                line1
            };
            if line1 > line2 || line1 == 0 { return Err(err()); }
            Some((line1, line2))
        } else {
            None
        };
        let pos = pos.map(|(s,e)| (file.map(|f| f.into()), s, e));
        let kind = try!(kind_from_str(m.name("kind").unwrap()).ok_or_else(|| err()));
        let msg = m.name("msg").unwrap().trim();
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
pub struct Test {
    pub file: PathBuf,
    pub first_line: usize, // the first line after the divider
    pub name: String, // can be an empty string
    pub input: Vec<String>,
    pub files: HashMap<String, Vec<String>>,
    pub output: Vec<String>,
    pub reports: Vec<Expected<'static>>,
    pub ignored: bool,
}

fn extract_tests(path: &Path) -> Result<Vec<Test>, TestError> {
    let f = try!(File::open(path).map_err(Box::new));
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
        let line = try!(line.map_err(Box::new));
        let lineno = lineno + 1;
        next_lineno = lineno + 1;

        let premature_err = || TestError::new(format!("found test spec before the first \
                                                       `--8<--` at line {}", lineno));

        // try to look at the divider (`-*--8<---* <test name> -*`)
        if let Some(pos) = line.find("--8<--").or_else(|| line.find("-->8--")) {
            if line[..pos].chars().all(|c| c == '-') {
                // we've found a divider, flush the current test
                flush!(lineno);

                let name = line[pos+6..].trim_matches(|c: char| c == '-' || c.is_whitespace());
                let ignored = &line[pos..pos+6] == "-->8--";
                test = Some(Test {
                    file: path.to_owned(),
                    first_line: next_lineno,
                    name: name.to_owned(),
                    input: Vec::new(),
                    files: HashMap::new(),
                    output: Vec::new(),
                    reports: Vec::new(),
                    ignored: ignored,
                });
                current_file = None;
                current_lines = Vec::new();
                continue;
            }
        }

        // `--! output` specifies the desired output
        // `--! output \` is same, but the trailing \ and the subsequent newline is stripped
        if let Some(pos) = line.find("--!") {
            let test = try!(test.as_mut().ok_or_else(|| premature_err()));

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
            let test = try!(test.as_mut().ok_or_else(|| premature_err()));

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
        let (line, spec) = try!(split_line(&line, current_file.as_ref().map(|s| &s[..]),
                                           current_lines.len() + 1));
        current_lines.push(line.to_owned());
        if let Some(spec) = spec {
            let test = try!(test.as_mut().ok_or_else(|| premature_err()));
            test.reports.push(spec.into_send());
        }
    }

    flush!(next_lineno);
    Ok(tests)
}

#[must_use]
pub struct Tester<T> {
    testing: T,
    filter: Option<Regex>,
    term: Box<StderrTerminal>,
    exact_diags: bool,
    message_lang: String,
    failed_logs: Vec<(Test, Source, String, Vec<(Kind, Span, String)>)>,
    num_tested: usize,
    num_passed: usize,
}

const MAIN_PATH: &'static str = "<test main>";

enum TestResult { Passed, Failed, Ignored }

impl<T: Testing> Tester<T> {
    pub fn new(testing: T) -> Tester<T> {
        let args: Vec<_> = env::args().skip(1).collect();
        let term = term::stderr().unwrap();
        let filter = if args.len() > 0 {
            Some(Regex::new(&args[0]).expect("pattern should be a valid regex"))
        } else {
            None
        };
        let exact_diags = match env::var("KAILUA_TEST_EXACT_DIAGS") {
            Ok(s) => !s.is_empty(),
            Err(_) => false,
        };
        let message_lang = match env::var("KAILUA_TEST_MESSAGE_LANG") {
            Ok(s) => s.to_lowercase(),
            Err(_) => String::from("en"),
        };
        Tester {
            testing: testing, filter: filter, term: term,
            exact_diags: exact_diags, message_lang: message_lang,
            failed_logs: Vec::new(), num_tested: 0, num_passed: 0,
        }
    }

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
                if test.ignored {
                    self.note_test(&test, TestResult::Ignored);
                } else {
                    self.test(test);
                }
            }
        }

        self
    }

    pub fn done(mut self) {
        let _ = writeln!(self.term, "");
        let _ = writeln!(self.term, "{} passed, {} failed",
                         self.num_passed, self.num_tested - self.num_passed);
        for (test, source, output, collected) in mem::replace(&mut self.failed_logs, Vec::new()) {
            self.note_failed_test(test, source, output, collected);
        }
        let _ = writeln!(self.term, "");

        if self.num_passed < self.num_tested {
            process::exit(1);
        }
    }

    fn test(&mut self, test: Test) {
        // prepare the source
        let mut source = Source::new();
        let inputspan = source.add_string(MAIN_PATH, test.input.join("\n").as_bytes());
        let mut filespans = HashMap::new();
        for (file, text) in test.files.iter() {
            filespans.insert(file.to_owned(), source.add_string(file, text.join("\n").as_bytes()));
        }

        let source = Rc::new(RefCell::new(source));
        let collected = Rc::new(CollectedReport::new(self.message_lang.clone()));
        let output = self.testing.run(source.clone(), inputspan, &filespans, collected.clone());
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
        let mut failed = false;
        let expected_output = test.output.join("\n");
        if !self.testing.check_output(&output, &expected_output) {
            failed = true;
        }

        // check if test.reports are all included in collected reports (multiset inclusion)
        // do not check if collected reports have some others, though (unless exact_diags is set)
        if !failed {
            let mut reportset = HashMap::new();
            for expected in &test.reports {
                let pos = expected.pos.as_ref().map(|&(ref p, s, e)| {
                    (p.as_ref().map_or(MAIN_PATH, |p| &p[..]).to_owned(), s, e)
                });
                let key = (pos, expected.kind, &expected.msg[..]);
                *reportset.entry(key).or_insert(0isize) += 1;
            }
            for &(kind, span, ref msg) in &collected {
                let pos = source.file_from_span(span).and_then(|file| {
                    file.lines_from_span(span).map(|(begin, _, end)| {
                        (file.path().to_owned(), begin + 1, end + 1)
                    })
                });
                let key = (pos, kind, &msg[..]);
                if let Some(value) = reportset.get_mut(&key) {
                    *value -= 1;
                } else if self.exact_diags {
                    failed = true; // seen a note we haven't expected
                }
            }
            if !failed {
                failed = if self.exact_diags {
                    reportset.values().any(|&v| v != 0)
                } else {
                    reportset.values().any(|&v| v > 0)
                };
            }
        }

        if failed {
            self.note_test(&test, TestResult::Failed);
            self.failed_logs.push((test, source, output, collected));
        } else {
            self.note_test(&test, TestResult::Passed);
            self.num_passed += 1;
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
        let (color, text) = match result {
            TestResult::Passed => (term::color::BRIGHT_GREEN, "PASSED"),
            TestResult::Failed => (term::color::BRIGHT_RED, "FAILED"),
            TestResult::Ignored => (term::color::BRIGHT_BLACK, "IGNORE"),
        };
        let _ = self.term.fg(color);
        let _ = write!(self.term, "  {}  ", text);
        let _ = self.term.fg(term::color::BRIGHT_WHITE);
        if !test.name.is_empty() {
            let _ = write!(self.term, "{}", test.name);
        } else {
            let _ = write!(self.term, "<anonymous test at line {}>", test.first_line);
        }
        let _ = self.term.reset();
        let _ = writeln!(self.term, "");
    }

    fn note_failed_test(&mut self, test: Test, source: Source, output: String,
                        collected: Vec<(Kind, Span, String)>) {
        let _ = writeln!(self.term, "");
        let _ = self.term.fg(term::color::BRIGHT_MAGENTA);
        let _ = write!(self.term, "{} ", test.file.display());
        let _ = self.term.fg(term::color::BRIGHT_WHITE);
        if !test.name.is_empty() {
            let _ = write!(self.term, "{} ", test.name);
        }
        let _ = self.term.reset();
        let _ = writeln!(self.term, "(at line {})", test.first_line);
        let _ = self.term.fg(term::color::BRIGHT_BLACK);
        let _ = writeln!(self.term, "{:-<60}", "EXPECTED ");
        let _ = self.term.fg(term::color::BRIGHT_WHITE);
        for line in test.output {
            let _ = writeln!(self.term, "{}", line);
        }
        let _ = self.term.reset();
        if !test.reports.is_empty() {
            let _ = writeln!(self.term, "");
            for expected in test.reports {
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
        let _ = self.term.fg(term::color::BRIGHT_BLACK);
        let _ = writeln!(self.term, "{:-<60}", "ACTUAL ");
        let _ = self.term.fg(term::color::BRIGHT_WHITE);
        let _ = writeln!(self.term, "{}", output);
        let _ = self.term.reset();
        if !collected.is_empty() {
            let _ = writeln!(self.term, "");
            let source = Rc::new(RefCell::new(source));
            let display = kailua_diag::ConsoleReport::new(source);
            for (kind, span, msg) in collected {
                let _ = display.add_span(kind, span, &msg);
            }
        }
        let _ = self.term.fg(term::color::BRIGHT_BLACK);
        let _ = writeln!(self.term, "{:-<60}", "");
        let _ = self.term.reset();
    }
}

