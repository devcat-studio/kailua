use std::str;
use std::cmp;
use std::result;
use std::cell::{Cell, RefCell};
use std::rc::Rc;
use unicode_width::UnicodeWidthChar;

use source::{Source, Span, Pos};
use dummy_term::{stderr_or_dummy};
use term::{color, StderrTerminal};
use message::{Localize, Localized, get_message_language};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Kind {
    Note,
    Warning,
    Error,
    Fatal,
}

impl Kind {
    pub fn colors(&self) -> (/*dim*/ color::Color, /*bright*/ color::Color) {
        match *self {
            Kind::Fatal => (color::RED, color::BRIGHT_RED),
            Kind::Error => (color::RED, color::BRIGHT_RED),
            Kind::Warning => (color::YELLOW, color::BRIGHT_YELLOW),
            Kind::Note => (color::CYAN, color::BRIGHT_CYAN),
        }
    }
}

// used to stop the further parsing or type checking
#[must_use]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Stop;

// XXX stop gap to aid kailua_check's transition to Stop type
impl From<Stop> for String {
    fn from(_: Stop) -> String { format!("stop requested") }
}

pub type Result<T> = result::Result<T, Stop>;

pub trait Report {
    fn add_span(&self, kind: Kind, span: Span, msg: &Localize) -> Result<()>;
    fn can_continue(&self) -> bool;
}

impl<'a, R: Report> Report for &'a R {
    fn add_span(&self, k: Kind, s: Span, m: &Localize) -> Result<()> { (**self).add_span(k, s, m) }
    fn can_continue(&self) -> bool { (**self).can_continue() }
}

impl<'a, R: Report> Report for &'a mut R {
    fn add_span(&self, k: Kind, s: Span, m: &Localize) -> Result<()> { (**self).add_span(k, s, m) }
    fn can_continue(&self) -> bool { (**self).can_continue() }
}

impl<'a> Report for &'a Report {
    fn add_span(&self, k: Kind, s: Span, m: &Localize) -> Result<()> { (**self).add_span(k, s, m) }
    fn can_continue(&self) -> bool { (**self).can_continue() }
}

impl<'a> Report for Rc<Report> {
    fn add_span(&self, k: Kind, s: Span, m: &Localize) -> Result<()> { (**self).add_span(k, s, m) }
    fn can_continue(&self) -> bool { (**self).can_continue() }
}

pub trait Reporter: Report + Sized {
    fn fatal<Loc: Into<Span>, Msg: Localize, T>(&self, loc: Loc, msg: Msg) -> ReportMore<T> {
        info!("reporting fatal error: {:?}", msg);
        let ret = self.add_span(Kind::Fatal, loc.into(), &msg);
        let ret = ret.map(|_| panic!("Report::fatal should always return Err"));
        ReportMore::new(self, ret)
    }

    fn error<Loc: Into<Span>, Msg: Localize>(&self, loc: Loc, msg: Msg) -> ReportMore<()> {
        info!("reporting error: {:?}", msg);
        let ret = self.add_span(Kind::Error, loc.into(), &msg);
        ReportMore::new(self, ret)
    }

    fn warn<Loc: Into<Span>, Msg: Localize>(&self, loc: Loc, msg: Msg) -> ReportMore<()> {
        info!("reporting warning: {:?}", msg);
        let ret = self.add_span(Kind::Warning, loc.into(), &msg);
        ReportMore::new(self, ret)
    }

    fn note<Loc: Into<Span>, Msg: Localize>(&self, loc: Loc, msg: Msg) -> ReportMore<()> {
        info!("reporting note: {:?}", msg);
        let ret = self.add_span(Kind::Note, loc.into(), &msg);
        ReportMore::new(self, ret)
    }
}

impl<T: Report> Reporter for T {}

#[must_use]
pub struct ReportMore<'a, T> {
    report: &'a Report,
    result: Result<T>,
}

impl<'a, T> ReportMore<'a, T> {
    fn new(report: &'a Report, result: Result<T>) -> ReportMore<'a, T> {
        ReportMore { report: report, result: result }
    }

    pub fn fatal<Loc: Into<Span>, Msg: Localize, U>(self, loc: Loc, msg: Msg) -> ReportMore<'a, U> {
        let ret = self.report.fatal(loc, msg).result;
        ReportMore::new(self.report, self.result.and(ret))
    }

    pub fn error<Loc: Into<Span>, Msg: Localize>(self, loc: Loc, msg: Msg) -> ReportMore<'a, T> {
        let ret = self.report.error(loc, msg).result;
        ReportMore::new(self.report, if let Err(e) = ret { Err(e) } else { self.result })
    }

    pub fn warn<Loc: Into<Span>, Msg: Localize>(self, loc: Loc, msg: Msg) -> ReportMore<'a, T> {
        let ret = self.report.warn(loc, msg).result;
        ReportMore::new(self.report, if let Err(e) = ret { Err(e) } else { self.result })
    }

    pub fn note<Loc: Into<Span>, Msg: Localize>(self, loc: Loc, msg: Msg) -> ReportMore<'a, T> {
        let ret = self.report.note(loc, msg).result;
        ReportMore::new(self.report, if let Err(e) = ret { Err(e) } else { self.result })
    }

    pub fn note_if<Loc: Into<Span>, Msg: Localize>(self, loc: Loc, msg: Msg) -> ReportMore<'a, T> {
        let loc = loc.into();
        if loc.is_dummy() {
            self
        } else {
            let ret = self.report.note(loc, msg).result;
            ReportMore::new(self.report, if let Err(e) = ret { Err(e) } else { self.result })
        }
    }

    pub fn done(self) -> Result<T> { self.result }
}

fn strip_newline(mut s: &[u8]) -> &[u8] {
    loop {
        match s.last() {
            Some(&b'\r') | Some(&b'\n') => { s = &s[..s.len()-1]; }
            _ => return s,
        }
    }
}

pub struct ConsoleReport {
    source: Rc<RefCell<Source>>,
    term: RefCell<Box<StderrTerminal>>,
    maxkind: Cell<Option<Kind>>,
    lang: String,
}

impl ConsoleReport {
    pub fn new(source: Rc<RefCell<Source>>) -> ConsoleReport {
        let lang = get_message_language().unwrap_or_else(|| String::new());
        ConsoleReport::with_lang(source, lang)
    }

    pub fn with_lang(source: Rc<RefCell<Source>>, lang: String) -> ConsoleReport {
        ConsoleReport {
            source: source,
            term: RefCell::new(stderr_or_dummy()),
            maxkind: Cell::new(None),
            lang: lang,
        }
    }

    pub fn lang(&self) -> &str {
        &self.lang
    }

    // column number starts from 0
    // the final newlines are ignored and not counted towards columns
    fn calculate_column(&self, linespan: Span, pos: Pos) -> usize {
        assert!(linespan.contains_or_end(pos));
        let off = pos.to_usize() - linespan.begin().to_usize();

        let source = self.source.borrow();
        let line = strip_newline(source.bytes_from_span(linespan));
        if let Ok(line) = str::from_utf8(line) {
            // it is a UTF-8 string, use unicode-width
            let mut lastcol = 0;
            let mut col = 0;
            for (i, c) in line.char_indices() {
                if off < i { return lastcol; } // the last character was at (or contained) pos
                lastcol = col;
                if c == '\t' {
                    // assume 8-space tabs (common in terminals)
                    col = (col + 8) & !7; // 0..7->8, 8..15->16, ...
                } else {
                    col += c.width_cjk().unwrap_or(1);
                }
            }
            // the else case is possible if pos points past the newlines
            if off < line.len() { lastcol } else { col }
        } else {
            // otherwise it is in the legacy encodings.
            // fortunately for us the column width and byte width for those encodings
            // generally agrees to each other, so we just use the byte offset
            let mut col = 0;
            for (i, &c) in line.iter().enumerate() {
                if off <= i { return col; }
                if c == b'\t' {
                    col = (col + 8) & !7;
                } else {
                    col += 1;
                }
            }
            col
        }
    }

    // similar to calculate_column but expands tab in `line`
    fn expand_tab_in_str(&self, line: &str, next_col: &mut usize) -> Vec<u8> {
        let mut col = *next_col;
        let mut ret = String::new();
        for c in line.chars() {
            if c == '\t' {
                let newcol = (col + 8) & !7;
                for _ in col..newcol { ret.push(' '); }
                col = newcol;
            } else {
                col += c.width_cjk().unwrap_or(1);
                ret.push(c);
            }
        }
        *next_col = col;
        ret.into_bytes()
    }

    fn expand_tab_in_bytes(&self, line: &[u8], next_col: &mut usize) -> Vec<u8> {
        let mut col = *next_col;
        let mut ret = Vec::new();
        for &c in line.iter() {
            if c == b'\t' {
                let newcol = (col + 8) & !7;
                for _ in col..newcol { ret.push(b' '); }
                col = newcol;
            } else {
                col += 1;
                ret.push(c);
            }
        }
        *next_col = col;
        ret
    }
}

impl Report for ConsoleReport {
    fn add_span(&self, kind: Kind, span: Span, msg: &Localize) -> Result<()> {
        let mut term = self.term.borrow_mut();
        let term = &mut *term;
        let source = self.source.borrow();

        let mut codeinfo = None;
        if let Some(f) = source.file_from_span(span) {
            if let Some((beginline, mut spans, endline)) = f.lines_from_span(span) {
                let beginspan = spans.next().unwrap();
                let begincol = self.calculate_column(beginspan, span.begin());
                let endspan = spans.next_back().unwrap_or(beginspan);
                let endcol = self.calculate_column(endspan, span.end());
                let _ = write!(term, "{}:{}:{}: ", f.path(), beginline + 1, begincol + 1);
                if span.begin() != span.end() {
                    let _ = write!(term, "{}:{} ", endline + 1, endcol + 1);
                }
                codeinfo = Some((beginline, begincol, beginspan, endline, endcol, endspan));
            }
        }

        let (dim, bright) = kind.colors();
        let _ = term.fg(dim);
        let _ = write!(term, "[");
        let _ = term.fg(bright);
        let _ = write!(term, "{:?}", kind);
        let _ = term.fg(dim);
        let _ = write!(term, "] ");
        let _ = term.fg(color::BRIGHT_WHITE);
        let _ = write!(term, "{}", Localized::new(msg, &self.lang));
        let _ = term.reset();
        let _ = writeln!(term, "");

        // if possible, print the source code as well
        if let Some((beginline, begincol, beginspan, endline, endcol, endspan)) = codeinfo {
            fn num_digits(mut x: usize) -> usize {
                let mut d = 1;
                while x > 9 { x /= 10; d += 1; }
                d
            }

            type Term<'a> = &'a mut Box<StderrTerminal>;

            let write_newline = |term: Term| {
                let _ = writeln!(term, "");
            };

            let ndigits = num_digits(endline + 1);
            let write_lineno = |term: Term, lineno| {
                let _ = term.fg(color::BRIGHT_BLACK);
                let _ = write!(term, "{:1$} | ", lineno + 1, ndigits);
                let _ = term.reset();
            };
            let write_lineno_empty = |term: Term| {
                let _ = term.fg(color::BRIGHT_BLACK);
                let _ = write!(term, "{:1$} | ", "", ndigits);
                let _ = term.reset();
            };
            let write_lineno_omitted = |term: Term| {
                let _ = term.fg(color::BRIGHT_BLACK);
                let _ = write!(term, "{:1$} :", "", ndigits);
                let _ = term.reset();
            };

            let write_bytes = |term: Term, bytes: &[u8], begin, end| {
                if let Ok(line) = str::from_utf8(bytes) {
                    let mut col = 0;
                    if begin > 0 {
                        let _ = term.write(&self.expand_tab_in_str(&line[..begin], &mut col));
                    }
                    let _ = term.fg(bright);
                    let _ = term.write(&self.expand_tab_in_str(&line[begin..end], &mut col));
                    let _ = term.reset();
                    if end < bytes.len() {
                        let _ = term.write(&self.expand_tab_in_str(&line[end..], &mut col));
                    }
                } else {
                    let mut col = 0;
                    if begin > 0 {
                        let _ = term.write(&self.expand_tab_in_bytes(&bytes[..begin], &mut col));
                    }
                    let _ = term.fg(bright);
                    let _ = term.write(&self.expand_tab_in_bytes(&bytes[begin..end], &mut col));
                    let _ = term.reset();
                    if end < bytes.len() {
                        let _ = term.write(&self.expand_tab_in_bytes(&bytes[end..], &mut col));
                    }
                }
            };

            if beginline == endline {
                let bytes = strip_newline(source.bytes_from_span(beginspan));
                let beginoff = span.begin().to_usize() - beginspan.begin().to_usize();
                let endoff = span.end().to_usize() - beginspan.begin().to_usize();

                // 123 | aaaabbbbbb     begincol = endcol
                //     |     *
                //
                // 123 | aaaaXXXXXbbb   begincol < endcol
                //     |     ^^^^^
                write_lineno(term, beginline);
                write_bytes(term, bytes, beginoff, endoff);
                write_newline(term);
                write_lineno_empty(term);
                let _ = term.fg(bright);
                if begincol == endcol {
                    let _ = write!(term, "{:1$}*", "", begincol);
                } else {
                    let _ = write!(term, "{:2$}{:^>3$}", "", "", begincol, endcol - begincol);
                }
                let _ = term.reset();
                write_newline(term);
            } else {
                // 123 | aaaaXXXXXXXX
                //     |     ^ from here...
                let beginbytes = strip_newline(source.bytes_from_span(beginspan));
                let beginoff = span.begin().to_usize() - beginspan.begin().to_usize();
                write_lineno(term, beginline);
                write_bytes(term, beginbytes, beginoff, beginbytes.len());
                write_newline(term);
                write_lineno_empty(term);
                let _ = term.fg(bright);
                let _ = write!(term, "{:1$}^", "", begincol);
                let _ = term.fg(dim);
                let _ = write!(term, " from here...");
                let _ = term.reset();
                write_newline(term);

                if endline - beginline > 1 {
                    write_lineno_omitted(term);
                    write_newline(term);
                }

                // 321 | bbbbbbbbbb     endcol = 0
                //     | * ...to here
                //
                // 321 | XXXXXbbbbb     endcol > 0
                //     |     ^ to here
                let endbytes = strip_newline(source.bytes_from_span(endspan));
                let endoff = span.end().to_usize() - endspan.begin().to_usize();
                write_lineno(term, endline);
                write_bytes(term, endbytes, 0, endoff);
                write_newline(term);
                write_lineno_empty(term);
                let _ = term.fg(bright);
                if endcol == 0 {
                    let _ = write!(term, "*");
                } else {
                    let _ = write!(term, "{:1$}^", "", endcol - 1);
                }
                let _ = term.fg(dim);
                let _ = write!(term, " ...to here");
                let _ = term.reset();
                write_newline(term);
            }
        }

        if let Some(maxkind) = self.maxkind.get() {
            self.maxkind.set(Some(cmp::max(maxkind, kind)));
        } else {
            self.maxkind.set(Some(kind));
        }

        if kind == Kind::Fatal { Err(Stop) } else { Ok(()) }
    }

    fn can_continue(&self) -> bool {
        self.maxkind.get() < Some(Kind::Error)
    }
}

pub struct CollectedReport {
    collected: RefCell<Vec<(Kind, Span, String)>>,
    lang: String,
}

impl CollectedReport {
    pub fn new(lang: String) -> CollectedReport {
        CollectedReport { collected: RefCell::new(Vec::new()), lang: lang }
    }

    pub fn into_reports(self) -> Vec<(Kind, Span, String)> {
        self.collected.into_inner()
    }
}

impl Report for CollectedReport {
    fn add_span(&self, kind: Kind, span: Span, msg: &Localize) -> Result<()> {
        let msg = Localized::new(msg, &self.lang).to_string();
        self.collected.borrow_mut().push((kind, span, msg));
        if kind == Kind::Fatal { Err(Stop) } else { Ok(()) }
    }

    fn can_continue(&self) -> bool {
        self.collected.borrow().iter().all(|&(kind, _, _)| kind < Kind::Error)
    }
}

pub struct NoReport;

impl Report for NoReport {
    fn add_span(&self, _kind: Kind, _span: Span, _msg: &Localize) -> Result<()> { Err(Stop) }
    fn can_continue(&self) -> bool { true }
}

