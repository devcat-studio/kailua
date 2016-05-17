use std::str;
use std::cmp;
use std::result;
use std::cell::{Cell, RefCell};

use source::{Source, Span, Pos};
use dummy_term::stderr_or_dummy;
use term::{color, StderrTerminal};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Kind {
    Note,
    Warn,
    Error,
    Fatal,
}

// used to stop the further parsing or type checking
//#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
//pub struct Stop;
pub type Stop = &'static str; // TODO should be eliminated!

pub type Result<T> = result::Result<T, Stop>;

pub trait Report {
    fn add_span(&self, kind: Kind, span: Span, msg: String) -> Result<()>;
}

impl<'a, R: Report> Report for &'a R {
    fn add_span(&self, k: Kind, s: Span, m: String) -> Result<()> { (**self).add_span(k, s, m) }
}

impl<'a> Report for &'a Report {
    fn add_span(&self, k: Kind, s: Span, m: String) -> Result<()> { (**self).add_span(k, s, m) }
}

pub trait Reporter: Report {
    fn fatal<Loc: Into<Span>, Msg: Into<String>, T>(&self, loc: Loc, msg: Msg) -> Result<T> {
        self.add_span(Kind::Fatal, loc.into(), msg.into())
            .map(|_| panic!("Report::fatal should always return Err"))
    }

    fn error<Loc: Into<Span>, Msg: Into<String>>(&self, loc: Loc, msg: Msg) -> Result<()> {
        self.add_span(Kind::Error, loc.into(), msg.into())
    }

    fn warn<Loc: Into<Span>, Msg: Into<String>>(&self, loc: Loc, msg: Msg) -> Result<()> {
        self.add_span(Kind::Warn, loc.into(), msg.into())
    }

    fn note<Loc: Into<Span>, Msg: Into<String>>(&self, loc: Loc, msg: Msg) -> Result<()> {
        self.add_span(Kind::Note, loc.into(), msg.into())
    }
}

impl<T: Report> Reporter for T {}

fn strip_newline(mut s: &[u8]) -> &[u8] {
    loop {
        match s.last() {
            Some(&b'\r') | Some(&b'\n') => { s = &s[..s.len()-1]; }
            _ => return s,
        }
    }
}

pub struct ConsoleReport<'a> {
    source: &'a Source,
    term: RefCell<Box<StderrTerminal>>,
    maxkind: Cell<Option<Kind>>,
}

impl<'a> ConsoleReport<'a> {
    pub fn new(source: &'a Source) -> ConsoleReport<'a> {
        ConsoleReport {
            source: source,
            term: RefCell::new(stderr_or_dummy()),
            maxkind: Cell::new(None),
        }
    }

    pub fn max_kind_seen(&self) -> Option<Kind> {
        self.maxkind.get()
    }

    // column number starts from 0
    // the final newlines are ignored and not counted towards columns
    fn calculate_column(&self, linespan: Span, pos: Pos) -> usize {
        assert!(linespan.contains_or_end(pos));
        let off = pos.to_usize() - linespan.begin().to_usize();

        let line = strip_newline(self.source.bytes_from_span(linespan));
        if let Ok(line) = str::from_utf8(line) {
            // it is a UTF-8 string, use unicode-width
            use unicode_width::UnicodeWidthChar;
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
}

impl<'a> Report for ConsoleReport<'a> {
    fn add_span(&self, kind: Kind, span: Span, msg: String) -> Result<()> {
        let mut term = self.term.borrow_mut();
        let term = &mut *term;

        let mut codeinfo = None;
        if let Some(f) = self.source.file_from_span(span) {
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

        let (dim, bright, text) = match kind {
            Kind::Fatal => (color::RED, color::BRIGHT_RED, "Fatal"),
            Kind::Error => (color::RED, color::BRIGHT_RED, "Error"),
            Kind::Warn => (color::YELLOW, color::BRIGHT_YELLOW, "Warning"),
            Kind::Note => (color::CYAN, color::BRIGHT_CYAN, "Note"),
        };
        let _ = term.fg(dim);
        let _ = write!(term, "[");
        let _ = term.fg(bright);
        let _ = write!(term, "{}", text);
        let _ = term.fg(dim);
        let _ = write!(term, "] ");
        let _ = term.fg(color::BRIGHT_WHITE);
        let _ = write!(term, "{}", msg);
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
                if begin > 0 {
                    let _ = term.write(&bytes[..begin]);
                }
                let _ = term.fg(bright);
                let _ = term.write(&bytes[begin..end]);
                let _ = term.reset();
                if end < bytes.len() {
                    let _ = term.write(&bytes[end..]);
                }
            };

            if beginline == endline {
                let bytes = strip_newline(self.source.bytes_from_span(beginspan));
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
                let beginbytes = strip_newline(self.source.bytes_from_span(beginspan));
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
                let endbytes = strip_newline(self.source.bytes_from_span(endspan));
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

        if kind == Kind::Fatal { Err("fatal error") } else { Ok(()) }
    }
}

