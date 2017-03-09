use std::ops;
use std::fmt;
use std::hash;
use std::sync::Arc;
use std::collections::HashSet;
use parking_lot::{Mutex, MutexGuard};

use kailua_env::{Pos, Span, Source, SourceFile, SourceSlice};
use kailua_diag::{self, Kind, Report, Locale, Localize, Localized};

use protocol::{Position, Range, DiagnosticSeverity, Diagnostic};

pub fn translate_span(span: Span, source: &Source) -> Option<(String, Range)> {
    // ignore any unknown span
    let (file, (beginline, mut spans, endline)) = match source.get_file(span.unit()) {
        Some(file) => match file.lines_from_span(span) {
            Some(lines) => (file, lines),
            None => return None,
        },
        None => return None,
    };

    // VS Code expects the u16 offset (i.e. as JavaScript .charAt expects)
    fn calculate_u16_offset(linebegin: Pos, pos: Pos, file: &SourceFile) -> usize {
        let begin = linebegin.to_usize();
        let off = pos.to_usize();
        assert!(begin <= off);

        match file.data() {
            // for u8 data, calculate the number of u16s from the slice
            SourceSlice::U8(s) => {
                let s = &s[begin..off];
                // the number of "codepoints" (i.e. Unicode scalar values) in s
                let ncps = s.iter().filter(|&&b| b & 0b1100_0000 != 0b1000_0000).count();
                // the number of four-byte (non-BMP) sequences in s
                // (as they will result in two u16s)
                let nnonbmps = s.iter().filter(|&&b| b >= 0b1111_0000).count();
                ncps + nnonbmps
            },

            // for u16 data, the simple subtraction suffices
            SourceSlice::U16(_) => off - begin,
        }
    }

    // calculate the range for VS Code
    // assumes that the SourceFile is constructed from u16 string
    let beginspan = spans.next().unwrap();
    let beginch = calculate_u16_offset(beginspan.begin(), span.begin(), file);
    let endspan = spans.next_back().unwrap_or(beginspan);
    let mut endch = calculate_u16_offset(endspan.begin(), span.end(), file);
    if span.begin() == span.end() { endch += 1; } // avoid creating an empty range

    Some((file.path().to_owned(), Range {
        start: Position { line: beginline as u64, character: beginch as u64 },
        end: Position { line: endline as u64, character: endch as u64 }, // exclusive
    }))
}

// hierarchical diagnostics, forming a DAG.
// a report tree can be optionally associated to a particular path, in which case the path is
// considered to be included in the reports (even though no report for that path is generated).
#[derive(Clone)]
pub struct ReportTree {
    inner: Arc<ReportTreeInner>,
}

#[derive(Debug)]
struct ReportTreeInner {
    path: Option<String>,
    locale: Locale,

    // while we check for dupes, better to make it possible to call .add_parent multiple times
    parents: Mutex<HashSet<Arc<ReportTreeInner>>>,

    collected: Mutex<Vec<(String, Diagnostic)>>,
}

impl PartialEq for ReportTreeInner {
    fn eq(&self, other: &ReportTreeInner) -> bool {
        self as *const _ == other as *const _
    }
}

impl Eq for ReportTreeInner {
}

// make Arc<ReportTreeInern> hash to the strict object identity
impl hash::Hash for ReportTreeInner {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        (self as *const _).hash(state)
    }
}

impl ReportTree {
    pub fn new(locale: Locale, path: Option<&str>) -> ReportTree {
        ReportTree {
            inner: Arc::new(ReportTreeInner {
                path: path.map(|s| s.to_owned()),
                locale: locale,
                parents: Mutex::new(HashSet::new()),
                collected: Mutex::new(Vec::new()),
            })
        }
    }

    pub fn path(&self) -> Option<&str> {
        self.inner.path.as_ref().map(|s| &s[..])
    }

    pub fn add_parent(&self, parent: ReportTree) {
        // we don't check for cycles, as ReportTrees already check for duplicates
        self.inner.parents.lock().insert(parent.inner);
    }

    pub fn trees(&self) -> ReportTrees {
        ReportTrees { seen: HashSet::new(), stack: vec![(false, self.inner.clone())] }
    }

    pub fn diagnostics<'a>(&'a self) -> Diagnostics<'a> {
        let slice = self.inner.collected.lock();
        let len = slice.len();
        Diagnostics { slice: slice, range: 0..len }
    }

    // ReportTree itself does not implement Report, because it needs Source
    // for building CDP's Diagnostic interface.
    // (but you can directly put `Diagnostic` with `add_diag` for exceptional cases)
    pub fn report<F>(&self, translate: F) -> ReportTreeReport<F>
        where F: Fn(Span) -> Option<(String, Range)>
    {
        ReportTreeReport { inner: self.inner.clone(), translate: translate }
    }

    pub fn add_diag(&self, path: String, diag: Diagnostic) {
        self.inner.collected.lock().push((path, diag));
    }
}

impl fmt::Debug for ReportTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

pub struct ReportTreeReport<F> {
    inner: Arc<ReportTreeInner>,
    translate: F,
}

impl<F> Report for ReportTreeReport<F>
    where F: Fn(Span) -> Option<(String, Range)>
{
    fn message_locale(&self) -> Locale {
        self.inner.locale
    }

    fn add_span(&self, kind: Kind, span: Span, msg: &Localize) -> kailua_diag::Result<()> {
        let msg = Localized::new(msg, self.inner.locale).to_string();

        // TODO span should be translated _after_ deciding whether to put a new diagnostic,
        // but this is currently used to guard against unspanned, yet-to-be-fixed stray messages
        if let Some((path, range)) = (self.translate)(span) {
            let (severity, prefix) = match kind {
                Kind::Fatal | Kind::Error => (DiagnosticSeverity::Error, None),
                Kind::Warning => (DiagnosticSeverity::Warning, None),
                Kind::Cause => (DiagnosticSeverity::Information, Some("└ ")),
                Kind::Info => (DiagnosticSeverity::Information, None),
                Kind::Note => (DiagnosticSeverity::Hint, Some("  • ")),
            };

            let mut collected = self.inner.collected.lock();

            if let (false, Some(prefix)) = (collected.is_empty(), prefix) {
                // merge notes and causes to the prior message (if exists)
                let (ref lastpath, ref mut last) = *collected.last_mut().unwrap();
                last.message.push('\n');
                last.message.push_str(prefix);
                last.message.push_str(&msg);
                last.message.push_str(" (");
                if path != *lastpath {
                    last.message.push_str(&path);
                    last.message.push(' ');
                }
                last.message.push_str(&format!("{}:{}", range.start.line + 1,
                                               range.start.character + 1));
                if range.start.line != range.end.line ||
                   range.start.character + 1 < range.end.character {
                    last.message.push_str(&format!("-{}:{}", range.end.line + 1,
                                                   range.end.character + 1));
                   }
                last.message.push_str(")");
            } else {
                // otherwise report normally
                collected.push((path, Diagnostic {
                    range: range, severity: Some(severity),
                    code: None, source: None, message: msg,
                }));
            }
        }

        if kind == Kind::Fatal { Err(kailua_diag::Stop) } else { Ok(()) }
    }
}

pub struct Diagnostics<'a> {
    slice: MutexGuard<'a, Vec<(String, Diagnostic)>>,
    range: ops::Range<usize>,
}

impl<'a> Iterator for Diagnostics<'a> {
    type Item = (String, Diagnostic); // path and corresponding diagnostic

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(i) = self.range.next() {
            Some(self.slice[i].clone())
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.slice.len();
        (len, Some(len))
    }
}

// a post-order iterator of diagnostic trees
pub struct ReportTrees {
    seen: HashSet<*const ReportTreeInner>,
    stack: Vec<(bool, Arc<ReportTreeInner>)>, // false if unexpanded, true if expanded
}

impl Iterator for ReportTrees {
    type Item = ReportTree;

    fn next(&mut self) -> Option<ReportTree> {
        loop {
            match self.stack.pop() {
                Some((false, tree)) => {
                    // put this tree to the seen set and expand it
                    if !self.seen.insert(&*tree as *const _) {
                        continue; // already seen, skip this tree
                    }
                    self.stack.push((true, tree.clone()));
                    for parent in tree.parents.lock().iter() {
                        self.stack.push((false, parent.clone()));
                    }
                }

                Some((true, tree)) => {
                    // the tree has been expanded and is ready to be returned
                    return Some(ReportTree { inner: tree });
                }

                None => {
                    // no more tree exists
                    return None;
                }
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.stack.len(), None)
    }
}

