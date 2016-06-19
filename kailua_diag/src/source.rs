use std::io;
use std::io::Read;
use std::fs;
use std::slice;
use std::path::Path;
use std::ops;
use std::cmp;
use std::fmt;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos {
    pos: u32,
}

impl Pos {
    pub fn dummy() -> Pos { Pos { pos: 0 } }

    pub fn is_dummy(&self) -> bool { self.pos == 0 }

    pub fn to_usize(&self) -> usize { self.pos as usize }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.pos == 0 {
            write!(f, "@_")
        } else {
            write!(f, "@{:x}", self.pos)
        }
    }
}

// span (0, 0) is dummy and indicates the absence of appropriate span infos.
// span (0, x) for non-zero x is reserved.
// span (x, x) for non-zero x indicates a point and can be lifted from Pos.
// span (x, y) for non-zero x < y is an ordinary span, with y exclusive.
//
// a span *may* span multiple files in principle---it may arise from `|` operation.
// the reporter should consider them dummy however.
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Span {
    begin: Pos,
    end: Pos,
}

impl Span {
    pub fn new(begin: Pos, end: Pos) -> Span {
        if begin.is_dummy() || end.is_dummy() {
            Span::dummy()
        } else {
            assert!(begin <= end);
            Span { begin: begin, end: end }
        }
    }

    pub fn dummy() -> Span { Span { begin: Pos::dummy(), end: Pos::dummy() } }

    pub fn is_dummy(&self) -> bool { self.begin.is_dummy() }

    pub fn to_pos(&self) -> Pos {
        if self.begin == self.end { self.begin } else { Pos::dummy() }
    }

    pub fn begin(&self) -> Pos { self.begin }
    pub fn end(&self) -> Pos { self.end }

    pub fn contains(&self, pos: Pos) -> bool { self.begin <= pos && pos < self.end }
    pub fn contains_or_end(&self, pos: Pos) -> bool { self.begin <= pos && pos <= self.end }

    pub fn before(self, end: Pos) -> Span {
        assert!(self.begin <= end && end <= self.end);
        Span { begin: self.begin, end: end }
    }

    pub fn after(self, begin: Pos) -> Span {
        assert!(self.begin <= begin && begin <= self.end);
        Span { begin: begin, end: self.end }
    }
}

impl ops::BitAnd for Span {
    type Output = Span;
    fn bitand(self, other: Span) -> Span {
        if self.is_dummy() || other.is_dummy() { return Span::dummy(); }
        let begin = cmp::max(self.begin, other.begin);
        let end = cmp::min(self.end, other.end);
        if begin > end { return Span::dummy(); }
        Span { begin: begin, end: end }
    }
}

impl ops::BitAndAssign for Span {
    fn bitand_assign(&mut self, other: Span) { *self = *self & other; }
}

impl ops::BitOr for Span {
    type Output = Span;
    fn bitor(self, other: Span) -> Span {
        if self.is_dummy() { return other; }
        if other.is_dummy() { return self; }
        Span { begin: cmp::min(self.begin, other.begin), end: cmp::max(self.end, other.end) }
    }
}

impl ops::BitOrAssign for Span {
    fn bitor_assign(&mut self, other: Span) { *self = *self | other; }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.begin.is_dummy() {
            write!(f, "@_")
        } else if self.begin == self.end {
            write!(f, "@{:x}", self.begin.pos)
        } else {
            write!(f, "@{:x}-{:x}", self.begin.pos, self.end.pos)
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub span: Span,
    pub base: T,
}

impl<T> Spanned<T> {
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned { span: self.span, base: f(self.base) }
    }
}

impl From<Pos> for Span {
    fn from(pos: Pos) -> Span { Span { begin: pos, end: pos } }
}

impl From<ops::Range<Pos>> for Span {
    fn from(range: ops::Range<Pos>) -> Span {
        if range.start <= range.end {
            Span::new(range.start, range.end)
        } else {
            // this is possible when the range actually describes an empty span.
            // in the ordinary case we take the beginning of the first token and
            // the end of the last token for the span:
            //
            // function f()    FIRST_TOKEN ... LAST_TOKEN    end
            //                 ^ begin               end ^
            //
            // but if the span is empty, the order is swapped:
            //
            // function f()    end
            //         end ^   ^ begin
            //
            // the most reasonable choice here would be using (end..begin)
            // as an indication of the empty span.
            Span::new(range.end, range.start)
        }
    }
}

impl<T> From<Spanned<T>> for Span {
    fn from(spanned: Spanned<T>) -> Span { spanned.span }
}

impl<'a, T> From<&'a Spanned<T>> for Span {
    fn from(spanned: &'a Spanned<T>) -> Span { spanned.span }
}

impl<'a, T> From<&'a mut Spanned<T>> for Span {
    fn from(spanned: &'a mut Spanned<T>) -> Span { spanned.span }
}

impl<T> ops::Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &T { &self.base }
}

impl<T> ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut T { &mut self.base }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(fmt::Debug::fmt(&self.base, f));
        if f.alternate() {
            try!(fmt::Debug::fmt(&self.span, f));
        }
        Ok(())
    }
}

pub trait WithLoc: Sized {
    fn with_loc<Loc: Into<Span>>(self, loc: Loc) -> Spanned<Self> {
        Spanned { span: loc.into(), base: self }
    }
}

impl<T> WithLoc for T {}

#[derive(Clone)]
pub struct SourceLineSpans<'a> {
    slice: &'a [u32],
    last: u32,
}

fn make_span(begin: u32, end: u32) -> Span {
    Span { begin: Pos { pos: begin }, end: Pos { pos: end } }
}

impl<'a> Iterator for SourceLineSpans<'a> {
    type Item = Span;

    fn next(&mut self) -> Option<Span> {
        if let Some((&p, newslice)) = self.slice.split_first() {
            self.slice = newslice;
            Some(make_span(p, newslice.first().map_or(self.last, |&q| q)))
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.slice.len();
        (len, Some(len))
    }

    fn count(self) -> usize { self.slice.len() }

    fn nth(&mut self, n: usize) -> Option<Span> {
        let len = self.slice.len();
        if n + 1 < len {
            let span = make_span(self.slice[n], self.slice[n+1]);
            self.slice = &self.slice[n+1..];
            Some(span)
        } else if n + 1 == len {
            let span = make_span(self.slice[n], self.last);
            self.slice = &self.slice[len..];
            Some(span)
        } else {
            self.slice = &self.slice[len..];
            None
        }
    }

    fn last(mut self) -> Option<Span> { self.next_back() }
}

impl<'a> DoubleEndedIterator for SourceLineSpans<'a> {
    fn next_back(&mut self) -> Option<Span> {
        if let Some((&p, newslice)) = self.slice.split_last() {
            let span = make_span(p, self.last);
            self.slice = newslice;
            self.last = p;
            Some(span)
        } else {
            None
        }
    }
}

impl<'a> ExactSizeIterator for SourceLineSpans<'a> {
    fn len(&self) -> usize { self.slice.len() }
}

pub struct SourceFile {
    path: String, // not PathBuf since it is solely for reporting
    span: Span,
    lineoffs: Vec<u32>,
}

impl SourceFile {
    fn new(path: String, mut span: Span, data: &[u8]) -> SourceFile {
        let mut data = &data[span.begin.to_usize()..span.end.to_usize()];
        if data.starts_with(b"\xef\xbb\xbf") { // strip BOM
            data = &data[3..];
            span.begin.pos += 3;
        }

        // calculate line offsets
        let mut it = data.iter().cloned();
        let mut off = span.begin.pos;
        let mut lineoffs = vec![off];
        let mut next = it.next();
        loop {
            match next {
                Some(b'\r') => {
                    off += 1;
                    // try to get rid of stray `\n` if any
                    next = it.next();
                    if next == Some(b'\n') {
                        off += 1;
                        next = it.next();
                    }
                    lineoffs.push(off);
                }
                Some(b'\n') => {
                    off += 1;
                    next = it.next();
                    lineoffs.push(off);
                }
                Some(_) => {
                    off += 1;
                    next = it.next();
                }
                None => break,
            }
        }

        SourceFile { path: path, span: span, lineoffs: lineoffs }
    }

    pub fn path(&self) -> &str { &self.path }

    pub fn span(&self) -> Span { self.span }

    pub fn line_spans(&self) -> SourceLineSpans {
        SourceLineSpans { slice: &self.lineoffs, last: self.span.end.pos }
    }

    // line number starts from 0
    pub fn line_from_pos(&self, pos: Pos) -> Option<(usize, Span)> {
        let i = match self.lineoffs.binary_search_by(|p| p.cmp(&pos.pos)) {
            Ok(i) => i,
            Err(0) => return None, // pos < span.begin()
            Err(i) => i-1,
        };
        let begin = Pos { pos: self.lineoffs[i] };
        assert!(begin <= pos);
        let end = self.lineoffs.get(i+1).map_or(self.span.end(), |&p| Pos { pos: p });
        if end < pos { return None; } // pos > span.end()
        Some((i as usize, Span::new(begin, end)))
    }

    // line number starts from 0
    // the line span iterator contains both the first and last line covered by the span
    pub fn lines_from_span(&self, span: Span) -> Option<(usize, SourceLineSpans, usize)> {
        let begin = if let Some((l, _)) = self.line_from_pos(span.begin) { l } else { return None };
        let end = if let Some((l, _)) = self.line_from_pos(span.end) { l } else { return None };
        let spans = SourceLineSpans {
            slice: &self.lineoffs[begin..(end+1)],
            last: self.lineoffs.get(end+1).map_or(self.span.end.pos, |&p| p),
        };
        Some((begin as usize, spans, end as usize))
    }
}

#[derive(Clone)]
pub struct SourceBytes<'a> {
    iter: slice::Iter<'a, u8>,
    pos: Pos,
}

impl<'a> SourceBytes<'a> {
    pub fn pos(&self) -> Pos { self.pos }
}

impl<'a> Iterator for SourceBytes<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(&c) = self.iter.next() {
            self.pos.pos += 1;
            Some(c)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) { self.iter.size_hint() }
}

pub struct Source {
    files: Vec<SourceFile>,
    data: Vec<u8>,
}

impl Source {
    pub fn new() -> Source {
        Source { files: Vec::new(), data: Vec::new() }
    }

    pub fn add_file(&mut self, path: &Path) -> io::Result<Span> {
        let mut f = try!(fs::File::open(path));
        self.data.push(0xff); // sentinel, avoids span offset ever being zero
        let begin = self.data.len();
        try!(f.read_to_end(&mut self.data));
        let end = self.data.len();
        drop(f);

        let span = Span::new(Pos { pos: begin as u32 }, Pos { pos: end as u32 });
        let file = SourceFile::new(path.display().to_string(), span, &self.data);
        let span = file.span; // may have been recalculated
        self.files.push(file);
        Ok(span)
    }

    pub fn add_string(&mut self, path: &str, data: &[u8]) -> Span {
        self.data.push(0xff); // sentinel
        let begin = self.data.len();
        self.data.extend(data.iter().cloned());
        let end = self.data.len();
        let span = Span::new(Pos { pos: begin as u32 }, Pos { pos: end as u32 });
        let file = SourceFile::new(path.into(), span, &self.data);
        let span = file.span; // may have been recalculated
        self.files.push(file);
        span
    }

    pub fn files(&self) -> &[SourceFile] { &self.files }

    pub fn bytes_from_span(&self, span: Span) -> &[u8] {
        &self.data[span.begin.to_usize()..span.end.to_usize()]
    }

    pub fn iter_bytes_from_span(&self, span: Span) -> SourceBytes {
        let data = self.bytes_from_span(span);
        SourceBytes { iter: data.iter(), pos: span.begin }
    }

    pub fn file_from_pos(&self, pos: Pos) -> Option<&SourceFile> {
        let f = match self.files.binary_search_by(|f| f.span.end.cmp(&pos)) {
            Ok(i) => &self.files[i], // refers to the sentinel, which belongs to the last line
            Err(i) if i == self.files.len() => return None, // pos > data.len()
            Err(i) => &self.files[i],
        };
        assert!(pos <= f.span.end);
        if pos < f.span.begin { return None; } // sentinel or stripped BOM
        Some(f)
    }

    pub fn file_from_span(&self, span: Span) -> Option<&SourceFile> {
        if let Some(f) = self.file_from_pos(span.begin) {
            if span.end <= f.span.end { return Some(f); }
        }
        None
    }
}

