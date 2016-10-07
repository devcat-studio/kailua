use std::io;
use std::io::Read;
use std::fs;
use std::slice;
use std::path::Path;
use std::ops;
use std::cmp;
use std::fmt;
use std::borrow::Borrow;
use std::collections::hash_map::{self, HashMap};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Unit {
    unit: u32,
}

const BUILTIN_UNIT: u32 = 0xffffffff;

impl Unit {
    pub fn dummy() -> Unit {
        Unit { unit: 0 }
    }

    pub fn builtin() -> Unit {
        Unit { unit: BUILTIN_UNIT }
    }

    pub fn is_dummy(&self) -> bool {
        self.unit == 0
    }

    pub fn is_source_dependent(&self) -> bool {
        self.unit > 0 && self.unit < BUILTIN_UNIT
    }

    pub fn to_usize(&self) -> usize {
        self.unit as usize
    }
}

impl fmt::Debug for Unit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            if self.unit == 0 {
                write!(f, "@_")
            } else if self.unit == BUILTIN_UNIT {
                write!(f, "@<builtin>")
            } else {
                write!(f, "@{}", self.unit)
            }
        } else {
            Ok(())
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos {
    unit: u32,
    pos: u32,
}

impl Pos {
    pub fn dummy() -> Pos {
        Pos { unit: 0, pos: 0 }
    }

    pub fn builtin() -> Pos {
        Pos { unit: BUILTIN_UNIT, pos: 0 }
    }

    pub fn is_dummy(&self) -> bool {
        self.unit().is_dummy()
    }

    pub fn is_source_dependent(&self) -> bool {
        self.unit().is_source_dependent()
    }

    pub fn unit(&self) -> Unit {
        Unit { unit: self.unit }
    }

    pub fn to_usize(&self) -> usize {
        self.pos as usize
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            if self.unit == 0 {
                write!(f, "@_")
            } else if self.unit == BUILTIN_UNIT {
                write!(f, "@<builtin>")
            } else {
                write!(f, "@{}/{}", self.unit, self.pos)
            }
        } else {
            Ok(())
        }
    }
}

// span (0, 0, 0) is dummy and indicates the absence of appropriate span infos.
// span (0, y, z) for non-zero y and z is reserved.
// span (x, y, y) for non-zero x and y indicates a point and can be lifted from Pos.
// span (x, y, z) for non-zero x, y and z (y < z) is an ordinary span, with z exclusive.
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Span {
    unit: u32,
    begin: u32,
    end: u32,
}

impl Span {
    pub fn new(begin: Pos, end: Pos) -> Span {
        if begin.is_dummy() || end.is_dummy() {
            Span::dummy()
        } else {
            assert!(begin.unit == end.unit, "Span::new with positions from different units");
            assert!(begin.pos <= end.pos, "Span::new with swapped positions");
            Span { unit: begin.unit, begin: begin.pos, end: end.pos }
        }
    }

    pub fn dummy() -> Span {
        Span { unit: 0, begin: 0, end: 0 }
    }

    pub fn builtin() -> Span {
        Span { unit: BUILTIN_UNIT, begin: 0, end: 0 }
    }

    pub fn is_dummy(&self) -> bool {
        self.unit().is_dummy()
    }

    pub fn is_source_dependent(&self) -> bool {
        self.unit().is_source_dependent()
    }

    pub fn to_pos(&self) -> Pos {
        if self.begin == self.end {
            Pos { unit: self.unit, pos: self.begin }
        } else {
            Pos::dummy()
        }
    }

    pub fn unit(&self) -> Unit {
        Unit { unit: self.unit }
    }

    pub fn begin(&self) -> Pos {
        Pos { unit: self.unit, pos: self.begin }
    }

    pub fn end(&self) -> Pos {
        Pos { unit: self.unit, pos: self.end }
    }

    pub fn contains(&self, pos: Pos) -> bool {
        self.unit > 0 && self.unit == pos.unit && self.begin <= pos.pos && pos.pos < self.end
    }

    pub fn contains_or_end(&self, pos: Pos) -> bool {
        self.unit > 0 && self.unit == pos.unit && self.begin <= pos.pos && pos.pos <= self.end
    }
}

impl ops::BitAnd for Span {
    type Output = Span;
    fn bitand(self, other: Span) -> Span {
        if self.is_dummy() || other.is_dummy() { return Span::dummy(); }
        if self.unit == other.unit {
            let begin = cmp::max(self.begin, other.begin);
            let end = cmp::min(self.end, other.end);
            if begin > end { return Span::dummy(); }
            Span { unit: self.unit, begin: begin, end: end }
        } else {
            Span::dummy()
        }
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
        if self.unit == other.unit {
            Span {
                unit: self.unit,
                begin: cmp::min(self.begin, other.begin),
                end: cmp::max(self.end, other.end),
            }
        } else {
            Span::dummy()
        }
    }
}

impl ops::BitOrAssign for Span {
    fn bitor_assign(&mut self, other: Span) { *self = *self | other; }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            if self.unit == 0 {
                write!(f, "@_")
            } else if self.unit == BUILTIN_UNIT {
                write!(f, "@<builtin>")
            } else if self.begin == self.end {
                write!(f, "@{}/{}", self.unit, self.begin)
            } else {
                write!(f, "@{}/{}-{}", self.unit, self.begin, self.end)
            }
        } else {
            Ok(())
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub span: Span,
    pub base: T,
}

impl<T> Spanned<T> {
    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned { span: self.span, base: &self.base }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned { span: self.span, base: f(self.base) }
    }
}

impl From<Pos> for Span {
    fn from(pos: Pos) -> Span {
        Span { unit: pos.unit, begin: pos.pos, end: pos.pos }
    }
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

impl<T> Borrow<T> for Spanned<T> {
    fn borrow(&self) -> &T { &self.base }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.base, f)
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(fmt::Debug::fmt(&self.base, f));
        try!(fmt::Debug::fmt(&self.span, f));
        Ok(())
    }
}

pub trait WithLoc: Sized {
    fn with_loc<Loc: Into<Span>>(self, loc: Loc) -> Spanned<Self> {
        Spanned { span: loc.into(), base: self }
    }

    fn without_loc(self) -> Spanned<Self> {
        Spanned { span: Span::dummy(), base: self }
    }
}

impl<T> WithLoc for T {}

#[derive(Clone)]
pub struct SourceLineSpans<'a> {
    slice: &'a [u32],
    unit: u32,
}

impl<'a> Iterator for SourceLineSpans<'a> {
    type Item = Span;

    fn next(&mut self) -> Option<Span> {
        if let Some((&p, newslice)) = self.slice.split_first() {
            if let Some(&q) = newslice.first() {
                self.slice = newslice;
                return Some(Span { unit: self.unit, begin: p, end: q });
            }
        }
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.slice.len() - 1;
        (len, Some(len))
    }

    fn count(self) -> usize {
        self.slice.len() - 1
    }

    fn nth(&mut self, n: usize) -> Option<Span> {
        let len = self.slice.len();
        if n + 1 < len {
            let span = Span { unit: self.unit, begin: self.slice[n], end: self.slice[n+1] };
            self.slice = &self.slice[n+1..];
            Some(span)
        } else {
            self.slice = &self.slice[len..];
            None
        }
    }

    fn last(mut self) -> Option<Span> {
        self.next_back()
    }
}

impl<'a> DoubleEndedIterator for SourceLineSpans<'a> {
    fn next_back(&mut self) -> Option<Span> {
        if let Some((&q, newslice)) = self.slice.split_last() {
            if let Some(&p) = newslice.last() {
                self.slice = newslice;
                return Some(Span { unit: self.unit, begin: p, end: q });
            }
        }
        None
    }
}

impl<'a> ExactSizeIterator for SourceLineSpans<'a> {
    fn len(&self) -> usize {
        self.slice.len() - 1
    }
}

enum SourceBuf {
    U8(Vec<u8>),
    U16(Vec<u16>),
}

pub struct SourceFile {
    path: String, // not PathBuf since it is solely for reporting
    buf: SourceBuf,
    unit: u32,
    lineoffs: Vec<u32>,
}

impl SourceFile {
    fn calculate_lineoffs<T: Copy + Eq>(mut data: &[T], bom: &[T], cr: T, lf: T) -> Vec<u32> {
        // strip BOM
        let end = data.len() as u32;
        let begin;
        if data.starts_with(bom) {
            data = &data[bom.len()..];
            begin = bom.len() as u32;
        } else {
            begin = 0;
        }

        // calculate line offsets
        let mut it = data.iter().cloned();
        let mut off = begin;
        let mut lineoffs = vec![off];
        let mut next = it.next();
        loop {
            match next {
                Some(c) if c == cr => {
                    off += 1;
                    // try to get rid of stray `\n` if any
                    next = it.next();
                    if next == Some(lf) {
                        off += 1;
                        next = it.next();
                    }
                    lineoffs.push(off);
                }
                Some(c) if c == lf => {
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

        lineoffs.push(end);
        lineoffs
    }

    pub fn from_file(path: &Path) -> io::Result<SourceFile> {
        let mut f = try!(fs::File::open(path));
        let mut data = Vec::new();
        try!(f.read_to_end(&mut data));
        drop(f);

        Ok(SourceFile::from_u8(path.display().to_string(), data))
    }

    pub fn from_u8(path: String, data: Vec<u8>) -> SourceFile {
        let lineoffs = SourceFile::calculate_lineoffs(&data, b"\xef\xbb\xbf", b'\r', b'\n');
        SourceFile { path: path, buf: SourceBuf::U8(data), unit: 0, lineoffs: lineoffs }
    }

    pub fn from_u16(path: String, data: Vec<u16>) -> SourceFile {
        let lineoffs = SourceFile::calculate_lineoffs(&data, &[0xfeff], 0xd, 0xa);
        SourceFile { path: path, buf: SourceBuf::U16(data), unit: 0, lineoffs: lineoffs }
    }

    fn set_unit(&mut self, unit: u32) {
        assert!(unit != 0 && self.unit == 0);
        self.unit = unit;
    }

    pub fn path(&self) -> &str { &self.path }

    pub fn span(&self) -> Span {
        Span {
            unit: self.unit,
            begin: *self.lineoffs.first().unwrap(),
            end: *self.lineoffs.last().unwrap(),
        }
    }

    pub fn line_spans(&self) -> SourceLineSpans {
        assert!(!self.lineoffs.is_empty());
        SourceLineSpans { slice: &self.lineoffs, unit: self.unit }
    }

    // line number starts from 0
    pub fn line_from_pos(&self, pos: Pos) -> Option<(usize, Span)> {
        let Pos { unit, pos } = pos;
        if unit == 0 || unit != self.unit {
            return None;
        }

        let lineoffs = &self.lineoffs[..self.lineoffs.len()-1];
        let i = match lineoffs.binary_search_by(|p| p.cmp(&pos)) {
            Ok(i) => i,
            Err(0) => return None, // pos < span.begin()
            Err(i) => i-1,
        };
        let begin = self.lineoffs[i];
        assert!(begin <= pos);
        let end = self.lineoffs[i+1];
        if end < pos { return None; } // pos > span.end()
        Some((i as usize, Span { unit: unit, begin: begin, end: end }))
    }

    // line number starts from 0
    // the line span iterator contains both the first and last line covered by the span
    pub fn lines_from_span(&self, span: Span) -> Option<(usize, SourceLineSpans, usize)> {
        let Span { unit, begin, end } = span;
        if unit == 0 || unit != self.unit {
            return None;
        }

        let beginpos = Pos { unit: unit, pos: begin };
        let endpos = Pos { unit: unit, pos: end };
        let begin = if let Some((l, _)) = self.line_from_pos(beginpos) { l } else { return None };
        let end = if let Some((l, _)) = self.line_from_pos(endpos) { l } else { return None };
        let spans = SourceLineSpans { slice: &self.lineoffs[begin..(end+2)], unit: unit };
        Some((begin as usize, spans, end as usize))
    }
}

#[test]
fn test_source_file() {
    macro_rules! assert_eq_alt {
        ($lhs:expr, $rhs:expr) => ({
            let lhs = $lhs;
            let rhs = $rhs;
            if !(lhs == rhs && rhs == lhs) {
                panic!("assertion failed: `(left == right)`\n\
                        left: {:#?}\n\
                        right: {:#?}\n", lhs, rhs);
            }
        })
    }

    let mk_pos = |pos| Pos { unit: 1, pos: pos };
    let mk_span = |begin, end| Span { unit: 1, begin: begin, end: end };
    let tr_lines = |(begin, spans, end): (usize, SourceLineSpans, usize)| {
        (begin, spans.collect::<Vec<Span>>(), end)
    };

    // empty file
    let mut f = SourceFile::from_u8("foo".into(), vec![]);
    f.set_unit(1);
    assert_eq_alt!(f.span(), mk_span(0, 0));
    assert_eq_alt!(f.line_spans().collect::<Vec<_>>(), vec![mk_span(0, 0)]);
    assert_eq_alt!(f.line_spans().len(), 1);
    assert_eq_alt!(f.line_spans().rev().collect::<Vec<_>>(), vec![mk_span(0, 0)]);
    assert_eq_alt!(f.line_spans().rev().len(), 1);
    {
        let mut lines = f.line_spans();
        assert_eq_alt!(lines.nth(0), Some(mk_span(0, 0)));
        assert_eq_alt!(lines.nth(0), None);
    }
    assert_eq_alt!(f.line_from_pos(mk_pos(0)), Some((0, mk_span(0, 0))));
    assert_eq_alt!(f.line_from_pos(mk_pos(1)), None);
    assert_eq_alt!(f.lines_from_span(mk_span(0, 0)).map(&tr_lines),
                   Some((0, vec![mk_span(0, 0)], 0)));

    const SOURCE: &'static [u8] = b"\xef\xbb\xbfhello\nworld\r\n\ra\n\nxyz";
    // line span:                               <--->  <--->   /\ ^ /\ <->
    // byte offset:                 0   1   2   345678 901234 5 6 78 9 012

    let mut f = SourceFile::from_u8("foo".into(), SOURCE.iter().cloned().collect());
    f.set_unit(1);
    assert_eq_alt!(f.span(), mk_span(3, 23));
    assert_eq_alt!(f.line_spans().collect::<Vec<_>>(),
                   vec![mk_span(3, 9), mk_span(9, 16), mk_span(16, 17), mk_span(17, 19),
                        mk_span(19, 20), mk_span(20, 23)]);
    assert_eq_alt!(f.line_spans().len(), 6);
    assert_eq_alt!(f.line_spans().rev().collect::<Vec<_>>(),
                   vec![mk_span(20, 23), mk_span(19, 20), mk_span(17, 19), mk_span(16, 17),
                        mk_span(9, 16), mk_span(3, 9)]);
    assert_eq_alt!(f.line_spans().rev().len(), 6);
    {
        let mut lines = f.line_spans();
        assert_eq_alt!(lines.nth(0), Some(mk_span(3, 9)));
        assert_eq_alt!(lines.nth(0), Some(mk_span(9, 16)));
        assert_eq_alt!(lines.nth(2), Some(mk_span(19, 20)));
        assert_eq_alt!(lines.nth(1), None);
    }
    assert_eq_alt!(f.line_from_pos(mk_pos(0)), None);
    assert_eq_alt!(f.line_from_pos(mk_pos(2)), None);
    assert_eq_alt!(f.line_from_pos(mk_pos(3)), Some((0, mk_span(3, 9))));
    assert_eq_alt!(f.line_from_pos(mk_pos(8)), Some((0, mk_span(3, 9))));
    assert_eq_alt!(f.line_from_pos(mk_pos(9)), Some((1, mk_span(9, 16))));
    assert_eq_alt!(f.line_from_pos(mk_pos(19)), Some((4, mk_span(19, 20))));
    assert_eq_alt!(f.line_from_pos(mk_pos(20)), Some((5, mk_span(20, 23))));
    assert_eq_alt!(f.line_from_pos(mk_pos(22)), Some((5, mk_span(20, 23))));
    assert_eq_alt!(f.line_from_pos(mk_pos(23)), Some((5, mk_span(20, 23)))); // allow past-the-end
    assert_eq_alt!(f.line_from_pos(mk_pos(24)), None);
    assert_eq_alt!(f.lines_from_span(mk_span(3, 5)).map(&tr_lines),
                   Some((0, vec![mk_span(3, 9)], 0)));
    assert_eq_alt!(f.lines_from_span(mk_span(3, 8)).map(&tr_lines),
                   Some((0, vec![mk_span(3, 9)], 0)));
    assert_eq_alt!(f.lines_from_span(mk_span(3, 9)).map(&tr_lines),
                   Some((0, vec![mk_span(3, 9), mk_span(9, 16)], 1)));
    assert_eq_alt!(f.lines_from_span(mk_span(14, 18)).map(&tr_lines),
                   Some((1, vec![mk_span(9, 16), mk_span(16, 17), mk_span(17, 19)], 3)));
    assert_eq_alt!(f.lines_from_span(mk_span(19, 23)).map(&tr_lines),
                   Some((4, vec![mk_span(19, 20), mk_span(20, 23)], 5)));
    assert_eq_alt!(f.lines_from_span(mk_span(5, 5)).map(&tr_lines),
                   Some((0, vec![mk_span(3, 9)], 0)));
    assert_eq_alt!(f.lines_from_span(mk_span(19, 19)).map(&tr_lines),
                   Some((4, vec![mk_span(19, 20)], 4)));
    assert_eq_alt!(f.lines_from_span(mk_span(20, 20)).map(&tr_lines),
                   Some((5, vec![mk_span(20, 23)], 5)));
    assert_eq_alt!(f.lines_from_span(mk_span(23, 23)).map(&tr_lines),
                   Some((5, vec![mk_span(20, 23)], 5)));
    assert_eq_alt!(f.lines_from_span(mk_span(3, 23)).map(&tr_lines),
                   Some((0, f.line_spans().collect::<Vec<_>>(), 5)));
    assert_eq_alt!(f.lines_from_span(mk_span(0, 3)).map(&tr_lines), None);
    assert_eq_alt!(f.lines_from_span(mk_span(23, 24)).map(&tr_lines), None);

    let mut f = SourceFile::from_u16("foo".into(), vec![0xac00, 0xd, 0xa, 0xac01]);
    f.set_unit(1);
    assert_eq_alt!(f.span(), mk_span(0, 4));
    assert_eq_alt!(f.line_spans().collect::<Vec<_>>(),
                   vec![mk_span(0, 3), mk_span(3, 4)]);
}

#[derive(Clone, Debug)]
pub enum SourceSlice<'a> {
    U8(&'a [u8]),
    U16(&'a [u16]),
}

impl<'a> SourceSlice<'a> {
    pub fn len(&self) -> usize {
        match *self {
            SourceSlice::U8(s) => s.len(),
            SourceSlice::U16(s) => s.len(),
        }
    }
}

#[derive(Clone)]
enum SourceSliceIter<'a> {
    U8(slice::Iter<'a, u8>),
    U16(slice::Iter<'a, u16>),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SourceData {
    U8(u8),
    U16(u16),
    EOF,
}

impl SourceData {
    pub fn u8(&self) -> u8 {
        match *self {
            SourceData::U8(v) => v,
            SourceData::U16(_) | SourceData::EOF => panic!("SourceData::u8 called with U16/EOF"),
        }
    }

    pub fn u16(&self) -> u16 {
        match *self {
            SourceData::U8(_) | SourceData::EOF => panic!("SourceData::u16 called with U8/EOF"),
            SourceData::U16(v) => v,
        }
    }
}

#[derive(Clone)]
pub struct SourceDataIter<'a> {
    iter: SourceSliceIter<'a>,
    pos: Pos,
    eof_sent: bool,
}

impl<'a> Iterator for SourceDataIter<'a> {
    type Item = Spanned<SourceData>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = match self.iter {
            SourceSliceIter::U8(ref mut iter) => iter.next().map(|&v| SourceData::U8(v)),
            SourceSliceIter::U16(ref mut iter) => iter.next().map(|&v| SourceData::U16(v)),
        };
        if let Some(c) = next {
            let prevpos = self.pos;
            if !self.pos.is_dummy() {
                self.pos.pos += 1;
            }
            Some(c.with_loc(prevpos..self.pos))
        } else if !self.eof_sent {
            self.eof_sent = true;
            Some(SourceData::EOF.with_loc(self.pos))
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (lo, hi) = match self.iter {
            SourceSliceIter::U8(ref iter) => iter.size_hint(),
            SourceSliceIter::U16(ref iter) => iter.size_hint(),
        };
        (lo.saturating_add(1), hi.and_then(|hi| hi.checked_add(1)))
    }
}

pub struct Source {
    files: HashMap<u32, SourceFile>,
    next_unit: u32,
}

impl Source {
    pub fn new() -> Source {
        Source { files: HashMap::new(), next_unit: 1 }
    }

    pub fn add(&mut self, mut file: SourceFile) -> Span {
        let unit = self.next_unit;
        file.set_unit(unit);
        let span = file.span();
        self.files.insert(self.next_unit, file);
        self.next_unit += 1;
        assert!(Unit { unit: self.next_unit }.is_source_dependent());
        span
    }

    pub fn replace(&mut self, unit: Unit, mut file: SourceFile) -> Option<Span> {
        if !unit.is_source_dependent() || unit.unit >= self.next_unit {
            return None;
        }

        file.set_unit(unit.unit);
        let span = file.span();
        self.files.insert(unit.unit, file);
        Some(span)
    }

    pub fn remove(&mut self, unit: Unit) -> Option<SourceFile> {
        self.files.remove(&unit.unit)
    }

    pub fn files(&self) -> hash_map::Values<u32, SourceFile> {
        self.files.values()
    }

    pub fn slice_from_span<'a>(&'a self, span: Span) -> Option<SourceSlice<'a>> {
        if let Some(file) = self.files.get(&span.unit) {
            let range = (span.begin as usize)..(span.end as usize);
            match file.buf {
                SourceBuf::U8(ref data) => Some(SourceSlice::U8(&data[range])),
                SourceBuf::U16(ref data) => Some(SourceSlice::U16(&data[range])),
            }
        } else {
            None
        }
    }

    pub fn iter_from_span(&self, span: Span) -> Option<SourceDataIter> {
        let iter = match self.slice_from_span(span) {
            Some(SourceSlice::U8(data)) => SourceSliceIter::U8(data.iter()),
            Some(SourceSlice::U16(data)) => SourceSliceIter::U16(data.iter()),
            None => return None,
        };
        Some(SourceDataIter { iter: iter, pos: span.begin(), eof_sent: false })
    }

    pub fn get_file(&self, unit: Unit) -> Option<&SourceFile> {
        self.files.get(&unit.unit)
    }
}

