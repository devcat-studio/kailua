use std::io;
use std::io::Read;
use std::fs;
use std::slice;
use std::path::Path;
use std::collections::hash_map::{self, HashMap};
use loc::{Unit, Pos, Span, Spanned, WithLoc};
use loc::{unit_from_u32, pos_from_u32, span_from_u32};

#[derive(Clone)]
pub struct SourceLineSpans<'a> {
    slice: &'a [u32],
    unit: Unit,
}

impl<'a> Iterator for SourceLineSpans<'a> {
    type Item = Span;

    fn next(&mut self) -> Option<Span> {
        if let Some((&p, newslice)) = self.slice.split_first() {
            if let Some(&q) = newslice.first() {
                self.slice = newslice;
                return Some(span_from_u32(self.unit, p, q));
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
            let span = span_from_u32(self.unit, self.slice[n], self.slice[n+1]);
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
                return Some(span_from_u32(self.unit, p, q));
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
    unit: Unit,
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
        let mut f = fs::File::open(path)?;
        let mut data = Vec::new();
        f.read_to_end(&mut data)?;
        drop(f);

        Ok(SourceFile::from_u8(path.display().to_string(), data))
    }

    pub fn from_u8(path: String, data: Vec<u8>) -> SourceFile {
        let lineoffs = SourceFile::calculate_lineoffs(&data, b"\xef\xbb\xbf", b'\r', b'\n');
        SourceFile {
            path: path,
            buf: SourceBuf::U8(data),
            unit: Unit::dummy(),
            lineoffs: lineoffs,
        }
    }

    pub fn from_u16(path: String, data: Vec<u16>) -> SourceFile {
        let lineoffs = SourceFile::calculate_lineoffs(&data, &[0xfeff], 0xd, 0xa);
        SourceFile {
            path: path,
            buf: SourceBuf::U16(data),
            unit: Unit::dummy(),
            lineoffs: lineoffs,
        }
    }

    fn set_unit(&mut self, unit: Unit) {
        assert!(!unit.is_dummy() && self.unit.is_dummy());
        self.unit = unit;
    }

    pub fn path(&self) -> &str { &self.path }

    pub fn span(&self) -> Span {
        span_from_u32(self.unit, *self.lineoffs.first().unwrap(), *self.lineoffs.last().unwrap())
    }

    pub fn data<'a>(&'a self) -> SourceSlice<'a> {
        match self.buf {
            SourceBuf::U8(ref data) => SourceSlice::U8(data),
            SourceBuf::U16(ref data) => SourceSlice::U16(data),
        }
    }

    pub fn line_spans(&self) -> SourceLineSpans {
        assert!(!self.lineoffs.is_empty());
        SourceLineSpans { slice: &self.lineoffs, unit: self.unit }
    }

    // line number starts from 0
    pub fn line_from_pos(&self, pos: Pos) -> Option<(usize, Span)> {
        let unit = pos.unit();
        let pos = pos.to_usize() as u32;
        if unit.is_dummy() || unit != self.unit {
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
        Some((i as usize, span_from_u32(unit, begin, end)))
    }

    // line number starts from 0
    // the line span iterator contains both the first and last line covered by the span
    pub fn lines_from_span(&self, span: Span) -> Option<(usize, SourceLineSpans, usize)> {
        let unit = span.unit();
        if unit.is_dummy() || unit != self.unit {
            return None;
        }

        let begin = match self.line_from_pos(span.begin()) {
            Some((l, _)) => l,
            None => return None,
        };
        let end = match self.line_from_pos(span.end()) {
            Some((l, _)) => l,
            None => return None,
        };
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

    let unit = unit_from_u32(1);
    let mk_pos = |pos| pos_from_u32(unit, pos);
    let mk_span = |begin, end| span_from_u32(unit, begin, end);
    let tr_lines = |(begin, spans, end): (usize, SourceLineSpans, usize)| {
        (begin, spans.collect::<Vec<Span>>(), end)
    };

    // empty file
    let mut f = SourceFile::from_u8("foo".into(), vec![]);
    f.set_unit(unit);
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
    f.set_unit(unit);
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
    f.set_unit(unit);
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
    unit: Unit,
    pos: u32,
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
            let prevpos = pos_from_u32(self.unit, self.pos);
            if !self.unit.is_dummy() {
                self.pos += 1;
            }
            let pos = pos_from_u32(self.unit, self.pos);
            Some(c.with_loc(prevpos..pos))
        } else if !self.eof_sent {
            self.eof_sent = true;
            let pos = pos_from_u32(self.unit, self.pos);
            Some(SourceData::EOF.with_loc(pos))
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
    files: HashMap<Unit, SourceFile>,
    next_unit: u32,
}

impl Source {
    pub fn new() -> Source {
        Source { files: HashMap::new(), next_unit: 1 }
    }

    pub fn add(&mut self, mut file: SourceFile) -> Span {
        let unit = unit_from_u32(self.next_unit);
        assert!(unit.is_source_dependent());
        file.set_unit(unit);
        let span = file.span();
        self.files.insert(unit, file);
        self.next_unit += 1;
        span
    }

    pub fn replace(&mut self, unit: Unit, mut file: SourceFile) -> Option<Span> {
        if !unit.is_source_dependent() || unit.to_usize() >= self.next_unit as usize {
            return None;
        }

        file.set_unit(unit);
        let span = file.span();
        self.files.insert(unit, file);
        Some(span)
    }

    pub fn remove(&mut self, unit: Unit) -> Option<SourceFile> {
        self.files.remove(&unit)
    }

    pub fn files(&self) -> hash_map::Values<Unit, SourceFile> {
        self.files.values()
    }

    pub fn slice_from_span<'a>(&'a self, span: Span) -> Option<SourceSlice<'a>> {
        if let Some(file) = self.files.get(&span.unit()) {
            let range = span.begin().to_usize()..span.end().to_usize();
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
        Some(SourceDataIter {
            iter: iter,
            unit: span.unit(),
            pos: span.begin().to_usize() as u32,
            eof_sent: false,
        })
    }

    pub fn get_file(&self, unit: Unit) -> Option<&SourceFile> {
        self.files.get(&unit)
    }
}

