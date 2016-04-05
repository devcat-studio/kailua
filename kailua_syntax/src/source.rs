use std::io;
use std::io::Read;
use std::fs;
use std::slice;
use std::path::Path;

struct SourceFile {
    path: String, // not PathBuf since it is solely for reporting
    begin: usize,
    end: usize,
}

pub struct SourceFiles<'a> {
    iter: slice::Iter<'a, SourceFile>,
}

impl<'a> Iterator for SourceFiles<'a> {
    type Item = (&'a str, Span);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(file) = self.iter.next() {
            Some((&file.path[..], Span::new(file.begin, file.end)))
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) { self.iter.size_hint() }
}

pub struct SourceBytes<'a> {
    iter: slice::Iter<'a, u8>,
    pos: usize,
}

impl<'a> Iterator for SourceBytes<'a> {
    type Item = (u8, Span);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(&c) = self.iter.next() {
            let pos = self.pos;
            self.pos += 1;
            Some((c, Span::new(pos, pos + 1)))
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

const BOMB: &'static [u8] = b"\xef\xbb\xbf";

impl Source {
    pub fn new() -> Source {
        Source { files: Vec::new(), data: Vec::new() }
    }

    pub fn add_file(&mut self, path: &Path) -> io::Result<Span> {
        let mut f = try!(fs::File::open(path));
        self.data.push(0xff); // sentinel, avoids span offset ever being zero
        let mut begin = self.data.len();
        try!(f.read_to_end(&mut self.data));
        let end = self.data.len();
        drop(f);

        if end - begin >= BOMB.len() && &self.data[begin..begin+BOMB.len()] == BOMB {
            // strip BOM
            begin += BOMB.len();
        }

        self.files.push(SourceFile { path: path.display().to_string(), begin: begin, end: end });
        Ok(Span::new(begin, end))
    }

    pub fn add_string(&mut self, path: &str, data: &[u8]) -> Span {
        self.data.push(0xff); // sentinel
        let mut begin = self.data.len();
        if data.starts_with(BOMB) {
            // strip BOM
            self.data.extend(data.iter().skip(BOMB.len()).cloned());
        } else {
            self.data.extend(data.iter().cloned());
        }
        let end = self.data.len();
        self.files.push(SourceFile { path: path.into(), begin: begin, end: end });
        Span::new(begin, end)
    }

    pub fn files(&self) -> SourceFiles {
        SourceFiles { iter: self.files.iter() }
    }

    pub fn bytes_from_span(&self, span: Span) -> SourceBytes {
        let data = &self.data[(span.0 as usize)..(span.1 as usize)];
        SourceBytes { iter: self.data.iter(), pos: span.0 as usize }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Pos(u32);

impl Pos {
    fn new(pos: usize) -> Pos {
        Pos(pos as u32)
    }

    pub fn dummy() -> Pos { Pos(0) }

    pub fn is_dummy(&self) -> bool { self.0 == 0 }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Span(u32, u32);

impl Span {
    fn new(begin: usize, end: usize) -> Span {
        assert!(begin <= end);
        Span(begin as u32, end as u32)
    }

    pub fn dummy() -> Span { Span(0, 0) }

    pub fn is_dummy(&self) -> bool { self.0 == 0 }

    pub fn begin(&self) -> Pos { Pos(self.0) }
    pub fn end(&self) -> Pos { Pos(self.1) }

    pub fn before(self, end: Pos) -> Span {
        assert!(self.0 <= end.0 && end.0 <= self.1);
        Span(self.0, end.0)
    }

    pub fn after(self, begin: Pos) -> Span {
        assert!(self.0 <= begin.0 && begin.0 <= self.1);
        Span(begin.0, self.1)
    }
}

