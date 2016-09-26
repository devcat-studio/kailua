use std::mem;
use std::ptr;
use std::io;
use std::panic::{self, AssertUnwindSafe};
use std::path::Path;
use std::sync::RwLock;
use widestring::{WideStr, WideString};
use kailua_diag::{Unit, Pos, Span, Source, SourceFile};

pub struct VSSource {
    source: RwLock<Source>,
}

impl VSSource {
    pub fn new() -> Box<VSSource> {
        Box::new(VSSource { source: RwLock::new(Source::new()) })
    }

    pub fn source(&self) -> &RwLock<Source> {
        &self.source
    }

    fn make_from_file(&self, path: &WideStr) -> io::Result<SourceFile> {
        let path = path.to_os_string();
        SourceFile::from_file(&Path::new(&path))
    }

    fn make_from_string(&self, path: WideString, data: WideString) -> SourceFile {
        SourceFile::from_u16(path.to_string_lossy(), data.into_vec())
    }

    fn add(&self, unit: Unit, file: SourceFile, span: Option<&mut Span>) -> i32 {
        let mut source = self.source.write().unwrap();
        let filespan = if unit.is_dummy() {
            Some(source.add(file))
        } else {
            source.replace(unit, file)
        };
        if let Some(filespan) = filespan {
            span.map(|span| *span = filespan);
            0
        } else {
            1
        }
    }

    pub fn add_file(&self, unit: Unit, path: &WideStr, span: Option<&mut Span>) -> i32 {
        if let Ok(file) = self.make_from_file(path) {
            self.add(unit, file, span)
        } else {
            1
        }
    }

    pub fn add_string(&self, unit: Unit,
                      path: WideString, data: WideString, span: Option<&mut Span>) -> i32 {
        self.add(unit, self.make_from_string(path, data), span)
    }

    pub fn remove(&self, unit: Unit) -> i32 {
        let mut source = self.source.write().unwrap();
        if source.remove(unit).is_some() {
            0
        } else {
            1
        }
    }

    // line number starts from 1, 0 indicates an invalid or dummy position
    pub fn line_from_pos(&self, pos: Pos, span: Option<&mut Span>) -> i32 {
        let source = self.source.read().unwrap();
        if let Some(f) = source.get_file(pos.unit()) {
            if let Some((line, linespan)) = f.line_from_pos(pos) {
                span.map(|span| *span = linespan);
                return line as i32 + 1;
            }
        }
        0
    }
}

#[no_mangle]
pub extern "C" fn kailua_source_new() -> *const VSSource {
    panic::catch_unwind(move || {
        let src = VSSource::new();
        unsafe { mem::transmute(src) }
    }).unwrap_or(ptr::null())
}

#[no_mangle]
pub extern "C" fn kailua_source_add_file(src: *const VSSource, unit: Unit,
                                         path: *const u16, pathlen: i32,
                                         span: *mut Span) -> i32 {
    if src.is_null() { return -1; }
    if path.is_null() || pathlen < 0 { return -1; }

    let src: &VSSource = unsafe { mem::transmute(src) };
    let path = unsafe { WideStr::from_ptr(path, pathlen as usize) };
    let span = unsafe { span.as_mut() };

    let src = AssertUnwindSafe(src);
    let span = AssertUnwindSafe(span);
    panic::catch_unwind(move || {
        src.add_file(unit, path, span.0)
    }).unwrap_or(-1)
}

#[no_mangle]
pub extern "C" fn kailua_source_add_string(src: *const VSSource, unit: Unit,
                                           path: *const u16, pathlen: i32,
                                           data: *const u16, datalen: i32,
                                           span: *mut Span) -> i32 {
    if src.is_null() { return -1; }
    if path.is_null() || pathlen < 0 { return -1; }
    if data.is_null() || datalen < 0 { return -1; }

    let src: &VSSource = unsafe { mem::transmute(src) };
    let path = unsafe { WideString::from_ptr(path, pathlen as usize) };
    let data = unsafe { WideString::from_ptr(data, datalen as usize) };
    let span = unsafe { span.as_mut() };

    let src = AssertUnwindSafe(src);
    let span = AssertUnwindSafe(span);
    panic::catch_unwind(move || {
        src.add_string(unit, path, data, span.0)
    }).unwrap_or(-1)
}

#[no_mangle]
pub extern "C" fn kailua_source_remove(src: *const VSSource, unit: Unit) -> i32 {
    if src.is_null() { return -1; }

    let src: &VSSource = unsafe { mem::transmute(src) };

    let src = AssertUnwindSafe(src);
    panic::catch_unwind(move || {
        src.remove(unit)
    }).unwrap_or(-1)
}

#[no_mangle]
pub extern "C" fn kailua_source_line_from_pos(src: *const VSSource, pos: *const Pos,
                                              span: *mut Span) -> i32 {
    if src.is_null() { return -1; }
    if pos.is_null() { return -1; }

    let src: &VSSource = unsafe { mem::transmute(src) };
    let pos = unsafe { *pos };
    let span = unsafe { span.as_mut() };

    let src = AssertUnwindSafe(src);
    let span = AssertUnwindSafe(span);
    panic::catch_unwind(move || {
        src.line_from_pos(pos, span.0)
    }).unwrap_or(-1)
}

#[no_mangle]
pub extern "C" fn kailua_source_free(src: *const VSSource) {
    if src.is_null() { return; }
    let src: Box<VSSource> = unsafe { mem::transmute(src) };

    let src = AssertUnwindSafe(src); // XXX use Unique when it is stabilized
    let _ = panic::catch_unwind(move || {
        drop(src);
    }); // cannot do much beyond this
}

