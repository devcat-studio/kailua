use std::mem;
use std::ptr;
use std::panic::{self, AssertUnwindSafe};
use std::path::Path;
use std::sync::Mutex;
use widestring::{WideStr, WideString};
use kailua_diag::{Span, Source, SourceFile};

pub struct VSSource {
    source: Mutex<Source>,
}

impl VSSource {
    pub fn new() -> Box<VSSource> {
        Box::new(VSSource { source: Mutex::new(Source::new()) })
    }

    pub fn source(&self) -> &Mutex<Source> {
        &self.source
    }

    pub fn add_file(&self, path: &WideStr, span: Option<&mut Span>) -> i32 {
        let mut source = self.source.lock().unwrap();
        let path = path.to_os_string();
        match SourceFile::from_file(&Path::new(&path)) {
            Ok(file) => {
                let filespan = source.add(file);
                span.map(|span| *span = filespan);
                0
            },
            Err(_e) => {
                1
            },
        }
    }

    pub fn add_string(&self, path: WideString, data: WideString, span: Option<&mut Span>) {
        let mut source = self.source.lock().unwrap();
        let file = SourceFile::from_u16(path.to_string_lossy(), data.into_vec());
        let filespan = source.add(file);
        span.map(|span| *span = filespan);
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
pub extern "C" fn kailua_source_add_file(src: *const VSSource,
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
        src.add_file(path, span.0)
    }).unwrap_or(-1)
}

#[no_mangle]
pub extern "C" fn kailua_source_add_string(src: *const VSSource,
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
        src.add_string(path, data, span.0);
        0
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

