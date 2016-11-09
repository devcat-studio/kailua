use std::mem;
use std::ptr;
use std::rc::Rc;
use std::cell::RefCell;
use std::panic::{self, AssertUnwindSafe};
use std::path::Path;
use report::VSReport;
use parse::VSParseTree;
use widestring::{WideStr, WideString};
use kailua_diag::Report;
use kailua_syntax::Chunk;
use kailua_check::{self, FsSource, FsOptions, Context};

pub type VSFsSourceCallback =
    extern "system" fn(*const u16, i32, usize, *mut *const VSParseTree) -> i32;

struct VSFsSource {
    callback: VSFsSourceCallback,
    callback_data: usize,
}

impl VSFsSource {
    fn new(callback: VSFsSourceCallback, callback_data: usize) -> VSFsSource {
        VSFsSource {
            callback: callback,
            callback_data: callback_data,
        }
    }
}

impl Clone for VSFsSource {
    fn clone(&self) -> Self {
        VSFsSource { ..*self }
    }
}

impl FsSource for VSFsSource {
    fn chunk_from_path(&self, resolved_path: &Path) -> Result<Option<Chunk>, String> {
        let path = WideString::from_str(resolved_path.as_os_str());
        let mut tree = ptr::null();
        let ret = (self.callback)(path.as_ptr(), path.len() as i32,
                                  self.callback_data, &mut tree as *mut _);
        if ret != 0 {
            Err(format!("callback returned an error"))
        } else {
            unsafe { Ok(tree.as_ref().map(|tree: &VSParseTree| tree.chunk().to_owned())) }
        }
    }
}

pub struct VSChecker {
    fssource: VSFsSource,
    report: Rc<Report>,
}

impl VSChecker {
    pub fn new(callback: VSFsSourceCallback, callback_data: usize,
               report: Rc<Report>) -> Box<VSChecker> {
        Box::new(VSChecker {
            fssource: VSFsSource::new(callback, callback_data),
            report: report,
        })
    }

    // this will call callbacks multiple times; the callback is free to deallocate
    // returned parse tree when the check ends.
    pub fn exec(&self, mainpath: &WideStr) -> i32 {
        let mainpath = mainpath.to_os_string();
        let mainpath = Path::new(&mainpath);
        let filechunk = match self.fssource.chunk_from_path(&mainpath) {
            Ok(Some(chunk)) => chunk,
            Ok(None) | Err(_) => return 1,
        };

        let root = mainpath.parent().unwrap_or(&Path::new(".."));
        let opts = Rc::new(RefCell::new(FsOptions::new(self.fssource.clone(), root.to_owned())));

        let mut context = Context::new(self.report.clone());
        if kailua_check::check_from_chunk(&mut context, filechunk, opts).is_ok() {
            0
        } else {
            1
        }
    }
}

#[no_mangle]
pub extern "C" fn kailua_checker_new(callback: VSFsSourceCallback,
                                     callback_data: usize,
                                     report: *const VSReport) -> *mut VSChecker {
    if report.is_null() { return ptr::null_mut(); }
    let report: &VSReport = unsafe { mem::transmute(report) };

    let report = AssertUnwindSafe(report); // XXX use Unique when it is stabilized
    panic::catch_unwind(move || {
        let checker = VSChecker::new(callback, callback_data, report.proxy());
        unsafe { mem::transmute(checker) }
    }).unwrap_or(ptr::null_mut())
}

#[no_mangle]
pub extern "C" fn kailua_checker_exec(checker: *mut VSChecker,
                                      mainpath: *const u16, mainpathlen: i32) -> i32 {
    if checker.is_null() { return -1; }
    if mainpath.is_null() || mainpathlen < 0 { return -1; }

    let checker: &VSChecker = unsafe { mem::transmute(checker) };
    let mainpath = unsafe { WideStr::from_ptr(mainpath, mainpathlen as usize) };

    let checker = AssertUnwindSafe(checker); // XXX use Unique when it is stabilized
    panic::catch_unwind(move || {
        checker.exec(&mainpath)
    }).unwrap_or(-1)
}

#[no_mangle]
pub extern "C" fn kailua_checker_free(checker: *mut VSChecker) {
    if checker.is_null() { return; }
    let checker: Box<VSChecker> = unsafe { mem::transmute(checker) };

    let checker = AssertUnwindSafe(checker); // XXX use Unique when it is stabilized
    let _ = panic::catch_unwind(move || {
        drop(checker);
    }); // cannot do much beyond this
}

