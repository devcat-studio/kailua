use std::mem;
use std::ptr;
use std::rc::Rc;
use std::cell::RefCell;
use std::sync::Arc;
use std::panic::{self, AssertUnwindSafe};
use std::path::Path;
use names::{VSNameEntry, VSNameEntries};
use report::VSReport;
use parse::VSParseTree;
use widestring::{WideStr, WideString};
use kailua_env::{Pos, Span, Spanned};
use kailua_diag::Report;
use kailua_syntax::Chunk;
use kailua_check::{self, FsSource, FsOptions, Id, Context, Slot, Tables, Key};
use kailua_check::flags::T_STRING;

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
    report: Arc<Report + Send + Sync>,
}

impl VSChecker {
    pub fn new(callback: VSFsSourceCallback, callback_data: usize,
               report: Arc<Report + Send + Sync>) -> Box<VSChecker> {
        Box::new(VSChecker {
            fssource: VSFsSource::new(callback, callback_data),
            report: report,
        })
    }

    // this will call callbacks multiple times; the callback is free to deallocate
    // returned parse tree when the check ends.
    pub fn exec(&self, mainpath: &WideStr, output: &mut Option<Box<VSCheckerOutput>>) -> i32 {
        *output = None;

        let mainpath = mainpath.to_os_string();
        let mainpath = Path::new(&mainpath);
        let filechunk = match self.fssource.chunk_from_path(&mainpath) {
            Ok(Some(chunk)) => chunk,
            Ok(None) | Err(_) => return 1,
        };

        let root = mainpath.parent().unwrap_or(&Path::new(".."));
        let opts = Rc::new(RefCell::new(FsOptions::new(self.fssource.clone(), root.to_owned())));

        let mut context = Context::new(self.report.clone());
        let success = kailua_check::check_from_chunk(&mut context, filechunk, opts).is_ok();
        *output = Some(Box::new(VSCheckerOutput { context: context }));
        if success { 0 } else { 1 }
    }
}

pub struct VSCheckerOutput {
    // TODO not all of this might be required
    context: Context<Arc<Report + Send + Sync>>,
}

impl VSCheckerOutput {
    pub fn global_names(&self) -> VSNameEntries {
        self.context.all().filter_map(|(id, _)| {
            match *id {
                Id::Global(ref s) => Some(VSNameEntry::new(s[..].to_owned(), 0)),
                Id::Local(..) => None
            }
        }).collect()
    }

    fn slot_before_pos(&self, pos: Pos) -> Option<Spanned<&Slot>> {
        // find all slot-associated spans that intersects (even at the end points) `pos`...
        let spans = self.context.spanned_slots().adjacencies(Span::from(pos));
        // ...and keep spans which actually _ends_ at `pos`...
        let spans_before = spans.filter(|slot| slot.span.end() == pos);
        // ...and pick the smallest one among them (there should be at most one such span).
        spans_before.min_by_key(|slot| slot.span.len())
    }

    // XXX required to be mutable because it can generate a union currently
    pub fn fields_after_pos(&mut self, pos: Pos) -> Option<VSNameEntries> {
        if let Some(slot) = self.slot_before_pos(pos).map(|s| s.map(|s| s.clone())) {
            if let Some(mut ty) = self.context.resolve_exact_type(&slot.unlift()) {
                if ty.flags() == T_STRING {
                    if let Some(metaslot) = self.context.get_string_meta() {
                        // use the string meta table for strings
                        if let Some(metaty) = self.context.resolve_exact_type(&metaslot.unlift()) {
                            ty = metaty;
                        }
                    }
                }

                if let Some(&Tables::Fields(ref fields)) = ty.get_tables() {
                    let fields = fields.keys().filter_map(|key| {
                        match *key {
                            Key::Str(ref s) => Some(VSNameEntry::new(s[..].to_owned(), -1)),
                            Key::Int(_) => None,
                        }
                    }).collect();
                    return Some(fields);
                }
            }
        }

        None
    }
}

#[no_mangle]
pub extern "C" fn kailua_checker_new(callback: VSFsSourceCallback,
                                     callback_data: usize,
                                     report: *const VSReport) -> *mut VSChecker {
    if report.is_null() { return ptr::null_mut(); }
    let report: &Arc<VSReport> = unsafe { mem::transmute(&report) };

    let report = AssertUnwindSafe(report); // XXX use Unique when it is stabilized
    panic::catch_unwind(move || {
        let checker = VSChecker::new(callback, callback_data, report.clone());
        unsafe { mem::transmute(checker) }
    }).unwrap_or(ptr::null_mut())
}

#[no_mangle]
pub extern "C" fn kailua_checker_exec(checker: *mut VSChecker,
                                      mainpath: *const u16, mainpathlen: i32,
                                      output: *mut *mut VSCheckerOutput) -> i32 {
    if checker.is_null() { return -1; }
    if mainpath.is_null() || mainpathlen < 0 { return -1; }
    if output.is_null() { return -1; }

    let checker: &VSChecker = unsafe { mem::transmute(checker) };
    let mainpath = unsafe { WideStr::from_ptr(mainpath, mainpathlen as usize) };
    let output: &mut Option<Box<VSCheckerOutput>> = unsafe { mem::transmute(output) };

    let checker = AssertUnwindSafe(checker);
    let output = AssertUnwindSafe(output);
    panic::catch_unwind(move || {
        checker.exec(&mainpath, output.0)
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

#[no_mangle]
pub extern "C" fn kailua_checker_output_global_names(output: *mut VSCheckerOutput,
                                                     names: *mut *mut VSNameEntry) -> i32 {
    if output.is_null() { return -1; }
    if names.is_null() { return -1; }

    let output: &mut VSCheckerOutput = unsafe { mem::transmute(output) };
    let names = unsafe { names.as_mut().unwrap() };

    let output = AssertUnwindSafe(output);
    let names = AssertUnwindSafe(names);
    panic::catch_unwind(move || {
        output.0.global_names().into_raw(names.0)
    }).unwrap_or(-1)
}

#[no_mangle]
pub extern "C" fn kailua_checker_output_fields_after_pos(output: *mut VSCheckerOutput,
                                                         pos: *const Pos,
                                                         fields: *mut *mut VSNameEntry) -> i32 {
    if output.is_null() { return -1; }
    if pos.is_null() { return -1; }
    if fields.is_null() { return -1; }

    let output: &mut VSCheckerOutput = unsafe { mem::transmute(output) };
    let pos = unsafe { *pos };
    let fields = unsafe { fields.as_mut().unwrap() };

    let output = AssertUnwindSafe(output);
    let fields = AssertUnwindSafe(fields);
    panic::catch_unwind(move || {
        if let Some(entries) = output.0.fields_after_pos(pos) {
            entries.into_raw(fields.0)
        } else {
            *fields.0 = ptr::null_mut();
            0
        }
    }).unwrap_or(-1)
}

#[no_mangle]
pub extern "C" fn kailua_checker_output_free(output: *mut VSCheckerOutput) {
    if output.is_null() { return; }
    let output: Box<VSCheckerOutput> = unsafe { mem::transmute(output) };

    let output = AssertUnwindSafe(output); // XXX use Unique when it is stabilized
    let _ = panic::catch_unwind(move || {
        drop(output);
    }); // cannot do much beyond this
}

