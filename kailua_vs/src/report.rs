use std::cmp;
use std::mem;
use std::ptr;
use std::cell::Cell;
use std::rc::Rc;
use std::sync::Mutex;
use std::sync::mpsc::{self, Sender, Receiver};
use std::panic::{self, AssertUnwindSafe};
use widestring::{WideCStr, WideCString};
use kailua_diag::{self, Span, Localize, Localized, Kind, Report, Stop};

// report can be shared by multiple parties, possibly across multiple threads.
// but the current Kailua interfaces are NOT thread-safe since its normal usage is a single thread.
// there is not much point to make it fully thread-safe (it changes a type, after all),
// so we instead have a proxy Report (VSReportProxy) for each usage
// which sends the diagnostics back to the main thread-safe Report (VSReport).

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum VSReportKind {
    Note = 0,
    Warning = 1,
    Error = 2,
    Fatal = 3,
}

impl VSReportKind {
    pub fn from(kind: Kind) -> VSReportKind {
        match kind {
            Kind::Note => VSReportKind::Note,
            Kind::Warning => VSReportKind::Warning,
            Kind::Error => VSReportKind::Error,
            Kind::Fatal => VSReportKind::Fatal,
        }
    }
}

#[derive(Clone)]
struct Diag {
    kind: Kind,
    span: Span,
    msg: String,
}

struct ReportInner {
    lang: String,
    sender: Sender<Diag>,
    receiver: Receiver<Diag>, // acts as an infinite queue
}

pub struct VSReport {
    // it is not Arc<Mutex<...>> since C# has no notion of ownership and thus Arc is not required
    inner: Mutex<ReportInner>,
}

impl VSReport {
    pub fn new(lang: &str) -> Box<VSReport> {
        let (sender, reciever) = mpsc::channel();
        Box::new(VSReport {
            inner: Mutex::new(ReportInner {
                lang: lang.to_owned(),
                sender: sender,
                receiver: reciever,
            }),
        })
    }

    // while this is Rc, it is uniquely owned by each use of Report
    pub fn proxy(&self) -> Rc<Report> {
        let inner = self.inner.lock().unwrap();
        Rc::new(VSReportProxy {
            lang: inner.lang.clone(),
            maxkind: Cell::new(None),
            sender: inner.sender.clone(),
        })
    }

    pub fn get_next(&self, kind: &mut VSReportKind, span: &mut Span,
                    msg: &mut WideCString) -> i32 {
        let inner = self.inner.lock().unwrap();
        if let Ok(diag) = inner.receiver.try_recv() {
            if let Ok(msgw) = WideCString::from_str(diag.msg) {
                *kind = VSReportKind::from(diag.kind);
                *span = diag.span;
                *msg = msgw;
                return 1;
            }
        }

        *kind = VSReportKind::Note;
        *span = Span::dummy();
        *msg = WideCString::new();
        0
    }
}

struct VSReportProxy {
    lang: String,
    maxkind: Cell<Option<Kind>>,
    sender: Sender<Diag>,
}

impl Report for VSReportProxy {
    fn add_span(&self, kind: Kind, span: Span, msg: &Localize) -> kailua_diag::Result<()> {
        let msg = Localized::new(msg, &self.lang).to_string();
        let diag = Diag { kind: kind, span: span, msg: msg };
        self.sender.send(diag).expect("VSReportProxy::add_span failed to send diags");

        // maxkind is local to the current proxy instance
        if let Some(maxkind) = self.maxkind.get() {
            self.maxkind.set(Some(cmp::max(maxkind, kind)));
        } else {
            self.maxkind.set(Some(kind));
        }
        if kind == Kind::Fatal { Err(Stop) } else { Ok(()) }
    }

    fn can_continue(&self) -> bool {
        self.maxkind.get() < Some(Kind::Error)
    }
}

#[no_mangle]
pub extern "C" fn kailua_report_new(lang: *const u16) -> *const VSReport {
    if lang.is_null() { return ptr::null(); }

    let lang = unsafe { WideCStr::from_ptr_str(lang) };
    let lang = lang.to_string_lossy();

    panic::catch_unwind(move || {
        let report = VSReport::new(&lang);
        unsafe { mem::transmute(report) }
    }).unwrap_or(ptr::null())
}

#[no_mangle]
pub extern "C" fn kailua_report_get_next(report: *const VSReport, kind: *mut VSReportKind,
                                         span: *mut Span, msg: *mut *mut u16) -> i32 {
    if report.is_null() { return -1; }
    if kind.is_null() { return -1; }
    if span.is_null() { return -1; }
    if msg.is_null() { return -1; }

    let report: &VSReport = unsafe { mem::transmute(report) };
    let kind = unsafe { kind.as_mut().unwrap() };
    let span = unsafe { span.as_mut().unwrap() };
    let msg = unsafe { msg.as_mut().unwrap() };

    let report = AssertUnwindSafe(report);
    let kind = AssertUnwindSafe(kind);
    let span = AssertUnwindSafe(span);
    let msg = AssertUnwindSafe(msg);
    panic::catch_unwind(move || {
        let mut msgstr = WideCString::new();
        let ret = report.get_next(kind.0, span.0, &mut msgstr);
        *msg.0 = msgstr.into_raw();
        ret
    }).unwrap_or(-1)
}

#[no_mangle]
pub extern "C" fn kailua_report_free(report: *const VSReport) {
    if report.is_null() { return; }
    let report: Box<VSReport> = unsafe { mem::transmute(report) };

    let report = AssertUnwindSafe(report); // XXX use Unique when it is stabilized
    let _ = panic::catch_unwind(move || {
        drop(report);
    }); // cannot do much beyond this
}

