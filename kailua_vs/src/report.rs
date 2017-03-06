use std::mem;
use std::ptr;
use std::sync::{Arc, Mutex};
use std::collections::VecDeque;
use std::panic::{self, AssertUnwindSafe};
use widestring::{WideCStr, WideCString};
use kailua_env::Span;
use kailua_diag::{self, Locale, Localize, Localized, Kind, Report, Stop};

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum VSReportKind {
    Note = 0,
    Info = 1,
    Cause = 5,
    Warning = 2,
    Error = 3,
    Fatal = 4,
}

impl VSReportKind {
    pub fn from(kind: Kind) -> VSReportKind {
        match kind {
            Kind::Note => VSReportKind::Note,
            Kind::Info => VSReportKind::Info,
            Kind::Cause => VSReportKind::Cause,
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

pub struct VSReport {
    locale: Locale,
    reports: Mutex<VecDeque<Diag>>,
}

impl VSReport {
    pub fn new(lang: &str) -> Option<Arc<VSReport>> {
        if let Some(locale) = Locale::new(lang) {
            Some(Arc::new(VSReport {
                locale: locale,
                reports: Mutex::new(VecDeque::new()),
            }))
        } else {
            None
        }
    }

    pub fn get_next(&self, kind: &mut VSReportKind, span: &mut Span,
                    msg: &mut WideCString) -> i32 {
        let mut reports = self.reports.lock().unwrap();
        if let Some(diag) = reports.pop_front() {
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

impl Report for VSReport {
    fn message_locale(&self) -> Locale {
        self.locale
    }

    fn add_span(&self, kind: Kind, span: Span, msg: &Localize) -> kailua_diag::Result<()> {
        let msg = Localized::new(msg, self.locale).to_string();
        let diag = Diag { kind: kind, span: span, msg: msg };

        self.reports.lock().unwrap().push_back(diag);

        if kind == Kind::Fatal { Err(Stop) } else { Ok(()) }
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

    let report: &Arc<VSReport> = unsafe { mem::transmute(&report) };
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
    let report: Arc<VSReport> = unsafe { mem::transmute(report) };

    let report = AssertUnwindSafe(report); // XXX use Unique when it is stabilized
    let _ = panic::catch_unwind(move || {
        drop(report);
    }); // cannot do much beyond this
}

