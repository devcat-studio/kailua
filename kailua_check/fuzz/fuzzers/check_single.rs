// run with:
// RUST_BACKTRACE=1 LSAN_OPTIONS=detect_leaks=0 cargo fuzz run -s leak check_single

#![no_main]

extern crate libfuzzer_sys;
#[macro_use] extern crate lazy_static;
extern crate thread_pool;
extern crate kailua_env;
extern crate kailua_diag;
extern crate kailua_syntax;
extern crate kailua_check;

#[export_name="rust_fuzzer_test_input"]
pub extern fn go(data: &[u8]) {
    use std::usize;
    use std::rc::Rc;
    use std::cell::RefCell;
    use std::sync::mpsc;
    use std::time::Duration;
    use thread_pool as tp;
    use kailua_env::{Span, Source, SourceFile};
    use kailua_diag::{Kind, Report, Locale, Localize};
    use kailua_check::{Context, Options};

    struct DummyReport;

    impl Report for DummyReport {
        fn message_locale(&self) -> Locale { Locale::dummy() }
        fn add_span(&self, _: Kind, _: Span, _: &Localize) -> kailua_diag::Result<()> { Ok(()) }
    }

    struct DummyOptions;

    impl Options for DummyOptions {}

    lazy_static! {
        static ref POOL: (tp::Sender<Box<tp::TaskBox>>, tp::ThreadPool<Box<tp::TaskBox>>) = {
            tp::Builder::new().work_queue_capacity(usize::MAX).build()
        };
    }

    const TIMEOUT_MS: u64 = 5000;

    let (sender, receiver) = mpsc::channel();
    let data = data.to_owned();
    let _ = POOL.0.send_fn(move || {
        let source = Rc::new(RefCell::new(Source::new()));
        let report = Rc::new(DummyReport);
        let file = SourceFile::from_u8("<fuzz input>".into(), data.to_owned());
        let span = source.borrow_mut().add(file);
        let source = source.borrow();
        if let Ok(chunk) = kailua_syntax::parse_chunk(&source, span, &*report) {
            let mut context = Context::new(report.clone());
            let opts = Rc::new(RefCell::new(DummyOptions));
            let _ = kailua_check::check_from_chunk(&mut context, chunk, opts);
        }
        let _ = sender.send(());
    });
    if receiver.recv_timeout(Duration::from_millis(TIMEOUT_MS)).is_err() {
        panic!("timed out after {}ms", TIMEOUT_MS);
    }
}

