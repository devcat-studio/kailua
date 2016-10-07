use std::mem;
use std::ptr;
use std::panic::{self, AssertUnwindSafe};
use lex::VSTokenStream;
use report::VSReport;
use kailua_diag::{Spanned, Report};
use kailua_syntax::{Block, St, Parser};

pub struct VSParseTree {
    chunk: Spanned<Block>,
}

impl VSParseTree {
    pub fn new(stream: Box<VSTokenStream>, report: &Report) -> Option<Box<VSParseTree>> {
        let tokens = stream.into_tokens();
        let parser = Parser::new(tokens.into_iter(), report);
        if let Ok(chunk) = parser.into_chunk() {
            Some(Box::new(VSParseTree { chunk: chunk }))
        } else {
            None
        }
    }

    pub fn to_chunk(&self) -> Spanned<Block> {
        self.chunk.clone() // XXX copy
    }

    // search for directives like `--# open lua51` likely in the entry point
    // TODO should really (transitively) check for `--# set lang_version = "5.1"` or so
    fn has_primitive_open(&self) -> i32 {
        match self.chunk.base.first().map(|st| &*st.base) {
            Some(&St::KailuaOpen(ref name)) if name.starts_with(&b"lua5"[..]) => 1,
            _ => 0,
        }
    }
}

#[no_mangle]
pub extern "C" fn kailua_parse_tree_new(stream: *mut VSTokenStream,
                                        report: *const VSReport) -> *mut VSParseTree {
    // stream should be consumed immediately if it is ever supplied!
    if stream.is_null() { return ptr::null_mut(); }
    let stream: Box<VSTokenStream> = unsafe { mem::transmute(stream) };

    if report.is_null() { return ptr::null_mut(); }
    let report: &VSReport = unsafe { mem::transmute(report) };

    let stream = AssertUnwindSafe(stream); // XXX use Unique when it is stabilized
    let report = AssertUnwindSafe(report);
    panic::catch_unwind(move || {
        let tree = VSParseTree::new(stream.0, &report.proxy());
        unsafe { mem::transmute(tree) }
    }).unwrap_or(ptr::null_mut())
}

#[no_mangle]
pub extern "C" fn kailua_parse_tree_has_prim_open(tree: *const VSParseTree) -> i32 {
    if tree.is_null() { return -1; }
    let tree: &VSParseTree = unsafe { mem::transmute(tree) };

    let tree = AssertUnwindSafe(tree);
    panic::catch_unwind(move || {
        tree.has_primitive_open()
    }).unwrap_or(-1)
}

#[no_mangle]
pub extern "C" fn kailua_parse_tree_free(tree: *mut VSParseTree) {
    if tree.is_null() { return; }
    let tree: Box<VSParseTree> = unsafe { mem::transmute(tree) };

    let tree = AssertUnwindSafe(tree); // XXX use Unique when it is stabilized
    let _ = panic::catch_unwind(move || {
        drop(tree);
    }); // cannot do much beyond this
}

