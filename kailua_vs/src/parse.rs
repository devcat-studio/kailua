use std::mem;
use std::ptr;
use std::panic::{self, AssertUnwindSafe};
use lex::VSTokenStream;
use report::VSReport;
use kailua_diag::{Spanned, Report};
use kailua_syntax::{Block, Parser};

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
pub extern "C" fn kailua_parse_tree_free(tree: *mut VSParseTree) {
    if tree.is_null() { return; }
    let tree: Box<VSParseTree> = unsafe { mem::transmute(tree) };

    let tree = AssertUnwindSafe(tree); // XXX use Unique when it is stabilized
    let _ = panic::catch_unwind(move || {
        drop(tree);
    }); // cannot do much beyond this
}

