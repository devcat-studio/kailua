use std::mem;
use std::ptr;
use std::i32;
use std::panic::{self, AssertUnwindSafe};
use lex::VSTokenStream;
use names::{VSNameEntry, VSNameEntries};
use report::VSReport;
use kailua_env::Pos;
use kailua_diag::Report;
use kailua_syntax::{St, Chunk, Parser};

pub struct VSParseTree {
    chunk: Chunk,
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

    pub fn chunk(&self) -> &Chunk { &self.chunk }

    // search for directives like `--# open lua51` likely in the entry point
    // TODO should really (transitively) check for `--# set lang_version = "5.1"` or so
    pub fn has_primitive_open(&self) -> i32 {
        match self.chunk.block.base.first().map(|st| &*st.base) {
            Some(&St::KailuaOpen(ref name)) if name.starts_with(&b"lua5"[..]) => 1,
            _ => 0,
        }
    }

    pub fn names_at_pos(&self, pos: Pos) -> Option<VSNameEntries> {
        self.chunk.map.scope_from_pos(pos).map(|scope| {
            self.chunk.map.names_and_scopes(scope).map(|(name, scope, _id)| {
                VSNameEntry::new(&name[..], scope.to_usize() as i32)
            }).collect()
        })
    }

    pub fn global_names(&self) -> VSNameEntries {
        self.chunk.global_scope.iter().map(|name| VSNameEntry::new(&name[..], 0)).collect()
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
pub extern "C" fn kailua_parse_tree_names_at_pos(tree: *const VSParseTree, pos: *const Pos,
                                                 out: *mut *mut VSNameEntry) -> i32 {
    if tree.is_null() { return -1; }
    if pos.is_null() { return -1; }
    if out.is_null() { return -1; }

    let tree: &VSParseTree = unsafe { mem::transmute(tree) };
    let pos = unsafe { *pos };
    let out = unsafe { out.as_mut().unwrap() };

    let tree = AssertUnwindSafe(tree);
    let out = AssertUnwindSafe(out);
    panic::catch_unwind(move || {
        if let Some(entries) = tree.0.names_at_pos(pos) {
            entries.into_raw(out.0)
        } else {
            *out.0 = ptr::null_mut();
            0
        }
    }).unwrap_or(-1)
}

#[no_mangle]
pub extern "C" fn kailua_parse_tree_global_names(tree: *const VSParseTree,
                                                 out: *mut *mut VSNameEntry) -> i32 {
    if tree.is_null() { return -1; }
    if out.is_null() { return -1; }

    let tree: &VSParseTree = unsafe { mem::transmute(tree) };
    let out = unsafe { out.as_mut().unwrap() };

    let tree = AssertUnwindSafe(tree);
    let out = AssertUnwindSafe(out);
    panic::catch_unwind(move || {
        tree.0.global_names().into_raw(out.0)
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

