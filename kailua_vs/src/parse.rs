use std::mem;
use std::ptr;
use std::i32;
use std::panic::{self, AssertUnwindSafe};
use lex::VSTokenStream;
use report::VSReport;
use kailua_env::Pos;
use kailua_diag::Report;
use kailua_syntax::{St, Chunk, Parser};

pub struct VSParseTree {
    chunk: Chunk,
}

#[repr(C)]
pub struct VSNameEntry {
    pub name: *const u8,
    pub namelen: usize,
    pub scope: u32, // 0 if global
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

    pub fn names_at_pos(&mut self, pos: Pos) -> Option<Box<[VSNameEntry]>> {
        self.chunk.map.scope_from_pos(pos).map(|scope| {
            self.chunk.map.names_and_scopes(scope).map(|(name, scope, _id)| {
                VSNameEntry {
                    name: name.as_ptr(),
                    namelen: name.len(),
                    scope: scope.to_usize() as u32,
                }
            }).collect::<Vec<_>>().into_boxed_slice()
        })
    }

    pub fn global_names(&self) -> Box<[VSNameEntry]> {
        self.chunk.global_scope.iter().map(|name| {
            VSNameEntry {
                name: name.as_ptr(),
                namelen: name.len(),
                scope: 0,
            }
        }).collect::<Vec<_>>().into_boxed_slice()
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
pub extern "C" fn kailua_parse_tree_names_at_pos(tree: *mut VSParseTree, pos: *const Pos,
                                                 out: *mut *mut VSNameEntry) -> i32 {
    if tree.is_null() { return -1; }
    if pos.is_null() { return -1; }
    if out.is_null() { return -1; }

    let tree: &mut VSParseTree = unsafe { mem::transmute(tree) };
    let pos = unsafe { *pos };
    let out = unsafe { out.as_mut().unwrap() };

    let tree = AssertUnwindSafe(tree);
    let out = AssertUnwindSafe(out);
    panic::catch_unwind(move || {
        if let Some(entries) = tree.0.names_at_pos(pos) {
            assert!(entries.len() <= i32::MAX as usize);
            let (entries, nentries): (*mut VSNameEntry, usize) = unsafe { mem::transmute(entries) };
            *out.0 = entries;
            nentries as i32
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
        let entries = tree.0.global_names();
        assert!(entries.len() <= i32::MAX as usize);
        let (entries, nentries): (*mut VSNameEntry, usize) = unsafe { mem::transmute(entries) };
        *out.0 = entries;
        nentries as i32
    }).unwrap_or(-1)
}

#[no_mangle]
pub extern "C" fn kailua_parse_tree_free_names(entries: *mut VSNameEntry, nentries: i32) {
    if entries.is_null() { return; }
    let nentries = nentries as usize;
    let entries: Box<[VSNameEntry]> = unsafe { mem::transmute((entries, nentries)) };

    let entries = AssertUnwindSafe(entries); // XXX use Unique when it is stabilized
    let _ = panic::catch_unwind(move || {
        drop(entries);
    }); // cannot do much beyond this
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

