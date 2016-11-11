use std::i32;
use std::mem;
use std::iter::FromIterator;
use std::panic::{self, AssertUnwindSafe};
use std::marker::PhantomData;

#[repr(C)]
pub struct VSNameEntry<'a> {
    pub name: *const u8,
    pub namelen: usize,
    pub scope: i32, // 0 if global, negative if not applicable
    _marker: PhantomData<&'a ()>,
}

impl<'a> VSNameEntry<'a> {
    pub fn new(name: &'a [u8], scope: i32) -> VSNameEntry<'a> {
        VSNameEntry {
            name: name.as_ptr(),
            namelen: name.len(),
            scope: scope,
            _marker: PhantomData,
        }
    }
}

#[repr(C)]
pub struct VSNameEntries<'a>(pub Box<[VSNameEntry<'a>]>);

impl<'a> VSNameEntries<'a> {
    pub fn from(entries: Vec<VSNameEntry<'a>>) -> VSNameEntries<'a> {
        VSNameEntries(entries.into_boxed_slice())
    }

    pub unsafe fn from_raw(entries: *mut VSNameEntry, nentries: i32) -> VSNameEntries<'a> {
        assert!(nentries >= 0);
        VSNameEntries(mem::transmute((entries, nentries as usize)))
    }

    pub fn into_raw(self, out: &mut *mut VSNameEntry) -> i32 {
        assert!(self.0.len() <= i32::MAX as usize);
        let (entries, nentries): (*mut VSNameEntry, usize) = unsafe { mem::transmute(self) };
        *out = entries;
        nentries as i32
    }
}

impl<'a> FromIterator<VSNameEntry<'a>> for VSNameEntries<'a> {
    fn from_iter<T: IntoIterator<Item=VSNameEntry<'a>>>(iter: T) -> Self {
        VSNameEntries::from(iter.into_iter().collect::<Vec<_>>())
    }
}

#[no_mangle]
pub extern "C" fn kailua_free_names(entries: *mut VSNameEntry, nentries: i32) {
    if entries.is_null() { return; }
    if nentries < 0 { return; }
    let entries = unsafe { VSNameEntries::from_raw(entries, nentries) };

    let entries = AssertUnwindSafe(entries); // XXX use Unique when it is stabilized
    let _ = panic::catch_unwind(move || {
        drop(entries);
    }); // cannot do much beyond this
}

