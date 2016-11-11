use std::i32;
use std::mem;
use std::borrow::Cow;
use std::iter::FromIterator;
use std::panic::{self, AssertUnwindSafe};
use std::marker::PhantomData;

pub const VS_NAME_ENTRY_NAME_ALLOC: u32 = 1;

#[repr(C)]
pub struct VSNameEntry<'a> {
    nameptr: *const u8,
    namelen: usize,
    scope: i32, // 0 if global, negative if not applicable
    flags: u32,
    _marker: PhantomData<&'a ()>,
}

impl<'a> VSNameEntry<'a> {
    pub fn new<N: Into<Cow<'a, [u8]>>>(name: N, scope: i32) -> VSNameEntry<'a> {
        let name: Cow<'a, [u8]> = name.into();
        let (nameptr, namelen, alloc) = match name {
            Cow::Borrowed(s) => (s.as_ptr(), s.len(), false),
            Cow::Owned(s) => {
                let s = s.into_boxed_slice();
                let raw = (s.as_ptr(), s.len(), true);
                mem::forget(s); // essentially owned by VSNameEntry
                raw
            }
        };
        VSNameEntry {
            nameptr: nameptr,
            namelen: namelen,
            scope: scope,
            flags: if alloc { VS_NAME_ENTRY_NAME_ALLOC } else { 0 },
            _marker: PhantomData,
        }
    }
}

impl<'a> Drop for VSNameEntry<'a> {
    fn drop(&mut self) {
        if self.flags & VS_NAME_ENTRY_NAME_ALLOC != 0 {
            let name: Box<[u8]> = unsafe { mem::transmute((self.nameptr, self.namelen)) };
            drop(name);
            self.flags &= !VS_NAME_ENTRY_NAME_ALLOC;
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
    if entries.is_null() || nentries < 0 { return; }
    let entries = unsafe { VSNameEntries::from_raw(entries, nentries) };

    let entries = AssertUnwindSafe(entries); // XXX use Unique when it is stabilized
    let _ = panic::catch_unwind(move || {
        drop(entries);
    }); // cannot do much beyond this
}

