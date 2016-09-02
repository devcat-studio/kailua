use widestring::WideCString;
use std::panic;

#[no_mangle]
pub extern "C" fn kailua_strbuf_free(buf: *mut u16) {
    if buf.is_null() { return; }
    let buf = unsafe { WideCString::from_raw(buf) };

    let _ = panic::catch_unwind(move || {
        drop(buf);
    }); // cannot do much beyond this
}

