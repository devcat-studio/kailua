use std::ops;
use std::fmt::{self, Write};
use std::str;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Asis<T>(pub T);

impl<T: AsRef<[u8]>> fmt::Display for Asis<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut remaining: &[u8] = self.0.as_ref();
        loop {
            match str::from_utf8(remaining) {
                Ok(s) => return f.write_str(s),
                Err(e) => {
                    f.write_str(str::from_utf8(&remaining[..e.valid_up_to()]).unwrap())?;
                    f.write_char('\u{fffd}')?;
                    remaining = &remaining[e.valid_up_to() + 1..];
                }
            }
        }
    }
}

pub struct Ellipsis;

impl fmt::Display for Ellipsis {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "...") }
}

impl fmt::Debug for Ellipsis {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "...") }
}

macro_rules! delegate_wrappers {
    ($($t:ident),*) => ($(
        impl<T> ops::Deref for $t<T> {
            type Target = T;
            fn deref(&self) -> &T { &self.0 }
        }

        impl<T> ops::DerefMut for $t<T> {
            fn deref_mut(&mut self) -> &mut T { &mut self.0 }
        }
    )*)
}

delegate_wrappers! { Asis }

