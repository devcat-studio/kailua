use std::fmt;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CancelError<T> {
    Canceled,
    Error(T),
}

// CancelError::Canceled is expected to be produced only from CancelToken::keep_going
impl<T> From<T> for CancelError<T> {
    fn from(err: T) -> CancelError<T> { CancelError::Error(err) }
}

#[derive(Clone)]
pub struct CancelToken(Arc<AtomicBool>);

impl fmt::Debug for CancelToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<CancelToken {}>", if self.0.load(Ordering::Relaxed) { "on" } else { "off" })
    }
}

impl CancelToken {
    pub fn new() -> CancelToken {
        CancelToken(Arc::new(AtomicBool::new(false)))
    }

    pub fn keep_going<T>(&self) -> Result<(), CancelError<T>> {
        if self.0.load(Ordering::Relaxed) {
            Err(CancelError::Canceled)
        } else {
            Ok(())
        }
    }

    pub fn cancel(&self) -> bool {
        !self.0.swap(true, Ordering::Relaxed)
    }
}

