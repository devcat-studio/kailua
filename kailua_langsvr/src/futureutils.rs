use std::fmt;
use std::sync::Arc;
use futures::{Future, Poll};
use futures::future::Shared;
use futures::sync::oneshot::{self, Receiver, Sender};
use parking_lot::Mutex;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CancelError<T> {
    Canceled,
    Error(T),
}

// CancelError::Canceled is expected to be produced only from CancelToken::keep_going
impl<T> From<T> for CancelError<T> {
    fn from(err: T) -> CancelError<T> { CancelError::Error(err) }
}

struct CancelTokenInner {
    sender: Mutex<Option<Sender<()>>>,
    receiver: Shared<Receiver<()>>,
}

#[derive(Clone)]
pub struct CancelToken(Arc<CancelTokenInner>);

impl fmt::Debug for CancelToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let canceled = self.0.sender.lock().is_none();
        write!(f, "<CancelToken {}>", if canceled { "on" } else { "off" })
    }
}

impl CancelToken {
    pub fn new() -> CancelToken {
        let (sender, receiver) = oneshot::channel();
        CancelToken(Arc::new(CancelTokenInner {
            sender: Mutex::new(Some(sender)),
            receiver: receiver.shared(),
        }))
    }

    pub fn keep_going<T>(&self) -> Result<(), CancelError<T>> {
        if self.0.sender.lock().is_none() {
            Err(CancelError::Canceled)
        } else {
            Ok(())
        }
    }

    pub fn future(&self) -> CancelFuture {
        CancelFuture { receiver: self.0.receiver.clone() }
    }

    pub fn cancel(&self) -> bool {
        if let Some(sender) = self.0.sender.lock().take() {
            // guaranteed to be entered by only one thread
            sender.complete(());
            true
        } else {
            false
        }
    }
}

#[derive(Clone)]
pub struct CancelFuture {
    receiver: Shared<Receiver<()>>,
}

impl Future for CancelFuture {
    type Item = (); // cancel was requested
    type Error = (); // cancel wasn't requested and the sender has gone (impossible)

    fn poll(&mut self) -> Poll<(), ()> {
        match self.receiver.poll() {
            Ok(async) => Ok(async.map(|_| ())),
            Err(_) => Err(()),
        }
    }
}

