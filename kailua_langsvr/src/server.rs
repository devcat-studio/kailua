use std::str;
use std::error::Error;
use std::collections::HashMap;
use std::io::{self, Read, BufRead, Write, BufReader};
use std::net::TcpStream;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use serde::Serialize;
use serde_json::{self, Value};
use parking_lot::Mutex;
use futures::future::{Future, BoxFuture};
use futures::sync::oneshot::{self, Sender};
use fmtutils::Asis;
use protocol::{self, Id, Method};
use protocol::{Request, RequestMessage, Notification, NotificationMessage};
use protocol::{ResponseMessage, ResponseError, Message, MessageError};

fn invalid<E: Into<Box<Error + Send + Sync>>>(e: E) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, e)
}

struct ServerInner {
    reader: Mutex<Box<BufRead + Send + Sync>>,
    writer: Mutex<Box<Write + Send + Sync>>,

    next_id: AtomicUsize,

    // Values are deserialized by Receiver
    futures: Mutex<HashMap<Id, Sender<Result<Option<Value>, ResponseError<Value>>>>>,
}

#[derive(Clone)]
pub struct Server {
    inner: Arc<ServerInner>,
}

#[derive(Clone, Debug)]
pub enum Received {
    Request(Id, Request),
    Notification(Notification),
}

impl Server {
    pub fn new(reader: Box<BufRead + Send + Sync>,
               writer: Box<Write + Send + Sync>) -> Server {
        Server {
            inner: Arc::new(ServerInner {
                reader: Mutex::new(reader),
                writer: Mutex::new(writer),
                next_id: AtomicUsize::new(0),
                futures: Mutex::new(HashMap::new()),
            }),
        }
    }

    pub fn from_stdio() -> Server {
        Server::new(Box::new(BufReader::new(io::stdin())) as Box<BufRead + Send + Sync>,
                    Box::new(io::stdout()) as Box<Write + Send + Sync>)
    }

    pub fn from_tcp_stream(stream: TcpStream) -> io::Result<Server> {
        let stream2 = stream.try_clone()?;
        Ok(Server::new(Box::new(BufReader::new(stream)) as Box<BufRead + Send + Sync>,
                       Box::new(stream2) as Box<Write + Send + Sync>))
    }

    fn recv_msg(&self) -> io::Result<Vec<u8>> {
        let mut reader = self.inner.reader.lock();

        let mut line = Vec::new();
        let mut bodylen = None;
        loop {
            line.clear();
            reader.read_until(b'\n', &mut line)?;
            if line.is_empty() { return Err(invalid("premature end of stream")); }
            if line.pop() != Some(b'\n') { return Err(invalid("invalid header line")); }
            if line.pop() != Some(b'\r') { return Err(invalid("invalid header line")); }
            let len = line.len();
            if len == 0 {
                break;
            } else if len > 16 && line[..16] == b"Content-Length: "[..] {
                if bodylen.is_some() {
                    return Err(invalid("duplicate Content-Length header"));
                }
                let value = str::from_utf8(&line[16..]).map_err(invalid)?;
                bodylen = Some(value.parse().map_err(invalid)?);
            } else if len > 14 && line[..14] == b"Content-Type: "[..] {
                // ignored
            } else {
                return Err(invalid("invalid or unexpected header line"));
            }
        }
        if bodylen.is_none() {
            return Err(invalid("Content-Length header is missing"));
        }

        let mut body = vec![0; bodylen.unwrap()];
        reader.read_exact(&mut body)?;
        if cfg!(debug_assertions) {
            trace!("read: {}", Asis(&body));
        }
        Ok(body)
    }

    fn send_msg(&self, buf: &[u8]) -> io::Result<()> {
        let mut writer = self.inner.writer.lock();
        write!(writer, "Content-Length: {}\r\n\r\n", buf.len())?;
        writer.write(buf)?;
        writer.flush()?;
        drop(writer);

        if cfg!(debug_assertions) {
            trace!("write: {}", Asis(buf));
        }

        Ok(())
    }

    pub fn recv(&self) -> io::Result<Option<Received>> {
        let body = self.recv_msg()?;

        // now we can recover from the error; any error past this point is represented by None
        let json = match serde_json::from_slice::<Value>(&body) {
            Ok(json) => json,
            Err(e) => {
                error!("Server::recv failed to parse: {}", e);
                self.send_err(None, protocol::error_codes::PARSE_ERROR,
                              format!("parsing failed: {}", e), ())?;
                return Ok(None);
            }
        };
        let msg = match serde_json::from_value::<Message>(json) {
            Ok(msg) => msg,
            Err(e) => {
                error!("Server::recv got an invalid request: {}", e);
                self.send_err(None, protocol::error_codes::INVALID_REQUEST,
                              format!("invalid request: {}", e), ())?;
                return Ok(None);
            },
        };

        let ret = match msg {
            Message::Request(msg) => match Request::from_message(msg) {
                (id, Ok(req)) => Ok(Received::Request(id, req)),
                (id, Err(e)) => Err((Some(id), e)),
            },

            Message::Notification(msg) => match Notification::from_message(msg) {
                Ok(req) => Ok(Received::Notification(req)),
                Err(e) => Err((None, e)),
            },

            Message::Response(msg) => {
                if let Some(ref id) = msg.id {
                    if let Some(sender) = self.inner.futures.lock().remove(id) {
                        if let Some(error) = msg.error {
                            sender.complete(Err(error));
                        } else {
                            sender.complete(Ok(msg.result));
                        }
                    } else {
                        warn!("no callback registered for {:?}", msg);
                    }
                } else {
                    // this indicates an error from the server side, and cannot be handled
                    warn!("got and ignored {:?}", msg);
                }
                return Ok(None);
            },
        };

        match ret {
            Ok(received) => Ok(Some(received)),
            Err((id, MessageError::MethodNotFound(method))) => {
                error!("Server::recv got an unknown method: {}", method);
                self.send_err(id, protocol::error_codes::METHOD_NOT_FOUND,
                              format!("method not found: {}", method), ())?;
                Ok(None)
            },
            Err((id, MessageError::InvalidParams(estr))) => {
                error!("Server::recv got invalid parameters: {}", estr);
                self.send_err(id, protocol::error_codes::INVALID_PARAMS,
                              format!("invalid parameters: {}", estr), ())?;
                Ok(None)
            },
        }
    }

    pub fn send_ok<R: Serialize>(&self, id: Id, result: R) -> io::Result<()> {
        let msg = ResponseMessage::<R, ()> {
            version: protocol::Version,
            id: Some(id),
            result: Some(result),
            error: None,
        };
        let buf = serde_json::to_vec(&msg).map_err(invalid)?;
        self.send_msg(&buf)
    }

    pub fn send_err<M: Into<String>, E: Serialize>(&self, id: Option<Id>, code: i64,
                                                   message: M, data: E) -> io::Result<()> {
        let msg = ResponseMessage::<(), E> {
            version: protocol::Version,
            id: id,
            result: None,
            error: Some(ResponseError {
                code: code,
                message: message.into(),
                data: Some(data),
            }),
        };
        let buf = serde_json::to_vec(&msg).map_err(invalid)?;
        self.send_msg(&buf)
    }

    pub fn send_req<T: Serialize>(&self, method: Method, params: T)
        -> io::Result<BoxFuture<Option<Value>, ResponseError<Value>>>
    {
        let id = Id::Number(self.inner.next_id.fetch_add(1, Ordering::SeqCst) as i64);
        let (sender, receiver) = oneshot::channel();
        let prev = self.inner.futures.lock().insert(id.clone(), sender);
        assert!(prev.is_none(), "duplicate request id?!");

        let msg = RequestMessage::<T> {
            version: protocol::Version,
            id: id,
            method: method.as_str().into(),
            params: Some(params),
        };
        let buf = serde_json::to_vec(&msg).map_err(invalid)?;
        self.send_msg(&buf)?;

        Ok(receiver.then(|ret| {
            match ret {
                Ok(Ok(result)) => Ok(result),
                Ok(Err(error)) => Err(error),
                Err(_) => {
                    // possible when the server is shut down before the future is dropped
                    Err(ResponseError {
                        code: protocol::error_codes::UNKNOWN_ERROR_CODE,
                        message: "Canceled".into(), data: None,
                    })
                },
            }
        }).boxed())
    }

    pub fn send_notify<T: Serialize>(&self, method: Method, params: T) -> io::Result<()> {
        let msg = NotificationMessage::<T> {
            version: protocol::Version,
            method: method.as_str().into(),
            params: Some(params),
        };
        let buf = serde_json::to_vec(&msg).map_err(invalid)?;
        self.send_msg(&buf)
    }
}

