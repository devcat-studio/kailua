use std::str;
use std::error::Error;
use std::io::{self, Read, BufRead, Write, BufReader};
use std::net::TcpStream;
use serde::Serialize;
use serde_json::{self, Value};
use parking_lot::Mutex;
use protocol::{self, Id, Request, RequestMessage, RequestError, ResponseMessage, ResponseError};

fn invalid<E: Into<Box<Error + Send + Sync>>>(e: E) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, e)
}

pub struct Server {
    reader: Mutex<Box<BufRead + Send + Sync>>,
    writer: Mutex<Box<Write + Send + Sync>>,
}

impl Server {
    pub fn new(reader: Box<BufRead + Send + Sync>,
               writer: Box<Write + Send + Sync>) -> Server {
        Server { reader: Mutex::new(reader), writer: Mutex::new(writer) }
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
        let mut reader = self.reader.lock();

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
        Ok(body)
    }

    fn send_msg(&self, buf: &[u8]) -> io::Result<()> {
        let mut writer = self.writer.lock();
        write!(writer, "Content-Length: {}\r\n\r\n", buf.len())?;
        writer.write(buf)?;
        drop(writer);

        if cfg!(debug_assertions) {
            let mut err = ::std::io::stderr();
            write!(err, "write: ")?;
            err.write(buf)?;
            write!(err, "\n")?;
        }

        Ok(())
    }

    pub fn recv(&self) -> io::Result<Option<Request>> {
        let body = self.recv_msg()?;

        // now we can recover from the error; any error past this point is represented by None
        let json = match serde_json::from_slice::<Value>(&body) {
            Ok(json) => json,
            Err(e) => {
                self.send_err(None, protocol::error_codes::PARSE_ERROR,
                              format!("parsing failed: {}", e), ())?;
                return Ok(None);
            }
        };
        let msg = match serde_json::from_value::<RequestMessage>(json) {
            Ok(msg) => msg,
            Err(e) => {
                self.send_err(None, protocol::error_codes::INVALID_REQUEST,
                              format!("invalid request: {}", e), ())?;
                return Ok(None);
            },
        };
        let req = match Request::from_message(msg) {
            Ok(req) => req,
            Err((id, RequestError::MethodNotFound(method))) => {
                self.send_err(id, protocol::error_codes::METHOD_NOT_FOUND,
                              format!("method not found: {}", method), ())?;
                return Ok(None);
            },
            Err((id, RequestError::InvalidRequest(estr))) => {
                self.send_err(id, protocol::error_codes::INVALID_REQUEST,
                              format!("invalid request: {}", estr), ())?;
                return Ok(None);
            },
            Err((id, RequestError::InvalidParams(estr))) => {
                self.send_err(id, protocol::error_codes::INVALID_PARAMS,
                              format!("invalid parameters: {}", estr), ())?;
                return Ok(None);
            },
        };

        Ok(Some(req))
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

    pub fn send_notify<T: Serialize>(&self, method: &str, params: T) -> io::Result<()> {
        let msg = RequestMessage::<T> {
            version: protocol::Version,
            id: None,
            method: method.into(),
            params: Some(params),
        };
        let buf = serde_json::to_vec(&msg).map_err(invalid)?;
        self.send_msg(&buf)
    }
}

