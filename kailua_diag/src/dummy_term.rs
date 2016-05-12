use std::io;
use std::io::Write;
use term;
use term::color::Color;
use term::{Attr, Terminal, StderrTerminal};

struct DummyTerminal<T: Write> {
    writer: T,
}

impl<T: Write> DummyTerminal<T> {
    pub fn new(writer: T) -> DummyTerminal<T> {
        DummyTerminal { writer: writer }
    }
}

impl<T: Write> Write for DummyTerminal<T> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> { self.writer.write(buf) }
    fn flush(&mut self) -> io::Result<()> { self.writer.flush() }
}

impl<T: Write> Terminal for DummyTerminal<T> {
    type Output = T;
    fn fg(&mut self, _color: Color) -> term::Result<()> { Err(term::Error::NotSupported) }
    fn bg(&mut self, _color: Color) -> term::Result<()> { Err(term::Error::NotSupported) }
    fn attr(&mut self, _attr: Attr) -> term::Result<()> { Err(term::Error::NotSupported) }
    fn supports_attr(&self, _attr: Attr) -> bool { false }
    fn reset(&mut self) -> term::Result<()> { Ok(()) }
    fn supports_reset(&self) -> bool { false }
    fn supports_color(&self) -> bool { false }
    fn cursor_up(&mut self) -> term::Result<()> { Err(term::Error::NotSupported) }
    fn delete_line(&mut self) -> term::Result<()> { Err(term::Error::NotSupported) }
    fn carriage_return(&mut self) -> term::Result<()> { Err(term::Error::NotSupported) }
    fn get_ref(&self) -> &Self::Output { &self.writer }
    fn get_mut(&mut self) -> &mut Self::Output { &mut self.writer }
    fn into_inner(self) -> Self::Output where Self: Sized { self.writer }
}

unsafe impl<T: Write + Sync> Sync for DummyTerminal<T> {}

pub fn stderr_or_dummy() -> Box<StderrTerminal> {
    match term::stderr() {
        Some(t) => t,
        None => Box::new(DummyTerminal::new(io::stderr())),
    }
}

