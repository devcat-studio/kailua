use std::cmp;
use std::cell::Cell;
use kailua_diag::{self, Span, Localize, Kind, Report, Stop};

pub struct VSReport {
    maxkind: Cell<Option<Kind>>,
}

impl VSReport {
    pub fn new() -> VSReport {
        VSReport { maxkind: Cell::new(None) }
    }
}

impl Report for VSReport {
    fn add_span(&self, kind: Kind, _span: Span, _msg: &Localize) -> kailua_diag::Result<()> {
        if let Some(maxkind) = self.maxkind.get() {
            self.maxkind.set(Some(cmp::max(maxkind, kind)));
        } else {
            self.maxkind.set(Some(kind));
        }
        if kind == Kind::Fatal { Err(Stop) } else { Ok(()) }
    }

    fn can_continue(&self) -> bool {
        self.maxkind.get() < Some(Kind::Error)
    }
}

