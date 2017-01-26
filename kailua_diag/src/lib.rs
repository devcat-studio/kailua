extern crate term;
extern crate unicode_width;
extern crate parse_generics_shim;
#[macro_use] extern crate log;
#[cfg(windows)] extern crate winapi;
#[cfg(windows)] extern crate kernel32;
extern crate kailua_env;

pub use message::{Localize, Localized, get_message_language};
pub use report::{Kind, Stop, Result, Report, ReportMore, Reporter};
pub use report::{ConsoleReport, CollectedReport, NoReport, TrackMaxKind};

mod message;
mod report;
mod dummy_term;

