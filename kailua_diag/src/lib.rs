//! Diagnostics and rudimentary localization support for Kailua.

extern crate term;
extern crate unicode_width;
extern crate parse_generics_shim;
#[macro_use] extern crate log;
#[cfg(windows)] extern crate winapi;
#[cfg(windows)] extern crate kernel32;
extern crate kailua_env;

pub use message::{Locale, Localize, Localized};
pub use report::{Kind, Stop, Result, Report, Reporter};
pub use report::{ConsoleReport, CollectedReport, NoReport, TrackMaxKind};

pub mod message;
pub mod report;
mod dummy_term;

