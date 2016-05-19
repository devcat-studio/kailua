extern crate term;
extern crate unicode_width;

pub use source::{Pos, Span, Spanned, WithLoc};
pub use source::{Source, SourceFile, SourceBytes, SourceLineSpans};
pub use report::{Kind, Stop, Result, Report, ReportMore, Reporter};
pub use report::{ConsoleReport, CollectedReport};

mod source;
mod report;
mod dummy_term;

