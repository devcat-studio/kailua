//! This crate provides the basic environments for Kailua.
//!
//! * Location types ([`Unit`](./struct.Unit.html), [`Pos`](./struct.Pos.html),
//!   [`Span`](./struct.Span.html)) and a location-bundled container
//!   ([`Spanned`](./struct.Spanned.html))
//!
//! * Scope identifiers and a location-to-scope map
//!   ([`kailua_env::scope`](./scope/index.html))
//!
//! * The resolver for locations
//!   ([`kailua_env::source`](./source/index.html))
//!
//! * An arbitrary mapping from location ranges to values
//!   ([`kailua_env::spanmap`](./spanmap/index.html))

mod loc;
pub mod scope;
pub mod source;
pub mod spanmap;

pub use loc::{Unit, Pos, Span, Spanned, WithLoc};
pub use scope::{Scope, ScopedId, ScopeMap};
pub use source::{Source, SourceFile, SourceSlice, SourceData};
pub use spanmap::SpanMap;

