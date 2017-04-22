#[macro_use] extern crate parse_generics_shim;
extern crate kailua_env;
#[macro_use] extern crate kailua_diag;
extern crate kailua_syntax;
#[macro_use] extern crate bitflags;
#[macro_use] extern crate log;
extern crate vec_map;
extern crate take_mut;
extern crate atomic;
extern crate parking_lot;

mod l10nutils; // used by diag and message but should not be public
pub mod diag;
mod message;
pub mod ty;
pub mod env;

