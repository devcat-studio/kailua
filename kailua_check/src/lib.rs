//! Type checker for Kailua.
//!
//! Type checking involves four types:
//!
//! * `kailua_check::env::Context` is a global context for all checked files.
//!   It also exposes an interface to the type environment, `kailua_types::env::Types`.
//!
//! * `kaliua_check::env::Env` is a per-file context.
//!   The checker will also build its own `Env` for newly loaded files,
//!   but the first `Env` should be given explicitly.
//!
//! * `kailua_check::options::Options` is a configurable portion of the type checker.
//!   Currently it allows you to configure the `require` path and the actual loading process.
//!
//! * `kailua_check::Checker` is the actual checker.
//!   Due to the internal architecture, it also holds some side information
//!   depending on the input chunk (and cannot be put to `Env` due to the lifetime mismatch).
//!
//! After the type checking, `Context` can be extracted into the `Output` for later analysis.

#[macro_use] extern crate parse_generics_shim;
extern crate kailua_env;
#[macro_use] extern crate kailua_diag;
extern crate kailua_syntax;
extern crate kailua_types;
#[macro_use] extern crate log;
extern crate take_mut;

use std::cell::RefCell;
use std::rc::Rc;
use kailua_diag::Report;

pub use check::Checker;

mod message;
pub mod options;
pub mod env;
mod defs;
mod check;

/// An one-off function to check a chunk with given `Options`.
pub fn check_from_chunk<R: Report>(context: &mut env::Context<R>,
                                   chunk: kailua_syntax::Chunk,
                                   opts: Rc<RefCell<options::Options>>) -> kailua_diag::Result<()> {
    let mut env = env::Env::new(context, opts, chunk.map);
    let mut checker = Checker::new(&mut env);
    checker.visit(&chunk.block)
}

