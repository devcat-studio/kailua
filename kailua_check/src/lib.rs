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

pub fn check_from_chunk<R: Report>(context: &mut env::Context<R>,
                                   chunk: kailua_syntax::Chunk,
                                   opts: Rc<RefCell<options::Options>>) -> kailua_diag::Result<()> {
    let mut env = env::Env::new(context, opts, chunk.map);
    let mut checker = Checker::new(&mut env);
    checker.visit(&chunk.block)
}

