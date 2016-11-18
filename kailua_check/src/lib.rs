#[macro_use] extern crate parse_generics_shim;
extern crate kailua_env;
#[macro_use] extern crate kailua_diag;
extern crate kailua_syntax;
#[macro_use] extern crate bitflags;
#[macro_use] extern crate log;
extern crate vec_map;
extern crate take_mut;

use std::cell::RefCell;
use std::rc::Rc;

pub use diag::{Error, CheckResult};
pub use ty::*;
pub use options::{Options, FsOptions, FsSource};
pub use env::{Id, IdDisplay, Frame, NameDef, TypeDef, ClassDef, Scope, Context, Env};
pub use check::Checker;

mod diag;
mod message;
mod ty;
mod options;
mod env;
mod defs;
mod check;

pub fn check_from_chunk(context: &mut Context,
                        chunk: kailua_syntax::Chunk,
                        opts: Rc<RefCell<Options>>) -> CheckResult<()> {
    let mut env = env::Env::new(context, opts, chunk.map);
    let mut checker = check::Checker::new(&mut env);
    checker.visit(&chunk.block)
}

