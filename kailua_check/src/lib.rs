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

use std::cell::RefCell;
use std::rc::Rc;
use kailua_diag::Report;

pub use diag::{TypeReport, TypeResult, TypeReportMore};
pub use ty::flags;
pub use ty::{Display, Displayed};
pub use ty::{RVar, TVar, ClassId, Class, TypeContext, TypeResolver, Union, Lattice};
pub use ty::{Numbers, Strings, Key, Tables, Function, Functions, Unioned, Dyn, Nil, T, Ty};
pub use ty::{F, S, Slot, SeqIter, TySeq, SpannedTySeq, SlotSeq, SpannedSlotSeq, Tag};
pub use options::{Options, FsOptions, FsSource};
pub use env::{Id, IdDisplay, Frame, NameDef, TypeDef, ClassDef, Scope, Context, Output, Env};
pub use check::Checker;

mod diag;
mod message;
mod ty;
mod options;
mod env;
mod defs;
mod check;

pub fn check_from_chunk<R: Report>(context: &mut Context<R>,
                                   chunk: kailua_syntax::Chunk,
                                   opts: Rc<RefCell<Options>>) -> kailua_diag::Result<()> {
    let mut env = env::Env::new(context, opts, chunk.map);
    let mut checker = check::Checker::new(&mut env);
    checker.visit(&chunk.block)
}

