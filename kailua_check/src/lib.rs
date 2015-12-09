extern crate kailua_syntax;

pub use ty::{Ty, T, Builtin};
pub use check::{TyInfo, Options, Env};

mod ty;
mod check;

