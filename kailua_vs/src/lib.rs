// ...so this is make Cargo shut up about the CamelCased dll file name.
// we don't want to pull this hack to submodules, so we override this immediately below.
#![allow(non_snake_case)]

extern crate widestring;
extern crate kailua_diag;
extern crate kailua_syntax;
extern crate kailua_check;

#[warn(non_snake_case)] pub mod util;
#[warn(non_snake_case)] pub mod source;
#[warn(non_snake_case)] pub mod report;
#[warn(non_snake_case)] pub mod lex;
#[warn(non_snake_case)] pub mod parse;
#[warn(non_snake_case)] pub mod check;

