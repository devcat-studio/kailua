extern crate kailua_diag;
extern crate kailua_syntax;
#[macro_use] extern crate bitflags;
#[macro_use] extern crate log;
extern crate vec_map;
extern crate take_mut;

pub use diag::{Error, CheckResult};
pub use ty::*;
pub use options::Options;
pub use env::Context;
pub use check::Checker;

mod diag;
mod ty;
mod options;
mod env;
mod defs;
mod check;

pub fn check_from_span(context: &mut Context,
                       span: kailua_diag::Span,
                       opts: &mut Options) -> CheckResult<()> {
    let chunk = kailua_syntax::parse_chunk(&opts.source().borrow(), span, context.report());
    let chunk = try!(chunk.map_err(|_| format!("parse error")));
    check_from_chunk(context, &chunk, opts)
}

pub fn check_from_chunk(context: &mut Context,
                        chunk: &kailua_diag::Spanned<kailua_syntax::Block>,
                        opts: &mut Options) -> CheckResult<()> {
    let mut env = env::Env::new(context);
    let mut checker = check::Checker::new(&mut env, opts);
    checker.visit(chunk)
}

