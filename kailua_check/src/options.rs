use std::cell::RefCell;

use kailua_diag::{Source, Spanned};
use kailua_syntax::Block;
use diag::CheckResult;

pub trait Options {
    fn source(&self) -> &RefCell<Source>;

    fn require_block(&mut self, _path: &[u8]) -> CheckResult<Spanned<Block>> {
        Err("not implemented".into())
    }
}

