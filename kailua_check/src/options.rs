use kailua_diag::Spanned;
use kailua_syntax::Block;
use diag::CheckResult;

pub trait Options {
    fn set_package_path(&mut self, _path: &[u8]) -> CheckResult<()> { Ok(()) }
    fn set_package_cpath(&mut self, _path: &[u8]) -> CheckResult<()> { Ok(()) }

    fn require_block(&mut self, _path: &[u8]) -> CheckResult<Spanned<Block>> {
        Err("not implemented".into())
    }
}

