extern crate kailua_syntax;

pub use ty::{Ty, T, Builtin};
pub use check::{TyInfo, Options, Env};

mod ty;
mod check;

#[test]
fn test_check() {
    fn check(s: &str) -> bool {
        kailua_syntax::parse_chunk(s.as_bytes()).ok().and_then(|chunk| {
            use std::collections::HashMap;
            struct Opts;
            impl Options for Opts {}
            let mut globals = HashMap::new();
            let mut opts = Opts;
            let mut env = Env::new(&mut globals, &mut opts);
            env.visit(&chunk).ok()
        }).is_some()
    }

    assert!(check("local p
                   p()"));
    assert!(!check("local c
                    if c then local p end
                    p()"));
    //assert!(!check("local c, p
    //                if c then p = 4 end
    //                p()"));
    assert!(!check("p()"));
    assert!(check("--# assume p: ?
                   p()"));
}
