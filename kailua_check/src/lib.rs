extern crate kailua_syntax;
#[macro_use] extern crate bitflags;
extern crate vec_map;

pub use diag::{Error, CheckResult};
pub use ty::{Ty, T};
pub use env::{Builtin, TyInfo, Context};
pub use check::{Options, Checker};

mod diag;
mod ty;
mod env;
mod check;

#[test]
fn test_check() {
    fn check(s: &str) -> CheckResult<()> {
        let parsed = kailua_syntax::parse_chunk(s.as_bytes());
        let chunk = try!(parsed.map_err(|s| format!("parse error: {}", s)));

        struct Opts;
        impl Options for Opts {}
        let mut context = Context::new();
        let mut opts = Opts;
        let mut checker = Checker::new(&mut context, &mut opts);
        checker.visit(&chunk)
    }

    macro_rules! assert_ok { ($e:expr) => (assert_eq!(check($e), Ok(()))) }
    macro_rules! assert_err { ($e:expr) => (assert!(check($e).is_err())) }

    assert_err!("local p
                 p()");
    assert_ok!("local function p() end
                p()");
    assert_err!("local c
                 if c then local p end
                 p()");
    //assert_err!("local c, p
    //             if c then p = 4 end
    //             p()");
    assert_err!("p()");
    assert_ok!("--# assume p: ?
                p()");
    assert_ok!("--# assume s: ?
                local p = s:find('hello')");
    assert_ok!("--# assume p: number
                local x = p + 3");
    assert_err!("--# assume p: number
                 local x = p + 'foo'");
    assert_err!("--# assume p: unknown_type");
    assert_ok!("local p = 3 + 4");
    assert_err!("local p = 3 + 'foo'");
    assert_err!("local p = true + 7");
    assert_err!("local p = ({})[3]");
    assert_ok!("local p = ({[3] = 4})[3]");
    assert_err!("local p = ({[2] = 4})[3]");
    assert_err!("local p = ({}).a");
    assert_ok!("local p = ({a = 4}).a");
    assert_err!("local p = ({a = 4}).b");
    assert_ok!("local p = ({}):hello()"); // XXX
    assert_err!("local p = (function() end)[3]"); // XXX
    //assert_ok!("local f
    //            f = 'hello?'");
    assert_ok!("local f = function() end
                f = function() return 54 end");
    assert_err!("local f = function() end
                 f = {54, 49}");
    assert_ok!("local f = function() end
                --# assume f: table
                local p = f.index");
    assert_err!("local p = 'hello' or 4
                 local q = p + 3");
    assert_ok!("local p = 'hello' or 4
                local q = p .. 3"); // since either one can be concatnated
    assert_ok!("--# assume p: string | number
                local q = p .. 3");
    assert_err!("local p = 'hello' or true
                 local q = p .. 3");
    assert_err!("--# assume p: string | boolean
                 local q = p .. 3");
    assert_ok!("local x
                --# assume x: 3
                x = 3");
    assert_ok!("local x
                --# assume x: 3 | 4
                x = 3");
    assert_err!("local x
                 --# assume x: 4 | 5
                 x = 3");
    assert_ok!("local x, y
                --# assume x: 3 | 4
                --# assume y: integer
                x = 3
                y = x");
    assert_ok!("local x, y, z
                --# assume x: integer
                --# assume y: integer
                --# assume z: integer
                z = x + y
                z = x - y
                z = x * y
                z = x % y");
    assert_err!("local x, y, z
                 --# assume x: integer
                 --# assume y: integer
                 --# assume z: integer
                 z = x / y");
    assert_ok!("function a(...)
                  return ...
                end");
    assert_err!("function a()
                   return ...
                 end");
    assert_err!("function a(...)
                   return function() return ... end
                 end");
    assert_ok!("local a = { x = 3, y = 'foo' }
                local b = a.x
                --# assume b: integer");
    assert_ok!("local a = { x = 3, y = 'foo' }
                local b = a.y
                --# assume b: string");
    assert_err!("local a = { x = 3, y = 'foo' }
                 local b = a.z");
    assert_ok!("function p(a) return a + 3 end
                local x = p(4.5)");
    assert_err!("function p(a) return a + 3 end
                 local x = p('what')");
}
