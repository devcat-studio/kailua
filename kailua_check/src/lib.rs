extern crate kailua_syntax;
#[macro_use] extern crate bitflags;
extern crate vec_map;

pub use diag::{Error, CheckResult};
pub use ty::*;
pub use env::{TyInfo, Context};
pub use check::{Options, Checker};

mod diag;
mod ty;
mod env;
mod check;

#[test]
fn test_check() {
    fn check(s: &str) -> CheckResult<()> {
        println!("");
        println!("checking `{}`", s);
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
    assert_err!("local c = true
                 if c then local p end
                 p()");
    assert_err!("local c = false
                 if c then local p end
                 p()");
    assert_err!("local c --: boolean
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
    assert_ok!("local f
                f = 'hello?'");
    assert_ok!("local f = function() end
                f = function() return 54 end");
    assert_ok!("local f = function() end
                f = {54, 49}");
    assert_ok!("local f = function() end
                --# assume f: {index = integer}
                local p = f.index");
    assert_err!("local f = function() end
                 --# assume f: table
                 local p = f.index");
    assert_ok!("local a = ('string' and 53) + 42");
    assert_err!("local a = (53 and 'string') + 42");
    assert_err!("local a = (nil and 'string') + 42");
    assert_err!("local a = (nil and 53) + 42");
    assert_ok!("local a = (53 or 'string') + 42");
    assert_ok!("local a = (53 or nil) + 42");
    assert_err!("local a = (nil or 'string') + 42");
    assert_ok!("local a = (nil or 53) + 42");
    assert_ok!("--# assume p: string | number
                local q = p .. 3");
    assert_err!("--# assume p: string | number
                 local q = p + 3");
    assert_err!("--# assume p: string | boolean
                 local q = p .. 3");
    assert_ok!("local x
                --# assume x: var 3
                x = 3");
    assert_ok!("local x
                --# assume x: var 3 | 4
                x = 3");
    assert_err!("local x
                 --# assume x: var 4 | 5
                 x = 3");
    assert_ok!("local x, y, z
                --# assume x: var integer
                --# assume y: var integer
                --# assume z: var integer
                z = x + y
                z = x - y
                z = x * y
                z = x % y");
    assert_err!("local x, y, z
                 --# assume x: var integer
                 --# assume y: var integer
                 --# assume z: var integer
                 z = x / y");
    assert_ok!("local p
                --# assume p: var integer
                p = 3 + 4");
    assert_err!("local p
                 --# assume p: var integer
                 p = 3.1 + 4");
    assert_ok!("local p, q
                --# assume p: ?
                --# assume q: var integer
                q = p + 3");
    assert_err!("local p, q
                 --# assume p: ?
                 --# assume q: var integer
                 q = p + 3.5");
    assert_ok!("local p, q
                --# assume p: ?
                --# assume q: var number
                q = p + 3.5");
    assert_ok!("local p, q
                --# assume p: ?
                --# assume q: var number
                q = p + p");
    assert_err!("local p, q
                 --# assume p: ?
                 --# assume q: var integer
                 q = p + p");
    assert_ok!("local a = true
                a = 'string'
                --# assume a: integer
                a = a + 3.1");
    assert_ok!("local a = 3 + #{1, 2, 3}");
    assert_ok!("local a = 3 + #'heck'");
    assert_err!("local a = 3 + #4");
    assert_ok!("--# assume a: var integer
                for i = 1, 9 do a = i end");
    assert_ok!("--# assume a: var integer
                for i = 1, 9, 2 do a = i end");
    assert_err!("--# assume a: var integer
                 for i = 1.1, 9 do a = i end");
    assert_err!("--# assume a: var integer
                 for i = 1, 9, 2.1 do a = i end");
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
                 local b = a.z + 1"); // z should be nil
    assert_err!("local a = { x = 3, y = 'foo' }
                 local b = a.z .. 'bar'"); // ditto
    assert_ok!("local a = {}
                a[1] = 1
                a[2] = 2
                a[3] = 3");
    // XXX can't propagate the number constraints upwards!
    assert_ok!("function p(a) --: number
                    return a + 3
                end
                local x = p(4.5)");
    assert_err!("function p(a) return a + 3 end
                 local x = p('what')");
    assert_ok!("local a = 'string'
                a = 42
                function p() return a end
                local b = p() + 54");
    assert_ok!("local a = 'string'
                a = 42
                function p() return a end
                a = p() * 54");
    /* XXX do not yet work
    assert_err!("local a = 'string'
                 a = 42
                 function p() return a end
                 a = true"); // a is now fixed to Var
    */
    assert_ok!("function p(x) --: string --> string
                    return x
                end
                local a = p('foo') .. 'bar'");
    assert_ok!("function p(x) --: string
                    return x
                end
                local a = p('foo') .. 'bar'");
    assert_err!("function p(x) --: string --> string
                     return x
                 end
                 local a = p('foo') + 3");
    assert_ok!("local a --: {}
                      = {} -- XXX parser bug
                a[1] = 42
                a[2] = 54");
    assert_ok!("local a --: {}
                      = {} -- XXX parser bug
                a[1] = 42
                a[2] = 54");
    assert_err!("local a --: var {} -- cannot be changed!
                       = {} -- XXX parser bug
                 a[1] = 42
                 a[2] = 54");
    assert_ok!("local a --: {}
                      = {} -- XXX parser bug
                a.what = 42");
    assert_ok!("local a --: {}
                      = {} -- XXX parser bug
                a[1] = 42
                a.what = 54");
    assert_ok!("local a --: {number}
                      = {} -- XXX parser bug
                a[1] = 42
                a.what = 54"); // {number} coerced to {[integer|string] = number} in the slot
    assert_ok!("local a --: var {number}
                      = {} -- XXX parser bug");
    assert_err!("local a --: var {number}
                       = {} -- XXX parser bug
                 a[1] = 42
                 a.what = 54");
    assert_ok!("local a --: var {[number] = number}
                      = {} -- XXX parser bug
                a[1] = 42
                a[3] = 54
                a[1] = nil
                local z --: var number?
                      = a[3] -- XXX parser bug");
    assert_err!("local a --: var {[number] = number}
                       = {} -- XXX parser bug
                 a[1] = 42
                 a[3] = 54
                 a[1] = nil
                 local z --: var integer?
                       = a[3] -- XXX parser bug");
    assert_err!("local a --: var {[number] = number}
                       = {} -- XXX parser bug
                 a[1] = 42
                 a[3] = 54
                 a[1] = nil
                 local z --: var integer
                       = a[3] -- XXX parser bug");
    assert_err!("local a --: const {[number] = number}
                       = {} -- XXX parser bug
                 a[1] = 42");
}
