use std::fmt;
use diag::CheckResult;

pub use self::literals::{Numbers, Strings};
pub use self::tables::Tables;
pub use self::functions::{Function, Functions};
pub use self::union::Union;
pub use self::value::{T, Ty};

mod literals;
mod tables;
mod functions;
mod union;
mod value;

fn error_not_bottom<T: fmt::Debug>(t: T) -> CheckResult<()> {
    Err(format!("impossible constraint requested: {:?} is bottom", t))
}

fn error_not_sub<T: fmt::Debug, U: fmt::Debug>(t: T, u: U) -> CheckResult<()> {
    Err(format!("impossible constraint requested: {:?} <: {:?}", t, u))
}

fn error_not_eq<T: fmt::Debug, U: fmt::Debug>(t: T, u: U) -> CheckResult<()> {
    Err(format!("impossible constraint requested: {:?} = {:?}", t, u))
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct TVar(pub u32);

impl fmt::Debug for TVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<#{}>", self.0)
    }
}

pub trait TVarContext {
    fn last_tvar(&self) -> Option<TVar>;
    fn gen_tvar(&mut self) -> TVar;
    fn assert_tvar_sub(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()>;
    fn assert_tvar_sup(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()>;
    fn assert_tvar_eq(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()>;
    fn assert_tvar_sub_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()>;
    fn assert_tvar_eq_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()>;
}

pub trait Lattice<Other = Self> {
    type Output;
    fn normalize(self) -> Self::Output;
    fn union(self, other: Other, ctx: &mut TVarContext) -> Self::Output;
    fn intersect(self, other: Other, ctx: &mut TVarContext) -> Self::Output;
    fn assert_sub(&self, other: &Other, ctx: &mut TVarContext) -> CheckResult<()>;
    fn assert_eq(&self, other: &Other, ctx: &mut TVarContext) -> CheckResult<()>;
}

impl<T: Lattice<Output=Option<T>> + fmt::Debug> Lattice for Option<T> {
    type Output = Option<T>;

    fn normalize(self) -> Option<T> {
        self.and_then(Lattice::normalize)
    }

    fn union(self, other: Option<T>, ctx: &mut TVarContext) -> Option<T> {
        match (self, other) {
            (Some(a), Some(b)) => a.union(b, ctx),
            (Some(a), None) => Some(a),
            (None, Some(b)) => Some(b),
            (None, None) => None,
        }
    }

    fn intersect(self, other: Option<T>, ctx: &mut TVarContext) -> Option<T> {
        match (self, other) {
            (Some(a), Some(b)) => a.intersect(b, ctx),
            (_, _) => None,
        }
    }

    fn assert_sub(&self, other: &Option<T>, ctx: &mut TVarContext) -> CheckResult<()> {
        match (self, other) {
            (&Some(ref a), &Some(ref b)) => a.assert_sub(b, ctx),
            (&Some(ref a), &None) => error_not_bottom(a),
            (&None, _) => Ok(())
        }
    }

    fn assert_eq(&self, other: &Option<T>, ctx: &mut TVarContext) -> CheckResult<()> {
        match (self, other) {
            (&Some(ref a), &Some(ref b)) => a.assert_eq(b, ctx),
            (&Some(ref a), &None) => error_not_bottom(a),
            (&None, &Some(ref b)) => error_not_bottom(b),
            (&None, &None) => Ok(())
        }
    }
}

// used when operands should not have any type variables
impl TVarContext for () {
    fn last_tvar(&self) -> Option<TVar> {
        None
    }
    fn gen_tvar(&mut self) -> TVar {
        panic!("gen_tvar is not supposed to be called here");
    }
    fn assert_tvar_sub(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()> {
        panic!("assert_tvar_sub({:?}, {:?}) is not supposed to be called here", lhs, *rhs);
    }
    fn assert_tvar_sup(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()> {
        panic!("assert_tvar_sup({:?}, {:?}) is not supposed to be called here", lhs, *rhs);
    }
    fn assert_tvar_eq(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()> {
        panic!("assert_tvar_eq({:?}, {:?}) is not supposed to be called here", lhs, *rhs);
    }
    fn assert_tvar_sub_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()> {
        panic!("assert_tvar_sub_tvar({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
    fn assert_tvar_eq_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()> {
        panic!("assert_tvar_eq_tvar({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
}

// it is generally hard to determine if a <: b \/ c (i.e. a <: b OR a <: c)
// and a /\ b <: c (i.e. a <: c OR b <: c) in the presence of
// immediate variable instantiation; it requires the costly backtracking.
//
// since we don't need the full union and intersection types
// (they are mostly created manually, or sometimes via the path merger),
// we instead provide the "best effort" inference by disallowing instantiation.
fn err_on_instantiation<T, F>(ctx: &mut TVarContext, f: F) -> CheckResult<T>
        where F: FnOnce(&mut TVarContext) -> CheckResult<T> {
    let last = ctx.last_tvar();
    let ret = try!(f(ctx));
    if last != ctx.last_tvar() {
        Err(format!("type variable cannot be instantiated inside a union or intersection"))
    } else {
        Ok(ret)
    }
}

impl Lattice for TVar {
    type Output = TVar;

    fn normalize(self) -> Self { self }

    fn union(self, other: Self, ctx: &mut TVarContext) -> Self {
        let u = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub_tvar(self, u), Ok(()));
        assert_eq!(ctx.assert_tvar_sub_tvar(other, u), Ok(()));
        u
    }

    fn intersect(self, other: Self, ctx: &mut TVarContext) -> Self {
        let i = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub_tvar(i, self), Ok(()));
        assert_eq!(ctx.assert_tvar_sub_tvar(i, other), Ok(()));
        i
    }

    fn assert_sub(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        ctx.assert_tvar_sub_tvar(*self, *other)
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        ctx.assert_tvar_eq_tvar(*self, *other)
    }
}

#[derive(Clone, PartialEq)]
pub struct Seq<T> {
    pub head: Vec<T>,
    pub tail: Option<T>,
}

impl<T> Seq<T> {
    pub fn new() -> Seq<T> {
        Seq { head: Vec::new(), tail: None }
    }

    pub fn from(t: T) -> Seq<T> {
        Seq { head: vec![t], tail: None }
    }
}

impl<T: Lattice + fmt::Debug> Seq<T> {
    fn assert_sub(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        let mut selfhead = self.head.iter();
        let mut otherhead = other.head.iter();
        let selftail = self.tail.as_ref();
        let othertail = other.tail.as_ref();

        loop {
            match (selfhead.next(), otherhead.next(), selftail, othertail) {
                (Some(a), Some(b), _, _) => try!(a.assert_sub(b, ctx)),
                (Some(a), None, _, Some(b)) => try!(a.assert_sub(b, ctx)),
                (Some(a), None, _, None) => return error_not_bottom(a),
                (None, Some(b), Some(a), _) => try!(a.assert_sub(b, ctx)),
                (None, Some(_), None, _) => {},

                // terminal conditions
                (None, None, Some(a), Some(b)) => return a.assert_sub(b, ctx),
                (None, None, Some(a), None) => return error_not_bottom(a),
                (None, None, None, _) => return Ok(()),
            }
        }
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TVarContext) -> CheckResult<()> {
        let mut selfhead = self.head.iter();
        let mut otherhead = other.head.iter();
        let selftail = self.tail.as_ref();
        let othertail = other.tail.as_ref();

        loop {
            match (selfhead.next(), otherhead.next(), selftail, othertail) {
                (Some(a), Some(b), _, _) => try!(a.assert_eq(b, ctx)),
                (Some(a), None, _, Some(b)) => try!(a.assert_eq(b, ctx)),
                (Some(a), None, _, None) => return error_not_bottom(a),
                (None, Some(b), Some(a), _) => try!(a.assert_eq(b, ctx)),
                (None, Some(b), None, _) => return error_not_bottom(b),

                // terminal conditions
                (None, None, Some(a), Some(b)) => return a.assert_eq(b, ctx),
                (None, None, Some(a), None) => return error_not_bottom(a),
                (None, None, None, Some(b)) => return error_not_bottom(b),
                (None, None, None, None) => return Ok(()),
            }
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Seq<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "("));
        let mut first = true;
        for t in &self.head {
            if first { first = false; } else { try!(write!(f, ", ")); }
            try!(write!(f, "{:?}", t));
        }
        if let Some(ref t) = self.tail {
            if !first { try!(write!(f, ", ")); }
            try!(write!(f, ", {:?}...", t));
        }
        write!(f, ")")
    }
}

bitflags! {
    flags Flags: u16 {
        const T_NONE       = 0b0_0000_0000,
        const T_DYNAMIC    = 0b0_0000_0001,
        const T_NIL        = 0b0_0000_0010,
        const T_TRUE       = 0b0_0000_0100,
        const T_FALSE      = 0b0_0000_1000,
        const T_BOOLEAN    = 0b0_0000_1100,
        const T_NONINTEGER = 0b0_0001_0000,
        const T_INTEGER    = 0b0_0010_0000,
        const T_NUMBER     = 0b0_0011_0000,
        const T_STRING     = 0b0_0100_0000,
        const T_TABLE      = 0b0_1000_0000,
        const T_FUNCTION   = 0b1_0000_0000,

        const T_INTEGRAL   = T_DYNAMIC.bits | T_INTEGER.bits,
        // strings can be also used in place of numbers in Lua but omitted here
        const T_NUMERIC    = T_DYNAMIC.bits | T_NUMBER.bits,
        const T_STRINGY    = T_DYNAMIC.bits | T_NUMBER.bits | T_STRING.bits,
        const T_TABULAR    = T_DYNAMIC.bits | T_STRING.bits | T_TABLE.bits,
        // "default" types that metatables are set or can be set
        // XXX shouldn't this be customizable?
        const T_CALLABLE   = T_DYNAMIC.bits | T_FUNCTION.bits,
    }
}

impl Flags {
    pub fn is_dynamic(&self) -> bool { *self & T_DYNAMIC != T_NONE }

    pub fn is_integral(&self) -> bool {
        (*self & T_DYNAMIC != T_NONE) || ((*self & T_INTEGRAL != T_NONE) &&
                                          (*self & !T_INTEGRAL == T_NONE))
    }

    pub fn is_numeric(&self) -> bool {
        (*self & T_DYNAMIC != T_NONE) || ((*self & T_NUMERIC != T_NONE) &&
                                          (*self & !T_NUMERIC == T_NONE))
    }

    pub fn is_stringy(&self) -> bool {
        (*self & T_DYNAMIC != T_NONE) || ((*self & T_STRINGY != T_NONE) &&
                                          (*self & !T_STRINGY == T_NONE))
    }

    pub fn is_tabular(&self) -> bool {
        (*self & T_DYNAMIC != T_NONE) || ((*self & T_TABULAR != T_NONE) &&
                                          (*self & !T_TABULAR == T_NONE))
    }

    pub fn is_callable(&self) -> bool {
        (*self & T_DYNAMIC != T_NONE) || ((*self & T_CALLABLE != T_NONE) &&
                                          (*self & !T_CALLABLE == T_NONE))
    }
}

pub mod flags {
    pub use super::{T_NONE, T_DYNAMIC, T_NIL, T_TRUE, T_FALSE, T_BOOLEAN,
                    T_NONINTEGER, T_INTEGER, T_NUMBER, T_STRING, T_TABLE, T_FUNCTION,
                    T_INTEGRAL, T_NUMERIC, T_STRINGY, T_TABULAR, T_CALLABLE};
}

#[cfg(test)] 
mod tests {
    use kailua_syntax::Str;
    use super::*;

    macro_rules! hash {
        ($($k:ident = $v:expr),*) => (vec![$((s(stringify!($k)), $v)),*])
    }

    fn b<T>(x: T) -> Box<T> { Box::new(x) }
    fn s(x: &str) -> Str { Str::from(x.as_bytes().to_owned()) }

    #[test]
    fn test_lattice() {
        macro_rules! check {
            ($l:expr, $r:expr; $u:expr, $i:expr) => ({
                let left = $l;
                let right = $r;
                let union = $u;
                let intersect = $i;
                let actualunion = left.clone().union(right.clone(), &mut ());
                if actualunion != union {
                    panic!("{:?} | {:?} = expected {:?}, actual {:?}",
                           left, right, union, actualunion);
                }
                let actualintersect = left.clone().intersect(right.clone(), &mut ());
                if actualintersect != intersect {
                    panic!("{:?} & {:?} = expected {:?}, actual {:?}",
                           left, right, intersect, actualintersect);
                }
            })
        }

        // dynamic vs. everything else
        check!(T::Dynamic, T::Dynamic; T::Dynamic, T::Dynamic);
        check!(T::Dynamic, T::integer(); T::Dynamic, T::integer());
        check!(T::tuple(vec![b(T::integer()), b(T::Boolean)]), T::Dynamic;
               T::Dynamic, T::tuple(vec![b(T::integer()), b(T::Boolean)]));

        // integer literals
        check!(T::integer(), T::number(); T::number(), T::integer());
        check!(T::number(), T::integer(); T::number(), T::integer());
        check!(T::number(), T::number(); T::number(), T::number());
        check!(T::integer(), T::integer(); T::integer(), T::integer());
        check!(T::int(3), T::int(3); T::int(3), T::int(3));
        check!(T::int(3), T::number(); T::number(), T::int(3));
        check!(T::integer(), T::int(3); T::integer(), T::int(3));
        check!(T::int(3), T::int(4); T::ints(vec![3, 4]), T::None);
        check!(T::ints(vec![3, 4]), T::int(3);
               T::ints(vec![3, 4]), T::int(3));
        check!(T::int(5), T::ints(vec![3, 4]);
               T::ints(vec![3, 4, 5]), T::None);
        check!(T::ints(vec![3, 4]), T::ints(vec![5, 4, 7]);
               T::ints(vec![3, 4, 5, 7]), T::int(4));
        check!(T::ints(vec![3, 4, 5]), T::ints(vec![2, 3, 4]);
               T::ints(vec![2, 3, 4, 5]), T::ints(vec![3, 4]));

        // string literals
        check!(T::string(), T::str(s("hello")); T::string(), T::str(s("hello")));
        check!(T::str(s("hello")), T::string(); T::string(), T::str(s("hello")));
        check!(T::str(s("hello")), T::str(s("hello"));
               T::str(s("hello")), T::str(s("hello")));
        check!(T::str(s("hello")), T::str(s("goodbye"));
               T::strs(vec![s("hello"), s("goodbye")]), T::None);
        check!(T::str(s("hello")), T::strs(vec![s("goodbye")]);
               T::strs(vec![s("hello"), s("goodbye")]), T::None);
        check!(T::strs(vec![s("hello"), s("goodbye")]), T::str(s("goodbye"));
               T::strs(vec![s("hello"), s("goodbye")]), T::str(s("goodbye")));
        check!(T::strs(vec![s("hello"), s("goodbye")]),
               T::strs(vec![s("what"), s("goodbye")]);
               T::strs(vec![s("hello"), s("goodbye"), s("what")]),
               T::str(s("goodbye")));
        check!(T::strs(vec![s("a"), s("b"), s("c")]),
               T::strs(vec![s("b"), s("c"), s("d")]);
               T::strs(vec![s("a"), s("b"), s("c"), s("d")]),
               T::strs(vec![s("b"), s("c")]));

        // tables
        check!(T::table(), T::array(b(T::integer())); T::table(), T::array(b(T::integer())));
        check!(T::array(b(T::integer())), T::array(b(T::integer()));
               T::array(b(T::integer())), T::array(b(T::integer())));
        check!(T::array(b(T::int(3))), T::array(b(T::int(4)));
               T::array(b(T::ints(vec![3, 4]))), T::array(b(T::None)));
        check!(T::tuple(vec![b(T::integer()), b(T::string())]),
               T::tuple(vec![b(T::number()), b(T::Dynamic), b(T::Boolean)]);
               T::tuple(vec![b(T::number()), b(T::Dynamic), b(T::Boolean | T::Nil)]),
               T::tuple(vec![b(T::integer()), b(T::string())]));
        check!(T::tuple(vec![b(T::integer()), b(T::string())]),
               T::tuple(vec![b(T::number()), b(T::Boolean), b(T::Dynamic)]);
               T::tuple(vec![b(T::number()), b(T::string() | T::Boolean), b(T::Dynamic)]),
               T::empty_table()); // boolean & string = _|_, so no way to reconcile
        check!(T::record(hash![foo=b(T::integer()), bar=b(T::string())]),
               T::record(hash![quux=b(T::Boolean)]);
               T::record(hash![foo=b(T::integer()), bar=b(T::string()), quux=b(T::Boolean)]),
               T::empty_table());
        check!(T::record(hash![foo=b(T::int(3)), bar=b(T::string())]),
               T::record(hash![foo=b(T::int(4))]);
               T::record(hash![foo=b(T::ints(vec![3, 4])), bar=b(T::string())]),
               T::empty_table());
        check!(T::record(hash![foo=b(T::integer()), bar=b(T::number()),
                                    quux=b(T::array(b(T::Dynamic)))]),
               T::record(hash![foo=b(T::number()), bar=b(T::string()),
                                    quux=b(T::array(b(T::Boolean)))]);
               T::record(hash![foo=b(T::number()), bar=b(T::number() | T::string()),
                                    quux=b(T::array(b(T::Dynamic)))]),
               T::record(hash![foo=b(T::integer()),
                                    quux=b(T::array(b(T::Boolean)))]));
        check!(T::record(hash![foo=b(T::int(3)), bar=b(T::number())]),
               T::map(b(T::string()), b(T::integer()));
               T::map(b(T::string()), b(T::number())),
               T::record(hash![foo=b(T::int(3)), bar=b(T::integer())]));
        check!(T::map(b(T::str(s("wat"))), b(T::integer())),
               T::map(b(T::string()), b(T::int(42)));
               T::map(b(T::string()), b(T::integer())),
               T::map(b(T::str(s("wat"))), b(T::int(42))));
        check!(T::array(b(T::number())), T::map(b(T::Dynamic), b(T::integer()));
               T::map(b(T::Dynamic), b(T::number())), T::array(b(T::integer())));
        check!(T::empty_table(), T::array(b(T::integer()));
               T::array(b(T::integer())), T::empty_table());

        // general unions
        check!(T::True, T::False; T::Boolean, T::None);
        check!(T::int(3) | T::Nil, T::int(4) | T::Nil;
               T::ints(vec![3, 4]) | T::Nil, T::Nil);
        check!(T::ints(vec![3, 5]) | T::Nil, T::int(4) | T::string();
               T::string() | T::ints(vec![3, 4, 5]) | T::Nil, T::None);
        check!(T::int(3) | T::string(), T::str(s("wat")) | T::int(4);
               T::ints(vec![3, 4]) | T::string(), T::str(s("wat")));
        check!(T::array(b(T::integer())), T::tuple(vec![b(T::string())]);
               T::map(b(T::integer()), b(T::integer() | T::string())), T::empty_table());
        //assert_eq!(T::map(b(T::string()), b(T::integer())),
        //           T::map(b(T::string()), b(T::integer() | T::Nil)));
    }

    #[test]
    fn test_sub() {
        use env::Context;

        let mut ctx = Context::new();

        {
            let v1 = ctx.gen_tvar();
            // v1 <: integer
            assert_eq!(T::TVar(v1).assert_sub(&T::integer(), &mut ctx), Ok(()));
            // v1 <: integer
            assert_eq!(T::TVar(v1).assert_sub(&T::integer(), &mut ctx), Ok(()));
            // v1 <: integer AND v1 <: string (!)
            assert!(T::TVar(v1).assert_sub(&T::string(), &mut ctx).is_err());
        }

        {
            let v1 = ctx.gen_tvar();
            let v2 = ctx.gen_tvar();
            // v1 <: v2
            assert_eq!(T::TVar(v1).assert_sub(&T::TVar(v2), &mut ctx), Ok(()));
            // v1 <: v2 <: string
            assert_eq!(T::TVar(v2).assert_sub(&T::string(), &mut ctx), Ok(()));
            // v1 <: v2 <: string AND v1 <: integer (!)
            assert!(T::TVar(v1).assert_sub(&T::integer(), &mut ctx).is_err());
        }

        {
            let v1 = ctx.gen_tvar();
            let v2 = ctx.gen_tvar();
            let t1 = T::record(hash![a=b(T::integer()), b=b(T::TVar(v1))]);
            let t2 = T::record(hash![a=b(T::TVar(v2)), b=b(T::string()), c=b(T::Boolean)]);
            // {a=integer, b=v1} <: {a=v2, b=string, c=boolean}
            assert_eq!(t1.assert_sub(&t2, &mut ctx), Ok(()));
            // ... AND v1 <: string
            assert_eq!(T::TVar(v1).assert_sub(&T::string(), &mut ctx), Ok(()));
            // ... AND v1 <: string AND v2 :> integer
            assert_eq!(T::integer().assert_sub(&T::TVar(v2), &mut ctx), Ok(()));
            // {a=integer, b=v1} = {a=v2, b=string, c=boolean} (!)
            assert!(t1.assert_eq(&t2, &mut ctx).is_err());
        }
    }
}

