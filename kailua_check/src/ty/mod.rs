use std::fmt;
use diag::CheckResult;

pub use self::literals::{Numbers, Strings};
pub use self::tables::{Key, Tables};
pub use self::functions::{Function, Functions};
pub use self::union::Union;
pub use self::value::{T, Ty};
pub use self::slot::{S, Slot};
pub use self::with_nil::{TyWithNil, SlotWithNil};
pub use self::seq::{TySeq, TySeqIter, SlotSeq, SlotSeqIter};
pub use self::builtin::Builtin;

mod literals;
mod tables;
mod functions;
mod union;
mod value;
mod slot;
mod with_nil;
mod seq;
mod builtin;

fn error_not_bottom<T: fmt::Debug>(t: T) -> CheckResult<()> {
    Err(format!("impossible constraint requested: {:?} is bottom", t))
}

fn error_not_sub<T: fmt::Debug, U: fmt::Debug>(t: T, u: U) -> CheckResult<()> {
    Err(format!("impossible constraint requested: {:?} <: {:?}", t, u))
}

fn error_not_eq<T: fmt::Debug, U: fmt::Debug>(t: T, u: U) -> CheckResult<()> {
    Err(format!("impossible constraint requested: {:?} = {:?}", t, u))
}

// anonymous, unifiable type variables
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct TVar(pub u32);

impl fmt::Debug for TVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<#{}>", self.0)
    }
}

// boolean marks for slot tracking
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Mark(pub u32);

impl Mark {
    pub fn any() -> Mark { Mark(0xffffffff) }

    pub fn assert_true(&self, ctx: &mut TypeContext) -> CheckResult<()> {
        ctx.assert_mark_true(*self)
    }
    pub fn assert_false(&self, ctx: &mut TypeContext) -> CheckResult<()> {
        ctx.assert_mark_false(*self)
    }
    pub fn assert_eq(&self, other: Mark, ctx: &mut TypeContext) -> CheckResult<()> {
        ctx.assert_mark_eq(*self, other)
    }
    pub fn assert_imply(&self, other: Mark, ctx: &mut TypeContext) -> CheckResult<()> {
        ctx.assert_mark_imply(*self, other)
    }
    pub fn assert_require_eq(&self, base: &T, ty: &T, ctx: &mut TypeContext) -> CheckResult<()> {
        ctx.assert_mark_require_eq(*self, base, ty)
    }
    pub fn assert_require_sup(&self, base: &T, ty: &T, ctx: &mut TypeContext) -> CheckResult<()> {
        ctx.assert_mark_require_sup(*self, base, ty)
    }
}

impl fmt::Debug for Mark {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if *self == Mark::any() {
            write!(f, "<mark #?>")
        } else {
            write!(f, "<mark #{}>", self.0)
        }
    }
}

pub trait TypeContext {
    fn last_tvar(&self) -> Option<TVar>;
    fn gen_tvar(&mut self) -> TVar;
    fn assert_tvar_sub(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()>;
    fn assert_tvar_sup(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()>;
    fn assert_tvar_eq(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()>;
    fn assert_tvar_sub_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()>;
    fn assert_tvar_eq_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()>;

    fn gen_mark(&mut self) -> Mark;
    fn assert_mark_true(&mut self, mark: Mark) -> CheckResult<()>;
    fn assert_mark_false(&mut self, mark: Mark) -> CheckResult<()>;
    fn assert_mark_eq(&mut self, lhs: Mark, rhs: Mark) -> CheckResult<()>;
    fn assert_mark_imply(&mut self, lhs: Mark, rhs: Mark) -> CheckResult<()>;
    // base should be identical over the subsequent method calls for same mark
    fn assert_mark_require_eq(&mut self, mark: Mark, base: &T, ty: &T) -> CheckResult<()>;
    fn assert_mark_require_sup(&mut self, mark: Mark, base: &T, ty: &T) -> CheckResult<()>;
}

pub trait Lattice<Other = Self> {
    type Output;
    fn normalize(self) -> Self::Output;
    fn union(&self, other: &Other, ctx: &mut TypeContext) -> Self::Output;
    fn assert_sub(&self, other: &Other, ctx: &mut TypeContext) -> CheckResult<()>;
    fn assert_eq(&self, other: &Other, ctx: &mut TypeContext) -> CheckResult<()>;
}

impl<T: Lattice<Output=Option<T>> + fmt::Debug + Clone> Lattice for Option<T> {
    type Output = Option<T>;

    fn normalize(self) -> Option<T> {
        self.and_then(Lattice::normalize)
    }

    fn union(&self, other: &Option<T>, ctx: &mut TypeContext) -> Option<T> {
        match (self, other) {
            (&Some(ref a), &Some(ref b)) => a.union(b, ctx),
            (&Some(ref a), &None) => Some(a.clone()),
            (&None, &Some(ref b)) => Some(b.clone()),
            (&None, &None) => None,
        }
    }

    fn assert_sub(&self, other: &Option<T>, ctx: &mut TypeContext) -> CheckResult<()> {
        match (self, other) {
            (&Some(ref a), &Some(ref b)) => a.assert_sub(b, ctx),
            (&Some(ref a), &None) => error_not_bottom(a),
            (&None, _) => Ok(())
        }
    }

    fn assert_eq(&self, other: &Option<T>, ctx: &mut TypeContext) -> CheckResult<()> {
        match (self, other) {
            (&Some(ref a), &Some(ref b)) => a.assert_eq(b, ctx),
            (&Some(ref a), &None) => error_not_bottom(a),
            (&None, &Some(ref b)) => error_not_bottom(b),
            (&None, &None) => Ok(())
        }
    }
}

impl<Left: Lattice<Right, Output=Output>, Right, Output> Lattice<Box<Right>> for Box<Left> {
    type Output = Box<Output>;

    fn normalize(self) -> Box<Output> {
        Box::new((*self).normalize())
    }

    fn union(&self, other: &Box<Right>, ctx: &mut TypeContext) -> Box<Output> {
        Box::new((**self).union(&**other, ctx))
    }

    fn assert_sub(&self, other: &Box<Right>, ctx: &mut TypeContext) -> CheckResult<()> {
        (**self).assert_sub(&**other, ctx)
    }

    fn assert_eq(&self, other: &Box<Right>, ctx: &mut TypeContext) -> CheckResult<()> {
        (**self).assert_eq(&**other, ctx)
    }
}

// used when operands should not have any type variables
impl TypeContext for () {
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

    fn gen_mark(&mut self) -> Mark {
        panic!("gen_mark is not supposed to be called here");
    }
    fn assert_mark_true(&mut self, mark: Mark) -> CheckResult<()> {
        panic!("assert_mark_true({:?}) is not supposed to be called here", mark);
    }
    fn assert_mark_false(&mut self, mark: Mark) -> CheckResult<()> {
        panic!("assert_mark_false({:?}) is not supposed to be called here", mark);
    }
    fn assert_mark_eq(&mut self, lhs: Mark, rhs: Mark) -> CheckResult<()> {
        panic!("assert_mark_eq({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
    fn assert_mark_imply(&mut self, lhs: Mark, rhs: Mark) -> CheckResult<()> {
        panic!("assert_mark_imply({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
    fn assert_mark_require_eq(&mut self, mark: Mark, base: &T, ty: &T) -> CheckResult<()> {
        panic!("assert_mark_require_eq({:?}, {:?}, {:?}) is not supposed to be called here",
               mark, *base, *ty);
    }
    fn assert_mark_require_sup(&mut self, mark: Mark, base: &T, ty: &T) -> CheckResult<()> {
        panic!("assert_mark_require_sup({:?}, {:?}, {:?}) is not supposed to be called here",
               mark, *base, *ty);
    }
}

// it is generally hard to determine if a <: b \/ c (i.e. a <: b OR a <: c)
// and a /\ b <: c (i.e. a <: c OR b <: c) in the presence of
// immediate variable instantiation; it requires the costly backtracking.
//
// since we don't need the full union and intersection types
// (they are mostly created manually, or sometimes via the path merger),
// we instead provide the "best effort" inference by disallowing instantiation.
fn err_on_instantiation<T, F>(ctx: &mut TypeContext, f: F) -> CheckResult<T>
        where F: FnOnce(&mut TypeContext) -> CheckResult<T> {
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

    fn union(&self, other: &Self, ctx: &mut TypeContext) -> Self {
        let u = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub_tvar(*self, u), Ok(()));
        assert_eq!(ctx.assert_tvar_sub_tvar(*other, u), Ok(()));
        u
    }

    fn assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        ctx.assert_tvar_sub_tvar(*self, *other)
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        ctx.assert_tvar_eq_tvar(*self, *other)
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
        const T_TRUTHY     = T_TRUE.bits | T_NONINTEGER.bits | T_INTEGER.bits | T_NUMBER.bits |
                             T_STRING.bits | T_TABLE.bits | T_FUNCTION.bits,
        const T_FALSY      = T_NIL.bits | T_FALSE.bits,
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

    pub fn is_truthy(&self) -> bool {
        (*self & T_TRUTHY != T_NONE) && (*self & !T_TRUTHY == T_NONE)
    }

    pub fn is_falsy(&self) -> bool {
        (*self & T_FALSY != T_NONE) && (*self & !T_FALSY == T_NONE)
    }
}

pub mod flags {
    pub use super::{T_NONE, T_DYNAMIC, T_NIL, T_TRUE, T_FALSE, T_BOOLEAN,
                    T_NONINTEGER, T_INTEGER, T_NUMBER, T_STRING, T_TABLE, T_FUNCTION,
                    T_INTEGRAL, T_NUMERIC, T_STRINGY, T_TABULAR, T_CALLABLE, T_TRUTHY, T_FALSY};
}

