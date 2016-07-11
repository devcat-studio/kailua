use std::fmt;
use std::borrow::Borrow;
use diag::CheckResult;
use kailua_diag::{self, Kind, Span, Spanned, Report, Reporter, Localize};
use kailua_syntax::Name;

pub use self::literals::{Numbers, Strings};
pub use self::tables::{Key, Tables};
pub use self::functions::{Function, Functions};
pub use self::union::Union;
pub use self::value::{T, Ty};
pub use self::slot::{F, S, Slot};
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

pub trait TypeResolver: Report {
    fn ty_from_name(&self, name: &Name) -> CheckResult<T<'static>>;
}

impl<'a, R: TypeResolver> TypeResolver for &'a R {
    fn ty_from_name(&self, name: &Name) -> CheckResult<T<'static>> { (**self).ty_from_name(name) }
}

impl<'a, R: TypeResolver> TypeResolver for &'a mut R {
    fn ty_from_name(&self, name: &Name) -> CheckResult<T<'static>> { (**self).ty_from_name(name) }
}

pub trait TypeContext: Report {
    // type variable management
    fn last_tvar(&self) -> Option<TVar>;
    fn gen_tvar(&mut self) -> TVar;
    fn assert_tvar_sub(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()>;
    fn assert_tvar_sup(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()>;
    fn assert_tvar_eq(&mut self, lhs: TVar, rhs: &T) -> CheckResult<()>;
    fn assert_tvar_sub_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()>;
    fn assert_tvar_eq_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()>;
    fn get_tvar_bounds(&self, tvar: TVar) -> (flags::Flags /*lb*/, flags::Flags /*ub*/);
    fn get_tvar_exact_type(&self, tvar: TVar) -> Option<T<'static>>;

    // flexibility mark management
    fn gen_mark(&mut self) -> Mark;
    fn assert_mark_true(&mut self, mark: Mark) -> CheckResult<()>;
    fn assert_mark_false(&mut self, mark: Mark) -> CheckResult<()>;
    fn assert_mark_eq(&mut self, lhs: Mark, rhs: Mark) -> CheckResult<()>;
    fn assert_mark_imply(&mut self, lhs: Mark, rhs: Mark) -> CheckResult<()>;
    // base should be identical over the subsequent method calls for same mark
    fn assert_mark_require_eq(&mut self, mark: Mark, base: &T, ty: &T) -> CheckResult<()>;
    fn assert_mark_require_sup(&mut self, mark: Mark, base: &T, ty: &T) -> CheckResult<()>;
}

impl<'a> Report for &'a mut TypeContext {
    fn add_span(&self, kind: Kind, span: Span, msg: &Localize) -> kailua_diag::Result<()> {
        (**self).add_span(kind, span, msg)
    }
    fn can_continue(&self) -> bool {
        (**self).can_continue()
    }
}

pub trait Lattice<Other = Self> {
    type Output;

    fn do_union(&self, other: &Other, ctx: &mut TypeContext) -> Self::Output;
    fn do_assert_sub(&self, other: &Other, ctx: &mut TypeContext) -> CheckResult<()>;
    fn do_assert_eq(&self, other: &Other, ctx: &mut TypeContext) -> CheckResult<()>;

    // convenience methods
    fn union<T: Borrow<Other>>(&self, other: &T, ctx: &mut TypeContext) -> Self::Output {
        self.do_union(other.borrow(), ctx)
    }
    fn assert_sub<T: Borrow<Other>>(&self, other: &T, ctx: &mut TypeContext) -> CheckResult<()> {
        self.do_assert_sub(other.borrow(), ctx)
    }
    fn assert_eq<T: Borrow<Other>>(&self, other: &T, ctx: &mut TypeContext) -> CheckResult<()> {
        self.do_assert_eq(other.borrow(), ctx)
    }
}

impl<T: Lattice<Output=T> + fmt::Debug + Clone> Lattice for Option<T> {
    type Output = Option<T>;

    fn do_union(&self, other: &Option<T>, ctx: &mut TypeContext) -> Option<T> {
        match (self, other) {
            (&Some(ref a), &Some(ref b)) => Some(a.union(b, ctx)),
            (&Some(ref a), &None) => Some(a.clone()),
            (&None, &Some(ref b)) => Some(b.clone()),
            (&None, &None) => None,
        }
    }

    fn do_assert_sub(&self, other: &Option<T>, ctx: &mut TypeContext) -> CheckResult<()> {
        match (self, other) {
            (&Some(ref a), &Some(ref b)) => a.assert_sub(b, ctx),
            (&Some(ref a), &None) => error_not_bottom(a),
            (&None, _) => Ok(())
        }
    }

    fn do_assert_eq(&self, other: &Option<T>, ctx: &mut TypeContext) -> CheckResult<()> {
        match (self, other) {
            (&Some(ref a), &Some(ref b)) => a.assert_eq(b, ctx),
            (&Some(ref a), &None) => error_not_bottom(a),
            (&None, &Some(ref b)) => error_not_bottom(b),
            (&None, &None) => Ok(())
        }
    }
}

impl<A: Display, B: Display> Lattice<Spanned<B>> for Spanned<A>
        where A: Lattice<B> {
    type Output = <A as Lattice<B>>::Output;

    fn do_union(&self, other: &Spanned<B>, ctx: &mut TypeContext) -> Self::Output {
        self.base.do_union(&other.base, ctx)
    }

    fn do_assert_sub(&self, other: &Spanned<B>, ctx: &mut TypeContext) -> CheckResult<()> {
        if let Err(e) = self.base.assert_sub(&other.base, ctx) {
            try!(ctx.error(self.span, format!("`{}` is not a subtype of `{}`",
                                              self.display(ctx), other.display(ctx)))
                    .note_if(other.span, "The right hand side originates here")
                    .done());
            Err(e) // XXX not sure if we can recover here
        } else {
            Ok(())
        }
    }

    fn do_assert_eq(&self, other: &Spanned<B>, ctx: &mut TypeContext) -> CheckResult<()> {
        if let Err(e) = self.base.assert_eq(&other.base, ctx) {
            try!(ctx.error(self.span, format!("`{}` does not equal to `{}`",
                                              self.display(ctx), other.display(ctx)))
                    .note_if(other.span, "The right hand side originates here")
                    .done());
            Err(e) // XXX not sure if we can recover here
        } else {
            Ok(())
        }
    }
}

// used when operands should not have any type variables
struct NoTypeContext;

impl Report for NoTypeContext {
    fn add_span(&self, _kind: Kind, _span: Span, _msg: &Localize) -> kailua_diag::Result<()> {
        Ok(()) // ignore any report
    }
    fn can_continue(&self) -> bool {
        panic!("can_continue() is not supposed to be called here");
    }
}

impl TypeContext for NoTypeContext {
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
    fn get_tvar_bounds(&self, tvar: TVar) -> (flags::Flags /*lb*/, flags::Flags /*ub*/) {
        panic!("get_tvar_bounds({:?}) is not supposed to be called here", tvar);
    }
    fn get_tvar_exact_type(&self, tvar: TVar) -> Option<T<'static>> {
        panic!("get_tvar_exact_type({:?}) is not supposed to be called here", tvar);
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

    fn do_union(&self, other: &Self, ctx: &mut TypeContext) -> Self {
        let u = ctx.gen_tvar();
        assert_eq!(ctx.assert_tvar_sub_tvar(*self, u), Ok(()));
        assert_eq!(ctx.assert_tvar_sub_tvar(*other, u), Ok(()));
        u
    }

    fn do_assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        ctx.assert_tvar_sub_tvar(*self, *other)
    }

    fn do_assert_eq(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        ctx.assert_tvar_eq_tvar(*self, *other)
    }
}

// human-readable description of various types requiring the type context.
// expected to implement fmt::Display.
pub struct Displayed<'b, 'c, T: Display + 'b> {
    base: &'b T,
    ctx: &'c TypeContext,
}

pub trait Display: Sized {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, ctx: &TypeContext) -> fmt::Result;

    fn display<'b, 'c>(&'b self, ctx: &'c TypeContext) -> Displayed<'b, 'c, Self> {
        Displayed { base: self, ctx: ctx }
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, ctx: &TypeContext) -> fmt::Result {
        self.base.fmt_displayed(f, ctx)
    }
}

impl<'b, 'c, T: Display + 'b> fmt::Display for Displayed<'b, 'c, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.base.fmt_displayed(f, self.ctx)
    }
}

pub mod flags {
    bitflags! {
        pub flags Flags: u16 {
            const T_NONE       = 0b000_0000_0000,
            const T_DYNAMIC    = 0b000_0000_0001,
            const T_NIL        = 0b000_0000_0010,
            const T_TRUE       = 0b000_0000_0100,
            const T_FALSE      = 0b000_0000_1000,
            const T_BOOLEAN    = 0b000_0000_1100,
            const T_NONINTEGER = 0b000_0001_0000,
            const T_INTEGER    = 0b000_0010_0000,
            const T_NUMBER     = 0b000_0011_0000,
            const T_STRING     = 0b000_0100_0000,
            const T_TABLE      = 0b000_1000_0000,
            const T_FUNCTION   = 0b001_0000_0000,
            const T_THREAD     = 0b010_0000_0000,
            const T_USERDATA   = 0b100_0000_0000,
            const T_ALL        = 0b111_1111_1110,

            const T_INTEGRAL   = T_DYNAMIC.bits | T_INTEGER.bits,
            // strings can be also used in place of numbers in Lua but omitted here
            const T_NUMERIC    = T_DYNAMIC.bits | T_NUMBER.bits,
            const T_STRINGY    = T_DYNAMIC.bits | T_NUMBER.bits | T_STRING.bits,
            const T_TABULAR    = T_DYNAMIC.bits | T_STRING.bits | T_TABLE.bits,
            // "default" types that metatables are set or can be set
            // XXX shouldn't this be customizable?
            const T_CALLABLE   = T_DYNAMIC.bits | T_FUNCTION.bits,
            const T_FALSY      = T_NIL.bits | T_FALSE.bits,
            const T_TRUTHY     = T_ALL.bits ^ T_FALSY.bits,
        }
    }

    impl Flags {
        pub fn is_dynamic(&self) -> bool { self.contains(T_DYNAMIC) }

        pub fn is_integral(&self) -> bool {
            self.is_dynamic() || (self.intersects(T_INTEGRAL) && !self.intersects(!T_INTEGRAL))
        }

        pub fn is_numeric(&self) -> bool {
            self.is_dynamic() || (self.intersects(T_NUMERIC) && !self.intersects(!T_NUMERIC))
        }

        pub fn is_stringy(&self) -> bool {
            self.is_dynamic() || (self.intersects(T_STRINGY) && !self.intersects(!T_STRINGY))
        }

        pub fn is_tabular(&self) -> bool {
            self.is_dynamic() || (self.intersects(T_TABULAR) && !self.intersects(!T_TABULAR))
        }

        pub fn is_callable(&self) -> bool {
            self.is_dynamic() || (self.intersects(T_CALLABLE) && !self.intersects(!T_CALLABLE))
        }

        pub fn is_truthy(&self) -> bool {
            self.intersects(T_TRUTHY) && !self.intersects(!T_TRUTHY)
        }

        pub fn is_falsy(&self) -> bool {
            self.intersects(T_FALSY) && !self.intersects(!T_FALSY)
        }
    }

    bitflags! {
        pub flags SimpleUnion: u16 {
            const U_NONE     = T_NONE.bits,
            const U_NIL      = T_NIL.bits,
            const U_TRUE     = T_TRUE.bits,
            const U_FALSE    = T_FALSE.bits,
            const U_BOOLEAN  = T_BOOLEAN.bits,
            const U_THREAD   = T_THREAD.bits,
            const U_USERDATA = T_USERDATA.bits,
        }
    }
}

