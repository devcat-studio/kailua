use std::fmt;
use diag::CheckResult;
use message as m;
use kailua_env::{Span, Spanned};
use kailua_diag::{self, Kind, Report, Reporter, Localize};
use kailua_syntax::Name;

pub use self::literals::{Numbers, Strings};
pub use self::tables::{Key, Tables};
pub use self::functions::{Function, Functions};
pub use self::union::Unioned;
pub use self::value::{Dyn, Nil, T, Ty};
pub use self::slot::{F, S, Slot};
pub use self::seq::{SeqIter, TySeq, SpannedTySeq, SlotSeq, SpannedSlotSeq};
pub use self::tag::Tag;

mod literals;
mod tables;
mod functions;
mod union;
mod value;
mod slot;
mod seq;
mod tag;

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
    pub fn assert_require_eq(&self, base: &Ty, ty: &Ty, ctx: &mut TypeContext) -> CheckResult<()> {
        ctx.assert_mark_require_eq(*self, base, ty)
    }
    pub fn assert_require_sup(&self, base: &Ty, ty: &Ty, ctx: &mut TypeContext) -> CheckResult<()> {
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

// nominal type identifiers
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ClassId(pub u32);

impl fmt::Debug for ClassId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<%{}>", self.0)
    }
}

// nominal type classification
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Class {
    Prototype(ClassId),
    Instance(ClassId),
}

impl fmt::Debug for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Class::Prototype(cid) => write!(f, "<%{} prototype>", cid.0),
            Class::Instance(cid) => write!(f, "<%{}>", cid.0),
        }
    }
}

pub trait TypeResolver: Report {
    fn context(&mut self) -> &mut TypeContext;
    fn ty_from_name(&self, name: &Spanned<Name>) -> CheckResult<Ty>;
}

impl<'a, R: TypeResolver + ?Sized> TypeResolver for &'a mut R {
    fn context(&mut self) -> &mut TypeContext {
        (**self).context()
    }
    fn ty_from_name(&self, name: &Spanned<Name>) -> CheckResult<Ty> {
        (**self).ty_from_name(name)
    }
}

pub trait TypeContext: Report {
    // type variable management
    fn last_tvar(&self) -> Option<TVar>;
    fn gen_tvar(&mut self) -> TVar;
    fn assert_tvar_sub(&mut self, lhs: TVar, rhs: &Ty) -> CheckResult<()>;
    fn assert_tvar_sup(&mut self, lhs: TVar, rhs: &Ty) -> CheckResult<()>;
    fn assert_tvar_eq(&mut self, lhs: TVar, rhs: &Ty) -> CheckResult<()>;
    fn assert_tvar_sub_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()>;
    fn assert_tvar_eq_tvar(&mut self, lhs: TVar, rhs: TVar) -> CheckResult<()>;
    fn get_tvar_bounds(&self, tvar: TVar) -> (flags::Flags /*lb*/, flags::Flags /*ub*/);
    fn get_tvar_exact_type(&self, tvar: TVar) -> Option<Ty>;

    // flexibility mark management
    fn gen_mark(&mut self) -> Mark;
    fn assert_mark_true(&mut self, mark: Mark) -> CheckResult<()>;
    fn assert_mark_false(&mut self, mark: Mark) -> CheckResult<()>;
    fn assert_mark_eq(&mut self, lhs: Mark, rhs: Mark) -> CheckResult<()>;
    fn assert_mark_imply(&mut self, lhs: Mark, rhs: Mark) -> CheckResult<()>;
    // base should be identical over the subsequent method calls for same mark
    fn assert_mark_require_eq(&mut self, mark: Mark, base: &Ty, ty: &Ty) -> CheckResult<()>;
    fn assert_mark_require_sup(&mut self, mark: Mark, base: &Ty, ty: &Ty) -> CheckResult<()>;
    fn get_mark_exact(&self, mark: Mark) -> Option<bool>;

    // nominal type management
    fn fmt_class(&self, cls: Class, f: &mut fmt::Formatter) -> fmt::Result;
    fn is_subclass_of(&self, lhs: ClassId, rhs: ClassId) -> bool;
}

pub trait Union<Other = Self> {
    type Output;

    fn union(&self, other: &Other, explicit: bool,
             ctx: &mut TypeContext) -> CheckResult<Self::Output>;
}

pub trait Lattice<Other = Self> {
    /// Asserts that `self` is a consistent subtype of `other` under the type context.
    fn assert_sub(&self, other: &Other, ctx: &mut TypeContext) -> CheckResult<()>;

    /// Asserts that `self` is a consistent type equal to `other` under the type context.
    fn assert_eq(&self, other: &Other, ctx: &mut TypeContext) -> CheckResult<()>;
}

impl<A: Union<B>, B> Union<Box<B>> for Box<A> {
    type Output = <A as Union<B>>::Output;

    fn union(&self, other: &Box<B>, explicit: bool,
             ctx: &mut TypeContext) -> CheckResult<Self::Output> {
        (**self).union(other, explicit, ctx)
    }
}

impl<A: Lattice<B>, B> Lattice<Box<B>> for Box<A> {
    fn assert_sub(&self, other: &Box<B>, ctx: &mut TypeContext) -> CheckResult<()> {
        (**self).assert_sub(other, ctx)
    }

    fn assert_eq(&self, other: &Box<B>, ctx: &mut TypeContext) -> CheckResult<()> {
        (**self).assert_eq(other, ctx)
    }
}

impl<T: Union<Output=T> + Clone> Union for Option<T> {
    type Output = Option<T>;

    fn union(&self, other: &Option<T>, explicit: bool,
             ctx: &mut TypeContext) -> CheckResult<Option<T>> {
        match (self, other) {
            (&Some(ref a), &Some(ref b)) => Ok(Some(a.union(b, explicit, ctx)?)),
            (&Some(ref a), &None) => Ok(Some(a.clone())),
            (&None, &Some(ref b)) => Ok(Some(b.clone())),
            (&None, &None) => Ok(None),
        }
    }
}

impl<T: Lattice + fmt::Debug> Lattice for Option<T> {
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

impl<A: Display, B: Display> Union<Spanned<B>> for Spanned<A> where A: Union<B> {
    type Output = <A as Union<B>>::Output;

    fn union(&self, other: &Spanned<B>, explicit: bool,
             ctx: &mut TypeContext) -> CheckResult<Self::Output> {
        let ret = self.base.union(&other.base, explicit, ctx);
        if let Err(_) = ret {
            ctx.error(self.span, m::InvalidUnionType { lhs: self.display(ctx),
                                                       rhs: other.display(ctx) })
               .note_if(other.span, m::OtherTypeOrigin {})
               .done()?;
            // XXX not sure if we can recover here
        }
        ret
    }
}

impl<A: Display, B: Display> Lattice<Spanned<B>> for Spanned<A> where A: Lattice<B> {
    fn assert_sub(&self, other: &Spanned<B>, ctx: &mut TypeContext) -> CheckResult<()> {
        let ret = self.base.assert_sub(&other.base, ctx);
        if let Err(_) = ret {
            ctx.error(self.span, m::NotSubtype { sub: self.display(ctx),
                                                 sup: other.display(ctx) })
               .note_if(other.span, m::OtherTypeOrigin {})
               .done()?;
            // XXX not sure if we can recover here
        }
        ret
    }

    fn assert_eq(&self, other: &Spanned<B>, ctx: &mut TypeContext) -> CheckResult<()> {
        let ret = self.base.assert_eq(&other.base, ctx);
        if let Err(_) = ret {
            ctx.error(self.span, m::NotEqual { lhs: self.display(ctx),
                                               rhs: other.display(ctx) })
               .note_if(other.span, m::OtherTypeOrigin {})
               .done()?;
            // XXX not sure if we can recover here
        }
        ret
    }
}

// used when operands should not have any type variables
struct NoTypeContext;

impl Report for NoTypeContext {
    fn add_span(&self, _kind: Kind, _span: Span, _msg: &Localize) -> kailua_diag::Result<()> {
        Ok(()) // ignore any report
    }
}

impl TypeContext for NoTypeContext {
    fn last_tvar(&self) -> Option<TVar> {
        None
    }
    fn gen_tvar(&mut self) -> TVar {
        panic!("gen_tvar is not supposed to be called here");
    }
    fn assert_tvar_sub(&mut self, lhs: TVar, rhs: &Ty) -> CheckResult<()> {
        panic!("assert_tvar_sub({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
    fn assert_tvar_sup(&mut self, lhs: TVar, rhs: &Ty) -> CheckResult<()> {
        panic!("assert_tvar_sup({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
    fn assert_tvar_eq(&mut self, lhs: TVar, rhs: &Ty) -> CheckResult<()> {
        panic!("assert_tvar_eq({:?}, {:?}) is not supposed to be called here", lhs, rhs);
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
    fn get_tvar_exact_type(&self, tvar: TVar) -> Option<Ty> {
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
    fn assert_mark_require_eq(&mut self, mark: Mark, base: &Ty, ty: &Ty) -> CheckResult<()> {
        panic!("assert_mark_require_eq({:?}, {:?}, {:?}) is not supposed to be called here",
               mark, base, ty);
    }
    fn assert_mark_require_sup(&mut self, mark: Mark, base: &Ty, ty: &Ty) -> CheckResult<()> {
        panic!("assert_mark_require_sup({:?}, {:?}, {:?}) is not supposed to be called here",
               mark, base, ty);
    }
    fn get_mark_exact(&self, mark: Mark) -> Option<bool> {
        panic!("get_mark_exact({:?}) is not supposed to be called here", mark);
    }

    fn fmt_class(&self, cls: Class, _f: &mut fmt::Formatter) -> fmt::Result {
        panic!("fmt_class({:?}, ...) is not supposed to be called here", cls);
    }
    fn is_subclass_of(&self, lhs: ClassId, rhs: ClassId) -> bool {
        panic!("is_subclass_of({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
}

impl Lattice for TVar {
    fn assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        ctx.assert_tvar_sub_tvar(*self, *other)
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TypeContext) -> CheckResult<()> {
        ctx.assert_tvar_eq_tvar(*self, *other)
    }
}

// human-readable description of various types requiring the type context.
// expected to implement fmt::Display.
pub struct Displayed<'b, 'c, T: Display + 'b> {
    base: &'b T,
    ctx: &'c TypeContext,
}

pub trait Display: fmt::Debug + Sized {
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

impl<T: Display> Display for Box<T> {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, ctx: &TypeContext) -> fmt::Result {
        (**self).fmt_displayed(f, ctx)
    }
}

impl<'b, 'c, T: Display + 'b> fmt::Display for Displayed<'b, 'c, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.base.fmt_displayed(f, self.ctx)
    }
}

impl<'b, 'c, T: Display + fmt::Debug + 'b> fmt::Debug for Displayed<'b, 'c, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.base, f)
    }
}

pub mod flags {
    use ty::value::Dyn;

    bitflags! {
        pub flags Flags: u16 {
            const T_NONE       = 0b0000_0000_0000,
            const T_WHATEVER   = 0b0000_0000_0001,
            const T_DYNAMIC    = 0b0000_0000_0011,
            const T_NOISY_NIL  = 0b0000_0000_0100, // silent nil is ignored
            const T_TRUE       = 0b0000_0000_1000,
            const T_FALSE      = 0b0000_0001_0000,
            const T_BOOLEAN    = 0b0000_0001_1000,
            const T_NONINTEGER = 0b0000_0010_0000,
            const T_INTEGER    = 0b0000_0100_0000,
            const T_NUMBER     = 0b0000_0110_0000,
            const T_STRING     = 0b0000_1000_0000,
            const T_TABLE      = 0b0001_0000_0000,
            const T_FUNCTION   = 0b0010_0000_0000,
            const T_THREAD     = 0b0100_0000_0000,
            const T_USERDATA   = 0b1000_0000_0000,
            const T_ALL        = 0b1111_1111_1100,

            const T_INTEGRAL   = T_DYNAMIC.bits | T_INTEGER.bits,
            // strings can be also used in place of numbers in Lua but omitted here
            const T_NUMERIC    = T_DYNAMIC.bits | T_NUMBER.bits,
            const T_STRINGY    = T_DYNAMIC.bits | T_NUMBER.bits | T_STRING.bits,
            const T_TABULAR    = T_DYNAMIC.bits | T_STRING.bits | T_TABLE.bits,
            // "default" types that metatables are set or can be set
            // XXX shouldn't this be customizable?
            const T_CALLABLE   = T_DYNAMIC.bits | T_FUNCTION.bits,
            const T_FALSY      = T_NOISY_NIL.bits | T_FALSE.bits,
            const T_TRUTHY     = T_ALL.bits ^ T_FALSY.bits,
        }
    }

    impl Flags {
        pub fn is_dynamic(&self) -> bool { self.intersects(T_DYNAMIC) }

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

        pub fn get_dynamic(&self) -> Option<Dyn> {
            if self.contains(T_DYNAMIC) {
                Some(Dyn::Oops)
            } else if self.contains(T_WHATEVER) {
                Some(Dyn::User)
            } else {
                None
            }
        }
    }

    bitflags! {
        pub flags UnionedSimple: u16 {
            const U_NONE     = T_NONE.bits,
            const U_TRUE     = T_TRUE.bits,
            const U_FALSE    = T_FALSE.bits,
            const U_BOOLEAN  = T_BOOLEAN.bits,
            const U_THREAD   = T_THREAD.bits,
            const U_USERDATA = T_USERDATA.bits,
        }
    }
}

