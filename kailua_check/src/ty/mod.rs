use std::fmt;
use diag::{CheckResult, TypeReport, TypeResult, Display};
use kailua_env::Spanned;
use kailua_diag::{Locale, Report};
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

// anonymous, unifiable type variables
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct TVar(pub u32);

impl fmt::Debug for TVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<#{}>", self.0)
    }
}

// row variable, where:
// - `empty` (row #0) denotes a inextensible "empty" row variable;
//   this cannot occur from unification, but is required to handle subtyping between
//   records and non-records, which effectively disables any further record extension
#[derive(Clone, PartialEq, Eq)]
pub struct RVar(u32);

impl RVar {
    pub fn new(id: usize) -> RVar { RVar(id as u32) }
    pub fn empty() -> RVar { RVar::new(0) }
    pub fn any() -> RVar { RVar::new(0xffffffff) }

    fn to_u32(&self) -> u32 {
        self.0
    }

    pub fn to_usize(&self) -> usize {
        self.to_u32() as usize
    }
}

impl fmt::Debug for RVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if *self == RVar::empty() {
            write!(f, "<empty row>")
        } else if *self == RVar::any() {
            write!(f, "<row #?>")
        } else {
            write!(f, "<row #{}>", self.to_u32())
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

// the type resolver is a superset of more constrained type context
// and has the name-to-type mapping (which is useless for the type operations themselves)
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

// every type-related operations are encapsulated into this trait
pub trait TypeContext {
    fn gen_report(&self) -> TypeReport;

    // type variable management
    fn last_tvar(&self) -> Option<TVar>;
    fn gen_tvar(&mut self) -> TVar;
    fn copy_tvar(&mut self, tvar: TVar) -> TVar;
    fn assert_tvar_sub(&mut self, lhs: TVar, rhs: &Ty) -> TypeResult<()>;
    fn assert_tvar_sup(&mut self, lhs: TVar, rhs: &Ty) -> TypeResult<()>;
    fn assert_tvar_eq(&mut self, lhs: TVar, rhs: &Ty) -> TypeResult<()>;
    fn assert_tvar_sub_tvar(&mut self, lhs: TVar, rhs: TVar) -> TypeResult<()>;
    fn assert_tvar_eq_tvar(&mut self, lhs: TVar, rhs: TVar) -> TypeResult<()>;
    fn get_tvar_bounds(&self, tvar: TVar) -> (flags::Flags /*lb*/, flags::Flags /*ub*/);
    fn get_tvar_exact_type(&self, tvar: TVar) -> Option<Ty>;

    // row variable management
    fn gen_rvar(&mut self) -> RVar;
    fn copy_rvar(&mut self, rvar: RVar) -> RVar;
    fn assert_rvar_sub(&mut self, lhs: RVar, rhs: RVar) -> TypeResult<()>;
    fn assert_rvar_eq(&mut self, lhs: RVar, rhs: RVar) -> TypeResult<()>;
    fn assert_rvar_includes(&mut self, lhs: RVar, rhs: &[(Key, Slot)]) -> TypeResult<()>;
    fn assert_rvar_closed(&mut self, rvar: RVar) -> TypeResult<()>;
    // should return RVar::any() when the last row variable is yet to be instantiated
    fn list_rvar_fields(&self, rvar: RVar,
                        f: &mut FnMut(&Key, &Slot) -> Result<(), ()>) -> Result<RVar, ()>;

    fn get_rvar_fields(&self, rvar: RVar) -> Vec<(Key, Slot)> {
        let mut fields = Vec::new();
        self.list_rvar_fields(rvar, &mut |k, v| {
            fields.push((k.clone(), v.clone()));
            Ok(())
        }).expect("list_rvar_fields exited early while we haven't break");
        fields // do not return the last rvar, to which operations are no-ops
    }

    // nominal type management
    fn fmt_class(&self, cls: Class, f: &mut fmt::Formatter) -> fmt::Result;
    fn is_subclass_of(&self, lhs: ClassId, rhs: ClassId) -> bool;
}

pub trait Union<Other = Self> {
    type Output;

    fn union(&self, other: &Other, explicit: bool,
             ctx: &mut TypeContext) -> TypeResult<Self::Output>;
}

pub trait Lattice<Other = Self> {
    /// Asserts that `self` is a consistent subtype of `other` under the type context.
    fn assert_sub(&self, other: &Other, ctx: &mut TypeContext) -> TypeResult<()>;

    /// Asserts that `self` is a consistent type equal to `other` under the type context.
    fn assert_eq(&self, other: &Other, ctx: &mut TypeContext) -> TypeResult<()>;
}

impl<A: Union<B>, B> Union<Box<B>> for Box<A> {
    type Output = <A as Union<B>>::Output;

    fn union(&self, other: &Box<B>, explicit: bool,
             ctx: &mut TypeContext) -> TypeResult<Self::Output> {
        (**self).union(other, explicit, ctx)
    }
}

impl<A: Lattice<B>, B> Lattice<Box<B>> for Box<A> {
    fn assert_sub(&self, other: &Box<B>, ctx: &mut TypeContext) -> TypeResult<()> {
        (**self).assert_sub(other, ctx)
    }

    fn assert_eq(&self, other: &Box<B>, ctx: &mut TypeContext) -> TypeResult<()> {
        (**self).assert_eq(other, ctx)
    }
}

impl<A: Display, B: Display> Union<Spanned<B>> for Spanned<A> where A: Union<B> {
    type Output = <A as Union<B>>::Output;

    fn union(&self, other: &Spanned<B>, explicit: bool,
             ctx: &mut TypeContext) -> TypeResult<Self::Output> {
        self.base.union(&other.base, explicit, ctx).map_err(|r| {
            r.cannot_union_attach_span(self.span, other.span, explicit)
        })
    }
}

impl<A: Display, B: Display> Lattice<Spanned<B>> for Spanned<A> where A: Lattice<B> {
    fn assert_sub(&self, other: &Spanned<B>, ctx: &mut TypeContext) -> TypeResult<()> {
        self.base.assert_sub(&other.base, ctx).map_err(|r| {
            r.not_sub_attach_span(self.span, other.span)
        })
    }

    fn assert_eq(&self, other: &Spanned<B>, ctx: &mut TypeContext) -> TypeResult<()> {
        self.base.assert_eq(&other.base, ctx).map_err(|r| {
            r.not_eq_attach_span(self.span, other.span)
        })
    }
}

// used when operands should not have any type variables
struct NoTypeContext;

impl TypeContext for NoTypeContext {
    fn gen_report(&self) -> TypeReport {
        TypeReport::new(Locale::dummy())
    }
    fn last_tvar(&self) -> Option<TVar> {
        None
    }
    fn gen_tvar(&mut self) -> TVar {
        panic!("gen_tvar is not supposed to be called here");
    }
    fn copy_tvar(&mut self, tvar: TVar) -> TVar {
        panic!("copy_tvar({:?}) is not supposed to be called here", tvar);
    }
    fn assert_tvar_sub(&mut self, lhs: TVar, rhs: &Ty) -> TypeResult<()> {
        panic!("assert_tvar_sub({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
    fn assert_tvar_sup(&mut self, lhs: TVar, rhs: &Ty) -> TypeResult<()> {
        panic!("assert_tvar_sup({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
    fn assert_tvar_eq(&mut self, lhs: TVar, rhs: &Ty) -> TypeResult<()> {
        panic!("assert_tvar_eq({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
    fn assert_tvar_sub_tvar(&mut self, lhs: TVar, rhs: TVar) -> TypeResult<()> {
        panic!("assert_tvar_sub_tvar({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
    fn assert_tvar_eq_tvar(&mut self, lhs: TVar, rhs: TVar) -> TypeResult<()> {
        panic!("assert_tvar_eq_tvar({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
    fn get_tvar_bounds(&self, tvar: TVar) -> (flags::Flags /*lb*/, flags::Flags /*ub*/) {
        panic!("get_tvar_bounds({:?}) is not supposed to be called here", tvar);
    }
    fn get_tvar_exact_type(&self, tvar: TVar) -> Option<Ty> {
        panic!("get_tvar_exact_type({:?}) is not supposed to be called here", tvar);
    }

    fn gen_rvar(&mut self) -> RVar {
        panic!("gen_rvar is not supposed to be called here");
    }
    fn copy_rvar(&mut self, rvar: RVar) -> RVar {
        panic!("copy_rvar({:?}) is not supposed to be called here", rvar);
    }
    fn assert_rvar_sub(&mut self, lhs: RVar, rhs: RVar) -> TypeResult<()> {
        panic!("assert_rvar_sub({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
    fn assert_rvar_eq(&mut self, lhs: RVar, rhs: RVar) -> TypeResult<()> {
        panic!("assert_rvar_eq({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
    fn assert_rvar_includes(&mut self, lhs: RVar, rhs: &[(Key, Slot)]) -> TypeResult<()> {
        panic!("assert_rvar_includes({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
    fn assert_rvar_closed(&mut self, rvar: RVar) -> TypeResult<()> {
        panic!("assert_rvar_closed({:?}) is not supposed to be called here", rvar);
    }
    fn list_rvar_fields(&self, rvar: RVar,
                        _f: &mut FnMut(&Key, &Slot) -> Result<(), ()>) -> Result<RVar, ()> {
        panic!("list_rvar_fields({:?}, ...) is not supposed to be called here", rvar)
    }

    fn fmt_class(&self, cls: Class, _f: &mut fmt::Formatter) -> fmt::Result {
        panic!("fmt_class({:?}, ...) is not supposed to be called here", cls);
    }
    fn is_subclass_of(&self, lhs: ClassId, rhs: ClassId) -> bool {
        panic!("is_subclass_of({:?}, {:?}) is not supposed to be called here", lhs, rhs);
    }
}

impl Lattice for TVar {
    fn assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> TypeResult<()> {
        ctx.assert_tvar_sub_tvar(*self, *other)
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TypeContext) -> TypeResult<()> {
        ctx.assert_tvar_eq_tvar(*self, *other)
    }
}

impl Lattice for RVar {
    fn assert_sub(&self, other: &Self, ctx: &mut TypeContext) -> TypeResult<()> {
        ctx.assert_rvar_sub(self.clone(), other.clone())
    }

    fn assert_eq(&self, other: &Self, ctx: &mut TypeContext) -> TypeResult<()> {
        ctx.assert_rvar_eq(self.clone(), other.clone())
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

