use std::fmt;
use kailua_env::{Span, Spanned, WithLoc};
use kailua_diag::{self, Report, Reporter};
use kailua_syntax::Name;
use kailua_types::ty::{TypeContext, ClassSystemId, ClassId, Class, Display, DisplayState};
use kailua_types::ty::{Slot, SpannedSlotSeq, Key, T, Nil};
use message as m;

/// Defines the various characteristics of class systems.
///
/// While each class system gets a unique class system identifier (`ClassSystemId`),
/// the system itself has no permanent knowledge about its identifier.
/// Instead every method that may have to produce a `ClassId` is given the current `ClassSystemId`,
/// either by a part of the input `ClassId` or a separate parameter if necessary.
///
/// The implementations are expected to lock internal data structures as necessary.
pub trait ClassSystem: Send + Sync + fmt::Debug {
    /// Invoked when a function with the `[make_class(<this system>)]` attribute gets called.
    /// Should return an internal handle to the class if possible, which gets stored in the context.
    ///
    /// This happens after the argument type checking, so the `argtys` are guaranteed to be
    /// correctly typed according to the original function (but one should take care of
    /// the situation that the function prototype itself is wrong).
    ///
    /// This method has a default implementation which accepts a single optional parameter
    /// for the parent class prototype and calls the `assume_class` method.
    fn make_class(&self, self_csid: ClassSystemId, argtys: SpannedSlotSeq, outerspan: Span,
                  ctx: &mut TypeContext, report: &Report) -> kailua_diag::Result<Option<ClassId>> {
        if let Some(parent) = extract_parent(argtys, ctx, report)? {
            self.assume_class(self_csid, parent, outerspan, ctx, report)
        } else {
            Ok(None)
        }
    }

    /// Invoked when `--# assume class(<this system>)` gets processed.
    fn assume_class(&self, self_csid: ClassSystemId, parent: Option<Spanned<ClassId>>,
                    outerspan: Span, ctx: &mut TypeContext,
                    report: &Report) -> kailua_diag::Result<Option<ClassId>>;

    /// Names a defined class. Fails with the previous name if already named.
    ///
    /// This is distinct from `make_class` and `assume_class` to allow unnamed classes,
    /// which can naturally occur in Kailua (`make_class` heavily relies on this too).
    fn name_class(&self, cid: ClassId, name: Spanned<Name>) -> Result<(), Spanned<Name>>;

    /// Should return true if the class `lhs` is a subtype of another class `rhs`.
    ///
    /// The caller guarantees that `lhs` refers to a class defined from given class system.
    fn is_subclass_of(&self, lhs: ClassId, rhs: ClassId) -> bool;

    /// Returns the type of the `value.key` appearing in the right hand side of assignments
    /// where `value`'s type is a given nominal type.
    ///
    /// Should return `None` if there is no such field or the field is forbidden from access.
    ///
    /// The caller guarantees that the `cls` refers to a class defined from given class system.
    fn index_rval(&self, cls: Class, key: Spanned<&Key>, expspan: Span,
                  ctx: &mut TypeContext, report: &Report) -> kailua_diag::Result<Option<Slot>>;

    /// Returns the type of the `value.key` appearing in the left hand side of assignments
    /// where `value`'s type is a given nominal type.
    ///
    /// If it could create a new field, it can make use of the optional `hint` type
    /// to initialize a new field. Whether to use the hint is mostly optional,
    /// but it is often required for avoiding issues with the current type constraint solver.
    /// The returned slot has no relation to the original hint type if any.
    ///
    /// Should return `None` if it is forbidden to assign (or create) to that field at all;
    /// an appropriate report should be generated in this case.
    /// Otherwise the first `bool` should be true when a new field has been created;
    /// the resulting slot can be a type variable to allow the eventual type assignment.
    ///
    /// The caller guarantees that the `cls` refers to a class defined from given class system.
    fn index_lval(&self, cls: Class, key: Spanned<&Key>, expspan: Span,
                  hint: Option<&Slot>, ctx: &mut TypeContext,
                  report: &Report) -> kailua_diag::Result<Option<(bool, Slot)>>;

    /// Prints the nominal type name (or an appropriate placeholder if unnamed) to the formatter.
    fn fmt_class(&self, cid: ClassId, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result;

    /// Iterates over a list of keys and corresponding types available to given nominal type.
    /// The closure can stop the iteration by returning `Err`.
    /// The iteration order is unspecified but all keys will be unique.
    fn list_fields(&self, cls: Class,
                   f: &mut FnMut(&Key, &Slot) -> Result<(), ()>) -> Result<(), ()>;

    /// Iterates over a list of direct parent nominal types of given class.
    /// The closure can stop the iteration by returning `Err`.
    /// The iteration order is unspecified but all values will be unique.
    fn list_parents(&self, cid: ClassId,
                    f: &mut FnMut(ClassId) -> Result<(), ()>) -> Result<(), ()>;
}

pub mod dumb;
pub mod gideros;

/// Returns the predefined class system object from the name.
///
/// This is currently the only way to define class systems from the Kailua source code.
pub fn make_predefined_class_system(name: &str) -> Option<Box<ClassSystem>> {
    match name {
        "gideros" => Some(Box::new(gideros::GiderosClassSystem::new())),
        _ => None,
    }
}

fn extract_parent(mut argtys: SpannedSlotSeq, ctx: &mut TypeContext,
                  report: &Report) -> kailua_diag::Result<Option<Option<Spanned<ClassId>>>> {
    let argty = argtys.ensure_at(0);

    let parent = if let Some(arg) = ctx.resolve_exact_type(&argty.unlift()) {
        if let T::None = *arg {
            Some(None)
        } else if let T::Class(Class::Prototype(cid)) = *arg {
            if arg.nil() == Nil::Silent { Some(Some(cid)) } else { None }
        } else {
            None
        }
    } else {
        None
    };

    if let Some(parent) = parent {
        Ok(Some(parent.map(|cid| cid.with_loc(argty))))
    } else {
        report.error(argty, m::BadClassParent { ty: argty.unlift().display(ctx) }).done()?;
        Ok(None)
    }
}

