// TODO many routines should be "generic" enough to be refactored

// notes on the Gideros class system:
//
// it is an "archetypal" Lua class system that shares many other features with
// most "naive" class systems for Lua. it distinguishes the prototype and the instance
// (thus the enum structure of `Class` type) and only instances are subject to subtyping.
// the internal constructor method `init` is called when the external constructor `new` is called,
// possibly following the inheritance chain. while it is technically possible to call `new`
// without matching `init` methods, we chose to require `init` to be defined for the consistency.
//
// overriding works by simply shadowing the parent methods explicit calls to the parent methods
// should be made if necessary---which is problematic because it has no concept of bound methods.
// for example if there are two methods `A.f` and `B.f` (where `B` is a child class of `A`),
// calling `x.f` where `x` is typed as `A` will really call either `A.f` or `B.f`,
// but the resolution happens when `x.f` is evaluated and not when it is actually called!
// consequently calling `x.f` with different value typed as `A` risks the soundness violation.
// it may be possible to force the use of `x:f()` to equate the self type and first argument,
// but this may be a serious drawback, so for now we only handle the constructors specificially:
// they cannot be accessed via instances, and in turn can be overriden as long as
// non-self arguments match. for other cases overriding requires strict subtyping
// and methods couldn't be overriden in a usual way.
//
// Gideros class system also features the topmost genesis class (normally `Object`) which is
// a parent of all other classes (and `class()` is equivalent to `class(Object)`).
// for the sake of clarity there can be only one class with no parent (assumed to be `Object`)
// and no other classes can be `--# assume`d with no parent class.

use std::fmt;
use std::collections::{HashSet, HashMap};
use vec_map::{self, VecMap};
use parking_lot::RwLock;

use kailua_env::{Span, Spanned, WithLoc};
use kailua_diag::{self, Report, Reporter};
use kailua_syntax::{Str, Name};
use kailua_types::ty::{TypeContext, ClassSystemId, ClassId, Class, Display, DisplayState, Lattice};
use kailua_types::ty::{Slot, SpannedSlotSeq, Key, T, Ty, TySeq, F, Functions, Function, Nil};
use kailua_types::diag::{TypeReportMore, TypeReportHint};
use message as m;
use super::ClassSystem;

#[derive(Clone, Debug)]
enum Field {
    // the field is defined in this class
    Slot(Spanned<Slot>),

    // field(s) are defined in the children class
    Children,

    // field(s) are defined in the instance of current class
    Instance,
}

#[derive(Clone, Debug)]
struct ClassDef {
    generation: u32,
    parent: Option<u32>,
    new_ty: Option<Slot>, // a dummy slot after the first error on the constructor typing
    class_fields: HashMap<Key, Field>,
    instance_fields: HashMap<Key, Field>,
}

impl ClassDef {
    fn fields(&self, proto: bool) -> &HashMap<Key, Field> {
        if proto { &self.class_fields } else { &self.instance_fields }
    }

    fn fields_mut(&mut self, proto: bool) -> &mut HashMap<Key, Field> {
        if proto { &mut self.class_fields } else { &mut self.instance_fields }
    }
}

struct Ancestors<'a> {
    classes: &'a [ClassDef],
    current: Option<u32>,
}

impl<'a> Ancestors<'a> {
    fn new(classes: &'a [ClassDef], current: u32) -> Ancestors<'a> {
        Ancestors { classes: classes, current: Some(current) }
    }
}

impl<'a> Iterator for Ancestors<'a> {
    type Item = (u32, &'a ClassDef);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = self.current {
            let cls = &self.classes[current as usize];
            self.current = cls.parent;
            Some((current, cls))
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct GiderosClassSystem {
    // the first class defined is assumed to be the genesis class
    classes: RwLock<Vec<ClassDef>>,

    // frequently read even when classes are locked, so has to be separated
    class_names: RwLock<VecMap<Spanned<Name>>>,
}

const GENESIS_CLASS: u32 = 0;

const NEW_KEY: &'static [u8] = b"new";
const INIT_KEY: &'static [u8] = b"init";

fn is_new_key(key: &Key) -> bool {
    match *key {
        Key::Str(ref s) => &s[..] == NEW_KEY,
        _ => false,
    }
}

fn is_init_key(key: &Key) -> bool {
    match *key {
        Key::Str(ref s) => &s[..] == INIT_KEY,
        _ => false,
    }
}

impl GiderosClassSystem {
    pub fn new() -> GiderosClassSystem {
        GiderosClassSystem {
            classes: RwLock::new(Vec::new()),
            class_names: RwLock::new(VecMap::new()),
        }
    }

    fn lookup<'a>(classes: &'a [ClassDef], cid: ClassId, proto: bool,
                  key: &Key) -> Option<&'a Spanned<Slot>> {
        for (_, cls) in Ancestors::new(&classes, cid.1) {
            let field = cls.fields(proto).get(key);
            if let Some(&Field::Slot(ref info)) = field {
                // skip the "children" mark, which is not readable nor updatable (for now)
                return Some(info);
            }
        }
        None
    }

    fn new_method_from_init(classes: &[ClassDef], cid: ClassId, init: &Spanned<Slot>,
                            ctx: &mut TypeContext, report: &Report) -> kailua_diag::Result<Slot> {
        // ensure that the type can be resolved...
        let ty = if let Some(ty) = ctx.resolve_exact_type(&init.unlift()) {
            ty
        } else {
            report.error(init, m::InexactInitMethod { init: init.base.display(ctx) }).done()?;
            return Ok(Slot::dummy());
        };

        // ...and is a function.
        let mut func = match *ty {
            T::Functions(ref func) => match **func {
                Functions::Simple(ref f) => f.to_owned(),
                _ => {
                    report.error(init, m::OverloadedFuncInitMethod { init: init.base.display(ctx) })
                          .done()?;
                    return Ok(Slot::dummy());
                }
            },
            _ => {
                report.error(init, m::NonFuncInitMethod { init: init.base.display(ctx) }).done()?;
                return Ok(Slot::dummy());
            },
        };

        // strip the first argument which should be a fresh class instance
        let mut selfarg_ok = false;
        if !func.args.head.is_empty() {
            let selfarg = func.args.head.remove(0);
            if let Some(selfarg) = ctx.resolve_exact_type(&selfarg) {
                if selfarg.nil() == Nil::Silent {
                    if let T::Class(Class::Instance(cid_)) = *selfarg {
                        // the constructor can be shared for multiple classes,
                        // so any parent class that can accept the current class is fine
                        selfarg_ok =
                            cid.0 == cid_.0 &&
                            Ancestors::new(classes, cid.1).find(|&(c, _)| c == cid_.1).is_some();
                    }
                }
            }
        }
        if !func.argnames.is_empty() {
            func.argnames.remove(0);
        }
        if !selfarg_ok {
            report.error(init, m::BadSelfInInitMethod { init: init.base.display(ctx) }).done()?;
            return Ok(Slot::dummy());
        }

        // now `init` is: function(/* removed self */, ...) -> any
        // fix the return type to make a signature for the `new` method
        let returns = T::Class(Class::Instance(cid));
        let ctor = Function { args: func.args, argnames: func.argnames,
                              returns: Some(TySeq::from(returns)) };
        let ctor = Slot::new(F::Const, Ty::new(T::func(ctor)));

        Ok(ctor)
    }
}

impl ClassSystem for GiderosClassSystem {
    fn make_class(&self, self_csid: ClassSystemId, argtys: SpannedSlotSeq, outerspan: Span,
                  ctx: &mut TypeContext, report: &Report) -> kailua_diag::Result<Option<ClassId>> {
        // we have to intercept `make_class` to apply the default parent class
        if let Some(mut parent) = super::extract_parent(argtys, ctx, report)? {
            if !self.classes.read().is_empty() {
                parent = parent.or(Some(ClassId(self_csid, GENESIS_CLASS).without_loc()));
            }
            self.assume_class(self_csid, parent, outerspan, ctx, report)
        } else {
            Ok(None)
        }
    }

    fn assume_class(&self, self_csid: ClassSystemId, parent: Option<Spanned<ClassId>>,
                    outerspan: Span, _ctx: &mut TypeContext,
                    report: &Report) -> kailua_diag::Result<Option<ClassId>> {
        let parent = match parent {
            Some(Spanned { base: ClassId(csid, cid), .. }) if csid == self_csid => Some(cid),
            Some(Spanned { base: ClassId(_, _), span }) => {
                report.error(span, m::ClassInheritFromDifferentClassSystem {}).done()?;
                None
            },
            None => None,
        };

        let mut classes = self.classes.write();

        let (parent, generation) = if let Some(parent) = parent {
            assert!((parent as usize) < classes.len(), "invalid ClassId");
            (Some(parent), classes[parent as usize].generation + 1)
        } else if classes.is_empty() {
            // only the genesis class can have no parent
            (None, 0)
        } else {
            // otherwise issue an error and fill the genesis class in
            report.error(outerspan, m::MissingParentClassForGideros {}).done()?;
            (Some(GENESIS_CLASS), 1)
        };

        let cid = ClassId(self_csid, classes.len() as u32);
        classes.push(ClassDef {
            generation: generation,
            parent: parent,
            new_ty: None,
            class_fields: HashMap::new(),
            instance_fields: HashMap::new(),
        });
        Ok(Some(cid))
    }

    fn name_class(&self, cid: ClassId, name: Spanned<Name>) -> Result<(), Spanned<Name>> {
        let mut names = self.class_names.write();
        match names.entry(cid.1 as usize) {
            vec_map::Entry::Occupied(e) => Err(e.get().clone()),
            vec_map::Entry::Vacant(e) => {
                info!("named {:?} as {:?}", cid, name);
                e.insert(name);
                Ok(())
            },
        }
    }

    fn is_subclass_of(&self, lhs: ClassId, rhs: ClassId) -> bool {
        if lhs.0 != rhs.0 {
            return false;
        }

        let classes = self.classes.read();

        let lhs = lhs.1;
        let rhs = rhs.1;

        assert!((lhs as usize) < classes.len(), "invalid ClassId for lhs");
        assert!((rhs as usize) < classes.len(), "invalid ClassId for rhs");

        if lhs == rhs {
            return true;
        }

        // as we keep the "generation" (depth of inheritance tree) for each class,
        // class A is a subtype of B if and only if gen(A) >= gen(B) and
        // parent^{gen(A) - gen(B)}(B) = A. (equality only holds for A = B.)

        let lgen = classes[lhs as usize].generation;
        let rgen = classes[rhs as usize].generation;

        (lgen >= rgen &&
         Ancestors::new(&classes, lhs).nth((lgen - rgen) as usize).map(|e| e.0) == Some(rhs))
    }

    fn index_rval(&self, cls: Class, key: Spanned<&Key>, expspan: Span, ctx: &mut TypeContext,
                  report: &Report) -> kailua_diag::Result<Option<Slot>> {
        let (proto, cid) = match cls {
            Class::Prototype(cid) => (true, cid),
            Class::Instance(cid) => (false, cid),
        };

        if !proto && is_init_key(&key) {
            report.error(&key, m::CannotAccessCtorThruInstance {}).done()?;
            return Ok(None);
        }

        let mut classes = self.classes.write();
        if is_new_key(&key) {
            if let Some(new) = classes[cid.1 as usize].new_ty.as_ref().map(|s| s.clone()) {
                Ok(Some(new))
            } else {
                // initialize the new field from the existing init field
                let slot = if let Some(init) = Self::lookup(&classes, cid, true,
                                                              &Key::from(Str::from(INIT_KEY))) {
                    let slot = Self::new_method_from_init(&classes, cid, init, ctx, report)?;
                    trace!("created a new method {:?} for {:?} from the constructor {:?}",
                           slot, cid, init);
                    slot
                } else {
                    report.error(&key, m::NoCtor {}).done()?;
                    Slot::dummy()
                };
                classes[cid.1 as usize].new_ty = Some(slot.clone());

                // we are going to use multiple readers, so switch to the shared lock
                drop(classes);
                let classes = self.classes.read();

                // finally, complete the deferred subtyping check for `init`.
                // this is no different from `index_lval` but excludes the `self` argument,
                // allowing for overriding `init` with identical arguments except for `self`.
                for (_, cls) in Ancestors::new(&classes, cid.1).skip(1) {
                    if let Some(ref parent_slot) = cls.new_ty {
                        if let Err(r) = slot.assert_sub(parent_slot, ctx) {
                            report.error(expspan,
                                         m::NotSubtypeOfParentField {
                                             key: &key, sub: slot.display(ctx),
                                             sup: parent_slot.display(ctx),
                                         })
                                  .report_types(r, TypeReportHint::None)
                                  .done()?;
                        }
                    }
                }

                Ok(Some(slot))
            }
        } else {
            if !proto {
                if let Some(info) = Self::lookup(&classes, cid, false, &key) {
                    return Ok(Some(info.base.clone()));
                }
            }
            if let Some(info) = Self::lookup(&classes, cid, true, &key) {
                return Ok(Some(info.base.clone()));
            }
            Ok(None)
        }
    }

    fn index_lval(&self, cls: Class, key: Spanned<&Key>, expspan: Span,
                  hint: Option<&Slot>, ctx: &mut TypeContext,
                  report: &Report) -> kailua_diag::Result<Option<(bool, Slot)>> {
        let (proto, cid) = match cls {
            Class::Prototype(cid) => (true, cid),
            Class::Instance(cid) => (false, cid),
        };

        if is_new_key(&key) {
            // `new` is only accessible as an r-value due to its dependency to `init`
            report.error(expspan, m::ReservedNewMethod {}).done()?;
            return Ok(None);
        }

        // `init` is exempted from stricter subtyping constraints (see above).
        // its constraints are only checked when `new` is accessed for the first time.
        let is_init = is_init_key(&key);
        if !proto && is_init {
            report.error(&key, m::CannotAccessCtorThruInstance {}).done()?;
            return Ok(None);
        }

        let mut classes = self.classes.write();
        match classes[cid.1 as usize].fields(proto).get(&key) {
            Some(&Field::Slot(ref slot)) => {
                // the field exists in the current class
                return Ok(Some((false, slot.base.clone())));
            }

            Some(&Field::Children) => {
                // the field does not exist in the current class, but is already used by children.
                // for now we disallow the assignment to such field.
                // in the future we can possibly allow a supertype of all currently defined types.
                report.error(&key, m::CannotCreateFieldDefinedInChildren { key: &key }).done()?;
                return Ok(None);
            }

            Some(&Field::Instance) => {
                // same as above
                report.error(&key, m::CannotCreateFieldDefinedInInstance { key: &key }).done()?;
                return Ok(None);
            }

            None => {}
        }

        // this type will be a type of the new field, if created (see below)
        let slot = if let Some(hint) = hint {
            let slot = hint.clone();
            slot.adapt(F::Var, ctx); // always adapt to Var
            slot
        } else {
            let tvar = T::TVar(ctx.gen_tvar());
            Slot::new(F::Unknown, Ty::new(tvar))
        };

        // mark missing fields of the same name in parents
        // so that any further assignments to those marks are subject to subtyping constraints.
        let mut mark_missing = |cls: &mut ClassDef, proto: bool, or_insert: Field, slot: &Slot| {
            match *cls.fields_mut(proto).entry(key.base.clone()).or_insert(or_insert) {
                // unlike prototypes (where all prototype types have distinct namespaces),
                // assigning to the instance field defined in parent classes will *not*
                // create a new field. for example, if the same instance is accessible via
                // two variables `a` and `b` with instance types `A` and its child `B`,
                // a field created via `a.x` should be accessible via `b.x` and vice versa.
                Field::Slot(ref parent_slot) if !proto => {
                    Ok(Some((false, parent_slot.base.clone())))
                },

                // if there is a field with the same name in the parents
                // a new field created should be a subtype of that field in order to be compatible.
                // (note that the subtyping will be actually useful only for const slots.)
                // we do allow for overwriting class fields with instance fields though.
                Field::Slot(ref parent_slot) if !is_init => {
                    if let Err(r) = slot.assert_sub(parent_slot, ctx) {
                        report.error(expspan,
                                     m::NotSubtypeOfParentField {
                                         key: &key, sub: slot.display(ctx),
                                         sup: parent_slot.base.display(ctx),
                                     })
                              .note_if(parent_slot, m::PreviousParentFieldType {})
                              .report_types(r, TypeReportHint::None)
                              .done()?;
                    }
                    Ok(None)
                },

                // see above for the `init` declaration in the prototype
                Field::Slot(_) => Ok(None),

                Field::Children | Field::Instance => Ok(None),
            }
        };

        // mark all prototype & instance fields that can be affected by this assignment:
        // - prototype: all parent prototypes and instances
        // - instance: all parent prototypes and instance, plus the current prototype
        if !proto {
            if let Some(slot) = mark_missing(&mut classes[cid.1 as usize], true,
                                             Field::Instance, &slot)? {
                return Ok(Some(slot));
            }
        }
        let mut parent_cid = classes[cid.1 as usize].parent;
        while let Some(cid) = parent_cid {
            let cls = &mut classes[cid as usize];
            if let Some(slot) = mark_missing(cls, false, Field::Children, &slot)? {
                return Ok(Some(slot));
            }
            if let Some(slot) = mark_missing(cls, true, Field::Children, &slot)? {
                return Ok(Some(slot));
            }
            parent_cid = cls.parent;
        }

        // finally update the current class
        let fields = classes[cid.1 as usize].fields_mut(proto);
        fields.insert(key.base.clone(), Field::Slot(slot.clone().with_loc(&key)));

        Ok(Some((true, slot)))
    }

    fn fmt_class(&self, cid: ClassId, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        let names = self.class_names.read();
        match (&st.locale[..], &names.get(cid.1 as usize)) {
            (_,    &Some(ref name)) => write!(f, "{:+}", name),
            ("ko", &None) => write!(f, "<이름 없는 클래스 #{}.{}>", (cid.0).0, cid.1),
            (_,    &None) => write!(f, "<unnamed class #{}.{}>", (cid.0).0, cid.1),
        }
    }

    fn list_fields(&self, cls: Class,
                   f: &mut FnMut(&Key, &Slot) -> Result<(), ()>) -> Result<(), ()> {
        let (proto, cid) = match cls {
            Class::Prototype(cid) => (true, cid),
            Class::Instance(cid) => (false, cid),
        };

        let classes = self.classes.read();
        let mut seen = HashSet::new();
        let mut list = |proto| {
            for (_, cls) in Ancestors::new(&classes, cid.1) {
                for (key, slot) in cls.fields(proto) {
                    if let Field::Slot(ref slot) = *slot {
                        if seen.insert(key) {
                            f(key, slot)?;
                        }
                    }
                }
            }
            Ok(())
        };

        if !proto {
            list(false)?;
        }
        list(true)
    }

    fn list_parents(&self, cid: ClassId,
                    f: &mut FnMut(ClassId) -> Result<(), ()>) -> Result<(), ()> {
        let self_csid = cid.0;
        if let Some(cid) = self.classes.read()[cid.1 as usize].parent {
            f(ClassId(self_csid, cid))?;
        }
        Ok(())
    }
}

