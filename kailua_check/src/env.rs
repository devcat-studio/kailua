use std::ops;
use std::str;
use std::fmt;
use std::result;
use std::cell::RefCell;
use std::rc::Rc;
use std::collections::{hash_map, HashMap, HashSet};

use kailua_env::{self, Span, Spanned, WithLoc, ScopedId, ScopeMap, SpanMap};
use kailua_diag::{Result, Kind, Report, Reporter, Locale, Localize};
use kailua_syntax::{Str, Name};
use kailua_syntax::ast::NameRef;
use kailua_types::diag::{TypeReportHint, TypeReportMore};
use kailua_types::ty::{Displayed, Display, DisplayName};
use kailua_types::ty::{Ty, TySeq, Nil, T, Slot, F, TVar, Lattice, Union, Tag};
use kailua_types::ty::{TypeContext, TypeResolver, ClassId, Class, Tables, Functions, Function, Key};
use kailua_types::ty::flags::*;
use kailua_types::env::Types;
use defs::get_defs;
use options::Options;
use check::Checker;
use message as m;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Id {
    Local(usize, ScopedId),
    Global(Name),
}

impl Id {
    pub fn from(map_index: usize, nameref: NameRef) -> Id {
        match nameref {
            NameRef::Local(scoped_id) => Id::Local(map_index, scoped_id),
            NameRef::Global(name) => Id::Global(name),
        }
    }

    pub fn name<'a, R: Report>(&'a self, ctx: &'a Context<R>) -> &'a Name {
        match *self {
            Id::Local(map_index, ref scoped_id) => scoped_id.name(&ctx.scope_maps[map_index]),
            Id::Global(ref name) => name,
        }
    }

    pub fn scope<R: Report>(&self, ctx: &Context<R>) -> Option<kailua_env::Scope> {
        match *self {
            Id::Local(map_index, ref scoped_id) =>
                Some(scoped_id.scope(&ctx.scope_maps[map_index])),
            Id::Global(_) => None,
        }
    }

    pub fn is_global(&self) -> bool {
        match *self {
            Id::Local(..) => false,
            Id::Global(..) => true,
        }
    }

    pub fn display<'a, R: Report>(&'a self, ctx: &'a Context<R>) -> IdDisplay<'a, R> {
        IdDisplay { id: self, ctx: ctx }
    }
}

#[must_use]
pub struct IdDisplay<'a, R: 'a> {
    id: &'a Id,
    ctx: &'a Context<R>,
}

impl<'a, R: Report> fmt::Display for IdDisplay<'a, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.id {
            Id::Local(map_index, ref scoped_id) => {
                let (name, scope) = self.ctx.scope_maps[map_index].find_id(scoped_id);
                write!(f, "{:?}$", name)?;
                { // bijective numeral: a b c d .. z aa ab .. az ba .. zz aaa ..
                    let mut index = map_index;
                    let mut mult = 26;
                    let mut len = 1;
                    while index >= mult {
                        index -= mult;
                        mult *= 26;
                        len += 1;
                    }
                    for _ in 0..len {
                        mult /= 26;
                        write!(f, "{}", (b'a' + (index / mult) as u8) as char)?;
                        index %= mult;
                    }
                }
                write!(f, "{}", scope.to_usize())
            }
            Id::Global(ref name) => {
                write!(f, "{:?}_", name)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Returns<T> {
    None, // the function hasn't returned and has no rettype spec
    Never, // the function has a diverging rettype spec
    Implicit(T), // the function returns but has no rettype spec, returns will be unioned
    Explicit(T), // the function has a rettype spec, cannot be updated
}

impl<T> Returns<T> {
    pub fn as_ref(&self) -> Returns<&T> {
        match *self {
            Returns::None => Returns::None,
            Returns::Never => Returns::Never,
            Returns::Implicit(ref v) => Returns::Implicit(v),
            Returns::Explicit(ref v) => Returns::Explicit(v),
        }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Returns<U> {
        match self {
            Returns::None => Returns::None,
            Returns::Never => Returns::Never,
            Returns::Implicit(v) => Returns::Implicit(f(v)),
            Returns::Explicit(v) => Returns::Explicit(f(v)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Frame {
    pub vararg: Option<TySeq>,
    pub returns: Returns<TySeq>,
}

#[derive(Clone, Debug)]
pub struct NameDef {
    pub span: Span,
    pub slot: NameSlot,
}

#[derive(Clone, Debug)]
pub enum NameSlot {
    None, // not initialized, no type specified
    Unset(Slot),
    Set(Slot),
}

impl NameSlot {
    pub fn set(&self) -> bool {
        match *self {
            NameSlot::None | NameSlot::Unset(_) => false,
            NameSlot::Set(_) => true,
        }
    }

    pub fn slot(&self) -> Option<&Slot> {
        match *self {
            NameSlot::None => None,
            NameSlot::Unset(ref slot) | NameSlot::Set(ref slot) => Some(slot),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeDef {
    pub span: Span,
    pub ty: Ty,
}

#[derive(Clone, Debug)]
pub struct ClassFields {
    pub span: Span,
    pub class_ty: HashMap<Key, Slot>,
    pub instance_ty: HashMap<Key, Slot>,
}

#[derive(Clone, Debug)]
pub struct Scope {
    frame: Option<Frame>,
    types: HashMap<Name, TypeDef>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope { frame: None, types: HashMap::new() }
    }

    pub fn new_function(frame: Frame) -> Scope {
        Scope { frame: Some(frame), types: HashMap::new() }
    }

    pub fn get_frame<'a>(&'a self) -> Option<&'a Frame> {
        self.frame.as_ref()
    }

    pub fn get_frame_mut<'a>(&'a mut self) -> Option<&'a mut Frame> {
        self.frame.as_mut()
    }

    // the span indicates the position of the initial definition
    pub fn get_type<'a>(&'a self, name: &Name) -> Option<&'a TypeDef> {
        self.types.get(name)
    }

    // the caller should check for the outermost types first
    pub fn put_type(&mut self, name: Spanned<Name>, ty: Ty) -> bool {
        self.types.insert(name.base, TypeDef { span: name.span, ty: ty }).is_none()
    }
}

#[derive(Clone, Debug)]
pub struct Module {
    pub returns: Option<Slot>, // None if the module never returns
    pub exported_types: HashMap<Name, TypeDef>,
}

impl Module {
    pub fn dummy() -> Module {
        Module { returns: Some(Slot::dummy()), exported_types: HashMap::new() }
    }
}

#[derive(Clone, Debug)]
enum LoadStatus {
    Done(Module),
    Ongoing(Span), // span for who to blame
}

#[derive(Clone, Debug)]
pub enum SlotSpec {
    // the slot is constructed implicitly (e.g. from `--: const`),
    // so the type should be still copied directly from the initialization
    Implicit(Spanned<Slot>),
    // the slot is specified explicitly and the initialization should be its subtype
    Explicit(Spanned<Slot>),
}

impl SlotSpec {
    pub fn slot(&self) -> &Spanned<Slot> {
        match *self {
            SlotSpec::Implicit(ref slot) | SlotSpec::Explicit(ref slot) => slot,
        }
    }

    pub fn map<F: FnOnce(Slot) -> Slot>(self, f: F) -> SlotSpec {
        match self {
            SlotSpec::Implicit(slot) => SlotSpec::Implicit(slot.map(f)),
            SlotSpec::Explicit(slot) => SlotSpec::Explicit(slot.map(f)),
        }
    }

    pub fn unwrap(self) -> Spanned<Slot> {
        match self {
            SlotSpec::Implicit(slot) | SlotSpec::Explicit(slot) => slot,
        }
    }
}

// global context (also acts as a type context).
// anything has to be retained across multiple files should be here
pub struct Context<R> {
    report: R,
    output: Output,
}

// a "reportless" version of Context, used for analysis
pub struct Output {
    // name, scope and span information
    ids: HashMap<Id, NameDef>,
    scope_maps: Vec<ScopeMap<Name>>,
    spanned_slots: SpanMap<Slot>,

    // TODO this might be eventually found useless
    global_scope: Scope,

    // type context
    types: Types,

    // module information
    opened: HashSet<String>,
    loaded: HashMap<Vec<u8>, LoadStatus>, // corresponds to `package.loaded`

    // runtime information
    string_meta: Option<Spanned<Slot>>,

    // class fields
    class_fields: HashMap<ClassId, ClassFields>,
}

impl<R: Report> Context<R> {
    pub fn new(report: R) -> Context<R> {
        let locale = report.message_locale();
        let mut ctx = Context {
            report: report,
            output: Output {
                ids: HashMap::new(),
                scope_maps: Vec::new(),
                spanned_slots: SpanMap::new(),
                global_scope: Scope::new(),
                types: Types::new(locale),
                opened: HashSet::new(),
                loaded: HashMap::new(),
                string_meta: None,
                class_fields: HashMap::new(),
            }
        };

        // it is fine to return from the top-level, so we treat it as like a function frame
        let global_frame = Frame { vararg: None, returns: Returns::None };
        ctx.global_scope.frame = Some(global_frame);
        ctx
    }

    pub fn report(&self) -> &R {
        &self.report
    }

    pub fn open_library(&mut self, name: &Spanned<Name>, opts: Rc<RefCell<Options>>) -> Result<()> {
        if let Some(defs) = str::from_utf8(&name.base).ok().and_then(get_defs) {
            // one library may consist of multiple files, so we defer duplicate check
            for def in defs {
                if self.opened.insert(def.name.to_owned()) {
                    // the built-in code is parsed independently and has no usable span
                    let chunk = def.to_chunk();
                    let mut env = Env::new(self, opts.clone(), chunk.map);
                    let mut checker = Checker::new(&mut env);
                    checker.visit(&chunk.block)?
                }
            }
        } else {
            self.error(name, m::CannotOpenLibrary {}).done()?;
        }
        Ok(())
    }

    pub fn get_loaded_module(&self, name: &[u8], span: Span) -> Result<Option<Module>> {
        match self.loaded.get(name) {
            Some(&LoadStatus::Done(ref module)) => Ok(Some(module.clone())),
            None => Ok(None),

            // this is allowed in Lua 5.2 and later, but will result in a loop anyway.
            Some(&LoadStatus::Ongoing(oldspan)) => {
                self.error(span, m::RecursiveRequire {})
                    .note(oldspan, m::PreviousRequire {})
                    .done()?;
                Ok(Some(Module::dummy()))
            }
        }
    }

    pub fn mark_module_as_loading(&mut self, name: &[u8], span: Span) {
        self.loaded.entry(name.to_owned()).or_insert(LoadStatus::Ongoing(span));
    }

    pub fn make_class(&mut self, parent: Option<ClassId>, span: Span) -> ClassId {
        let cid = self.types.make_class(parent);
        self.class_fields.insert(cid, ClassFields {
            span: span,
            class_ty: HashMap::new(),
            instance_ty: HashMap::new(),
        });
        cid
    }

    pub fn name_class(&mut self, cid: ClassId, name: Spanned<Name>) -> Result<()> {
        let namespan = name.span;
        if let Err(prevspan) = self.types.name_class(cid, name).map_err(|name| name.span) {
            self.report.warn(namespan, m::RedefinedClassName {})
                       .note(prevspan, m::PreviousClassName {})
                       .done()?;
        }
        Ok(())
    }

    pub fn into_output(self) -> Output {
        self.output
    }
}

impl Output {
    pub fn types(&self) -> &Types {
        &self.types
    }

    pub fn types_mut(&mut self) -> &mut Types {
        &mut self.types
    }

    pub fn spanned_slots(&self) -> &SpanMap<Slot> {
        &self.spanned_slots
    }

    pub fn spanned_slots_mut(&mut self) -> &mut SpanMap<Slot> {
        &mut self.spanned_slots
    }

    pub fn global_scope(&self) -> &Scope {
        &self.global_scope
    }

    pub fn global_scope_mut(&mut self) -> &mut Scope {
        &mut self.global_scope
    }

    pub fn get<'a>(&'a self, id: &Id) -> Option<&'a NameDef> {
        self.ids.get(id)
    }

    pub fn get_mut<'a>(&'a mut self, id: &Id) -> Option<&'a mut NameDef> {
        self.ids.get_mut(id)
    }

    pub fn all<'a>(&'a self) -> hash_map::Iter<'a, Id, NameDef> {
        self.ids.iter()
    }

    pub fn get_class_fields<'a>(&'a self, cid: ClassId) -> Option<&'a ClassFields> {
        self.class_fields.get(&cid)
    }

    pub fn get_class_fields_mut<'a>(&'a mut self, cid: ClassId) -> Option<&'a mut ClassFields> {
        self.class_fields.get_mut(&cid)
    }

    pub fn get_string_meta(&self) -> Option<Spanned<Slot>> {
        self.string_meta.clone()
    }

    // TODO if we've got a common crate for IDE support, this will be there
    pub fn get_available_fields<'a>(&'a self, ty: &Ty) -> Option<HashMap<Key, Slot>> {
        if let Some(mut ty) = self.resolve_exact_type(ty) {
            // if the type includes an explicit nil no direct field access is safe
            if ty.nil() == Nil::Noisy {
                return None;
            }

            // nominal types
            match *ty {
                T::Class(Class::Prototype(cid)) => {
                    let mut fields = HashMap::new();
                    let mut cid = Some(cid);
                    while let Some(cid_) = cid {
                        if let Some(def) = self.get_class_fields(cid_) {
                            // do not update the existing (children's) fields
                            for (k, v) in def.class_ty.iter() {
                                fields.entry(k.clone()).or_insert(v.clone());
                            }
                        }
                        cid = self.get_parent_class(cid_);
                    }
                    return Some(fields);
                }

                T::Class(Class::Instance(cid)) => {
                    // instance fields take preference over class fields, which get overwritten
                    let mut instfields = HashMap::new();
                    let mut fields = HashMap::new();
                    let mut cid = Some(cid);
                    while let Some(cid_) = cid {
                        if let Some(def) = self.get_class_fields(cid_) {
                            // do not update the existing (children's) fields
                            for (k, v) in def.instance_ty.iter() {
                                instfields.entry(k.clone()).or_insert(v.clone());
                            }
                            for (k, v) in def.class_ty.iter() {
                                fields.entry(k.clone()).or_insert(v.clone());
                            }
                        }
                        cid = self.get_parent_class(cid_);
                    }
                    fields.extend(instfields.into_iter());
                    return Some(fields);
                }

                _ => {}
            }

            // string types (use the current metatable instead)
            if ty.flags() == T_STRING {
                if let Some(metaslot) = self.get_string_meta() {
                    if let Some(metaty) = self.resolve_exact_type(&metaslot.unlift()) {
                        ty = metaty;
                    }
                }
            }

            // otherwise it should be a record or similar
            match ty.get_tables() {
                Some(&Tables::Fields(ref rvar)) => {
                    let mut fields = HashMap::new();
                    self.list_rvar_fields(rvar.clone(), &mut |k, v| -> result::Result<(), ()> {
                        fields.insert(k.clone(), v.clone());
                        Ok(())
                    }).expect("list_rvar_fields exited early while we haven't break");
                    return Some(fields);
                }

                Some(&Tables::ArrayN(ref value)) => {
                    // has the only definite field `n`
                    let mut fields = HashMap::new();
                    fields.insert(Key::from(Str::from(b"n"[..].to_owned())),
                                  Slot::new(value.flex(), Ty::new(T::Integer)));
                    return Some(fields);
                }

                _ => {}
            }
        }

        None
    }
}

impl ops::Deref for Output {
    type Target = Types;
    fn deref(&self) -> &Types { &self.types }
}

impl ops::DerefMut for Output {
    fn deref_mut(&mut self) -> &mut Types { &mut self.types }
}

impl<R: Report> ops::Deref for Context<R> {
    type Target = Output;
    fn deref(&self) -> &Output { &self.output }
}

impl<R: Report> ops::DerefMut for Context<R> {
    fn deref_mut(&mut self) -> &mut Output { &mut self.output }
}

impl<R: Report> Report for Context<R> {
    fn message_locale(&self) -> Locale {
        self.report.message_locale()
    }

    fn add_span(&self, k: Kind, s: Span, m: &Localize) -> Result<()> {
        self.report.add_span(k, s, m)
    }
}

// per-file environment
pub struct Env<'ctx, R: 'ctx> {
    context: &'ctx mut Context<R>,
    opts: Rc<RefCell<Options>>,
    map_index: usize,
    scopes: Vec<Scope>,
    // separate from scoped types, `--# type` will set both
    exported_types: HashMap<Name, TypeDef>,
}

impl<'ctx, R: Report> Env<'ctx, R> {
    pub fn new(context: &'ctx mut Context<R>, opts: Rc<RefCell<Options>>,
               map: ScopeMap<Name>) -> Env<'ctx, R> {
        let map_index = context.scope_maps.len();
        context.scope_maps.push(map);
        let global_frame = Frame { vararg: None, returns: Returns::None };
        Env {
            context: context,
            opts: opts,
            map_index: map_index,
            // we have local variables even at the global position, so we need at least one Scope
            scopes: vec![Scope::new_function(global_frame)],
            exported_types: HashMap::new(),
        }
    }

    pub fn types(&mut self) -> &mut Types {
        &mut self.context.types
    }

    // not to be called internally; it intentionally reduces the lifetime
    pub fn context(&mut self) -> &mut Context<R> {
        self.context
    }

    pub fn opts(&self) -> &Rc<RefCell<Options>> {
        &self.opts
    }

    pub fn scope_map(&self) -> &ScopeMap<Name> {
        &self.context.scope_maps[self.map_index]
    }

    // convenience function to avoid mutable references
    pub fn display<'a, 'c, T: Display>(&'c self, x: &'a T) -> Displayed<'a, T, &'c TypeContext> {
        x.display(&self.context.types)
    }

    pub fn id_from_nameref(&self, nameref: &Spanned<NameRef>) -> Spanned<Id> {
        Id::from(self.map_index, nameref.base.clone()).with_loc(nameref)
    }

    pub fn enter(&mut self, scope: Scope) {
        debug!("entering to a scope {:#?}", scope);
        self.scopes.push(scope);
    }

    pub fn leave(&mut self) {
        assert!(self.scopes.len() > 1);
        let scope = self.scopes.pop().unwrap();
        debug!("leaving from a scope {:#?}", scope);
    }

    // returns a pair of type flags that is an exact lower and upper bound for that type
    // used as an approximate type bound testing like arithmetics;
    // better be replaced with a non-instantiating assertion though.
    pub fn get_type_bounds(&self, ty: &Ty) -> (/*lb*/ Flags, /*ub*/ Flags) {
        self.context.get_type_bounds(ty)
    }

    // exactly resolves the type variable inside `ty` if possible
    // this is a requirement for table indexing and function calls
    pub fn resolve_exact_type<'a>(&self, ty: &Ty) -> Option<Ty> {
        self.context.resolve_exact_type(ty)
    }

    pub fn return_from_module(mut self, modname: &[u8], diverging: bool,
                              span: Span) -> Result<Option<Module>> {
        // note that this scope is distinct from the global scope
        let top_scope = self.scopes.drain(..).next().unwrap();
        let returns = match top_scope.frame.unwrap().returns {
            Returns::Implicit(returns) | Returns::Explicit(returns) => Some(returns.into_first()),
            // chunk implicitly returns nil at the end (unless it's diverging)
            Returns::None if !diverging => Some(Ty::noisy_nil()),
            Returns::None | Returns::Never => None,
        };

        let modty = if let Some(returns) = returns {
            if let Some(ty) = self.resolve_exact_type(&returns) {
                let flags = ty.flags();

                // prepare for the worse
                if !flags.is_dynamic() && flags.contains(T_FALSE) {
                    self.error(span, m::ModCannotReturnFalse {}).done()?;
                    return Ok(None);
                }

                // simulate `require` behavior, i.e. nil translates to true
                let ty = if ty.nil() == Nil::Noisy {
                    let tywithoutnil = ty.without_nil().with_loc(span);
                    tywithoutnil.union(&T::True.without_loc(), false, self.types()).expect(
                        "failed to union the module return type with True, this should be \
                         always possible because we know the return type doesn't have a tvar!"
                    )
                } else {
                    ty
                };

                Some(ty)
            } else {
                // TODO ideally we would want to resolve type variables in this type
                self.error(span, m::ModCannotReturnInexactType { returns: self.display(&returns) })
                    .done()?;
                return Ok(None);
            }
        } else {
            None
        };

        // this has to be Var since the module is shared across the entire program
        let module = Module {
            returns: modty.map(|ty| Slot::new(F::Var, ty)),
            exported_types: self.exported_types,
        };
        self.context.loaded.insert(modname.to_owned(), LoadStatus::Done(module.clone()));
        Ok(Some(module))
    }

    // not to be called internally; it intentionally reduces the lifetime
    pub fn global_scope(&self) -> &Scope {
        self.context.global_scope()
    }

    // not to be called internally; it intentionally reduces the lifetime
    pub fn global_scope_mut(&mut self) -> &mut Scope {
        self.context.global_scope_mut()
    }

    pub fn current_scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn get_name<'a>(&'a self, nameref: &'a NameRef) -> &'a Name {
        match *nameref {
            NameRef::Local(ref scoped_id) =>
                scoped_id.name(&self.context.scope_maps[self.map_index]),
            NameRef::Global(ref name) => name,
        }
    }

    pub fn get_var<'a>(&'a self, nameref: &NameRef) -> Option<&'a NameDef> {
        self.context.ids.get(&Id::from(self.map_index, nameref.clone()))
    }

    pub fn get_var_mut<'a>(&'a mut self, nameref: &NameRef) -> Option<&'a mut NameDef> {
        self.context.ids.get_mut(&Id::from(self.map_index, nameref.clone()))
    }

    pub fn get_frame<'a>(&'a self) -> &'a Frame {
        for scope in self.scopes.iter().rev() {
            if let Some(frame) = scope.get_frame() { return frame; }
        }
        self.context.global_scope().get_frame().expect("global scope lacks a frame")
    }

    pub fn get_frame_mut<'a>(&'a mut self) -> &'a mut Frame {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(frame) = scope.get_frame_mut() { return frame; }
        }
        self.context.global_scope_mut().get_frame_mut().expect("global scope lacks a frame")
    }

    pub fn get_vararg<'a>(&'a self) -> Option<&'a TySeq> {
        self.get_frame().vararg.as_ref()
    }

    pub fn get_string_meta(&self) -> Option<Spanned<Slot>> {
        self.context.get_string_meta()
    }

    // why do we need this special method?
    // conceptually, assigning to the placeholder `<prototype>.init` should give
    // the exact type for the `init` method, thus also a type for automatically created `new`.
    // in reality we instead get a type bound which cannot be exactly resolved;
    // we instead put [constructor] tag to the placeholder and
    // catch the exact type being assigned (method definitions guarantee the resolvability).
    fn create_new_method_from_init(&mut self, init: &Spanned<Slot>) -> Result<()> {
        // ensure that the type can be resolved...
        let ty = if let Some(ty) = self.resolve_exact_type(&init.unlift()) {
            ty
        } else {
            self.error(init, m::InexactInitMethod { init: self.display(init) }).done()?;
            return Ok(());
        };

        // ...and is a function...
        let mut func = match *ty {
            T::Functions(ref func) => match **func {
                Functions::Simple(ref f) => f.to_owned(),
                _ => {
                    self.error(init, m::OverloadedFuncInitMethod { init: self.display(init) })
                        .done()?;
                    return Ok(());
                }
            },
            _ => {
                self.error(init, m::NonFuncInitMethod { init: self.display(init) }).done()?;
                return Ok(());
            },
        };

        // ...and has the first argument readily known as a [constructible] class instance type.
        let mut cid = None;
        if !func.args.head.is_empty() {
            let selfarg = func.args.head.remove(0);
            if let Some(selfarg) = self.resolve_exact_type(&selfarg) {
                if selfarg.tag() == Some(Tag::Constructible) {
                    if let T::Class(Class::Instance(cid_)) = *selfarg {
                        cid = Some(cid_);
                    }
                }
            }
        }
        if let Some(cid) = cid {
            if !func.argnames.is_empty() {
                func.argnames.remove(0);
            }

            // now `init` is: function(/* removed: [constructible] <%cid> */, ...) -> any
            // fix the return type to make a signature for the `new` method
            let returns = T::Class(Class::Instance(cid));
            let ctor = Function { args: func.args, argnames: func.argnames,
                                  returns: Some(TySeq::from(returns)) };
            let ctor = Slot::new(F::Const, Ty::new(T::func(ctor)));

            debug!("implicitly setting the `new` method of {:?} as {:?}", cid, ctor);
            let cfields = self.context.get_class_fields_mut(cid).expect("invalid ClassId");
            let key = Key::Str(b"new"[..].into());
            let prevctor = cfields.class_ty.insert(key, ctor);
            assert_eq!(prevctor, None); // should have been prevented
        } else {
            self.error(init, m::BadSelfInInitMethod { init: self.display(init) }).done()?;
            return Ok(());
        }

        Ok(())
    }

    // returns false if the assignment is failed and constraints should not be added
    fn assign_special(&mut self, lhs: &Spanned<Slot>, rhs: &Spanned<Slot>) -> Result<bool> {
        match lhs.tag() {
            Some(b @ Tag::PackagePath) |
            Some(b @ Tag::PackageCpath) => {
                if let Some(s) = self.resolve_exact_type(&rhs.unlift())
                                     .and_then(|t| t.as_string().map(|s| s.to_owned())) {
                    let ret = if b == Tag::PackagePath {
                        self.opts.borrow_mut().set_package_path((&s[..]).with_loc(rhs), self)
                    } else {
                        self.opts.borrow_mut().set_package_cpath((&s[..]).with_loc(rhs), self)
                    };

                    // the implementation may have reported by its own
                    match ret {
                        Ok(()) => {}
                        Err(None) => {
                            self.warn(rhs, m::CannotAssignToPackagePath { name: b.name() }).done()?;
                            return Ok(false);
                        }
                        Err(Some(stop)) => {
                            return Err(stop);
                        }
                    }
                } else {
                    self.warn(rhs, m::UnknownAssignToPackagePath { name: b.name() }).done()?;
                }
            }

            Some(Tag::Constructible) => {
                self.error(lhs, m::SelfCannotBeAssignedInCtor {}).done()?;
                return Ok(false);
            }

            Some(Tag::Constructor) => {
                self.create_new_method_from_init(rhs)?;
            }

            _ => {}
        }

        Ok(true)
    }

    fn assign_(&mut self, lhs: &Spanned<Slot>, rhs: &Spanned<Slot>, init: bool) -> Result<()> {
        if self.assign_special(lhs, rhs)? {
            if lhs.accept(rhs, self.types(), init).is_err() {
                self.error(lhs, m::CannotAssign { lhs: self.display(lhs), rhs: self.display(rhs) })
                    .note_if(rhs, m::OtherTypeOrigin {})
                    .done()?;
            }
        }
        Ok(())
    }

    // same to Slot::accept but also able to handle the built-in semantics;
    // should be used for any kind of non-internal assignments.
    pub fn assign(&mut self, lhs: &Spanned<Slot>, rhs: &Spanned<Slot>) -> Result<()> {
        trace!("assigning {:?} to an existing slot {:?}", rhs, lhs);
        self.assign_(lhs, rhs, false)
    }

    // merge the initialization and optional type specification to produce a single slot type.
    fn assign_from_spec(&mut self, init: &Spanned<Slot>,
                        spec: Option<&SlotSpec>) -> Result<Spanned<Slot>> {
        match spec {
            // type spec is explicit: init <: spec
            Some(&SlotSpec::Explicit(ref spec)) => {
                self.assign_(spec, init, true)?;
                Ok(spec.clone())
            },

            // type spec is implicit, the type part is expected to be a type variable
            Some(&SlotSpec::Implicit(ref spec)) => {
                if self.assign_special(spec, init)? {
                    // ignore the flexibility
                    let lty = (*spec.unlift()).clone().with_loc(spec);
                    let rty = (*init.unlift()).clone().with_loc(init);
                    if let Err(r) = lty.assert_eq(&rty, self.types()) {
                        self.error(spec, m::CannotAssign { lhs: self.display(spec),
                                                           rhs: self.display(init) })
                            .note_if(init, m::OtherTypeOrigin {})
                            .report_types(r, TypeReportHint::None)
                            .done()?;
                    }
                }
                Ok(spec.clone())
            },

            // type spec is *not* given, use the coerced version of initialization
            None => Ok(init.clone().map(|s| s.coerce())),
        }
    }

    // same to `assign` but the slot is assumed to be newly created (out of field)
    // and the strict equality instead of subtyping is applied.
    // this is required because the slot itself is generated before doing any assignment;
    // the usual notion of accepting by subtyping does not work well here.
    // this is technically two assignments, of which the latter is done via the strict equality.
    pub fn assign_new(&mut self, lhs: &Spanned<Slot>, initrhs: &Spanned<Slot>,
                      specrhs: Option<&SlotSpec>) -> Result<()> {
        trace!("assigning {:?} to a new slot {:?} with type {:?}", initrhs, lhs, specrhs);

        // first assignment of initrhs to specrhs, if any
        let specrhs = self.assign_from_spec(initrhs, specrhs)?;

        // second "assignment" of specrhs (or initrhs) to lhs
        specrhs.adapt(lhs.flex(), self.types());
        if self.assign_special(lhs, &specrhs)? {
            if let Err(r) = lhs.assert_eq(&specrhs, self.types()) {
                self.error(lhs, m::CannotAssign { lhs: self.display(lhs),
                                                  rhs: self.display(&specrhs) })
                    .note_if(&specrhs, m::OtherTypeOrigin {})
                    .report_types(r, TypeReportHint::None)
                    .done()?;
            }
        }

        Ok(())
    }

    pub fn ensure_var(&mut self, nameref: &Spanned<NameRef>) -> Result<Slot> {
        trace!("ensuring {:?} has been initialized", nameref);
        let id = self.id_from_nameref(nameref);

        let defslot;
        {
            let def = self.context.ids.get_mut(&id);
            let mut def = def.expect("Env::ensure_var with an undefined var");
            match def.slot {
                NameSlot::None => {
                    // do not try to accept the slot again
                    let nil = Slot::just(Ty::noisy_nil());
                    def.slot = NameSlot::Set(nil.clone());
                    return Ok(nil);
                }
                NameSlot::Unset(ref slot) => {
                    // needs to be updated, but we hold the context right now.
                    // clone the slot and continue from the outside.
                    defslot = slot.clone().with_loc(def.span);
                }
                NameSlot::Set(ref slot) => {
                    return Ok(slot.clone());
                }
            }
        }

        // not yet set but typed (e.g. `local x --: string`), the type should accept `nil`
        let nil = Slot::just(Ty::noisy_nil()).without_loc();
        if self.assign_special(&defslot, &nil)? {
            if defslot.accept(&nil, self.types(), true).is_ok() { // this IS still initialization
                self.context.ids.get_mut(&id).unwrap().slot = NameSlot::Set(defslot.base.clone());
            } else {
                // won't alter the set flag, so subsequent uses are still errors
                self.error(&id, m::UseOfUnassignedVar {})
                    .note_if(&defslot, m::UnassignedVarOrigin { var: self.display(&defslot) })
                    .done()?;
            }
        }

        Ok(defslot.base)
    }

    fn name_class_if_any(&mut self, id: &Spanned<Id>, info: &Spanned<Slot>) -> Result<()> {
        match **info.unlift() {
            T::Class(Class::Prototype(cid)) => {
                let name = id.name(&self.context).clone().with_loc(id);

                // check if the name conflicts in the type namespace earlier
                // note that even when the type is defined in the global scope
                // we check both the local and global scope for the type name
                if let Some(def) = self.get_named_type(&name) {
                    self.error(&name, m::CannotRedefineTypeAsClass { name: &name.base })
                        .note(def.span, m::AlreadyDefinedType {})
                        .done()?;
                    return Ok(());
                }

                self.context.name_class(cid, name.clone())?;

                let scope = match id.base {
                    Id::Local(..) => self.current_scope_mut(),
                    Id::Global(..) => self.global_scope_mut(),
                };
                let ret = scope.put_type(name, Ty::new(T::Class(Class::Instance(cid))));
                assert!(ret, "failed to insert the type");
            }

            T::Union(ref u) => {
                // should raise an error if a prototype is used within a union
                let is_prototype = |&cls| if let Class::Prototype(_) = cls { true } else { false };
                if u.classes.iter().any(is_prototype) {
                    self.error(info, m::CannotNameUnknownClass { cls: self.display(info) })
                        .done()?;
                }
            }

            _ => {}
        }

        Ok(())
    }

    // adds a local variable with the explicit type `specinfo` and the implicit type `initinfo`.
    pub fn add_var(&mut self, nameref: &Spanned<NameRef>,
                   specinfo: Option<SlotSpec>,
                   initinfo: Option<Spanned<Slot>>) -> Result<()> {
        let id = self.id_from_nameref(nameref);
        debug!("adding a variable {} with {:?} (specified) and {:?} (initialized)",
               id.display(&self.context), specinfo, initinfo);

        // raise an error in any case that the nameref is defined with a spec multiple times.
        // practically this means that the global var has been redefined.
        // (local variables are created uniquely and unlikely to trigger this with a correct AST)
        if self.context.ids.get(&id).is_some() {
            let name = id.name(&self.context);
            self.error(nameref, m::CannotRedefineVar { name: name }).done()?;
            return Ok(());
        }

        let slot = if let Some(initinfo) = initinfo {
            let specinfo = self.assign_from_spec(&initinfo, specinfo.as_ref())?;

            // name the class if it is currently unnamed
            self.name_class_if_any(&id, &initinfo)?;

            let varname = id.name(self.context).clone().with_loc(nameref);
            let specinfo = specinfo.base.set_display(DisplayName::Var(varname));
            NameSlot::Set(specinfo)
        } else if let Some(specinfo) = specinfo {
            let varname = id.name(self.context).clone().with_loc(nameref);
            let specinfo = specinfo.unwrap().base.set_display(DisplayName::Var(varname));
            NameSlot::Unset(specinfo)
        } else {
            NameSlot::None
        };

        self.context.ids.insert(id.base, NameDef { span: id.span, slot: slot });
        Ok(())
    }

    pub fn add_local_var_already_set(&mut self, scoped_id: &Spanned<ScopedId>,
                                     info: Spanned<Slot>) -> Result<()> {
        let id = Id::Local(self.map_index, scoped_id.base.clone()).with_loc(scoped_id);
        debug!("adding a local variable {} already set to {:?}", id.display(&self.context), info);

        // we cannot blindly `accept` the `initinfo`, since it will discard the flexibility
        // (e.g. if the callee requests `F::Var`, we need to keep that).
        // therefore we just remap `F::Just` to `F::Var`.
        info.adapt(F::Var, self.types());

        self.name_class_if_any(&id, &info)?;

        let varname = id.name(self.context).clone().with_loc(scoped_id);
        let info = info.base.set_display(DisplayName::Var(varname));
        self.context.ids.insert(id.base, NameDef { span: id.span, slot: NameSlot::Set(info) });
        Ok(())
    }

    // assigns to a global or local variable with a right-hand-side type of `info`.
    // it may create a new global variable if there is no variable with that name.
    pub fn assign_to_var(&mut self, nameref: &Spanned<NameRef>, info: Spanned<Slot>) -> Result<()> {
        let id = self.id_from_nameref(nameref);

        let (previnfo, prevset, needslotassign) = if self.context.ids.contains_key(&id.base) {
            let mut def = self.context.ids.get_mut(&id.base).unwrap();
            let (previnfo, prevset, needslotassign) = match def.slot {
                NameSlot::None => (info.base.clone(), false, false),
                NameSlot::Unset(ref slot) => (slot.clone(), false, true),
                NameSlot::Set(ref slot) => (slot.clone(), true, true),
            };
            def.slot = NameSlot::Set(previnfo.clone());
            (previnfo, prevset, needslotassign)
        } else {
            let varname = id.name(self.context).clone().with_loc(nameref);
            let info = info.base.clone().set_display(DisplayName::Var(varname));
            self.context.ids.insert(id.base.clone(),
                                    NameDef { span: id.span, slot: NameSlot::Set(info.clone()) });
            (info, true, true)
        };
        debug!("assigning {:?} to a variable {} with type {:?}",
               info, id.display(&self.context), previnfo);

        if needslotassign {
            self.assign_(&previnfo.with_loc(&id), &info, !prevset)?;
        }
        self.name_class_if_any(&id, &info)?;
        Ok(())
    }

    fn assume_special(&mut self, info: &Spanned<Slot>) -> Result<()> {
        match info.tag() {
            Some(Tag::StringMeta) => {
                if let Some(ref prevmeta) = self.context.string_meta {
                    // while it is possible to alter the string metatable from C,
                    // we don't think that it is useful after the initialization.
                    self.error(info, m::CannotRedefineStringMeta {})
                        .note(prevmeta, m::PreviousStringMeta {})
                        .done()?;
                }
                self.context.string_meta = Some(info.clone());
            }
            _ => {}
        }

        Ok(())
    }

    pub fn assume_var(&mut self, name: &Spanned<NameRef>, info: Spanned<Slot>) -> Result<()> {
        let id = Id::from(self.map_index, name.base.clone());
        debug!("(force) adding a variable {} as {:?}", id.display(&self.context), info);

        self.assume_special(&info)?;

        let varname = id.name(self.context).clone().with_loc(name);
        let info = info.base.set_display(DisplayName::Var(varname));

        let mut def = self.context.ids.entry(id).or_insert_with(|| {
            NameDef { span: name.span, slot: NameSlot::None }
        });
        def.slot = NameSlot::Set(info);

        Ok(())
    }

    pub fn get_tvar_bounds(&self, tvar: TVar) -> (Flags /*lb*/, Flags /*ub*/) {
        self.context.get_tvar_bounds(tvar)
    }

    pub fn get_tvar_exact_type(&self, tvar: TVar) -> Option<Ty> {
        self.context.get_tvar_exact_type(tvar)
    }

    pub fn get_named_global_type<'a>(&'a self, name: &Name) -> Option<&'a TypeDef> {
        self.context.global_scope().get_type(name)
    }

    pub fn get_named_local_type<'a>(&'a self, name: &Name) -> Option<&'a TypeDef> {
        for scope in self.scopes.iter().rev() {
            if let Some(def) = scope.get_type(name) { return Some(def); }
        }
        None
    }

    pub fn get_named_type<'a>(&'a self, name: &Name) -> Option<&'a TypeDef> {
        self.get_named_local_type(name).or_else(|| self.get_named_global_type(name))
    }

    pub fn define_local_type(&mut self, name: &Spanned<Name>, ty: Ty) -> Result<()> {
        if let Some(def) = self.get_named_local_type(name) {
            self.error(name, m::CannotRedefineLocalType { name: &name.base })
                .note(def.span, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        } else if let Some(def) = self.get_named_global_type(name) {
            self.error(name, m::CannotRedefineGlobalType { name: &name.base })
                .note(def.span, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        }

        let ty = ty.and_display(DisplayName::Type(name.clone()));
        let ret = self.current_scope_mut().put_type(name.clone(), ty);
        assert!(ret, "failed to insert the type");
        Ok(())
    }

    pub fn define_global_type(&mut self, name: &Spanned<Name>, ty: Ty) -> Result<()> {
        if let Some(def) = self.get_named_local_type(name) {
            self.error(name, m::CannotRedefineLocalTypeAsGlobal { name: &name.base })
                .note(def.span, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        } else if let Some(def) = self.get_named_global_type(name) {
            self.error(name, m::CannotRedefineGlobalType { name: &name.base })
                .note(def.span, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        }

        let ty = ty.and_display(DisplayName::Type(name.clone()));
        let ret = self.global_scope_mut().put_type(name.clone(), ty);
        assert!(ret, "failed to insert the type");
        Ok(())
    }

    pub fn define_and_export_type(&mut self, name: &Spanned<Name>, ty: Ty) -> Result<()> {
        if let Some(def) = self.get_named_type(name) {
            self.error(name, m::CannotRedefineAndReexportType { name: &name.base })
                .note(def.span, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        }

        // insert to the exported types (distinct from scoped types)
        let defspan = match self.exported_types.entry(name.base.clone()) {
            hash_map::Entry::Vacant(e) => {
                e.insert(TypeDef { ty: ty.clone(), span: name.span });
                None
            },
            hash_map::Entry::Occupied(e) => Some(e.get().span),
        };
        if let Some(defspan) = defspan {
            // if the parser has been working correctly this should be impossible,
            // because a set of exported types should be a subset of top-level local types.
            // therefore this error can only occur when the AST is not well-formed.
            self.error(name, m::CannotRedefineAndReexportType { name: &name.base })
                .note(defspan, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        }

        // insert a locally scoped type
        let ty = ty.and_display(DisplayName::Type(name.clone()));
        let ret = self.current_scope_mut().put_type(name.clone(), ty);
        assert!(ret, "failed to insert the type");
        Ok(())
    }

    pub fn redefine_global_type(&mut self, name: &Spanned<Name>, tyspan: Span) -> Result<()> {
        let ty = if let Some(def) = self.get_named_local_type(name) {
            def.ty.clone()
        } else if let Some(def) = self.get_named_global_type(name) {
            self.error(name, m::CannotRedefineGlobalType { name: &name.base })
                .note(def.span, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        } else {
            self.error(tyspan, m::NoType { name: &name.base }).done()?;
            Ty::dummy()
        };

        let ret = self.global_scope_mut().put_type(name.clone(), ty);
        assert!(ret, "failed to insert the type");
        Ok(())
    }

    pub fn reexport_local_type(&mut self, name: &Spanned<Name>, tyspan: Span) -> Result<()> {
        let ty = if let Some(def) = self.get_named_local_type(name) {
            def.ty.clone()
        } else if let Some(def) = self.get_named_global_type(name) {
            self.error(name, m::CannotRedefineGlobalType { name: &name.base })
                .note(def.span, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        } else {
            self.error(tyspan, m::NoType { name: &name.base }).done()?;
            Ty::dummy()
        };

        // insert to the exported types
        let defspan = match self.exported_types.entry(name.base.clone()) {
            hash_map::Entry::Vacant(e) => {
                e.insert(TypeDef { ty: ty.clone(), span: name.span });
                None
            },
            hash_map::Entry::Occupied(e) => Some(e.get().span),
        };
        if let Some(defspan) = defspan {
            // see `Env::define_and_export_type` for the possibility of this case
            self.error(name, m::CannotReexportType { name: &name.base })
                .note(defspan, m::AlreadyDefinedType {})
                .done()?;
            return Ok(());
        }

        Ok(())
    }

    pub fn import_types(&mut self, typedefs: Spanned<HashMap<Name, TypeDef>>) -> Result<()> {
        for (name, def) in typedefs.base.into_iter() {
            if let Some(prevdef) = self.get_named_type(&name) {
                self.error(typedefs.span, m::CannotImportAlreadyDefinedType { name: &name })
                    .note(prevdef.span, m::AlreadyDefinedType {})
                    .done()?;
                return Ok(());
            }

            let ret = self.current_scope_mut().put_type(name.with_loc(def.span), def.ty);
            assert!(ret, "failed to insert the type");
        }

        Ok(())
    }
}

impl<'ctx, R: Report> Report for Env<'ctx, R> {
    fn message_locale(&self) -> Locale {
        self.context.report.message_locale()
    }

    fn add_span(&self, k: Kind, s: Span, m: &Localize) -> Result<()> {
        self.context.report.add_span(k, s, m)
    }
}

impl<'ctx, R: Report> TypeResolver for Env<'ctx, R> {
    fn context(&self) -> &TypeContext {
        &self.context.types
    }

    fn context_mut(&mut self) -> &mut TypeContext {
        &mut self.context.types
    }

    fn ty_from_name(&self, name: &Spanned<Name>) -> Result<Ty> {
        if let Some(def) = self.get_named_type(name) {
            Ok(def.ty.clone())
        } else {
            self.error(name, m::NoType { name: &name.base }).done()?;
            Ok(Ty::dummy())
        }
    }
}

#[test]
fn test_context_is_send_and_sync() {
    use kailua_diag::NoReport;

    fn _assert_send<T: Send>(_x: T) {}
    fn _assert_sync<T: Sync>(_x: T) {}

    _assert_send(Context::new(NoReport));
    _assert_sync(Context::new(NoReport));
}

