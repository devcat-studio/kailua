// TODO many routines should be "generic" enough to be refactored

use std::fmt;
use std::collections::{HashSet, HashMap};
use std::collections::hash_map::Entry;
use vec_map::{self, VecMap};
use parking_lot::RwLock;

use kailua_env::{Span, Spanned};
use kailua_diag::{self, Report, Reporter};
use kailua_syntax::Name;
use kailua_types::ty::{TypeContext, ClassSystemId, ClassId, Class, DisplayState};
use kailua_types::ty::{Slot, Key, T, Ty, F};
use message as m;
use super::ClassSystem;

#[derive(Clone, Debug)]
struct ClassDef {
    class_fields: HashMap<Key, Slot>,
    instance_fields: HashMap<Key, Slot>,
}

impl ClassDef {
    fn fields(&self, proto: bool) -> &HashMap<Key, Slot> {
        if proto { &self.class_fields } else { &self.instance_fields }
    }

    fn fields_mut(&mut self, proto: bool) -> &mut HashMap<Key, Slot> {
        if proto { &mut self.class_fields } else { &mut self.instance_fields }
    }
}

/// A "dumb" class system which does not support any special methods or inheritance.
#[derive(Debug)]
pub struct DumbClassSystem {
    classes: RwLock<Vec<ClassDef>>,

    // frequently read even when classes are locked, so has to be separated
    class_names: RwLock<VecMap<Spanned<Name>>>,
}

impl DumbClassSystem {
    pub fn new() -> DumbClassSystem {
        DumbClassSystem {
            classes: RwLock::new(Vec::new()),
            class_names: RwLock::new(VecMap::new()),
        }
    }
}

impl ClassSystem for DumbClassSystem {
    fn assume_class(&self, self_csid: ClassSystemId, parent: Option<Spanned<ClassId>>,
                    _outerspan: Span, _ctx: &mut TypeContext,
                    report: &Report) -> kailua_diag::Result<Option<ClassId>> {
        if let Some(ref parent) = parent {
            report.error(parent, m::NoInheritanceInDumbClassSystem {}).done()?;
        }

        let mut classes = self.classes.write();
        let cid = ClassId(self_csid, classes.len() as u32);
        classes.push(ClassDef {
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
        lhs == rhs
    }

    fn index_rval(&self, cls: Class, key: Spanned<&Key>, _expspan: Span,
                  _ctx: &mut TypeContext, _report: &Report) -> kailua_diag::Result<Option<Slot>> {
        let (proto, cid) = match cls {
            Class::Prototype(cid) => (true, cid),
            Class::Instance(cid) => (false, cid),
        };

        let classes = self.classes.read();
        let cls = &classes[cid.1 as usize];
        if !proto {
            if let Some(info) = cls.instance_fields.get(&key) {
                return Ok(Some(info.clone()));
            }
        }
        if let Some(info) = cls.class_fields.get(&key) {
            return Ok(Some(info.clone()));
        }
        Ok(None)
    }

    fn index_lval(&self, cls: Class, key: Spanned<&Key>, _expspan: Span,
                  hint: Option<&Slot>, ctx: &mut TypeContext,
                  _report: &Report) -> kailua_diag::Result<Option<(bool, Slot)>> {
        let (proto, cid) = match cls {
            Class::Prototype(cid) => (true, cid),
            Class::Instance(cid) => (false, cid),
        };

        let mut classes = self.classes.write();
        let cls = &mut classes[cid.1 as usize];
        match cls.fields_mut(proto).entry(key.base.clone()) {
            Entry::Occupied(e) => Ok(Some((false, e.get().clone()))),
            Entry::Vacant(e) => {
                let slot = if let Some(hint) = hint {
                    let slot = hint.clone();
                    slot.adapt(F::Var, ctx); // always adapt to Var
                    slot
                } else {
                    let tvar = T::TVar(ctx.gen_tvar());
                    Slot::new(F::Unknown, Ty::new(tvar))
                };
                e.insert(slot.clone());
                Ok(Some((true, slot)))
            },
        }
    }

    fn fmt_class(&self, cid: ClassId, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        let names = self.class_names.read();
        match (&st.locale[..], &names.get(cid.1 as usize)) {
            (_,    &Some(ref name)) => write!(f, "{:+}", name),
            ("ko", &None) => write!(f, "이름 없는 클래스 #{}.{}", (cid.0).0, cid.1),
            (_,    &None) => write!(f, "unnamed class #{}.{}", (cid.0).0, cid.1),
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
            let cls = &classes[cid.1 as usize];
            for (key, slot) in cls.fields(proto) {
                if seen.insert(key) {
                    f(key, slot)?;
                }
            }
            Ok(())
        };

        if !proto {
            list(false)?;
        }
        list(true)
    }

    fn list_parents(&self, _cid: ClassId,
                    _f: &mut FnMut(ClassId) -> Result<(), ()>) -> Result<(), ()> {
        Ok(())
    }
}

