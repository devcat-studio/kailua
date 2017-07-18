//! Type environments.

use std::mem;
use std::str;
use std::fmt;
use std::collections::{HashMap, HashSet};
use vec_map::{self, VecMap};
use atomic::Atomic;
use atomic::Ordering::Relaxed;

use kailua_env::Spanned;
use kailua_diag::Locale;
use kailua_syntax::Name;
use diag::{Origin, TypeReport, TypeResult};
use ty::{Ty, T, Slot, TVar, RVar, Lattice};
use ty::{TypeContext, ClassId, Key, DisplayState};
use ty::flags::*;
use self::partitions::{Partition, Partitions};

mod partitions;

#[derive(Debug)]
struct Bound {
    parent: Atomic<u32>,
    rank: u8,
    bound: Option<Ty>,
}

// a set of constraints that can be organized as a tree
#[derive(Debug)]
struct Constraints {
    op: &'static str,
    bounds: Partitions<Box<Bound>>,
}

// is this bound trivial so that one can always overwrite?
fn is_bound_trivial(t: &Option<Ty>) -> bool {
    // TODO special casing ? is not enough, should resolve b.bound's inner ?s as well
    if let Some(ref t) = *t {
        match **t { T::None | T::Dynamic(_) => true, _ => false }
    } else {
        true
    }
}

impl Partition for Box<Bound> {
    fn create(parent: usize, rank: usize) -> Box<Bound> {
        Box::new(Bound { parent: Atomic::new(parent as u32), rank: rank as u8, bound: None })
    }

    fn read(&self) -> (usize /*parent*/, usize /*rank*/) {
        (self.parent.load(Relaxed) as usize, self.rank as usize)
    }

    fn write_parent(&self, parent: usize) {
        self.parent.store(parent as u32, Relaxed);
    }

    fn increment_rank(&mut self) {
        self.rank += 1;
    }
}

impl Constraints {
    fn new(op: &'static str) -> Constraints {
        Constraints { op: op, bounds: Partitions::new() }
    }

    fn is(&self, lhs: TVar, rhs: TVar) -> bool {
        lhs == rhs || self.bounds.find(lhs.0 as usize) == self.bounds.find(rhs.0 as usize)
    }

    fn get_bound<'a>(&'a self, lhs: TVar) -> Option<&'a Bound> {
        let lhs = self.bounds.find(lhs.0 as usize);
        self.bounds.get(lhs).map(|b| &**b)
    }

    // Some(bound) indicates that the bound already exists and is not consistent to rhs
    fn add_bound<'a>(&'a mut self, lhs: TVar, rhs: &Ty) -> Option<&'a Ty> {
        let lhs_ = self.bounds.find(lhs.0 as usize);
        let b = self.bounds.entry(lhs_).or_insert_with(|| Partition::create(lhs_, 0));
        if is_bound_trivial(&b.bound) {
            b.bound = Some(rhs.clone());
        } else {
            let lhsbound = b.bound.as_ref().unwrap();
            if lhsbound != rhs { return Some(lhsbound); }
        }
        None
    }

    fn add_relation(&mut self, lhs: TVar, rhs: TVar) -> bool {
        if lhs == rhs { return true; }

        let lhs_ = self.bounds.find(lhs.0 as usize);
        let rhs_ = self.bounds.find(rhs.0 as usize);
        if lhs_ == rhs_ { return true; }

        fn take_bound(bounds: &mut VecMap<Box<Bound>>, i: usize) -> Option<Ty> {
            if let Some(b) = bounds.get_mut(i) {
                mem::replace(&mut b.bound, None)
            } else {
                None
            }
        }

        // take the bounds from each representative variable.
        let lhsbound = take_bound(&mut self.bounds, lhs_);
        let rhsbound = take_bound(&mut self.bounds, rhs_);

        let bound = match (is_bound_trivial(&lhsbound), is_bound_trivial(&rhsbound)) {
            (false, _) => lhsbound,
            (true, false) => rhsbound,
            (true, true) if lhsbound == rhsbound => lhsbound,
            (true, true) => {
                info!("variables {:?}/{:?} cannot have multiple bounds \
                       (left {} {:?}, right {} {:?})",
                      lhs, rhs, self.op, lhsbound, self.op, rhsbound);
                return false;
            },
        };

        // update the shared bound to the merged representative
        let new = self.bounds.union(lhs_, rhs_);
        if !is_bound_trivial(&bound) {
            // the merged entry should have non-zero rank, so unwrap() is fine
            self.bounds.get_mut(new).unwrap().bound = bound;
        }

        true
    }
}

#[derive(Debug)]
struct RowInfo {
    // the hashmap being None indicates that it is currently recursing;
    // the value can be Some(slot) for "positive" fields, which the row variable contains that key,
    // or None for "negative" fields, which the variable _cannot_ contain that key
    // and it's an error for the unification to introduce it.
    fields: Option<HashMap<Key, Option<Slot>>>,

    // when the next variable is None, it is not instantiated yet and
    // implicitly thought to have negative (absent) fields for each key in fields
    next: Option<RVar>,
}

impl RowInfo {
    fn new() -> RowInfo {
        RowInfo { fields: Some(HashMap::new()), next: None }
    }
}

#[derive(Debug)]
struct ClassDef {
    name: Option<Spanned<Name>>,
    parent: Option<ClassId>,
}

/// The type environment.
pub struct Types {
    message_locale: Locale,

    // type variable information
    next_tvar: TVar,
    tvar_sub: Constraints, // upper bound
    tvar_sup: Constraints, // lower bound
    tvar_eq: Constraints, // tight bound

    // row variable information
    next_rvar: RVar,
    row_infos: VecMap<Box<RowInfo>>,

    // classes defined (only has shallow informations here)
    classes: Vec<ClassDef>,
}

impl Types {
    /// Creates a new fresh type environment.
    pub fn new(locale: Locale) -> Types {
        Types {
            message_locale: locale,
            next_tvar: TVar(1), // TVar(0) for the top-level return
            tvar_sub: Constraints::new("<:"),
            tvar_sup: Constraints::new(":>"),
            tvar_eq: Constraints::new("="),
            next_rvar: RVar::new(1), // RVar::new(0) == RVar::empty()
            row_infos: VecMap::new(),
            classes: Vec::new(),
        }
    }

    /// Returns the current message locale.
    pub fn locale(&self) -> Locale {
        self.message_locale
    }

    /// Sets the current message locale.
    pub fn set_locale(&mut self, locale: Locale) {
        self.message_locale = locale;
    }

    /// Registers a new class (a nominal instantiable type) with an optional parent class.
    pub fn make_class(&mut self, parent: Option<ClassId>) -> ClassId {
        assert!(parent.map_or(true, |cid| (cid.0 as usize) < self.classes.len()));

        let cid = ClassId(self.classes.len() as u32);
        self.classes.push(ClassDef { name: None, parent: parent });
        cid
    }

    /// Names an existing class with given name.
    ///
    /// Returns an `Err` with a previous nmae when the same class is named twice.
    /// The span is mostly used to give an appropriate error in this case.
    pub fn name_class(&mut self, cid: ClassId, name: Spanned<Name>) -> Result<(), &Spanned<Name>> {
        let cls = &mut self.classes[cid.0 as usize];
        if let Some(ref prevname) = cls.name {
            Err(prevname)
        } else {
            info!("named {:?} as {:?}", cid, name);
            cls.name = Some(name);
            Ok(())
        }
    }

    /// Returns a parent class of given class, if any.
    pub fn get_parent_class(&self, cid: ClassId) -> Option<ClassId> {
        self.classes.get(cid.0 as usize).and_then(|cls| cls.parent)
    }

    fn assert_rvar_rel(&mut self, lhs: RVar, rhs: RVar, is_sub: bool) -> TypeResult<()> {
        trace!("{:?} should be {} {:?}", lhs, if is_sub { "<:" } else { "=" }, rhs);

        // we want to avoid modifying shared row variables (which we cannot distinguish,
        // therefore we can only modify a row variable which next var is not yet instantiated),
        // while making sure that all fields known to be present or absent in both operands
        // are common to each other as well. we therefore have to go through a chain of
        // row variables for each operand, collect positive and negative fields respectively,
        // set the bounds from mismatching fields to the last row variable in the chain
        // _and_ finally set the next row variables for both to the same row variable.
        //
        // this is going to be tough.

        fn collect_fields(mut r: RVar, ctx: &mut Types) -> TypeResult<(HashMap<Key, Slot>, RVar)> {
            let mut present = HashMap::new();
            let mut absent = HashSet::new(); // only used for filling the last row if none

            #[derive(Clone)]
            enum E { Recursive, Duplicate(Key) }

            let err;
            'err: loop {
                if r == RVar::empty() {
                    return Ok((present, r));
                }

                match ctx.row_infos.entry(r.to_usize()) {
                    vec_map::Entry::Occupied(row) => {
                        let row = row.get();

                        let fields = if let Some(ref fields) = row.fields {
                            fields
                        } else {
                            err = E::Recursive;
                            break 'err;
                        };

                        for (k, v) in fields {
                            if let Some(ref v) = *v {
                                // positive field should be unique
                                if present.insert(k.clone(), v.clone()).is_some() {
                                    err = E::Duplicate(k.clone());
                                    break 'err;
                                }
                            } else {
                                // negative field will *not* be unique
                                absent.insert(k.clone());
                            }
                        }

                        if let Some(ref next) = row.next {
                            r = next.clone();
                        } else {
                            return Ok((present, r));
                        }
                    }

                    vec_map::Entry::Vacant(e) => {
                        // instantiate the row and implicitly fill negative fields
                        let mut fields = HashMap::new();
                        fields.extend(present.iter().map(|(k, _)| (k.clone(), None)));
                        fields.extend(absent.into_iter().map(|k| (k, None)));
                        e.insert(Box::new(RowInfo { fields: Some(fields), next: None }));
                        return Ok((present, r));
                    }
                }
            }

            match err {
                E::Recursive => Err(ctx.gen_report().recursive_record()),
                E::Duplicate(key) => Err(ctx.gen_report().record_duplicate_key(&key)),
            }
        };

        if lhs == rhs {
            return Ok(());
        }

        let (lfields, lnext) = collect_fields(lhs.clone(), self)?;
        let (rfields, rnext) = collect_fields(rhs.clone(), self)?;

        let mut matching = Vec::new();
        let mut lmissing = Vec::new();
        let mut rmissing = Vec::new();

        // collect missing fields and matching fields
        // (we cannot immediately check for them due to borrowing)
        for (k, lv) in lfields.iter() {
            if let Some(rv) = rfields.get(k) {
                matching.push((k.clone(), lv.clone(), rv.clone()));
            } else {
                rmissing.push((k.clone(), lv.clone()));
            }
        }
        for (k, rv) in rfields.iter() {
            if !lfields.contains_key(k) {
                lmissing.push((k.clone(), rv.clone()));
            }
        }

        // to make tests reproducible :-)
        matching.sort_by(|a, b| a.0.cmp(&b.0));
        lmissing.sort_by(|a, b| a.0.cmp(&b.0));
        rmissing.sort_by(|a, b| a.0.cmp(&b.0));

        // check for the matching fields
        if is_sub {
            for (_k, lv, rv) in matching {
                lv.assert_sub(&rv, self)?;
            }
        } else {
            for (_k, lv, rv) in matching {
                lv.assert_eq(&rv, self)?;
            }
        }

        // remaining fields should be in the relevant updatable row variable
        if !lmissing.is_empty() {
            self.assert_rvar_includes_(lnext.clone(), &lmissing, false)?;
        }
        if !rmissing.is_empty() {
            self.assert_rvar_includes_(rnext.clone(), &rmissing, is_sub)?;
        }

        // finally two row variables should be linked by setting the common last row variable
        let last = self.gen_rvar();
        if lnext != RVar::empty() {
            let mut row =
                self.row_infos.entry(lnext.to_usize()).or_insert_with(|| Box::new(RowInfo::new()));
            assert_eq!(row.next, None);
            row.next = Some(last.clone());
        }
        if rnext != RVar::empty() && rnext != lnext {
            // avoid setting the next twice, which breaks the assertion
            let mut row =
                self.row_infos.entry(rnext.to_usize()).or_insert_with(|| Box::new(RowInfo::new()));
            assert_eq!(row.next, None);
            row.next = Some(last);
        }

        Ok(())
    }

    fn assert_rvar_includes_(&mut self, lhs: RVar, includes: &[(Key, Slot)],
                             nilable: bool) -> TypeResult<()> {
        trace!("{:?} should include {:?} ({} nil)",
               lhs, includes, if nilable { "allows" } else { "disallows" });

        // optimize a no-op just in case
        if includes.is_empty() {
            return Ok(());
        }

        if lhs == RVar::empty() {
            return Err(self.gen_report().inextensible_record());
        }

        let lhs_ = lhs.to_usize();

        // take fields out, so that we can detect an infinite recursion
        let fields_and_next = {
            let row = self.row_infos.entry(lhs_).or_insert_with(|| Box::new(RowInfo::new()));
            row.fields.take().map(|fields| (fields, row.next.clone()))
        };
        let (mut fields, next) = if let Some(fields_and_next) = fields_and_next {
            fields_and_next
        } else {
            return Err(self.gen_report().recursive_record());
        };
        trace!("{:?} already had {:?} and {:?}", lhs, fields, next);

        let e = inner(self, lhs, includes, nilable, &mut fields, next);
        self.row_infos.get_mut(lhs_).unwrap().fields = Some(fields);
        return e;

        fn inner(ctx: &mut Types, _lhs: RVar, includes: &[(Key, Slot)], nilable: bool,
                 fields: &mut HashMap<Key, Option<Slot>>, next: Option<RVar>) -> TypeResult<()> {
            // collect missing fields, whether positive or negative, and
            // check if other matching fields are compatible
            let mut missing = Vec::new();
            for &(ref k, ref rv) in includes {
                match fields.get(k) {
                    Some(&Some(ref lv)) => {
                        // the existing fields should be compatible
                        rv.assert_sub(lv, ctx)?;
                    }
                    Some(&None) => {
                        // the field is excluded, immediately fail
                        return Err(ctx.gen_report().record_cannot_have_key(k));
                    }
                    None => {
                        // the field should be added to the next row variable (if any)
                        missing.push((k.clone(), rv.clone()));
                    }
                }
            }

            // if we have missing fields they should be in the next row variable if any;
            // we can avoid instantiation when it has not yet been instantiated though
            if let Some(next) = next {
                // we need to put fields back, so this cannot be a tail recursion
                ctx.assert_rvar_includes_(next, &missing, nilable)?;
            } else {
                for (k, v) in missing.into_iter() {
                    // when the record is extended due to the (in)equality relation,
                    // the type that is not explicitly nilable is disallowed
                    if nilable || v.unlift().can_omit() {
                        fields.insert(k, Some(v));
                    } else {
                        return Err(ctx.gen_report().record_extended_with_non_nil(&k, v, ctx));
                    }
                }
            }

            Ok(())
        }
    }
}

impl TypeContext for Types {
    fn gen_report(&self) -> TypeReport {
        TypeReport::new(self.message_locale)
    }

    fn last_tvar(&self) -> Option<TVar> {
        let tvar = self.next_tvar;
        if tvar == TVar(0) { None } else { Some(TVar(tvar.0 - 1)) }
    }

    fn gen_tvar(&mut self) -> TVar {
        self.next_tvar.0 += 1;
        self.next_tvar
    }

    fn copy_tvar(&mut self, tvar: TVar) -> TVar {
        if self.tvar_eq.get_bound(tvar).map_or(false, |b| b.bound.is_some()) {
            // we have an equal bound, so tvar has no chance to be extended
            trace!("copying {:?} is a no-op", tvar);
            tvar
        } else {
            let tvar_ = self.gen_tvar();
            trace!("copied {:?} to {:?}", tvar, tvar_);
            if let Some(ub) = self.tvar_sub.get_bound(tvar).and_then(|b| b.bound.clone()) {
                let oldub = self.tvar_sub.add_bound(tvar_, &ub);
                assert!(oldub.is_none(), "bounding fresh tvar should not fail");
            }
            if let Some(lb) = self.tvar_sup.get_bound(tvar).and_then(|b| b.bound.clone()) {
                let oldlb = self.tvar_sup.add_bound(tvar_, &lb);
                assert!(oldlb.is_none(), "bounding fresh tvar should not fail");
            }
            tvar_
        }
    }

    fn assert_tvar_sub(&mut self, lhs: TVar, rhs0: &Ty) -> TypeResult<()> {
        let rhs = rhs0.clone().coerce();
        debug!("adding a constraint {:?} <: {:?} (coerced to {:?})", lhs, rhs0, rhs);
        if let Some(eb) = self.tvar_eq.get_bound(lhs).and_then(|b| b.bound.clone()) {
            eb.assert_sub(&rhs, self)?;
        } else {
            if let Some(ub) = self.tvar_sub.add_bound(lhs, &rhs).map(|b| b.clone()) {
                // the original bound is not consistent, bound <: rhs still has to hold
                if let Err(e) = ub.assert_sub(&rhs, self) {
                    info!("variable {:?} cannot have multiple possibly disjoint \
                           bounds (original <: {:?}, later <: {:?}): {:?}", lhs, ub, rhs, e);
                    return Err(e);
                }
            }
            if let Some(lb) = self.tvar_sup.get_bound(lhs).and_then(|b| b.bound.clone()) {
                lb.assert_sub(&rhs, self)?;
            }
        }
        Ok(())
    }

    fn assert_tvar_sup(&mut self, lhs: TVar, rhs: &Ty) -> TypeResult<()> {
        // no coercion here, as type coercion will always expand the type
        debug!("adding a constraint {:?} :> {:?}", lhs, rhs);
        if let Some(eb) = self.tvar_eq.get_bound(lhs).and_then(|b| b.bound.clone()) {
            rhs.assert_sub(&eb, self)?;
        } else {
            if let Some(lb) = self.tvar_sup.add_bound(lhs, rhs).map(|b| b.clone()) {
                // the original bound is not consistent, bound :> rhs still has to hold
                if let Err(e) = rhs.assert_sub(&lb, self) {
                    info!("variable {:?} cannot have multiple possibly disjoint \
                           bounds (original :> {:?}, later :> {:?}): {:?}", lhs, lb, rhs, e);
                    return Err(e);
                }
            }
            if let Some(ub) = self.tvar_sub.get_bound(lhs).and_then(|b| b.bound.clone()) {
                rhs.assert_sub(&ub, self)?;
            }
        }
        Ok(())
    }

    fn assert_tvar_eq(&mut self, lhs: TVar, rhs0: &Ty) -> TypeResult<()> {
        let rhs = rhs0.clone().coerce();
        debug!("adding a constraint {:?} = {:?} (coerced to {:?})", lhs, rhs0, rhs);
        if let Some(eb) = self.tvar_eq.add_bound(lhs, &rhs).map(|b| b.clone()) {
            // the original bound is not consistent, bound = rhs still has to hold
            if let Err(e) = eb.assert_eq(&rhs, self) {
                info!("variable {:?} cannot have multiple possibly disjoint \
                       bounds (original = {:?}, later = {:?}): {:?}", lhs, eb, rhs, e);
                return Err(e);
            }
        } else {
            if let Some(ub) = self.tvar_sub.get_bound(lhs).and_then(|b| b.bound.clone()) {
                rhs.assert_sub(&ub, self)?;
            }
            if let Some(lb) = self.tvar_sup.get_bound(lhs).and_then(|b| b.bound.clone()) {
                lb.assert_sub(&rhs, self)?;
            }
        }
        Ok(())
    }

    fn assert_tvar_sub_tvar(&mut self, lhs: TVar, rhs: TVar) -> TypeResult<()> {
        debug!("adding a constraint {:?} <: {:?}", lhs, rhs);
        if !self.tvar_eq.is(lhs, rhs) {
            if !self.tvar_sub.add_relation(lhs, rhs) {
                // TODO
                return Err(self.gen_report().not_sub(Origin::TVar, "<tvar>", "<tvar>", self));
            }
            if !self.tvar_sup.add_relation(rhs, lhs) {
                // TODO
                return Err(self.gen_report().not_sub(Origin::TVar, "<tvar>", "<tvar>", self));
            }
        }
        Ok(())
    }

    fn assert_tvar_eq_tvar(&mut self, lhs: TVar, rhs: TVar) -> TypeResult<()> {
        debug!("adding a constraint {:?} = {:?}", lhs, rhs);
        // do not update tvar_sub & tvar_sup, tvar_eq will be consulted first
        if !self.tvar_eq.add_relation(lhs, rhs) {
            // TODO
            return Err(self.gen_report().not_eq(Origin::TVar, "<tvar>", "<tvar>", self));
        }
        Ok(())
    }

    fn get_tvar_bounds(&self, tvar: TVar) -> (Flags /*lb*/, Flags /*ub*/) {
        if let Some(b) = self.tvar_eq.get_bound(tvar).and_then(|b| b.bound.as_ref()) {
            let flags = b.flags();
            (flags, flags)
        } else {
            let lb = self.tvar_sup.get_bound(tvar).and_then(|b| b.bound.as_ref())
                                                  .map_or(T_NONE, |b| b.flags());
            let ub = self.tvar_sub.get_bound(tvar).and_then(|b| b.bound.as_ref())
                                                  .map_or(!T_NONE, |b| b.flags());
            assert_eq!(lb & !ub, T_NONE);
            (lb, ub)
        }
    }

    fn get_tvar_exact_type(&self, tvar: TVar) -> Option<Ty> {
        self.tvar_eq.get_bound(tvar).and_then(|b| b.bound.as_ref()).cloned()
    }

    fn gen_rvar(&mut self) -> RVar {
        let rvar = self.next_rvar.clone();
        self.next_rvar = RVar::new(rvar.to_usize() + 1);
        rvar
    }

    fn copy_rvar(&mut self, rvar0: RVar) -> RVar {
        let mut fields = HashMap::new();
        let mut rvar = rvar0.clone();

        loop {
            if rvar == RVar::empty() {
                // the original rvar has no chance to be extended, so we can safely return that
                return rvar0;
            }

            if let Some(info) = self.row_infos.get(rvar.to_usize()) {
                for (k, v) in info.fields.as_ref().unwrap().iter() {
                    if let Some(ref v) = *v {
                        // positive fields can overwrite negative fields
                        let prev = fields.insert(k.clone(), Some(v.clone()));
                        assert!(prev.map_or(true, |t| t.is_none()),
                                "duplicate field {:?} in the chain {:?}..{:?}", k, rvar0, rvar);
                    } else {
                        // negative fields should not overwrite positive fields
                        fields.entry(k.clone()).or_insert(None);
                    }
                }
                if let Some(ref next) = info.next {
                    rvar = next.clone();
                } else {
                    break;
                }
            } else {
                // next row variable has been instantiated but not set any fields, stop here
                break;
            }

            // TODO is recursion detection required here?
        }

        // we know that rvar0 contains `fields` plus an unspecified non-empty row variable,
        // which should be replaced with an (uninstantiated) fresh row variable.
        let rvar = self.gen_rvar();
        self.row_infos.insert(rvar.to_usize(),
                              Box::new(RowInfo { fields: Some(fields), next: None }));
        rvar
    }

    fn assert_rvar_sub(&mut self, lhs: RVar, rhs: RVar) -> TypeResult<()> {
        // TODO appropriate labels just in case
        self.assert_rvar_rel(lhs.clone(), rhs.clone(), true).map_err(|r| {
            r.not_sub(Origin::RVar, "<rvar>", "<rvar>", self)
        })
    }

    fn assert_rvar_eq(&mut self, lhs: RVar, rhs: RVar) -> TypeResult<()> {
        // TODO appropriate labels just in case
        self.assert_rvar_rel(lhs.clone(), rhs.clone(), false).map_err(|r| {
            r.not_eq(Origin::RVar, "<rvar>", "<rvar>", self)
        })
    }

    fn assert_rvar_includes(&mut self, lhs: RVar, rhs: &[(Key, Slot)]) -> TypeResult<()> {
        self.assert_rvar_includes_(lhs.clone(), rhs, true).map_err(|r| {
            r.record_should_have_keys(rhs.iter().map(|&(ref k, _)| k))
        })
    }

    fn assert_rvar_closed(&mut self, mut rvar: RVar) -> TypeResult<()> {
        trace!("{:?} should not be extensible", rvar);

        // detect a cycle by advancing slowrvar 1/2x slower than rvar;
        // if rvar == slowrvar is true after the initial loop, it's a cycle
        let mut slowrvar = rvar.clone();
        let mut slowtick = true;

        loop {
            // a row variable has been already closed
            if rvar == RVar::empty() {
                return Ok(());
            }

            {
                let rvar_ = rvar.to_usize();
                let info = self.row_infos.entry(rvar_).or_insert_with(|| Box::new(RowInfo::new()));
                if let Some(ref next) = info.next {
                    rvar = next.clone();
                } else {
                    info.next = Some(RVar::empty());
                    return Ok(());
                }
            }

            // advance slowrvar on the 2nd, 4th, 6th, ... iterations and compare with rvar
            // slowrvar is guaranteed not to be special, as it has once been rvar previously
            if slowtick {
                slowtick = false;
            } else {
                slowrvar = self.row_infos.get(slowrvar.to_usize()).unwrap().next.clone().unwrap();
                if slowrvar == rvar {
                    return Err(self.gen_report().recursive_record());
                }
                slowtick = true;
            }
        }
    }

    fn list_rvar_fields(
        &self, mut rvar: RVar, f: &mut FnMut(&Key, &Slot) -> Result<(), ()>
    ) -> Result<RVar, ()> {
        loop {
            if let Some(info) = self.row_infos.get(rvar.to_usize()) {
                if let Some(ref fields) = info.fields {
                    trace!("{:?} contains {:?}", rvar, fields);
                    for (k, v) in fields.iter() {
                        if let Some(ref v) = *v { // skip negative fields
                            f(k, v)?; // handle user requested break
                        }
                    }
                }
                if let Some(ref next) = info.next {
                    rvar = next.clone();
                } else {
                    return Ok(RVar::any());
                }
            } else {
                // return immediately if the row variable is special or not yet instantiated
                return Ok(rvar);
            }
        }
    }

    fn fmt_class_name(&self, cid: ClassId, f: &mut fmt::Formatter,
                      st: &DisplayState) -> fmt::Result {
        let name = self.classes.get(cid.0 as usize).and_then(|cls| {
            cls.name.as_ref().map(|name| &name.base)
        });
        match (&st.locale[..], name) {
            (_,    Some(ref name)) => write!(f, "{:+}", name),
            ("ko", None) => write!(f, "이름 없는 클래스 #{}", cid.0),
            (_,    None) => write!(f, "unnamed class #{}", cid.0),
        }
    }

    fn is_subclass_of(&self, mut lhs: ClassId, rhs: ClassId) -> bool {
        if lhs == rhs { return true; }

        while let Some(parent) = self.classes[lhs.0 as usize].parent {
            assert!(parent < lhs);
            lhs = parent;
            if lhs == rhs { return true; }
        }

        false
    }
}

#[test]
fn test_types_is_send_and_sync() {
    fn _assert_send<T: Send>(_x: T) {}
    fn _assert_sync<T: Sync>(_x: T) {}

    _assert_send(Types::new(Locale::dummy()));
    _assert_sync(Types::new(Locale::dummy()));
}

#[test]
fn test_types_tvar() {
    let mut types = Types::new(Locale::dummy());

    { // idempotency of bounds
        let v1 = types.gen_tvar();
        assert!(types.assert_tvar_sub(v1, &Ty::new(T::Integer)).is_ok());
        assert!(types.assert_tvar_sub(v1, &Ty::new(T::Integer)).is_ok());
        assert!(types.assert_tvar_sub(v1, &Ty::new(T::String)).is_err());
    }

    { // empty bounds (lb & ub = bottom)
        let v1 = types.gen_tvar();
        assert!(types.assert_tvar_sub(v1, &Ty::new(T::Integer)).is_ok());
        assert!(types.assert_tvar_sup(v1, &Ty::new(T::String)).is_err());

        let v2 = types.gen_tvar();
        assert!(types.assert_tvar_sup(v2, &Ty::new(T::Integer)).is_ok());
        assert!(types.assert_tvar_sub(v2, &Ty::new(T::String)).is_err());
    }

    { // empty bounds (lb & ub != bottom)
        let v1 = types.gen_tvar();
        assert!(types.assert_tvar_sub(v1, &Ty::new(T::ints(vec![3, 4, 5]))).is_ok());
        assert!(types.assert_tvar_sup(v1, &Ty::new(T::ints(vec![1, 2, 3]))).is_err());

        let v2 = types.gen_tvar();
        assert!(types.assert_tvar_sup(v2, &Ty::new(T::ints(vec![3, 4, 5]))).is_ok());
        assert!(types.assert_tvar_sub(v2, &Ty::new(T::ints(vec![1, 2, 3]))).is_err());
    }

    { // implicitly disjoint bounds
        let v1 = types.gen_tvar();
        let v2 = types.gen_tvar();
        assert!(types.assert_tvar_sub_tvar(v1, v2).is_ok());
        assert!(types.assert_tvar_sub(v2, &Ty::new(T::String)).is_ok());
        assert!(types.assert_tvar_sub(v1, &Ty::new(T::Integer)).is_err());

        let v3 = types.gen_tvar();
        let v4 = types.gen_tvar();
        assert!(types.assert_tvar_sub_tvar(v3, v4).is_ok());
        assert!(types.assert_tvar_sup(v3, &Ty::new(T::String)).is_ok());
        assert!(types.assert_tvar_sup(v4, &Ty::new(T::Integer)).is_err());
    }

    { // equality propagation
        let v1 = types.gen_tvar();
        assert!(types.assert_tvar_eq(v1, &Ty::new(T::Integer)).is_ok());
        assert!(types.assert_tvar_sub(v1, &Ty::new(T::Number)).is_ok());
        assert!(types.assert_tvar_sup(v1, &Ty::new(T::String)).is_err());

        let v2 = types.gen_tvar();
        assert!(types.assert_tvar_sub(v2, &Ty::new(T::Number)).is_ok());
        assert!(types.assert_tvar_eq(v2, &Ty::new(T::Integer)).is_ok());
        assert!(types.assert_tvar_sup(v2, &Ty::new(T::String)).is_err());

        let v3 = types.gen_tvar();
        assert!(types.assert_tvar_sub(v3, &Ty::new(T::Number)).is_ok());
        assert!(types.assert_tvar_eq(v3, &Ty::new(T::String)).is_err());
    }
}

