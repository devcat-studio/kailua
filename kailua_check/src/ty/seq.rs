use std::cmp;
use std::fmt;
use std::vec;
use std::usize;

use kailua_diag::Spanned;
use kailua_syntax::{Seq, Kind};
use diag::CheckResult;
use super::{T, Ty, TyWithNil, Slot, SlotWithNil, Lattice, Displayed, Display};
use super::{TypeContext, TypeResolver};

pub struct TySeqIter {
    head: vec::IntoIter<Ty>,
    tail: Ty,
}

impl Iterator for TySeqIter {
    type Item = Ty;

    fn next(&mut self) -> Option<Ty> {
        Some(self.head.next().unwrap_or_else(|| self.tail.clone()))
    }

    fn size_hint(&self) -> (usize, Option<usize>) { (usize::MAX, None) }
}

#[derive(Clone, PartialEq)]
pub struct TySeq {
    pub head: Vec<Ty>,

    // why is this TyWithNil? the TySeq (and others) is never infinite,
    // so there should be a `nil` somewhere. since we don't know where it ends,
    // the tail is implicitly unioned with `nil` in all practical sense.
    //
    // None is same to Some(Box::new(T::Nil)) but has a display hint
    // and has a different behavior for the arity calculation.
    // (we may need this because, well, C/C++ API *can* count the proper number of args.
    // TODO this is not yet enforced due to a number of concerns)
    pub tail: Option<Box<TyWithNil>>,
}

impl TySeq {
    pub fn new() -> TySeq {
        TySeq { head: Vec::new(), tail: None }
    }

    pub fn from(t: T) -> TySeq {
        TySeq { head: vec![Box::new(t.into_send())], tail: None }
    }

    pub fn from_kind_seq(seq: &Seq<Spanned<Kind>>,
                         resolv: &mut TypeResolver) -> CheckResult<TySeq> {
        let head = try!(seq.head.iter()
                                .map(|k| T::from(k, resolv).map(Box::new))
                                .collect());
        let tail = if let Some(ref k) = seq.tail {
            Some(Box::new(TyWithNil::from(try!(T::from(k, resolv)))))
        } else {
            None
        };
        Ok(TySeq { head: head, tail: tail })
    }

    fn ensure_index(&mut self, i: usize) {
        while self.head.len() <= i {
            let new = Box::new(self.tail.as_ref().map_or(T::Nil, |t| t.clone().into_type()));
            self.head.push(new);
        }
    }

    pub fn ensure_at(&mut self, i: usize) -> &Ty {
        self.ensure_index(i);
        &self.head[i]
    }

    pub fn ensure_at_mut(&mut self, i: usize) -> &mut Ty {
        self.ensure_index(i);
        &mut self.head[i]
    }

    pub fn into_iter(self) -> TySeqIter {
        TySeqIter { head: self.head.into_iter(),
                    tail: Box::new(self.tail.map_or(T::Nil, |t| t.into_type())) }
    }

    pub fn into_first(self) -> Ty {
        if let Some(head) = self.head.into_iter().next() { return head; }
        if let Some(tail) = self.tail { return Box::new(tail.into_type()); }
        Box::new(T::Nil)
    }

    fn fmt_generic<WriteTy>(&self, f: &mut fmt::Formatter,
                            mut write_ty: WriteTy) -> fmt::Result
            where WriteTy: FnMut(&T, &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "("));
        let mut first = true;
        for t in &self.head {
            if first { first = false; } else { try!(write!(f, ", ")); }
            try!(write_ty(t, f));
        }
        if let Some(ref t) = self.tail {
            if !first { try!(write!(f, ", ")); }
            try!(write_ty(t.as_type_without_nil(), f));
            try!(write!(f, "..."));
        }
        write!(f, ")")
    }
}

impl Lattice for TySeq {
    type Output = TySeq;

    fn do_union(&self, other: &TySeq, ctx: &mut TypeContext) -> TySeq {
        let selftail = self.tail.clone().map_or(T::Nil, |t| t.into_type());
        let othertail = other.tail.clone().map_or(T::Nil, |t| t.into_type());

        let mut head = Vec::new();
        let n = cmp::min(self.head.len(), other.head.len());
        for (l, r) in self.head[..n].iter().zip(other.head[..n].iter()) {
            head.push(Box::new(l.union(r, ctx)));
        }
        for l in &self.head[n..] {
            head.push(Box::new(l.union(&othertail, ctx)));
        }
        for r in &other.head[n..] {
            head.push(Box::new(selftail.union(r, ctx)));
        }

        let tail = if self.tail.is_some() || other.tail.is_some() {
            Some(Box::new(TyWithNil::from(selftail.union(&othertail, ctx))))
        } else {
            None
        };

        TySeq { head: head, tail: tail }
    }

    fn do_assert_sub(&self, other: &TySeq, ctx: &mut TypeContext) -> CheckResult<()> {
        debug!("asserting a constraint {:?} <: {:?}", *self, *other);

        let mut selfhead = self.head.iter().fuse();
        let mut otherhead = other.head.iter().fuse();
        let selftail = self.tail.clone().map_or(T::Nil, |t| t.into_type());
        let othertail = other.tail.clone().map_or(T::Nil, |t| t.into_type());
        loop {
            match (selfhead.next(), otherhead.next()) {
                (Some(a), Some(b)) => try!(a.assert_sub(b, ctx)),
                (Some(a), None) => try!(a.assert_sub(&othertail, ctx)),
                (None, Some(b)) => try!(selftail.assert_sub(b, ctx)),
                (None, None) => return selftail.assert_sub(&othertail, ctx),
            }
        }
    }

    fn do_assert_eq(&self, other: &TySeq, ctx: &mut TypeContext) -> CheckResult<()> {
        debug!("asserting a constraint {:?} = {:?}", *self, *other);

        let mut selfhead = self.head.iter().fuse();
        let mut otherhead = other.head.iter().fuse();
        let selftail = self.tail.clone().map_or(T::Nil, |t| t.into_type());
        let othertail = other.tail.clone().map_or(T::Nil, |t| t.into_type());
        loop {
            match (selfhead.next(), otherhead.next()) {
                (Some(a), Some(b)) => try!(a.assert_eq(b, ctx)),
                (Some(a), None) => try!(a.assert_eq(&othertail, ctx)),
                (None, Some(b)) => try!(selftail.assert_eq(b, ctx)),
                (None, None) => return selftail.assert_eq(&othertail, ctx),
            }
        }
    }
}

impl Display for TySeq {}

impl<'b, 'c> fmt::Display for Displayed<'b, 'c, TySeq> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.base.fmt_generic(f, |t, f| fmt::Display::fmt(&t.display(self.ctx), f))
    }
}

impl fmt::Debug for TySeq {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_generic(f, |t, f| fmt::Debug::fmt(t, f))
    }
}

pub struct SlotSeqIter {
    head: vec::IntoIter<Slot>,
    tail: Slot,
}

impl Iterator for SlotSeqIter {
    type Item = Slot;

    fn next(&mut self) -> Option<Slot> {
        Some(self.head.next().unwrap_or_else(|| self.tail.clone()))
    }

    fn size_hint(&self) -> (usize, Option<usize>) { (usize::MAX, None) }
}

#[derive(Clone, PartialEq)]
pub struct SlotSeq {
    pub head: Vec<Slot>,
    pub tail: Option<SlotWithNil>,
}

impl SlotSeq {
    pub fn new() -> SlotSeq {
        SlotSeq { head: Vec::new(), tail: None }
    }

    pub fn from(t: T) -> SlotSeq {
        SlotSeq::from_slot(Slot::just(t))
    }

    pub fn from_slot(s: Slot) -> SlotSeq {
        SlotSeq { head: vec![s], tail: None }
    }

    pub fn from_seq(seq: TySeq) -> SlotSeq {
        SlotSeq { head: seq.head.into_iter().map(|t| Slot::just(*t)).collect(),
                  tail: seq.tail.map(|t| SlotWithNil::from_ty_with_nil(*t)) }
    }

    fn ensure_index(&mut self, i: usize) {
        while self.head.len() <= i {
            let new = self.tail.as_ref().map(|s| s.clone().into_slot())
                                        .unwrap_or_else(|| Slot::just(T::Nil));
            self.head.push(new);
        }
    }

    pub fn ensure_at(&mut self, i: usize) -> &Slot {
        self.ensure_index(i);
        &self.head[i]
    }

    pub fn ensure_at_mut(&mut self, i: usize) -> &mut Slot {
        self.ensure_index(i);
        &mut self.head[i]
    }

    pub fn into_iter(self) -> SlotSeqIter {
        SlotSeqIter {
            head: self.head.into_iter(),
            tail: self.tail.map(|s| s.into_slot()).unwrap_or_else(|| Slot::just(T::Nil)),
        }
    }

    pub fn into_first(self) -> Slot {
        if let Some(head) = self.head.into_iter().next() { return head; }
        if let Some(tail) = self.tail { return tail.into_slot(); }
        Slot::just(T::Nil)
    }

    pub fn unlift(self) -> TySeq {
        let head =
            self.head.into_iter().map(|s| Box::new(s.unlift().clone().into_send()));
        let tail = if let Some(tail) = self.tail {
            Some(Box::new(TyWithNil::from(tail.as_slot_without_nil().unlift().clone())))
        } else {
            None
        };
        TySeq { head: head.collect(), tail: tail }
    }

    fn fmt_generic<WriteSlot>(&self, f: &mut fmt::Formatter,
                              mut write_slot: WriteSlot) -> fmt::Result
            where WriteSlot: FnMut(&Slot, &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "("));
        let mut first = true;
        for t in &self.head {
            if first { first = false; } else { try!(write!(f, ", ")); }
            try!(write_slot(t, f));
        }
        if let Some(ref t) = self.tail {
            if !first { try!(write!(f, ", ")); }
            try!(write_slot(t.as_slot_without_nil(), f));
            try!(write!(f, "..."));
        }
        write!(f, ")")
    }
}

impl Lattice for SlotSeq {
    type Output = SlotSeq;

    fn do_union(&self, other: &SlotSeq, ctx: &mut TypeContext) -> SlotSeq {
        let selftail = self.tail.clone().map_or(Slot::just(T::Nil), |s| s.into_slot());
        let othertail = other.tail.clone().map_or(Slot::just(T::Nil), |s| s.into_slot());

        let mut head = Vec::new();
        let n = cmp::min(self.head.len(), other.head.len());
        for (l, r) in self.head[..n].iter().zip(other.head[..n].iter()) {
            head.push(l.union(r, ctx));
        }
        for l in &self.head[n..] {
            head.push(l.union(&othertail, ctx));
        }
        for r in &other.head[n..] {
            head.push(selftail.union(r, ctx));
        }

        let tail = if self.tail.is_some() || other.tail.is_some() {
            Some(SlotWithNil::from_slot(selftail.union(&othertail, ctx)))
        } else {
            None
        };

        SlotSeq { head: head, tail: tail }
    }

    fn do_assert_sub(&self, other: &SlotSeq, ctx: &mut TypeContext) -> CheckResult<()> {
        debug!("asserting a constraint {:?} <: {:?}", *self, *other);

        let mut selfhead = self.head.iter().fuse();
        let mut otherhead = other.head.iter().fuse();
        let selftail = self.tail.clone().map_or(Slot::just(T::Nil), |s| s.into_slot());
        let othertail = other.tail.clone().map_or(Slot::just(T::Nil), |s| s.into_slot());
        loop {
            match (selfhead.next(), otherhead.next()) {
                (Some(a), Some(b)) => try!(a.assert_sub(b, ctx)),
                (Some(a), None) => try!(a.assert_sub(&othertail, ctx)),
                (None, Some(b)) => try!(selftail.assert_sub(b, ctx)),
                (None, None) => return selftail.assert_sub(&othertail, ctx),
            }
        }
    }

    fn do_assert_eq(&self, other: &SlotSeq, ctx: &mut TypeContext) -> CheckResult<()> {
        debug!("asserting a constraint {:?} = {:?}", *self, *other);

        let mut selfhead = self.head.iter().fuse();
        let mut otherhead = other.head.iter().fuse();
        let selftail = self.tail.clone().map_or(Slot::just(T::Nil), |s| s.into_slot());
        let othertail = other.tail.clone().map_or(Slot::just(T::Nil), |s| s.into_slot());
        loop {
            match (selfhead.next(), otherhead.next()) {
                (Some(a), Some(b)) => try!(a.assert_eq(b, ctx)),
                (Some(a), None) => try!(a.assert_eq(&othertail, ctx)),
                (None, Some(b)) => try!(selftail.assert_eq(b, ctx)),
                (None, None) => return selftail.assert_eq(&othertail, ctx),
            }
        }
    }
}

impl Display for SlotSeq {}

impl<'b, 'c> fmt::Display for Displayed<'b, 'c, SlotSeq> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.base.fmt_generic(f, |s, f| fmt::Display::fmt(&s.display(self.ctx), f))
    }
}

impl fmt::Debug for SlotSeq {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_generic(f, fmt::Debug::fmt)
    }
}

