use std::cmp;
use std::fmt;
use std::vec;
use std::usize;
use diag::CheckResult;
use super::{T, Ty, Slot, Lattice, TypeContext};

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
    pub tail: Option<Ty>, // None is same to Some(Box::new(T::Nil)) but has a display hint
}

impl TySeq {
    pub fn new() -> TySeq {
        TySeq { head: Vec::new(), tail: None }
    }

    pub fn from(t: T) -> TySeq {
        TySeq { head: vec![Box::new(t.into_send())], tail: None }
    }

    pub fn into_iter(self) -> TySeqIter {
        TySeqIter { head: self.head.into_iter(),
                    tail: self.tail.unwrap_or_else(|| Box::new(T::Nil)) }
    }

    pub fn into_first(self) -> Ty {
        if let Some(head) = self.head.into_iter().next() { return head; }
        if let Some(tail) = self.tail { return tail; }
        Box::new(T::Nil)
    }
}

impl Lattice for TySeq {
    type Output = TySeq;

    fn normalize(self) -> TySeq {
        TySeq { head: self.head.into_iter().map(Lattice::normalize).collect(),
                tail: self.tail.map(Lattice::normalize) }
    }

    fn union(&self, other: &TySeq, ctx: &mut TypeContext) -> TySeq {
        let nil = T::Nil;
        let selftail = self.tail.as_ref().map_or(&nil, |t| &**t);
        let othertail = other.tail.as_ref().map_or(&nil, |t| &**t);

        let mut head = Vec::new();
        let n = cmp::min(self.head.len(), other.head.len());
        for (l, r) in self.head[..n].iter().zip(other.head[..n].iter()) {
            head.push(l.union(r, ctx));
        }
        for l in &self.head[n..] {
            head.push(Box::new((**l).union(othertail, ctx)));
        }
        for r in &other.head[n..] {
            head.push(Box::new(selftail.union(&*r, ctx)));
        }

        let tail = if self.tail.is_some() || other.tail.is_some() {
            Some(Box::new(selftail.union(othertail, ctx)))
        } else {
            None
        };

        TySeq { head: head, tail: tail }
    }

    fn assert_sub(&self, other: &TySeq, ctx: &mut TypeContext) -> CheckResult<()> {
        println!("asserting a constraint {:?} <: {:?}", *self, *other);

        let mut selfhead = self.head.iter().fuse();
        let mut otherhead = other.head.iter().fuse();
        let nil = T::Nil;
        let selftail = self.tail.as_ref().map_or(&nil, |t| &**t);
        let othertail = other.tail.as_ref().map_or(&nil, |t| &**t);
        loop {
            match (selfhead.next(), otherhead.next()) {
                (Some(a), Some(b)) => try!(a.assert_sub(b, ctx)),
                (Some(a), None) => try!((**a).assert_sub(othertail, ctx)),
                (None, Some(b)) => try!(selftail.assert_sub(&*b, ctx)),
                (None, None) => return selftail.assert_sub(othertail, ctx),
            }
        }
    }

    fn assert_eq(&self, other: &TySeq, ctx: &mut TypeContext) -> CheckResult<()> {
        println!("asserting a constraint {:?} = {:?}", *self, *other);

        let mut selfhead = self.head.iter().fuse();
        let mut otherhead = other.head.iter().fuse();
        let nil = T::Nil;
        let selftail = self.tail.as_ref().map_or(&nil, |t| &*t);
        let othertail = other.tail.as_ref().map_or(&nil, |t| &*t);
        loop {
            match (selfhead.next(), otherhead.next()) {
                (Some(a), Some(b)) => try!(a.assert_eq(b, ctx)),
                (Some(a), None) => try!((**a).assert_eq(othertail, ctx)),
                (None, Some(b)) => try!(selftail.assert_eq(&*b, ctx)),
                (None, None) => return selftail.assert_eq(othertail, ctx),
            }
        }
    }
}

impl fmt::Debug for TySeq {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "("));
        let mut first = true;
        for t in &self.head {
            if first { first = false; } else { try!(write!(f, ", ")); }
            try!(write!(f, "{:?}", t));
        }
        if let Some(ref t) = self.tail {
            if !first { try!(write!(f, ", ")); }
            try!(write!(f, ", {:?}...", t));
        }
        write!(f, ")")
    }
}

pub struct SlotSeqIter {
    head: vec::IntoIter<Slot>,
    tail: Option<Slot>,
}

impl Iterator for SlotSeqIter {
    type Item = Slot;

    fn next(&mut self) -> Option<Slot> {
        self.head.next().or_else(|| self.tail.clone())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.tail.is_some() { (usize::MAX, None) } else { self.head.size_hint() }
    }
}

#[derive(Clone, PartialEq)]
pub struct SlotSeq {
    pub head: Vec<Slot>,
    pub tail: Option<Slot>,
}

impl SlotSeq {
    pub fn new() -> SlotSeq {
        SlotSeq { head: Vec::new(), tail: None }
    }

    pub fn from(t: T) -> SlotSeq {
        SlotSeq::from_slot(Slot::from(t))
    }

    pub fn from_slot(s: Slot) -> SlotSeq {
        SlotSeq { head: vec![s], tail: None }
    }

    pub fn from_seq(seq: TySeq) -> SlotSeq {
        SlotSeq { head: seq.head.into_iter().map(|t| Slot::just(*t)).collect(),
                  tail: seq.tail.map(|t| Slot::just(*t)) }
    }

    pub fn into_iter(self) -> SlotSeqIter {
        SlotSeqIter { head: self.head.into_iter(), tail: self.tail }
    }

    pub fn into_first(self) -> Slot {
        if let Some(head) = self.head.into_iter().next() { return head; }
        if let Some(tail) = self.tail { return tail; }
        Slot::just(T::Nil)
    }

    pub fn assert_sub(&self, other: &SlotSeq, ctx: &mut TypeContext) -> CheckResult<()> {
        println!("asserting a constraint {:?} <: {:?}", *self, *other);

        let mut selfhead = self.head.iter().fuse();
        let mut otherhead = other.head.iter().fuse();
        let nil = Slot::just(T::Nil);
        let selftail = self.tail.as_ref().unwrap_or(&nil);
        let othertail = other.tail.as_ref().unwrap_or(&nil);
        loop {
            match (selfhead.next(), otherhead.next()) {
                (Some(a), Some(b)) => try!(a.assert_sub(b, ctx)),
                (Some(a), None) => try!(a.assert_sub(othertail, ctx)),
                (None, Some(b)) => try!(selftail.assert_sub(&b, ctx)),
                (None, None) => return selftail.assert_sub(othertail, ctx),
            }
        }
    }

    pub fn assert_eq(&self, other: &SlotSeq, ctx: &mut TypeContext) -> CheckResult<()> {
        println!("asserting a constraint {:?} = {:?}", *self, *other);

        let mut selfhead = self.head.iter().fuse();
        let mut otherhead = other.head.iter().fuse();
        let nil = Slot::just(T::Nil);
        let selftail = self.tail.as_ref().unwrap_or(&nil);
        let othertail = other.tail.as_ref().unwrap_or(&nil);
        loop {
            match (selfhead.next(), otherhead.next()) {
                (Some(a), Some(b)) => try!(a.assert_eq(b, ctx)),
                (Some(a), None) => try!(a.assert_eq(othertail, ctx)),
                (None, Some(b)) => try!(selftail.assert_eq(&b, ctx)),
                (None, None) => return selftail.assert_eq(othertail, ctx),
            }
        }
    }
}

impl fmt::Debug for SlotSeq {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "("));
        let mut first = true;
        for t in &self.head {
            if first { first = false; } else { try!(write!(f, ", ")); }
            try!(write!(f, "{:?}", t));
        }
        if let Some(ref t) = self.tail {
            if !first { try!(write!(f, ", ")); }
            try!(write!(f, ", {:?}...", t));
        }
        write!(f, ")")
    }
}

