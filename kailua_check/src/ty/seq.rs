use std::cmp;
use std::fmt;
use std::vec;
use std::usize;

use kailua_diag::{Span, Spanned, WithLoc};
use kailua_syntax::{Seq, Kind};
use diag::CheckResult;
use super::{T, Ty, TyWithNil, Slot, SlotWithNil, Lattice, Display};
use super::{TypeContext, TypeResolver};

pub struct SeqIter<Item: Clone> {
    head: vec::IntoIter<Item>,
    tail: Item,
}

impl<Item: Clone> Iterator for SeqIter<Item> {
    type Item = Item;

    fn next(&mut self) -> Option<Item> {
        Some(self.head.next().unwrap_or_else(|| self.tail.clone()))
    }

    fn size_hint(&self) -> (usize, Option<usize>) { (usize::MAX, None) }
}

pub type TySeqIter = SeqIter<Ty>;
pub type SpannedTySeqIter = SeqIter<Spanned<Ty>>;
pub type SlotSeqIter = SeqIter<Slot>;
pub type SpannedSlotSeqIter = SeqIter<Spanned<Slot>>;

macro_rules! define_tyseq {
    ($(
        type $tyseq:ident {
            t = $t:ty;
            ty = $ty:ty;
            tynil = $tynil:ty;
            unspanned_t = $unspanned_t:ty; // &t should be coercable to &unspanned_t

            nil_ty = $nil_ty:expr;
            dummy_tynil = $dummy_tynil:expr;
            ty_union = $ty_union:expr;
            t_to_ty = $t_to_ty:expr;
            tynil_to_ty = $tynil_to_ty:expr;
            ty_to_tynil = $ty_to_tynil:expr;
            spanned_kind_to_ty = $spanned_kind_to_ty:expr;
        }
    )*) => ($(
        #[derive(Clone, PartialEq)]
        pub struct $tyseq {
            pub head: Vec<$ty>,

            // why is this TyWithNil? the TySeq (and others) is never infinite,
            // so there should be a `nil` somewhere. since we don't know where it ends,
            // the tail is implicitly unioned with `nil` in all practical sense.
            //
            // None is same to Some(Box::new(T::Nil)) but has a display hint
            // and has a different behavior for the arity calculation.
            // (we may need this because, well, C/C++ API *can* count the proper number of args.
            // TODO this is not yet enforced due to a number of concerns)
            pub tail: Option<$tynil>,
        }

        impl $tyseq {
            pub fn new() -> $tyseq {
                $tyseq { head: Vec::new(), tail: None }
            }

            pub fn from(t: $t) -> $tyseq {
                $tyseq { head: vec![$t_to_ty(t)], tail: None }
            }

            pub fn from_kind_seq(seq: &Seq<Spanned<Kind>>,
                                 resolv: &mut TypeResolver) -> CheckResult<$tyseq> {
                let head = try!(seq.head.iter()
                                        .map(|k| $spanned_kind_to_ty(k, resolv))
                                        .collect());
                let tail = if let Some(ref k) = seq.tail {
                    Some($ty_to_tynil(try!($spanned_kind_to_ty(k, resolv))))
                } else {
                    None
                };
                Ok($tyseq { head: head, tail: tail })
            }

            pub fn dummy() -> $tyseq {
                $tyseq { head: Vec::new(), tail: Some($dummy_tynil) }
            }

            fn tail_to_type(&self) -> $ty {
                if let Some(ref t) = self.tail { $tynil_to_ty(t.clone()) } else { $nil_ty }
            }

            fn ensure_index(&mut self, i: usize) {
                while self.head.len() <= i {
                    let new = self.tail_to_type();
                    self.head.push(new);
                }
            }

            pub fn ensure_at(&mut self, i: usize) -> &$ty {
                self.ensure_index(i);
                &self.head[i]
            }

            pub fn ensure_at_mut(&mut self, i: usize) -> &mut $ty {
                self.ensure_index(i);
                &mut self.head[i]
            }

            pub fn into_iter(self) -> SeqIter<$ty> {
                let tail = if let Some(t) = self.tail { $tynil_to_ty(t) } else { $nil_ty };
                SeqIter { head: self.head.into_iter(), tail: tail }
            }

            pub fn into_first(self) -> $ty {
                if let Some(head) = self.head.into_iter().next() { return head; }
                if let Some(tail) = self.tail { return $tynil_to_ty(tail); }
                $nil_ty
            }

            fn fmt_generic<WriteTy>(&self, f: &mut fmt::Formatter,
                                    mut write_ty: WriteTy) -> fmt::Result
                    where WriteTy: FnMut(&$unspanned_t, &mut fmt::Formatter) -> fmt::Result {
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

        impl Lattice for $tyseq {
            type Output = $tyseq;

            fn union(&self, other: &$tyseq, ctx: &mut TypeContext) -> $tyseq {
                let selftail = self.tail_to_type();
                let othertail = other.tail_to_type();

                let mut head = Vec::new();
                let n = cmp::min(self.head.len(), other.head.len());
                for (l, r) in self.head[..n].iter().zip(other.head[..n].iter()) {
                    head.push($ty_union(l, r, ctx));
                }
                for l in &self.head[n..] {
                    head.push($ty_union(l, &othertail, ctx));
                }
                for r in &other.head[n..] {
                    head.push($ty_union(&selftail, r, ctx));
                }

                let tail = if self.tail.is_some() || other.tail.is_some() {
                    Some($ty_to_tynil($ty_union(&selftail, &othertail, ctx)))
                } else {
                    None
                };

                $tyseq { head: head, tail: tail }
            }

            fn assert_sub(&self, other: &$tyseq, ctx: &mut TypeContext) -> CheckResult<()> {
                debug!("asserting a constraint {:?} <: {:?}", *self, *other);

                let mut selfhead = self.head.iter().fuse();
                let mut otherhead = other.head.iter().fuse();
                let selftail = self.tail_to_type();
                let othertail = other.tail_to_type();
                loop {
                    match (selfhead.next(), otherhead.next()) {
                        (Some(a), Some(b)) => try!(a.assert_sub(b, ctx)),
                        (Some(a), None) => try!(a.assert_sub(&othertail, ctx)),
                        (None, Some(b)) => try!(selftail.assert_sub(b, ctx)),
                        (None, None) => return selftail.assert_sub(&othertail, ctx),
                    }
                }
            }

            fn assert_eq(&self, other: &$tyseq, ctx: &mut TypeContext) -> CheckResult<()> {
                debug!("asserting a constraint {:?} = {:?}", *self, *other);

                let mut selfhead = self.head.iter().fuse();
                let mut otherhead = other.head.iter().fuse();
                let selftail = self.tail_to_type();
                let othertail = other.tail_to_type();
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

        impl Display for $tyseq {
            fn fmt_displayed(&self, f: &mut fmt::Formatter, ctx: &TypeContext) -> fmt::Result {
                self.fmt_generic(f, |t, f| fmt::Display::fmt(&t.display(ctx), f))
            }
        }

        impl fmt::Debug for $tyseq {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.fmt_generic(f, |t, f| fmt::Debug::fmt(t, f))
            }
        }
    )*)
}

macro_rules! define_slotseq {
    ($(
        type $slotseq:ident <- $tyseq:ident {
            slot = $slot:ty;
            slotnil = $slotnil:ty;
            t = $t:ty;
            ty = $ty:ty;
            tynil = $tynil:ty;
            unspanned_slot = $unspanned_slot:ty; // &slot should be coercable to &unspanned_slot

            nil_slot = $nil_slot:expr;
            dummy_slotnil = $dummy_slotnil:expr;
            slot_union = $slot_union:expr;
            t_to_slot = $t_to_slot:expr;
            ty_to_slot = $ty_to_slot:expr;
            tynil_to_slotnil = $tynil_to_slotnil:expr;
            slotnil_to_slot = $slotnil_to_slot:expr;
            slot_to_slotnil = $slot_to_slotnil:expr;
            slot_to_ty = $slot_to_ty:expr;
            slotnil_to_tynil = $slotnil_to_tynil:expr;
        }
    )*) => ($(
        #[derive(Clone, PartialEq)]
        pub struct $slotseq {
            pub head: Vec<$slot>,
            pub tail: Option<$slotnil>,
        }

        impl $slotseq {
            pub fn new() -> $slotseq {
                $slotseq { head: Vec::new(), tail: None }
            }

            pub fn from(t: $t) -> $slotseq {
                $slotseq::from_slot($t_to_slot(t))
            }

            pub fn from_slot(s: $slot) -> $slotseq {
                $slotseq { head: vec![s], tail: None }
            }

            pub fn from_seq(seq: $tyseq) -> $slotseq {
                $slotseq { head: seq.head.into_iter().map(|t| $ty_to_slot(t)).collect(),
                           tail: seq.tail.map(|t| $tynil_to_slotnil(t)) }
            }

            pub fn dummy() -> $slotseq {
                $slotseq { head: Vec::new(), tail: Some($dummy_slotnil) }
            }

            fn tail_to_slot(&self) -> $slot {
                if let Some(ref s) = self.tail { $slotnil_to_slot(s.clone()) } else { $nil_slot }
            }

            fn ensure_index(&mut self, i: usize) {
                while self.head.len() <= i {
                    let new = self.tail_to_slot();
                    self.head.push(new);
                }
            }

            pub fn ensure_at(&mut self, i: usize) -> &$slot {
                self.ensure_index(i);
                &self.head[i]
            }

            pub fn ensure_at_mut(&mut self, i: usize) -> &mut $slot {
                self.ensure_index(i);
                &mut self.head[i]
            }

            pub fn into_iter(self) -> SeqIter<$slot> {
                let tail = if let Some(t) = self.tail { $slotnil_to_slot(t) } else { $nil_slot };
                SeqIter { head: self.head.into_iter(), tail: tail }
            }

            pub fn into_first(self) -> $slot {
                if let Some(head) = self.head.into_iter().next() { return head; }
                if let Some(tail) = self.tail { return $slotnil_to_slot(tail); }
                $nil_slot
            }

            pub fn unlift(self) -> $tyseq {
                $tyseq { head: self.head.into_iter().map(|s| $slot_to_ty(s)).collect(),
                         tail: self.tail.map(|s| $slotnil_to_tynil(s)) }
            }

            fn fmt_generic<WriteSlot>(&self, f: &mut fmt::Formatter,
                                      mut write_slot: WriteSlot) -> fmt::Result
                    where WriteSlot: FnMut(&$unspanned_slot, &mut fmt::Formatter) -> fmt::Result {
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

        impl Lattice for $slotseq {
            type Output = $slotseq;

            fn union(&self, other: &$slotseq, ctx: &mut TypeContext) -> $slotseq {
                let selftail = self.tail_to_slot();
                let othertail = other.tail_to_slot();

                let mut head = Vec::new();
                let n = cmp::min(self.head.len(), other.head.len());
                for (l, r) in self.head[..n].iter().zip(other.head[..n].iter()) {
                    head.push($slot_union(l, r, ctx));
                }
                for l in &self.head[n..] {
                    head.push($slot_union(l, &othertail, ctx));
                }
                for r in &other.head[n..] {
                    head.push($slot_union(&selftail, r, ctx));
                }

                let tail = if self.tail.is_some() || other.tail.is_some() {
                    Some($slot_to_slotnil($slot_union(&selftail, &othertail, ctx)))
                } else {
                    None
                };

                $slotseq { head: head, tail: tail }
            }

            fn assert_sub(&self, other: &$slotseq, ctx: &mut TypeContext) -> CheckResult<()> {
                debug!("asserting a constraint {:?} <: {:?}", *self, *other);

                let mut selfhead = self.head.iter().fuse();
                let mut otherhead = other.head.iter().fuse();
                let selftail = self.tail_to_slot();
                let othertail = other.tail_to_slot();
                loop {
                    match (selfhead.next(), otherhead.next()) {
                        (Some(a), Some(b)) => try!(a.assert_sub(b, ctx)),
                        (Some(a), None) => try!(a.assert_sub(&othertail, ctx)),
                        (None, Some(b)) => try!(selftail.assert_sub(b, ctx)),
                        (None, None) => return selftail.assert_sub(&othertail, ctx),
                    }
                }
            }

            fn assert_eq(&self, other: &$slotseq, ctx: &mut TypeContext) -> CheckResult<()> {
                debug!("asserting a constraint {:?} = {:?}", *self, *other);

                let mut selfhead = self.head.iter().fuse();
                let mut otherhead = other.head.iter().fuse();
                let selftail = self.tail_to_slot();
                let othertail = other.tail_to_slot();
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

        impl Display for $slotseq {
            fn fmt_displayed(&self, f: &mut fmt::Formatter, ctx: &TypeContext) -> fmt::Result {
                self.fmt_generic(f, |s, f| fmt::Display::fmt(&s.display(ctx), f))
            }
        }

        impl fmt::Debug for $slotseq {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.fmt_generic(f, fmt::Debug::fmt)
            }
        }
    )*)
}

define_tyseq! {
    type TySeq {
        t = T;
        ty = Ty;
        tynil = Box<TyWithNil>;
        unspanned_t = T;

        nil_ty = Box::new(T::Nil);
        dummy_tynil = Box::new(TyWithNil::dummy());
        ty_union = |lhs: &Ty, rhs: &Ty, ctx| Box::new(lhs.union(rhs, ctx));
        t_to_ty = |t: T| Box::new(t.into_send());
        tynil_to_ty = |t: Box<TyWithNil>| Box::new(t.into_type());
        ty_to_tynil = |t: Ty| Box::new(TyWithNil::from(*t));
        spanned_kind_to_ty = |k: &Spanned<Kind>, resolv| T::from(k, resolv).map(Box::new);
    }

    type SpannedTySeq {
        t = Spanned<T>;
        ty = Spanned<Ty>;
        tynil = Spanned<Box<TyWithNil>>;
        unspanned_t = T;

        nil_ty = Box::new(T::Nil).without_loc();
        dummy_tynil = Box::new(TyWithNil::dummy()).without_loc();
        ty_union = |lhs: &Spanned<Ty>, rhs: &Spanned<Ty>, ctx|
            Box::new(lhs.base.union(&rhs.base, ctx)).without_loc();
        t_to_ty = |t: Spanned<T>| t.map(|t| Box::new(t.into_send()));
        tynil_to_ty = |t: Spanned<Box<TyWithNil>>| t.map(|t| Box::new(t.into_type()));
        ty_to_tynil = |t: Spanned<Ty>| t.map(|t| Box::new(TyWithNil::from(*t)));
        spanned_kind_to_ty = |k: &Spanned<Kind>, resolv|
            T::from(&k.base, resolv).map(|t| Box::new(t).with_loc(k));
    }
}

define_slotseq! {
    type SlotSeq <- TySeq {
        slot = Slot;
        slotnil = SlotWithNil;
        t = T;
        ty = Ty;
        tynil = Box<TyWithNil>;
        unspanned_slot = Slot;

        nil_slot = Slot::just(T::Nil);
        dummy_slotnil = SlotWithNil::dummy();
        slot_union = |lhs: &Slot, rhs: &Slot, ctx| lhs.union(rhs, ctx);
        t_to_slot = |t: T| Slot::just(t);
        ty_to_slot = |t: Ty| Slot::just(*t);
        tynil_to_slotnil = |t: Box<TyWithNil>| SlotWithNil::from_ty_with_nil(*t);
        slotnil_to_slot = |s: SlotWithNil| s.into_slot();
        slot_to_slotnil = |s: Slot| SlotWithNil::from_slot(s);
        slot_to_ty = |s: Slot| Box::new(s.unlift().clone().into_send());
        slotnil_to_tynil = |s: SlotWithNil|
            Box::new(TyWithNil::from(s.as_slot_without_nil().unlift().clone()));
    }

    type SpannedSlotSeq <- SpannedTySeq {
        slot = Spanned<Slot>;
        slotnil = Spanned<SlotWithNil>;
        t = Spanned<T>;
        ty = Spanned<Ty>;
        tynil = Spanned<Box<TyWithNil>>;
        unspanned_slot = Slot;

        nil_slot = Slot::just(T::Nil).without_loc();
        dummy_slotnil = SlotWithNil::dummy().without_loc();
        slot_union = |lhs: &Spanned<Slot>, rhs: &Spanned<Slot>, ctx|
            lhs.base.union(&rhs.base, ctx).without_loc();
        t_to_slot = |t: Spanned<T>| t.map(|t| Slot::just(t));
        ty_to_slot = |t: Spanned<Ty>| t.map(|t| Slot::just(*t));
        tynil_to_slotnil = |t: Spanned<Box<TyWithNil>>|
            t.map(|t| SlotWithNil::from_ty_with_nil(*t));
        slotnil_to_slot = |s: Spanned<SlotWithNil>| s.map(|s| s.into_slot());
        slot_to_slotnil = |s: Spanned<Slot>| s.map(|s| SlotWithNil::from_slot(s));
        slot_to_ty = |s: Spanned<Slot>| s.map(|s| Box::new(s.unlift().clone().into_send()));
        slotnil_to_tynil = |s: Spanned<SlotWithNil>|
            s.map(|s| Box::new(TyWithNil::from(s.as_slot_without_nil().unlift().clone())));
    }
}

impl TySeq {
    pub fn all_with_loc<Loc: Into<Span>>(self, loc: Loc) -> SpannedTySeq {
        let span: Span = loc.into();
        SpannedTySeq { head: self.head.into_iter().map(|t| t.with_loc(span)).collect(),
                       tail: self.tail.map(|t| t.with_loc(span)) }
    }

    pub fn all_without_loc(self) -> SpannedTySeq {
        self.all_with_loc(Span::dummy())
    }
}

impl SlotSeq {
    pub fn all_with_loc<Loc: Into<Span>>(self, loc: Loc) -> SpannedSlotSeq {
        let span: Span = loc.into();
        SpannedSlotSeq { head: self.head.into_iter().map(|s| s.with_loc(span)).collect(),
                         tail: self.tail.map(|s| s.with_loc(span)) }
    }

    pub fn all_without_loc(self) -> SpannedSlotSeq {
        self.all_with_loc(Span::dummy())
    }
}

impl SpannedTySeq {
    pub fn unspan(self) -> TySeq {
        TySeq { head: self.head.into_iter().map(|t| t.base).collect(),
                tail: self.tail.map(|t| t.base) }
    }

    pub fn all_span(&self) -> Span {
        let mut span = Span::dummy();
        for head in &self.head { span |= head.span; }
        if let Some(ref tail) = self.tail { span |= tail.span; }
        span
    }

    pub fn all_with_loc<Loc: Into<Span>>(mut self, loc: Loc) -> SpannedTySeq {
        let span: Span = loc.into();
        for head in &mut self.head { head.span = span; }
        if let Some(ref mut tail) = self.tail { tail.span = span; }
        self
    }

    pub fn all_without_loc(self) -> SpannedTySeq {
        self.all_with_loc(Span::dummy())
    }
}

impl SpannedSlotSeq {
    pub fn unspan(self) -> SlotSeq {
        SlotSeq { head: self.head.into_iter().map(|s| s.base).collect(),
                  tail: self.tail.map(|s| s.base) }
    }

    pub fn all_span(&self) -> Span {
        let mut span = Span::dummy();
        for head in &self.head { span |= head.span; }
        if let Some(ref tail) = self.tail { span |= tail.span; }
        span
    }

    pub fn all_with_loc<Loc: Into<Span>>(mut self, loc: Loc) -> SpannedSlotSeq {
        let span: Span = loc.into();
        for head in &mut self.head { head.span = span; }
        if let Some(ref mut tail) = self.tail { tail.span = span; }
        self
    }

    pub fn all_without_loc(self) -> SpannedSlotSeq {
        self.all_with_loc(Span::dummy())
    }
}

