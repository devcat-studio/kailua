use std::cmp;
use std::fmt;
use std::vec;
use std::usize;
use std::iter;

use kailua_env::{Span, Spanned, WithLoc};
use kailua_syntax::{Seq, Kind};
use diag::CheckResult;
use super::{T, Ty, Slot, Lattice, Union, Display};
use super::{TypeContext, TypeResolver};

pub struct SeqIter<Item: Clone> {
    head: vec::IntoIter<Item>,
    tail: Option<Item>,
}

impl<Item: Clone> Iterator for SeqIter<Item> {
    type Item = Item;

    fn next(&mut self) -> Option<Item> {
        self.head.next().or_else(|| self.tail.clone())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.tail.is_some() { (usize::MAX, None) } else { self.head.size_hint() }
    }
}

pub type SeqIterWithNil<Item> =
    iter::Chain<SeqIter<Item>, iter::Repeat<Item>>;

pub type SeqIterWithNone<Item> =
    iter::Chain<iter::Map<SeqIter<Item>, fn(Item) -> Option<Item>>, iter::Repeat<Option<Item>>>;

macro_rules! define_tyseq {
    ($(
        type $tyseq:ident {
            t = [$($tt:tt)*] $t:ty;
            ty = $ty:ty;

            make_nil_ty = $make_nil_ty:expr;
            dummy_ty = $dummy_ty:expr;
            ty_union = $ty_union:expr;
            t_to_ty = $t_to_ty:expr;
            spanned_kind_to_ty = $spanned_kind_to_ty:expr;
            ty_with_nil = $ty_with_nil:expr;

            $(span $span:ident: $spanty:ty {
                dummy = $dummy_span:expr;
                union = $span_union:expr;
                from_t = $t_to_span:expr;
                from_ty = $ty_to_span:expr;
            };)*
        }
    )*) => ($(
        #[derive(Clone, PartialEq)]
        pub struct $tyseq {
            pub head: Vec<$ty>,

            // implicitly unioned with `nil` in every possible handling.
            // why? the TySeq (and others) is never infinite,
            // so there should be a `nil` somewhere. since we don't know where it ends,
            // the tail is implicitly unioned with `nil` in all practical sense.
            //
            // None is same to Some(Ty::noisy_nil()) but has a display hint
            // and has a different behavior for the arity calculation.
            // (we may need this because, well, C/C++ API *can* count the proper number of args.
            // TODO this is not yet enforced due to a number of concerns)
            pub tail: Option<$ty>,

            // this span is used for finer diagnostics
            $(pub $span: $spanty,)*
        }

        impl<$($tt)*> From<$t> for $tyseq {
            fn from(t: $t) -> $tyseq {
                $(let $span = $t_to_span(&t);)*
                $tyseq { head: vec![$t_to_ty(t)], tail: None, $($span: $span,)* }
            }
        }

        impl From<$ty> for $tyseq {
            fn from(t: $ty) -> $tyseq {
                $(let $span = $ty_to_span(&t);)*
                $tyseq { head: vec![t], tail: None, $($span: $span,)* }
            }
        }

        impl $tyseq {
            pub fn new($($span: $spanty,)*) -> $tyseq {
                $tyseq { head: Vec::new(), tail: None, $($span: $span,)* }
            }

            pub fn from_kind_seq(seq: &Seq<Spanned<Kind>>,
                                 resolv: &mut TypeResolver,
                                 $($span: $spanty,)*) -> CheckResult<$tyseq> {
                let head = seq.head.iter()
                                   .map(|k| $spanned_kind_to_ty(k, resolv))
                                   .collect::<Result<_,_>>()?;
                let tail = if let Some(ref k) = seq.tail {
                    Some($spanned_kind_to_ty(k, resolv)?)
                } else {
                    None
                };
                Ok($tyseq { head: head, tail: tail, $($span: $span,)* })
            }

            pub fn dummy() -> $tyseq {
                $tyseq { head: Vec::new(), tail: Some($dummy_ty), $($span: $dummy_span,)* }
            }

            fn tail_to_type(&self) -> $ty {
                if let Some(ref t) = self.tail {
                    $ty_with_nil(t.clone())
                } else {
                    $make_nil_ty($(self.$span,)*)
                }
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
                SeqIter { head: self.head.into_iter(), tail: self.tail.map($ty_with_nil) }
            }

            pub fn into_iter_with_none(self) -> SeqIterWithNone<$ty> {
                self.into_iter().map(Some::<$ty> as fn(_) -> _).chain(iter::repeat(None::<$ty>))
            }

            pub fn into_iter_with_nil(self) -> SeqIterWithNil<$ty> {
                let nil = $make_nil_ty($(self.$span,)*);
                self.into_iter().chain(iter::repeat(nil))
            }

            pub fn into_first(self) -> $ty {
                if let Some(head) = self.head.into_iter().next() { return head; }
                if let Some(tail) = self.tail { return $ty_with_nil(tail); }
                $make_nil_ty($(self.$span,)*)
            }

            fn fmt_generic<WriteTy>(&self, f: &mut fmt::Formatter,
                                    mut write_ty: WriteTy) -> fmt::Result
                    where WriteTy: FnMut(&$ty, &mut fmt::Formatter, bool) -> fmt::Result {
                write!(f, "(")?;
                let mut first = true;
                for t in &self.head {
                    if first { first = false; } else { write!(f, ", ")?; }
                    write_ty(t, f, false)?;
                }
                if let Some(ref t) = self.tail {
                    if !first { write!(f, ", ")?; }
                    write_ty(t, f, true)?;
                    write!(f, "...")?;
                }
                write!(f, ")")?;
                $(fmt::Debug::fmt(&self.$span, f)?;)*
                Ok(())
            }
        }

        impl Union for $tyseq {
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
                    Some($ty_union(&selftail, &othertail, ctx))
                } else {
                    None
                };

                $tyseq { head: head, tail: tail, $($span: $span_union(self.$span, other.$span),)* }
            }
        }

        impl Lattice for $tyseq {
            fn assert_sub(&self, other: &$tyseq, ctx: &mut TypeContext) -> CheckResult<()> {
                debug!("asserting a constraint {:?} <: {:?}", *self, *other);

                let mut selfhead = self.head.iter().fuse();
                let mut otherhead = other.head.iter().fuse();
                let selftail = self.tail_to_type();
                let othertail = other.tail_to_type();
                loop {
                    match (selfhead.next(), otherhead.next()) {
                        (Some(a), Some(b)) => a.assert_sub(b, ctx)?,
                        (Some(a), None) => a.assert_sub(&othertail, ctx)?,
                        (None, Some(b)) => selftail.assert_sub(b, ctx)?,
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
                        (Some(a), Some(b)) => a.assert_eq(b, ctx)?,
                        (Some(a), None) => a.assert_eq(&othertail, ctx)?,
                        (None, Some(b)) => selftail.assert_eq(b, ctx)?,
                        (None, None) => return selftail.assert_eq(&othertail, ctx),
                    }
                }
            }
        }

        impl Display for $tyseq {
            fn fmt_displayed(&self, f: &mut fmt::Formatter, ctx: &TypeContext) -> fmt::Result {
                self.fmt_generic(f, |t, f, without_nil| {
                    let t = t.display(ctx);
                    if without_nil { write!(f, "{:#}", t) } else { write!(f, "{}", t) }
                })
            }
        }

        impl fmt::Debug for $tyseq {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.fmt_generic(f, |t, f, without_nil| {
                    if without_nil { write!(f, "{:#?}", t) } else { write!(f, "{:?}", t) }
                })
            }
        }
    )*)
}

macro_rules! define_slotseq {
    ($(
        type $slotseq:ident <- $tyseq:ident {
            slot = $slot:ty;
            t = [$($tt:tt)*] $t:ty;
            ty = $ty:ty;

            make_nil_slot = $make_nil_slot:expr;
            dummy_slot = $dummy_slot:expr;
            slot_union = $slot_union:expr;
            t_to_slot = $t_to_slot:expr;
            ty_to_slot = $ty_to_slot:expr;
            slot_to_ty = $slot_to_ty:expr;
            slot_with_nil = $slot_with_nil:expr;

            $(span $span:ident: $spanty:ty {
                dummy = $dummy_span:expr;
                union = $span_union:expr;
                from_slot = $slot_to_span:expr;
            };)*
        }
    )*) => ($(
        #[derive(Clone, PartialEq)]
        pub struct $slotseq {
            pub head: Vec<$slot>,
            pub tail: Option<$slot>,
            $(pub $span: $spanty,)*
        }

        impl<$($tt)*> From<$t> for $slotseq {
            fn from(t: $t) -> $slotseq {
                $slotseq::from($t_to_slot(t))
            }
        }

        impl From<$ty> for $slotseq {
            fn from(t: $ty) -> $slotseq {
                $slotseq::from($ty_to_slot(t))
            }
        }

        impl From<$slot> for $slotseq {
            fn from(s: $slot) -> $slotseq {
                $(let $span = $slot_to_span(&s);)*
                $slotseq { head: vec![s], tail: None, $($span: $span,)* }
            }
        }

        impl $slotseq {
            pub fn new($($span: $spanty,)*) -> $slotseq {
                $slotseq { head: Vec::new(), tail: None, $($span: $span,)* }
            }

            pub fn from_seq(seq: $tyseq) -> $slotseq {
                $slotseq { head: seq.head.into_iter().map(|t| $ty_to_slot(t)).collect(),
                           tail: seq.tail.map(|t| $ty_to_slot(t)),
                           $($span: seq.$span,)* }
            }

            pub fn dummy() -> $slotseq {
                $slotseq { head: Vec::new(), tail: Some($dummy_slot), $($span: $dummy_span,)* }
            }

            fn tail_to_slot(&self) -> $slot {
                if let Some(ref s) = self.tail {
                    $slot_with_nil(s.clone())
                } else {
                    $make_nil_slot($(self.$span,)*)
                }
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
                SeqIter { head: self.head.into_iter(), tail: self.tail.map($slot_with_nil) }
            }

            pub fn into_iter_with_none(self) -> SeqIterWithNone<$slot> {
                self.into_iter().map(Some::<$slot> as fn(_) -> _).chain(iter::repeat(None::<$slot>))
            }

            pub fn into_iter_with_nil(self) -> SeqIterWithNil<$slot> {
                let nil = $make_nil_slot($(self.$span,)*);
                self.into_iter().chain(iter::repeat(nil))
            }

            pub fn into_first(self) -> $slot {
                if let Some(head) = self.head.into_iter().next() { return head; }
                if let Some(tail) = self.tail { return $slot_with_nil(tail); }
                $make_nil_slot($(self.$span,)*)
            }

            pub fn unlift(self) -> $tyseq {
                $tyseq { head: self.head.into_iter().map(|s| $slot_to_ty(s)).collect(),
                         tail: self.tail.map(|s| $slot_to_ty(s)),
                         $($span: self.$span,)* }
            }

            fn fmt_generic<WriteSlot>(&self, f: &mut fmt::Formatter,
                                      mut write_slot: WriteSlot) -> fmt::Result
                    where WriteSlot: FnMut(&$slot, &mut fmt::Formatter, bool) -> fmt::Result {
                write!(f, "(")?;
                let mut first = true;
                for t in &self.head {
                    if first { first = false; } else { write!(f, ", ")?; }
                    write_slot(t, f, false)?;
                }
                if let Some(ref t) = self.tail {
                    if !first { write!(f, ", ")?; }
                    write_slot(t, f, true)?;
                    write!(f, "...")?;
                }
                write!(f, ")")?;
                $(fmt::Debug::fmt(&self.$span, f)?;)*
                Ok(())
            }
        }

        impl Union for $slotseq {
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
                    Some($slot_union(&selftail, &othertail, ctx))
                } else {
                    None
                };

                $slotseq { head: head, tail: tail,
                           $($span: $span_union(self.$span, other.$span),)* }
            }
        }

        impl Lattice for $slotseq {
            fn assert_sub(&self, other: &$slotseq, ctx: &mut TypeContext) -> CheckResult<()> {
                debug!("asserting a constraint {:?} <: {:?}", *self, *other);

                let mut selfhead = self.head.iter().fuse();
                let mut otherhead = other.head.iter().fuse();
                let selftail = self.tail_to_slot();
                let othertail = other.tail_to_slot();
                loop {
                    match (selfhead.next(), otherhead.next()) {
                        (Some(a), Some(b)) => a.assert_sub(b, ctx)?,
                        (Some(a), None) => a.assert_sub(&othertail, ctx)?,
                        (None, Some(b)) => selftail.assert_sub(b, ctx)?,
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
                        (Some(a), Some(b)) => a.assert_eq(b, ctx)?,
                        (Some(a), None) => a.assert_eq(&othertail, ctx)?,
                        (None, Some(b)) => selftail.assert_eq(b, ctx)?,
                        (None, None) => return selftail.assert_eq(&othertail, ctx),
                    }
                }
            }
        }

        impl Display for $slotseq {
            fn fmt_displayed(&self, f: &mut fmt::Formatter, ctx: &TypeContext) -> fmt::Result {
                self.fmt_generic(f, |s, f, without_nil| {
                    let s = s.display(ctx);
                    if without_nil { write!(f, "{:#}", s) } else { write!(f, "{}", s) }
                })
            }
        }

        impl fmt::Debug for $slotseq {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.fmt_generic(f, |s, f, without_nil| {
                    if without_nil { write!(f, "{:#?}", s) } else { write!(f, "{:?}", s) }
                })
            }
        }
    )*)
}

define_tyseq! {
    type TySeq {
        t = ['a] T<'a>;
        ty = Ty;

        make_nil_ty = || Ty::noisy_nil();
        dummy_ty = Ty::dummy();
        ty_union = |lhs: &Ty, rhs: &Ty, ctx| lhs.union(rhs, ctx);
        t_to_ty = |t: T| Ty::new(t.into_send());
        spanned_kind_to_ty = |k: &Spanned<Kind>, resolv| Ty::from_kind(k, resolv);
        ty_with_nil = |t: Ty| t.with_nil();
    }

    type SpannedTySeq {
        t = ['a] Spanned<T<'a>>;
        ty = Spanned<Ty>;

        make_nil_ty = |span: Span| Ty::noisy_nil().with_loc(span.end());
        dummy_ty = Ty::dummy().without_loc();
        ty_union = |lhs: &Spanned<Ty>, rhs: &Spanned<Ty>, ctx|
            lhs.base.union(&rhs.base, ctx).without_loc();
        t_to_ty = |t: Spanned<T>| t.map(|t| Ty::new(t.into_send()));
        spanned_kind_to_ty = |k: &Spanned<Kind>, resolv|
            Ty::from_kind(&k.base, resolv).map(|t| t.with_loc(k));
        ty_with_nil = |t: Spanned<Ty>| t.map(|t| t.with_nil());

        span span: Span {
            dummy = Span::dummy();
            union = |_lhs, _rhs| Span::dummy(); // do not try to merge them
            from_t = |t: &Spanned<T>| t.span;
            from_ty = |t: &Spanned<Ty>| t.span;
        };
    }
}

define_slotseq! {
    type SlotSeq <- TySeq {
        slot = Slot;
        t = ['a] T<'a>;
        ty = Ty;

        make_nil_slot = || Slot::just(Ty::noisy_nil());
        dummy_slot = Slot::dummy();
        slot_union = |lhs: &Slot, rhs: &Slot, ctx| lhs.union(rhs, ctx);
        t_to_slot = |t: T| Slot::just(Ty::new(t.into_send()));
        ty_to_slot = |t: Ty| Slot::just(t);
        slot_to_ty = |s: Slot| s.unlift().clone();
        slot_with_nil = |s: Slot| s.with_nil();
    }

    type SpannedSlotSeq <- SpannedTySeq {
        slot = Spanned<Slot>;
        t = ['a] Spanned<T<'a>>;
        ty = Spanned<Ty>;

        make_nil_slot = |span: Span| Slot::just(Ty::noisy_nil()).with_loc(span.end());
        dummy_slot = Slot::dummy().without_loc();
        slot_union = |lhs: &Spanned<Slot>, rhs: &Spanned<Slot>, ctx|
            lhs.base.union(&rhs.base, ctx).without_loc();
        t_to_slot = |t: Spanned<T>| t.map(|t| Slot::just(Ty::new(t.into_send())));
        ty_to_slot = |t: Spanned<Ty>| t.map(|t| Slot::just(t));
        slot_to_ty = |s: Spanned<Slot>| s.map(|s| s.unlift().clone());
        slot_with_nil = |s: Spanned<Slot>| s.map(|s| s.with_nil());

        span span: Span {
            dummy = Span::dummy();
            union = |_lhs, _rhs| Span::dummy(); // do not try to merge them
            from_slot = |s: &Spanned<Slot>| s.span;
        };
    }
}

impl TySeq {
    pub fn all_with_loc<Loc: Into<Span>>(self, loc: Loc) -> SpannedTySeq {
        let span: Span = loc.into();
        SpannedTySeq { head: self.head.into_iter().map(|t| t.with_loc(span)).collect(),
                       tail: self.tail.map(|t| t.with_loc(span)),
                       span: span }
    }

    pub fn all_without_loc(self) -> SpannedTySeq {
        self.all_with_loc(Span::dummy())
    }
}

impl SlotSeq {
    pub fn all_with_loc<Loc: Into<Span>>(self, loc: Loc) -> SpannedSlotSeq {
        let span: Span = loc.into();
        SpannedSlotSeq { head: self.head.into_iter().map(|s| s.with_loc(span)).collect(),
                         tail: self.tail.map(|s| s.with_loc(span)),
                         span: span }
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

