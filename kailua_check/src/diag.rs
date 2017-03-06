use std::fmt;

use kailua_env::{Span, Spanned, WithLoc};
use kailua_diag::{self, ReportMore, Locale, Localize, Localized};
use kailua_syntax::Keyword;
use message as m;
use ty::TypeContext;

pub type CheckResult<T> = kailua_diag::Result<T>;

pub fn unquotable_name(s: &[u8]) -> bool {
    fn is_first(c: u8) -> bool {
        match c { b'_' | b'a'...b'z' | b'A'...b'Z' => true, _ => false }
    }

    fn is_next(c: u8) -> bool {
        match c { b'_' | b'a'...b'z' | b'A'...b'Z' | b'0'...b'9' => true, _ => false }
    }

    !s.is_empty() && is_first(s[0])
                  && s[1..].iter().all(|&c| is_next(c))
                  && Keyword::from(s, true).is_none()
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ordinal(pub usize);

impl Localize for Ordinal {
    fn fmt_localized(&self, f: &mut fmt::Formatter, locale: Locale) -> fmt::Result {
        match &locale[..] {
            "ko" => {
                let w = match self.0 {
                    0 => "첫번째",
                    1 => "두번째",
                    2 => "세번째",
                    3 => "네번째",
                    4 => "다섯번째",
                    5 => "여섯번째",
                    6 => "일곱번째",
                    7 => "여덟번째",
                    8 => "아홉번째",
                    i => return write!(f, "{}번째", i + 1),
                };
                write!(f, "{}", w)
            },

            _ => {
                let (wc, wl) = match self.0 {
                    0 => ("First",   "first"),
                    1 => ("Second",  "second"),
                    2 => ("Third",   "third"),
                    3 => ("Fourth",  "fourth"),
                    4 => ("Fifth",   "fifth"),
                    5 => ("Sixth",   "sixth"),
                    6 => ("Seventh", "seventh"),
                    7 => ("Eighth",  "eighth"),
                    8 => ("Ninth",   "ninth"),
                    i if i % 10 == 0 && i % 100 != 10 => return write!(f, "{}-st", i + 1),
                    i if i % 10 == 1 && i % 100 != 10 => return write!(f, "{}-nd", i + 1),
                    i if i % 10 == 2 && i % 100 != 10 => return write!(f, "{}-rd", i + 1),
                    i => return write!(f, "{}-th", i + 1),
                };
                if f.sign_plus() {
                    write!(f, "{}", wc)
                } else {
                    write!(f, "{}", wl)
                }
            },
        }
    }
}

// human-readable description of various types requiring the type context.
// expected to implement fmt::Display.
pub trait Display: fmt::Debug + Sized {
    fn fmt_displayed(&self, f: &mut fmt::Formatter,
                     locale: Locale, ctx: &TypeContext) -> fmt::Result;

    fn display<'b, 'c>(&'b self, ctx: &'c TypeContext) -> Displayed<'b, 'c, Self> {
        Displayed { base: self, ctx: ctx }
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt_displayed(&self, f: &mut fmt::Formatter,
                     locale: Locale, ctx: &TypeContext) -> fmt::Result {
        self.base.fmt_displayed(f, locale, ctx)
    }
}

impl<T: Display + ?Sized> Display for Box<T> {
    fn fmt_displayed(&self, f: &mut fmt::Formatter,
                     locale: Locale, ctx: &TypeContext) -> fmt::Result {
        (**self).fmt_displayed(f, locale, ctx)
    }
}

impl<'a, T: Display + ?Sized> Display for &'a T {
    fn fmt_displayed(&self, f: &mut fmt::Formatter,
                     locale: Locale, ctx: &TypeContext) -> fmt::Result {
        (**self).fmt_displayed(f, locale, ctx)
    }
}

impl<'a, T: Display + ?Sized> Display for &'a mut T {
    fn fmt_displayed(&self, f: &mut fmt::Formatter,
                     locale: Locale, ctx: &TypeContext) -> fmt::Result {
        (**self).fmt_displayed(f, locale, ctx)
    }
}

impl Display for String {
    fn fmt_displayed(&self, f: &mut fmt::Formatter,
                     _locale: Locale, _ctx: &TypeContext) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl<'a> Display for &'a str {
    fn fmt_displayed(&self, f: &mut fmt::Formatter,
                     _locale: Locale, _ctx: &TypeContext) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

pub struct Displayed<'b, 'c, T: Display + 'b> {
    base: &'b T,
    ctx: &'c TypeContext,
}

impl<'b, 'c, T: Display + 'b> Displayed<'b, 'c, T> {
    // a shortcut for `Localized::new` (hard to provide for Localize itself due to trait object)
    pub fn localized<'a>(&'a self, locale: Locale) -> Localized<'a, Displayed<'b, 'c, T>> {
        Localized::new(self, locale)
    }
}

impl<'b, 'c, T: Display + 'b> Localize for Displayed<'b, 'c, T> {
    fn fmt_localized(&self, f: &mut fmt::Formatter, locale: Locale) -> fmt::Result {
        self.base.fmt_displayed(f, locale, self.ctx)
    }
}

impl<'b, 'c, T: Display + fmt::Debug + 'b> fmt::Debug for Displayed<'b, 'c, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self.base, f)
    }
}

// type-level report is used to collect hierarchical information about failures.
// this is distinct from CheckResult which is non-hierarchical by nature.
#[derive(Clone, Debug)]
pub struct TypeReport {
    locale: Locale,
    messages: Vec<ReportItem>, // in the reverse chronological order
}

// a single report item can be composed of multiple errors at each layer of type operation.
// since showing all of them is not desirable, we use "origins" to distinguish each layer
// and to determine whether to flatten the item.
//
// for this reason origins are hierarchically ordered and the origin at higher classes can
// only be preceded by the origin at lower classes. if the origin "inversion" does occur,
// that would mean that an _irrelevant_ report item has been placed and should not be flattened.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Origin {
    RVar = 0x00, TVar = 0x01,
    Tables = 0x10, Numbers = 0x11, Strings = 0x12, Functions = 0x13,
    Union = 0x20,
    TUnion = 0x30, // binary operation between T and Union
    T = 0x40,
    TTy = 0x50, // binary operation between T and Ty
    Ty = 0x60,
    Slot = 0x70,
}

impl Origin {
    fn origin_class(&self) -> u8 {
        *self as u8 >> 4
    }

    pub fn can_overwrite(&self, prev: Origin) -> bool {
        let self_class = self.origin_class();
        let prev_class = prev.origin_class();
        self_class > prev_class
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum BinaryReportKind {
    NotSubtype,
    NotEqual,
    CannotUnion(bool /*explicit*/),
}

#[derive(Clone, Debug)]
enum ReportItem {
    Binary(BinaryReportKind, Origin, Spanned<String>, Spanned<String>, Option<usize>),
    Other(Origin, String),
}

impl TypeReport {
    pub fn new(locale: Locale) -> TypeReport {
        TypeReport { locale: locale, messages: Vec::new() }
    }

    pub fn put(mut self, org: Origin, s: String) -> TypeReport {
        self.messages.push(ReportItem::Other(org, s));
        self
    }

    pub fn put_with_locale<F: FnOnce(Locale) -> String>(self, org: Origin, f: F) -> TypeReport {
        let locale = self.locale;
        self.put(org, f(locale))
    }

    fn binary<T: Display, U: Display>(mut self, kind: BinaryReportKind, org: Origin,
                                      lhs: T, rhs: U, ctx: &TypeContext) -> TypeReport {
        trace!("TypeReport::binary({:?}, {:?}, {:?}, {:?})", kind, org, lhs, rhs);

        let locale = self.locale;
        let lhs = Localized::new(&lhs.display(ctx), locale).to_string();
        let rhs = Localized::new(&rhs.display(ctx), locale).to_string();

        let item = match self.messages.pop() {
            Some(ReportItem::Binary(kind_, org_, Spanned { span: lhsspan, .. },
                                                 Spanned { span: rhsspan, .. }, idx))
                if kind == kind_ && org.can_overwrite(org_) &&
                   lhsspan.is_dummy() && rhsspan.is_dummy()
            => {
                ReportItem::Binary(kind, org, lhs.with_loc(lhsspan), rhs.with_loc(rhsspan), idx)
            },
            last => {
                if let Some(last) = last {
                    self.messages.push(last); // push back an irrelevant item
                }
                ReportItem::Binary(kind, org, lhs.without_loc(), rhs.without_loc(), None)
            },
        };
        self.messages.push(item);
        self
    }

    fn binary_attach_span(mut self, kind: BinaryReportKind,
                          lhsspan: Span, rhsspan: Span) -> TypeReport {
        trace!("TypeReport::binary_attach_span({:?}, {:#?}, {:#?})", kind, lhsspan, rhsspan);

        match self.messages.last_mut() {
            Some(&mut ReportItem::Binary(kind_, _org, ref mut lhs, ref mut rhs, _idx))
                if kind == kind_ && lhs.span.is_dummy() && rhs.span.is_dummy()
            => {
                lhs.span = lhsspan;
                rhs.span = rhsspan;
            }
            last => {
                panic!("TypeReport::binary_attach_span({:?}, {:#?}, {:#?}) \
                        called with the last report item {:#?}", kind, lhsspan, rhsspan, last);
            }
        }
        self
    }

    fn binary_attach_index(mut self, kind: BinaryReportKind,
                           org: Origin, index: usize) -> TypeReport {
        trace!("TypeReport::binary_attach_index({:?}, {:?}, {})", kind, org, index);

        match self.messages.last_mut() {
            Some(&mut ReportItem::Binary(ref kind_, ref org_, _, _, ref mut idx))
                if kind == *kind_ && org == *org_ && idx.is_none()
            => {
                *idx = Some(index);
            }
            last => {
                panic!("TypeReport::binary_attach_index({:?}, {:?}, {}) \
                        called with the last report item {:#?}", kind, org, index, last);
            }
        }
        self
    }

    pub fn not_sub<T: Display, U: Display>(self, org: Origin, lhs: T, rhs: U,
                                           ctx: &TypeContext) -> TypeReport {
        self.binary(BinaryReportKind::NotSubtype, org, lhs, rhs, ctx)
    }

    pub fn not_sub_attach_span(self, lhsspan: Span, rhsspan: Span) -> TypeReport {
        self.binary_attach_span(BinaryReportKind::NotSubtype, lhsspan, rhsspan)
    }

    pub fn not_sub_attach_index(self, org: Origin, index: usize) -> TypeReport {
        self.binary_attach_index(BinaryReportKind::NotSubtype, org, index)
    }

    pub fn not_eq<T: Display, U: Display>(self, org: Origin, lhs: T, rhs: U,
                                          ctx: &TypeContext) -> TypeReport {
        self.binary(BinaryReportKind::NotEqual, org, lhs, rhs, ctx)
    }

    pub fn not_eq_attach_span(self, lhsspan: Span, rhsspan: Span) -> TypeReport {
        self.binary_attach_span(BinaryReportKind::NotEqual, lhsspan, rhsspan)
    }

    pub fn not_eq_attach_index(self, org: Origin, index: usize) -> TypeReport {
        self.binary_attach_index(BinaryReportKind::NotEqual, org, index)
    }

    pub fn cannot_union<T: Display, U: Display>(self, org: Origin, lhs: T, rhs: U, explicit: bool,
                                                ctx: &TypeContext) -> TypeReport {
        self.binary(BinaryReportKind::CannotUnion(explicit), org, lhs, rhs, ctx)
    }

    pub fn cannot_union_attach_span(self, lhsspan: Span, rhsspan: Span,
                                    explicit: bool) -> TypeReport {
        self.binary_attach_span(BinaryReportKind::CannotUnion(explicit), lhsspan, rhsspan)
    }

    pub fn cannot_union_attach_index(self, org: Origin, index: usize,
                                     explicit: bool) -> TypeReport {
        self.binary_attach_index(BinaryReportKind::CannotUnion(explicit), org, index)
    }

    pub fn cannot_assign<T: Display, U: Display>(self, org: Origin, t: T, u: U,
                                                 ctx: &TypeContext) -> TypeReport {
        let locale = self.locale;
        self.put(org, format!("cannot assign {} to {}",
                              Localized::new(&u.display(ctx), locale),
                              Localized::new(&t.display(ctx), locale)))
    }

    pub fn cannot_assign_in_place<T: Display>(self, org: Origin, t: T,
                                              ctx: &TypeContext) -> TypeReport {
        let locale = self.locale;
        self.put(org, format!("cannot update {}", Localized::new(&t.display(ctx), locale)))
    }

    pub fn cannot_filter_by_flags<T: Display>(self, org: Origin, t: T,
                                              ctx: &TypeContext) -> TypeReport {
        let locale = self.locale;
        self.put(org, format!("cannot filter {}", Localized::new(&t.display(ctx), locale)))
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TypeReportHint {
    None,
    FuncArgs,
    FuncReturns,
}

pub trait TypeReportMore {
    fn report_types(self, r: TypeReport, hint: TypeReportHint) -> Self;
}

impl<'a, T> TypeReportMore for ReportMore<'a, T> {
    fn report_types(mut self, r: TypeReport, mut hint: TypeReportHint) -> ReportMore<'a, T> {
        trace!("collected type reports: {:#?}", r);

        fn report_binary<
            'a, 'b, T,
            M: 'b + Localize, MA: 'b + Localize, MR: 'b + Localize,
            Msg, MsgArgs, MsgReturns
        >(
            more: ReportMore<'a, T>, lhs: &'b Spanned<String>, rhs: &'b Spanned<String>,
            idx: Option<usize>, hint: &mut TypeReportHint,
            make_msg: Msg, make_msg_in_args: MsgArgs, make_msg_in_returns: MsgReturns
        ) -> ReportMore<'a, T>
            where Msg: FnOnce(&'b str, &'b str) -> M,
                  MsgArgs: FnOnce(&'b str, &'b str, Ordinal) -> MA,
                  MsgReturns: FnOnce(&'b str, &'b str, Ordinal) -> MR
        {
            match idx {
                Some(idx) if *hint == TypeReportHint::FuncArgs => {
                    *hint = TypeReportHint::None;
                    if lhs.span.is_dummy() && !rhs.span.is_dummy() {
                        more.cause(rhs, make_msg_in_args(lhs, rhs, Ordinal(idx)))
                    } else {
                        more.cause(lhs, make_msg_in_args(lhs, rhs, Ordinal(idx)))
                            .note_if(rhs, m::OtherTypeOrigin {})
                    }
                },

                Some(idx) if *hint == TypeReportHint::FuncReturns => {
                    *hint = TypeReportHint::None;
                    if lhs.span.is_dummy() && !rhs.span.is_dummy() {
                        more.cause(rhs, make_msg_in_returns(lhs, rhs, Ordinal(idx)))
                    } else {
                        more.cause(lhs, make_msg_in_returns(lhs, rhs, Ordinal(idx)))
                            .note_if(rhs, m::OtherTypeOrigin {})
                    }
                },

                _ => {
                    if lhs.span.is_dummy() && !rhs.span.is_dummy() {
                        more.cause(rhs, make_msg(lhs, rhs))
                    } else {
                        more.cause(lhs, make_msg(lhs, rhs))
                            .note_if(rhs, m::OtherTypeOrigin {})
                    }
                },
            }
        }

        for item in r.messages.into_iter().rev() {
            match item {
                ReportItem::Binary(BinaryReportKind::NotSubtype, _org, ref sub, ref sup, idx) => {
                    self = report_binary(
                        self, sub, sup, idx, &mut hint,
                        |sub, sup| m::NotSubtype { sub: sub, sup: sup },
                        |sub, sup, i| m::NotSubtypeInFuncArgs { sub: sub, sup: sup, index: i },
                        |sub, sup, i| m::NotSubtypeInFuncReturns { sub: sub, sup: sup, index: i },
                    )
                },

                ReportItem::Binary(BinaryReportKind::NotEqual, _org, ref lhs, ref rhs, idx) => {
                    self = report_binary(
                        self, lhs, rhs, idx, &mut hint,
                        |lhs, rhs| m::NotEqual { lhs: lhs, rhs: rhs },
                        |lhs, rhs, i| m::NotEqualInFuncArgs { lhs: lhs, rhs: rhs, index: i },
                        |lhs, rhs, i| m::NotEqualInFuncReturns { lhs: lhs, rhs: rhs, index: i },
                    )
                },

                ReportItem::Binary(BinaryReportKind::CannotUnion(_explicit), _org,
                                   ref lhs, ref rhs, idx) => {
                    self = report_binary(
                        self, lhs, rhs, idx, &mut hint,
                        |lhs, rhs| m::InvalidUnionType { lhs: lhs, rhs: rhs },
                        |lhs, rhs, i| m::InvalidUnionTypeInFuncArgs { lhs: lhs, rhs: rhs,
                                                                      index: i },
                        |lhs, rhs, i| m::InvalidUnionTypeInFuncReturns { lhs: lhs, rhs: rhs,
                                                                         index: i },
                    )
                },

                ReportItem::Other(_org, msg) => {
                    self = self.note(Span::dummy(), format!("XXX {}", msg));
                }
            }
        }

        self
    }
}

pub type TypeResult<T> = Result<T, TypeReport>;

