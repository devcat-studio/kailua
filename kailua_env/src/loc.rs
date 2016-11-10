use std::ops;
use std::cmp;
use std::fmt;
use std::borrow::Borrow;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unit {
    unit: u32,
}

// internal use only, not exposed outside
pub fn unit_from_u32(unit: u32) -> Unit {
    Unit { unit: unit }
}

const BUILTIN_UNIT: u32 = 0xffffffff;

impl Unit {
    pub fn dummy() -> Unit {
        Unit { unit: 0 }
    }

    pub fn builtin() -> Unit {
        Unit { unit: BUILTIN_UNIT }
    }

    pub fn is_dummy(&self) -> bool {
        self.unit == 0
    }

    pub fn is_source_dependent(&self) -> bool {
        self.unit > 0 && self.unit < BUILTIN_UNIT
    }

    pub fn to_usize(&self) -> usize {
        self.unit as usize
    }
}

impl fmt::Debug for Unit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            if self.unit == 0 {
                write!(f, "@_")
            } else if self.unit == BUILTIN_UNIT {
                write!(f, "@<builtin>")
            } else {
                write!(f, "@{}", self.unit)
            }
        } else {
            Ok(())
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos {
    unit: u32,
    pos: u32,
}

// internal use only, not exposed outside
pub fn pos_from_u32(unit: Unit, pos: u32) -> Pos {
    Pos { unit: unit.unit, pos: pos }
}

impl Pos {
    pub fn dummy() -> Pos {
        Pos { unit: 0, pos: 0 }
    }

    pub fn builtin() -> Pos {
        Pos { unit: BUILTIN_UNIT, pos: 0 }
    }

    pub fn is_dummy(&self) -> bool {
        self.unit().is_dummy()
    }

    pub fn is_source_dependent(&self) -> bool {
        self.unit().is_source_dependent()
    }

    pub fn unit(&self) -> Unit {
        Unit { unit: self.unit }
    }

    pub fn to_usize(&self) -> usize {
        self.pos as usize
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            if self.unit == 0 {
                write!(f, "@_")
            } else if self.unit == BUILTIN_UNIT {
                write!(f, "@<builtin>")
            } else {
                write!(f, "@{}/{}", self.unit, self.pos)
            }
        } else {
            Ok(())
        }
    }
}

// span (0, 0, 0) is dummy and indicates the absence of appropriate span infos.
// span (0, y, z) for non-zero y and z is reserved.
// span (x, y, y) for non-zero x and y indicates a point and can be lifted from Pos.
// span (x, y, z) for non-zero x, y and z (y < z) is an ordinary span, with z exclusive.
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Span {
    unit: u32,
    begin: u32,
    end: u32,
}

// internal use only, not exposed outside
pub fn span_from_u32(unit: Unit, begin: u32, end: u32) -> Span {
    Span { unit: unit.unit, begin: begin, end: end }
}

impl Span {
    pub fn new(begin: Pos, end: Pos) -> Span {
        if begin.is_dummy() || end.is_dummy() {
            Span::dummy()
        } else {
            assert!(begin.unit == end.unit, "Span::new with positions from different units");
            if begin.pos <= end.pos {
                Span { unit: begin.unit, begin: begin.pos, end: end.pos }
            } else {
                // this is possible when the range actually describes an empty span.
                // in the ordinary case we take the beginning of the first token and
                // the end of the last token for the span:
                //
                // function f()    FIRST_TOKEN ... LAST_TOKEN    end
                //                 ^ begin               end ^
                //
                // but if the span is empty, the order is swapped:
                //
                // function f()    end
                //         end ^   ^ begin
                //
                // the most reasonable choice here would be using (end..begin)
                // as an indication of the empty span.
                Span { unit: begin.unit, begin: end.pos, end: begin.pos }
            }
        }
    }

    pub fn dummy() -> Span {
        Span { unit: 0, begin: 0, end: 0 }
    }

    pub fn builtin() -> Span {
        Span { unit: BUILTIN_UNIT, begin: 0, end: 0 }
    }

    pub fn is_dummy(&self) -> bool {
        self.unit().is_dummy()
    }

    pub fn is_source_dependent(&self) -> bool {
        self.unit().is_source_dependent()
    }

    pub fn to_pos(&self) -> Pos {
        if self.begin == self.end {
            Pos { unit: self.unit, pos: self.begin }
        } else {
            Pos::dummy()
        }
    }

    pub fn unit(&self) -> Unit {
        Unit { unit: self.unit }
    }

    pub fn begin(&self) -> Pos {
        Pos { unit: self.unit, pos: self.begin }
    }

    pub fn end(&self) -> Pos {
        Pos { unit: self.unit, pos: self.end }
    }

    pub fn len(&self) -> usize {
        if self.is_source_dependent() {
            0
        } else {
            (self.end - self.begin) as usize
        }
    }

    pub fn contains(&self, pos: Pos) -> bool {
        self.unit > 0 && self.unit == pos.unit && self.begin <= pos.pos && pos.pos < self.end
    }

    pub fn contains_or_end(&self, pos: Pos) -> bool {
        self.unit > 0 && self.unit == pos.unit && self.begin <= pos.pos && pos.pos <= self.end
    }
}

impl ops::BitAnd for Span {
    type Output = Span;
    fn bitand(self, other: Span) -> Span {
        if self.is_dummy() || other.is_dummy() { return Span::dummy(); }
        if self.unit == other.unit {
            let begin = cmp::max(self.begin, other.begin);
            let end = cmp::min(self.end, other.end);
            if begin > end { return Span::dummy(); }
            Span { unit: self.unit, begin: begin, end: end }
        } else {
            Span::dummy()
        }
    }
}

impl ops::BitAndAssign for Span {
    fn bitand_assign(&mut self, other: Span) { *self = *self & other; }
}

impl ops::BitOr for Span {
    type Output = Span;
    fn bitor(self, other: Span) -> Span {
        if self.is_dummy() { return other; }
        if other.is_dummy() { return self; }
        if self.unit == other.unit {
            Span {
                unit: self.unit,
                begin: cmp::min(self.begin, other.begin),
                end: cmp::max(self.end, other.end),
            }
        } else {
            Span::dummy()
        }
    }
}

impl ops::BitOrAssign for Span {
    fn bitor_assign(&mut self, other: Span) { *self = *self | other; }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            if self.unit == 0 {
                write!(f, "@_")
            } else if self.unit == BUILTIN_UNIT {
                write!(f, "@<builtin>")
            } else if self.begin == self.end {
                write!(f, "@{}/{}", self.unit, self.begin)
            } else {
                write!(f, "@{}/{}-{}", self.unit, self.begin, self.end)
            }
        } else {
            Ok(())
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub span: Span,
    pub base: T,
}

impl<T> Spanned<T> {
    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned { span: self.span, base: &self.base }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned { span: self.span, base: f(self.base) }
    }
}

impl From<Pos> for Span {
    fn from(pos: Pos) -> Span {
        Span { unit: pos.unit, begin: pos.pos, end: pos.pos }
    }
}

impl From<ops::Range<Pos>> for Span {
    fn from(range: ops::Range<Pos>) -> Span {
        Span::new(range.start, range.end)
    }
}

impl<T> From<Spanned<T>> for Span {
    fn from(spanned: Spanned<T>) -> Span { spanned.span }
}

impl<'a, T> From<&'a Spanned<T>> for Span {
    fn from(spanned: &'a Spanned<T>) -> Span { spanned.span }
}

impl<'a, T> From<&'a mut Spanned<T>> for Span {
    fn from(spanned: &'a mut Spanned<T>) -> Span { spanned.span }
}

impl<T> ops::Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &T { &self.base }
}

impl<T> ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut T { &mut self.base }
}

impl<T> Borrow<T> for Spanned<T> {
    fn borrow(&self) -> &T { &self.base }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.base, f)
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(fmt::Debug::fmt(&self.base, f));
        try!(fmt::Debug::fmt(&self.span, f));
        Ok(())
    }
}

pub trait WithLoc: Sized {
    fn with_loc<Loc: Into<Span>>(self, loc: Loc) -> Spanned<Self> {
        Spanned { span: loc.into(), base: self }
    }

    fn without_loc(self) -> Spanned<Self> {
        Spanned { span: Span::dummy(), base: self }
    }
}

impl<T> WithLoc for T {}

