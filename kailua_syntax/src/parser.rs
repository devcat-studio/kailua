use std::iter;
use std::i32;
use std::fmt;
use std::result;
use std::collections::{hash_map, HashMap};

use kailua_diag as diag;
use kailua_diag::{Pos, Span, Spanned, WithLoc, Report, Localize};
use message as m;
use lex::{Tok, Punct, Keyword};
use ast::{Name, Str, Var, Seq, Presig, Sig, Attr};
use ast::{Ex, Exp, UnOp, BinOp, NameScope, SelfParam, St, Stmt, Block};
use ast::{M, K, Kind, SlotKind, FuncKind, TypeSpec};

pub struct Parser<'a, T> {
    iter: iter::Fuse<T>,

    // the lookahead stream (in this order)
    elided_newline: Option<Span>, // same to Side.elided_tokens below
    lookahead: Option<Spanned<Tok>>,
    lookahead2: Option<Spanned<Tok>>,
    // ...follows self.iter.next()

    // the spans for the most recent `read()` tokens
    last_span: Span,
    last_span2: Span,

    ignore_after_newline: Option<Punct>,
    report: &'a Report,

    // a list of delimiting pairs which contain the current cursor
    open_nestings: Vec<Nesting>,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Nesting {
    // implicitly starts, ends with the EOF
    // used to simplify the handling of EOF
    Top,
    // starts with meta token, ends with newline or other meta token (opens another nesting)
    Meta,
    // starts with `(`, ends with `)`
    Paren,
    // starts with `{`, ends with `}`
    Brace,
    // starts with `[`, ends with `]`
    Bracket,
    // starts with `for` or `while`, ends with `do`; normally inside Nesting::End
    Do,
    // starts with `if` or `elseif`, ends with `then`; normally inside Nesting::Else
    Then,
    // starts with `if` or `elseif`, ends with `elseif` or `else`; normally inside Nesting::End
    Else,
    // starts with any block statement, ends with `end`
    End,
    // starts with `repeat`, ends with `until`
    Until,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct EOF; // a placeholder arg to `expect`

#[must_use]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Stop {
    // this cannot be recovered and should terminate immediately
    Fatal,
    // the callee will do its best to clean things up,
    // the caller can choose to recover or terminate immediately
    Recover,
}

impl From<diag::Stop> for Stop {
    fn from(_: diag::Stop) -> Stop { Stop::Fatal }
}

// parser keeps the internal information to recover as much as possible
type Result<T> = result::Result<T, Stop>;

impl Localize for EOF {
    fn fmt_localized(&self, f: &mut fmt::Formatter, lang: &str) -> fmt::Result {
        match lang {
            "ko" => write!(f, "파일의 끝"),
            _ => write!(f, "end of file"),
        }
    }
}

pub trait Expectable: Localize {
    fn check_token(&self, tok: &Tok) -> bool;
}

impl Expectable for Punct {
    fn check_token(&self, tok: &Tok) -> bool { tok == &Tok::Punct(*self) }
}

impl Expectable for Keyword {
    fn check_token(&self, tok: &Tok) -> bool { tok == &Tok::Keyword(*self) }
}

impl Expectable for EOF {
    fn check_token(&self, tok: &Tok) -> bool { tok == &Tok::EOF }
}

trait Recover {
    fn recover() -> Self;
}

impl Recover for () {
    fn recover() -> Self { () }
}

impl Recover for K {
    fn recover() -> Self { K::Oops }
}

impl Recover for Ex {
    fn recover() -> Self { Ex::Oops }
}

impl Recover for St {
    fn recover() -> Self { St::Oops }
}

impl<T> Recover for Option<T> {
    fn recover() -> Self { None }
}

impl<T> Recover for Vec<T> {
    fn recover() -> Self { Vec::new() }
}

impl<T: Recover> Recover for Spanned<T> {
    fn recover() -> Self {
        let v: T = Recover::recover();
        v.without_loc()
    }
}

impl<T: Recover> Recover for Box<T> {
    fn recover() -> Self { Box::new(Recover::recover()) }
}

macro_rules! lastly {
    (|$i:ident| $e:expr $(=> $f:expr)+) => (
        |$i| {
            let ret = try!($e);
            $(try!($f);)+
            Ok(ret)
        }
    )
}

// `read()` returns this side information along with the token;
// `unread()` should have been given the same side information.
#[derive(Clone, Debug)]
struct Side {
    // represents a sequence of elided tokens (one or more (newline + meta begin)s) when Some.
    // the span is used to reconstruct the token for meta beginning.
    elided_tokens: ElidedTokens,

    // delta information required for rolling parser.open_nestings back
    nesting_delta: NestingDelta,
}

#[derive(Copy, Clone, Debug)]
struct ElidedTokens(Option<Span>);

#[derive(Clone, Debug)]
struct NestingDelta {
    removed: Vec<Nesting>,
    added: usize,
}

enum AtomicKind { One(Spanned<Kind>), Seq(Seq<Spanned<Kind>>) }

impl<'a, T> Report for Parser<'a, T> {
    fn add_span(&self, k: diag::Kind, s: Span, m: &Localize) -> diag::Result<()> {
        self.report.add_span(k, s, m)
    }
}

// wrappers around kailua_diag::{ReportMore, Reporter}, used to remap `done` method
#[must_use]
struct ReportMore<'a, T>(diag::ReportMore<'a, T>);

#[allow(dead_code)]
impl<'a, U> Parser<'a, U> {
    fn fatal<Loc: Into<Span>, Msg: Localize, T>(&self, loc: Loc, msg: Msg) -> ReportMore<T> {
        ReportMore(diag::Reporter::fatal(self, loc, msg))
    }

    fn error<Loc: Into<Span>, Msg: Localize>(&self, loc: Loc, msg: Msg) -> ReportMore<()> {
        ReportMore(diag::Reporter::error(self, loc, msg))
    }

    fn warn<Loc: Into<Span>, Msg: Localize>(&self, loc: Loc, msg: Msg) -> ReportMore<()> {
        ReportMore(diag::Reporter::warn(self, loc, msg))
    }

    fn note<Loc: Into<Span>, Msg: Localize>(&self, loc: Loc, msg: Msg) -> ReportMore<()> {
        ReportMore(diag::Reporter::note(self, loc, msg))
    }
}

#[allow(dead_code)]
impl<'a, T> ReportMore<'a, T> {
    fn fatal<Loc: Into<Span>, Msg: Localize, U>(self, loc: Loc, msg: Msg) -> ReportMore<'a, U> {
        ReportMore(self.0.fatal(loc, msg))
    }

    fn error<Loc: Into<Span>, Msg: Localize>(self, loc: Loc, msg: Msg) -> ReportMore<'a, T> {
        ReportMore(self.0.error(loc, msg))
    }

    fn warn<Loc: Into<Span>, Msg: Localize>(self, loc: Loc, msg: Msg) -> ReportMore<'a, T> {
        ReportMore(self.0.warn(loc, msg))
    }

    fn note<Loc: Into<Span>, Msg: Localize>(self, loc: Loc, msg: Msg) -> ReportMore<'a, T> {
        ReportMore(self.0.note(loc, msg))
    }

    fn note_if<Loc: Into<Span>, Msg: Localize>(self, loc: Loc, msg: Msg) -> ReportMore<'a, T> {
        ReportMore(self.0.note_if(loc, msg))
    }

    fn done(self) -> Result<T> {
        self.0.done().map_err(|_| Stop::Fatal)
    }
}

impl<'a, T: Iterator<Item=Spanned<Tok>>> Parser<'a, T> {
    pub fn new(iter: T, report: &'a Report) -> Parser<'a, T> {
        let mut parser = Parser {
            iter: iter.fuse(),
            elided_newline: None,
            lookahead: None,
            lookahead2: None,
            last_span: Span::dummy(),
            last_span2: Span::dummy(),
            ignore_after_newline: None,
            report: report,
            open_nestings: vec![Nesting::Top],
        };
        // read the first token and fill the last_span
        let first = parser._next().expect("lexer gave no token");
        parser.last_span = first.span.begin().into();
        parser.lookahead = Some(first);
        parser
    }

    fn _next(&mut self) -> Option<Spanned<Tok>> {
        loop {
            let next = self.iter.next();

            if let Some(ref t) = next {
                trace!("got {:?}", *t);
                if false { // useful for debugging
                    let _ = self.note(t.span, format!("got {:?}", t.base)).done();
                }
            }

            // comments should be ignored in the parser
            if let Some(Spanned { base: Tok::Comment, .. }) = next { continue; }
            return next;
        }
    }

    fn _read(&mut self) -> (ElidedTokens, Spanned<Tok>) {
        let mut next = self.lookahead.take().or_else(|| self.lookahead2.take())
                                            .or_else(|| self._next());

        let mut elided = self.elided_newline;
        self.elided_newline = None;
        if let Some(meta) = self.ignore_after_newline {
            // lookahead2 can definitely be made empty, let's simplify the assumption
            assert_eq!(self.lookahead, None);
            self.lookahead = self.lookahead2.take();

            while next.as_ref().map(|t| &t.base) == Some(&Tok::Punct(Punct::Newline)) {
                let next2 = self.lookahead.take().or_else(|| self._next());
                if next2.as_ref().map(|t| &t.base) == Some(&Tok::Punct(meta)) {
                    // we can ignore them, but we may have another ignorable tokens there
                    elided = Some(next2.unwrap().span);
                    assert_eq!(self.lookahead, None); // yeah, we are now sure
                    next = self._next();
                } else {
                    // return next, store next2 into the lookahead
                    self.lookahead = next2;
                    break;
                }
            }
        } else {
            // token elision should have been handled by self.end_meta_comment
            assert_eq!(elided, None);
        }

        (ElidedTokens(elided), next.expect("Parser::read tried to read past EOF"))
    }

    fn read(&mut self) -> (Side, Spanned<Tok>) {
        let (elided, next) = self._read();
        let delta = self.update_nestings(&next);
        self.last_span2 = self.last_span;
        self.last_span = next.span;
        (Side { elided_tokens: elided, nesting_delta: delta }, next)
    }

    fn _unread(&mut self, ElidedTokens(elided): ElidedTokens, tok: Spanned<Tok>) {
        assert!(self.lookahead.is_none() || self.lookahead2.is_none(),
                "at most two lookahead tokens are supported");

        // this overwrites the prior value, i.e. newlines elided between the first lookahead
        // and the second lookahead will be ignored.
        // this is fine, because the secondary lookahead is only used
        // in `try_parse_kailua_atomic_kind_seq` and is thus fine to be ignored.
        self.elided_newline = elided;
        if let Some(tok) = self.lookahead.take() {
            self.lookahead2 = Some(tok);
        }
        self.lookahead = Some(tok);
    }

    fn unread(&mut self, tok: (Side, Spanned<Tok>)) {
        let (side, tok) = tok;
        self._unread(side.elided_tokens, tok);

        // if we can, reconstruct the last span from the last elided token
        if let ElidedTokens(Some(span)) = side.elided_tokens {
            self.last_span = span;
        } else {
            self.last_span = self.last_span2;
            self.last_span2 = Span::dummy();
        }

        self.revert_nestings(side.nesting_delta);
    }

    fn peek<'b>(&'b mut self) -> &'b Spanned<Tok> {
        if self.lookahead.is_none() {
            // keep the side information as much as possible
            let (elided, tok) = self._read();
            self._unread(elided, tok);
        }
        self.lookahead.as_ref().unwrap()
    }

    fn expect<Tok: Expectable>(&mut self, tok: Tok) -> Result<()> {
        let read = self.read();
        if !tok.check_token(&read.1.base) {
            try!(self.error(read.1.span, m::ExpectFailed { expected: tok, read: &read.1.base })
                     .done());
            self.unread(read);
            Err(Stop::Recover)
        } else {
            Ok(())
        }
    }

    fn lookahead<Tok: Expectable>(&mut self, tok: Tok) -> bool {
        tok.check_token(self.peek())
    }

    fn may_expect<Tok: Expectable>(&mut self, tok: Tok) -> bool {
        if self.lookahead(tok) {
            let (_side, next) = self._read();
            self.update_nestings(&next);
            self.last_span2 = self.last_span;
            self.last_span = next.span;
            true
        } else {
            false
        }
    }

    fn update_nestings(&mut self, tok: &Tok) -> NestingDelta {
        enum Action {
            None,
            // push one nesting
            Push(Nesting),
            // push two nestings in this order
            Push2(Nesting, Nesting),
            // push three nestings in this order
            Push3(Nesting, Nesting, Nesting),
            // pop until a particular nesting appears; do nothing if no match
            Pop(Nesting),
            // pop until a particular nestings appears,
            // then push another nesting in place of the popped nesting; do nothing if no match
            PopAndPush(Nesting, Nesting),
            // pop until the first nesting appears; push the second nesting if no match
            PopOrPush(Nesting, Nesting),
        }

        let action = match *tok {
            // natural delimiters
            // no angle brackets, hard to distinguish comparison ops from delimiters
            Tok::Punct(Punct::LParen) => Action::Push(Nesting::Paren),
            Tok::Punct(Punct::LBrace) => Action::Push(Nesting::Brace),
            Tok::Punct(Punct::LBracket) => Action::Push(Nesting::Bracket),
            Tok::Punct(Punct::RParen) => Action::Pop(Nesting::Paren),
            Tok::Punct(Punct::RBrace) => Action::Pop(Nesting::Brace),
            Tok::Punct(Punct::RBracket) => Action::Pop(Nesting::Bracket),

            // meta blocks
            // Newline token is only generated inside a meta block by the lexer
            Tok::Punct(Punct::DashDashHash) |
            Tok::Punct(Punct::DashDashV) |
            Tok::Punct(Punct::DashDashColon) |
            Tok::Punct(Punct::DashDashGt) => Action::Push(Nesting::Meta),
            Tok::Punct(Punct::Newline) => Action::Pop(Nesting::Meta),

            // while, for and do blocks
            //
            // while/for ... do ... end
            // -----------End----------
            // -------Do-------
            //
            // do ... end
            // ----End---
            Tok::Keyword(Keyword::While) |
            Tok::Keyword(Keyword::For) => Action::Push2(Nesting::End, Nesting::Do),
            Tok::Keyword(Keyword::Do) => Action::PopOrPush(Nesting::Do, Nesting::End),

            // function block
            //
            // function [NAME] ( ... ) ... end
            // -------------End-------------
            //                 -Paren-
            Tok::Keyword(Keyword::Function) => Action::Push(Nesting::End),

            // if blocks
            //
            // if ... then ... elseif ... then ... elseif ... then ... else ... end
            // --------------------------------End---------------------------------
            // ---------Else---vvvvvv              vvvvvv----Else----------
            //                 ^^^^^^-----Else-----^^^^^^
            // ----Then---     ------Then-----     ------Then-----
            //
            // `elseif` is a particularly complex token to handle.
            // it is conceptually closing the if--else nesting but should open one simultaneously.
            // therefore while the affected nesting does not change per se,
            // we mark the (conceptual) change in the delta so that the caller can recognize it.
            Tok::Keyword(Keyword::If) =>
                Action::Push3(Nesting::End, Nesting::Else, Nesting::Then),
            Tok::Keyword(Keyword::Then) =>
                Action::Pop(Nesting::Then),
            Tok::Keyword(Keyword::Elseif) =>
                Action::PopAndPush(Nesting::Else, Nesting::Else),
            Tok::Keyword(Keyword::Else) =>
                Action::Pop(Nesting::Else),

            // repeat-until block
            //
            // repeat ... until ...
            // -----Until------
            Tok::Keyword(Keyword::Repeat) => Action::Push(Nesting::Until),
            Tok::Keyword(Keyword::Until) => Action::Pop(Nesting::Until),

            // the universal `end` token
            Tok::Keyword(Keyword::End) => Action::Pop(Nesting::End),

            // EOF (conceptually forms the top-level nesting)
            Tok::EOF => Action::Pop(Nesting::Top),

            _ => Action::None,
        };

        let (removed, added) = match action {
            Action::None => (Vec::new(), 0),

            Action::Push(e) => {
                trace!("token {:?} opened a nesting {:?}", *tok, e);
                self.open_nestings.push(e);
                (Vec::new(), 1)
            },

            Action::Push2(e1, e2) => {
                trace!("token {:?} opened nestings {:?} and {:?}", *tok, e1, e2);
                self.open_nestings.push(e1);
                self.open_nestings.push(e2);
                (Vec::new(), 2)
            },

            Action::Push3(e1, e2, e3) => {
                trace!("token {:?} opened nestings {:?}, {:?} and {:?}", *tok, e1, e2, e3);
                self.open_nestings.push(e1);
                self.open_nestings.push(e2);
                self.open_nestings.push(e3);
                (Vec::new(), 3)
            },

            Action::Pop(epop) => {
                if let Some(i) = self.open_nestings.iter().rposition(|&e| e == epop) {
                    trace!("token {:?} closed a nesting {:?}", *tok, epop);
                    let removed = self.open_nestings.drain(i..).collect();
                    (removed, 0)
                } else {
                    trace!("token {:?} tried to close a nesting {:?}", *tok, epop);
                    (Vec::new(), 0)
                }
            },

            Action::PopAndPush(epop, epush) => {
                if let Some(i) = self.open_nestings.iter().rposition(|&e| e == epop) {
                    trace!("token {:?} closed a nesting {:?} and opened a nesting {:?}",
                           *tok, epop, epush);
                    let removed = self.open_nestings.drain(i..).collect();
                    self.open_nestings.push(epush);
                    (removed, 1) // intentionally do not check if epop == epush; see above
                } else {
                    trace!("token {:?} tried to close a nesting {:?} and open a nesting {:?}",
                           *tok, epop, epush);
                    (Vec::new(), 0)
                }
            },

            Action::PopOrPush(epop, epush) => {
                if let Some(i) = self.open_nestings.iter().rposition(|&e| e == epop) {
                    trace!("token {:?} closed a nesting {:?} instead of opening a nesting {:?}",
                           *tok, epop, epush);
                    let removed = self.open_nestings.drain(i..).collect();
                    (removed, 0)
                } else {
                    trace!("token {:?} tried to close a nesting {:?} but \
                            opened a nesting {:?} instead", *tok, epop, epush);
                    self.open_nestings.push(epush);
                    (Vec::new(), 1)
                }
            },
        };

        NestingDelta { removed: removed, added: added }
    }

    fn revert_nestings(&mut self, delta: NestingDelta) {
        assert!(delta.added <= self.open_nestings.len());
        let newlen = self.open_nestings.len() - delta.added;
        self.open_nestings.truncate(newlen);
        self.open_nestings.extend(delta.removed.into_iter());
    }

    // ensures that the nesting depth of the pseudo-tree of tokens is lower than the beginning
    fn recover_retry<F, R>(&mut self, f: F) -> Result<R>
        where F: FnOnce(&mut Parser<'a, T>) -> Result<R>
    {
        let depth = self.open_nestings.len();
        trace!("set the recover trap, nestings = {:?}", self.open_nestings);
        let ret = f(self);
        if ret.is_err() {
            debug!("recovering from the error, init nestings = {:?}", self.open_nestings);
            let mut last_tok = None;
            while self.open_nestings.len() >= depth {
                let tok = self.read();
                trace!("skipping {:?}, nestings = {:?}", tok.1, self.open_nestings);
                last_tok = Some(tok);
            }
            if depth - self.open_nestings.len() > 1 {
                // if the last token has closed too many nestings we need to keep
                // that last token to allow the further match from the caller.
                // this case includes a premature EOF, whether reading EOF would close
                // at least two nestings (the initial nesting, the top-level nesting).
                if let Some(tok) = last_tok {
                    self.unread(tok);
                }
            }
            debug!("recovered from the error, final nestings = {:?}", self.open_nestings);
        }
        ret
    }

    fn recover_with<F, H, R>(&mut self, f: F, handler: H) -> Result<R>
        where F: FnOnce(&mut Parser<'a, T>) -> Result<R>, H: FnOnce() -> R
    {
        match self.recover_retry(f) {
            Ok(v) => Ok(v),
            Err(Stop::Recover) => Ok(handler()),
            Err(Stop::Fatal) => Err(Stop::Fatal),
        }
    }

    fn recover<F, R: Recover>(&mut self, f: F) -> Result<R>
        where F: FnOnce(&mut Parser<'a, T>) -> Result<R>
    {
        self.recover_with(f, Recover::recover)
    }

    fn recover_to_close(&mut self) {
        let _: Result<()> = self.recover_retry(|_| Err(Stop::Recover));
    }

    fn pos(&mut self) -> Pos {
        self.peek().span.begin()
    }

    fn last_pos(&self) -> Pos {
        assert!(!self.last_span.is_dummy(), "Parser::last_pos has lost prior span information");
        self.last_span.end()
    }

    // does *not* consume the meta comment!
    fn begin_meta_comment(&mut self, meta: Punct) {
        self.ignore_after_newline = Some(meta);
    }

    // also consumes a newline
    fn end_meta_comment(&mut self, meta: Punct) -> Result<()> {
        assert_eq!(self.ignore_after_newline, Some(meta));

        // if the next token (sans elided newline-meta pairs) is a newline, it's the end.
        // otherwise a newline token may have been elided; try to reconstruct if possible.
        let elided = self.elided_newline;
        self.elided_newline = None; // may_expect requires this
        if !self.may_expect(Punct::Newline) {
            // ...and may_expect may have set this again!
            let elided = self.elided_newline.or(elided);
            self.elided_newline = None;

            if let Some(metaspan) = elided {
                // newline (implicitly consumed) - meta - original lookahead
                assert_eq!(self.lookahead2, None);
                self.lookahead2 = self.lookahead.take();
                self.lookahead = Some(Tok::Punct(meta).with_loc(metaspan));
            } else {
                let next = self.peek().clone();
                try!(self.error(next.span, m::NoNewline { read: &next.base }).done());
                return self.skip_meta_comment(meta);
            }
        }

        self.ignore_after_newline = None;
        Ok(())
    }

    // consumes a newline, used for error recovery
    fn skip_meta_comment(&mut self, meta: Punct) -> Result<()> {
        assert_eq!(self.ignore_after_newline, Some(meta));
        loop {
            match self.read() {
                (_, Spanned { base: Tok::Punct(Punct::Newline), .. }) => {
                    break;
                }
                tok @ (_, Spanned { base: Tok::EOF, .. }) => {
                    // we need to keep the EOF to the queue, otherwise `read` will hit past EOF
                    self.unread(tok);
                    break;
                },
                _ => {}
            }
        }
        self.ignore_after_newline = None;
        Ok(())
    }

    fn dummy_kind(&self) -> Spanned<Kind> {
        Box::new(K::Oops).without_loc()
    }

    // Some(Some(k)) for concrete kinds, Some(None) for generic kinds
    fn builtin_kind(&self, name: &[u8]) -> Option<Option<K>> {
        match name {
            b"WHATEVER" => Some(Some(K::Dynamic)),
            b"any"      => Some(Some(K::Any)),
            b"boolean"  => Some(Some(K::Boolean)),
            b"number"   => Some(Some(K::Number)),
            b"integer"  => Some(Some(K::Integer)),
            b"string"   => Some(Some(K::String)),
            b"table"    => Some(Some(K::Table)),
            b"function" => Some(Some(K::Function)), // allow for quoted `function` too
            b"thread"   => Some(Some(K::Thread)),
            b"userdata" => Some(Some(K::UserData)),
            b"vector"   => Some(None),
            b"map"      => Some(None),
            _ => None,
        }
    }

    fn try_parse_name(&mut self) -> Option<Spanned<Name>> {
        let tok = self.read();
        if let Tok::Name(name) = tok.1.base {
            Some(Name::from(name).with_loc(tok.1.span))
        } else {
            self.unread(tok);
            None
        }
    }

    fn parse_name(&mut self) -> Result<Spanned<Name>> {
        let tok = self.read();
        if let Tok::Name(name) = tok.1.base {
            Ok(Name::from(name).with_loc(tok.1.span))
        } else {
            self.fatal(tok.1.span, m::NoName { read: &tok.1.base }).done()
        }
    }

    fn try_name_or_keyword(&mut self) -> Result<Spanned<Name>> {
        match self.read() {
            (_, Spanned { base: Tok::Name(name), span }) => {
                Ok(Name::from(name).with_loc(span))
            }
            (_, Spanned { base: Tok::Keyword(keyword), span }) => {
                Ok(Name::from(keyword.name()).with_loc(span))
            }
            tok => {
                try!(self.error(tok.1.span, m::NoName { read: &tok.1.base }).done());
                self.unread(tok);
                Err(Stop::Recover)
            }
        }
    }

    fn parse_block(&mut self) -> Result<Spanned<Block>> {
        debug!("parsing block");

        let begin = self.pos();
        let mut stmts = Vec::new();
        let mut pastlast = false;
        let mut excessspan = None;
        while let Some(stmt) = try!(self.try_parse_stmt()) {
            self.may_expect(Punct::Semicolon);

            // if the statement is the final one, further parsing is an error
            let last = match &*stmt.base {
                &St::Return(..) | &St::Break => true,
                _ => false,
            };

            if pastlast {
                excessspan = Some(excessspan.unwrap_or(Span::dummy()) | stmt.span);
            }
            pastlast |= last;
            stmts.push(stmt);
        }

        if let Some(span) = excessspan {
            try!(self.error(span, m::StmtAfterReturnOrBreak {}).done());
        }
        Ok(stmts.with_loc(begin..self.last_pos()))
    }

    // unlike `parse_block`, this will try to parse as much as possible until EOF
    fn parse_block_then_eof(&mut self) -> Result<Spanned<Block>> {
        debug!("parsing block then EOF");

        let begin = self.pos();
        let mut stmts = Vec::new();
        let mut pastlast = false;
        let mut excessspan = None;
        loop {
            let stmt = match try!(self.try_parse_stmt()) {
                Some(stmt) => stmt,
                None => {
                    // try to consume the EOF (and return).
                    // if we can't, we will discard one token and retry.
                    if self.may_expect(EOF) { break; }
                    stmts.push(Recover::recover());
                    let tok = self.read();
                    try!(self.error(tok.1.span, m::NoStmt { read: &tok.1.base }).done());
                    continue;
                }
            };
            self.may_expect(Punct::Semicolon);

            // if the statement is the final one, further parsing is an error
            let last = match &*stmt.base {
                &St::Return(..) | &St::Break => true,
                _ => false,
            };

            if pastlast {
                excessspan = Some(excessspan.unwrap_or(Span::dummy()) | stmt.span);
            }
            pastlast |= last;
            stmts.push(stmt);
        }

        if let Some(span) = excessspan {
            try!(self.error(span, m::StmtAfterReturnOrBreak {}).done());
        }
        Ok(stmts.with_loc(begin..self.last_pos()))
    }

    fn update_type_specs_with_typeseq_spec<Item>(&self,
                                                 oldspecs: Spanned<Vec<TypeSpec<Spanned<Item>>>>,
                                                 specs: Spanned<Vec<Spanned<(M, Spanned<Kind>)>>>,
                                                 note_on_dup: &Localize,
                                                 note_on_less: &Localize,
                                                 note_on_more: &Localize)
            -> Result<Spanned<Vec<TypeSpec<Spanned<Item>>>>> {
        if oldspecs.iter().any(|oldspec| oldspec.modf != M::None ||
                                         oldspec.kind.is_some()) {
            try!(self.error(specs.span, note_on_dup).done());
            Ok(oldspecs)
        } else if oldspecs.len() > specs.base.len() {
            let span = {
                let excess = &oldspecs[specs.base.len()..];
                excess.iter().fold(Span::dummy(), |span, i| {
                    span | i.base.span | i.kind.as_ref().map_or(Span::dummy(), |k| k.span)
                })
            };
            try!(self.error(span, note_on_less).done());
            Ok(oldspecs)
        } else if oldspecs.len() < specs.base.len() {
            let span = {
                let excess = &specs.base[oldspecs.len()..];
                excess.iter().fold(Span::dummy(), |span, i| span | i.span)
            };
            try!(self.error(span, note_on_more).done());
            Ok(oldspecs)
        } else {
            let span = oldspecs.span;
            let newspecs: Vec<_> =
                oldspecs.base.into_iter().zip(specs.base.into_iter()).map(|(oldspec, spec)| {
                    self.make_kailua_typespec(oldspec.base, Some(spec))
                }).collect();
            Ok(newspecs.with_loc(span))
        }
    }

    fn try_parse_stmt(&mut self) -> Result<Option<Spanned<Stmt>>> {
        debug!("parsing stmt");
        let begin = self.pos();

        let funcspec = try!(self.try_parse_kailua_func_spec());
        if let Some(ref funcspec) = funcspec {
            // limit the possible lookahead
            let allowed = match self.peek().base {
                Tok::Keyword(Keyword::Function) => true,
                Tok::Keyword(Keyword::Local) => true, // `local NAME = ...` is filtered later
                _ => false,
            };
            if !allowed {
                try!(self.error(funcspec.span, m::MissingFuncDeclAfterFuncSpec {}).done());
            }
        }

        // if there exists a spec stmt return it first.
        // a spec may be empty, so loop until no spec exists or a spec is found.
        loop {
            match try!(self.try_parse_kailua_spec()) {
                Some(Some(spec)) => return Ok(Some(spec)),
                Some(None) => continue,
                None => break,
            }
        }

        let stmt = match self.read() {
            (_, Spanned { base: Tok::Keyword(Keyword::Do), .. }) => {
                let block = try!(self.recover(
                    lastly!(|p| p.parse_block() => p.expect(Keyword::End))
                ));
                Box::new(St::Do(block))
            }

            (_, Spanned { base: Tok::Keyword(Keyword::While), .. }) => {
                let cond = try!(self.recover(
                    lastly!(|p| p.parse_exp() => p.expect(Keyword::Do))
                ));
                let block = try!(self.recover(
                    lastly!(|p| p.parse_block() => p.expect(Keyword::End))
                ));
                Box::new(St::While(cond, block))
            }

            (_, Spanned { base: Tok::Keyword(Keyword::Repeat), .. }) => {
                let block = try!(self.recover(
                    lastly!(|p| p.parse_block() => p.expect(Keyword::Until))
                ));
                let cond = try!(self.parse_exp());
                Box::new(St::Repeat(block, cond))
            }

            (_, Spanned { base: Tok::Keyword(Keyword::If), .. }) => {
                let cond = try!(self.recover(
                    lastly!(|p| p.parse_exp() => p.expect(Keyword::Then))
                ));
                let block = try!(self.parse_block());
                let mut blocks = vec![(cond, block).with_loc(begin..self.last_pos())];
                let mut begin = self.pos();
                while self.may_expect(Keyword::Elseif) {
                    let cond = try!(self.parse_exp());
                    try!(self.expect(Keyword::Then));
                    let block = try!(self.parse_block());
                    blocks.push((cond, block).with_loc(begin..self.last_pos()));
                    begin = self.pos();
                }
                let mut lastblock = if self.may_expect(Keyword::Else) {
                    Some(try!(self.parse_block()))
                } else {
                    None
                };
                try!(self.expect(Keyword::End));

                // fix the span for the last block appeared to include `end`
                if let Some(ref mut lastblock) = lastblock {
                    lastblock.span |= self.last_pos().into();
                } else {
                    blocks.last_mut().unwrap().span |= self.last_pos().into();
                }
                Box::new(St::If(blocks, lastblock))
            }

            (_, Spanned { base: Tok::Keyword(Keyword::For), .. }) => {
                let name = try!(self.parse_name());
                match self.read() {
                    // for NAME "=" ...
                    (_, Spanned { base: Tok::Punct(Punct::Eq), .. }) => {
                        let start = try!(self.parse_exp());
                        try!(self.expect(Punct::Comma));
                        let end = try!(self.parse_exp());
                        let step = if self.may_expect(Punct::Comma) {
                            Some(try!(self.parse_exp()))
                        } else {
                            None
                        };
                        try!(self.expect(Keyword::Do));
                        let block = try!(self.parse_block());
                        try!(self.expect(Keyword::End));
                        Box::new(St::For(name, start, end, step, block))
                    }

                    // for NAME in ...
                    (_, Spanned { base: Tok::Keyword(Keyword::In), .. }) => {
                        let span = name.span;
                        try!(self.parse_stmt_for_in(vec![name].with_loc(span)))
                    }

                    // for NAME [SPEC] "," ... in ...
                    (_, Spanned { base: Tok::Punct(Punct::Comma), .. }) => {
                        let mut vars = vec![name];
                        let span = try!(self.scan_list(Self::parse_name, |name| vars.push(name)));
                        try!(self.expect(Keyword::In));
                        try!(self.parse_stmt_for_in(vars.with_loc(span)))
                    }

                    tok => {
                        try!(self.error(tok.1.span, m::NoForInSep { read: &tok.1.base }).done());
                        self.unread(tok);
                        self.recover_to_close();
                        Recover::recover()
                    }
                }
            }

            (_, Spanned { base: Tok::Keyword(Keyword::Function), .. }) => {
                let mut names = vec![try!(self.parse_name())];
                while self.may_expect(Punct::Dot) {
                    names.push(try!(self.parse_name()));
                }

                let mut funcspec = funcspec;
                let mut selfparam = None;
                if self.may_expect(Punct::Colon) {
                    names.push(try!(self.parse_name()));

                    // if this is a method, the pre-signature (if any) should have `self`
                    // as the first argument, either typed or not.
                    // (`parse_func_body` doesn't like untyped params, so we need to pop it)
                    if let Some(Spanned { base: (_, Some(ref mut presig)), .. }) = funcspec {
                        if !presig.args.head.is_empty() &&
                                *presig.args.head[0].base.base.base == b"self" {
                            let Spanned { base: arg, span } = presig.args.head.remove(0);
                            selfparam = Some(TypeSpec { base: SelfParam.with_loc(span),
                                                        modf: arg.modf, kind: arg.kind });
                        } else {
                            try!(self.error(presig, m::MissingSelfInFuncSpec {}).done());
                            // and assume that there *were* a `self` without a type
                        }
                    }
                    if selfparam.is_none() {
                        selfparam = Some(TypeSpec { base: SelfParam.without_loc(),
                                                    modf: M::None, kind: None });
                    }
                }

                let (sig, body) = try!(self.parse_func_body(funcspec));
                if names.len() == 1 {
                    assert!(selfparam.is_none(), "ordinary function cannot have an implicit self");
                    let name = names.pop().unwrap();
                    Box::new(St::FuncDecl(NameScope::Global, name, sig, body))
                } else {
                    Box::new(St::MethodDecl(names, selfparam, sig, body))
                }
            }

            (_, Spanned { base: Tok::Keyword(Keyword::Local), .. }) => {
                match self.read() {
                    // local function ...
                    (_, Spanned { base: Tok::Keyword(Keyword::Function), .. }) => {
                        let name = try!(self.parse_name());
                        let (sig, body) = try!(self.parse_func_body(funcspec));
                        Box::new(St::FuncDecl(NameScope::Local, name, sig, body))
                    }

                    // local NAME ...
                    tok @ (_, Spanned { base: Tok::Name(_), .. }) => {
                        self.unread(tok);

                        // forbid `--v ...` then `local NAME ...`
                        if let Some(ref funcspec) = funcspec {
                            try!(self.error(funcspec.span, m::MissingFuncDeclAfterFuncSpec {})
                                     .done());
                        }

                        let mut names = Vec::new();
                        let (span, eq) =
                            try!(self.scan_list_with_spec(Self::parse_name,
                                                          |namespec| names.push(namespec)));
                        let mut names = names.with_loc(span);

                        let exps = if eq {
                            let mut exps = Vec::new();
                            let span = try!(self.scan_list(Self::parse_exp, |exp| exps.push(exp)));
                            exps.with_loc(span)
                        } else {
                            Vec::new().with_loc(self.last_pos())
                        };

                        if let Some(specs) = try!(self.try_parse_kailua_typeseq_spec()) {
                            names = try!(self.update_type_specs_with_typeseq_spec(
                                names, specs,
                                &m::DuplicateTypeSpecInLocal {},
                                &m::ExcessNamesInLocal {},
                                &m::ExcessTypeSpecsInLocal {}));
                        }
                        Box::new(St::Local(names, exps))
                    }

                    (_, tok) => {
                        return self.fatal(tok.span, m::NoFuncOrNameAfterLocal { read: &tok.base })
                                   .done();
                    }
                }
            }

            (_, Spanned { base: Tok::Keyword(Keyword::Return), .. }) => {
                let mut exps = Vec::new();
                let (span, _) = try!(self.try_scan_explist(|exp| exps.push(exp)));
                Box::new(St::Return(exps.with_loc(span)))
            }

            (_, Spanned { base: Tok::Keyword(Keyword::Break), .. }) => {
                Box::new(St::Break)
            }

            tok => {
                self.unread(tok);

                if let Some(exp) = try!(self.try_parse_prefix_exp()) {
                    // prefixexp consumes pretty much everything.
                    // it might be a single statement as whole,
                    // or the beginning of `varlist "=" explist`.
                    // determine if prefixexp is a function call or an indexing op,
                    // and convert it to `Var` for the latter.
                    // (note that the prefixexp might as well be an op
                    // indexing the return from a function call!)
                    match self.convert_prefix_exp_to_var(exp) {
                        // var {"," var} "=" explist
                        Ok(firstvar) => {
                            let mut lhs = Vec::new();
                            let mut span = firstvar.span;
                            let mut eq = false;

                            // we've already read the first var, but not comma and/or specs yet
                            // read the spec for the first var (if any) first,
                            // then proceed to normal var w/ spec list,
                            // then an equal sign (to allow the last spec to appear after `=`)
                            let mut firstspec = try!(self.try_parse_kailua_type_spec());
                            if self.may_expect(Punct::Comma) {
                                if firstspec.is_none() {
                                    firstspec = try!(self.try_parse_kailua_type_spec());
                                }
                                lhs.push(self.make_kailua_typespec(firstvar, firstspec));
                                let (span_, eq_) =
                                    try!(self.scan_list_with_spec(Self::parse_var,
                                                                  |varspec| lhs.push(varspec)));
                                span |= span_;
                                eq = eq_;
                            } else {
                                lhs.push(self.make_kailua_typespec(firstvar, firstspec));
                            }
                            if !eq {
                                // the equal sign is mandatory, so ensure it here
                                try!(self.expect(Punct::Eq));
                            }
                            let mut lhs = lhs.with_loc(span);

                            let mut rhs = Vec::new();
                            let span = try!(self.scan_list(Self::parse_exp, |exp| rhs.push(exp)));
                            let rhs = rhs.with_loc(span);

                            if let Some(specs) = try!(self.try_parse_kailua_typeseq_spec()) {
                                lhs = try!(self.update_type_specs_with_typeseq_spec(
                                    lhs, specs,
                                    &m::DuplicateTypeSpecInAssign {},
                                    &m::ExcessLvaluesInAssign {},
                                    &m::ExcessTypeSpecsInAssign {}));
                            }

                            Box::new(St::Assign(lhs, rhs))
                        }

                        // prefixexp
                        Err(exp) => {
                            Box::new(St::Void(exp))
                        }
                    }
                } else {
                    return Ok(None);
                }
            }
        };

        Ok(Some(stmt.with_loc(begin..self.last_pos())))
    }

    fn parse_stmt_for_in(&mut self, names: Spanned<Vec<Spanned<Name>>>) -> Result<Stmt> {
        let mut exps = Vec::new();
        let span = try!(self.scan_list(Self::parse_exp, |exp| exps.push(exp)));
        let exps = exps.with_loc(span);
        try!(self.expect(Keyword::Do));
        let block = try!(self.parse_block());
        try!(self.expect(Keyword::End));
        Ok(Box::new(St::ForIn(names, exps, block)))
    }

    fn parse_func_body(&mut self,
                       funcspec: Option<Spanned<(Vec<Spanned<Attr>>, Option<Spanned<Presig>>)>>)
            -> Result<(Sig, Spanned<Block>)> {
        let mut args = Vec::new();
        let mut varargs = None;
        let returns; // to check the error case

        // the type specifier may come at any reasonable positions:
        //
        // 1) `name <spec> ,`
        // 2) `name , <spec>`
        // 3) `name ) <spec>`
        // 4) `... <spec> )`
        // 5) `... ) <spec>`
        //
        // each parsing code is scattered throughout the following block.

        try!(self.expect(Punct::LParen));
        let begin = self.pos();
        let end;
        let mut name = None;
        let mut spec = None;
        let mut variadic = None;
        match self.read() {
            (_, Spanned { base: Tok::Punct(Punct::DotDotDot), span }) => {
                variadic = Some(span);
            }
            (_, Spanned { base: Tok::Name(name0), span }) => {
                name = Some(Name::from(name0).with_loc(span));
                spec = try!(self.try_parse_kailua_type_spec()); // 1)
                while self.may_expect(Punct::Comma) {
                    // try to read the type spec after a comma if there was no prior spec
                    if spec.is_none() { spec = try!(self.try_parse_kailua_type_spec()); } // 2)
                    args.push((name.take().unwrap(), spec.take()));
                    let begin = self.pos();
                    if self.may_expect(Punct::DotDotDot) {
                        variadic = Some((begin..self.last_pos()).into());
                        break;
                    } else {
                        name = Some(try!(self.parse_name()));
                        spec = try!(self.try_parse_kailua_type_spec()); // 1)
                    }
                }
            }
            tok @ (_, Spanned { base: Tok::Punct(Punct::RParen), .. }) => {
                self.unread(tok);
            }
            (_, tok) => {
                return self.fatal(tok.span, m::BadFuncArg { read: &tok.base }).done();
            }
        }
        if let Some(span) = variadic {
            // we've already read `...` and flushed the name-spec pair
            let mut varargs_ = try!(self.try_parse_kailua_type_spec_with_spanned_modf()); // 4)
            end = self.last_pos();
            try!(self.expect(Punct::RParen));
            if varargs_.is_none() {
                varargs_ = try!(self.try_parse_kailua_type_spec_with_spanned_modf()); // 5)
            }
            let varargs_ = if let Some(Spanned { base: (m, kind), .. }) = varargs_ {
                if m.base != M::None {
                    try!(self.error(m.span, m::NoModfAllowedInVarargs {}).done());
                }
                Some(kind)
            } else {
                None
            };
            varargs = Some(varargs_.with_loc(span));
        } else {
            end = self.last_pos();
            try!(self.expect(Punct::RParen));
            if let Some(name) = name {
                // the last type spec may follow the right parenthesis
                if spec.is_none() { spec = try!(self.try_parse_kailua_type_spec()); } // 3)
                args.push((name, spec));
            }
        }
        returns = try!(self.try_parse_kailua_rettype_spec());

        enum SigOrOthers {
            Sig(Sig),
            Others(Vec<Spanned<Attr>>, Option<Spanned<Seq<Spanned<Kind>>>>),
        }

        let sig_or_others = if let Some(Spanned { base: (attrs, presig), .. }) = funcspec {
            if let Some(presig) = presig {
                // before any checking, we should ensure that every parameter has a type attached
                // (`self` is a notable exception, but should have been removed by now)
                for arg in &presig.base.args.head {
                    if arg.kind.is_none() {
                        try!(self.error(&arg.base.base, m::MissingArgTypeInFuncSpec {}).done());
                    }
                }

                // if there is a pre-signature, any mismatching parameter or
                // inline argument type spec is an error
                if args.len() > presig.base.args.head.len() {
                    let excess = &args[presig.base.args.head.len()..];
                    let span = excess.iter().fold(Span::dummy(), |span, i| span | i.0.span);
                    try!(self.error(span, m::ExcessArgsInFuncDecl {}).done());
                } else if args.len() < presig.base.args.head.len() {
                    let excess = &presig.base.args.head[args.len()..];
                    let span = excess.iter().fold(Span::dummy(), |span, i| span | i.span);
                    try!(self.error(span, m::ExcessArgsInFuncSpec {}).done());
                }
                match (&varargs, &presig.base.args.tail) {
                    (&Some(ref v), &None) => {
                        try!(self.error(v.span, m::MissingVarargsInFuncSpec {}).done());
                    }
                    (&None, &Some(ref v)) => {
                        try!(self.error(v.span, m::MissingVarargsInFuncDecl {}).done());
                    }
                    (&Some(Spanned { base: Some(_), span }), &Some(ref v)) => {
                        try!(self.error(span, m::DuplicateVarargsSpecInFuncDecl {})
                                 .note(v.span, m::PriorVarargsSpecInFuncSpec {})
                                 .done());
                    }
                    (_, _) => {}
                }
                for (arg, sigarg) in args.iter().zip(presig.base.args.head.iter()) {
                    if arg.0.base != sigarg.base.base.base { // TODO might not be needed
                        try!(self.error(arg.0.span, m::ArgNameMismatchInFuncDecl {})
                                 .note(sigarg.base.base.span, m::PriorArgNameInFuncSpec {})
                                 .done());
                    }
                    if let Some(ref spec) = arg.1 {
                        try!(self.error(spec.span, m::DuplicateSpecInFuncDecl {})
                                 .note(presig.span, m::PriorFuncSpec {})
                                 .done());
                    }
                }
                if let Some(returns) = returns {
                    try!(self.error(returns.span, m::DuplicateReturnSpecInFuncDecl {})
                             .note(presig.span, m::PriorFuncSpec {})
                             .done());
                }
                SigOrOthers::Sig(presig.base.to_sig(attrs))
            } else {
                SigOrOthers::Others(attrs, returns)
            }
        } else {
            SigOrOthers::Others(Vec::new(), returns)
        };

        let sig = match sig_or_others {
            SigOrOthers::Sig(sig) => sig,
            SigOrOthers::Others(attrs, returns) => {
                let args = Seq {
                    head: args.into_iter().map(|(n,s)| self.make_kailua_typespec(n, s)).collect(),
                    tail: varargs.map(|tt| tt.base),
                };
                Sig {
                    attrs: attrs,
                    args: args.with_loc(begin..end),
                    returns: returns.map(|ret| ret.base),
                }
            },
        };

        let block = try!(self.parse_block());
        try!(self.expect(Keyword::End));

        Ok((sig, block))
    }

    fn scan_tabular_body<Scan, Item>(&mut self, mut scan: Scan) -> Result<Vec<Item>>
            where Scan: FnMut(&mut Self) -> Result<Item> {
        let mut items = Vec::new();

        while !self.may_expect(Punct::RBrace) {
            let item = try!(scan(self));
            items.push(item);

            // ";" - "," - ";" "}" - "," "}" - "}"
            match self.read() {
                (_, Spanned { base: Tok::Punct(Punct::Comma), .. }) |
                (_, Spanned { base: Tok::Punct(Punct::Semicolon), .. }) => {}
                (_, Spanned { base: Tok::Punct(Punct::RBrace), .. }) => break,
                tok => {
                    try!(self.error(tok.1.span, m::NoTableSep { read: &tok.1.base }).done());
                    self.recover_to_close();
                    break;
                }
            }
        }

        Ok(items)
    }

    fn parse_table_body(&mut self) -> Result<Vec<(Option<Spanned<Exp>>, Spanned<Exp>)>> {
        self.scan_tabular_body(|parser| {
            if parser.may_expect(Punct::LBracket) {
                let key = Some(try!(parser.parse_exp()));
                try!(parser.expect(Punct::RBracket));
                try!(parser.expect(Punct::Eq));
                let value = try!(parser.parse_exp());
                Ok((key, value))
            } else {
                // it is hard to disambiguiate `NAME "=" exp` and `exp`,
                // so parse `exp` first and check if it's a `NAME` followed by `=`.
                let exp = try!(parser.parse_exp());
                let name_or_exp = if parser.lookahead(Punct::Eq) {
                    let span = exp.span;
                    match *exp.base {
                        Ex::Var(name) => Ok(name),
                        exp => Err(Box::new(exp).with_loc(span)),
                    }
                } else {
                    Err(exp)
                };
                match name_or_exp {
                    Ok(name) => {
                        let key = Some(Box::new(Ex::Str(name.base.into())).with_loc(name.span));
                        try!(parser.expect(Punct::Eq));
                        let value = try!(parser.parse_exp());
                        Ok((key, value))
                    }
                    Err(exp) => {
                        Ok((None, exp))
                    }
                }
            }
        })
    }

    fn try_parse_args(&mut self) -> Result<Option<Spanned<Vec<Spanned<Exp>>>>> {
        let begin = self.pos();
        match self.read() {
            (_, Spanned { base: Tok::Punct(Punct::LParen), .. }) => {
                let mut args = Vec::new();
                let (span, _) = try!(self.recover_with(
                    lastly!(|p| p.try_scan_explist(|exp| args.push(exp)) =>
                                p.expect(Punct::RParen)),
                    || (Span::dummy(), false)
                ));
                Ok(Some(args.with_loc(span)))
            }
            (_, Spanned { base: Tok::Str(s), span }) => {
                Ok(Some(vec![Box::new(Ex::Str(s.into())).with_loc(span)].with_loc(span)))
            }
            (_, Spanned { base: Tok::Punct(Punct::LBrace), .. }) => {
                let exp = Box::new(Ex::Table(try!(self.parse_table_body())));
                let span = Span::new(begin, self.last_pos());
                Ok(Some(vec![exp.with_loc(span)].with_loc(span)))
            }
            tok => {
                self.unread(tok);
                Ok(None)
            }
        }
    }

    fn try_parse_prefix_exp(&mut self) -> Result<Option<Spanned<Exp>>> {
        // any prefixexp starts with name or parenthesized exp
        let mut exp;
        let begin = self.pos();
        match self.read() {
            (_, Spanned { base: Tok::Punct(Punct::LParen), .. }) => {
                // TODO should we ignore the span for parentheses?
                exp = try!(self.recover(
                    lastly!(|p| p.parse_exp() => p.expect(Punct::RParen))
                ));
            }
            (_, Spanned { base: Tok::Name(name), span }) => {
                exp = Box::new(Ex::Var(Name::from(name).with_loc(span))).with_loc(span);
            }
            tok => {
                self.unread(tok);
                return Ok(None);
            }
        }

        // parse any postfix attachments
        loop {
            match self.read() {
                // prefixexp "." ...
                (_, Spanned { base: Tok::Punct(Punct::Dot), .. }) => {
                    let tok = self.read().1;
                    if let Tok::Name(name) = tok.base {
                        let name = Box::new(Ex::Str(name.into())).with_loc(tok.span);
                        let span = begin..self.last_pos();
                        exp = Box::new(Ex::Index(exp, name)).with_loc(span);
                    } else {
                        return self.fatal(tok.span, m::NoNameAfterExpDot { read: &tok.base })
                                   .done();
                    }
                }

                // prefixexp "[" ...
                (_, Spanned { base: Tok::Punct(Punct::LBracket), .. }) => {
                    let exp2 = try!(self.parse_exp());
                    try!(self.expect(Punct::RBracket));
                    let span = begin..self.last_pos();
                    exp = Box::new(Ex::Index(exp, exp2)).with_loc(span);
                }

                // prefixexp ":" ...
                (_, Spanned { base: Tok::Punct(Punct::Colon), .. }) => {
                    let name = try!(self.parse_name());
                    if let Some(args) = try!(self.try_parse_args()) {
                        let span = begin..self.last_pos();
                        exp = Box::new(Ex::MethodCall(exp, name, args)).with_loc(span);
                    } else {
                        let tok = self.read().1;
                        return self.fatal(tok.span, m::NoArgsAfterExpColonName { read: &tok.base })
                                   .done();
                    }
                }

                // prefixexp STR
                // prefixexp "("
                // prefixexp "{"
                tok => {
                    self.unread(tok);
                    if let Some(args) = try!(self.try_parse_args()) {
                        let span = begin..self.last_pos();
                        exp = Box::new(Ex::FuncCall(exp, args)).with_loc(span);
                        continue;
                    }
                    break;
                }
            }
        }
        Ok(Some(exp))
    }

    fn convert_prefix_exp_to_var(&self,
                                 exp: Spanned<Exp>) -> result::Result<Spanned<Var>, Spanned<Exp>> {
        let span = exp.span;
        let base = *exp.base;
        match base {
            Ex::Var(name) => Ok(Var::Name(name).with_loc(span)),
            Ex::Index(e1, e2) => Ok(Var::Index(e1, e2).with_loc(span)),
            base => Err(Box::new(base).with_loc(span)),
        }
    }

    fn try_parse_atomic_exp(&mut self) -> Result<Option<Spanned<Exp>>> {
        let begin = self.pos();

        let funcspec = try!(self.try_parse_kailua_func_spec());
        if let Some(ref funcspec) = funcspec {
            if !self.lookahead(Keyword::Function) {
                // limit the possible lookahead
                try!(self.error(funcspec.span, m::MissingFuncLitAfterFuncSpec {}).done());
            }
        }

        match self.read() {
            (_, Spanned { base: Tok::Keyword(Keyword::Nil), span }) =>
                Ok(Some(Box::new(Ex::Nil).with_loc(span))),
            (_, Spanned { base: Tok::Keyword(Keyword::False), span }) =>
                Ok(Some(Box::new(Ex::False).with_loc(span))),
            (_, Spanned { base: Tok::Keyword(Keyword::True), span }) =>
                Ok(Some(Box::new(Ex::True).with_loc(span))),
            (_, Spanned { base: Tok::Num(v), span }) =>
                Ok(Some(Box::new(Ex::Num(v)).with_loc(span))),
            (_, Spanned { base: Tok::Str(s), span }) =>
                Ok(Some(Box::new(Ex::Str(s.into())).with_loc(span))),
            (_, Spanned { base: Tok::Punct(Punct::DotDotDot), span }) =>
                Ok(Some(Box::new(Ex::Varargs).with_loc(span))),

            (_, Spanned { base: Tok::Keyword(Keyword::Function), .. }) => {
                let (sig, body) = try!(self.parse_func_body(funcspec));
                Ok(Some(Box::new(Ex::Func(sig, body)).with_loc(begin..self.last_pos())))
            }

            (_, Spanned { base: Tok::Punct(Punct::LBrace), .. }) => {
                let table = try!(self.parse_table_body());
                Ok(Some(Box::new(Ex::Table(table)).with_loc(begin..self.last_pos())))
            }

            tok => {
                self.unread(tok);
                self.try_parse_prefix_exp()
            }
        }
    }

    fn try_parse_prefix_unary_exp<Term, Op>(&mut self,
                                            mut check_op: Op,
                                            mut try_parse_term: Term)
            -> Result<Option<Spanned<Exp>>>
            where Term: FnMut(&mut Self) -> Result<Option<Spanned<Exp>>>,
                  Op: FnMut(&Tok) -> Option<UnOp> {
        let mut ops = Vec::new();
        while let Some(op) = check_op(self.peek()) {
            let opspan = self.read().1.span;
            ops.push(op.with_loc(opspan));
        }
        if let Some(exp) = try!(try_parse_term(self)) {
            let mut exp = exp;
            while let Some(op) = ops.pop() {
                let span = op.span | exp.span;
                exp = Box::new(Ex::Un(op, exp)).with_loc(span);
            }
            Ok(Some(exp))
        } else if ops.is_empty() {
            Ok(None)
        } else {
            let tok = self.read().1;
            self.fatal(tok.span, m::NoExp { read: &tok.base }).done()
        }
    }

    fn try_parse_left_assoc_binary_exp<Term, Op>(&mut self,
                                                 mut check_op: Op,
                                                 mut try_parse_term: Term)
            -> Result<Option<Spanned<Exp>>>
            where Term: FnMut(&mut Self) -> Result<Option<Spanned<Exp>>>,
                  Op: FnMut(&Tok) -> Option<BinOp> {
        if let Some(exp) = try!(try_parse_term(self)) {
            let mut exp = exp;
            while let Some(op) = check_op(self.peek()) {
                let opspan = self.read().1.span;
                if let Some(exp2) = try!(try_parse_term(self)) {
                    let span = exp.span | opspan | exp2.span;
                    exp = Box::new(Ex::Bin(exp, op.with_loc(opspan), exp2)).with_loc(span);
                } else {
                    let tok = self.read().1;
                    return self.fatal(tok.span, m::NoExp { read: &tok.base }).done();
                }
            }
            Ok(Some(exp))
        } else {
            Ok(None)
        }
    }

    fn try_parse_right_assoc_binary_exp<Term, Op>(&mut self,
                                                  mut check_op: Op,
                                                  mut try_parse_term: Term)
            -> Result<Option<Spanned<Exp>>>
            where Term: FnMut(&mut Self) -> Result<Option<Spanned<Exp>>>,
                  Op: FnMut(&Tok) -> Option<BinOp> {
        if let Some(exp) = try!(try_parse_term(self)) {
            // store the terms and process in the reverse order
            // e.g. <exp:terms[0].0> <op:terms[0].1> <exp:terms[1].0> <op:terms[1].1> <exp:last_exp>
            let mut exp = exp;
            let mut terms = vec![];
            while let Some(op) = check_op(self.peek()) {
                let opspan = self.read().1.span;
                terms.push((exp, op.with_loc(opspan)));
                if let Some(e) = try!(try_parse_term(self)) {
                    exp = e;
                } else {
                    let tok = self.read().1;
                    return self.fatal(tok.span, m::NoExp { read: &tok.base }).done();
                }
            }
            while let Some((exp1, op)) = terms.pop() {
                let span = exp1.span | op.span | exp.span;
                exp = Box::new(Ex::Bin(exp1, op, exp)).with_loc(span);
            }
            Ok(Some(exp))
        } else {
            Ok(None)
        }
    }

    fn try_parse_exp(&mut self) -> Result<Option<Spanned<Exp>>> {
        debug!("parsing exp");

        macro_rules! make_check_ops {
            ($($name:ident: $ty:ident { $($tokty:ident::$tok:ident => $op:ident),+ $(,)* };)*) => (
                $(
                    fn $name(tok: &Tok) -> Option<$ty> {
                        match *tok {
                            $(Tok::$tokty($tokty::$tok) => Some($ty::$op),)*
                            _ => None,
                        }
                    }
                )*
            )
        }

        make_check_ops! {
            check_pow_op: BinOp {
                Punct::Caret => Pow,
            };
            check_un_op: UnOp {
                Keyword::Not => Not,
                Punct::Hash => Len,
                Punct::Dash => Neg,
            };
            check_mul_op: BinOp {
                Punct::Star => Mul,
                Punct::Slash => Div,
                Punct::Percent => Mod,
            };
            check_add_op: BinOp {
                Punct::Plus => Add,
                Punct::Dash => Sub,
            };
            check_cat_op: BinOp {
                Punct::DotDot => Cat,
            };
            check_comp_op: BinOp {
                Punct::Lt => Lt,
                Punct::Gt => Gt,
                Punct::LtEq => Le,
                Punct::GtEq => Ge,
                Punct::TildeEq => Ne,
                Punct::EqEq => Eq,
            };
            check_and_op: BinOp {
                Keyword::And => And,
            };
            check_or_op: BinOp {
                Keyword::Or => Or,
            };
        }

        let mut parser = self;
        parser.try_parse_left_assoc_binary_exp(check_or_op, |parser|
            parser.try_parse_left_assoc_binary_exp(check_and_op, |parser|
                parser.try_parse_left_assoc_binary_exp(check_comp_op, |parser|
                    parser.try_parse_right_assoc_binary_exp(check_cat_op, |parser|
                        parser.try_parse_left_assoc_binary_exp(check_add_op, |parser|
                            parser.try_parse_left_assoc_binary_exp(check_mul_op, |parser|
                                parser.try_parse_prefix_unary_exp(check_un_op, |parser|
                                    parser.try_parse_right_assoc_binary_exp(check_pow_op, |parser|
                                        parser.try_parse_atomic_exp()))))))))
    }

    fn parse_exp(&mut self) -> Result<Spanned<Exp>> {
        if let Some(exp) = try!(self.try_parse_exp()) {
            Ok(exp)
        } else {
            let tok = self.read();
            try!(self.error(tok.1.span, m::NoExp { read: &tok.1.base }).done());
            self.unread(tok);
            Ok(Recover::recover())
        }
    }

    fn try_parse_var(&mut self) -> Result<Option<Spanned<Var>>> {
        if let Some(exp) = try!(self.try_parse_prefix_exp()) {
            let var = self.convert_prefix_exp_to_var(exp).unwrap();
            Ok(Some(var))
        } else {
            Ok(None)
        }
    }

    fn parse_var(&mut self) -> Result<Spanned<Var>> {
        if let Some(var) = try!(self.try_parse_var()) {
            Ok(var)
        } else {
            let tok = self.read().1;
            self.fatal(tok.span, m::NoVar { read: &tok.base }).done()
        }
    }

    // ITEM {(SPEC `,` | `,` [SPEC]) ITEM} [SPEC | SPEC `=` | `=` SPEC]
    // bool is true if it has also consumed `=`
    fn scan_list_with_spec<Item, Scan, F>(&mut self,
                                          mut scan: Scan,
                                          mut f: F) -> Result<(Span, bool)>
            where Scan: FnMut(&mut Self) -> Result<Item>,
                  F: FnMut(TypeSpec<Item>) {
        let begin = self.pos();
        let mut item = try!(scan(self));
        let mut spec = try!(self.try_parse_kailua_type_spec());
        while self.may_expect(Punct::Comma) {
            // try to read the type spec after a comma if there was no prior spec
            if spec.is_none() { spec = try!(self.try_parse_kailua_type_spec()); }
            f(self.make_kailua_typespec(item, spec));
            item = try!(scan(self));
            spec = try!(self.try_parse_kailua_type_spec());
        }
        let end = self.last_pos();
        if self.may_expect(Punct::Eq) {
            if spec.is_none() { spec = try!(self.try_parse_kailua_type_spec()); }
            f(self.make_kailua_typespec(item, spec));
            Ok((Span::new(begin, end), true))
        } else {
            f(self.make_kailua_typespec(item, spec));
            Ok((Span::new(begin, self.last_pos()), false)) // update the last position
        }
    }

    fn scan_list<Item, Scan, F>(&mut self,
                                mut scan: Scan,
                                mut f: F) -> Result<Span>
            where Scan: FnMut(&mut Self) -> Result<Item>,
                  F: FnMut(Item) {
        let begin = self.pos();
        f(try!(scan(self)));
        while self.may_expect(Punct::Comma) {
            f(try!(scan(self)));
        }
        let end = self.last_pos();
        Ok(Span::new(begin, end))
    }

    fn try_scan_explist<F>(&mut self, mut f: F) -> Result<(Span, bool)>
            where F: FnMut(Spanned<Exp>) {
        let begin = self.pos();
        if let Some(exp) = try!(self.try_parse_exp()) {
            f(exp);
            while self.may_expect(Punct::Comma) {
                f(try!(self.parse_exp()));
            }
            let end = self.last_pos();
            Ok((Span::new(begin, end), true))
        } else {
            Ok((Span::from(begin), false))
        }
    }

    // Kailua-specific syntaxes

    fn make_kailua_typespec<Base>(&self, base: Base,
                                  spec: Option<Spanned<(M, Spanned<Kind>)>>) -> TypeSpec<Base> {
        if let Some(Spanned { base: (modf, kind), .. }) = spec {
            TypeSpec { base: base, modf: modf, kind: Some(kind) }
        } else {
            TypeSpec { base: base, modf: M::None, kind: None }
        }
    }

    fn try_parse_kailua_attr(&mut self) -> Result<Option<Spanned<Attr>>> {
        let begin = self.pos();
        if self.may_expect(Punct::LBracket) {
            // `[` NAME `]`
            let name = try!(self.recover_retry(
                lastly!(|p| p.try_name_or_keyword() => p.expect(Punct::RBracket)),
            ));
            let attr = Attr { name: name };
            Ok(Some(attr.with_loc(begin..self.last_pos())))
        } else {
            Ok(None)
        }
    }

    fn parse_kailua_mod(&mut self) -> M {
        if self.may_expect(Keyword::Const) {
            M::Const
        } else {
            M::None
        }
    }

    fn parse_kailua_slotkind(&mut self) -> Result<Spanned<SlotKind>> {
        let begin = self.pos();
        let modf = self.parse_kailua_mod();
        let kind = try!(self.parse_kailua_kind());
        Ok(SlotKind { modf: modf, kind: kind }.with_loc(begin..self.last_pos()))
    }

    // may contain ellipsis, the caller is expected to error on unwanted cases
    fn parse_kailua_kindlist(&mut self) -> Result<Seq<Spanned<Kind>>> {
        let mut kinds = Vec::new();
        // KIND {"," KIND} ["..."] or empty
        // try to read the first KIND
        let kind = match try!(self.try_parse_kailua_kind()) {
            Some(kind) => kind,
            None => match self.read() {
                (_, Spanned { base: Tok::Punct(Punct::DotDotDot), span }) => {
                    try!(self.error(span, m::NoKindBeforeEllipsis {}).done());
                    // pretend that (an invalid) `(...)` is `(?...)`
                    return Ok(Seq { head: kinds, tail: Some(Box::new(K::Dynamic).with_loc(span)) });
                }
                tok => {
                    self.unread(tok);
                    return Ok(Seq { head: kinds, tail: None });
                }
            },
        };
        kinds.push(kind);
        while self.may_expect(Punct::Comma) {
            let kind = match try!(self.try_parse_kailua_kind()) {
                Some(kind) => kind,
                None => match self.read() {
                    (_, Spanned { base: Tok::Punct(Punct::DotDotDot), span }) => {
                        try!(self.error(span, m::NoKindBeforeEllipsis {}).done());
                        // pretend that (an invalid) `(<explist>, ...)` is `(<explist>, ?...)`
                        return Ok(Seq { head: kinds,
                                        tail: Some(Box::new(K::Dynamic).with_loc(span)) });
                    }
                    (_, tok) => {
                        return self.fatal(tok.span, m::NoKind { read: &tok.base }).done();
                    }
                },
            };
            kinds.push(kind);
        }
        if self.may_expect(Punct::DotDotDot) {
            let last = kinds.pop();
            Ok(Seq { head: kinds, tail: last })
        } else {
            Ok(Seq { head: kinds, tail: None })
        }
    }

    // the first argument may be `self`, which can have its type omitted
    // may also contain ellipsis, the caller is expected to error on unwanted cases
    fn parse_kailua_namekindlist(&mut self)
            -> Result<Spanned<Seq<Spanned<TypeSpec<Spanned<Name>>>,
                                  Spanned<Option<Spanned<Kind>>>>>> {
        let mut variadic = None;
        let mut specs = Vec::new();

        let begin = self.pos();
        if self.may_expect(Punct::DotDotDot) { // "..." [":" KIND]
            variadic = Some(begin);
        } else { // NAME ":" KIND {"," NAME ":" KIND} ["," "..." [":" KIND]] or empty
            let name = match self.try_parse_name() {
                Some(name) => name,
                None => {
                    return Ok(Seq { head: specs, tail: None }.with_loc(begin..self.last_pos()));
                },
            };
            let modf;
            let kind;
            if *name.base == b"self" {
                // `self` as the first argument can omit its type
                if self.may_expect(Punct::Colon) {
                    modf = self.parse_kailua_mod();
                    kind = Some(try!(self.parse_kailua_kind()));
                } else {
                    modf = M::None;
                    kind = None;
                }
            } else {
                try!(self.expect(Punct::Colon));
                modf = self.parse_kailua_mod();
                kind = Some(try!(self.parse_kailua_kind()));
            }
            let spec = TypeSpec { base: name, modf: modf, kind: kind };
            specs.push(spec.with_loc(begin..self.last_pos()));

            while self.may_expect(Punct::Comma) {
                let begin = self.pos();
                if self.may_expect(Punct::DotDotDot) {
                    variadic = Some(begin);
                    break;
                }
                let name = try!(self.parse_name());
                try!(self.expect(Punct::Colon));
                let modf = self.parse_kailua_mod();
                let kind = try!(self.parse_kailua_kind());
                let spec = TypeSpec { base: name, modf: modf, kind: Some(kind) };
                specs.push(spec.with_loc(begin..self.last_pos()));
            }
        }

        let tail = if let Some(begin) = variadic {
            // we've already read `...`
            let varargs = if self.may_expect(Punct::Colon) {
                let kind = try!(self.parse_kailua_kind());
                Some(kind)
            } else {
                None
            };
            Some(varargs.with_loc(begin..self.last_pos()))
        } else {
            None
        };

        let end = self.last_pos();
        Ok(Seq { head: specs, tail: tail }.with_loc(begin..end))
    }

    fn parse_kailua_funckind(&mut self) -> Result<Spanned<FuncKind>> {
        let begin = self.pos();
        try!(self.expect(Punct::LParen));
        let args = try!(self.parse_kailua_kindlist());
        try!(self.expect(Punct::RParen));
        let returns = if self.may_expect(Punct::DashDashGt) {
            // "(" ... ")" "-->" ...
            try!(self.parse_kailua_kind_seq())
        } else {
            // "(" ... ")"
            Seq { head: Vec::new(), tail: None }
        };
        let span = begin..self.last_pos();
        Ok(FuncKind { args: args, returns: returns }.with_loc(span))
    }

    fn try_parse_kailua_kind_params(&mut self)
            -> Result<Option<Spanned<Vec<(Spanned<M>, Spanned<Kind>)>>>> {
        let begin = self.pos();
        if self.may_expect(Punct::Lt) {
            // `<` MODF KIND [`,` MODF KIND] `>`
            let mut kinds = vec![try!(self.parse_kailua_kind_with_spanned_modf())];
            while self.may_expect(Punct::Comma) {
                kinds.push(try!(self.parse_kailua_kind_with_spanned_modf()));
            }
            if !self.may_expect(Punct::Gt) {
                // try to match against `>>` as well (Lua 5.2+)
                // for now, we intentionally put an edited token (`>`) back;
                // this can be done in a better way, though.
                let tok = self.read();
                if let (side, Spanned { base: Tok::Punct(Punct::GtGt), span }) = tok {
                    // XXX this span is bad, but we are unlikely to use this span anyway...
                    self.unread((side, Tok::Punct(Punct::Gt).with_loc(span)));
                } else {
                    try!(self.error(tok.1.span, m::NoKindParamsClose { read: &tok.1.base }).done());
                    self.unread(tok);
                }
            }
            let end = self.last_pos();
            Ok(Some(kinds.with_loc(begin..end)))
        } else {
            Ok(None)
        }
    }

    fn parse_kailua_kind_params(&mut self)
            -> Result<Spanned<Vec<(Spanned<M>, Spanned<Kind>)>>> {
        if let Some(params) = try!(self.try_parse_kailua_kind_params()) {
            Ok(params)
        } else {
            let tok = self.read();
            try!(self.error(tok.1.span, m::NoKindParams { read: &tok.1.base }).done());
            self.unread(tok);
            Ok(Vec::new().without_loc())
        }
    }

    // returns true if it can be followed by postfix operators
    fn try_parse_kailua_atomic_kind_seq(&mut self) -> Result<Option<AtomicKind>> {
        let begin = self.pos();

        let mut kind = match self.read() {
            (_, Spanned { base: Tok::Keyword(Keyword::Function), span }) => {
                // either a "function" type or a function signature
                if self.lookahead(Punct::LParen) {
                    // function `(` ... `)` [`-->` ...]
                    let func = try!(self.parse_kailua_funckind());
                    // cannot be followed by postfix operators
                    let kind = Box::new(K::Func(func)).with_loc(begin..self.last_pos());
                    return Ok(Some(AtomicKind::Seq(Seq { head: vec![kind], tail: None })));
                } else {
                    Box::new(K::Function).with_loc(span)
                }
            }

            (_, Spanned { base: Tok::Punct(Punct::LParen), .. }) => {
                let mut args = try!(self.parse_kailua_kindlist());
                try!(self.expect(Punct::RParen));
                if args.head.len() != 1 || args.tail.is_some() {
                    // cannot be followed by postfix operators - XXX really?
                    return Ok(Some(AtomicKind::Seq(args)));
                } else {
                    args.head.pop().unwrap()
                }
            }

            (_, Spanned { base: Tok::Punct(Punct::LBrace), .. }) => {
                let kind = match self.read() {
                    // "{" "}"
                    (_, Spanned { base: Tok::Punct(Punct::RBrace), .. }) =>
                        Box::new(K::EmptyTable),

                    // tuple or record -- distinguished by the secondary lookahead
                    tok => {
                        let is_record = if let (_, Spanned { base: Tok::Name(_), .. }) = tok {
                            self.lookahead(Punct::Eq)
                        } else {
                            false
                        };
                        self.unread(tok);

                        if is_record {
                            // "{" NAME "=" MODF KIND {"," NAME "=" MODF KIND} "}"
                            let mut seen = HashMap::new(); // value denotes the first span
                            let fields = try!(self.scan_tabular_body(|parser| {
                                let name = try!(parser.parse_name());
                                match seen.entry(name.base.clone()) {
                                    hash_map::Entry::Occupied(e) => {
                                        try!(parser.error(name.span,
                                                          m::DuplicateFieldNameInRec
                                                              { name: &name.base })
                                                   .note(*e.get(), m::FirstFieldNameInRec {})
                                                   .done());
                                    }
                                    hash_map::Entry::Vacant(e) => {
                                        e.insert(name.span);
                                    }
                                }
                                let name = Str::from(name.base).with_loc(name.span);
                                try!(parser.expect(Punct::Eq));
                                let slotkind = try!(parser.parse_kailua_slotkind());
                                Ok((name, slotkind))
                            }));
                            Box::new(K::Record(fields))
                        } else {
                            // "{" MODF KIND "," [MODF KIND {"," MODF KIND}] "}"
                            let fields = try!(self.scan_tabular_body(|parser| {
                                parser.parse_kailua_slotkind()
                            }));
                            Box::new(K::Tuple(fields))
                        }
                    }
                };
                kind.with_loc(begin..self.last_pos())
            }

            (_, Spanned { base: Tok::Keyword(Keyword::Nil), span }) =>
                Box::new(K::Nil).with_loc(span),
            (_, Spanned { base: Tok::Keyword(Keyword::True), span }) =>
                Box::new(K::BooleanLit(true)).with_loc(span),
            (_, Spanned { base: Tok::Keyword(Keyword::False), span }) =>
                Box::new(K::BooleanLit(false)).with_loc(span),

            (_, Spanned { base: Tok::Keyword(Keyword::Vector), .. }) => {
                let params = try!(self.parse_kailua_kind_params());
                if params.len() != 1 {
                    if !params.is_empty() {
                        try!(self.error(&params, m::WrongVectorParamsArity {}).done());
                    }
                    self.dummy_kind()
                } else {
                    let mut it = params.base.into_iter();
                    let (vm, v) = it.next().unwrap();
                    let vspan = vm.span | v.span;
                    let v = SlotKind { modf: vm.base, kind: v }.with_loc(vspan);
                    Box::new(K::Array(v)).with_loc(begin..self.last_pos())
                }
            }
            (_, Spanned { base: Tok::Keyword(Keyword::Map), .. }) => {
                let params = try!(self.parse_kailua_kind_params());
                if params.len() != 2 {
                    if !params.is_empty() {
                        try!(self.error(&params, m::WrongMapParamsArity {}).done());
                    }
                    self.dummy_kind()
                } else {
                    let mut it = params.base.into_iter();
                    let (km, k) = it.next().unwrap();
                    if km.base != M::None {
                        try!(self.error(&km, m::WrongMapParamsModf {}).done());
                    }
                    let (vm, v) = it.next().unwrap();
                    let vspan = vm.span | v.span;
                    let v = SlotKind { modf: vm.base, kind: v }.with_loc(vspan);
                    Box::new(K::Map(k, v)).with_loc(begin..self.last_pos())
                }
            }

            (_, Spanned { base: Tok::Name(name), span }) => {
                if name == b"error" {
                    // may follow an error reason
                    let reason = match self.read() {
                        (_, Spanned { base: Tok::Str(s), span }) => {
                            Some(Str::from(s).with_loc(span))
                        }
                        tok => {
                            self.unread(tok);
                            None
                        }
                    };
                    Box::new(K::Error(reason)).with_loc(span)
                } else {
                    let name = Name::from(name);
                    let kind = match self.builtin_kind(&name) {
                        Some(Some(kind)) => kind,
                        Some(None) => {
                            try!(self.error(span, m::ReservedKindName { name: &name }).done());
                            K::Oops
                        },
                        None => {
                            K::Named(name.with_loc(span))
                        },
                    };
                    Box::new(kind).with_loc(span)
                }
            }

            (_, Spanned { base: Tok::Num(v), span })
                    if i32::MIN as f64 <= v && v <= i32::MAX as f64 && v.floor() == v => {
                Box::new(K::IntegerLit(v as i32)).with_loc(span)
            }

            (_, Spanned { base: Tok::Str(ref s), span }) => {
                Box::new(K::StringLit(s.to_owned().into())).with_loc(span)
            }

            tok => {
                self.unread(tok);
                return Ok(None);
            }
        };

        // postfix type operators cannot appear twice in a row
        // (can work around with parens, but still not well-formed and the checker will error)
        if self.may_expect(Punct::Ques) {
            kind = Box::new(K::WithNil(kind)).with_loc(begin..self.last_pos());
        } else if self.may_expect(Punct::Bang) {
            kind = Box::new(K::WithoutNil(kind)).with_loc(begin..self.last_pos());
        }

        Ok(Some(AtomicKind::One(kind)))
    }

    fn try_parse_kailua_prefixed_kind_seq(&mut self) -> Result<Option<AtomicKind>> {
        let begin = self.pos();

        let attr = match self.try_parse_kailua_attr() {
            Ok(attr) => attr,
            Err(Stop::Recover) => None,
            Err(Stop::Fatal) => return Err(Stop::Fatal),
        };
        if let Some(kindseq) = try!(self.try_parse_kailua_atomic_kind_seq()) {
            let end = self.last_pos();

            // apply an attribute if any
            if let Some(attr) = attr {
                let apply_attr = |kind| {
                    let kind = Box::new(K::Attr(kind, attr));
                    kind.with_loc(begin..end)
                };

                match kindseq {
                    AtomicKind::One(kind) => {
                        let kind = apply_attr(kind);
                        Ok(Some(AtomicKind::One(kind)))
                    }
                    AtomicKind::Seq(mut kindseq) => {
                        if kindseq.head.len() == 1 && kindseq.tail.is_none() {
                            // we are using AtomicKind::Seq to signal the end of the parsing,
                            // so we need to keep the variant as is
                            let kind = apply_attr(kindseq.head.pop().unwrap());
                            kindseq.head.push(kind);
                        } else {
                            try!(self.error(begin..end, m::AttrToKindSeq {}).done());
                        }
                        Ok(Some(AtomicKind::Seq(kindseq)))
                    }
                }
            } else {
                Ok(Some(kindseq))
            }
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_kind_seq(&mut self) -> Result<Option<Spanned<Seq<Spanned<Kind>>>>> {
        let begin = self.pos();
        match try!(self.try_parse_kailua_prefixed_kind_seq()) {
            None => Ok(None),
            Some(AtomicKind::Seq(kindseq)) => {
                Ok(Some(kindseq.with_loc(begin..self.last_pos())))
            }
            Some(AtomicKind::One(mut kind)) => {
                if self.lookahead(Punct::Pipe) { // A | B | ...
                    // TODO the current parser is massively ambiguous about pipes in atomic types
                    let mut kinds = vec![kind];
                    while self.may_expect(Punct::Pipe) {
                        let begin = self.pos();
                        match try!(self.try_parse_kailua_prefixed_kind_seq()) {
                            Some(AtomicKind::One(kind2)) => {
                                kinds.push(kind2);
                            }
                            Some(AtomicKind::Seq(..)) => {
                                try!(self.error(begin..self.last_pos(), m::NoTypeSeqInUnion {})
                                         .done());
                                kinds.push(self.dummy_kind());
                            }
                            None => {
                                let tok = self.read().1;
                                return self.fatal(tok.span, m::NoType { read: &tok.base }).done();
                            }
                        }
                    }
                    kind = Box::new(K::Union(kinds)).with_loc(begin..self.last_pos());
                }
                let kindseq = Seq { head: vec![kind], tail: None };
                Ok(Some(kindseq.with_loc(begin..self.last_pos())))
            }
        }
    }

    fn try_parse_kailua_kind(&mut self) -> Result<Option<Spanned<Kind>>> {
        if let Some(mut kindseq) = try!(self.try_parse_kailua_kind_seq()) {
            if kindseq.base.head.len() == 1 && kindseq.base.tail.is_none() {
                let first = kindseq.base.head.pop().unwrap();
                let span = kindseq.span | first.span; // overwrite the span
                Ok(Some(first.base.with_loc(span)))
            } else {
                try!(self.error(kindseq.span, m::NoSingleTypeButTypeSeq {}).done());
                Ok(Some(self.dummy_kind()))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_kailua_kind_seq(&mut self) -> Result<Seq<Spanned<Kind>>> {
        if let Some(kindseq) = try!(self.try_parse_kailua_kind_seq()) {
            Ok(kindseq.base)
        } else {
            let tok = self.read().1;
            try!(self.error(tok.span, m::NoTypeOrTypeSeq { read: &tok.base }).done());
            Ok(Seq { head: vec![self.dummy_kind()], tail: None })
        }
    }

    fn parse_kailua_kind(&mut self) -> Result<Spanned<Kind>> {
        if let Some(kind) = try!(self.try_parse_kailua_kind()) {
            Ok(kind)
        } else {
            let tok = self.read();
            try!(self.error(tok.1.span, m::NoSingleType { read: &tok.1.base }).done());
            self.unread(tok);
            Ok(self.dummy_kind())
        }
    }

    fn parse_kailua_kind_with_spanned_modf(&mut self) -> Result<(Spanned<M>, Spanned<Kind>)> {
        let begin = self.pos();
        let modf = match self.parse_kailua_mod() {
            M::None => M::None.with_loc(begin), // i.e. the beginning of the kind
            modf => modf.with_loc(begin..self.last_pos()),
        };
        let kind = try!(self.parse_kailua_kind());
        Ok((modf, kind))
    }

    fn try_parse_kailua_type_spec_with_spanned_modf(&mut self)
            -> Result<Option<Spanned<(Spanned<M>, Spanned<Kind>)>>> {
        let metabegin = self.pos();
        if self.may_expect(Punct::DashDashColon) {
            // allow for `--: { a = foo,
            //            --:   b = bar }`
            self.begin_meta_comment(Punct::DashDashColon);
            let spec = try!(self.parse_kailua_kind_with_spanned_modf());
            let metaend = self.last_pos();
            // allow for `--: last type --> return type` in the function decl
            if !self.lookahead(Punct::DashDashGt) {
                try!(self.end_meta_comment(Punct::DashDashColon));
            } else {
                assert_eq!(self.ignore_after_newline, Some(Punct::DashDashColon));
                self.ignore_after_newline = None;
                // it is possible to have `--: { a = foo,
                //                         --:   b = bar }
                //                         --: --> return_type`. seems harmless though.
                self.elided_newline = None;
            }
            Ok(Some(spec.with_loc(metabegin..metaend)))
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_type_spec(&mut self) -> Result<Option<Spanned<(M, Spanned<Kind>)>>> {
        let spec = try!(self.try_parse_kailua_type_spec_with_spanned_modf());
        Ok(spec.map(|spec| spec.map(|(m,k)| (m.base, k))))
    }

    fn try_parse_kailua_typeseq_spec(&mut self)
            -> Result<Option<Spanned<Vec<Spanned<(M, Spanned<Kind>)>>>>> {
        let metabegin = self.pos();
        if self.may_expect(Punct::DashDashColon) {
            self.begin_meta_comment(Punct::DashDashColon);

            let mut specs = Vec::new();
            try!(self.scan_list(|parser| {
                let begin = parser.pos();
                let spec = try!(parser.parse_kailua_kind_with_spanned_modf());
                Ok(spec.with_loc(begin..parser.last_pos()))
            }, |spec| {
                specs.push(spec.map(|(m,k)| (m.base, k)));
            }));

            let metaend = self.last_pos();
            if !self.lookahead(Punct::DashDashGt) {
                try!(self.end_meta_comment(Punct::DashDashColon));
            } else {
                assert_eq!(self.ignore_after_newline, Some(Punct::DashDashColon));
                self.ignore_after_newline = None;
                self.elided_newline = None;
            }
            Ok(Some(specs.with_loc(metabegin..metaend)))
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_rettype_spec(&mut self)
            -> Result<Option<Spanned<Seq<Spanned<Kind>>>>> {
        let begin = self.pos();
        if self.may_expect(Punct::DashDashGt) {
            self.begin_meta_comment(Punct::DashDashGt);
            let kinds = try!(self.parse_kailua_kind_seq());
            let end = self.last_pos();
            try!(self.end_meta_comment(Punct::DashDashGt));

            Ok(Some(kinds.with_loc(begin..end)))
        } else {
            Ok(None)
        }
    }

    // it may supply only attributes, so we need to return attributes and pre-signatures separately
    fn try_parse_kailua_func_spec(&mut self)
            -> Result<Option<Spanned<(Vec<Spanned<Attr>>, Option<Spanned<Presig>>)>>> {
        let metabegin = self.pos();
        if self.may_expect(Punct::DashDashV) {
            self.begin_meta_comment(Punct::DashDashV);

            let mut attrs = Vec::new();
            let mut attrs_seen = false;
            loop {
                match self.try_parse_kailua_attr() {
                    Ok(Some(attr)) => { attrs_seen = true; attrs.push(attr); }
                    Ok(None) => break,
                    Err(Stop::Fatal) => return Err(Stop::Fatal),
                    // we have seen an attribute but couldn't parse it
                    Err(Stop::Recover) => { attrs_seen = true; }
                }
            }

            // function "(" [NAME ":" KIND] {"," NAME ":" KIND} ["," "..."] ")" ["-->" KIND]
            let begin = self.pos();
            let has_sig = if !attrs_seen {
                // force reading signatures if no attributes are present
                try!(self.expect(Keyword::Function));
                true
            } else {
                self.may_expect(Keyword::Function)
            };
            let sig = if has_sig {
                try!(self.expect(Punct::LParen));
                let args = try!(self.parse_kailua_namekindlist());
                try!(self.expect(Punct::RParen));
                let returns = if self.may_expect(Punct::DashDashGt) {
                    try!(self.parse_kailua_kind_seq())
                } else {
                    Seq { head: Vec::new(), tail: None }
                };
                let end = self.last_pos();
                Some(Presig { args: args, returns: Some(returns) }.with_loc(begin..end))
            } else {
                None
            };

            let metaend = self.last_pos();
            try!(self.end_meta_comment(Punct::DashDashV));
            Ok(Some((attrs, sig).with_loc(metabegin..metaend)))
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_spec(&mut self) -> Result<Option<Option<Spanned<Stmt>>>> {
        let begin = self.pos();
        if self.may_expect(Punct::DashDashHash) {
            self.begin_meta_comment(Punct::DashDashHash);

            let stmt = match self.read() {
                // assume [global] NAME ":" MODF KIND
                (_, Spanned { base: Tok::Keyword(Keyword::Assume), .. }) => {
                    let scope = if self.may_expect(Keyword::Global) {
                        NameScope::Global
                    } else {
                        NameScope::Local
                    };
                    let name = try!(self.parse_name());
                    try!(self.expect(Punct::Colon));
                    let modf = self.parse_kailua_mod();
                    let kind = try!(self.parse_kailua_kind());

                    Some(Box::new(St::KailuaAssume(scope, name, modf, kind)))
                }

                // open NAME
                (_, Spanned { base: Tok::Keyword(Keyword::Open), .. }) => {
                    let name = try!(self.parse_name());
                    // TODO also set the parser option
                    Some(Box::new(St::KailuaOpen(name)))
                }

                // type NAME = KIND
                (_, Spanned { base: Tok::Keyword(Keyword::Type), .. }) => {
                    let name = try!(self.parse_name());
                    try!(self.expect(Punct::Eq));
                    let kind = try!(self.parse_kailua_kind());

                    // forbid overriding builtin types
                    if self.builtin_kind(&*name.base).is_some() {
                        try!(self.error(name.span, m::CannotRedefineBuiltin {}).done());
                    }

                    Some(Box::new(St::KailuaType(name, kind)))
                }

                tok => {
                    self.unread(tok);
                    None // empty `--#` is valid
                }
            };

            let end = self.last_pos();
            try!(self.end_meta_comment(Punct::DashDashHash));
            Ok(Some(stmt.map(|st| st.with_loc(begin..end))))
        } else {
            Ok(None)
        }
    }

    pub fn into_chunk(mut self) -> diag::Result<Spanned<Block>> {
        self.parse_block_then_eof().map_err(|_| diag::Stop)
    }
}

