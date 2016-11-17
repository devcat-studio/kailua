use std::iter;
use std::u8;
use std::i32;
use std::fmt;
use std::result;
use std::collections::{hash_map, HashMap, HashSet};
use kailua_env::{Pos, Span, Spanned, WithLoc, Scope, ScopeMap};
use kailua_diag as diag;
use kailua_diag::{Report, Localize};

use message as m;
use lang::{Language, Lua, Kailua};
use lex::{Tok, Punct, Keyword, NestedToken, NestingCategory, NestingSerial};
use ast::{Name, NameRef, Str, Var, Seq, Presig, Sig, Attr, Args};
use ast::{Ex, Exp, UnOp, BinOp, SelfParam, St, Stmt, Block};
use ast::{M, K, Kind, SlotKind, FuncKind, TypeSpec, Chunk};

pub struct Parser<'a> {
    iter: iter::Fuse<&'a mut Iterator<Item=NestedToken>>,
    language: Language,

    // the lookahead stream (in this order)
    elided_newline: Option<Span>, // same to Side.elided_tokens below
    lookahead: Option<NestedToken>,
    lookahead2: Option<NestedToken>,
    // ...follows self.iter.next()

    // the spans for the most recent `read()` tokens
    last_span: Span,
    last_span2: Span,

    last_nesting_depth: u16,
    last_nesting_serial: NestingSerial,

    ignore_after_newline: Option<Punct>,
    report: &'a Report,

    scope_map: ScopeMap<Name>,
    // the global scope; visible to every other file and does not go through a scope map
    // (the local root scopes are generated as needed, and invisible from the outside)
    global_scope: HashSet<Name>,
    // Pos for the starting position
    scope_stack: Vec<(Scope, Pos)>,
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

// superset of Expectable, used for auto-recovery delimiter
trait ExpectableDelim {
    fn expect_delim<'a>(self, parser: &mut Parser<'a>) -> Result<()>;
}

impl<Exp: Expectable> ExpectableDelim for Exp {
    fn expect_delim<'a>(self, parser: &mut Parser<'a>) -> Result<()> {
        parser.expect(self)
    }
}

// when used with `Parser::recover[_with]`, it means that the enclosed closure will end
// the closing token as well. this is normally not recommended (easy to miss) but
// meta comments typically have a longer cleanup process, so this is required.
#[derive(Copy, Clone)]
struct DelimAlreadyRead;

impl ExpectableDelim for DelimAlreadyRead {
    fn expect_delim<'a>(self, _parser: &mut Parser<'a>) -> Result<()> {
        Ok(())
    }
}

// the "default" dummy value for recovered types
trait Recover {
    fn recover() -> Self;
}

impl Recover for () {
    fn recover() -> Self { () }
}

impl<T1: Recover, T2: Recover> Recover for (T1, T2) {
    fn recover() -> Self { (Recover::recover(), Recover::recover()) }
}

impl<T1: Recover, T2: Recover, T3: Recover> Recover for (T1, T2, T3) {
    fn recover() -> Self { (Recover::recover(), Recover::recover(), Recover::recover()) }
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
    ($e:expr $(=> $delim:expr)+) => (
        |parser: &mut Parser<'a>| {
            let saved = $e(parser)?;
            $($delim.expect_delim(parser)?;)+
            Ok(saved)
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

    // the prior nesting information before reading the token
    last_nesting_serial: NestingSerial,
    last_nesting_depth: u16,

    // a side information provided from NestedToken
    nesting_depth: u16,
    nesting_category: NestingCategory,
    nesting_serial: NestingSerial,
}

#[derive(Copy, Clone, Debug)]
struct ElidedTokens(Option<Span>);

enum AtomicKind { One(Spanned<Kind>), Seq(Seq<Spanned<Kind>>) }

impl<'a> Report for Parser<'a> {
    fn add_span(&self, k: diag::Kind, s: Span, m: &Localize) -> diag::Result<()> {
        self.report.add_span(k, s, m)
    }
}

// wrappers around kailua_diag::{ReportMore, Reporter}, used to remap `done` method
#[must_use]
struct ReportMore<'a, T>(diag::ReportMore<'a, T>);

#[allow(dead_code)]
impl<'a> Parser<'a> {
    fn fatal<Loc: Into<Span>, Msg: Localize, T>(&self, loc: Loc, msg: Msg) -> ReportMore<T> {
        ReportMore(diag::Reporter::fatal(self, loc, msg))
    }

    fn error<Loc: Into<Span>, Msg: Localize>(&self, loc: Loc, msg: Msg) -> ReportMore<()> {
        ReportMore(diag::Reporter::error(self, loc, msg))
    }

    fn warn<Loc: Into<Span>, Msg: Localize>(&self, loc: Loc, msg: Msg) -> ReportMore<()> {
        ReportMore(diag::Reporter::warn(self, loc, msg))
    }

    fn info<Loc: Into<Span>, Msg: Localize>(&self, loc: Loc, msg: Msg) -> ReportMore<()> {
        ReportMore(diag::Reporter::info(self, loc, msg))
    }
}

#[allow(dead_code)]
impl<'a, T> ReportMore<'a, T> {
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

impl<'a> Parser<'a> {
    pub fn new(iter: &'a mut Iterator<Item=NestedToken>, report: &'a Report) -> Parser<'a> {
        let mut parser = Parser {
            iter: iter.fuse(),
            language: Language::new(Lua::Lua51, Kailua::Kailua01), // XXX for now
            elided_newline: None,
            lookahead: None,
            lookahead2: None,
            last_span: Span::dummy(),
            last_span2: Span::dummy(),
            last_nesting_depth: 0,
            last_nesting_serial: NestingSerial::dummy(),
            ignore_after_newline: None,
            report: report,
            scope_map: ScopeMap::new(),
            global_scope: HashSet::new(),
            scope_stack: Vec::new(),
        };

        // read the first token and fill the last_span
        let first = parser._next().expect("lexer gave no token");
        parser.last_span = first.tok.span.begin().into();
        parser.lookahead = Some(first);
        parser
    }

    fn _next(&mut self) -> Option<NestedToken> {
        loop {
            let mut next = self.iter.next();

            if let Some(ref mut t) = next {
                trace!("got {:?}", *t);
                if false { // useful for debugging
                    let _ = self.info(t.tok.span, format!("got {:?}", t.tok.base)).done();
                }

                // comments should be ignored in the parser
                if let Tok::Comment = t.tok.base { continue; }

                // `goto` is converted to a name on Lua 5.1
                let lua = self.language.lua();
                if lua < Lua::Lua52 {
                    if let Tok::Keyword(kw @ Keyword::Goto) = t.tok.base {
                        // XXX don't want to make this failable
                        let _ = self.warn(t.tok.span,
                                          m::FutureKeyword { read: &t.tok.base, current: lua,
                                                             future: Lua::Lua52 })
                                    .done();
                        t.tok.base = Tok::Name(kw.name().to_owned());
                    }
                }
            }

            return next;
        }
    }

    fn _read(&mut self) -> (ElidedTokens, NestedToken) {
        let mut next = self.lookahead.take().or_else(|| self.lookahead2.take())
                                            .or_else(|| self._next());

        let mut elided = self.elided_newline.take();
        if let Some(meta) = self.ignore_after_newline {
            // lookahead2 can definitely be made empty, let's simplify the assumption
            assert_eq!(self.lookahead, None);
            self.lookahead = self.lookahead2.take();

            while next.as_ref().map(|t| &t.tok.base) == Some(&Tok::Punct(Punct::Newline)) {
                let next2 = self.lookahead.take().or_else(|| self._next());
                if next2.as_ref().map(|t| &t.tok.base) == Some(&Tok::Punct(meta)) {
                    // we can ignore them, but we may have another ignorable tokens there
                    elided = Some(next2.unwrap().tok.span);
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
        let side = Side {
            elided_tokens: elided,
            last_nesting_serial: self.last_nesting_serial,
            last_nesting_depth: self.last_nesting_depth,
            nesting_depth: next.depth,
            nesting_category: next.category,
            nesting_serial: next.serial,
        };

        self.last_span2 = self.last_span;
        self.last_span = next.tok.span;
        self.last_nesting_depth = next.depth;
        self.last_nesting_serial = next.serial;

        (side, next.tok)
    }

    fn _unread(&mut self, ElidedTokens(elided): ElidedTokens, tok: NestedToken) {
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
        let tok = NestedToken {
            tok: tok,
            depth: side.nesting_depth,
            category: side.nesting_category,
            serial: side.nesting_serial,
        };
        self._unread(side.elided_tokens, tok);

        // if we can, reconstruct the last span from the last elided token
        if let ElidedTokens(Some(span)) = side.elided_tokens {
            self.last_span = span;
        } else {
            self.last_span = self.last_span2;
            self.last_span2 = Span::dummy();
        }
        self.last_nesting_depth = side.last_nesting_depth;
        self.last_nesting_serial = side.last_nesting_serial;
    }

    fn peek<'b>(&'b mut self) -> &'b Spanned<Tok> {
        if self.lookahead.is_none() {
            // keep the side information as much as possible
            let (elided, tok) = self._read();
            self._unread(elided, tok);
        }
        &self.lookahead.as_ref().unwrap().tok
    }

    fn expect<Tok: Expectable>(&mut self, tok: Tok) -> Result<()> {
        let read = self.read();
        if !tok.check_token(&read.1.base) {
            self.error(read.1.span, m::ExpectFailed { expected: tok, read: &read.1.base }).done()?;
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
            self.read();
            true
        } else {
            false
        }
    }

    // some notes on error recovery.
    //
    // the error recovery basically works by skipping zero or more tokens and
    // resuming parsing at that point. we have several dummy nodes (conventionally
    // named `Oops`) for this purpose and the `Recover` trait can be used for automating that.
    // we can do much better, but the recovery is at best approximate and
    // complex recovery process results in ambiguity and parsing failure.
    //
    // the parser recognizes a set of "delimiter" tokens that forms a hierarchy of
    // "nestings", and reading or unreading a token will update the current list of
    // nestings where the cursor is located. the delimiter is not limited to
    // the natural delimiters like parentheses; known keywords are also used as delimiters.
    // one important thing about nestings is that they can be closed even when nested;
    // if one is given `for i = x * (3 + end`, one can conclude that the final `end` is for
    // the first `for` even though `(` has opened a new nesting. the exception is
    // when there is no matching opening token at all, in which case the token is ignored.
    // (there are also some special cases, e.g. one token can close and immediately open
    // the same nesting, but otherwise they are fairly regular.)
    //
    // other parts of the parser are expected to do one of the following on error:
    //
    // 1. unread any read token and signal a "recoverable" error, `Stop::Recover`.
    //    this is a default action for the parsing errors.
    //
    // 2. unread any read token and immediately continue with a dummy node.
    //    this should only be used for the well-formedness errors,
    //    otherwise it will confuse the callers and raise multiple errors at best
    //    or read past the EOF and panic at worst.
    //
    // 3. skip to the next closing delimiting token.
    //    note that this should keep the initial depth of nestings,
    //    so that nested errors uncaught can be correctly recovered.
    //    this is a default action at the delimiter boundaries.
    //
    //    there are three possibilities in what to do after the recovery.
    //    since it is common to expect the closing delimiter after the item,
    //    first two options will read the closing delimiter on the successful parsing.
    //
    //    - creating a dummy node manually: `self.recover_with(body, closing_delim, dummy)?`.
    //
    //    - creating a dummy node automatically: `self.recover(body, closing_delim)?`.
    //      same to `self.recover_with(body, closing_delim, Recover::recover)?`.
    //
    //    - propagating the error: `self.recover_retry(false, body)?`.
    //      this is rarely used when there is no suitable dummy node.
    //
    //    `recover_to_close` can also be used when it is hard to use the "wrapping" style.
    //    (in this case the closing delimiter is implicit. in fact, `closing_delim` is never
    //    checked and only used to ensure that they are used in the correct fashion.)
    //    in any case, the opening token should have been already read.
    //
    // 4. same to above but keep the delimiting token.
    //    this is a default action for the items that do not read the delimiter themselves,
    //    so that this can be called multiple times in the same nesting.
    //
    //    `recover_upto`, `recover_upto_with` methods and
    //    `recover_retry` call with `true` argument are used for this action.
    //    combined with the action 3, any delimiter closing an explicitly read nesting
    //    will be either explicitly read or implicitly skipped during the action 3.
    //
    // 5. propagate any error (via `?`).
    //
    // 6. (not recommended) raise a fatal error, `Stop::Fatal`.

    fn _recover_save(&self) -> (u16, NestingSerial) {
        let init_depth = self.last_nesting_depth;
        let init_serial = self.last_nesting_serial;
        assert!(init_depth > 0);
        (init_depth, init_serial)
    }

    // always_unread is true if the caller wants to close the nesting by its own,
    // so the recovery procedure itself should not read the closing delimiter.
    //
    // ignore_same_depth_update is true if the recovery procedure should not treat
    // the update to the nesting at the same level (e.g. `elseif` block is popped and repushed).
    // this is required for the meta comment recovery, because a consecutive meta comment is
    // seen as connected to the parser (without no newline tokens);
    // each meta comment will receive a different nesting serial number and
    // the parser will try to read the first token after meta token in the next line
    // as a normal, non-meta token (which is likely to fail).
    // unless we are doing a component-wise recovery (e.g. recovering kinds in
    // `--# assume x: SOME INVALID KIND SYNTAX`), it's better to span all the comments.
    fn _recover(&mut self, always_unread: bool, ignore_same_depth_update: bool,
                (init_depth, init_serial): (u16, NestingSerial)) {
        debug!("recovering from the error, {:?} at depth {} -> {:?} at depth {}",
               init_serial, init_depth, self.last_nesting_serial, self.last_nesting_depth);

        if self.last_nesting_depth >= init_depth {
            let mut last_tok;
            let mut same_depth_update;
            loop {
                last_tok = self.read();
                let depth = self.last_nesting_depth;
                let serial = self.last_nesting_serial;
                trace!("skipping {:?}, {:?} at depth {}", last_tok.1, serial, depth);

                // we stop when the nesting depth falls below the initial depth _at any time_,
                // not just after the call. the nesting serial number can detect this case.
                same_depth_update = !ignore_same_depth_update &&
                                    depth == init_depth && serial != init_serial;
                if depth < init_depth || same_depth_update { break; }
            }

            // if the last token has closed too many nestings we need to keep
            // that last token to allow the further match from the caller.
            // this case includes a premature EOF, where reading EOF would close
            // at least two nestings (the initial nesting, the top-level nesting).
            //
            // on the other hands, a different minimal depth and current depth indicates
            // that a new nesting is introduced by this token and that token should be kept.
            // (this is just a convention, the other choice is possible)
            let excessive_closing = self.last_nesting_depth < init_depth - 1;
            if always_unread || excessive_closing || same_depth_update {
                trace!("unreading the final token {}",
                       if always_unread { "at a request" }
                       else if excessive_closing { "due to excessive closing" }
                       else { "due to nesting update at the same level" });
                self.unread(last_tok);
            }
        }

        debug!("recovered from the error, {:?} at depth {}",
               self.last_nesting_serial, self.last_nesting_depth);
    }

    // ensures that the nesting depth of the pseudo-tree of tokens is lower than the beginning
    fn recover_retry<F, R>(&mut self, always_unread: bool, ignore_same_depth_update: bool,
                           f: F) -> Result<R>
        where F: FnOnce(&mut Parser<'a>) -> Result<R>
    {
        let saved = self._recover_save();
        trace!("set the recover trap, {:?} at depth {}",
               self.last_nesting_serial, self.last_nesting_depth);

        let ret = f(self);

        if ret.is_err() {
            self._recover(always_unread, ignore_same_depth_update, saved);
        } else {
            trace!("no recovery required, {:?} at depth {}",
                   self.last_nesting_serial, self.last_nesting_depth);
        }

        ret
    }

    fn recover_with<F, D, H, R>(&mut self, f: F, delim: D, handler: H) -> Result<R>
        where F: FnOnce(&mut Parser<'a>) -> Result<R>, D: ExpectableDelim, H: FnOnce() -> R
    {
        match self.recover_retry(false, false, lastly!(f => delim)) {
            Ok(v) => Ok(v),
            Err(Stop::Recover) => Ok(handler()),
            Err(Stop::Fatal) => Err(Stop::Fatal),
        }
    }

    fn recover_upto_with<F, H, R>(&mut self, f: F, handler: H) -> Result<R>
        where F: FnOnce(&mut Parser<'a>) -> Result<R>, H: FnOnce() -> R
    {
        match self.recover_retry(true, false, f) {
            Ok(v) => Ok(v),
            Err(Stop::Recover) => Ok(handler()),
            Err(Stop::Fatal) => Err(Stop::Fatal),
        }
    }

    fn recover<F, D, R>(&mut self, f: F, delim: D) -> Result<R>
        where F: FnOnce(&mut Parser<'a>) -> Result<R>, D: ExpectableDelim, R: Recover
    {
        self.recover_with(f, delim, Recover::recover)
    }

    fn recover_upto<F, R: Recover>(&mut self, f: F) -> Result<R>
        where F: FnOnce(&mut Parser<'a>) -> Result<R>
    {
        self.recover_upto_with(f, Recover::recover)
    }

    fn recover_to_close(&mut self) {
        let _: Result<()> = self.recover_retry(false, false, |_| Err(Stop::Recover));
    }

    fn recover_meta<F, H, R>(&mut self, f: F, handler: H) -> Result<R>
        where F: FnOnce(&mut Parser<'a>) -> Result<R>, H: FnOnce() -> R
    {
        match self.recover_retry(false, true, f) {
            Ok(v) => Ok(v),
            Err(Stop::Recover) => Ok(handler()),
            Err(Stop::Fatal) => Err(Stop::Fatal),
        }
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
                // meta will receive the same nesting information as the current token
                assert_eq!(self.lookahead2, None);
                self.lookahead2 = self.lookahead.take();
                self.lookahead = Some(NestedToken {
                    tok: Tok::Punct(meta).with_loc(metaspan),
                    depth: self.last_nesting_depth,
                    category: NestingCategory::Meta,
                    serial: self.last_nesting_serial,
                });
            } else {
                let next = self.peek().clone();
                self.error(next.span, m::NoNewline { read: &next.base }).done()?;
                self.skip_meta_comment(meta);
            }
        }

        self.ignore_after_newline = None;
        Ok(())
    }

    // consumes a newline, used for error recovery
    fn skip_meta_comment(&mut self, meta: Punct) {
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

    // resolve the name referenced in the current scope (might be global) into a reference
    fn resolve_name(&mut self, name: Name) -> NameRef {
        let local = if let Some(&(scope, _)) = self.scope_stack.last() {
            self.scope_map.find_name_in_scope(scope, &name)
        } else {
            None
        };
        if let Some((_, scoped_id)) = local {
            NameRef::Local(scoped_id)
        } else {
            NameRef::Global(name)
        }
    }

    fn generate_sibling_scope(&mut self) -> Scope {
        if let Some(&(parent, _parentbegin)) = self.scope_stack.last() {
            self.scope_map.generate(parent)
        } else {
            self.scope_map.generate_root()
        }
    }

    fn push_scope(&mut self, scope: Scope) {
        let scopebegin = self.last_pos();
        self.scope_stack.push((scope, scopebegin));
    }

    fn pop_scope_upto(&mut self, nscopes: usize) {
        if self.scope_stack.len() > nscopes {
            // XXX end might be too short on the recovery case.
            // scope span should be closely related to the recovery procedure
            let end = self.pos();
            while self.scope_stack.len() > nscopes {
                let (scope, scopebegin) = self.scope_stack.pop().unwrap();
                self.scope_map.set_span(scope.with_loc(scopebegin..end));
            }
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
            self.error(tok.1.span, m::NoName { read: &tok.1.base }).done()?;
            self.unread(tok);
            Err(Stop::Recover)
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
                self.error(tok.1.span, m::NoName { read: &tok.1.base }).done()?;
                self.unread(tok);
                Err(Stop::Recover)
            }
        }
    }

    fn _parse_block(&mut self) -> Result<Spanned<Block>> {
        trace!("parsing block");

        let begin = self.pos();
        let mut stmts = Vec::new();
        let mut pastlast = false;
        let mut excessspan = None;
        while let Some(stmt) = self.try_parse_stmt()? {
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
            self.error(span, m::StmtAfterReturnOrBreak {}).done()?;
        }
        Ok(stmts.with_loc(begin..self.last_pos()))
    }

    // will reset the scope after the end of the block
    fn parse_block(&mut self) -> Result<Spanned<Block>> {
        let nscopes = self.scope_stack.len();
        let block = self._parse_block();
        self.pop_scope_upto(nscopes);
        block
    }

    // same to `parse_block`, but will also set the scope in the block.
    // in order to keep the lexical order of scoped ids (purely for cosmetic & debugging reasons),
    // the block can be supplied to run on that scope before `parse_block`.
    fn parse_block_with_scope<F, X>(&mut self, preblock: F) -> (X, Scope, Result<Spanned<Block>>)
        where F: FnOnce(&mut Parser<'a>, Scope) -> X
    {
        let nscopes = self.scope_stack.len();
        let scope = self.generate_sibling_scope();
        self.push_scope(scope);
        let preret = preblock(self, scope);
        let block = self._parse_block();
        self.pop_scope_upto(nscopes);
        (preret, scope, block)
    }

    // unlike `parse_block`, this will try to parse as much as possible until EOF
    // never consumes the EOF itself, required for the later cleanup process
    fn parse_block_until_eof(&mut self) -> Result<Spanned<Block>> {
        trace!("parsing block then EOF");

        let begin = self.pos();
        let mut stmts = Vec::new();
        let mut pastlast = false;
        let mut excessspan = None;
        loop {
            let stmt = match self.try_parse_stmt() {
                Ok(Some(stmt)) => stmt,
                Ok(None) => {
                    // try to detect the EOF (and return).
                    // if we can't, we will discard one token and retry.
                    if self.lookahead(EOF) { break; }
                    stmts.push(Recover::recover());
                    let tok = self.read();
                    self.error(tok.1.span, m::NoStmt { read: &tok.1.base }).done()?;
                    continue;
                }
                Err(_) => {
                    // try to detect the EOF (and return).
                    // if we can't, we will discard one token and retry.
                    if self.lookahead(EOF) { break; }
                    stmts.push(Recover::recover());
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
            self.error(span, m::StmtAfterReturnOrBreak {}).done()?;
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
            self.error(specs.span, note_on_dup).done()?;
            Ok(oldspecs)
        } else if oldspecs.len() > specs.base.len() {
            let span = {
                let excess = &oldspecs[specs.base.len()..];
                excess.iter().fold(Span::dummy(), |span, i| {
                    span | i.base.span | i.kind.as_ref().map_or(Span::dummy(), |k| k.span)
                })
            };
            self.error(span, note_on_less).done()?;
            Ok(oldspecs)
        } else if oldspecs.len() < specs.base.len() {
            let span = {
                let excess = &specs.base[oldspecs.len()..];
                excess.iter().fold(Span::dummy(), |span, i| span | i.span)
            };
            self.error(span, note_on_more).done()?;
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
        trace!("parsing stmt");
        let begin = self.pos();

        let funcspec = self.try_parse_kailua_func_spec()?;
        if let Some(ref funcspec) = funcspec {
            // limit the possible lookahead
            let allowed = match self.peek().base {
                Tok::Keyword(Keyword::Function) => true,
                Tok::Keyword(Keyword::Local) => true, // `local NAME = ...` is filtered later
                _ => false,
            };
            if !allowed {
                self.error(funcspec.span, m::MissingFuncDeclAfterFuncSpec {}).done()?;
            }
        }

        // if there exists a spec stmt return it first.
        // a spec may be empty, so loop until no spec exists or a spec is found.
        loop {
            match self.try_parse_kailua_spec()? {
                Some(Some(spec)) => return Ok(Some(spec)),
                Some(None) => continue,
                None => break,
            }
        }

        let stmt = match self.read() {
            (_, Spanned { base: Tok::Keyword(Keyword::Do), .. }) => {
                let block = self.recover(Self::parse_block, Keyword::End)?;
                Box::new(St::Do(block))
            }

            (_, Spanned { base: Tok::Keyword(Keyword::While), .. }) => {
                let cond = self.recover(Self::parse_exp, Keyword::Do)?;
                let block = self.recover(Self::parse_block, Keyword::End)?;
                Box::new(St::While(cond, block))
            }

            (_, Spanned { base: Tok::Keyword(Keyword::Repeat), .. }) => {
                let block = self.recover(Self::parse_block, Keyword::Until)?;
                let cond = self.parse_exp()?;
                Box::new(St::Repeat(block, cond))
            }

            (_, Spanned { base: Tok::Keyword(Keyword::If), .. }) => {
                let mut blocks = Vec::new();
                let mut lastblock = self.recover(|parser| {
                    let cond = parser.recover(Self::parse_exp, Keyword::Then)?;
                    let block = parser.recover_upto(Self::parse_block)?;
                    blocks.push((cond, block).with_loc(begin..parser.last_pos()));
                    let mut begin = parser.pos();
                    while parser.may_expect(Keyword::Elseif) {
                        let cond = parser.recover(Self::parse_exp, Keyword::Then)?;
                        let block = parser.recover_upto(Self::parse_block)?;
                        blocks.push((cond, block).with_loc(begin..parser.last_pos()));
                        begin = parser.pos();
                    }
                    let lastblock = if parser.may_expect(Keyword::Else) {
                        Some(parser.recover_upto(Self::parse_block)?)
                    } else {
                        None
                    };
                    Ok(lastblock)
                }, Keyword::End)?;

                // fix the span for the last block appeared to include `end`
                if let Some(ref mut lastblock) = lastblock {
                    lastblock.span |= self.last_pos().into();
                } else {
                    blocks.last_mut().unwrap().span |= self.last_pos().into();
                }
                Box::new(St::If(blocks, lastblock))
            }

            (_, Spanned { base: Tok::Keyword(Keyword::For), .. }) => {
                let name = self.parse_name()?;
                match self.read() {
                    // for NAME "=" ...
                    (_, Spanned { base: Tok::Punct(Punct::Eq), .. }) => {
                        let start = self.parse_exp()?;
                        self.expect(Punct::Comma)?;
                        let end = self.parse_exp()?;
                        let step = if self.may_expect(Punct::Comma) {
                            Some(self.parse_exp()?)
                        } else {
                            None
                        };
                        self.expect(Keyword::Do)?;
                        let (id, scope, block) = self.parse_block_with_scope(|parser, scope| {
                            let id = name.map(|name| parser.scope_map.add_name(scope, name));
                            id
                        });
                        let block = self.recover(|_| block, Keyword::End)?;
                        Box::new(St::For(id, start, end, step, scope, block))
                    }

                    // for NAME in ...
                    (_, Spanned { base: Tok::Keyword(Keyword::In), .. }) => {
                        let span = name.span;
                        self.parse_stmt_for_in(vec![name].with_loc(span))?
                    }

                    // for NAME [SPEC] "," ... in ...
                    (_, Spanned { base: Tok::Punct(Punct::Comma), .. }) => {
                        let mut vars = vec![name];
                        let span = self.scan_list(Self::parse_name, |name| vars.push(name))?;
                        self.expect(Keyword::In)?;
                        self.parse_stmt_for_in(vars.with_loc(span))?
                    }

                    tok => {
                        self.error(tok.1.span, m::NoForInSep { read: &tok.1.base }).done()?;
                        self.unread(tok);
                        self.recover_to_close();
                        Recover::recover()
                    }
                }
            }

            (_, Spanned { base: Tok::Keyword(Keyword::Function), .. }) => {
                let namesbegin = self.pos();
                let rootname = self.parse_name()?.map(|name| self.resolve_name(name));
                if let NameRef::Global(ref name) = rootname.base {
                    // this will assign a global name, and is handled like a global assignment
                    self.global_scope.insert(name.clone());
                }
                let mut names = Vec::new();
                while self.may_expect(Punct::Dot) {
                    names.push(self.parse_name()?);
                }

                let mut funcspec = funcspec;
                let mut selfparam = None;
                if self.may_expect(Punct::Colon) {
                    names.push(self.parse_name()?);

                    // if this is a method, the pre-signature (if any) should have `self`
                    // as the first argument, either typed or not.
                    // (`parse_func_body` doesn't like untyped params, so we need to pop it)
                    if let Some(Spanned { base: (_, Some(ref mut presig)), .. }) = funcspec {
                        if !presig.args.head.is_empty() &&
                                **presig.args.head[0].base.base == *b"self" {
                            let Spanned { base: arg, span } = presig.args.head.remove(0);
                            selfparam = Some(TypeSpec { base: span,
                                                        modf: arg.modf, kind: arg.kind });
                        } else {
                            self.error(presig, m::MissingSelfInFuncSpec {}).done()?;
                            // and assume that there *were* a `self` without a type
                        }
                    }
                    if selfparam.is_none() {
                        selfparam = Some(TypeSpec { base: Span::dummy(),
                                                    modf: M::None, kind: None });
                    }
                }
                let namesend = self.last_pos();

                if let Some((selfparam, sig, scope, body)) = self.parse_func_body(selfparam,
                                                                                  funcspec)? {
                    if names.is_empty() {
                        assert!(selfparam.is_none(),
                                "ordinary function cannot have an implicit self");
                        Box::new(St::FuncDecl(rootname, sig, scope, body, None))
                    } else {
                        Box::new(St::MethodDecl((rootname, names).with_loc(namesbegin..namesend),
                                 selfparam, sig, scope, body))
                    }
                } else {
                    Box::new(St::Oops)
                }
            }

            (_, Spanned { base: Tok::Keyword(Keyword::Local), .. }) => {
                match self.read() {
                    // local function ...
                    (_, Spanned { base: Tok::Keyword(Keyword::Function), .. }) => {
                        let name = self.parse_name()?;
                        if let Some((_, sig, scope, body)) = self.parse_func_body(None, funcspec)? {
                            let sibling_scope = self.generate_sibling_scope();
                            self.push_scope(sibling_scope);
                            let name = name.map(|name| {
                                NameRef::Local(self.scope_map.add_name(sibling_scope, name))
                            });
                            Box::new(St::FuncDecl(name, sig, scope, body, Some(sibling_scope)))
                        } else {
                            Box::new(St::Oops)
                        }
                    }

                    // local NAME ...
                    tok @ (_, Spanned { base: Tok::Name(_), .. }) => {
                        self.unread(tok);

                        // forbid `--v ...` then `local NAME ...`
                        if let Some(ref funcspec) = funcspec {
                            self.error(funcspec.span, m::MissingFuncDeclAfterFuncSpec {}).done()?;
                        }

                        let mut names = Vec::new();
                        let (span, eq) =
                            self.scan_list_with_spec(Self::parse_name,
                                                     |namespec| names.push(namespec))?;
                        let mut names = names.with_loc(span);

                        let exps = if eq {
                            let mut exps = Vec::new();
                            let span = self.scan_list(Self::parse_exp, |exp| exps.push(exp))?;
                            exps.with_loc(span)
                        } else {
                            Vec::new().with_loc(self.last_pos())
                        };

                        if let Some(specs) = self.try_parse_kailua_typeseq_spec()? {
                            names = self.update_type_specs_with_typeseq_spec(
                                names, specs,
                                &m::DuplicateTypeSpecInLocal {},
                                &m::ExcessNamesInLocal {},
                                &m::ExcessTypeSpecsInLocal {})?;
                        }

                        let sibling_scope = self.generate_sibling_scope();
                        self.push_scope(sibling_scope);
                        // XXX should also mention all excess arguments
                        let namerefs = names.map(|names: Vec<_>| {
                            names.into_iter().map(|namespec: TypeSpec<_>| {
                                namespec.map(|name: Spanned<Name>| {
                                    name.map(|name| self.scope_map.add_name(sibling_scope, name))
                                })
                            }).collect()
                        });
                        Box::new(St::Local(namerefs, exps, sibling_scope))
                    }

                    tok => {
                        self.error(tok.1.span, m::NoFuncOrNameAfterLocal { read: &tok.1.base })
                            .done()?;
                        self.unread(tok);
                        Box::new(St::Oops)
                    }
                }
            }

            (_, Spanned { base: Tok::Keyword(Keyword::Return), .. }) => {
                let mut exps = Vec::new();
                let (span, _) = self.try_scan_explist(|exp| exps.push(exp))?;
                Box::new(St::Return(exps.with_loc(span)))
            }

            (_, Spanned { base: Tok::Keyword(Keyword::Break), .. }) => {
                Box::new(St::Break)
            }

            tok => {
                self.unread(tok);

                // only prefixexp can appear at this position, but it is very common that
                // incomplete expression results in a (partial) non-prefix expression.
                let exp = match self.try_parse_exp()? {
                    Some(exp) => exp,
                    None => return Ok(None),
                };

                // now there are three cases possible:
                // it might be a single statement as whole (a function call),
                // the beginning of `varlist "=" explist`,
                // or just an error otherwise.
                //
                // determine if prefixexp is a function call or an indexing op,
                // and convert it to `Var` for the second case.
                // (note that the prefixexp might as well be an op
                // indexing the return from a function call!)

                let possibly_lhs = match self.peek().base {
                    Tok::Punct(Punct::Comma) => true,           // exp `,` (exp ... `=` ...)
                    Tok::Punct(Punct::DashDashColon) => true,   // exp `--:` (KIND ...)
                    Tok::Punct(Punct::Eq) => true,              // exp `=` (...)
                    _ => false,
                };

                let exp_or_var = if possibly_lhs {
                    self.convert_and_register_var_from_exp(exp)
                } else {
                    // even when exp is prefixexp, if it is surely not a part of assignment,
                    // we will avoid putting it into var to avoid excess autocompletion entries.
                    Err(exp)
                };
                match exp_or_var {
                    // var {"," var} "=" explist
                    Ok(firstvar) => {
                        let mut lhs = Vec::new();
                        let mut span = firstvar.span;
                        let mut eq = false;

                        // we've already read the first var, but not comma and/or specs yet
                        // read the spec for the first var (if any) first,
                        // then proceed to normal var w/ spec list,
                        // then an equal sign (to allow the last spec to appear after `=`)
                        let mut firstspec = self.try_parse_kailua_type_spec()?;
                        if self.may_expect(Punct::Comma) {
                            if firstspec.is_none() {
                                firstspec = self.try_parse_kailua_type_spec()?;
                            }
                            lhs.push(self.make_kailua_typespec(firstvar, firstspec));
                            let (span_, eq_) =
                                self.scan_list_with_spec(Self::parse_var,
                                                         |varspec| lhs.push(varspec))?;
                            span |= span_;
                            eq = eq_;
                        } else {
                            lhs.push(self.make_kailua_typespec(firstvar, firstspec));
                        }

                        // the equal sign is mandatory, so ensure it here
                        let mut lhs = lhs.with_loc(span);
                        if !eq && !self.may_expect(Punct::Eq) {
                            let read = self.read();
                            self.error(&read.1, m::NoEq { read: &read.1 }).done()?;
                            self.unread(read);

                            // assume that it is like an assignment but without rhs
                            Box::new(St::Assign(lhs, None))
                        } else {
                            let mut rhs = Vec::new();
                            let span = self.scan_list(Self::parse_exp, |exp| rhs.push(exp))?;
                            let rhs = rhs.with_loc(span);

                            if let Some(specs) = self.try_parse_kailua_typeseq_spec()? {
                                lhs = self.update_type_specs_with_typeseq_spec(
                                    lhs, specs,
                                    &m::DuplicateTypeSpecInAssign {},
                                    &m::ExcessLvaluesInAssign {},
                                    &m::ExcessTypeSpecsInAssign {})?;
                            }

                            Box::new(St::Assign(lhs, Some(rhs)))
                        }
                    }

                    // prefixexp
                    Err(exp) => {
                        // non-prefix expression is an error but otherwise can be recovered
                        match *exp.base {
                            Ex::FuncCall(..) | Ex::MethodCall(..) => {}
                            _ => { self.error(&exp, m::NoFuncCall {}).done()?; }
                        }

                        Box::new(St::Void(exp))
                    }
                }
            }
        };

        Ok(Some(stmt.with_loc(begin..self.last_pos())))
    }

    fn parse_stmt_for_in(&mut self, names: Spanned<Vec<Spanned<Name>>>) -> Result<Stmt> {
        let mut exps = Vec::new();
        let span = self.scan_list(Self::parse_exp, |exp| exps.push(exp))?;
        let exps = exps.with_loc(span);
        self.expect(Keyword::Do)?;
        let (names, scope, block) = self.parse_block_with_scope(|parser, scope| {
            let names = names.map(|names: Vec<_>| {
                names.into_iter().map(|name: Spanned<Name>| {
                    name.map(|name| parser.scope_map.add_name(scope, name))
                }).collect()
            });
            names
        });
        let block = self.recover(|_| block, Keyword::End)?;
        Ok(Box::new(St::ForIn(names, exps, scope, block)))
    }

    fn parse_func_body(&mut self,
                       selfparam: Option<TypeSpec<Span>>,
                       funcspec: Option<Spanned<(Vec<Spanned<Attr>>, Option<Spanned<Presig>>)>>)
            -> Result<Option<(Option<TypeSpec<Spanned<SelfParam>>>, Sig, Scope, Spanned<Block>)>> {
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

        match self.read() {
            (_, Spanned { base: Tok::Punct(Punct::LParen), .. }) => {}
            tok => {
                // if no parentheses are found, the function parsing fails and should return Oops
                self.error(&tok.1, m::NoFuncArgs { read: &tok.1 }).done()?;
                self.unread(tok);
                self.recover_to_close();
                return Ok(None);
            }
        }

        let begin = self.pos();
        let mut spec = None;
        let mut variadic = None;
        let end = self.recover(|parser| {
            let mut name = None;
            match parser.read() {
                (_, Spanned { base: Tok::Punct(Punct::DotDotDot), span }) => {
                    variadic = Some(span);
                }
                (_, Spanned { base: Tok::Name(name0), span }) => {
                    name = Some(Name::from(name0).with_loc(span));
                    spec = parser.try_parse_kailua_type_spec()?; // 1)
                    while parser.may_expect(Punct::Comma) {
                        // try to read the type spec after a comma if there was no prior spec
                        if spec.is_none() { spec = parser.try_parse_kailua_type_spec()?; } // 2)
                        args.push((name.take().unwrap(), spec.take()));
                        let begin = parser.pos();
                        if parser.may_expect(Punct::DotDotDot) {
                            variadic = Some((begin..parser.last_pos()).into());
                            break;
                        } else {
                            name = Some(parser.parse_name()?);
                            spec = parser.try_parse_kailua_type_spec()?; // 1)
                        }
                    }
                }
                tok @ (_, Spanned { base: Tok::Punct(Punct::RParen), .. }) => {
                    parser.unread(tok);
                }
                tok => {
                    parser.error(tok.1.span, m::BadFuncArg { read: &tok.1.base }).done()?;
                    parser.unread(tok);
                    return Err(Stop::Recover);
                }
            }

            let end;
            if let Some(span) = variadic {
                // we've already read `...` and flushed the name-spec pair
                let mut varargs_ = parser.try_parse_kailua_type_spec_with_spanned_modf()?; // 4)
                end = parser.last_pos();
                parser.expect(Punct::RParen)?;
                if varargs_.is_none() {
                    varargs_ = parser.try_parse_kailua_type_spec_with_spanned_modf()?; // 5)
                }
                let varargs_ = if let Some(Spanned { base: (m, kind), .. }) = varargs_ {
                    if m.base != M::None {
                        parser.error(m.span, m::NoModfAllowedInVarargs {}).done()?;
                    }
                    Some(kind)
                } else {
                    None
                };
                varargs = Some(varargs_.with_loc(span));
            } else {
                end = parser.last_pos();
                parser.expect(Punct::RParen)?;
                if let Some(name) = name {
                    // the last type spec may follow the right parenthesis
                    if spec.is_none() { spec = parser.try_parse_kailua_type_spec()?; } // 3)
                    args.push((name, spec));
                }
            }

            Ok(Some(end))
        }, DelimAlreadyRead)?;
        let end = end.unwrap_or_else(|| self.last_pos());
        returns = self.try_parse_kailua_rettype_spec()?;

        let (attrs, args, returns) = if let Some(Spanned { base: (attrs, presig), .. }) = funcspec {
            if let Some(presig) = presig {
                // before any checking, we should ensure that every parameter has a type attached
                // (`self` is a notable exception, but should have been removed by now)
                for arg in &presig.base.args.head {
                    if arg.kind.is_none() {
                        self.error(&arg.base.base, m::MissingArgTypeInFuncSpec {}).done()?;
                    }
                }

                // if there is a pre-signature, any mismatching parameter or
                // inline argument type spec is an error
                if args.len() > presig.base.args.head.len() {
                    let excess = &args[presig.base.args.head.len()..];
                    let span = excess.iter().fold(Span::dummy(), |span, i| span | i.0.span);
                    self.error(span, m::ExcessArgsInFuncDecl {}).done()?;
                } else if args.len() < presig.base.args.head.len() {
                    let excess = &presig.base.args.head[args.len()..];
                    let span = excess.iter().fold(Span::dummy(), |span, i| span | i.span);
                    self.error(span, m::ExcessArgsInFuncSpec {}).done()?;
                }
                match (&varargs, &presig.base.args.tail) {
                    (&Some(ref v), &None) => {
                        self.error(v.span, m::MissingVarargsInFuncSpec {}).done()?;
                    }
                    (&None, &Some(ref v)) => {
                        self.error(v.span, m::MissingVarargsInFuncDecl {}).done()?;
                    }
                    (&Some(Spanned { base: Some(_), span }), &Some(ref v)) => {
                        self.error(span, m::DuplicateVarargsSpecInFuncDecl {})
                            .note(v.span, m::PriorVarargsSpecInFuncSpec {})
                            .done()?;
                    }
                    (_, _) => {}
                }
                for (arg, sigarg) in args.iter().zip(presig.base.args.head.iter()) {
                    if arg.0.base != sigarg.base.base.base { // TODO might not be needed
                        self.error(arg.0.span, m::ArgNameMismatchInFuncDecl {})
                            .note(sigarg.base.base.span, m::PriorArgNameInFuncSpec {})
                            .done()?;
                    }
                    if let Some(ref spec) = arg.1 {
                        self.error(spec.span, m::DuplicateSpecInFuncDecl {})
                            .note(presig.span, m::PriorFuncSpec {})
                            .done()?;
                    }
                }
                if let Some(returns) = returns {
                    self.error(returns.span, m::DuplicateReturnSpecInFuncDecl {})
                        .note(presig.span, m::PriorFuncSpec {})
                        .done()?;
                }

                let args = presig.base.args.map(|args| {
                    Seq { head: args.head.into_iter().map(|arg| arg.base).collect(),
                          tail: args.tail.map(|arg| arg.base) }
                });
                (attrs, Ok(args), presig.base.returns)
            } else {
                (attrs, Err(args), returns.map(|ret| ret.base))
            }
        } else {
            (Vec::new(), Err(args), returns.map(|ret| ret.base))
        };

        let args = args.unwrap_or_else(|args| {
            Seq {
                head: args.into_iter().map(|(n,s)| self.make_kailua_typespec(n, s)).collect(),
                tail: varargs.map(|tt| tt.base),
            }.with_loc(begin..end)
        });

        // resolve every parameter (including self)
        let ((selfparam, args), scope, block) = self.parse_block_with_scope(|parser, scope| {
            // attach all arguments to the function body scope
            // XXX should also mention all excess arguments
            // TODO should we add varargs?
            let selfparam = selfparam.map(|paramspec: TypeSpec<Span>| {
                paramspec.map(|span| {
                    let scoped_id =
                        parser.scope_map.add_name(scope, Name::from(b"self"[..].to_owned()));
                    SelfParam(scoped_id).with_loc(span)
                })
            });
            let args = args.map(|args: Seq<_, _>| {
                args.map(|argspec: TypeSpec<_>| {
                    argspec.map(|arg: Spanned<Name>| {
                        arg.map(|name| parser.scope_map.add_name(scope, name))
                    })
                }, |varargspec| varargspec)
            });
            (selfparam, args)
        });
        let block = self.recover(|_| block, Keyword::End)?;

        let sig = Sig { attrs: attrs, args: args, returns: returns };
        Ok(Some((selfparam, sig, scope, block)))
    }

    fn scan_tabular_body<Scan, Item>(&mut self, mut scan: Scan) -> Result<Vec<Item>>
            where Scan: FnMut(&mut Self) -> Result<Item> {
        let mut items = Vec::new();

        self.recover(|parser| {
            while !parser.may_expect(Punct::RBrace) {
                let item = scan(parser)?;
                items.push(item);

                match parser.read() {
                    // `,` [`}`]
                    // `;` [`}`]
                    (_, Spanned { base: Tok::Punct(Punct::Comma), .. }) |
                    (_, Spanned { base: Tok::Punct(Punct::Semicolon), .. }) => {}

                    // `}`
                    (_, Spanned { base: Tok::Punct(Punct::RBrace), .. }) => break,

                    tok => {
                        parser.error(tok.1.span, m::NoTableSep { read: &tok.1.base }).done()?;
                        parser.unread(tok);
                        return Err(Stop::Recover);
                    }
                }
            }
            Ok(())
        }, DelimAlreadyRead)?;

        Ok(items)
    }

    fn parse_table_body(&mut self) -> Result<Vec<(Option<Spanned<Exp>>, Spanned<Exp>)>> {
        self.scan_tabular_body(|parser| {
            parser.recover_upto(|parser| {
                if parser.may_expect(Punct::LBracket) {
                    let key = parser.recover(Self::parse_exp, Punct::RBracket)?;
                    let value = parser.recover_upto(|parser| {
                        parser.expect(Punct::Eq)?;
                        parser.parse_exp()
                    })?;
                    Ok((Some(key), value))
                } else {
                    // try to disambiguate `NAME "=" exp` from `exp`
                    let key = match parser.read() {
                        (side, Spanned { base: Tok::Name(name), span }) => {
                            if parser.may_expect(Punct::Eq) {
                                Some(Box::new(Ex::Str(Str::from(name))).with_loc(span))
                            } else {
                                parser.unread((side, Tok::Name(name).with_loc(span)));
                                None
                            }
                        }
                        tok => {
                            parser.unread(tok);
                            None
                        }
                    };
                    let value = parser.parse_exp()?;
                    Ok((key, value))
                }
            })
        })
    }

    fn try_parse_args(&mut self) -> Result<Option<Spanned<Args>>> {
        let begin = self.pos();
        match self.read() {
            (_, Spanned { base: Tok::Punct(Punct::LParen), .. }) => {
                let mut args = Vec::new();
                let (span, _) = self.recover_with(
                    |p| p.try_scan_explist(|exp| args.push(exp)), Punct::RParen,
                    || (Span::dummy(), false)
                )?;
                Ok(Some(Args::List(args).with_loc(span)))
            }
            (_, Spanned { base: Tok::Str(s), span }) => {
                Ok(Some(Args::Str(s.into()).with_loc(span)))
            }
            (_, Spanned { base: Tok::Punct(Punct::LBrace), .. }) => {
                let fields = self.parse_table_body()?;
                Ok(Some(Args::Table(fields).with_loc(begin..self.last_pos())))
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
                exp = self.recover(Self::parse_exp, Punct::RParen)?;
            }
            (_, Spanned { base: Tok::Name(name), span }) => {
                let nameref = self.resolve_name(Name::from(name));
                exp = Box::new(Ex::Var(nameref.with_loc(span))).with_loc(span);
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
                    if let Some(name) = self.try_parse_name() {
                        exp = Box::new(Ex::IndexName(exp, name)).with_loc(begin..self.last_pos());
                    } else {
                        let tok = self.read();
                        self.error(tok.1.span, m::NoNameAfterExpDot { read: &tok.1.base }).done()?;
                        self.unread(tok);
                        break;
                    }
                }

                // prefixexp "[" ...
                (_, Spanned { base: Tok::Punct(Punct::LBracket), .. }) => {
                    let exp2 = self.recover(Self::parse_exp, Punct::RBracket)?;
                    let span = begin..self.last_pos();
                    exp = Box::new(Ex::Index(exp, exp2)).with_loc(span);
                }

                // prefixexp ":" ...
                (_, Spanned { base: Tok::Punct(Punct::Colon), .. }) => {
                    let name = if let Some(name) = self.try_parse_name() {
                        name
                    } else {
                        let tok = self.read();
                        self.error(tok.1.span, m::NoArgsAfterExpColon { read: &tok.1.base })
                            .done()?;
                        self.unread(tok);
                        break;
                    };
                    let namesend = self.last_pos();

                    if let Some(args) = self.try_parse_args()? {
                        let expname = (exp, name).with_loc(begin..namesend);
                        let span = begin..self.last_pos();
                        exp = Box::new(Ex::MethodCall(expname, args)).with_loc(span);
                    } else {
                        let tok = self.read();
                        self.error(tok.1.span, m::NoArgsAfterExpColonName { read: &tok.1.base })
                            .done()?;
                        self.unread(tok);

                        // convert `prefixexp ":" NAME` into `prefixexp "." NAME`
                        exp = Box::new(Ex::IndexName(exp, name)).with_loc(begin..namesend);
                        break;
                    }
                }

                // prefixexp STR
                // prefixexp "("
                // prefixexp "{"
                tok => {
                    self.unread(tok);
                    if let Some(args) = self.try_parse_args()? {
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

    fn convert_and_register_var_from_exp(&mut self, exp: Spanned<Exp>)
        -> result::Result<Spanned<Var>, Spanned<Exp>>
    {
        let span = exp.span;
        let base = *exp.base;
        match base {
            Ex::Var(name) => {
                // if Var refers to a global variable assignment,
                // register its name to the global scope
                if let NameRef::Global(ref name) = name.base {
                    self.global_scope.insert(name.clone());
                }
                Ok(Var::Name(name).with_loc(span))
            },
            Ex::Index(e1, e2) => {
                Ok(Var::Index(e1, e2).with_loc(span))
            },
            Ex::IndexName(e, name) => {
                Ok(Var::IndexName(e, name).with_loc(span))
            },
            base => {
                Err(Box::new(base).with_loc(span))
            },
        }
    }

    fn try_parse_atomic_exp(&mut self) -> Result<Option<Spanned<Exp>>> {
        let begin = self.pos();

        let funcspec = self.try_parse_kailua_func_spec()?;
        if let Some(ref funcspec) = funcspec {
            if !self.lookahead(Keyword::Function) {
                // limit the possible lookahead
                self.error(funcspec.span, m::MissingFuncLitAfterFuncSpec {}).done()?;
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
                let exp = match self.parse_func_body(None, funcspec)? {
                    Some((_, sig, scope, body)) => Ex::Func(sig, scope, body),
                    None => Ex::Oops,
                };
                Ok(Some(Box::new(exp).with_loc(begin..self.last_pos())))
            }

            (_, Spanned { base: Tok::Punct(Punct::LBrace), .. }) => {
                let table = self.parse_table_body()?;
                Ok(Some(Box::new(Ex::Table(table)).with_loc(begin..self.last_pos())))
            }

            tok => {
                self.unread(tok);
                self.try_parse_prefix_exp()
            }
        }
    }

    // operator-precedence parser for a subset of the Lua expression grammar.
    // more accurately, this is a "precedence climbing" algorithm [1] as used by Lua as well.
    // in short one function call will try to collect subexpressions in the form of...
    //     ((atom op1 subexpr1) op2 subexpr2) ... opN subexprN
    // ...so that operators' precedences are _monotonically_ decreasing and
    // each subexprK only has operators with higher precedence than opK.
    //
    // this is represented by `minprec` argument, where higher precedence assigns
    // a larger number and only operators with precedence > minprec get accepted.
    // we don't need to actually track if the precedences are decreasing;
    // due to prior requirements, every operator in subexprK has lower precedence than opK,
    // thus op(K+1) is the first token with precedence higher than or equal to opK.
    //
    // for right-associative operators precedences should be _strictly_ decreasing instead.
    // to simplify the matter we have a difference precedence for comparison and recursion.
    // while left-associative operators have the same precedence for both,
    // right-associative operators have comparison precedence higher than recusion precedence;
    // if opK is right-associative, subexprK can contain opK and result in a correct tree.
    //
    // for prefix unary operators the above form can be rewritten as follows...
    //     (((opU subexprU) op1 subexpr1) op2 subexpr2) ... opN subexprN
    // ...where the relation between opU and subexprU is same to others.
    // practically, though, Lua allows an unary operator before any atomic expression,
    // so opU itself is not affected by `minprec`.
    //
    // [1] https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing

    fn try_peek_unary_op(&mut self) -> Option<Spanned<UnOp>> {
        let tok = self.peek();
        let op = match tok.base {
            Tok::Punct(Punct::Dash) => Some(UnOp::Neg),
            Tok::Keyword(Keyword::Not) => Some(UnOp::Not),
            Tok::Punct(Punct::Hash) => Some(UnOp::Len),
            _ => None,
        };
        op.map(|op| op.with_loc(tok))
    }

    fn try_peek_binary_op(&mut self) -> Option<Spanned<BinOp>> {
        let tok = self.peek();
        let op = match tok.base {
            Tok::Punct(Punct::Plus) => Some(BinOp::Add),
            Tok::Punct(Punct::Dash) => Some(BinOp::Sub),
            Tok::Punct(Punct::Star) => Some(BinOp::Mul),
            Tok::Punct(Punct::Slash) => Some(BinOp::Div),
            Tok::Punct(Punct::Caret) => Some(BinOp::Pow),
            Tok::Punct(Punct::Percent) => Some(BinOp::Mod),
            Tok::Punct(Punct::DotDot) => Some(BinOp::Cat),
            Tok::Punct(Punct::Lt) => Some(BinOp::Lt),
            Tok::Punct(Punct::LtEq) => Some(BinOp::Le),
            Tok::Punct(Punct::Gt) => Some(BinOp::Gt),
            Tok::Punct(Punct::GtEq) => Some(BinOp::Ge),
            Tok::Punct(Punct::EqEq) => Some(BinOp::Eq),
            Tok::Punct(Punct::TildeEq) => Some(BinOp::Ne),
            Tok::Keyword(Keyword::And) => Some(BinOp::And),
            Tok::Keyword(Keyword::Or) => Some(BinOp::Or),
            _ => None,
        };
        op.map(|op| op.with_loc(tok))
    }

    // the precedence level 0 is reserved and used at the top level
    fn try_parse_partial_exp(&mut self, minprec: u8) -> Result<Option<Spanned<Exp>>> {
        fn unary_prec(op: UnOp) -> /*recursion*/ u8 {
            match op {
                // binary ^ operator here
                UnOp::Neg | UnOp::Not | UnOp::Len => 8,
                // other binary operators here
            }
        }

        fn binary_prec(op: BinOp) -> (/*comparison*/ u8, /*recursion*/ u8) {
            match op {
                BinOp::Pow => (10, 9),
                // unary operators here
                BinOp::Mul | BinOp::Div | BinOp::Mod => (7, 7),
                BinOp::Add | BinOp::Sub => (6, 6),
                BinOp::Cat => (5, 4),
                BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge | BinOp::Eq | BinOp::Ne => (3, 3),
                BinOp::And => (2, 2),
                BinOp::Or => (1, 1),
            }
        }

        trace!("parsing exp with min prec {}", minprec);

        let begin = self.pos();
        let mut exp = if let Some(op) = self.try_peek_unary_op() {
            // unop exp ...
            self.read();
            let rprec = unary_prec(op.base);
            let exp = self.parse_partial_exp(rprec)?;
            Box::new(Ex::Un(op, exp)).with_loc(begin..self.last_pos())
        } else if let Some(exp) = self.try_parse_atomic_exp()? {
            // atomicexp ...
            exp
        } else {
            return Ok(None);
        };

        // (unop exp | atomicexp) {binop <exp with lower prec>}
        while let Some(op) = self.try_peek_binary_op() {
            let (cprec, rprec) = binary_prec(op.base);
            if cprec <= minprec { break; }
            self.read();
            let exp2 = self.parse_partial_exp(rprec)?;
            exp = Box::new(Ex::Bin(exp, op, exp2)).with_loc(begin..self.last_pos());
        }

        Ok(Some(exp))
    }

    fn parse_partial_exp(&mut self, minprec: u8) -> Result<Spanned<Exp>> {
        if let Some(exp) = self.try_parse_partial_exp(minprec)? {
            Ok(exp)
        } else {
            let tok = self.read();
            self.error(tok.1.span, m::NoExp { read: &tok.1.base }).done()?;
            self.unread(tok);
            Ok(Box::new(Ex::Oops).with_loc(self.last_pos()))
        }
    }

    fn try_parse_exp(&mut self) -> Result<Option<Spanned<Exp>>> {
        self.try_parse_partial_exp(0)
    }

    fn parse_exp(&mut self) -> Result<Spanned<Exp>> {
        self.parse_partial_exp(0)
    }

    fn parse_var(&mut self) -> Result<Spanned<Var>> {
        if let Some(exp) = self.try_parse_exp()? {
            match self.convert_and_register_var_from_exp(exp) {
                Ok(var) => Ok(var),
                Err(exp) => {
                    self.error(&exp, m::NoVarButExp {}).done()?;
                    // since it's already a complete expression, it is beneficial to treat it
                    // as a part of incomplete AST---missing indexing key in this case.
                    let expspan = exp.span;
                    Ok(Var::Index(exp, Box::new(Ex::Oops).without_loc()).with_loc(expspan))
                }
            }
        } else {
            let tok = self.read();
            self.error(tok.1.span, m::NoVar { read: &tok.1.base }).done()?;
            self.unread(tok);
            Err(Stop::Recover)
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
        let mut item = scan(self)?;
        let mut spec = self.try_parse_kailua_type_spec()?;
        while self.may_expect(Punct::Comma) {
            // try to read the type spec after a comma if there was no prior spec
            if spec.is_none() { spec = self.try_parse_kailua_type_spec()?; }
            f(self.make_kailua_typespec(item, spec));
            item = scan(self)?;
            spec = self.try_parse_kailua_type_spec()?;
        }
        let end = self.last_pos();
        if self.may_expect(Punct::Eq) {
            if spec.is_none() { spec = self.try_parse_kailua_type_spec()?; }
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
        f(scan(self)?);
        while self.may_expect(Punct::Comma) {
            f(scan(self)?);
        }
        let end = self.last_pos();
        Ok(Span::new(begin, end))
    }

    fn try_scan_explist<F>(&mut self, mut f: F) -> Result<(Span, bool)>
            where F: FnMut(Spanned<Exp>) {
        let begin = self.pos();
        if let Some(exp) = self.try_parse_exp()? {
            f(exp);
            while self.may_expect(Punct::Comma) {
                f(self.parse_exp()?);
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
            let name = self.recover_retry(false, false,
                lastly!(Parser::try_name_or_keyword => Punct::RBracket),
            )?;
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
        let kind = self.parse_kailua_kind()?;
        Ok(SlotKind { modf: modf, kind: kind }.with_loc(begin..self.last_pos()))
    }

    // may contain ellipsis, the caller is expected to error on unwanted cases
    fn parse_kailua_kindlist(&mut self) -> Result<Seq<Spanned<Kind>>> {
        let mut kinds = Vec::new();
        // KIND {"," KIND} ["..."] or empty
        // try to read the first KIND
        let kind = match self.try_parse_kailua_kind()? {
            Some(kind) => kind,
            None => match self.read() {
                (_, Spanned { base: Tok::Punct(Punct::DotDotDot), span }) => {
                    self.error(span, m::NoKindBeforeEllipsis {}).done()?;
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
            let kind = match self.try_parse_kailua_kind()? {
                Some(kind) => kind,
                None => match self.read() {
                    (_, Spanned { base: Tok::Punct(Punct::DotDotDot), span }) => {
                        self.error(span, m::NoKindBeforeEllipsis {}).done()?;
                        // pretend that (an invalid) `(<explist>, ...)` is `(<explist>, ?...)`
                        return Ok(Seq { head: kinds,
                                        tail: Some(Box::new(K::Dynamic).with_loc(span)) });
                    }
                    tok => {
                        self.error(tok.1.span, m::NoKind { read: &tok.1.base }).done()?;
                        self.unread(tok);
                        Box::new(K::Oops).without_loc()
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
            if *name.base == *b"self" {
                // `self` as the first argument can omit its type
                if self.may_expect(Punct::Colon) {
                    modf = self.parse_kailua_mod();
                    kind = Some(self.parse_kailua_kind()?);
                } else {
                    modf = M::None;
                    kind = None;
                }
            } else {
                self.expect(Punct::Colon)?;
                modf = self.parse_kailua_mod();
                kind = Some(self.parse_kailua_kind()?);
            }
            let spec = TypeSpec { base: name, modf: modf, kind: kind };
            specs.push(spec.with_loc(begin..self.last_pos()));

            while self.may_expect(Punct::Comma) {
                let begin = self.pos();
                if self.may_expect(Punct::DotDotDot) {
                    variadic = Some(begin);
                    break;
                }
                let name = self.parse_name()?;
                self.expect(Punct::Colon)?;
                let modf = self.parse_kailua_mod();
                let kind = self.parse_kailua_kind()?;
                let spec = TypeSpec { base: name, modf: modf, kind: Some(kind) };
                specs.push(spec.with_loc(begin..self.last_pos()));
            }
        }

        let tail = if let Some(begin) = variadic {
            // we've already read `...`
            let varargs = if self.may_expect(Punct::Colon) {
                let kind = self.parse_kailua_kind()?;
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
        self.expect(Punct::LParen)?;
        let args = self.parse_kailua_kindlist()?;
        self.expect(Punct::RParen)?;
        let returns = if self.may_expect(Punct::DashDashGt) {
            // "(" ... ")" "-->" ...
            self.parse_kailua_kind_seq()?
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
            let mut kinds = vec![self.parse_kailua_kind_with_spanned_modf()?];
            while self.may_expect(Punct::Comma) {
                kinds.push(self.parse_kailua_kind_with_spanned_modf()?);
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
                    self.error(tok.1.span, m::NoKindParamsClose { read: &tok.1.base }).done()?;
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
        if let Some(params) = self.try_parse_kailua_kind_params()? {
            Ok(params)
        } else {
            let tok = self.read();
            self.error(tok.1.span, m::NoKindParams { read: &tok.1.base }).done()?;
            self.unread(tok);
            Err(Stop::Recover)
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
                    let func = self.parse_kailua_funckind()?;
                    // cannot be followed by postfix operators
                    let kind = Box::new(K::Func(func)).with_loc(begin..self.last_pos());
                    return Ok(Some(AtomicKind::Seq(Seq { head: vec![kind], tail: None })));
                } else {
                    Box::new(K::Function).with_loc(span)
                }
            }

            (_, Spanned { base: Tok::Punct(Punct::LParen), .. }) => {
                let mut args = self.parse_kailua_kindlist()?;
                self.expect(Punct::RParen)?;
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
                        let is_record = if let Tok::Name(_) = tok.1.base {
                            self.lookahead(Punct::Eq)
                        } else {
                            false
                        };
                        self.unread(tok);

                        if is_record {
                            // "{" NAME "=" MODF KIND {"," NAME "=" MODF KIND} "}"
                            let mut seen = HashMap::new(); // value denotes the first span
                            let fields = self.scan_tabular_body(|parser| {
                                let name = parser.parse_name()?;
                                match seen.entry(name.base.clone()) {
                                    hash_map::Entry::Occupied(e) => {
                                        parser.error(name.span,
                                                     m::DuplicateFieldNameInRec
                                                         { name: &name.base })
                                              .note(*e.get(), m::FirstFieldNameInRec {})
                                              .done()?;
                                    }
                                    hash_map::Entry::Vacant(e) => {
                                        e.insert(name.span);
                                    }
                                }
                                let name = Str::from(name.base).with_loc(name.span);
                                parser.expect(Punct::Eq)?;
                                let slotkind = parser.parse_kailua_slotkind()?;
                                Ok((name, slotkind))
                            })?;
                            Box::new(K::Record(fields))
                        } else {
                            // "{" MODF KIND "," [MODF KIND {"," MODF KIND}] "}"
                            let fields = self.scan_tabular_body(|parser| {
                                parser.parse_kailua_slotkind()
                            })?;
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
                let params = self.parse_kailua_kind_params()?;
                if params.len() != 1 {
                    if !params.is_empty() {
                        self.error(&params, m::WrongVectorParamsArity {}).done()?;
                    }
                    Recover::recover()
                } else {
                    let mut it = params.base.into_iter();
                    let (vm, v) = it.next().unwrap();
                    let vspan = vm.span | v.span;
                    let v = SlotKind { modf: vm.base, kind: v }.with_loc(vspan);
                    Box::new(K::Array(v)).with_loc(begin..self.last_pos())
                }
            }
            (_, Spanned { base: Tok::Keyword(Keyword::Map), .. }) => {
                let params = self.parse_kailua_kind_params()?;
                if params.len() != 2 {
                    if !params.is_empty() {
                        self.error(&params, m::WrongMapParamsArity {}).done()?;
                    }
                    Recover::recover()
                } else {
                    let mut it = params.base.into_iter();
                    let (km, k) = it.next().unwrap();
                    if km.base != M::None {
                        self.error(&km, m::WrongMapParamsModf {}).done()?;
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
                            self.error(span, m::ReservedKindName { name: &name }).done()?;
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
        if let Some(kindseq) = self.try_parse_kailua_atomic_kind_seq()? {
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
                            self.error(begin..end, m::AttrToKindSeq {}).done()?;
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
        match self.try_parse_kailua_prefixed_kind_seq()? {
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
                        match self.try_parse_kailua_prefixed_kind_seq()? {
                            Some(AtomicKind::One(kind2)) => {
                                kinds.push(kind2);
                            }
                            Some(AtomicKind::Seq(..)) => {
                                self.error(begin..self.last_pos(), m::NoTypeSeqInUnion {})
                                    .done()?;
                                kinds.push(Recover::recover());
                            }
                            None => {
                                let tok = self.read();
                                self.error(tok.1.span, m::NoType { read: &tok.1.base }).done()?;
                                break;
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
        if let Some(mut kindseq) = self.try_parse_kailua_kind_seq()? {
            if kindseq.base.head.len() == 1 && kindseq.base.tail.is_none() {
                let first = kindseq.base.head.pop().unwrap();
                let span = kindseq.span | first.span; // overwrite the span
                Ok(Some(first.base.with_loc(span)))
            } else {
                self.error(kindseq.span, m::NoSingleTypeButTypeSeq {}).done()?;
                Ok(Some(Recover::recover()))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_kailua_kind_seq(&mut self) -> Result<Seq<Spanned<Kind>>> {
        if let Some(kindseq) = self.try_parse_kailua_kind_seq()? {
            Ok(kindseq.base)
        } else {
            let tok = self.read().1;
            self.error(tok.span, m::NoTypeOrTypeSeq { read: &tok.base }).done()?;
            Ok(Seq { head: vec![Recover::recover()], tail: None })
        }
    }

    fn parse_kailua_kind(&mut self) -> Result<Spanned<Kind>> {
        if let Some(kind) = self.try_parse_kailua_kind()? {
            Ok(kind)
        } else {
            let tok = self.read();
            self.error(tok.1.span, m::NoSingleType { read: &tok.1.base }).done()?;
            self.unread(tok);
            Err(Stop::Recover)
        }
    }

    fn parse_kailua_kind_with_spanned_modf(&mut self) -> Result<(Spanned<M>, Spanned<Kind>)> {
        let begin = self.pos();
        let modf = match self.parse_kailua_mod() {
            M::None => M::None.with_loc(begin), // i.e. the beginning of the kind
            modf => modf.with_loc(begin..self.last_pos()),
        };
        let kind = self.recover_upto(Self::parse_kailua_kind)?;
        Ok((modf, kind))
    }

    fn try_parse_kailua_type_spec_with_spanned_modf(&mut self)
            -> Result<Option<Spanned<(Spanned<M>, Spanned<Kind>)>>> {
        let metabegin = self.pos();
        if self.may_expect(Punct::DashDashColon) {
            self.recover_meta(|parser| {
                // allow for `--: { a = foo,
                //            --:   b = bar }`
                parser.begin_meta_comment(Punct::DashDashColon);
                let spec = parser.parse_kailua_kind_with_spanned_modf()?;
                let metaend = parser.last_pos();
                // allow for `--: last type --> return type` in the function decl
                if !parser.lookahead(Punct::DashDashGt) {
                    parser.end_meta_comment(Punct::DashDashColon)?;
                } else {
                    assert_eq!(parser.ignore_after_newline, Some(Punct::DashDashColon));
                    parser.ignore_after_newline = None;
                    // it is possible to have `--: { a = foo,
                    //                         --:   b = bar }
                    //                         --: --> return_type`. seems harmless though.
                    parser.elided_newline = None;
                }
                Ok(Some(spec.with_loc(metabegin..metaend)))
            }, Recover::recover)
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_type_spec(&mut self) -> Result<Option<Spanned<(M, Spanned<Kind>)>>> {
        trace!("parsing kailua type spec");
        let spec = self.try_parse_kailua_type_spec_with_spanned_modf()?;
        Ok(spec.map(|spec| spec.map(|(m,k)| (m.base, k))))
    }

    fn try_parse_kailua_typeseq_spec(&mut self)
            -> Result<Option<Spanned<Vec<Spanned<(M, Spanned<Kind>)>>>>> {
        trace!("parsing kailua type sequence spec");
        let metabegin = self.pos();
        if self.may_expect(Punct::DashDashColon) {
            self.recover_meta(|parser| {
                parser.begin_meta_comment(Punct::DashDashColon);

                let mut specs = Vec::new();
                parser.scan_list(|parser| {
                    let begin = parser.pos();
                    let spec = parser.parse_kailua_kind_with_spanned_modf()?;
                    Ok(spec.with_loc(begin..parser.last_pos()))
                }, |spec| {
                    specs.push(spec.map(|(m,k)| (m.base, k)));
                })?;

                let metaend = parser.last_pos();
                if !parser.lookahead(Punct::DashDashGt) {
                    parser.end_meta_comment(Punct::DashDashColon)?;
                } else {
                    assert_eq!(parser.ignore_after_newline, Some(Punct::DashDashColon));
                    parser.ignore_after_newline = None;
                    parser.elided_newline = None;
                }
                Ok(Some(specs.with_loc(metabegin..metaend)))
            }, Recover::recover)
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_rettype_spec(&mut self)
            -> Result<Option<Spanned<Seq<Spanned<Kind>>>>> {
        trace!("parsing kailua return type spec");
        let begin = self.pos();
        if self.may_expect(Punct::DashDashGt) {
            self.recover_meta(|parser| {
                parser.begin_meta_comment(Punct::DashDashGt);
                let kinds = parser.parse_kailua_kind_seq()?;
                let end = parser.last_pos();
                parser.end_meta_comment(Punct::DashDashGt)?;

                Ok(Some(kinds.with_loc(begin..end)))
            }, Recover::recover)
        } else {
            Ok(None)
        }
    }

    // it may supply only attributes, so we need to return attributes and pre-signatures separately
    fn try_parse_kailua_func_spec(&mut self)
            -> Result<Option<Spanned<(Vec<Spanned<Attr>>, Option<Spanned<Presig>>)>>> {
        trace!("parsing kailua function spec");
        let metabegin = self.pos();

        if self.may_expect(Punct::DashDashV) {
            self.recover_meta(|parser| {
                parser.begin_meta_comment(Punct::DashDashV);

                let mut attrs = Vec::new();
                let mut attrs_seen = false;
                loop {
                    match parser.try_parse_kailua_attr() {
                        Ok(Some(attr)) => { attrs_seen = true; attrs.push(attr); }
                        Ok(None) => break,
                        Err(Stop::Fatal) => return Err(Stop::Fatal),
                        // we have seen an attribute but couldn't parse it
                        Err(Stop::Recover) => { attrs_seen = true; }
                    }
                }

                // function "(" [NAME ":" KIND] {"," NAME ":" KIND} ["," "..."] ")" ["-->" KIND]
                let begin = parser.pos();
                let has_sig = if !attrs_seen {
                    // force reading signatures if no attributes are present
                    parser.expect(Keyword::Function)?;
                    true
                } else {
                    parser.may_expect(Keyword::Function)
                };
                let sig = if has_sig {
                    parser.expect(Punct::LParen)?;
                    let args = parser.parse_kailua_namekindlist()?;
                    parser.expect(Punct::RParen)?;
                    let returns = if parser.may_expect(Punct::DashDashGt) {
                        parser.parse_kailua_kind_seq()?
                    } else {
                        Seq { head: Vec::new(), tail: None }
                    };
                    let end = parser.last_pos();
                    Some(Presig { args: args, returns: Some(returns) }.with_loc(begin..end))
                } else {
                    None
                };

                let metaend = parser.last_pos();
                parser.end_meta_comment(Punct::DashDashV)?;
                Ok(Some((attrs, sig).with_loc(metabegin..metaend)))
            }, Recover::recover)
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_spec(&mut self) -> Result<Option<Option<Spanned<Stmt>>>> {
        trace!("parsing kailua spec");
        let begin = self.pos();

        if self.may_expect(Punct::DashDashHash) {
            let (stmt, end) = self.recover_meta(|parser| {
                parser.begin_meta_comment(Punct::DashDashHash);

                let mut sibling_scope = None;
                let stmt = match parser.read() {
                    // assume [global] NAME ":" MODF KIND
                    (_, Spanned { base: Tok::Keyword(Keyword::Assume), .. }) => {
                        let global = parser.may_expect(Keyword::Global);
                        let name = parser.parse_name()?;
                        parser.expect(Punct::Colon)?;
                        let modf = parser.parse_kailua_mod();
                        let kind = parser.recover_upto(Self::parse_kailua_kind)?;

                        let name = if global {
                            parser.global_scope.insert(name.base.clone());
                            name.map(NameRef::Global)
                        } else {
                            let scope = parser.generate_sibling_scope();
                            sibling_scope = Some(scope);
                            name.map(|name| NameRef::Local(parser.scope_map.add_name(scope, name)))
                        };
                        Some(Box::new(St::KailuaAssume(name, modf, kind, sibling_scope)))
                    }

                    // open NAME
                    (_, Spanned { base: Tok::Keyword(Keyword::Open), .. }) => {
                        let name = parser.parse_name()?;
                        // TODO also set the parser option
                        Some(Box::new(St::KailuaOpen(name)))
                    }

                    // type NAME = KIND
                    (_, Spanned { base: Tok::Keyword(Keyword::Type), .. }) => {
                        let name = parser.parse_name()?;
                        parser.expect(Punct::Eq)?;
                        let kind = parser.recover_upto(Self::parse_kailua_kind)?;

                        // forbid overriding builtin types
                        if parser.builtin_kind(&*name.base).is_some() {
                            parser.error(name.span, m::CannotRedefineBuiltin {}).done()?;
                        }

                        Some(Box::new(St::KailuaType(name, kind)))
                    }

                    tok => {
                        parser.unread(tok);
                        None // empty `--#` is valid
                    }
                };

                let end = parser.last_pos();
                { // the new sibling scope should happen after the meta block
                    let ret = parser.end_meta_comment(Punct::DashDashHash);
                    if let Some(scope) = sibling_scope {
                        parser.push_scope(scope);
                    }
                    ret?;
                }
                Ok((stmt, Some(end)))
            }, || (Some(Box::new(St::Oops)), None))?;

            let end = end.unwrap_or_else(|| self.last_pos());
            Ok(Some(stmt.map(|st| st.with_loc(begin..end))))
        } else {
            Ok(None)
        }
    }

    pub fn into_chunk(mut self) -> diag::Result<Chunk> {
        self.scope_stack = vec![];
        let ret = self.parse_block_until_eof();

        // any remaining scope is considered to end at the last token read
        // (unlike normal cases of `pop_scope_upto`, as this might be past EOF)
        let end = self.last_pos();
        while let Some((scope, scopebegin)) = self.scope_stack.pop() {
            self.scope_map.set_span(scope.with_loc(scopebegin..end));
        }

        if let Ok(block) = ret {
            Ok(Chunk { block: block, global_scope: self.global_scope, map: self.scope_map })
        } else {
            Err(diag::Stop)
        }
    }
}

