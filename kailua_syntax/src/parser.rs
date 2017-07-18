use std::ops;
use std::iter;
use std::u8;
use std::i32;
use std::usize;
use std::fmt;
use std::result;
use std::collections::{hash_map, HashMap};
use kailua_env::{Pos, Span, Spanned, WithLoc, Scope, ScopedId, ScopeMap};
use kailua_diag::{report, Locale, Report, Reporter, Localize};

use message as m;
use lang::{Language, Lua, Kailua};
use lex::{Tok, Punct, Keyword, NestedToken, NestingCategory, NestingSerial};
use string::{Str, Name};
use ast::{NameRef, RenameRef, Var, Seq, Sig, Attr, AttrValue, Args, Table};
use ast::{Ex, Exp, UnOp, BinOp, SelfParam, TypeScope, St, Stmt, Block};
use ast::{M, MM, K, Kind, SlotKind, FuncKind, TypeSpec, Varargs, Returns};
use ast::{LocalName, LocalNameKind, TokenAux, Chunk};

/// The parser.
pub struct Parser<'a> {
    iter: iter::Fuse<&'a mut Iterator<Item=NestedToken>>,
    language: Language,

    // the lookahead stream (in this order)
    elided_newline: Option<ElidedTokens>,
    lookahead: Option<(usize, NestedToken)>,
    lookahead2: Option<(usize, NestedToken)>,
    // ...follows self.iter.next()

    // token indices and spans for the most recent `read()` tokens
    last_idx: usize,
    last_idx2: usize,
    last_span: Span,
    last_span2: Span,

    last_nesting_depth: u16,
    last_nesting_serial: NestingSerial,

    ignore_after_newline: Option<Punct>,
    report: &'a Report,

    scope_map: ScopeMap<Name>,
    local_names: HashMap<ScopedId, LocalName>,

    // the global scope; visible to every other file and does not go through a scope map
    // (the local root scopes are generated as needed, and invisible from the outside)
    global_scope: HashMap<Name, Span>,

    // Pos for the starting position
    // this is different from blocks, sibling scopes can be nested within one block
    scope_stack: Vec<(Scope, Pos)>,
    block_depth: usize,

    // auxiliary info for each *input* token (i.e. including elided tokens)
    token_aux: Vec<TokenAux>,
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

impl From<report::Stop> for Stop {
    fn from(_: report::Stop) -> Stop { Stop::Fatal }
}

// parser keeps the internal information to recover as much as possible
type Result<T> = result::Result<T, Stop>;

impl Localize for EOF {
    fn fmt_localized(&self, f: &mut fmt::Formatter, locale: Locale) -> fmt::Result {
        match &locale[..] {
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

#[derive(Copy, Clone, Debug)]
struct FixedName(&'static str);

impl Localize for FixedName {
    fn fmt_localized(&self, f: &mut fmt::Formatter, _locale: Locale) -> fmt::Result {
        write!(f, "`{}`", self.0)
    }
}

impl Expectable for FixedName {
    fn check_token(&self, tok: &Tok) -> bool {
        if let Tok::Name(ref name) = *tok { **name == *self.0.as_bytes() } else { false } 
    }
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

impl Recover for MM {
    fn recover() -> Self { MM::None }
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

impl<T: Recover> Recover for Option<T> {
    fn recover() -> Self { Some(T::recover()) }
}

impl<T> Recover for Vec<T> {
    fn recover() -> Self { Vec::new() }
}

impl<T: Recover> Recover for Spanned<T> {
    fn recover() -> Self { T::recover().without_loc() }
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
    elided_tokens: Option<ElidedTokens>,

    // the prior nesting information before reading the token
    last_nesting_serial: NestingSerial,
    last_nesting_depth: u16,

    // a side information provided from NestedToken
    nesting_depth: u16,
    nesting_category: NestingCategory,
    nesting_serial: NestingSerial,
}

#[derive(Copy, Clone, Debug)]
struct ElidedTokens {
    // both refer for the last meta token elided
    last_meta_idx: usize,
    last_meta_span: Span,
}

#[derive(Copy, Clone, Debug, PartialEq)]
struct TokenIdx(usize);

#[derive(Clone, Debug)]
struct IndexedName {
    idx: TokenIdx,
    name: Name,
}

impl ops::Deref for IndexedName {
    type Target = Name;
    fn deref(&self) -> &Name { &self.name }
}

impl PartialEq for IndexedName {
    fn eq(&self, other: &IndexedName) -> bool { self.name == other.name }
}

impl Eq for IndexedName {
}

// more spanned version of Sig, only used during the parsing
#[derive(Clone, PartialEq)]
struct Presig {
    prefix: Spanned<bool>, // true for `method`, false for `function` (span included)
    args: Spanned<Seq<Spanned<TypeSpec<Spanned<IndexedName>>>, Spanned<Option<Spanned<Kind>>>>>,
    returns: Option<Returns>,
}

enum AtomicKind { One(Spanned<Kind>), Seq(Seq<Spanned<Kind>>) }

impl<'a> Report for Parser<'a> {
    fn message_locale(&self) -> Locale {
        self.report.message_locale()
    }

    fn add_span(&self, k: report::Kind, s: Span, m: &Localize) -> report::Result<()> {
        self.report.add_span(k, s, m)
    }
}

// wrappers around kailua_diag::report::{ReportMore, Reporter}, used to remap `done` method
#[must_use]
struct ReportMore<'a, T>(report::ReportMore<'a, T>);

#[allow(dead_code)]
impl<'a> Parser<'a> {
    fn fatal<Loc: Into<Span>, Msg: Localize, T>(&self, loc: Loc, msg: Msg) -> ReportMore<T> {
        ReportMore(Reporter::fatal(self, loc, msg))
    }

    fn error<Loc: Into<Span>, Msg: Localize>(&self, loc: Loc, msg: Msg) -> ReportMore<()> {
        ReportMore(Reporter::error(self, loc, msg))
    }

    fn warn<Loc: Into<Span>, Msg: Localize>(&self, loc: Loc, msg: Msg) -> ReportMore<()> {
        ReportMore(Reporter::warn(self, loc, msg))
    }

    fn info<Loc: Into<Span>, Msg: Localize>(&self, loc: Loc, msg: Msg) -> ReportMore<()> {
        ReportMore(Reporter::info(self, loc, msg))
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

// simplify the common case of errors on the next lookahead (possibly read temporarily)
macro_rules! error_with {
    ($slf:expr, $tok:expr, m::$msg:ident $({ $($k:ident: $v:expr),* $(,)* })*) => ({
        let tok = $tok;
        $slf.error(tok.1.span, m::$msg { $($($k: $v,)*)* read: &tok.1.base }).done()?;
        $slf.unread(tok);
    });

    ($slf:expr, m::$msg:ident $({ $($k:ident: $v:expr),* $(,)* })*) => (
        error_with!($slf, $slf.read(), m::$msg $({ $($k: $v),* })*)
    );
}

// it is very common to read a token, match against patterns and unread it only when unmatched.
// since this is very prone to error (e.g. forgetting to unread can easily cause EOF errors),
// we automate the entire process with a macro.
macro_rules! match_next {
    (
        $slf:expr;
        // this set of arms can consume a read token (and optionally retrieve a span).
        // the unreading information (the first of the tuple) is discarded.
        $($(Tok::$ct:ident$(($($cp:pat),*))*)|+ $(in $csp:ident)* => $ce:expr;)*
        // this set of arms can't consume a read token, but it can match against that.
        // unreading information (the first of the tuple) is invisible and given to `unread` as is.
        // if the message is present, the error is reported with that message before unreading.
        // the body is run after unreading; it has no access to the matched component.
        $('unread: $($up:pat)|+ $(, m::$um:ident)* => $ue:expr);* $(;)*
    ) => (
        match $slf.read() {
            $(
                $((_, Spanned { base: Tok::$ct$(($($cp),*))*, span: _span }))|+ => {
                    $(let $csp = _span;)*
                    $ce
                },
            )*
            $(
                $(tok @ (_, Spanned { base: $up, .. }))|+ => {
                    $($slf.error(tok.1.span, m::$um { read: &tok.1.base }).done()?;)*
                    $slf.unread(tok);
                    $ue
                },
            )*
        }
    )
}

impl<'a> Parser<'a> {
    /// Creates a new nesting analyzer with given stream of spanned tokens
    /// with nesting informations and the report receiver.
    pub fn new(iter: &'a mut Iterator<Item=NestedToken>, report: &'a Report) -> Parser<'a> {
        let mut parser = Parser {
            iter: iter.fuse(),
            language: Language::new(Lua::Lua51, Kailua::Kailua10), // XXX should be configurable
            elided_newline: None,
            lookahead: None,
            lookahead2: None,
            last_idx: usize::MAX, // triggers a panic whenever it is used
            last_idx2: usize::MAX,
            last_span: Span::dummy(),
            last_span2: Span::dummy(),
            last_nesting_depth: 0,
            last_nesting_serial: NestingSerial::dummy(),
            ignore_after_newline: None,
            report: report,
            scope_map: ScopeMap::new(),
            local_names: HashMap::new(),
            global_scope: HashMap::new(),
            scope_stack: Vec::new(),
            block_depth: 0,
            token_aux: Vec::new(),
        };

        // read the first token and fill the last_span
        let first = parser._next().expect("lexer gave no token");
        parser.last_idx = first.0;
        parser.last_span = first.1.tok.span.begin().into();
        parser.lookahead = Some(first);
        parser
    }

    fn _next(&mut self) -> Option<(usize, NestedToken)> {
        loop {
            let next = self.iter.next();

            let next = if let Some(mut t) = next {
                trace!("got {:?}", t);
                if false { // useful for debugging
                    let _ = self.info(t.tok.span, format!("got {:?}", t.tok.base)).done();
                }

                // add a room for the auxiliary info
                let token_idx = self.token_aux.len();
                self.token_aux.push(TokenAux::None);

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
                        t.tok.base = Tok::Name(kw.into());
                    }
                }

                Some((token_idx, t))
            } else {
                None
            };

            return next;
        }
    }

    fn _read(&mut self) -> (Option<ElidedTokens>, (usize, NestedToken)) {
        let mut next = self.lookahead.take().or_else(|| self.lookahead2.take())
                                            .or_else(|| self._next());

        let mut elided = self.elided_newline.take();
        if let Some(meta) = self.ignore_after_newline {
            // lookahead2 can definitely be made empty, let's simplify the assumption
            assert_eq!(self.lookahead, None);
            self.lookahead = self.lookahead2.take();

            while next.as_ref().map(|t| &t.1.tok.base) == Some(&Tok::Punct(Punct::Newline)) {
                let next2 = self.lookahead.take().or_else(|| self._next());
                if next2.as_ref().map(|t| &t.1.tok.base) == Some(&Tok::Punct(meta)) {
                    // we can ignore them, but we may have another ignorable tokens there
                    let next2 = next2.unwrap();
                    elided = Some(ElidedTokens {
                        last_meta_idx: next2.0,
                        last_meta_span: next2.1.tok.span,
                    });
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
            assert!(elided.is_none());
        }

        (elided, next.expect("Parser::read tried to read past EOF"))
    }

    fn read(&mut self) -> (Side, Spanned<Tok>) {
        let (elided, (token_idx, next)) = self._read();
        let side = Side {
            elided_tokens: elided,
            last_nesting_serial: self.last_nesting_serial,
            last_nesting_depth: self.last_nesting_depth,
            nesting_depth: next.depth,
            nesting_category: next.category,
            nesting_serial: next.serial,
        };

        self.last_idx2 = self.last_idx;
        self.last_idx = token_idx;
        self.last_span2 = self.last_span;
        self.last_span = next.tok.span;
        self.last_nesting_depth = next.depth;
        self.last_nesting_serial = next.serial;

        (side, next.tok)
    }

    fn _unread(&mut self, elided: Option<ElidedTokens>, tok: (usize, NestedToken)) {
        assert!(self.lookahead.is_none() || self.lookahead2.is_none(),
                "at most two lookahead tokens are supported:\n\
                 \tunread: {:?}\n\telided: {:?}\n\tlookahead: {:?}\n\tlookahead2: {:?}",
                tok, elided, self.lookahead, self.lookahead2);

        // this overwrites the prior value, i.e. newlines elided between the first lookahead
        // and the second lookahead will be ignored (fine as long as we don't use them).
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
        let idx = self.last_idx;
        self._unread(side.elided_tokens, (idx, tok));

        // if we can, reconstruct the last span from the last elided token
        if let Some(elided) = side.elided_tokens {
            self.last_span = elided.last_meta_span;
        } else {
            self.last_span = self.last_span2;
            self.last_span2 = Span::dummy();
        }
        self.last_idx = self.last_idx2;
        self.last_idx2 = usize::MAX;
        self.last_nesting_depth = side.last_nesting_depth;
        self.last_nesting_serial = side.last_nesting_serial;
    }

    fn peek<'b>(&'b mut self) -> &'b Spanned<Tok> {
        if self.lookahead.is_none() {
            // keep the side information as much as possible
            let (elided, tok) = self._read();
            self._unread(elided, tok);
        }
        &self.lookahead.as_ref().unwrap().1.tok
    }

    fn expect<Tok: Expectable>(&mut self, tok: Tok) -> Result<()> {
        let read = self.read();
        if !tok.check_token(&read.1.base) {
            error_with!(self, read, m::ExpectFailed { expected: tok });
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
            //
            // in addition, recovery never skips the EOF; while most premature EOFs are
            // handled by the former rule, in which case that reading EOF would close
            // at least two nestings (the initial nesting, the top-level nesting),
            // there are some cases that the recovery runs at the topmost nesting.
            //
            // on the other hands, a different minimal depth and current depth indicates
            // that a new nesting is introduced by this token and that token should be kept.
            // (this is just a convention, the other choice is possible)
            let excessive_closing = self.last_nesting_depth < init_depth - 1;
            let eof = if let Tok::EOF = last_tok.1.base { true } else { false };
            if eof || always_unread || excessive_closing || same_depth_update {
                trace!("unreading the final token {}",
                       if eof { "because it's EOF" }
                       else if always_unread { "at a request" }
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

    fn last_token_idx(&self) -> TokenIdx {
        assert!(self.last_idx != usize::MAX, "Parser::last_token_idx has lost prior token index");
        TokenIdx(self.last_idx)
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

            if let Some(elided) = elided {
                // newline (implicitly consumed) - meta - original lookahead
                // meta will receive the same nesting information as the current token
                assert_eq!(self.lookahead2, None);
                self.lookahead2 = self.lookahead.take();
                self.lookahead = Some((elided.last_meta_idx, NestedToken {
                    tok: Tok::Punct(meta).with_loc(elided.last_meta_span),
                    depth: self.last_nesting_depth,
                    category: NestingCategory::Meta,
                    serial: self.last_nesting_serial,
                }));
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

    // skip one token at the current nesting, and also tokens at the inner nestings if any.
    // for example, `(3, 4, 5)` or `--: string <newline>` will be consumed at once,
    // while only the first token of `abc def` or `3/4` will be consumed.
    // used for last-resort error recovery.
    fn skip_token_at_current_nesting(&mut self) {
        let init_nesting_depth = self.last_nesting_depth;
        self.read(); // at least one token is always consumed
        if init_nesting_depth < self.last_nesting_depth {
            // if the last token introduced a new nesting, read until that nesting ends
            self.recover_to_close();
        }
    }

    // Some(Some(k)) for concrete kinds, Some(None) for generic kinds
    fn builtin_kind(&self, name: &[u8]) -> Option<Option<K>> {
        match name {
            b"WHATEVER"          => Some(Some(K::Dynamic)),
            b"any"               => Some(Some(K::Any)),
            b"boolean" | b"bool" => Some(Some(K::Boolean)),
            b"number"            => Some(Some(K::Number)),
            b"integer" | b"int"  => Some(Some(K::Integer)),
            b"string"            => Some(Some(K::String)),
            b"table"             => Some(Some(K::Table)),
            b"function"          => Some(Some(K::Function)), // allow for quoted `function` too
            b"thread"            => Some(Some(K::Thread)),
            b"userdata"          => Some(Some(K::UserData)),
            b"vector"            => Some(None),
            b"map"               => Some(None),
            _ => None,
        }
    }

    fn indexed_name_from(&self, name: Name, span: Span) -> Spanned<IndexedName> {
        IndexedName { idx: self.last_token_idx(), name: name }.with_loc(span)
    }

    fn resolve_local_name_without_idx(&mut self, name: &Name) -> Option<ScopedId> {
        self.scope_stack.last().and_then(|&(scope, _)| {
            self.scope_map.find_name_in_scope(scope, name).map(|(_, scoped_id)| scoped_id)
        })
    }

    // resolve the name referenced in the current scope (might be global) into a reference
    fn resolve_name(&mut self, name: IndexedName) -> NameRef {
        if let Some(scoped_id) = self.resolve_local_name_without_idx(&name.name) {
            self.set_token_aux(name.idx, TokenAux::LocalVarName(scoped_id.clone()));
            NameRef::Local(scoped_id)
        } else {
            self.set_token_aux(name.idx, TokenAux::GlobalVarName);
            NameRef::Global(name.name)
        }
    }

    fn add_spanned_local_name_with_prev_span(&mut self, scope: Scope, name: Spanned<Name>,
                                             kind: LocalNameKind)
        -> Result<(Spanned<ScopedId>, Option<Span>)>
    {
        let id = name.map(|name| self.scope_map.add_name(scope, name));
        let localname = LocalName { def_span: id.span, kind: kind };
        let prevdef = self.local_names.insert(id.base.clone(), localname);
        Ok((id, prevdef.map(|def| def.def_span)))
    }

    fn add_spanned_local_name_without_idx(&mut self, scope: Scope, name: Spanned<Name>,
                                          kind: LocalNameKind) -> Result<Spanned<ScopedId>> {
        let (id, prevspan) = self.add_spanned_local_name_with_prev_span(scope, name, kind)?;
        if let Some(prevspan) = prevspan {
            self.error(id.span, m::DuplicateNameInSameScope {})
                .note(prevspan, m::PreviousNameInSameScope {})
                .done()?;
            // and we proceed with the overwritten name
        }
        Ok(id)
    }

    fn add_spanned_local_name_with_kind(&mut self, scope: Scope, name: Spanned<IndexedName>,
                                        kind: LocalNameKind) -> Result<Spanned<ScopedId>> {
        let idx = name.idx;
        let id = self.add_spanned_local_name_without_idx(scope, name.map(|n| n.name), kind)?;
        self.set_token_aux(idx, TokenAux::LocalVarName(id.base.clone()));
        Ok(id)
    }

    fn add_spanned_local_name(&mut self, scope: Scope,
                              name: Spanned<IndexedName>) -> Result<Spanned<ScopedId>> {
        self.add_spanned_local_name_with_kind(scope, name, LocalNameKind::User)
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

    fn set_token_aux(&mut self, TokenIdx(idx): TokenIdx, aux: TokenAux) {
        self.token_aux[idx] = aux;
    }

    fn try_parse_name(&mut self) -> Option<Spanned<IndexedName>> {
        match_next! { self;
            Tok::Name(name) in span => Some(self.indexed_name_from(name, span));
            'unread: _ => None;
        }
    }

    fn parse_name(&mut self) -> Result<Spanned<IndexedName>> {
        match_next! { self;
            Tok::Name(name) in span => Ok(self.indexed_name_from(name, span));
            'unread: _, m::NoName => Err(Stop::Recover);
        }
    }

    fn try_name_or_keyword(&mut self) -> Result<Spanned<Name>> {
        match_next! { self;
            Tok::Name(name) in span => Ok(name.with_loc(span));
            Tok::Keyword(keyword) in span => Ok(Name::from(keyword).with_loc(span));
            'unread: _, m::NoName => Err(Stop::Recover);
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
        self.block_depth += 1;
        let block = self._parse_block();
        self.block_depth -= 1;
        self.pop_scope_upto(nscopes);
        block
    }

    // same to `self.parse_block()` followed by `self.expect(Keyword::End)`,
    // but will also set the scope in the block and recover from the error.
    // in order to keep the lexical order of scoped ids (purely for cosmetic & debugging reasons),
    // the block can be supplied to run on that scope before `parse_block`.
    fn parse_block_end_with_scope<F, X>(&mut self,
                                        preblock: F) -> Result<(X, Scope, Spanned<Block>)>
        where F: FnOnce(&mut Parser<'a>, Scope) -> Result<X>
    {
        let nscopes = self.scope_stack.len();
        let scope = self.generate_sibling_scope();
        self.push_scope(scope);
        let preret = preblock(self, scope)?;
        self.block_depth += 1;
        let block = self._parse_block();
        self.block_depth -= 1;
        self.pop_scope_upto(nscopes);
        let block = self.recover(|_| block, Keyword::End)?;
        Ok((preret, scope, block))
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
                    error_with!(self, m::NoStmt);
                    self.skip_token_at_current_nesting();
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

    fn update_type_specs_with_typeseq_spec<Item>(
        &self,
        oldspecs: Spanned<Vec<TypeSpec<Spanned<Item>>>>,
        specs: Spanned<Vec<Spanned<(MM, Option<Spanned<Kind>>)>>>,
        note_on_dup: &Localize,
        note_on_less: &Localize,
        note_on_more: &Localize,
    ) -> Result<Spanned<Vec<TypeSpec<Spanned<Item>>>>> {
        if oldspecs.iter().any(|oldspec| oldspec.modf != MM::None ||
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

        let stmt = match_next! { self;
            Tok::Keyword(Keyword::Do) => {
                let block = self.recover(Self::parse_block, Keyword::End)?;
                Box::new(St::Do(block))
            };

            Tok::Keyword(Keyword::While) => {
                let cond = self.recover(Self::parse_exp, Keyword::Do)?;
                let block = self.recover(Self::parse_block, Keyword::End)?;
                Box::new(St::While(cond, block))
            };

            Tok::Keyword(Keyword::Repeat) => {
                let block = self.recover(Self::parse_block, Keyword::Until)?;
                let cond = self.parse_exp()?;
                Box::new(St::Repeat(block, cond))
            };

            Tok::Keyword(Keyword::If) => {
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
            };

            Tok::Keyword(Keyword::For) => {
                let name = self.parse_name()?;
                match_next! { self;
                    // for NAME "=" ...
                    Tok::Punct(Punct::Eq) => {
                        let start = self.parse_exp()?;
                        self.expect(Punct::Comma)?;
                        let end = self.parse_exp()?;
                        let step = if self.may_expect(Punct::Comma) {
                            Some(self.parse_exp()?)
                        } else {
                            None
                        };
                        self.expect(Keyword::Do)?;
                        let (id, scope, block) = self.parse_block_end_with_scope(|parser, scope| {
                            parser.add_spanned_local_name(scope, name)
                        })?;
                        Box::new(St::For(id, start, end, step, scope, block))
                    };

                    // for NAME in ...
                    Tok::Keyword(Keyword::In) => {
                        let span = name.span;
                        self.parse_stmt_for_in(vec![name].with_loc(span))?
                    };

                    // for NAME [SPEC] "," ... in ...
                    Tok::Punct(Punct::Comma) => {
                        let mut vars = vec![name];
                        let span = self.scan_list(Self::parse_name, |name| vars.push(name))?;
                        self.expect(Keyword::In)?;
                        self.parse_stmt_for_in(vars.with_loc(span))?
                    };

                    'unread: _, m::NoForInSep => {
                        self.recover_to_close();
                        Recover::recover()
                    };
                }
            };

            Tok::Keyword(Keyword::Function) => {
                let namesbegin = self.pos();
                let rootname = self.parse_name()?.map(|name| self.resolve_name(name));
                if let NameRef::Global(ref name) = rootname.base {
                    // this will assign a global name, and is handled like a global assignment
                    self.global_scope.entry(name.clone()).or_insert(rootname.span);
                }
                let mut names = Vec::new();
                while self.may_expect(Punct::Dot) {
                    let name = self.parse_name()?;
                    names.push(name.map(|n| n.name));
                }

                let selfparam = self.may_expect(Punct::Colon);
                if selfparam {
                    let name = self.parse_name()?;
                    names.push(name.map(|n| n.name));
                }
                let namesend = self.last_pos();

                // check for the prefix mismatch
                if let Some(Spanned { base: (_, Some(ref presig)), .. }) = funcspec {
                    match (presig.prefix.base, selfparam) {
                        (true, false) => {
                            self.error(presig.prefix.span, m::FunctionWithMethodSig {}).done()?;
                        }
                        (false, true) => {
                            self.error(presig.prefix.span, m::MethodWithFuncSig {}).done()?;
                        }
                        (true, true) | (false, false) => {}
                    }
                    // otherwise the prefix is ignored
                }

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
            };

            Tok::Keyword(Keyword::Local) => {
                match_next! { self;
                    // local function ...
                    Tok::Keyword(Keyword::Function) => {
                        let name = self.parse_name()?;
                        if let Some((_, sig, scope, body)) = self.parse_func_body(false,
                                                                                  funcspec)? {
                            let sibling_scope = self.generate_sibling_scope();
                            self.push_scope(sibling_scope);
                            let name = self.add_spanned_local_name(sibling_scope, name)?;
                            let name = name.map(NameRef::Local);
                            Box::new(St::FuncDecl(name, sig, scope, body, Some(sibling_scope)))
                        } else {
                            Box::new(St::Oops)
                        }
                    };

                    // local NAME ...
                    'unread: Tok::Name(_) => {
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
                        let mut namerefs = Vec::new().with_loc(names.span);
                        for namespec in names.base {
                            let name = self.add_spanned_local_name(sibling_scope, namespec.base)?;
                            namerefs.push(TypeSpec { base: name, modf: namespec.modf,
                                                     kind: namespec.kind });
                        }
                        Box::new(St::Local(namerefs, exps, sibling_scope))
                    };

                    'unread: _, m::NoFuncOrNameAfterLocal => Box::new(St::Oops);
                }
            };

            Tok::Keyword(Keyword::Return) => {
                let mut exps = Vec::new();
                let (span, _) = self.try_scan_explist(|exp| exps.push(exp))?;
                Box::new(St::Return(exps.with_loc(span)))
            };

            Tok::Keyword(Keyword::Break) => Box::new(St::Break);

            'unread: _ => {
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
                            error_with!(self, m::NoEq);

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
            };
        };

        Ok(Some(stmt.with_loc(begin..self.last_pos())))
    }

    fn parse_stmt_for_in(&mut self, names: Spanned<Vec<Spanned<IndexedName>>>) -> Result<Stmt> {
        let mut exps = Vec::new();
        let span = self.scan_list(Self::parse_exp, |exp| exps.push(exp))?;
        let exps = exps.with_loc(span);
        self.expect(Keyword::Do)?;
        let (names, scope, block) = self.parse_block_end_with_scope(move |parser, scope| {
            let mut names_ = Vec::new().with_loc(names.span);
            for name in names.base {
                names_.push(parser.add_spanned_local_name(scope, name)?);
            }
            Ok(names_)
        })?;
        Ok(Box::new(St::ForIn(names, exps, scope, block)))
    }

    fn parse_func_body(&mut self, selfparam: bool,
                       funcspec: Option<Spanned<(Vec<Spanned<Attr>>, Option<Spanned<Presig>>)>>)
            -> Result<Option<(Option<Spanned<SelfParam>>, Sig, Scope, Spanned<Block>)>> {
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

        match_next! { self;
            Tok::Punct(Punct::LParen) => {};

            // if no parentheses are found, the function parsing fails and should return Oops
            'unread: _, m::NoFuncArgs => {
                self.recover_to_close();
                return Ok(None);
            };
        }

        let begin = self.pos();
        let mut spec = None;
        let mut variadic = None;
        let end = self.recover_with(|parser| {
            let mut name = None;
            match_next! { parser;
                Tok::Punct(Punct::DotDotDot) in span => {
                    variadic = Some(span);
                };

                Tok::Name(name0) in span => {
                    name = Some(parser.indexed_name_from(name0, span));
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
                };

                'unread: Tok::Punct(Punct::RParen) => {};

                'unread: _, m::BadFuncArg => return Err(Stop::Recover);
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
                    if m.base != MM::None {
                        parser.error(m.span, m::NoModfAllowedInVarargs {}).done()?;
                    }
                    kind
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
        }, DelimAlreadyRead, || None)?;
        let end = end.unwrap_or_else(|| self.last_pos());
        returns = self.try_parse_kailua_rettype_spec()?;

        let (attrs, args, returns) = match funcspec {
            Some(Spanned { base: (attrs, Some(presig)), .. }) => {
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

                // we will prefer the actual signature at the final AST,
                // but we still need to record both token indices for each argument
                // (only when they are identical)
                let mut combinedargs = Vec::new();
                let mut sigargs = presig.base.args.base.head.into_iter();
                for (argname, argspec) in args {
                    let sigarg = sigargs.next();

                    let mut sigidx = None;
                    if let Some(ref sigarg) = sigarg {
                        if argname.name == sigarg.base.base.name {
                            sigidx = Some(sigarg.base.base.idx);
                        } else {
                            self.error(argname.span, m::ArgNameMismatchInFuncDecl {})
                                .note(sigarg.base.base.span, m::PriorArgNameInFuncSpec {})
                                .done()?;
                        }
                    }
                    if let Some(ref spec) = argspec {
                        self.error(spec.span, m::DuplicateSpecInFuncDecl {})
                            .note(presig.span, m::PriorFuncSpec {})
                            .done()?;
                        // and ignore that
                    }

                    let name = (argname, sigidx);
                    let spec = if let Some(TypeSpec { modf, kind, .. }) = sigarg.map(|s| s.base) {
                        TypeSpec { base: name, modf: modf, kind: kind }
                    } else {
                        self.make_kailua_typespec(name, argspec)
                    };
                    combinedargs.push(spec);
                }

                if let Some(returns) = returns {
                    self.error(returns.span, m::DuplicateReturnSpecInFuncDecl {})
                        .note(presig.span, m::PriorFuncSpec {})
                        .done()?;
                }

                let args = Seq { head: combinedargs, tail: presig.base.args.base.tail };
                (attrs, Ok(args.with_loc(presig.base.args.span)), presig.base.returns)
            },

            Some(Spanned { base: (attrs, None), .. }) => {
                (attrs, Err(args), returns.map(|ret| ret.base))
            },

            None => {
                (Vec::new(), Err(args), returns.map(|ret| ret.base))
            },
        };

        let args = args.unwrap_or_else(|args| {
            Seq {
                head: args.into_iter().map(|(name, spec)| {
                    self.make_kailua_typespec((name, None), spec)
                }).collect(),
                tail: varargs,
            }.with_loc(begin..end)
        });

        // resolve every parameter (including self)
        let ((selfparam, args), scope, block) = self.parse_block_end_with_scope(move |parser,
                                                                                      scope| {
            // attach all arguments to the function body scope
            // XXX should also mention all excess arguments
            // TODO should we add varargs?
            let selfparam = if selfparam {
                // use `begin` (that is, right after `(`) as an implicit span
                let selfname = Name::from(b"self"[..].to_owned()).with_loc(begin);
                let id = parser.add_spanned_local_name_without_idx(scope, selfname,
                                                                   LocalNameKind::ImplicitSelf)?;
                Some(id.map(SelfParam))
            } else {
                None
            };

            let mut head = Vec::new();
            for argspec in args.base.head {
                let name = parser.add_spanned_local_name(scope, argspec.base.0)?;
                if let Some(idx) = argspec.base.1 {
                    parser.set_token_aux(idx, TokenAux::LocalVarName(name.base.clone()));
                }
                head.push(TypeSpec { base: name, modf: argspec.modf, kind: argspec.kind });
            }

            let tail = if let Some(varargspec) = args.base.tail {
                let arg = if parser.language.lua() <= Lua::Lua51 { // legacy varargs
                    let argname = Name::from(b"arg"[..].to_owned()).with_loc(&varargspec);
                    let (id, prevspan) = parser.add_spanned_local_name_with_prev_span(
                        scope, argname, LocalNameKind::ImplicitLegacyArg,
                    )?;
                    if let Some(prevspan) = prevspan {
                        parser.error(id.span, m::LegacyArgNameInSameScope {})
                              .note(prevspan, m::PreviousNameInSameScope {})
                              .done()?;
                    }
                    Some(id)
                } else {
                    None
                };
                Some(Varargs { kind: varargspec.base, legacy_arg: arg })
            } else {
                None
            };

            let args = Seq { head: head, tail: tail }.with_loc(args.span);
            Ok((selfparam, args))
        })?;

        let sig = Sig { attrs: attrs, args: args, returns: returns };
        Ok(Some((selfparam, sig, scope, block)))
    }

    fn scan_tabular_body<Scan, Item>(&mut self, allow_ellipsis: bool,
                                     mut scan: Scan) -> Result<(bool /*ellipsis*/, Vec<Item>)>
            where Scan: FnMut(&mut Self) -> Result<Item> {
        let mut ellipsis = false;
        let mut items = Vec::new();

        self.recover(|parser| {
            while !parser.may_expect(Punct::RBrace) {
                if allow_ellipsis && parser.may_expect(Punct::DotDotDot) {
                    // this should be the last token before RBrace
                    ellipsis = true;
                    parser.expect(Punct::RBrace)?;
                    break;
                }
                let item = scan(parser)?;
                items.push(item);

                match_next! { parser;
                    // `,` [`}`]
                    // `;` [`}`]
                    Tok::Punct(Punct::Comma) | Tok::Punct(Punct::Semicolon) => {};

                    // `}`
                    Tok::Punct(Punct::RBrace) => break;

                    'unread: _, m::NoTableSep => return Err(Stop::Recover);
                }
            }
            Ok(())
        }, DelimAlreadyRead)?;

        Ok((ellipsis, items))
    }

    fn parse_table_body(&mut self) -> Result<Table> {
        let (_ellipsis, items) = self.scan_tabular_body(false, |parser| {
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
                    match_next! { parser;
                        Tok::Name(name) in span => {
                            if parser.may_expect(Punct::Eq) {
                                let key = Box::new(Ex::Str(Str::from(name))).with_loc(span);
                                let value = parser.parse_exp()?;
                                Ok((Some(key), value))
                            } else {
                                let name = parser.indexed_name_from(name, span);
                                let value = parser.parse_exp_after_name(name)?;
                                Ok((None, value))
                            }
                        };

                        'unread: _ => {
                            let value = parser.parse_exp()?;
                            Ok((None, value))
                        };
                    }
                }
            })
        })?;
        Ok(Table { items: items })
    }

    fn try_parse_args(&mut self) -> Result<Option<Spanned<Args>>> {
        let begin = self.pos();

        match_next! { self;
            Tok::Punct(Punct::LParen) => {
                let mut args = Vec::new();
                let (span, _) = self.recover_with(
                    |p| p.try_scan_explist(|exp| args.push(exp)), Punct::RParen,
                    || (Span::dummy(), false)
                )?;
                Ok(Some(Args::List(args).with_loc(span)))
            };

            Tok::Str(s) in span => Ok(Some(Args::Str(s).with_loc(span)));

            Tok::Punct(Punct::LBrace) => {
                let fields = self.parse_table_body()?;
                Ok(Some(Args::Table(fields).with_loc(begin..self.last_pos())))
            };

            'unread: _ => Ok(None);
        }
    }

    fn try_parse_prefix_exp(&mut self) -> Result<Option<Spanned<Exp>>> {
        // any prefixexp starts with name or parenthesized exp
        let begin = self.pos();
        let exp = match_next! { self;
            Tok::Punct(Punct::LParen) => {
                let exp_ = self.recover(Self::parse_exp, Punct::RParen)?;
                Box::new(Ex::Exp(exp_)).with_loc(begin..self.last_pos())
            };

            Tok::Name(name) in span => {
                let name = self.indexed_name_from(name, span);
                let nameref = self.resolve_name(name.base);
                Box::new(Ex::Var(nameref.with_loc(span))).with_loc(span)
            };

            'unread: _ => return Ok(None);
        };

        let exp = self.parse_prefix_exp_suffix(begin, exp)?;
        Ok(Some(exp))
    }

    // parse any postfix attachments
    fn parse_prefix_exp_suffix(&mut self, begin: Pos,
                               mut exp: Spanned<Exp>) -> Result<Spanned<Exp>> {
        loop {
            match_next! { self;
                // prefixexp "." ...
                Tok::Punct(Punct::Dot) => {
                    if let Some(name) = self.try_parse_name() {
                        let name = name.map(|n| n.name);
                        exp = Box::new(Ex::IndexName(exp, name)).with_loc(begin..self.last_pos());
                    } else {
                        error_with!(self, m::NoNameAfterExpDot);
                        break;
                    }
                };

                // prefixexp "[" ...
                Tok::Punct(Punct::LBracket) => {
                    let exp2 = self.recover(Self::parse_exp, Punct::RBracket)?;
                    let span = begin..self.last_pos();
                    exp = Box::new(Ex::Index(exp, exp2)).with_loc(span);
                };

                // prefixexp ":" ...
                Tok::Punct(Punct::Colon) => {
                    let name = if let Some(name) = self.try_parse_name() {
                        name.map(|n| n.name)
                    } else {
                        error_with!(self, m::NoArgsAfterExpColon);
                        break;
                    };
                    let namesend = self.last_pos();

                    if let Some(args) = self.try_parse_args()? {
                        let expname = (exp, name).with_loc(begin..namesend);
                        let span = begin..self.last_pos();
                        exp = Box::new(Ex::MethodCall(expname, args)).with_loc(span);
                    } else {
                        error_with!(self, m::NoArgsAfterExpColonName);

                        // convert `prefixexp ":" NAME` into `prefixexp "." NAME`
                        exp = Box::new(Ex::IndexName(exp, name)).with_loc(begin..namesend);
                        break;
                    }
                };

                // prefixexp STR
                // prefixexp "("
                // prefixexp "{"
                'unread: _ => {
                    if let Some(args) = self.try_parse_args()? {
                        let span = begin..self.last_pos();
                        exp = Box::new(Ex::FuncCall(exp, args)).with_loc(span);
                        continue;
                    }
                    break;
                }
            };
        }

        Ok(exp)
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
                    self.global_scope.entry(name.clone()).or_insert(span);
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

        match_next! { self;
            Tok::Keyword(Keyword::Nil) in span => Ok(Some(Box::new(Ex::Nil).with_loc(span)));
            Tok::Keyword(Keyword::False) in span => Ok(Some(Box::new(Ex::False).with_loc(span)));
            Tok::Keyword(Keyword::True) in span => Ok(Some(Box::new(Ex::True).with_loc(span)));
            Tok::Num(v) in span => Ok(Some(Box::new(Ex::Num(v)).with_loc(span)));
            Tok::Str(s) in span => Ok(Some(Box::new(Ex::Str(s)).with_loc(span)));
            Tok::Punct(Punct::DotDotDot) in span => Ok(Some(Box::new(Ex::Varargs).with_loc(span)));

            Tok::Keyword(Keyword::Function) => {
                let exp = match self.parse_func_body(false, funcspec)? {
                    Some((_, sig, scope, body)) => Ex::Func(sig, scope, body),
                    None => Ex::Oops,
                };
                Ok(Some(Box::new(exp).with_loc(begin..self.last_pos())))
            };

            Tok::Punct(Punct::LBrace) => {
                let table = self.parse_table_body()?;
                Ok(Some(Box::new(Ex::Table(table)).with_loc(begin..self.last_pos())))
            };

            'unread: _ => self.try_parse_prefix_exp();
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
    // to simplify the matter we have different precedences for comparison and recursion.
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
        trace!("parsing exp with min prec {}", minprec);

        let begin = self.pos();
        if let Some(exp) = self.try_parse_partial_unary_exp()? {
            let exp = self.parse_partial_binary_exp(minprec, begin, exp)?;
            Ok(Some(exp))
        } else {
            Ok(None)
        }
    }

    fn try_parse_partial_unary_exp(&mut self) -> Result<Option<Spanned<Exp>>> {
        fn unary_prec(op: UnOp) -> /*recursion*/ u8 {
            match op {
                // binary ^ operator here
                UnOp::Neg | UnOp::Not | UnOp::Len => 8,
                // other binary operators here
            }
        }

        let begin = self.pos();
        if let Some(op) = self.try_peek_unary_op() {
            // unop exp ...
            self.read();
            let rprec = unary_prec(op.base);
            let exp = self.parse_partial_exp(rprec)?;
            Ok(Some(Box::new(Ex::Un(op, exp)).with_loc(begin..self.last_pos())))
        } else {
            // atomicexp ...
            self.try_parse_atomic_exp()
        }
    }

    fn parse_partial_binary_exp(&mut self, minprec: u8, begin: Pos,
                                mut exp: Spanned<Exp>) -> Result<Spanned<Exp>> {
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

        // (unop exp | atomicexp) {binop <exp with lower prec>}
        while let Some(op) = self.try_peek_binary_op() {
            let (cprec, rprec) = binary_prec(op.base);
            if cprec <= minprec { break; }
            self.read();
            let exp2 = self.parse_partial_exp(rprec)?;
            exp = Box::new(Ex::Bin(exp, op, exp2)).with_loc(begin..self.last_pos());
        }

        Ok(exp)
    }

    fn parse_partial_exp(&mut self, minprec: u8) -> Result<Spanned<Exp>> {
        if let Some(exp) = self.try_parse_partial_exp(minprec)? {
            Ok(exp)
        } else {
            error_with!(self, m::NoExp);
            Ok(Box::new(Ex::Oops).with_loc(self.last_pos()))
        }
    }

    fn try_parse_exp(&mut self) -> Result<Option<Spanned<Exp>>> {
        self.try_parse_partial_exp(0)
    }

    fn parse_exp(&mut self) -> Result<Spanned<Exp>> {
        self.parse_partial_exp(0)
    }

    fn parse_exp_after_name(&mut self, name: Spanned<IndexedName>) -> Result<Spanned<Exp>> {
        trace!("continuing to parse exp after name");

        // NAME <...suffix...> <binop> <exp> ...
        // |-- prefix exp ---|                 |
        // |---- partial exp at minprec=0 -----|

        let begin = self.pos();
        let nameref = self.resolve_name(name.base);
        let exp = Box::new(Ex::Var(nameref.with_loc(name.span))).with_loc(name.span);
        let exp = self.parse_prefix_exp_suffix(begin, exp)?;
        self.parse_partial_binary_exp(0, begin, exp)
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
            error_with!(self, m::NoVar);
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

    fn make_kailua_typespec<Base>(
        &self, base: Base, spec: Option<Spanned<(MM, Option<Spanned<Kind>>)>>
    ) -> TypeSpec<Base> {
        if let Some(Spanned { base: (modf, kind), .. }) = spec {
            TypeSpec { base: base, modf: modf, kind: kind }
        } else {
            TypeSpec { base: base, modf: MM::None, kind: None }
        }
    }

    fn try_parse_kailua_attr(&mut self) -> Result<Option<Spanned<Attr>>> {
        let begin = self.pos();
        if self.may_expect(Punct::LBracket) {
            // `[` NAME [`(` VALUE ... `)` ] `]`
            let attr = self.recover_with(|parser| {
                let name = parser.try_name_or_keyword()?;
                let begin = parser.pos();
                let values = if parser.may_expect(Punct::LParen) {
                    let mut values = Vec::new();
                    parser.recover(|parser| {
                        if let Some(value) = parser.try_parse_kailua_attr_value()? {
                            values.push(value);
                            while parser.may_expect(Punct::Comma) {
                                values.push(parser.parse_kailua_attr_value()?);
                            }
                        }
                        Ok(())
                    }, Punct::RParen)?;
                    Some(values.with_loc(begin..parser.last_pos()))
                } else {
                    None
                };
                Ok(Some(Attr { name: name, values: values }))
            }, Punct::RBracket, || None)?;
            Ok(attr.map(|attr| attr.with_loc(begin..self.last_pos())))
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_attr_value(&mut self) -> Result<Option<Spanned<AttrValue>>> {
        match_next! { self;
            Tok::Name(name) in span => {
                Ok(Some(AttrValue::Name(Name::from(name).with_loc(span)).with_loc(span)))
            };
            'unread: _ => Ok(None);
        }
    }

    fn parse_kailua_attr_value(&mut self) -> Result<Spanned<AttrValue>> {
        if let Some(value) = self.try_parse_kailua_attr_value()? {
            Ok(value)
        } else {
            error_with!(self, m::NoAttrValue);
            Err(Stop::Recover)
        }
    }

    fn parse_kailua_modf(&mut self) -> Result<Spanned<M>> {
        let begin = self.pos();
        if self.may_expect(Keyword::Const) {
            let end = self.last_pos();
            Ok(M::Const.with_loc(begin..end))
        } else if self.may_expect(Keyword::Module) {
            let end = self.last_pos();
            self.error(begin..end, m::ModuleModfInNonAssign {}).done()?;
            Ok(M::None.with_loc(begin..end))
        } else {
            Ok(M::None.with_loc(begin))
        }
    }

    fn parse_kailua_modf_with_module(&mut self) -> Result<Spanned<MM>> {
        let begin = self.pos();
        let (modf, end) = if self.may_expect(Keyword::Const) {
            (MM::Const, self.last_pos())
        } else if self.may_expect(Keyword::Module) {
            // this gets ignored in the checker unless at proper places
            (MM::Module, self.last_pos())
        } else {
            (MM::None, begin)
        };
        Ok(modf.with_loc(begin..end))
    }

    fn parse_kailua_slotkind(&mut self) -> Result<Spanned<SlotKind>> {
        let begin = self.pos();
        let modf = self.parse_kailua_modf()?;
        let kind = self.parse_kailua_kind()?;
        Ok(SlotKind { modf: modf.base, kind: kind }.with_loc(begin..self.last_pos()))
    }

    fn parse_kailua_slotkind_after_name(&mut self, begin: Pos,
                                        name: Spanned<IndexedName>) -> Result<Spanned<SlotKind>> {
        let kind = self.parse_kailua_kind_after_name(begin, name)?;
        Ok(SlotKind { modf: M::None, kind: kind }.with_loc(begin..self.last_pos()))
    }

    // parses either [NAME ":"] KIND {"," [NAME ":"] KIND} ["," KIND "..."]; or [KIND "..."].
    // used for kind sequences after `--:` (without names) or function types (with names).
    // names should be unique if present.
    //
    // the caller is expected to error on unwanted cases.
    fn parse_kailua_kindlist(&mut self, allow_name: bool)
        -> Result<Seq<(Option<Spanned<Name>>, Spanned<Kind>), Spanned<Kind>>>
    {
        let mut head = Vec::new();
        let mut seen = HashMap::new(); // value denotes the first span

        let mut try_parse_named_kind = |parser: &mut Self| -> Result<Option<(Option<Spanned<Name>>,
                                                                             Spanned<Kind>)>> {
            if !allow_name {
                return Ok(parser.try_parse_kailua_kind()?.map(|kind| (None, kind)));
            }

            // NAME is a subset of KIND; distinguish by the secondary token
            let begin = parser.pos();
            match_next! { parser;
                Tok::Name(name) in span => {
                    let name = parser.indexed_name_from(name, span);
                    if parser.lookahead(Punct::Colon) {
                        match seen.entry(name.base.name.clone()) {
                            hash_map::Entry::Occupied(e) => {
                                parser.error(span, m::DuplicateArgNameInFuncKind { name: &name })
                                      .note(*e.get(), m::FirstArgNameInFuncKind {})
                                      .done()?;
                            }
                            hash_map::Entry::Vacant(e) => {
                                e.insert(name.span);
                            }
                        }

                        parser.expect(Punct::Colon)?;
                        let name = name.map(|n| n.name);
                        let kind = parser.parse_kailua_kind()?;
                        Ok(Some((Some(name), kind)))
                    } else {
                        let kind = parser.parse_kailua_kind_after_name(begin, name)?;
                        Ok(Some((None, kind)))
                    }
                };

                'unread: _ => {
                    let kind = parser.try_parse_kailua_kind()?;
                    Ok(kind.map(|kind| (None, kind)))
                };
            }
        };

        // try to read the first [NAME ":"] KIND
        let namekind = match try_parse_named_kind(self)? {
            Some(namekind) => namekind,
            None => match_next! { self;
                Tok::Punct(Punct::DotDotDot) in span => {
                    self.error(span, m::NoKindBeforeEllipsis {}).done()?;
                    // pretend that (an invalid) `(...)` is `(?...)`
                    return Ok(Seq { head: head, tail: Some(Box::new(K::Dynamic).with_loc(span)) });
                };
                'unread: _ => return Ok(Seq { head: head, tail: None });
            },
        };
        head.push(namekind);

        // continue reading {"," [NAME ":"] KIND}
        while self.may_expect(Punct::Comma) {
            let namekind = match try_parse_named_kind(self)? {
                Some(namekind) => namekind,
                None => match_next! { self;
                    Tok::Punct(Punct::DotDotDot) in span => {
                        self.error(span, m::NoKindBeforeEllipsis {}).done()?;
                        // pretend that (an invalid) `(<explist>, ...)` is `(<explist>, ?...)`
                        return Ok(Seq { head: head,
                                        tail: Some(Box::new(K::Dynamic).with_loc(span)) });
                    };
                    'unread: _, m::NoKind => (None, Box::new(K::Oops).without_loc());
                },
            };
            head.push(namekind);
        }

        // if "..." appears, the last KIND should be a tail kind (and no [NAME ":"] should appear)
        let tail = if self.may_expect(Punct::DotDotDot) {
            if let Some((name, kind)) = head.pop() {
                if let Some(name) = name {
                    self.error(name.span, m::VarargsNameInFuncKind {}).done()?;
                    // ignore the name
                }
                Some(kind)
            } else {
                None
            }
        } else {
            None
        };

        Ok(Seq { head: head, tail: tail })
    }

    // parses either NAME ":" KIND {"," NAME ":" KIND} ["," "..." [":" KIND]];
    // or ["..." [":" KIND]]. this is used for function type specification.
    //
    // the caller is expected to error on unwanted cases.
    fn parse_kailua_namekindlist(&mut self)
            -> Result<Spanned<Seq<Spanned<TypeSpec<Spanned<IndexedName>>>,
                                  Spanned<Option<Spanned<Kind>>>>>> {
        let mut variadic = None;
        let mut specs = Vec::new();

        let begin = self.pos();
        if self.may_expect(Punct::DotDotDot) { // "..." [":" KIND]
            variadic = Some(begin);
        } else { // NAME ":" KIND {"," NAME ":" KIND} ["," "..." [":" KIND]] or empty
            let parse_named_type_spec = |parser: &mut Self, name: Spanned<IndexedName>, begin: Pos|
                    -> Result<Spanned<TypeSpec<Spanned<IndexedName>>>> {
                parser.expect(Punct::Colon)?;
                let modf = parser.parse_kailua_modf_with_module()?;
                let kind = parser.parse_kailua_kind()?;
                let spec = TypeSpec { base: name, modf: modf.base, kind: Some(kind) };
                Ok(spec.with_loc(begin..parser.last_pos()))
            };

            if let Some(name) = self.try_parse_name() {
                specs.push(parse_named_type_spec(self, name, begin)?);
            } else {
                return Ok(Seq { head: specs, tail: None }.with_loc(begin..self.last_pos()));
            }

            while self.may_expect(Punct::Comma) {
                let begin = self.pos();
                if self.may_expect(Punct::DotDotDot) {
                    variadic = Some(begin);
                    break;
                }
                let name = self.parse_name()?;
                specs.push(parse_named_type_spec(self, name, begin)?);
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
        let args = self.parse_kailua_kindlist(true)?;
        self.expect(Punct::RParen)?;

        // error on partially named arguments (delay til here so that we have a parenthesized span)
        let namedcount = args.head.iter().filter(|&&(ref name, _)| name.is_some()).count();
        if namedcount > 0 && namedcount < args.head.len() {
            let span = begin..self.last_pos();
            self.error(span, m::PartiallyNamedFieldsInFuncKind {}).done()?;
        }

        let returns = if self.may_expect(Punct::DashDashGt) {
            // "(" ... ")" "-->" ...
            self.parse_kailua_returns()?
        } else {
            // "(" ... ")"
            Returns::Seq(Seq::empty())
        };

        let span = begin..self.last_pos();
        Ok(FuncKind { args: args, returns: returns }.with_loc(span))
    }

    fn try_parse_kailua_kind_params(&mut self)
            -> Result<Option<Spanned<Vec<(Spanned<M>, Spanned<Kind>)>>>> {
        let begin = self.pos();
        if self.may_expect(Punct::Lt) {
            // `<` MODF KIND [`,` MODF KIND] `>`
            let modf = self.parse_kailua_modf()?;
            let kind = self.recover_upto(Self::parse_kailua_kind)?;
            let mut kinds = vec![(modf, kind)];
            while self.may_expect(Punct::Comma) {
                let modf = self.parse_kailua_modf()?;
                let kind = self.recover_upto(Self::parse_kailua_kind)?;
                kinds.push((modf, kind));
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
                    error_with!(self, tok, m::NoKindParamsClose);
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
            error_with!(self, m::NoKindParams);
            Err(Stop::Recover)
        }
    }

    fn parse_kailua_kind_suffix(&mut self, begin: Pos, kind: Spanned<Kind>) -> Spanned<Kind> {
        // postfix type operators cannot appear twice in a row
        // (can work around with parens, but still not well-formed and the checker will error)
        if self.may_expect(Punct::Ques) {
            Box::new(K::WithNil(kind)).with_loc(begin..self.last_pos())
        } else if self.may_expect(Punct::Bang) {
            Box::new(K::WithoutNil(kind)).with_loc(begin..self.last_pos())
        } else {
            kind
        }
    }

    fn parse_kailua_kind_after_name(&mut self, begin: Pos,
                                    name: Spanned<IndexedName>) -> Result<Spanned<Kind>> {
        let kind = if *name.base.name == b"error"[..] {
            // may follow an error reason
            let reason = match_next! { self;
                Tok::Str(s) in span => Some(s.with_loc(span));
                'unread: _ => None;
            };
            Box::new(K::Error(reason)).with_loc(name.span)
        } else {
            let namespan = name.span;
            let kind = match self.builtin_kind(&name.base.name) {
                Some(Some(kind)) => kind,
                Some(None) => {
                    self.error(&name, m::ReservedKindName { name: &name }).done()?;
                    K::Oops
                },
                None => {
                    K::Named(name.map(|n| n.name))
                },
            };
            Box::new(kind).with_loc(namespan)
        };

        let kind = self.parse_kailua_kind_suffix(begin, kind); // handle ? or !
        let kind = self.parse_kailua_kind_after_kind(begin, kind)?; // handle `| KIND ...`
        Ok(kind)
    }

    // returns true if it can be followed by postfix operators
    fn try_parse_kailua_atomic_kind_seq(&mut self) -> Result<Option<AtomicKind>> {
        let begin = self.pos();

        let kind = match_next! { self;
            Tok::Keyword(Keyword::Function) in span => {
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
            };

            Tok::Punct(Punct::LParen) => {
                let mut args = self.parse_kailua_kindlist(false)?;
                self.expect(Punct::RParen)?;
                if args.head.len() != 1 || args.tail.is_some() {
                    // cannot be followed by postfix operators - XXX really?
                    let args = Seq {
                        head: args.head.into_iter().map(|(_name, kind)| kind).collect(),
                        tail: args.tail,
                    };
                    return Ok(Some(AtomicKind::Seq(args)));
                } else {
                    args.head.pop().unwrap().1
                }
            };

            Tok::Punct(Punct::LBrace) => {
                // "{" NAME ":" MODF KIND {"," NAME ":" MODF KIND} ["," "..."] "}"
                // we have already read up to the first NAME
                let parse_rec = |parser: &mut Self, first_name: Spanned<IndexedName>| -> Result<K> {
                    let mut seen = HashMap::new(); // value denotes the first span
                    let mut first_name = Some(first_name);
                    let (extensible, fields) = parser.scan_tabular_body(true, |parser| {
                        let name = if let Some(name) = first_name.take() {
                            name
                        } else {
                            parser.parse_name()?
                        };
                        match seen.entry(name.base.name.clone()) {
                            hash_map::Entry::Occupied(e) => {
                                parser.error(name.span,
                                             m::DuplicateFieldNameInRec { name: &name.base })
                                      .note(*e.get(), m::FirstFieldNameInRec {})
                                      .done()?;
                            }
                            hash_map::Entry::Vacant(e) => {
                                e.insert(name.span);
                            }
                        }
                        let name = Str::from(name.base.name).with_loc(name.span);
                        parser.expect(Punct::Colon)?;
                        let slotkind = parser.parse_kailua_slotkind()?;
                        Ok((name, slotkind))
                    })?;
                    Ok(K::Record(fields, extensible))
                };

                // "{" MODF KIND "," [MODF KIND {"," MODF KIND}] "}"
                // we have already read up to the first KIND
                let parse_tup = |parser: &mut Self,
                                 first_slotkind: Spanned<SlotKind>| -> Result<K> {
                    let mut first_slotkind = Some(first_slotkind);
                    let (_, mut fields) = parser.scan_tabular_body(false, |parser| {
                        // fake the first SlotKind read (but mind that this closure
                        // can never be called if the lookahead is `}`)
                        if let Some(slotkind) = first_slotkind.take() {
                            Ok(slotkind)
                        } else {
                            parser.parse_kailua_slotkind()
                        }
                    })?;
                    if let Some(slotkind) = first_slotkind {
                        // ...therefore this can happen for a valid code
                        assert!(fields.is_empty());
                        fields.push(slotkind);
                    }
                    Ok(K::Tuple(fields))
                };

                let kind = match_next! { self;
                    // "{" "}"
                    Tok::Punct(Punct::RBrace) => K::EmptyTable;

                    // "{" "..." "}"
                    Tok::Punct(Punct::DotDotDot) => {
                        self.expect(Punct::RBrace)?;
                        K::Record(Vec::new(), true)
                    };

                    // "{" NAME
                    // tuple or record -- distinguished by the secondary token
                    Tok::Name(name) in span => {
                        let name = self.indexed_name_from(name, span);
                        if self.lookahead(Punct::Colon) {
                            parse_rec(self, name)?
                        } else {
                            let slotkind = self.parse_kailua_slotkind_after_name(begin, name)?;
                            parse_tup(self, slotkind)?
                        }
                    };

                    // "{" KIND
                    // definitely tuple
                    'unread: _ => {
                        let slotkind = self.parse_kailua_slotkind()?;
                        parse_tup(self, slotkind)?
                    };
                };

                Box::new(kind).with_loc(begin..self.last_pos())
            };

            Tok::Keyword(Keyword::Nil) in span => Box::new(K::Nil).with_loc(span);
            Tok::Keyword(Keyword::True) in span => Box::new(K::BooleanLit(true)).with_loc(span);
            Tok::Keyword(Keyword::False) in span => Box::new(K::BooleanLit(false)).with_loc(span);

            Tok::Keyword(Keyword::Vector) => {
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
            };

            Tok::Keyword(Keyword::Map) => {
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
            };

            Tok::Name(name) in span => {
                let name = self.indexed_name_from(name, span);
                let kind = self.parse_kailua_kind_after_name(begin, name)?;
                return Ok(Some(AtomicKind::One(kind)));
            };

            Tok::Num(v) in span => {
                if i32::MIN as f64 <= v && v <= i32::MAX as f64 && v.floor() == v {
                    Box::new(K::IntegerLit(v as i32)).with_loc(span)
                } else {
                    self.error(span, m::NonIntegerType {}).done()?;
                    Box::new(K::Oops).with_loc(span)
                }
            };

            Tok::Str(s) in span => Box::new(K::StringLit(s)).with_loc(span);

            'unread: _ => return Ok(None);
        };

        let kind = self.parse_kailua_kind_suffix(begin, kind);
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

    fn parse_kailua_kind_after_kind(&mut self, begin: Pos,
                                    kind: Spanned<Kind>) -> Result<Spanned<Kind>> {
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
                        error_with!(self, m::NoType);
                        break;
                    }
                }
            }
            Ok(Box::new(K::Union(kinds)).with_loc(begin..self.last_pos()))
        } else {
            Ok(kind)
        }
    }

    fn try_parse_kailua_kind_seq(&mut self) -> Result<Option<Spanned<Seq<Spanned<Kind>>>>> {
        let begin = self.pos();
        match self.try_parse_kailua_prefixed_kind_seq()? {
            None => Ok(None),
            Some(AtomicKind::Seq(kindseq)) => {
                Ok(Some(kindseq.with_loc(begin..self.last_pos())))
            }
            Some(AtomicKind::One(kind)) => {
                let kind = self.parse_kailua_kind_after_kind(begin, kind)?;
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
            error_with!(self, m::NoTypeOrTypeSeq);
            Ok(Seq { head: vec![Recover::recover()], tail: None })
        }
    }

    fn parse_kailua_kind(&mut self) -> Result<Spanned<Kind>> {
        if let Some(kind) = self.try_parse_kailua_kind()? {
            Ok(kind)
        } else {
            error_with!(self, m::NoSingleType);
            Err(Stop::Recover)
        }
    }

    fn parse_kailua_returns(&mut self) -> Result<Returns> {
        match_next! { self;
            Tok::Punct(Punct::Bang) in span => Ok(Returns::Never(span));
            'unread: _ => {
                let seq = self.parse_kailua_kind_seq()?;
                Ok(Returns::Seq(seq))
            };
        }
    }

    fn try_parse_kailua_type_spec_with_spanned_modf(&mut self)
        -> Result<Option<Spanned<(Spanned<MM>, Option<Spanned<Kind>>)>>>
    {
        let metabegin = self.pos();
        if self.may_expect(Punct::DashDashColon) {
            self.recover_meta(|parser| {
                // allow for `--: { a = foo,
                //            --:   b = bar }`
                parser.begin_meta_comment(Punct::DashDashColon);

                let modf = parser.parse_kailua_modf_with_module()?;
                let mut kind = parser.recover_upto(Self::try_parse_kailua_kind)?;
                if kind.is_none() && !(parser.lookahead(Punct::DashDashGt) ||
                                       parser.lookahead(Punct::Newline)) {
                    // the next `end_meta_comment` call is guaranteed to fail, which leaves
                    // the kind empty (i.e. inferred later). better to make it an oops.
                    kind = Some(Recover::recover());
                }

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
                Ok(Some((modf, kind).with_loc(metabegin..metaend)))
            }, Recover::recover)
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_type_spec(&mut self)
        -> Result<Option<Spanned<(MM, Option<Spanned<Kind>>)>>>
    {
        trace!("parsing kailua type spec");
        let spec = self.try_parse_kailua_type_spec_with_spanned_modf()?;
        Ok(spec.map(|spec| spec.map(|(m,k)| (m.base, k))))
    }

    fn try_parse_kailua_typeseq_spec(&mut self)
        -> Result<Option<Spanned<Vec<Spanned<(MM, Option<Spanned<Kind>>)>>>>>
    {
        trace!("parsing kailua type sequence spec");
        let metabegin = self.pos();
        if self.may_expect(Punct::DashDashColon) {
            let mut specs = Vec::new();

            let metaend = self.recover_meta(|parser| {
                parser.begin_meta_comment(Punct::DashDashColon);

                parser.scan_list(|parser| {
                    let begin = parser.pos();
                    let modf = parser.parse_kailua_modf_with_module()?;
                    let mut kind = parser.recover_upto(Self::try_parse_kailua_kind)?;
                    if kind.is_none() && !(parser.lookahead(Punct::DashDashGt) ||
                                           parser.lookahead(Punct::Newline) ||
                                           parser.lookahead(Punct::Comma)) {
                        // the next `end_meta_comment` call is guaranteed to fail, which leaves
                        // the kind empty (i.e. inferred later). better to make it an oops.
                        kind = Some(Recover::recover());
                    }
                    Ok((modf, kind).with_loc(begin..parser.last_pos()))
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
                Ok(Some(metaend))
            }, || None)?;

            let metaend = metaend.unwrap_or_else(|| self.last_pos());
            Ok(Some(specs.with_loc(metabegin..metaend)))
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_rettype_spec(&mut self)
            -> Result<Option<Spanned<Returns>>> {
        trace!("parsing kailua return type spec");
        let begin = self.pos();
        if self.may_expect(Punct::DashDashGt) {
            self.recover_meta(|parser| {
                parser.begin_meta_comment(Punct::DashDashGt);
                let returns = parser.parse_kailua_returns()?;
                let end = parser.last_pos();
                parser.end_meta_comment(Punct::DashDashGt)?;

                Ok(Some(returns.with_loc(begin..end)))
            }, || None)
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
                let sigprefix = match_next! { parser;
                    Tok::Keyword(Keyword::Function) in span => Some(false.with_loc(span));
                    Tok::Keyword(Keyword::Method) in span => Some(true.with_loc(span));
                    'unread: _ => {
                        if !attrs_seen {
                            // force reading signatures if no attributes are present
                            error_with!(parser, m::NoFunctionOrMethodBeforeSig);
                        }
                        None
                    }
                };
                let sig = if let Some(prefix) = sigprefix {
                    parser.expect(Punct::LParen)?;
                    let args = parser.parse_kailua_namekindlist()?;
                    parser.expect(Punct::RParen)?;
                    let returns = if parser.may_expect(Punct::DashDashGt) {
                        parser.parse_kailua_returns()?
                    } else {
                        Returns::Seq(Seq::empty())
                    };
                    let end = parser.last_pos();
                    let presig = Presig { prefix: prefix, args: args, returns: Some(returns) };
                    Some(presig.with_loc(begin..end))
                } else {
                    None
                };

                let metaend = parser.last_pos();
                parser.end_meta_comment(Punct::DashDashV)?;
                Ok(Some((attrs, sig).with_loc(metabegin..metaend)))
            }, || None)
        } else {
            Ok(None)
        }
    }

    fn resolve_kailua_assume_rename(&mut self, global: bool, scopespan: Span,
                                    name: Spanned<IndexedName>, allow_local_shadowing: bool)
        -> Result<(Spanned<RenameRef>, Option<Scope>)>
    {
        if global {
            let local_shadowing =
                self.resolve_local_name_without_idx(&name.name).is_some();
            if self.block_depth > 0 {
                self.error(scopespan, m::AssumeGlobalInLocalScope {}).done()?;
            }
            if local_shadowing {
                self.error(&name, m::AssumeShadowedGlobal { name: &name.name }).done()?;
            }

            self.set_token_aux(name.base.idx, TokenAux::GlobalVarName);
            if self.block_depth == 0 && !local_shadowing {
                // only register a new global variable when it didn't error
                self.global_scope.entry(name.base.name.clone())
                                 .or_insert(name.span);
            }

            let rename = name.map(|n| {
                RenameRef {
                    before: NameRef::Global(n.name.clone()),
                    after: NameRef::Global(n.name),
                }
            });
            Ok((rename, None))
        } else {
            let scope = self.generate_sibling_scope();
            let rawname = name.clone();
            let name = name.map(|name| self.resolve_name(name));
            let kind = match name.base {
                NameRef::Local(ref scoped_id) => {
                    // the scoped id itself can be assumed!
                    let def = self.local_names.get(scoped_id).expect("unregistered scoped id");
                    match def.kind {
                        LocalNameKind::AssumedToGlobal => LocalNameKind::AssumedToGlobal,
                        _ => LocalNameKind::AssumedToLocal(scoped_id.clone()),
                    }
                },
                NameRef::Global(_) => {
                    if !allow_local_shadowing {
                        self.error(&name, m::AssumeShadowsGlobalScope { name: &rawname.name })
                            .done()?;
                    }
                    LocalNameKind::AssumedToGlobal
                },
            };
            let scoped_id = self.add_spanned_local_name_with_kind(scope, rawname, kind)?;
            let rename = name.map(move |n| {
                RenameRef { before: n, after: NameRef::Local(scoped_id.base) }
            });
            Ok((rename, Some(scope)))
        }
    }

    // assume [global] NAME ":" MODF KIND
    // assume [static] NAME {"." NAME} ":" MODF KIND
    // assume NAME {"." NAME} ":" MODF "method" ...
    // assume [global] class ["(" NAME ")"] NAME [":" NAME] ["=" MODF]
    //
    // returns a sibling scope if created.
    fn try_parse_kailua_assume(&mut self) -> Result<(Stmt, Option<Scope>)> {
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        enum Scope { Implied, Global, Static }

        #[derive(Clone, Debug)]
        enum Kindlike {
            Kind(Spanned<Kind>),
            Method(Span, Option<Spanned<FuncKind>>),
        }

        let scopebegin = self.pos();
        let scope = if self.may_expect(Keyword::Global) {
            Scope::Global
        } else if self.may_expect(Keyword::Static) {
            Scope::Static
        } else {
            Scope::Implied
        };
        let scope = scope.with_loc(scopebegin..self.last_pos());

        if self.may_expect(Keyword::Class) {
            let system = if self.may_expect(Punct::LParen) {
                self.recover_with(|parser| {
                    parser.parse_name().map(|n| Some(n.map(|n| n.name)))
                }, Punct::RParen, || None)?
            } else {
                None
            };
            let classname = self.parse_name()?;
            let parenttype = if self.may_expect(Punct::Colon) {
                self.recover_upto_with(|parser| {
                    parser.parse_name().map(|n| Some(n.map(|n| n.name)))
                }, || None)?
            } else {
                None
            };

            // ignore `static`
            if scope.base == Scope::Static {
                self.error(&scope, m::AssumeClassStatic {}).done()?;
            }

            let (renameref, sibling_scope) =
                self.resolve_kailua_assume_rename(scope.base == Scope::Global,
                                                  scope.span, classname, false)?;
            Ok((Box::new(St::KailuaAssumeClass(system, renameref, parenttype, sibling_scope)),
                sibling_scope))
        } else {
            let namesbegin = self.pos();
            let rootname = self.parse_name()?;
            let mut names = Vec::new();
            while self.may_expect(Punct::Dot) {
                let name = self.parse_name()?;
                names.push(name.map(|n| n.name));
            }
            let namesend = self.last_pos();

            self.expect(Punct::Colon)?;
            let modf = self.parse_kailua_modf()?.base;
            let kindbegin = self.pos();
            let kind = if self.may_expect(Keyword::Method) {
                let funckind = self.recover_upto_with(|p| {
                    p.parse_kailua_funckind().map(Some)
                }, || None)?;
                // if the parsing fails later, we need a span to construct K::Func
                Kindlike::Method(Span::new(kindbegin, self.last_pos()), funckind)
            } else {
                Kindlike::Kind(self.recover_upto(Self::parse_kailua_kind)?)
            };

            if names.is_empty() {
                // method() special form is not available for non-fields;
                // assume that it is a typo of function()
                let kind = match kind {
                    Kindlike::Kind(kind) => kind,
                    Kindlike::Method(kindspan, funckind) => {
                        self.error(kindspan, m::AssumeMethodToNonInstanceField {})
                              .done()?;
                        if let Some(funckind) = funckind {
                            Box::new(K::Func(funckind)).with_loc(kindspan)
                        } else {
                            Kind::recover().with_loc(kindspan)
                        }
                    }
                };

                // ignore `static`
                if scope.base == Scope::Static {
                    self.error(&scope, m::AssumeNameStatic {}).done()?;
                }
                let (renameref, sibling_scope) =
                    self.resolve_kailua_assume_rename(scope.base == Scope::Global,
                                                      scope.span, rootname, true)?;
                Ok((Box::new(St::KailuaAssume(renameref, modf, kind, sibling_scope)),
                    sibling_scope))
            } else {
                if scope.base == Scope::Global {
                    self.error(&scope, m::AssumeFieldGlobal {}).done()?;
                    // and treated as like Scope::Implied
                }

                let rootname0 = rootname.clone();
                let rootname = rootname.map(|name| self.resolve_name(name));
                if let NameRef::Global(_) = rootname.base {
                    if self.block_depth > 0 {
                        self.error(&rootname0,
                                   m::AssumeFieldGlobalInLocalScope { name: &rootname0.name })
                            .done()?;
                    }
                }

                let is_static = scope.base == Scope::Static;
                let names = (rootname, names).with_loc(namesbegin..namesend);
                let st = match kind {
                    Kindlike::Kind(kind) => St::KailuaAssumeField(is_static, names, modf, kind),
                    Kindlike::Method(kindspan, funckind) =>{
                        if scope.base != Scope::Implied {
                            self.error(kindspan, m::AssumeMethodToNonInstanceField {}).done()?;
                        }
                        if let Some(funckind) = funckind {
                            St::KailuaAssumeMethod(names, modf, funckind)
                        } else {
                            St::KailuaAssumeField(is_static, names, modf,
                                                  Kind::recover().without_loc())
                        }
                    },
                };
                Ok((Box::new(st), None))
            }
        }
    }

    fn try_parse_kailua_spec(&mut self) -> Result<Option<Option<Spanned<Stmt>>>> {
        trace!("parsing kailua spec");
        let begin = self.pos();

        if self.may_expect(Punct::DashDashHash) {
            let (stmt, end) = self.recover_meta(|parser| {
                parser.begin_meta_comment(Punct::DashDashHash);

                let mut sibling_scope = None;
                let stmt = match_next! { parser;
                    // assume ...
                    Tok::Keyword(Keyword::Assume) => {
                        let (stmt, new_sibling_scope) = parser.try_parse_kailua_assume()?;
                        sibling_scope = new_sibling_scope;
                        Some(stmt)
                    };

                    // class system ...
                    Tok::Keyword(Keyword::Class) => {
                        parser.expect(FixedName("system"))?;
                        let name = parser.parse_name()?;
                        Some(Box::new(St::KailuaClassSystem(name.map(|n| n.name))))
                    };

                    // open NAME
                    Tok::Keyword(Keyword::Open) => {
                        let name = parser.parse_name()?;
                        // TODO also set the parser option
                        Some(Box::new(St::KailuaOpen(name.map(|n| n.name))))
                    };

                    // type [local | global] NAME = KIND
                    Tok::Keyword(Keyword::Type) => {
                        let typescope = if parser.may_expect(Keyword::Local) {
                            TypeScope::Local
                        } else if parser.may_expect(Keyword::Global) {
                            TypeScope::Global
                        } else {
                            TypeScope::Exported
                        };

                        let name = parser.parse_name()?;
                        parser.expect(Punct::Eq)?;
                        let kind = parser.recover_upto(Self::parse_kailua_kind)?;

                        // forbid overriding builtin types
                        if parser.builtin_kind(&*name.base.name).is_some() {
                            parser.error(name.span, m::CannotRedefineBuiltin {}).done()?;
                        }

                        // error on module-level type definitions in the local scope
                        let end = parser.last_pos();
                        if parser.block_depth != 0 {
                            match typescope {
                                TypeScope::Local => {}
                                TypeScope::Global => {
                                    parser.error(begin..end, m::TypeGlobalInLocalScope {}).done()?;
                                }
                                TypeScope::Exported => {
                                    parser.error(begin..end, m::TypeExportInLocalScope {}).done()?;
                                }
                            }
                        }

                        Some(Box::new(St::KailuaType(typescope, name.map(|n| n.name), kind)))
                    };

                    'unread: _ => None; // empty `--#` is valid
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

    /// Parses the entire file and returns a chunk (while generating reports).
    ///
    /// Most parsing errors can be recovered, so the caller should also determine if
    /// it can continue in spite of reported errors.
    /// `kailua_diag::report::TrackMaxKind` is useful for this.
    pub fn into_chunk(mut self) -> report::Result<Chunk> {
        self.scope_stack = vec![];
        self.block_depth = 0;
        let ret = self.parse_block_until_eof();

        // any remaining scope is considered to end at the last token read
        // (unlike normal cases of `pop_scope_upto`, as this might be past EOF)
        let end = self.last_pos();
        while let Some((scope, scopebegin)) = self.scope_stack.pop() {
            self.scope_map.set_span(scope.with_loc(scopebegin..end));
        }

        if let Ok(block) = ret {
            Ok(Chunk {
                block: block,
                global_scope: self.global_scope,
                map: self.scope_map,
                local_names: self.local_names,
                token_aux: self.token_aux,
            })
        } else {
            Err(report::Stop)
        }
    }
}

