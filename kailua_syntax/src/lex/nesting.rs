// nesting analysis.
// this is a post-processing engine after tokenization, to give useful information to
// the autocompletion engine and parser (for error recovery).

use std::cmp;
use std::cell::Cell;

use kailua_env::Spanned;
use super::{Tok, Punct, Keyword};

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

impl Nesting {
    fn is_stmt_level(&self) -> bool {
        match *self {
            Nesting::Top | Nesting::Meta |
            Nesting::Do | Nesting::Then | Nesting::Else | Nesting::End | Nesting::Until => true,
            _ => false,
        }
    }
}

// the major category of the current nesting, most commonly used for autocompletion
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum NestingCategory {
    Expr = 0,
    Meta = 1,
}

// a monotonically increasing serial number, only incremented when the nesting changes.
// the exact number has no meaning, only useful for the comparison.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct NestingSerial(u32);

impl NestingSerial {
    pub fn dummy() -> NestingSerial { NestingSerial(0) }

    pub fn to_usize(&self) -> usize { self.0 as usize }
}

#[derive(Clone, PartialEq, Debug)]
pub struct NestedToken {
    pub tok: Spanned<Tok>,

    // conceptually this nesting information applies to the token and following whitespaces
    pub depth: u16,
    pub category: NestingCategory,
    pub serial: NestingSerial,
}

pub struct Nest<'a> {
    iter: &'a mut Iterator<Item=Spanned<Tok>>,
    open_nestings: Vec<(Nesting, u32)>,
    next_serial: u32,
    meta_idx: Option<usize>, // invariant: if not None, open_nestings[meta] == Nesting::Meta
}

impl<'a> Nest<'a> {
    pub fn new(iter: &'a mut Iterator<Item=Spanned<Tok>>) -> Nest<'a> {
        Nest {
            iter: iter,
            open_nestings: vec![(Nesting::Top, 1)],
            next_serial: 2,
            meta_idx: None,
        }
    }

    fn update_nestings(&mut self, tok: &Tok) {
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
            // same to `Pop(Nesting::Meta)` followed by `Push(Nesting::Meta)`,
            // but additionally marks a new meta block
            ReplaceMeta,
        }

        let next_serial = Cell::new(self.next_serial);
        let gen_serial = || {
            let serial = next_serial.get();
            // to avoid panic on overflow (very, very unlikely)
            next_serial.set(serial.wrapping_add(1));
            serial
        };

        // `pop_non_stmt` is a statement-level flag; it will always pop any punctuation-delimted
        // nestings before actually executing an action (even if it would be a no-op).
        let meta = self.meta_idx.is_some();
        let (pop_non_stmt, action) = match (meta, tok) {
            // natural delimiters
            // no angle brackets, hard to distinguish comparison ops from delimiters
            (_, &Tok::Punct(Punct::LParen)) => (false, Action::Push(Nesting::Paren)),
            (_, &Tok::Punct(Punct::LBrace)) => (false, Action::Push(Nesting::Brace)),
            (_, &Tok::Punct(Punct::LBracket)) => (false, Action::Push(Nesting::Bracket)),
            (_, &Tok::Punct(Punct::RParen)) => (false, Action::Pop(Nesting::Paren)),
            (_, &Tok::Punct(Punct::RBrace)) => (false, Action::Pop(Nesting::Brace)),
            (_, &Tok::Punct(Punct::RBracket)) => (false, Action::Pop(Nesting::Bracket)),

            // meta blocks
            // Newline token is only generated inside a meta block by the lexer
            // Meta can be nested (sometimes), but closes the prior open Meta nesting
            (_, &Tok::Punct(Punct::DashDashV)) |
            (_, &Tok::Punct(Punct::DashDashColon)) =>
                // can appear in the expression context:
                // `--v` before function literals, `--:` inside an argument list (separate nesting)
                // note that `-->` always appear at the statement level due to the positioning
                (false, Action::ReplaceMeta),
            (_, &Tok::Punct(Punct::DashDashGt)) |
            (_, &Tok::Punct(Punct::DashDashHash)) =>
                (true, Action::ReplaceMeta),
            (_, &Tok::Punct(Punct::Newline)) =>
                (false, Action::Pop(Nesting::Meta)),

            // while, for and do blocks
            //
            // while/for ... do ... end
            // -----------End----------
            // -------Do-------
            //
            // do ... end
            // ----End---
            (false, &Tok::Keyword(Keyword::While)) |
            (false, &Tok::Keyword(Keyword::For)) =>
                (true, Action::Push2(Nesting::End, Nesting::Do)),
            (false, &Tok::Keyword(Keyword::Do)) =>
                (true, Action::PopOrPush(Nesting::Do, Nesting::End)),

            // function block
            //
            // function [NAME] ( ... ) ... end
            // -------------End-------------
            //                 -Paren-
            (false, &Tok::Keyword(Keyword::Function)) =>
                (false, Action::Push(Nesting::End)), // can appear in an expression

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
            (false, &Tok::Keyword(Keyword::If)) =>
                (true, Action::Push3(Nesting::End, Nesting::Else, Nesting::Then)),
            (false, &Tok::Keyword(Keyword::Then)) =>
                (true, Action::Pop(Nesting::Then)),
            (false, &Tok::Keyword(Keyword::Elseif)) =>
                (true, Action::PopAndPush(Nesting::Else, Nesting::Else)),
            (false, &Tok::Keyword(Keyword::Else)) =>
                (true, Action::Pop(Nesting::Else)),

            // repeat-until block
            //
            // repeat ... until ...
            // -----Until------
            (false, &Tok::Keyword(Keyword::Repeat)) => (true, Action::Push(Nesting::Until)),
            (false, &Tok::Keyword(Keyword::Until)) => (true, Action::Pop(Nesting::Until)),

            // the universal `end` token
            (_, &Tok::Keyword(Keyword::End)) => (!meta, Action::Pop(Nesting::End)),

            // EOF (conceptually forms the top-level nesting)
            (_, &Tok::EOF) => (true, Action::Pop(Nesting::Top)),

            _ => (false, Action::None),
        };

        if pop_non_stmt {
            let stmt = self.open_nestings.iter().rposition(|&(e, _)| e.is_stmt_level());
            let i = stmt.expect("Nest::update_nestings got a corrupted list of nestings") + 1;
            if i < self.open_nestings.len() {
                trace!("token {:?} closed {} non-statement nesting(s)",
                       tok, self.open_nestings.len() - i);
            }
            self.open_nestings.truncate(i);
        }

        let mut min_depth = self.open_nestings.len();
        let mut meta_updated = false;
        match action {
            Action::None => {}

            Action::Push(e) => {
                let e = (e, gen_serial());
                trace!("token {:?} opened a nesting {:?}", tok, e);
                self.open_nestings.push(e);
            }

            Action::Push2(e1, e2) => {
                let e1 = (e1, gen_serial());
                let e2 = (e2, gen_serial());
                trace!("token {:?} opened nestings {:?} and {:?}", tok, e1, e2);
                self.open_nestings.push(e1);
                self.open_nestings.push(e2);
            }

            Action::Push3(e1, e2, e3) => {
                let e1 = (e1, gen_serial());
                let e2 = (e2, gen_serial());
                let e3 = (e3, gen_serial());
                trace!("token {:?} opened nestings {:?}, {:?} and {:?}", tok, e1, e2, e3);
                self.open_nestings.push(e1);
                self.open_nestings.push(e2);
                self.open_nestings.push(e3);
            }

            Action::Pop(epop) => {
                if let Some(i) = self.open_nestings.iter().rposition(|&(e, _)| e == epop) {
                    let epop = self.open_nestings[i];
                    trace!("token {:?} closed a nesting {:?}", tok, epop);
                    self.open_nestings.truncate(i);
                    min_depth = i;
                } else {
                    trace!("token {:?} tried to close a nesting {:?}", tok, epop);
                }
            }

            Action::PopAndPush(epop, epush) => {
                if let Some(i) = self.open_nestings.iter().rposition(|&(e, _)| e == epop) {
                    let epop = self.open_nestings[i];
                    let epush = (epush, gen_serial());
                    trace!("token {:?} closed a nesting {:?} and opened a nesting {:?}",
                           tok, epop, epush);
                    self.open_nestings.truncate(i);
                    self.open_nestings.push(epush);
                    min_depth = i;
                } else {
                    trace!("token {:?} tried to close a nesting {:?} and open a nesting {:?}",
                           tok, epop, epush);
                }
            }

            Action::PopOrPush(epop, epush) => {
                if let Some(i) = self.open_nestings.iter().rposition(|&(e, _)| e == epop) {
                    let epop = self.open_nestings[i];
                    trace!("token {:?} closed a nesting {:?} instead of opening a nesting {:?}",
                           tok, epop, epush);
                    self.open_nestings.truncate(i);
                    min_depth = i;
                } else {
                    let epush = (epush, gen_serial());
                    trace!("token {:?} tried to close a nesting {:?} but \
                            opened a nesting {:?} instead", tok, epop, epush);
                    self.open_nestings.push(epush);
                }
            }

            Action::ReplaceMeta => {
                let epush = (Nesting::Meta, gen_serial());
                if let Some(i) = self.meta_idx {
                    assert_eq!(self.open_nestings.get(i).map(|e| e.0), Some(Nesting::Meta));
                    let epop = self.open_nestings[i];
                    trace!("token {:?} closed a meta block {:?} and reopened a meta block {:?}",
                           tok, epop, epush);
                    self.open_nestings.truncate(i);
                    min_depth = i;
                } else {
                    trace!("token {:?} opened a meta block {:?}", tok, epush);
                }
                self.meta_idx = Some(self.open_nestings.len());
                self.open_nestings.push(epush);
                meta_updated = true;
            }
        }

        // check if the prior action has unknowingly destroyed the Meta nesting
        // (Meta can only be introduced by ReplaceMeta)
        if let Some(i) = self.meta_idx {
            if !meta_updated && i >= min_depth {
                self.meta_idx = None;
            }
        }

        self.next_serial = next_serial.get();
    }
}

impl<'a> Iterator for Nest<'a> {
    type Item = NestedToken;

    fn next(&mut self) -> Option<NestedToken> {
        let tok = match self.iter.next() {
            Some(tok) => {
                assert!(self.open_nestings.len() > 0);
                tok
            },
            None => {
                // the final token should be EOF, and Newline has been generated as needed
                assert_eq!(self.open_nestings.len(), 0);
                assert_eq!(self.meta_idx, None);
                return None;
            }
        };

        self.update_nestings(&tok);

        // if depth exceeds u16 range, there is not much point doing AC anyway
        let depth = cmp::min(0xffff, self.open_nestings.len()) as u16;
        let category = match self.meta_idx {
            Some(_) => NestingCategory::Meta,
            None => NestingCategory::Expr,
        };
        let serial = self.open_nestings.last().map_or(0, |e| e.1);
        Some(NestedToken {
            tok: tok, depth: depth, category: category,
            serial: NestingSerial(serial),
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

