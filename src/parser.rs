#![allow(unused_imports)]
use lex::{Tok, Keyword};
use ast::{Name, Str, Var, Params, E, Exp, UnOp, BinOp, S, Stmt, Block};
extern crate lalrpop_util as __lalrpop_util;
use self::__lalrpop_util::ParseError as __ParseError;

mod __parse__Block {
    #![allow(non_snake_case, non_camel_case_types, unused_mut, unused_variables, unused_imports)]

    use lex::{Tok, Keyword};
    use ast::{Name, Str, Var, Params, E, Exp, UnOp, BinOp, S, Stmt, Block};
    extern crate lalrpop_util as __lalrpop_util;
    use self::__lalrpop_util::ParseError as __ParseError;
    use super::__ToTriple;
    pub fn parse_Block<
        __TOKEN: __ToTriple<Error=()>,
        __TOKENS: IntoIterator<Item=__TOKEN>,
    >(
        __tokens: __TOKENS,
    ) -> Result<Block, __ParseError<(),Tok,()>>
    {
        let __tokens = __tokens.into_iter();
        let mut __tokens = __tokens.map(|t| __ToTriple::to_triple(t));
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match try!(__state0(None, &mut __tokens, __lookahead)) {
            (_, Some(__lookahead), _) => {
                Err(__ParseError::ExtraToken { token: __lookahead })
            }
            (_, None, __Nonterminal::____Block(__nt)) => {
                Ok(__nt)
            }
            _ => unreachable!(),
        }
    }

    #[allow(dead_code)]
    pub enum __Nonterminal<> {
        _22_3b_22_3f(::std::option::Option<Tok>),
        _28_3cLastStmt_3e_20_22_3b_22_3f_29(Stmt),
        _28_3cLastStmt_3e_20_22_3b_22_3f_29_3f(::std::option::Option<Stmt>),
        _28_3cStmt_3e_20_22_3b_22_3f_29(Stmt),
        _28_3cStmt_3e_20_22_3b_22_3f_29_2a(::std::vec::Vec<Stmt>),
        Block(Block),
        LastStmt(Stmt),
        Stmt(Stmt),
        Term(f64),
        ____Block(Block),
        ____Term(f64),
    }

    // State 0
    //   (<Stmt> ";"?)* = (*) [EOF]
    //   (<Stmt> ";"?)* = (*) ["break"]
    //   (<Stmt> ";"?)* = (*) ["do"]
    //   (<Stmt> ";"?)* = (*) (<Stmt> ";"?)* (<Stmt> ";"?) [EOF]
    //   (<Stmt> ";"?)* = (*) (<Stmt> ";"?)* (<Stmt> ";"?) ["break"]
    //   (<Stmt> ";"?)* = (*) (<Stmt> ";"?)* (<Stmt> ";"?) ["do"]
    //   Block = (*) (<Stmt> ";"?)* (<LastStmt> ";"?)? [EOF]
    //   __Block = (*) Block [EOF]
    //
    //   EOF -> Reduce((<Stmt> ";"?)* =  => Call(ActionFn(10));)
    //   "break" -> Reduce((<Stmt> ";"?)* =  => Call(ActionFn(10));)
    //   "do" -> Reduce((<Stmt> ";"?)* =  => Call(ActionFn(10));)
    //
    //   (<Stmt> ";"?)* -> S1
    //   Block -> S2
    pub fn __state0<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            None |
            Some((_, Tok::Keyword(Keyword::Break), _)) |
            Some((_, Tok::Keyword(Keyword::Do), _)) => {
                let __nt = super::__action10();
                __result = (__lookbehind, __lookahead, __Nonterminal::_28_3cStmt_3e_20_22_3b_22_3f_29_2a(__nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        loop {
            let (__lookbehind, __lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_28_3cStmt_3e_20_22_3b_22_3f_29_2a(__nt) => {
                    let __sym0 = &mut Some(__nt);
                    __result = try!(__state1(__lookbehind, __tokens, __lookahead, __sym0));
                }
                __Nonterminal::Block(__nt) => {
                    let __sym0 = &mut Some(__nt);
                    __result = try!(__state2(__lookbehind, __tokens, __lookahead, __sym0));
                }
                _ => {
                    return Ok((__lookbehind, __lookahead, __nt));
                }
            }
        }
    }

    // State 1
    //   (<LastStmt> ";"?) = (*) LastStmt ";"? [EOF]
    //   (<LastStmt> ";"?)? = (*) [EOF]
    //   (<LastStmt> ";"?)? = (*) (<LastStmt> ";"?) [EOF]
    //   (<Stmt> ";"?) = (*) Stmt ";"? [EOF]
    //   (<Stmt> ";"?) = (*) Stmt ";"? ["break"]
    //   (<Stmt> ";"?) = (*) Stmt ";"? ["do"]
    //   (<Stmt> ";"?)* = (<Stmt> ";"?)* (*) (<Stmt> ";"?) [EOF]
    //   (<Stmt> ";"?)* = (<Stmt> ";"?)* (*) (<Stmt> ";"?) ["break"]
    //   (<Stmt> ";"?)* = (<Stmt> ";"?)* (*) (<Stmt> ";"?) ["do"]
    //   Block = (<Stmt> ";"?)* (*) (<LastStmt> ";"?)? [EOF]
    //   LastStmt = (*) "break" [EOF]
    //   LastStmt = (*) "break" [";"]
    //   Stmt = (*) "do" Block "end" [EOF]
    //   Stmt = (*) "do" Block "end" [";"]
    //   Stmt = (*) "do" Block "end" ["break"]
    //   Stmt = (*) "do" Block "end" ["do"]
    //
    //   EOF -> Reduce((<LastStmt> ";"?)? =  => Call(ActionFn(8));)
    //   "break" -> Shift(S8)
    //   "do" -> Shift(S9)
    //
    //   (<LastStmt> ";"?) -> S3
    //   (<LastStmt> ";"?)? -> S4
    //   (<Stmt> ";"?) -> S5
    //   LastStmt -> S6
    //   Stmt -> S7
    pub fn __state1<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<::std::vec::Vec<Stmt>>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            Some((_, __tok @ Tok::Keyword(Keyword::Break), __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym1 = &mut Some((__tok));
                __result = try!(__state8(__lookbehind, __tokens, __sym1));
            }
            Some((_, __tok @ Tok::Keyword(Keyword::Do), __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym1 = &mut Some((__tok));
                __result = try!(__state9(__lookbehind, __tokens, __sym1));
            }
            None => {
                let __nt = super::__action8();
                __result = (__lookbehind, __lookahead, __Nonterminal::_28_3cLastStmt_3e_20_22_3b_22_3f_29_3f(__nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookbehind, __lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_28_3cLastStmt_3e_20_22_3b_22_3f_29(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state3(__lookbehind, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::_28_3cLastStmt_3e_20_22_3b_22_3f_29_3f(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state4(__lookbehind, __tokens, __lookahead, __sym0, __sym1));
                }
                __Nonterminal::_28_3cStmt_3e_20_22_3b_22_3f_29(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state5(__lookbehind, __tokens, __lookahead, __sym0, __sym1));
                }
                __Nonterminal::LastStmt(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state6(__lookbehind, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::Stmt(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state7(__lookbehind, __tokens, __lookahead, __sym1));
                }
                _ => {
                    return Ok((__lookbehind, __lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    // State 2
    //   __Block = Block (*) [EOF]
    //
    //   EOF -> Reduce(__Block = Block => Call(ActionFn(0));)
    //
    pub fn __state2<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<Block>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __nt = super::__action0(__sym0);
                return Ok((__lookbehind, __lookahead, __Nonterminal::____Block(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 3
    //   (<LastStmt> ";"?)? = (<LastStmt> ";"?) (*) [EOF]
    //
    //   EOF -> Reduce((<LastStmt> ";"?)? = (<LastStmt> ";"?) => Call(ActionFn(7));)
    //
    pub fn __state3<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<Stmt>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __nt = super::__action7(__sym0);
                return Ok((__lookbehind, __lookahead, __Nonterminal::_28_3cLastStmt_3e_20_22_3b_22_3f_29_3f(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 4
    //   Block = (<Stmt> ";"?)* (<LastStmt> ";"?)? (*) [EOF]
    //
    //   EOF -> Reduce(Block = (<Stmt> ";"?)*, (<LastStmt> ";"?)? => Call(ActionFn(2));)
    //
    pub fn __state4<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<::std::vec::Vec<Stmt>>,
        __sym1: &mut Option<::std::option::Option<Stmt>>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __nt = super::__action2(__sym0, __sym1);
                return Ok((__lookbehind, __lookahead, __Nonterminal::Block(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 5
    //   (<Stmt> ";"?)* = (<Stmt> ";"?)* (<Stmt> ";"?) (*) [EOF]
    //   (<Stmt> ";"?)* = (<Stmt> ";"?)* (<Stmt> ";"?) (*) ["break"]
    //   (<Stmt> ";"?)* = (<Stmt> ";"?)* (<Stmt> ";"?) (*) ["do"]
    //
    //   EOF -> Reduce((<Stmt> ";"?)* = (<Stmt> ";"?)*, (<Stmt> ";"?) => Call(ActionFn(11));)
    //   "break" -> Reduce((<Stmt> ";"?)* = (<Stmt> ";"?)*, (<Stmt> ";"?) => Call(ActionFn(11));)
    //   "do" -> Reduce((<Stmt> ";"?)* = (<Stmt> ";"?)*, (<Stmt> ";"?) => Call(ActionFn(11));)
    //
    pub fn __state5<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<::std::vec::Vec<Stmt>>,
        __sym1: &mut Option<Stmt>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            None |
            Some((_, Tok::Keyword(Keyword::Break), _)) |
            Some((_, Tok::Keyword(Keyword::Do), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __nt = super::__action11(__sym0, __sym1);
                return Ok((__lookbehind, __lookahead, __Nonterminal::_28_3cStmt_3e_20_22_3b_22_3f_29_2a(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 6
    //   ";"? = (*) [EOF]
    //   ";"? = (*) ";" [EOF]
    //   (<LastStmt> ";"?) = LastStmt (*) ";"? [EOF]
    //
    //   EOF -> Reduce(";"? =  => Call(ActionFn(14));)
    //   ";" -> Shift(S11)
    //
    //   ";"? -> S10
    pub fn __state6<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<Stmt>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            Some((_, __tok @ Tok::Semicolon, __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym1 = &mut Some((__tok));
                __result = try!(__state11(__lookbehind, __tokens, __sym1));
            }
            None => {
                let __nt = super::__action14();
                __result = (__lookbehind, __lookahead, __Nonterminal::_22_3b_22_3f(__nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookbehind, __lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_22_3b_22_3f(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state10(__lookbehind, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookbehind, __lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    // State 7
    //   ";"? = (*) [EOF]
    //   ";"? = (*) ["break"]
    //   ";"? = (*) ["do"]
    //   ";"? = (*) ";" [EOF]
    //   ";"? = (*) ";" ["break"]
    //   ";"? = (*) ";" ["do"]
    //   (<Stmt> ";"?) = Stmt (*) ";"? [EOF]
    //   (<Stmt> ";"?) = Stmt (*) ";"? ["break"]
    //   (<Stmt> ";"?) = Stmt (*) ";"? ["do"]
    //
    //   EOF -> Reduce(";"? =  => Call(ActionFn(14));)
    //   ";" -> Shift(S13)
    //   "break" -> Reduce(";"? =  => Call(ActionFn(14));)
    //   "do" -> Reduce(";"? =  => Call(ActionFn(14));)
    //
    //   ";"? -> S12
    pub fn __state7<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<Stmt>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            Some((_, __tok @ Tok::Semicolon, __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym1 = &mut Some((__tok));
                __result = try!(__state13(__lookbehind, __tokens, __sym1));
            }
            None |
            Some((_, Tok::Keyword(Keyword::Break), _)) |
            Some((_, Tok::Keyword(Keyword::Do), _)) => {
                let __nt = super::__action14();
                __result = (__lookbehind, __lookahead, __Nonterminal::_22_3b_22_3f(__nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookbehind, __lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_22_3b_22_3f(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state12(__lookbehind, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookbehind, __lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    // State 8
    //   LastStmt = "break" (*) [EOF]
    //   LastStmt = "break" (*) [";"]
    //
    //   EOF -> Reduce(LastStmt = "break" => Call(ActionFn(4));)
    //   ";" -> Reduce(LastStmt = "break" => Call(ActionFn(4));)
    //
    pub fn __state8<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<Tok>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match __lookahead {
            None |
            Some((_, Tok::Semicolon, _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __nt = super::__action4(__sym0);
                return Ok((__lookbehind, __lookahead, __Nonterminal::LastStmt(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 9
    //   (<Stmt> ";"?)* = (*) ["break"]
    //   (<Stmt> ";"?)* = (*) ["do"]
    //   (<Stmt> ";"?)* = (*) ["end"]
    //   (<Stmt> ";"?)* = (*) (<Stmt> ";"?)* (<Stmt> ";"?) ["break"]
    //   (<Stmt> ";"?)* = (*) (<Stmt> ";"?)* (<Stmt> ";"?) ["do"]
    //   (<Stmt> ";"?)* = (*) (<Stmt> ";"?)* (<Stmt> ";"?) ["end"]
    //   Block = (*) (<Stmt> ";"?)* (<LastStmt> ";"?)? ["end"]
    //   Stmt = "do" (*) Block "end" [EOF]
    //   Stmt = "do" (*) Block "end" [";"]
    //   Stmt = "do" (*) Block "end" ["break"]
    //   Stmt = "do" (*) Block "end" ["do"]
    //
    //   "break" -> Reduce((<Stmt> ";"?)* =  => Call(ActionFn(10));)
    //   "do" -> Reduce((<Stmt> ";"?)* =  => Call(ActionFn(10));)
    //   "end" -> Reduce((<Stmt> ";"?)* =  => Call(ActionFn(10));)
    //
    //   (<Stmt> ";"?)* -> S14
    //   Block -> S15
    pub fn __state9<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<Tok>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match __lookahead {
            Some((_, Tok::Keyword(Keyword::Break), _)) |
            Some((_, Tok::Keyword(Keyword::Do), _)) |
            Some((_, Tok::Keyword(Keyword::End), _)) => {
                let __nt = super::__action10();
                __result = (__lookbehind, __lookahead, __Nonterminal::_28_3cStmt_3e_20_22_3b_22_3f_29_2a(__nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookbehind, __lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_28_3cStmt_3e_20_22_3b_22_3f_29_2a(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state14(__lookbehind, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::Block(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state15(__lookbehind, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookbehind, __lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    // State 10
    //   (<LastStmt> ";"?) = LastStmt ";"? (*) [EOF]
    //
    //   EOF -> Reduce((<LastStmt> ";"?) = LastStmt, ";"? => Call(ActionFn(9));)
    //
    pub fn __state10<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<Stmt>,
        __sym1: &mut Option<::std::option::Option<Tok>>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __nt = super::__action9(__sym0, __sym1);
                return Ok((__lookbehind, __lookahead, __Nonterminal::_28_3cLastStmt_3e_20_22_3b_22_3f_29(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 11
    //   ";"? = ";" (*) [EOF]
    //
    //   EOF -> Reduce(";"? = ";" => Call(ActionFn(13));)
    //
    pub fn __state11<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<Tok>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __nt = super::__action13(__sym0);
                return Ok((__lookbehind, __lookahead, __Nonterminal::_22_3b_22_3f(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 12
    //   (<Stmt> ";"?) = Stmt ";"? (*) [EOF]
    //   (<Stmt> ";"?) = Stmt ";"? (*) ["break"]
    //   (<Stmt> ";"?) = Stmt ";"? (*) ["do"]
    //
    //   EOF -> Reduce((<Stmt> ";"?) = Stmt, ";"? => Call(ActionFn(12));)
    //   "break" -> Reduce((<Stmt> ";"?) = Stmt, ";"? => Call(ActionFn(12));)
    //   "do" -> Reduce((<Stmt> ";"?) = Stmt, ";"? => Call(ActionFn(12));)
    //
    pub fn __state12<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<Stmt>,
        __sym1: &mut Option<::std::option::Option<Tok>>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            None |
            Some((_, Tok::Keyword(Keyword::Break), _)) |
            Some((_, Tok::Keyword(Keyword::Do), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __nt = super::__action12(__sym0, __sym1);
                return Ok((__lookbehind, __lookahead, __Nonterminal::_28_3cStmt_3e_20_22_3b_22_3f_29(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 13
    //   ";"? = ";" (*) [EOF]
    //   ";"? = ";" (*) ["break"]
    //   ";"? = ";" (*) ["do"]
    //
    //   EOF -> Reduce(";"? = ";" => Call(ActionFn(13));)
    //   "break" -> Reduce(";"? = ";" => Call(ActionFn(13));)
    //   "do" -> Reduce(";"? = ";" => Call(ActionFn(13));)
    //
    pub fn __state13<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<Tok>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match __lookahead {
            None |
            Some((_, Tok::Keyword(Keyword::Break), _)) |
            Some((_, Tok::Keyword(Keyword::Do), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __nt = super::__action13(__sym0);
                return Ok((__lookbehind, __lookahead, __Nonterminal::_22_3b_22_3f(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 14
    //   (<LastStmt> ";"?) = (*) LastStmt ";"? ["end"]
    //   (<LastStmt> ";"?)? = (*) ["end"]
    //   (<LastStmt> ";"?)? = (*) (<LastStmt> ";"?) ["end"]
    //   (<Stmt> ";"?) = (*) Stmt ";"? ["break"]
    //   (<Stmt> ";"?) = (*) Stmt ";"? ["do"]
    //   (<Stmt> ";"?) = (*) Stmt ";"? ["end"]
    //   (<Stmt> ";"?)* = (<Stmt> ";"?)* (*) (<Stmt> ";"?) ["break"]
    //   (<Stmt> ";"?)* = (<Stmt> ";"?)* (*) (<Stmt> ";"?) ["do"]
    //   (<Stmt> ";"?)* = (<Stmt> ";"?)* (*) (<Stmt> ";"?) ["end"]
    //   Block = (<Stmt> ";"?)* (*) (<LastStmt> ";"?)? ["end"]
    //   LastStmt = (*) "break" [";"]
    //   LastStmt = (*) "break" ["end"]
    //   Stmt = (*) "do" Block "end" [";"]
    //   Stmt = (*) "do" Block "end" ["break"]
    //   Stmt = (*) "do" Block "end" ["do"]
    //   Stmt = (*) "do" Block "end" ["end"]
    //
    //   "break" -> Shift(S21)
    //   "do" -> Shift(S22)
    //   "end" -> Reduce((<LastStmt> ";"?)? =  => Call(ActionFn(8));)
    //
    //   (<LastStmt> ";"?) -> S16
    //   (<LastStmt> ";"?)? -> S17
    //   (<Stmt> ";"?) -> S18
    //   LastStmt -> S19
    //   Stmt -> S20
    pub fn __state14<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<::std::vec::Vec<Stmt>>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            Some((_, __tok @ Tok::Keyword(Keyword::Break), __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym1 = &mut Some((__tok));
                __result = try!(__state21(__lookbehind, __tokens, __sym1));
            }
            Some((_, __tok @ Tok::Keyword(Keyword::Do), __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym1 = &mut Some((__tok));
                __result = try!(__state22(__lookbehind, __tokens, __sym1));
            }
            Some((_, Tok::Keyword(Keyword::End), _)) => {
                let __nt = super::__action8();
                __result = (__lookbehind, __lookahead, __Nonterminal::_28_3cLastStmt_3e_20_22_3b_22_3f_29_3f(__nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookbehind, __lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_28_3cLastStmt_3e_20_22_3b_22_3f_29(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state16(__lookbehind, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::_28_3cLastStmt_3e_20_22_3b_22_3f_29_3f(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state17(__lookbehind, __tokens, __lookahead, __sym0, __sym1));
                }
                __Nonterminal::_28_3cStmt_3e_20_22_3b_22_3f_29(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state18(__lookbehind, __tokens, __lookahead, __sym0, __sym1));
                }
                __Nonterminal::LastStmt(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state19(__lookbehind, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::Stmt(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state20(__lookbehind, __tokens, __lookahead, __sym1));
                }
                _ => {
                    return Ok((__lookbehind, __lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    // State 15
    //   Stmt = "do" Block (*) "end" [EOF]
    //   Stmt = "do" Block (*) "end" [";"]
    //   Stmt = "do" Block (*) "end" ["break"]
    //   Stmt = "do" Block (*) "end" ["do"]
    //
    //   "end" -> Shift(S23)
    //
    pub fn __state15<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<Tok>,
        __sym1: &mut Option<Block>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            Some((_, __tok @ Tok::Keyword(Keyword::End), __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym2 = &mut Some((__tok));
                __result = try!(__state23(__lookbehind, __tokens, __sym0, __sym1, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    // State 16
    //   (<LastStmt> ";"?)? = (<LastStmt> ";"?) (*) ["end"]
    //
    //   "end" -> Reduce((<LastStmt> ";"?)? = (<LastStmt> ";"?) => Call(ActionFn(7));)
    //
    pub fn __state16<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<Stmt>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            Some((_, Tok::Keyword(Keyword::End), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __nt = super::__action7(__sym0);
                return Ok((__lookbehind, __lookahead, __Nonterminal::_28_3cLastStmt_3e_20_22_3b_22_3f_29_3f(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 17
    //   Block = (<Stmt> ";"?)* (<LastStmt> ";"?)? (*) ["end"]
    //
    //   "end" -> Reduce(Block = (<Stmt> ";"?)*, (<LastStmt> ";"?)? => Call(ActionFn(2));)
    //
    pub fn __state17<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<::std::vec::Vec<Stmt>>,
        __sym1: &mut Option<::std::option::Option<Stmt>>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            Some((_, Tok::Keyword(Keyword::End), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __nt = super::__action2(__sym0, __sym1);
                return Ok((__lookbehind, __lookahead, __Nonterminal::Block(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 18
    //   (<Stmt> ";"?)* = (<Stmt> ";"?)* (<Stmt> ";"?) (*) ["break"]
    //   (<Stmt> ";"?)* = (<Stmt> ";"?)* (<Stmt> ";"?) (*) ["do"]
    //   (<Stmt> ";"?)* = (<Stmt> ";"?)* (<Stmt> ";"?) (*) ["end"]
    //
    //   "break" -> Reduce((<Stmt> ";"?)* = (<Stmt> ";"?)*, (<Stmt> ";"?) => Call(ActionFn(11));)
    //   "do" -> Reduce((<Stmt> ";"?)* = (<Stmt> ";"?)*, (<Stmt> ";"?) => Call(ActionFn(11));)
    //   "end" -> Reduce((<Stmt> ";"?)* = (<Stmt> ";"?)*, (<Stmt> ";"?) => Call(ActionFn(11));)
    //
    pub fn __state18<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<::std::vec::Vec<Stmt>>,
        __sym1: &mut Option<Stmt>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            Some((_, Tok::Keyword(Keyword::Break), _)) |
            Some((_, Tok::Keyword(Keyword::Do), _)) |
            Some((_, Tok::Keyword(Keyword::End), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __nt = super::__action11(__sym0, __sym1);
                return Ok((__lookbehind, __lookahead, __Nonterminal::_28_3cStmt_3e_20_22_3b_22_3f_29_2a(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 19
    //   ";"? = (*) ["end"]
    //   ";"? = (*) ";" ["end"]
    //   (<LastStmt> ";"?) = LastStmt (*) ";"? ["end"]
    //
    //   ";" -> Shift(S25)
    //   "end" -> Reduce(";"? =  => Call(ActionFn(14));)
    //
    //   ";"? -> S24
    pub fn __state19<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<Stmt>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            Some((_, __tok @ Tok::Semicolon, __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym1 = &mut Some((__tok));
                __result = try!(__state25(__lookbehind, __tokens, __sym1));
            }
            Some((_, Tok::Keyword(Keyword::End), _)) => {
                let __nt = super::__action14();
                __result = (__lookbehind, __lookahead, __Nonterminal::_22_3b_22_3f(__nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookbehind, __lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_22_3b_22_3f(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state24(__lookbehind, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookbehind, __lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    // State 20
    //   ";"? = (*) ["break"]
    //   ";"? = (*) ["do"]
    //   ";"? = (*) ["end"]
    //   ";"? = (*) ";" ["break"]
    //   ";"? = (*) ";" ["do"]
    //   ";"? = (*) ";" ["end"]
    //   (<Stmt> ";"?) = Stmt (*) ";"? ["break"]
    //   (<Stmt> ";"?) = Stmt (*) ";"? ["do"]
    //   (<Stmt> ";"?) = Stmt (*) ";"? ["end"]
    //
    //   ";" -> Shift(S27)
    //   "break" -> Reduce(";"? =  => Call(ActionFn(14));)
    //   "do" -> Reduce(";"? =  => Call(ActionFn(14));)
    //   "end" -> Reduce(";"? =  => Call(ActionFn(14));)
    //
    //   ";"? -> S26
    pub fn __state20<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<Stmt>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            Some((_, __tok @ Tok::Semicolon, __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym1 = &mut Some((__tok));
                __result = try!(__state27(__lookbehind, __tokens, __sym1));
            }
            Some((_, Tok::Keyword(Keyword::Break), _)) |
            Some((_, Tok::Keyword(Keyword::Do), _)) |
            Some((_, Tok::Keyword(Keyword::End), _)) => {
                let __nt = super::__action14();
                __result = (__lookbehind, __lookahead, __Nonterminal::_22_3b_22_3f(__nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookbehind, __lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_22_3b_22_3f(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state26(__lookbehind, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookbehind, __lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    // State 21
    //   LastStmt = "break" (*) [";"]
    //   LastStmt = "break" (*) ["end"]
    //
    //   ";" -> Reduce(LastStmt = "break" => Call(ActionFn(4));)
    //   "end" -> Reduce(LastStmt = "break" => Call(ActionFn(4));)
    //
    pub fn __state21<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<Tok>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match __lookahead {
            Some((_, Tok::Semicolon, _)) |
            Some((_, Tok::Keyword(Keyword::End), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __nt = super::__action4(__sym0);
                return Ok((__lookbehind, __lookahead, __Nonterminal::LastStmt(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 22
    //   (<Stmt> ";"?)* = (*) ["break"]
    //   (<Stmt> ";"?)* = (*) ["do"]
    //   (<Stmt> ";"?)* = (*) ["end"]
    //   (<Stmt> ";"?)* = (*) (<Stmt> ";"?)* (<Stmt> ";"?) ["break"]
    //   (<Stmt> ";"?)* = (*) (<Stmt> ";"?)* (<Stmt> ";"?) ["do"]
    //   (<Stmt> ";"?)* = (*) (<Stmt> ";"?)* (<Stmt> ";"?) ["end"]
    //   Block = (*) (<Stmt> ";"?)* (<LastStmt> ";"?)? ["end"]
    //   Stmt = "do" (*) Block "end" [";"]
    //   Stmt = "do" (*) Block "end" ["break"]
    //   Stmt = "do" (*) Block "end" ["do"]
    //   Stmt = "do" (*) Block "end" ["end"]
    //
    //   "break" -> Reduce((<Stmt> ";"?)* =  => Call(ActionFn(10));)
    //   "do" -> Reduce((<Stmt> ";"?)* =  => Call(ActionFn(10));)
    //   "end" -> Reduce((<Stmt> ";"?)* =  => Call(ActionFn(10));)
    //
    //   (<Stmt> ";"?)* -> S14
    //   Block -> S28
    pub fn __state22<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<Tok>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match __lookahead {
            Some((_, Tok::Keyword(Keyword::Break), _)) |
            Some((_, Tok::Keyword(Keyword::Do), _)) |
            Some((_, Tok::Keyword(Keyword::End), _)) => {
                let __nt = super::__action10();
                __result = (__lookbehind, __lookahead, __Nonterminal::_28_3cStmt_3e_20_22_3b_22_3f_29_2a(__nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookbehind, __lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_28_3cStmt_3e_20_22_3b_22_3f_29_2a(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state14(__lookbehind, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::Block(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state28(__lookbehind, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookbehind, __lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    // State 23
    //   Stmt = "do" Block "end" (*) [EOF]
    //   Stmt = "do" Block "end" (*) [";"]
    //   Stmt = "do" Block "end" (*) ["break"]
    //   Stmt = "do" Block "end" (*) ["do"]
    //
    //   EOF -> Reduce(Stmt = "do", Block, "end" => Call(ActionFn(3));)
    //   ";" -> Reduce(Stmt = "do", Block, "end" => Call(ActionFn(3));)
    //   "break" -> Reduce(Stmt = "do", Block, "end" => Call(ActionFn(3));)
    //   "do" -> Reduce(Stmt = "do", Block, "end" => Call(ActionFn(3));)
    //
    pub fn __state23<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<Tok>,
        __sym1: &mut Option<Block>,
        __sym2: &mut Option<Tok>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match __lookahead {
            None |
            Some((_, Tok::Semicolon, _)) |
            Some((_, Tok::Keyword(Keyword::Break), _)) |
            Some((_, Tok::Keyword(Keyword::Do), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __nt = super::__action3(__sym0, __sym1, __sym2);
                return Ok((__lookbehind, __lookahead, __Nonterminal::Stmt(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 24
    //   (<LastStmt> ";"?) = LastStmt ";"? (*) ["end"]
    //
    //   "end" -> Reduce((<LastStmt> ";"?) = LastStmt, ";"? => Call(ActionFn(9));)
    //
    pub fn __state24<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<Stmt>,
        __sym1: &mut Option<::std::option::Option<Tok>>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            Some((_, Tok::Keyword(Keyword::End), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __nt = super::__action9(__sym0, __sym1);
                return Ok((__lookbehind, __lookahead, __Nonterminal::_28_3cLastStmt_3e_20_22_3b_22_3f_29(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 25
    //   ";"? = ";" (*) ["end"]
    //
    //   "end" -> Reduce(";"? = ";" => Call(ActionFn(13));)
    //
    pub fn __state25<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<Tok>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match __lookahead {
            Some((_, Tok::Keyword(Keyword::End), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __nt = super::__action13(__sym0);
                return Ok((__lookbehind, __lookahead, __Nonterminal::_22_3b_22_3f(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 26
    //   (<Stmt> ";"?) = Stmt ";"? (*) ["break"]
    //   (<Stmt> ";"?) = Stmt ";"? (*) ["do"]
    //   (<Stmt> ";"?) = Stmt ";"? (*) ["end"]
    //
    //   "break" -> Reduce((<Stmt> ";"?) = Stmt, ";"? => Call(ActionFn(12));)
    //   "do" -> Reduce((<Stmt> ";"?) = Stmt, ";"? => Call(ActionFn(12));)
    //   "end" -> Reduce((<Stmt> ";"?) = Stmt, ";"? => Call(ActionFn(12));)
    //
    pub fn __state26<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<Stmt>,
        __sym1: &mut Option<::std::option::Option<Tok>>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            Some((_, Tok::Keyword(Keyword::Break), _)) |
            Some((_, Tok::Keyword(Keyword::Do), _)) |
            Some((_, Tok::Keyword(Keyword::End), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __nt = super::__action12(__sym0, __sym1);
                return Ok((__lookbehind, __lookahead, __Nonterminal::_28_3cStmt_3e_20_22_3b_22_3f_29(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 27
    //   ";"? = ";" (*) ["break"]
    //   ";"? = ";" (*) ["do"]
    //   ";"? = ";" (*) ["end"]
    //
    //   "break" -> Reduce(";"? = ";" => Call(ActionFn(13));)
    //   "do" -> Reduce(";"? = ";" => Call(ActionFn(13));)
    //   "end" -> Reduce(";"? = ";" => Call(ActionFn(13));)
    //
    pub fn __state27<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<Tok>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match __lookahead {
            Some((_, Tok::Keyword(Keyword::Break), _)) |
            Some((_, Tok::Keyword(Keyword::Do), _)) |
            Some((_, Tok::Keyword(Keyword::End), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __nt = super::__action13(__sym0);
                return Ok((__lookbehind, __lookahead, __Nonterminal::_22_3b_22_3f(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 28
    //   Stmt = "do" Block (*) "end" [";"]
    //   Stmt = "do" Block (*) "end" ["break"]
    //   Stmt = "do" Block (*) "end" ["do"]
    //   Stmt = "do" Block (*) "end" ["end"]
    //
    //   "end" -> Shift(S29)
    //
    pub fn __state28<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<Tok>,
        __sym1: &mut Option<Block>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            Some((_, __tok @ Tok::Keyword(Keyword::End), __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym2 = &mut Some((__tok));
                __result = try!(__state29(__lookbehind, __tokens, __sym0, __sym1, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    // State 29
    //   Stmt = "do" Block "end" (*) [";"]
    //   Stmt = "do" Block "end" (*) ["break"]
    //   Stmt = "do" Block "end" (*) ["do"]
    //   Stmt = "do" Block "end" (*) ["end"]
    //
    //   ";" -> Reduce(Stmt = "do", Block, "end" => Call(ActionFn(3));)
    //   "break" -> Reduce(Stmt = "do", Block, "end" => Call(ActionFn(3));)
    //   "do" -> Reduce(Stmt = "do", Block, "end" => Call(ActionFn(3));)
    //   "end" -> Reduce(Stmt = "do", Block, "end" => Call(ActionFn(3));)
    //
    pub fn __state29<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<Tok>,
        __sym1: &mut Option<Block>,
        __sym2: &mut Option<Tok>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match __lookahead {
            Some((_, Tok::Semicolon, _)) |
            Some((_, Tok::Keyword(Keyword::Break), _)) |
            Some((_, Tok::Keyword(Keyword::Do), _)) |
            Some((_, Tok::Keyword(Keyword::End), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __nt = super::__action3(__sym0, __sym1, __sym2);
                return Ok((__lookbehind, __lookahead, __Nonterminal::Stmt(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }
}
pub use self::__parse__Block::parse_Block;

mod __parse__Term {
    #![allow(non_snake_case, non_camel_case_types, unused_mut, unused_variables, unused_imports)]

    use lex::{Tok, Keyword};
    use ast::{Name, Str, Var, Params, E, Exp, UnOp, BinOp, S, Stmt, Block};
    extern crate lalrpop_util as __lalrpop_util;
    use self::__lalrpop_util::ParseError as __ParseError;
    use super::__ToTriple;
    pub fn parse_Term<
        __TOKEN: __ToTriple<Error=()>,
        __TOKENS: IntoIterator<Item=__TOKEN>,
    >(
        __tokens: __TOKENS,
    ) -> Result<f64, __ParseError<(),Tok,()>>
    {
        let __tokens = __tokens.into_iter();
        let mut __tokens = __tokens.map(|t| __ToTriple::to_triple(t));
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match try!(__state0(None, &mut __tokens, __lookahead)) {
            (_, Some(__lookahead), _) => {
                Err(__ParseError::ExtraToken { token: __lookahead })
            }
            (_, None, __Nonterminal::____Term(__nt)) => {
                Ok(__nt)
            }
            _ => unreachable!(),
        }
    }

    #[allow(dead_code)]
    pub enum __Nonterminal<> {
        _22_3b_22_3f(::std::option::Option<Tok>),
        _28_3cLastStmt_3e_20_22_3b_22_3f_29(Stmt),
        _28_3cLastStmt_3e_20_22_3b_22_3f_29_3f(::std::option::Option<Stmt>),
        _28_3cStmt_3e_20_22_3b_22_3f_29(Stmt),
        _28_3cStmt_3e_20_22_3b_22_3f_29_2a(::std::vec::Vec<Stmt>),
        Block(Block),
        LastStmt(Stmt),
        Stmt(Stmt),
        Term(f64),
        ____Block(Block),
        ____Term(f64),
    }

    // State 0
    //   Term = (*) "(" Term ")" [EOF]
    //   Term = (*) Num [EOF]
    //   __Term = (*) Term [EOF]
    //
    //   "(" -> Shift(S2)
    //   Num -> Shift(S3)
    //
    //   Term -> S1
    pub fn __state0<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            Some((_, __tok @ Tok::LParen, __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym0 = &mut Some((__tok));
                __result = try!(__state2(__lookbehind, __tokens, __sym0));
            }
            Some((_, Tok::Num(__tok0), __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym0 = &mut Some((__tok0));
                __result = try!(__state3(__lookbehind, __tokens, __sym0));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        loop {
            let (__lookbehind, __lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::Term(__nt) => {
                    let __sym0 = &mut Some(__nt);
                    __result = try!(__state1(__lookbehind, __tokens, __lookahead, __sym0));
                }
                _ => {
                    return Ok((__lookbehind, __lookahead, __nt));
                }
            }
        }
    }

    // State 1
    //   __Term = Term (*) [EOF]
    //
    //   EOF -> Reduce(__Term = Term => Call(ActionFn(1));)
    //
    pub fn __state1<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<f64>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __nt = super::__action1(__sym0);
                return Ok((__lookbehind, __lookahead, __Nonterminal::____Term(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 2
    //   Term = (*) "(" Term ")" [")"]
    //   Term = "(" (*) Term ")" [EOF]
    //   Term = (*) Num [")"]
    //
    //   "(" -> Shift(S5)
    //   Num -> Shift(S6)
    //
    //   Term -> S4
    pub fn __state2<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<Tok>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match __lookahead {
            Some((_, __tok @ Tok::LParen, __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym1 = &mut Some((__tok));
                __result = try!(__state5(__lookbehind, __tokens, __sym1));
            }
            Some((_, Tok::Num(__tok0), __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym1 = &mut Some((__tok0));
                __result = try!(__state6(__lookbehind, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookbehind, __lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::Term(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state4(__lookbehind, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookbehind, __lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    // State 3
    //   Term = Num (*) [EOF]
    //
    //   EOF -> Reduce(Term = Num => Call(ActionFn(5));)
    //
    pub fn __state3<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<f64>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __nt = super::__action5(__sym0);
                return Ok((__lookbehind, __lookahead, __Nonterminal::Term(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 4
    //   Term = "(" Term (*) ")" [EOF]
    //
    //   ")" -> Shift(S7)
    //
    pub fn __state4<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<Tok>,
        __sym1: &mut Option<f64>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            Some((_, __tok @ Tok::RParen, __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym2 = &mut Some((__tok));
                __result = try!(__state7(__lookbehind, __tokens, __sym0, __sym1, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    // State 5
    //   Term = (*) "(" Term ")" [")"]
    //   Term = "(" (*) Term ")" [")"]
    //   Term = (*) Num [")"]
    //
    //   "(" -> Shift(S5)
    //   Num -> Shift(S6)
    //
    //   Term -> S8
    pub fn __state5<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<Tok>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match __lookahead {
            Some((_, __tok @ Tok::LParen, __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym1 = &mut Some((__tok));
                __result = try!(__state5(__lookbehind, __tokens, __sym1));
            }
            Some((_, Tok::Num(__tok0), __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym1 = &mut Some((__tok0));
                __result = try!(__state6(__lookbehind, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookbehind, __lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::Term(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state8(__lookbehind, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookbehind, __lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    // State 6
    //   Term = Num (*) [")"]
    //
    //   ")" -> Reduce(Term = Num => Call(ActionFn(5));)
    //
    pub fn __state6<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<f64>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match __lookahead {
            Some((_, Tok::RParen, _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __nt = super::__action5(__sym0);
                return Ok((__lookbehind, __lookahead, __Nonterminal::Term(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 7
    //   Term = "(" Term ")" (*) [EOF]
    //
    //   EOF -> Reduce(Term = "(", Term, ")" => Call(ActionFn(6));)
    //
    pub fn __state7<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<Tok>,
        __sym1: &mut Option<f64>,
        __sym2: &mut Option<Tok>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __nt = super::__action6(__sym0, __sym1, __sym2);
                return Ok((__lookbehind, __lookahead, __Nonterminal::Term(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    // State 8
    //   Term = "(" Term (*) ")" [")"]
    //
    //   ")" -> Shift(S9)
    //
    pub fn __state8<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __lookahead: Option<((), Tok, ())>,
        __sym0: &mut Option<Tok>,
        __sym1: &mut Option<f64>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        match __lookahead {
            Some((_, __tok @ Tok::RParen, __loc)) => {
                let mut __lookbehind = Some(__loc);
                let mut __sym2 = &mut Some((__tok));
                __result = try!(__state9(__lookbehind, __tokens, __sym0, __sym1, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    // State 9
    //   Term = "(" Term ")" (*) [")"]
    //
    //   ")" -> Reduce(Term = "(", Term, ")" => Call(ActionFn(6));)
    //
    pub fn __state9<
        __TOKENS: Iterator<Item=Result<((), Tok, ()),()>>,
    >(
        __lookbehind: Option<()>,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<Tok>,
        __sym1: &mut Option<f64>,
        __sym2: &mut Option<Tok>,
    ) -> Result<(Option<()>, Option<((), Tok, ())>, __Nonterminal<>), __ParseError<(),Tok,()>>
    {
        let mut __result: (Option<()>, Option<((), Tok, ())>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(__ParseError::User { error: e }),
        };
        match __lookahead {
            Some((_, Tok::RParen, _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __nt = super::__action6(__sym0, __sym1, __sym2);
                return Ok((__lookbehind, __lookahead, __Nonterminal::Term(__nt)));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }
}
pub use self::__parse__Term::parse_Term;

pub fn __action0<
>(
    __0: Block,
) -> Block
{
    (__0)
}

pub fn __action1<
>(
    __0: f64,
) -> f64
{
    (__0)
}

pub fn __action2<
>(
    stmts: ::std::vec::Vec<Stmt>,
    laststmt: ::std::option::Option<Stmt>,
) -> Block
{
    {
        let mut stmts = stmts;
        if let Some(laststmt) = laststmt {
            stmts.push(laststmt);
        }
        stmts
    }
}

pub fn __action3<
>(
    _: Tok,
    __0: Block,
    _: Tok,
) -> Stmt
{
    Box::new(S::Do(__0))
}

pub fn __action4<
>(
    __0: Tok,
) -> Stmt
{
    Box::new(S::Break)
}

pub fn __action5<
>(
    __0: f64,
) -> f64
{
    (__0)
}

pub fn __action6<
>(
    _: Tok,
    __0: f64,
    _: Tok,
) -> f64
{
    (__0)
}

pub fn __action7<
>(
    __0: Stmt,
) -> ::std::option::Option<Stmt>
{
    Some(__0)
}

pub fn __action8<
>(
) -> ::std::option::Option<Stmt>
{
    None
}

pub fn __action9<
>(
    __0: Stmt,
    _: ::std::option::Option<Tok>,
) -> Stmt
{
    (__0)
}

pub fn __action10<
>(
) -> ::std::vec::Vec<Stmt>
{
    vec![]
}

pub fn __action11<
>(
    v: ::std::vec::Vec<Stmt>,
    e: Stmt,
) -> ::std::vec::Vec<Stmt>
{
    { let mut v = v; v.push(e); v }
}

pub fn __action12<
>(
    __0: Stmt,
    _: ::std::option::Option<Tok>,
) -> Stmt
{
    (__0)
}

pub fn __action13<
>(
    __0: Tok,
) -> ::std::option::Option<Tok>
{
    Some(__0)
}

pub fn __action14<
>(
) -> ::std::option::Option<Tok>
{
    None
}

pub trait __ToTriple<> {
    type Error;
    fn to_triple(value: Self) -> Result<((),Tok,()),Self::Error>;
}

impl<> __ToTriple<> for Tok {
    type Error = ();
    fn to_triple(value: Self) -> Result<((),Tok,()),()> {
        Ok(((), value, ()))
    }
}
impl<> __ToTriple<> for Result<(Tok),()> {
    type Error = ();
    fn to_triple(value: Self) -> Result<((),Tok,()),()> {
        value.map(|v| ((), v, ()))
    }
}
