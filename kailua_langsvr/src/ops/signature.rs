// signature help: resolve what is the closest function expression for the possible call,
// then wait for checker outputs to actually return the signature

use kailua_env::Pos;
use kailua_diag::{Localize, Localized};
use kailua_syntax::lex::{Tok, Punct, NestedToken};
use kailua_check::{TypeContext, Display, Output, Nil, Functions};

use protocol::*;
use message as m;
use super::{get_prefix_expr_slot, last_non_comment, PrefixExprSlot};

// for the interactivity, the lookbehind is limited to a reasonable number
const LOOKBEHIND_LIMIT: usize = 4096;

// look for the enclosing function call and the argument position of the caret.
// returns the index to the first token of arguments and the argument index, if any.
// the callee would have to seek more to find the end position of the function expression,
// and resolve the function slot from that position to continue.
//
// it seeks for the tokens `(` or `{` preceded by something resembling a function,
// while counting the number of commas *at the nesting opened by that `(` or `{`*.
// the token is considered to be the end of a function if it's `)` or a name;
// otherwise it is a subexpression inside a call and the number of commas is reset to 0.
//
// this should also handle a special case of `) "string"` or `NAME "string"`.
fn enclosing_func_call(tokens: &[NestedToken], pos: Pos) -> Option<(usize, usize)> {
    //            blank                    end of line
    //              |                           |
    //  ( N A M E , _ " s t _ r i n g " _ ) _ _ $
    // 0 1 1 1 1 2 3 3 3 3 3 3 3 3 3 3 4 4 5 5 5 = idx
    let idx = match tokens.binary_search_by(|tok| tok.tok.span.end().cmp(&pos)) {
        Ok(i) => i + 1, // tokens[i].end == pos
        Err(i) => i, // tokens[i-1].end or -inf < pos < tokens[i].end or inf
    };

    // no token before the caret or the caret is strictly inside the first token; no call here
    if idx == 0 { return None; }

    // in the delimited sequence of tokens, the opening token and subsequent tokens are
    // in the same nesting, and the closing token is in the outermost nesting.
    // (this is because each token first updates the nestings and gets assigned its nesting.)
    // therefore the nesting for the caret (between two tokens) should be that of the first.
    let mut last_tok;
    let mut init_depth;
    let mut init_serial;
    if let Some(tok) = tokens.get(idx - 1) {
        last_tok = tok;
        init_depth = tok.depth;
        init_serial = tok.serial;
    } else {
        return None;
    }

    // a special case for `<func> "str"`, which has no nesting changes
    let ptok = if idx > 1 { tokens.get(idx - 2) } else { None };
    match (ptok.map(|tok| &tok.tok.base), &last_tok.tok.base) {
        (Some(&Tok::Name(_)), &Tok::Str(_)) |
        (Some(&Tok::Punct(Punct::RParen)), &Tok::Str(_)) => return Some((idx - 1, 0)),
        (_, _) => {}
    }

    let mut commas = 0;
    for (i, tok) in tokens[..idx - 1].iter().enumerate().rev().take(LOOKBEHIND_LIMIT) {
        let prev_tok = last_tok;
        last_tok = tok;

        if tok.depth <= init_depth && tok.serial != init_serial {
            // escaped the current nesting, the last token should have been the opening token.
            match (&tok.tok.base, &prev_tok.tok.base) {
                (&Tok::Name(_), &Tok::Punct(Punct::LParen)) |
                (&Tok::Punct(Punct::RParen), &Tok::Punct(Punct::LParen)) => {
                    // `tok` is likely the last token of the function expression
                    return Some((i + 1, commas));
                }

                (&Tok::Name(_), &Tok::Punct(Punct::LBrace)) |
                (&Tok::Punct(Punct::RParen), &Tok::Punct(Punct::LBrace)) => {
                    // same as above, but the call is `<func> {...}` which has a single argument
                    return Some((i + 1, 0));
                }

                (_, _) => {
                    // otherwise we move to the parent nesting and reset the # of commas
                    init_depth = tok.depth;
                    init_serial = tok.serial;
                    commas = 0;
                }
            }
        } else if tok.depth > init_depth {
            // ignore more nested tokens (but count them towards the threshold)
            continue;
        }

        if let Tok::Punct(Punct::Comma) = prev_tok.tok.base {
            // the number of commas at the current nesting = the eventual argument index
            // note that we take acount for prev_tok as `a , | b` will start with prev_tok = `,`.
            commas += 1;
        }
    }

    None
}

#[derive(Clone, Debug)]
pub struct Loc {
    pub args_token_idx: usize,
    pub arg_idx: usize,
}

pub fn locate(tokens: &[NestedToken], pos: Pos) -> Option<Loc> {
    enclosing_func_call(tokens, pos).map(|(token_idx, arg_idx)| {
        Loc { args_token_idx: token_idx, arg_idx: arg_idx }
    })
}

pub fn help<F>(tokens: &[NestedToken], loc: &Loc, output: &Output,
               mut localize: F) -> Option<SignatureHelp>
    where F: for<'a> FnMut(&'a Localize) -> Localized<'a, Localize>
{
    use std::fmt::Write;

    let empty_signature = || {
        SignatureHelp { signatures: Vec::new(), activeSignature: None, activeParameter: None }
    };

    // parameters are highlighted in the signature as a string pattern.
    // this is darn wrong especially for Kailua
    // because it fails to highlight, for example, `function(string, string)` correctly.
    // to deal with this, we prefix a series of invisible characters (again) to each parameter.
    let write_invisible_num = |s: &mut String, mut n: usize| {
        s.push('\u{2060}');
        while n > 0 {
            s.push(['\u{200b}', '\u{200c}', '\u{200d}'][n % 3]);
            n /= 3;
        }
    };

    let res = get_prefix_expr_slot(tokens, loc.args_token_idx, output);
    debug!("signature_help: get_prefix_expr_slot returns {:?}", res);

    match res {
        // fail fast, this is not a prefix expression
        None => Some(empty_signature()),

        // we may retry for the newer output if there is no slot available
        Some(PrefixExprSlot::NotFound) => None,

        // check if it's a callable function (otherwise we fail fast)
        Some(PrefixExprSlot::Found(end_idx, slot)) => {
            let ty = if let Some(ty) = output.resolve_exact_type(&slot.unlift()) {
                ty
            } else {
                return Some(empty_signature());
            };

            if ty.nil() == Nil::Noisy {
                // nilable function is not callable
                return Some(empty_signature());
            }

            let func = if let Some(&Functions::Simple(ref func)) = ty.get_functions() {
                func
            } else {
                return Some(empty_signature());
            };

            // seek more to determine this is a method call or not.
            // tokens[end_idx] is never a comment, so we are sure that
            // tokens[end_idx] is a name and preceding non-comment token is `:`
            // when this is a method call.
            let mut is_method = false;
            if let Tok::Name(_) = tokens[end_idx].tok.base {
                let prev_tok = last_non_comment(&tokens[..end_idx]).map(|(_, tok)| &tok.tok.base);
                if let Some(&Tok::Punct(Punct::Colon)) = prev_tok {
                    is_method = true;
                }
            }

            // they should be constructed in a lock step,
            // as matching params in the label are underlined.
            let mut label = format!("{}(", if is_method { "method" } else { "function" });
            let mut params = Vec::new();
            let types = output.types() as &TypeContext;

            let mut first = true;
            let mut names = func.argnames.iter();
            for t in &func.args.head {
                let implicit;
                if first {
                    first = false;
                    implicit = is_method;
                    if implicit {
                        let _ = write!(label, "{} ", localize(&m::OmittedSelfLabel {}));
                    }
                } else {
                    label.push_str(", ");
                    implicit = false;
                }

                let mut param = String::new();
                write_invisible_num(&mut param, params.len());
                if let Some(name) = names.next() {
                    if let Some(ref name) = *name {
                        let _ = write!(param, "{:+}: ", name);
                    }
                }
                let _ = write!(param, "{}", localize(&t.display(types)));
                label.push_str(&param);
                if !implicit {
                    // implicit parameter is not listed
                    params.push(ParameterInformation { label: param, documentation: None });
                }
            }

            if let Some(ref t) = func.args.tail {
                if !first {
                    label.push_str(", ");
                }
                let mut param = String::new();
                write_invisible_num(&mut param, params.len());
                let _ = write!(param, "{:#}...", localize(&t.display(types)));
                label.push_str(&param);
                params.push(ParameterInformation { label: param, documentation: None });
            }

            match func.returns {
                Some(ref returns) => match (returns.head.len(), returns.tail.is_some()) {
                    (0, false) => {
                        label.push_str(")");
                    }
                    (1, false) => {
                        let _ = write!(label, ") --> {}",
                                       localize(&returns.head[0].display(types)));
                    }
                    (_, _) => {
                        let _ = write!(label, ") --> {}", localize(&returns.display(types)));
                    }
                },
                None => {
                    label.push_str(") --> !");
                }
            }

            let param_idx = if loc.arg_idx < params.len() {
                Some(loc.arg_idx as u32)
            } else if func.args.tail.is_some() {
                // clamp to the number of parameters listed,
                // mapping the last arguments to the variadic position
                assert!(!params.is_empty());
                Some((params.len() - 1) as u32)
            } else {
                // otherwise it's an excess parameter and should not be highlighted
                None
            };

            return Some(SignatureHelp {
                signatures: vec![
                    SignatureInformation { label: label, documentation: None, parameters: params },
                ],
                activeSignature: Some(0),
                activeParameter: param_idx,
            });
        },
    }
}

