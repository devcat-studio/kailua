// go to definition: classify whether it's a name or a simple function call expr,
// then go through all open files to locate the definition
// or wait for checker outputs to see if it's indeed an expr like `require(...)`
//
// rename: determine a variable for the token (shared with go to definition)
// and go through the current file or all open files for the matching token

use kailua_env::{Pos, Span, ScopedId};
use kailua_syntax::{Str, Name};
use kailua_syntax::lex::{Tok, Punct, NestedToken};
use kailua_syntax::ast::{NameRef, TokenAux, LocalNameKind, Chunk};

use super::last_non_comment;

#[derive(Clone, Debug)]
pub enum Class {
    // the cursor is at (or by) a name token resolving to given variable.
    Var(usize, NameRef),

    // the cursor is at (or by) a string token preceded by a `(` or an identifier,
    // so that it is possibly a function call to `require` and further checking is required.
    PossiblyRequire(usize /*end of prefix expr*/, usize /*str*/, Str),
}

fn is_name_or_str(tok: &NestedToken) -> bool {
    match tok.tok.base { Tok::Str(_) | Tok::Name(_) => true, _ => false }
}

pub fn classify(tokens: &[NestedToken], chunk: &Chunk, pos: Pos) -> Option<Class> {
    // locate a name or str token which entirely include `pos` or share an end point with `pos`.
    // for correctly parsed tokens there may be at most two such tokens. pick the later one.
    // (why later? in order to uniformly handle `require"foo"` and `require "foo"`.)
    let name_idx = match tokens.binary_search_by(|tok| tok.tok.span.begin().cmp(&pos)) {
        Ok(i) => { // tokens[i].begin == pos
            if is_name_or_str(&tokens[i]) {
                Some(i)
            } else if i > 0 && tokens[i-1].tok.span.end() >= pos && is_name_or_str(&tokens[i-1]) {
                Some(i - 1)
            } else {
                None
            }
        },
        Err(0) => { // pos < tokens[0].begin or inf
            None
        },
        Err(i) => { // tokens[i-1].begin < pos < tokens[i].begin or inf
            if pos <= tokens[i-1].tok.span.end() && is_name_or_str(&tokens[i-1]) {
                Some(i - 1)
            } else {
                None
            }
        },
    };

    if let Some(idx) = name_idx {
        match tokens[idx].tok.base {
            Tok::Str(ref s) => {
                if let Some((pidx, ptok)) = last_non_comment(&tokens[..idx]) {
                    match ptok.tok.base {
                        // NAME STR, the prefix expr ends after NAME
                        Tok::Name(_) =>
                            Some(Class::PossiblyRequire(pidx + 1, idx, s.clone())),
                        // `(` STR, the prefix expr ends before `(`
                        Tok::Punct(Punct::LParen) =>
                            Some(Class::PossiblyRequire(pidx, idx, s.clone())),
                        // otherwise it's just a string
                        _ => None,
                    }
                } else {
                    None
                }
            },

            Tok::Name(ref name) => {
                // auxiliary info has a resolution
                match chunk.token_aux[idx] {
                    TokenAux::None => None,
                    TokenAux::LocalVarName(ref id) => {
                        // we need to resolve the actual name if the name has been assumed!
                        let nameref = match chunk.local_names.get(id).map(|def| &def.kind) {
                            Some(&LocalNameKind::AssumedToLocal(ref id)) =>
                                NameRef::Local(id.clone()),
                            Some(&LocalNameKind::AssumedToGlobal) =>
                                NameRef::Global(name.clone()),
                            _ => NameRef::Local(id.clone()),
                        };
                        Some(Class::Var(idx, nameref))
                    },
                    TokenAux::GlobalVarName => {
                        Some(Class::Var(idx, NameRef::Global(name.clone())))
                    },
                }
            },

            _ => None,
        }
    } else {
        None
    }
}

pub fn local_var_definition(last_chunk: &Chunk, scoped_id: &ScopedId) -> Option<Span> {
    last_chunk.local_names.get(scoped_id).map(|def| def.def_span)
}

// this should be called for _each_ chunk in the workspace,
// since definition can occur multiple times throughout the project
pub fn global_var_definition(chunk: &Chunk, name: &Name) -> Option<Span> {
    chunk.global_scope.get(name).cloned()
}

pub fn local_var_uses(tokens: &[NestedToken], chunk: &Chunk, scoped_id: &ScopedId) -> Vec<Span> {
    let mut spans = Vec::new();

    for (tok, aux) in tokens.iter().zip(chunk.token_aux.iter()) {
        let found = match (&tok.tok.base, aux) {
            (&Tok::Name(_), &TokenAux::LocalVarName(ref tok_id)) => {
                match chunk.local_names.get(tok_id).map(|def| &def.kind) {
                    Some(&LocalNameKind::User) => tok_id == scoped_id,
                    Some(&LocalNameKind::AssumedToLocal(ref id)) => id == scoped_id,
                    _ => false,
                }
            },
            (_, _) => false,
        };

        if found {
            spans.push(tok.tok.span);
        }
    }

    spans
}

// this should be called for _each_ chunk in the workspace
pub fn global_var_uses(tokens: &[NestedToken], chunk: &Chunk, name: &Name) -> Vec<Span> {
    let mut spans = Vec::new();

    for (tok, aux) in tokens.iter().zip(chunk.token_aux.iter()) {
        let found = match (&tok.tok.base, aux) {
            (&Tok::Name(ref tok_name), &TokenAux::LocalVarName(ref tok_id)) => {
                match chunk.local_names.get(tok_id).map(|def| &def.kind) {
                    Some(&LocalNameKind::AssumedToGlobal) => tok_name[..] == name[..],
                    _ => false,
                }
            },
            (&Tok::Name(ref tok_name), &TokenAux::GlobalVarName) => tok_name[..] == name[..],
            (_, _) => false,
        };

        if found {
            spans.push(tok.tok.span);
        }
    }

    spans
}

