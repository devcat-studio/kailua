// completions: classify whether it's a name completion or a field completion,
// then wait for parser/checker outputs to actually gather the informations

use std::cmp::Ordering;
use std::collections::HashSet;
use std::sync::Arc;

use kailua_env::{Pos, Span, Source};
use kailua_syntax::lex::{Tok, Punct, Keyword, NestedToken, NestingCategory};
use kailua_syntax::ast::Chunk;
use kailua_check::{Output, Key};

use protocol::*;
use super::{get_prefix_expr_slot, PrefixExprSlot};

fn index_and_neighbor<T, F>(tokens: &[T], pos: Pos, as_span: F) -> (usize, bool, bool)
    where F: Fn(&T) -> Span
{
    //        pos
    // ________v________   idx  end   after
    //     BOF^  0000000   0    false false
    //     BOF^  $EOF      0    false false
    //     BOF^$EOF        0    false false
    // iiiiiiii  jjjjjjj   i    true  true
    // iiiiiiiijjjjjjjjj   i    true  true
    // iiiiii    jjjjjjj   i    false false
    // iiiiii  jjjjjjjjj   j    false false
    // iiii   jjjjjj kkk   j    false true
    // iii jjjjj   kkkkk   j    false true
    // ii jjjjj   kkkkkk   j    true  true
    // zzzzzzzz   $EOF     z    true  true
    // zzzz    $EOF        z    false false
    match tokens.binary_search_by(|tok| as_span(tok).begin().cmp(&pos)) {
        Ok(i) => { // tokens[i].begin == pos
            if i > 0 && as_span(&tokens[i-1]).end() == pos {
                (i-1, true, true)
            } else {
                (i, false, false)
            }
        },
        Err(0) => { // pos < tokens[0].begin or inf
            (0, false, false)
        },
        Err(i) => { // tokens[i-1].begin < pos < tokens[i].begin or inf
            match pos.cmp(&as_span(&tokens[i-1]).end()) {
                Ordering::Less => (i-1, false, true),
                Ordering::Equal => (i-1, true, true),
                Ordering::Greater => (i-1, false, false),
            }
        },
    }
}

#[test]
fn test_index_and_neighbor() {
    use kailua_env::SourceFile;

    // we need a sizable span to construct a dummy list of "tokens" (solely represented by spans)
    let mut source = Source::new();
    let span = source.add(SourceFile::from_u8("dummy".to_string(), b"0123456789"[..].to_owned()));
    let pos = |i| span.clone().nth(i).unwrap();

    let tokens = [Span::new(pos(1), pos(2)), Span::new(pos(2), pos(4)),
                  Span::new(pos(5), pos(7)), Span::new(pos(8), pos(8))];
    assert_eq!(index_and_neighbor(&tokens, pos(0), |&sp| sp), (0, false, false));
    assert_eq!(index_and_neighbor(&tokens, pos(1), |&sp| sp), (0, false, false));
    assert_eq!(index_and_neighbor(&tokens, pos(2), |&sp| sp), (0, true, true));
    assert_eq!(index_and_neighbor(&tokens, pos(3), |&sp| sp), (1, false, true));
    assert_eq!(index_and_neighbor(&tokens, pos(4), |&sp| sp), (1, true, true));
    assert_eq!(index_and_neighbor(&tokens, pos(5), |&sp| sp), (2, false, false));
    assert_eq!(index_and_neighbor(&tokens, pos(6), |&sp| sp), (2, false, true));
    assert_eq!(index_and_neighbor(&tokens, pos(7), |&sp| sp), (2, true, true));
    assert_eq!(index_and_neighbor(&tokens, pos(8), |&sp| sp), (3, false, false));
    assert_eq!(index_and_neighbor(&tokens, pos(9), |&sp| sp), (3, false, false));

    let tokens = [Span::new(pos(1), pos(2)), Span::new(pos(2), pos(4)),
                  Span::new(pos(5), pos(8)), Span::new(pos(8), pos(8))];
    assert_eq!(index_and_neighbor(&tokens, pos(7), |&sp| sp), (2, false, true));
    assert_eq!(index_and_neighbor(&tokens, pos(8), |&sp| sp), (2, true, true));
    assert_eq!(index_and_neighbor(&tokens, pos(9), |&sp| sp), (3, false, false));
}

fn make_item(label: String, kind: CompletionItemKind, detail: Option<String>) -> CompletionItem {
    CompletionItem {
        label: label,
        kind: Some(kind),
        detail: detail,
        documentation: None,
        sortText: None,
        filterText: None,
        insertText: None,
        textEdit: None,
        additionalTextEdits: Vec::new(),
        command: None,
        data: None,
    }
}

// for the interactivity, the lookbehind is limited to a reasonable number
const LOOKBEHIND_LIMIT: usize = 4096;

// check if the caret is located in regions where the autocompletion should be disabled:
//
// 1. `local NAME ... | ... [= ...]`
// 2. `for NAME ... | ... = ... do ... end`
// 3. `function NAME ... | ( ... )`
// 4. `function [NAME ...] ( ... | ... )`
//
// the check for 1 and 2 is handled by looking backward for the first token
// that is not a comment, a name or a comma and is in the same nesting as the caret.
// if the token exists and it's `local` or `for`, autocompletion is disabled.
//
// the check for 3 is handled by looking backward for the first token
// that is not a comment, a name, a dot or a colon and is in the same nesting as the caret.
// if the token exists and it's `function`, autocompletion is disabled.
//
// the check for 4 is handled similarly to the check for 1 and 2,
// but once seen a `(` token, it will switch to the check for 3 at the parent nesting.
fn is_name_completion_disabled(tokens: &[NestedToken], name_idx: usize) -> bool {
    let name_tok = &tokens[name_idx];
    let mut init_depth = name_tok.depth;
    let init_serial = name_tok.serial;

    let mut name_decl_possible = true; // case 1, 2 and 4a
    let mut func_sig_possible = true; // case 3 and 4b
    for (i, tok) in tokens[..name_idx].iter().enumerate().rev().take(LOOKBEHIND_LIMIT) {
        if !(name_decl_possible || func_sig_possible) { break; }

        if tok.depth <= init_depth && tok.serial != init_serial {
            // escaped the current nesting, stop the search
            return false;
        } else if tok.depth > init_depth {
            // ignore more nested tokens (but count them towards the threshold)
            continue;
        }

        // name_decl_possible can continue to func_sig_possible in place, so this should be first
        if func_sig_possible {
            match tok.tok.base {
                Tok::Comment |
                Tok::Name(_) |
                Tok::Punct(Punct::Dot) |
                Tok::Punct(Punct::Colon) => {},

                Tok::Keyword(Keyword::Function) => return true,

                _ => func_sig_possible = false,
            }
        }

        if name_decl_possible {
            match tok.tok.base {
                Tok::Comment |
                Tok::Name(_) |
                Tok::Punct(Punct::Comma) |
                // Newline to account for meta comments (other tokens are nested)
                Tok::Punct(Punct::Newline) => {},

                Tok::Punct(Punct::LParen) => {
                    // `function ... ( ... | ... )` is possible
                    // update the initial nesting to point to a token before `(` and proceed
                    if i == 0 { return false; }
                    init_depth = tokens[i-1].depth;
                    name_decl_possible = false;
                    func_sig_possible = true;
                }

                Tok::Keyword(Keyword::Local) |
                Tok::Keyword(Keyword::For) => return true,

                _ => name_decl_possible = false,
            }
        }
    }

    false
}

const EXPR_KEYWORDS: &'static [&'static str] = &[
    "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "goto", "if",
    "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while",
];
const META_KEYWORDS: &'static [&'static str] = &[
    "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "goto", "if",
    "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while",
    "assume", "const", "global", "map", "module", "once", "open", "type", "var", "vector",
];

fn keywords_per_category(nesting_category: NestingCategory) -> &'static [&'static str] {
    match nesting_category {
        NestingCategory::Expr => EXPR_KEYWORDS,
        NestingCategory::Meta => META_KEYWORDS,
    }
}

fn detail_from_span(span: Span, source: &Source) -> Option<String> {
    let begin = span.begin(); // span.end() won't be in the different line, probably
    if let Some(file) = source.file(begin.unit()) {
        // XXX the file names can collide
        let path = file.path().split(|c| c == '\\' || c == '/').last().unwrap_or("");
        if let Some((line, _)) = file.line_from_pos(begin) {
            Some(format!("{}:{}", path, line + 1))
        } else {
            Some(format!("{}", path))
        }
    } else {
        None
    }
}

#[derive(Clone, Debug)]
pub enum Class {
    // complete a top-level name or keyword.
    // the caret is located inside tokens[i] or on the intersection of tokens[i-1] and tokens[i].
    // in the former case tokens[i] should be a name; in the latter case tokens[i-1] may not exist.
    Name(usize, NestingCategory),

    // complete a field or method after `.` or `:`.
    // the caret is located after tokens[i] (which should be `.` or `:`).
    Field(usize),
}

pub fn classify(tokens: &[NestedToken], pos: Pos) -> Option<Class> {
    let (idx, end, after) = index_and_neighbor(tokens, pos, |tok| tok.tok.span);

    let ptok = if idx > 0 { tokens.get(idx - 1) } else { None };
    let tok = tokens.get(idx);
    match (end, after, ptok.map(|tok| &tok.tok.base), tok.map(|tok| &tok.tok.base)) {
        // ... `.` | ...
        // ... `:` | ...
        (true, true, _, Some(&Tok::Punct(Punct::Dot))) |
        (true, true, _, Some(&Tok::Punct(Punct::Colon))) => {
            Some(Class::Field(idx))
        },

        // ... `.` NAM|E ...
        // ... `:` NAM|E ...
        // ... `.` NAME | ... (with no space between NAME and the caret)
        // ... `:` NAME | ...
        (_, true, Some(&Tok::Punct(Punct::Dot)), Some(&Tok::Name(_))) |
        (_, true, Some(&Tok::Punct(Punct::Dot)), Some(&Tok::Keyword(_))) |
        (_, true, Some(&Tok::Punct(Punct::Colon)), Some(&Tok::Name(_))) |
        (_, true, Some(&Tok::Punct(Punct::Colon)), Some(&Tok::Keyword(_))) => {
            Some(Class::Field(idx - 1)) // should point to `ptok`
        },

        // ... NAME | ... (ditto)
        (_, true, _, Some(&Tok::Name(_))) |
        (_, true, _, Some(&Tok::Keyword(_))) => {
            Some(Class::Name(idx, tok.unwrap().category))
        },

        _ => None,
    }
}

pub fn complete_name(tokens: &[NestedToken], name_idx: usize, nesting_category: NestingCategory,
                     pos: Pos, last_chunk: &Chunk, all_chunks: &[Arc<Chunk>],
                     source: &Source) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // check if the caret is at the name definition and autocompletion should be disabled
    if nesting_category == NestingCategory::Expr && is_name_completion_disabled(tokens, name_idx) {
        return items;
    }

    // if the current word being typed matches exactly a keyword, we temporarily pause
    // the completion to avoid capturing the carriage return from the completion. (XXX suboptimal)
    let name_token = &tokens[name_idx].tok;
    if name_token.span.end() == pos {
        if let Tok::Keyword(_) = name_token.base {
            return items;
        }
    }

    let mut seen = HashSet::new();

    if let Some(scope) = last_chunk.map.scope_from_pos(pos) {
        for (name, _scope, id) in last_chunk.map.names_and_scopes(scope) {
            if seen.insert(name) { // ignore shadowed names (always appear later)
                let name = String::from_utf8_lossy(name).into_owned();
                let detail = last_chunk.local_names.get(&id).and_then(|def| {
                    detail_from_span(def.def_span, source)
                });
                items.push(make_item(name, CompletionItemKind::Variable, detail));
            }
        }
    }

    for chunk in all_chunks {
        for (name, &span) in chunk.global_scope.iter() {
            if seen.insert(name) {
                let name = String::from_utf8_lossy(name).into_owned();
                let detail = detail_from_span(span, source);
                items.push(make_item(name, CompletionItemKind::Variable, detail));
            }
        }
    }

    for keyword in keywords_per_category(nesting_category) {
        items.push(make_item(keyword[..].to_owned(), CompletionItemKind::Keyword, None));
    }

    items
}

pub fn complete_field(tokens: &[NestedToken], sep_idx: usize,
                      output: &Output) -> Option<Vec<CompletionItem>> {
    let res = get_prefix_expr_slot(tokens, sep_idx, output);
    debug!("complete_field: get_prefix_expr_slot returns {:?}", res);

    match res {
        // fail fast, this is not a prefix expression
        None => Some(Vec::new()),

        // we may retry for the newer output if there is no slot available
        Some(PrefixExprSlot::NotFound) => None,

        // now we've got the closest slot for given position;
        // check if it's actually a table or similar (if it's not, we will fail fast)
        Some(PrefixExprSlot::Found(_, slot)) => {
            let mut items = Vec::new();
            if let Some(fields) = output.get_available_fields(&slot.unlift()) {
                for (k, _v) in fields {
                    if let Key::Str(ref s) = k {
                        let name = String::from_utf8_lossy(&s).into_owned();
                        items.push(make_item(name, CompletionItemKind::Field, None));
                    }
                }
            }
            Some(items)
        },
    }
}


