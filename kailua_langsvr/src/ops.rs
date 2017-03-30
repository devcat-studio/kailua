use std::cmp::Ordering;
use std::collections::HashSet;
use std::sync::Arc;

use kailua_env::{Pos, Span, Source};
use kailua_diag::{Localize, Localized};
use kailua_syntax::{Tok, Punct, Keyword, NestedToken, NestingCategory, Chunk};
use kailua_check::{TypeContext, Display, Output, Key, Nil, Slot, Functions};

use diags;
use protocol::*;
use message as m;

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
enum PrefixExprSlot {
    NotFound,
    Found(usize /*the last non-comment token index for prefix expr*/, Slot),
}

fn last_non_comment(tokens: &[NestedToken]) -> Option<(usize, &NestedToken)> {
    tokens.iter().enumerate().rev().find(|&(_, tok)| {
        match tok.tok.base { Tok::Comment => false, _ => true }
    })
}

// get a slot for a prefix expression which is bounded by tokens[..idx], if possible.
// may return None if the preceding tokens do not look like a prefix expression at all.
fn get_prefix_expr_slot(tokens: &[NestedToken], idx: usize,
                        output: &Output) -> Option<PrefixExprSlot> {
    // note that this approach of using the closest non-comment token's end is
    // prone to syntax error; while completing `f"":` should work,
    // `"":` (invalid, should have been `(""):`) will also work.
    // as such a mistake can be readily reported, however, we don't try to perfect the approach.
    let (end_idx, end) = if let Some((idx, tok)) = last_non_comment(&tokens[..idx]) {
        (idx, tok.tok.span.end())
    } else {
        return None;
    };

    // find all slot-associated spans that intersects (even at the end points) the end pos...
    let spans = output.spanned_slots().adjacencies(Span::from(end));
    // ...and keep spans which actually _ends_ at the end pos...
    let spans_before = spans.filter(|slot| slot.span.end() == end);
    // ...and pick the smallest one among them (there should be at most one such span).
    let closest_slot = spans_before.min_by_key(|slot| slot.span.len());

    if let Some(slot) = closest_slot {
        Some(PrefixExprSlot::Found(end_idx, slot.base.clone()))
    } else {
        Some(PrefixExprSlot::NotFound)
    }
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

// completions: classify whether it's a name completion or a field completion,
// then wait for parser/checker outputs to actually gather the informations

#[derive(Clone, Debug)]
pub enum CompletionClass {
    // complete a top-level name or keyword.
    // the caret is located inside tokens[i] or on the intersection of tokens[i-1] and tokens[i].
    // in the former case tokens[i] should be a name; in the latter case tokens[i-1] may not exist.
    Name(usize, NestingCategory),

    // complete a field or method after `.` or `:`.
    // the caret is located after tokens[i] (which should be `.` or `:`).
    Field(usize),
}

pub fn classify_completion(tokens: &[NestedToken], pos: Pos) -> Option<CompletionClass> {
    let (idx, end, after) = index_and_neighbor(tokens, pos, |tok| tok.tok.span);

    let ptok = if idx > 0 { tokens.get(idx - 1) } else { None };
    let tok = tokens.get(idx);
    match (end, after, ptok.map(|tok| &tok.tok.base), tok.map(|tok| &tok.tok.base)) {
        // ... `.` | ...
        // ... `:` | ...
        (true, true, _, Some(&Tok::Punct(Punct::Dot))) |
        (true, true, _, Some(&Tok::Punct(Punct::Colon))) => {
            Some(CompletionClass::Field(idx))
        },

        // ... `.` NAM|E ...
        // ... `:` NAM|E ...
        // ... `.` NAME | ... (with no space between NAME and the caret)
        // ... `:` NAME | ...
        (_, true, Some(&Tok::Punct(Punct::Dot)), Some(&Tok::Name(_))) |
        (_, true, Some(&Tok::Punct(Punct::Dot)), Some(&Tok::Keyword(_))) |
        (_, true, Some(&Tok::Punct(Punct::Colon)), Some(&Tok::Name(_))) |
        (_, true, Some(&Tok::Punct(Punct::Colon)), Some(&Tok::Keyword(_))) => {
            Some(CompletionClass::Field(idx - 1)) // should point to `ptok`
        },

        // ... NAME | ... (ditto)
        (_, true, _, Some(&Tok::Name(_))) |
        (_, true, _, Some(&Tok::Keyword(_))) => {
            Some(CompletionClass::Name(idx, tok.unwrap().category))
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
                let detail =
                    last_chunk.decl_spans.get(&id).and_then(|&s| detail_from_span(s, source));
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

// hover help: wait for checker outputs and get the informations from given position

pub fn hover_help<F>(output: &Output, pos: Pos, source: &Source, mut localize: F) -> Option<Hover>
    where F: for<'a> FnMut(&'a Localize) -> Localized<'a, Localize>
{
    // find all slot-associated spans that contains the pos...
    let spans = output.spanned_slots().contains(pos);
    // ...and pick the smallest one among them (there should be at most one such span).
    let closest_slot = spans.min_by_key(|slot| slot.span.len());

    // format the slot if available
    if let Some(slot) = closest_slot {
        // the resulting output should be colorized as if it's in `--:`.
        // in order to use a single syntax, we use a sequence of random invisible
        // characters to "trick" the colorizer.
        const TYPE_PREFIX: &'static str =
            "\u{200c}\u{200d}\u{200d}\u{200c}\u{2060}\u{200c}\u{200b}\u{200d}\
             \u{200c}\u{200c}\u{200d}\u{200b}\u{2060}\u{200d}\u{2060}\u{2060}";

        let range = diags::translate_span(slot.span, source).map(|(_, range)| range);
        let types = output.types() as &TypeContext;
        Some(Hover {
            contents: vec![
                MarkedString {
                    language: format!("lua"),
                    value: format!("{}{}", TYPE_PREFIX, localize(&slot.display(types))),
                }
            ],
            range: range,
        })
    } else {
        None
    }
}

// signature help: resolve what is the closest function expression for the possible call,
// then wait for checker outputs to actually return the signature

#[derive(Clone, Debug)]
pub struct SignatureLoc {
    pub args_token_idx: usize,
    pub arg_idx: usize,
}

pub fn locate_signature(tokens: &[NestedToken], pos: Pos) -> Option<SignatureLoc> {
    enclosing_func_call(tokens, pos).map(|(token_idx, arg_idx)| {
        SignatureLoc { args_token_idx: token_idx, arg_idx: arg_idx }
    })
}

pub fn signature_help<F>(tokens: &[NestedToken], loc: &SignatureLoc, output: &Output,
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

