use kailua_env::Span;
use kailua_syntax::lex::{Tok, NestedToken};
use kailua_types::ty::Slot;
use kailua_check::env::Output;

pub mod completion;
pub mod hover;
pub mod signature;
pub mod definition; // also contains rename

// common routines

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

