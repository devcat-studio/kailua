use kailua_env::{Span, Pos};
use kailua_syntax::lex::{Tok, NestedToken};
use kailua_types::ty::Slot;
use kailua_check::env::Output;

pub mod completion;
pub mod hover;
pub mod signature;
pub mod definition; // also contains rename

// common routines

fn last_non_comment(tokens: &[NestedToken]) -> Option<(usize, &NestedToken)> {
    tokens.iter().enumerate().rev().find(|&(_, tok)| {
        match tok.tok.base { Tok::Comment => false, _ => true }
    })
}

// get a slot for a prefix expression which is bounded by (exclusive) end position, if possible.
fn get_prefix_expr_slot(end: Pos, output: &Output) -> Option<Slot> {
    // find all slot-associated spans that intersects (even at the end points) the end pos...
    let spans = output.spanned_slots().adjacencies(Span::from(end));
    // ...and keep spans which actually _ends_ at the end pos...
    let spans_before = spans.filter(|slot| slot.span.end() == end);
    // ...and pick the smallest one among them (there should be at most one such span).
    let closest_slot = spans_before.min_by_key(|slot| slot.span.len());

    closest_slot.map(|slot| slot.base.clone())
}

