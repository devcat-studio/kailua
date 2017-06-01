// hover help: wait for checker outputs and get the informations from given position

use std::sync::Arc;
use std::collections::HashSet;

use kailua_env::{Pos, Source};
use kailua_diag::{Localize, Localized};
use kailua_types::ty::{TypeContext, Display};
use kailua_check::env::Output;

use diags;
use protocol::*;

pub fn help<F>(outputs: &[Arc<Output>], pos: Pos, source: &Source, mut localize: F) -> Option<Hover>
    where F: for<'a> FnMut(&'a Localize) -> Localized<'a, Localize>
{
    // for multiple outputs, we deduplicate the identical types
    let mut hover_range = None;
    let mut contents = Vec::new();
    let mut seen = HashSet::new();

    for output in outputs {
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
            let value = format!("{}{:0.1}", TYPE_PREFIX, localize(&slot.display(types)));
            if seen.insert(value.clone()) {
                // XXX currently this can differ throughout the outputs
                hover_range = Some(range);
                contents.push(MarkedString { language: format!("lua"), value: value });
            }
        }
    }

    hover_range.map(|range| Hover { contents: contents, range: range })
}

