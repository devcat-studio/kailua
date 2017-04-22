//! An arbitrary mapping from location ranges to values.

use std::fmt;
use std::cmp::Ordering;
use std::collections::{hash_map, HashMap};
use loc::{Unit, Pos, Span, Spanned, span_from_u32};

/// An efficient mapping from spans to values.
//
// segment-point interval tree (self-balanced as like AVL), used to make a mapping
// from the pos/span to a list of overlapping spans associated with arbitrary data.
#[derive(Clone)]
pub struct SpanMap<V> {
    roots: HashMap<Unit, Option<Box<Node<V>>>>,
    size: usize,
}

#[derive(Clone, Debug)]
struct Node<V> {
    // a span associated to the value, low is ordered
    low: u32,
    high: u32,
    // the upper bound of this subtree (so low <= high <= max)
    max: u32,
    // subtree height (>= 1)
    height: u16,
    value: V,
    left: Option<Box<Node<V>>>,
    right: Option<Box<Node<V>>>,
}

impl<V> Node<V> {
    fn new(low: u32, high: u32, value: V) -> Node<V> {
        debug_assert!(low <= high);
        Node { low: low, high: high, max: high, height: 1, value: value, left: None, right: None }
    }

    // called whenever height, low, high, left or right has been updated
    // assumes that subtrees themselves are up to date
    fn update(mut self: Box<Self>) -> Box<Self> {
        let mut max = self.high;
        let mut height = 0;
        if let Some(ref n) = self.left {
            if max < n.max { max = n.max; }
            if height < n.height { height = n.height; }
        }
        if let Some(ref n) = self.right {
            if max < n.max { max = n.max; }
            if height < n.height { height = n.height; }
        }
        self.max = max;
        self.height = height + 1;
        self
    }

    fn skew(&self) -> i32 {
        let mut skew = 0;
        if let Some(ref n) = self.left {
            skew += n.height as i32;
        }
        if let Some(ref n) = self.right {
            skew -= n.height as i32;
        }
        skew
    }

    fn balance(mut self: Box<Self>) -> Box<Self> {
        self.left = self.left.take().map(Node::balance);
        self.right = self.right.take().map(Node::balance);

        let skew = self.skew();
        if skew > 1 { // left heavy
            // check if left subtree is potentially right heavy
            if self.left.as_ref().map_or(false, |n| n.skew() < 0) {
                // left-right rotation
                // ((x b (y c z)) a w) -> ((x b y) c (z a w))
                let mut a = self;
                let mut b = a.left.take().unwrap();
                let mut c = b.right.take().unwrap();
                a.left = c.right.take();
                b.right = c.left.take();
                c.right = Some(a.update());
                c.left = Some(b.update());
                c.update()
            } else {
                // left-left rotation
                // ((x b y) a z) -> (x b (y a z))
                let mut a = self;
                let mut b = a.left.take().unwrap();
                a.left = b.right.take();
                b.right = Some(a.update());
                b.update()
            }
        } else if skew < -1 { // right heavy
            // check if right subtree is potentially left heavy
            if self.right.as_ref().map_or(false, |n| n.skew() > 0) {
                // right-left rotation
                // (x a ((y c z) b w)) -> ((x a y) c (z b w))
                let mut a = self;
                let mut b = a.right.take().unwrap();
                let mut c = b.left.take().unwrap();
                a.right = c.left.take();
                b.left = c.right.take();
                c.left = Some(a.update());
                c.right = Some(b.update());
                c.update()
            } else {
                // right-right rotation
                // (x a (y b c)) -> ((x a y) b c)
                let mut a = self;
                let mut b = a.right.take().unwrap();
                a.right = b.left.take();
                b.left = Some(a.update());
                b.update()
            }
        } else {
            self.update()
        }
    }
}

impl<V> SpanMap<V> {
    pub fn new() -> SpanMap<V> {
        SpanMap { roots: HashMap::new(), size: 0 }
    }

    pub fn len(&self) -> usize {
        self.size
    }

    pub fn insert(&mut self, value: Spanned<V>) -> bool {
        // to keep a stack of trail nodes to update; won't recurse too much anyway
        fn recur<V>(node: Option<Box<Node<V>>>, low: u32, high: u32,
                    value: V) -> (bool, Box<Node<V>>) {
            if let Some(mut node) = node {
                match (node.low, node.high).cmp(&(low, high)) {
                    Ordering::Less => {
                        let (created, right) = recur(node.right.take(), low, high, value);
                        node.right = Some(right);
                        (created, node.update())
                    },
                    Ordering::Greater => {
                        let (created, left) = recur(node.left.take(), low, high, value);
                        node.left = Some(left);
                        (created, node.update())
                    },
                    Ordering::Equal => {
                        node.value = value;
                        (false, node) // no need to update!
                    },
                }
            } else {
                (true, Box::new(Node::new(low, high, value)))
            }
        }

        let Spanned { span, base } = value;

        let rootptr = self.roots.entry(span.unit()).or_insert(None);
        let low = span.begin().to_usize() as u32;
        let high = span.end().to_usize() as u32;

        let (created, root) = recur(rootptr.take(), low, high, base);
        *rootptr = Some(root.balance());
        if created {
            self.size += 1;
        }
        created
    }

    pub fn iter<'a>(&'a self) -> SpannedValues<'a, V> {
        SpannedValues::from(self.roots.iter())
    }

    pub fn contains<'a>(&'a self, pos: Pos) -> Contains<'a, V> {
        let unit = pos.unit();
        if let Some(root) = self.roots.get(&unit) {
            let pos = pos.to_usize() as u32;
            Contains::from_root(unit, root, pos)
        } else {
            Contains::new()
        }
    }

    pub fn overlaps<'a>(&'a self, span: Span) -> Overlaps<'a, V> {
        let unit = span.unit();
        if let Some(root) = self.roots.get(&unit) {
            let begin = span.begin().to_usize() as u32;
            let end = span.end().to_usize() as u32;
            Overlaps::from_root(unit, root, begin, end)
        } else {
            Overlaps::new()
        }
    }

    pub fn adjacencies<'a>(&'a self, span: Span) -> Adjacencies<'a, V> {
        let unit = span.unit();
        if let Some(root) = self.roots.get(&unit) {
            let begin = span.begin().to_usize() as u32;
            let end = span.end().to_usize() as u32;
            Adjacencies::from_root(unit, root, begin, end)
        } else {
            Adjacencies::new()
        }
    }
}

impl<V: fmt::Debug> fmt::Debug for SpanMap<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map().entries(self.iter().map(|x| (x.span, x.base))).finish()
    }
}

impl<'a, V> IntoIterator for &'a SpanMap<V> {
    type Item = Spanned<&'a V>;
    type IntoIter = SpannedValues<'a, V>;
    fn into_iter(self) -> SpannedValues<'a, V> { self.iter() }
}

/// Yields each span in the `SpanMap` with the associated value. The order is unspecified.
pub struct SpannedValues<'a, V: 'a> {
    iter: hash_map::Iter<'a, Unit, Option<Box<Node<V>>>>,
    unit: Unit,
    stack: Vec<&'a Node<V>>,
}

impl<'a, V: 'a> SpannedValues<'a, V> {
    fn from(mut iter: hash_map::Iter<'a, Unit, Option<Box<Node<V>>>>) -> SpannedValues<'a, V> {
        while let Some((&unit, root)) = iter.next() {
            if let Some(ref root) = *root {
                return SpannedValues { iter: iter, unit: unit, stack: vec![root] };
            }
        }

        // there is no value in this map, `unit` won't be used at all
        SpannedValues { iter: iter, unit: Unit::dummy(), stack: Vec::new() }
    }
}

impl<'a, V: 'a> Iterator for SpannedValues<'a, V> {
    type Item = Spanned<&'a V>;

    fn next(&mut self) -> Option<Spanned<&'a V>> {
        loop {
            while let Some(n) = self.stack.pop() {
                if let Some(ref left) = n.left {
                    self.stack.push(left);
                }
                if let Some(ref right) = n.right {
                    self.stack.push(right);
                }
                return Some(Spanned {
                    span: span_from_u32(self.unit, n.low, n.high), base: &n.value,
                });
            }

            if let Some((&unit, root)) = self.iter.next() {
                if let Some(ref root) = *root {
                    self.unit = unit;
                    self.stack.push(root);
                }
            } else {
                return None;
            }
        }
    }
}

macro_rules! impl_traversals {
    ($(
        $(#[$attr:meta])*
        pub traversal $name:ident($($field:ident),*) {
            ubound $overlaps_ubound:expr;
            lbound $overlaps_lbound:expr;
            span $overlaps_span:expr;
        }
    )*) => ($(
        $(#[$attr])*
        pub struct $name<'a, V: 'a> {
            stack: Vec<&'a Node<V>>,
            unit: Unit,
            $($field: u32,)*
        }

        impl<'a, V: 'a> $name<'a, V> {
            fn new() -> $name<'a, V> {
                $name { stack: Vec::new(), unit: Unit::dummy(), $($field: 0,)* }
            }

            fn from_root(unit: Unit, root: &'a Option<Box<Node<V>>>,
                         $($field: u32,)*) -> $name<'a, V> {
                let mut stack = Vec::new();
                if let Some(ref n) = *root {
                    stack.push(&**n);
                }
                $name { stack: stack, unit: unit, $($field: $field,)* }
            }
        }

        impl<'a, V: 'a> Iterator for $name<'a, V> {
            type Item = Spanned<&'a V>;

            fn next(&mut self) -> Option<Spanned<&'a V>> {
                while let Some(n) = self.stack.pop() {
                    // prune if every span in this subtree is left to the query
                    if $overlaps_ubound(self, n.max) {
                        // push next subtrees first...
                        if let Some(ref left) = n.left {
                            self.stack.push(left);
                        }
                        if let Some(ref right) = n.right {
                            // prune if every span in the right subtree is right to the query
                            // this is possible because node's low is ordered (but high is not)
                            if $overlaps_lbound(n.low, self) {
                                self.stack.push(right);
                            }
                        }

                        // ...then return the current node if needed
                        if $overlaps_span(n.low, self, n.high) {
                            return Some(Spanned {
                                span: span_from_u32(self.unit, n.low, n.high), base: &n.value,
                            });
                        }
                    }
                }

                None
            }
        }
    )*)
}

impl_traversals! {
    /// Yields each span in the `SpanMap` containing given position, with the associated value.
    /// The order is unspecified.
    pub traversal Contains(pos) {
        ubound |it: &Self, high| it.pos < high;
        lbound |low, it: &Self| low <= it.pos;
        span |low, it: &Self, high| low <= it.pos && it.pos < high;
    }

    /// Yields each span in the `SpanMap` that has a non-empty intersection with given span,
    /// with the associated value. The order is unspecified.
    pub traversal Overlaps(low, high) { // high is exclusive
        ubound |it: &Self, high| it.low < high;
        lbound |low, it: &Self| low < it.high;
        span |low, it: &Self, high| low < it.high && it.low < high;
    }

    /// Yields each span in the `SpanMap` that has a (possibly empty) intersection with given span,
    /// with the associated value. The order is unspecified.
    pub traversal Adjacencies(low, high) { // high is inclusive
        ubound |it: &Self, high| it.low <= high;
        lbound |low, it: &Self| low <= it.high;
        span |low, it: &Self, high| low <= it.high && it.low <= high;
    }
}

#[test]
fn test_spanmap() {
    use loc::{unit_from_u32, pos_from_u32, span_from_u32, WithLoc};

    let unit = unit_from_u32(1);
    let pos = |pos| pos_from_u32(unit, pos);
    let span = |lo, hi| span_from_u32(unit, lo, hi);

    let posx = |unit, pos| pos_from_u32(unit_from_u32(unit), pos);
    let spanx = |unit, lo, hi| span_from_u32(unit_from_u32(unit), lo, hi);

    let mut map = SpanMap::new();
    assert!(map.insert(1.with_loc(span(1, 8))));
    assert!(map.insert(2.with_loc(span(2, 3))));
    assert!(map.insert(3.with_loc(span(4, 12))));
    assert!(map.insert(4.with_loc(span(3, 7))));
    assert!(!map.insert(5.with_loc(span(2, 3)))); // update
    assert!(map.insert(6.with_loc(span(4, 6))));
    assert_eq!(map.len(), 5);

    macro_rules! sorted {
        ($it:expr) => ({
            let mut vec: Vec<_> = $it.map(|v| (v.span, v.base)).collect();
            vec.sort_by_key(|&(span, _)| {
                (span.unit(), span.begin().to_usize(), span.end().to_usize())
            });
            vec
        })
    };

    assert_eq!(sorted!(map.contains(pos(0))), []);
    assert_eq!(sorted!(map.contains(pos(1))), [(span(1, 8), &1)]);
    assert_eq!(sorted!(map.contains(pos(2))), [(span(1, 8), &1), (span(2, 3), &5)]);
    assert_eq!(sorted!(map.contains(pos(3))), [(span(1, 8), &1), (span(3, 7), &4)]);
    assert_eq!(sorted!(map.contains(pos(4))), [(span(1, 8), &1), (span(3, 7), &4),
                                               (span(4, 6), &6), (span(4, 12), &3)]);
    assert_eq!(sorted!(map.contains(pos(5))), [(span(1, 8), &1), (span(3, 7), &4),
                                               (span(4, 6), &6), (span(4, 12), &3)]);
    assert_eq!(sorted!(map.contains(pos(6))), [(span(1, 8), &1), (span(3, 7), &4),
                                               (span(4, 12), &3)]);
    assert_eq!(sorted!(map.contains(pos(7))), [(span(1, 8), &1), (span(4, 12), &3)]);
    assert_eq!(sorted!(map.contains(pos(8))), [(span(4, 12), &3)]);
    assert_eq!(sorted!(map.contains(pos(11))), [(span(4, 12), &3)]);
    assert_eq!(sorted!(map.contains(pos(12))), []);
    assert_eq!(sorted!(map.contains(posx(2, 5))), []);

    assert_eq!(sorted!(map.overlaps(span(3, 5))), [(span(1, 8), &1), (span(3, 7), &4),
                                                   (span(4, 6), &6), (span(4, 12), &3)]);
    assert_eq!(sorted!(map.overlaps(span(7, 7))), [(span(1, 8), &1), (span(4, 12), &3)]);
    assert_eq!(sorted!(map.overlaps(span(7, 8))), [(span(1, 8), &1), (span(4, 12), &3)]);
    assert_eq!(sorted!(map.overlaps(span(8, 8))), [(span(4, 12), &3)]);
    assert_eq!(sorted!(map.overlaps(span(12, 14))), []);
    assert_eq!(sorted!(map.overlaps(spanx(2, 3, 5))), []);

    assert_eq!(sorted!(map.adjacencies(span(3, 5))), [(span(1, 8), &1), (span(2, 3), &5),
                                                      (span(3, 7), &4), (span(4, 6), &6),
                                                      (span(4, 12), &3)]);
    assert_eq!(sorted!(map.adjacencies(span(7, 7))), [(span(1, 8), &1), (span(3, 7), &4),
                                                      (span(4, 12), &3)]);
    assert_eq!(sorted!(map.adjacencies(span(7, 8))), [(span(1, 8), &1), (span(3, 7), &4),
                                                      (span(4, 12), &3)]);
    assert_eq!(sorted!(map.adjacencies(span(8, 8))), [(span(1, 8), &1), (span(4, 12), &3)]);
    assert_eq!(sorted!(map.adjacencies(span(12, 14))), [(span(4, 12), &3)]);
    assert_eq!(sorted!(map.adjacencies(spanx(2, 3, 5))), []);

    // we are very sure that the mapping is real-time
    assert!(map.insert(7.with_loc(span(8, 12))));

    assert_eq!(sorted!(map.contains(pos(7))), [(span(1, 8), &1), (span(4, 12), &3)]);
    assert_eq!(sorted!(map.contains(pos(8))), [(span(4, 12), &3), (span(8, 12), &7)]);
    assert_eq!(sorted!(map.contains(pos(11))), [(span(4, 12), &3), (span(8, 12), &7)]);
    assert_eq!(sorted!(map.contains(pos(12))), []);

    assert_eq!(sorted!(map.overlaps(span(7, 7))), [(span(1, 8), &1), (span(4, 12), &3)]);
    assert_eq!(sorted!(map.overlaps(span(7, 8))), [(span(1, 8), &1), (span(4, 12), &3)]);
    // this still does not include [8, 12) because the query is an empty span
    assert_eq!(sorted!(map.overlaps(span(8, 8))), [(span(4, 12), &3)]);
    assert_eq!(sorted!(map.overlaps(span(8, 9))), [(span(4, 12), &3), (span(8, 12), &7)]);

    assert_eq!(sorted!(map.adjacencies(span(7, 7))), [(span(1, 8), &1), (span(3, 7), &4),
                                                      (span(4, 12), &3)]);
    assert_eq!(sorted!(map.adjacencies(span(7, 8))), [(span(1, 8), &1), (span(3, 7), &4),
                                                      (span(4, 12), &3), (span(8, 12), &7)]);
    assert_eq!(sorted!(map.adjacencies(span(8, 8))), [(span(1, 8), &1), (span(4, 12), &3),
                                                      (span(8, 12), &7)]);

    // and the empty span can be assigned
    assert!(map.insert((-1).with_loc(span(5, 5))));
    assert!(map.insert(9.with_loc(span(9, 9))));
    assert!(!map.insert(8.with_loc(span(5, 5))));

    assert_eq!(sorted!(map.contains(pos(5))), [(span(1, 8), &1), (span(3, 7), &4),
                                               (span(4, 6), &6), (span(4, 12), &3)]);
    assert_eq!(sorted!(map.contains(pos(9))), [(span(4, 12), &3), (span(8, 12), &7)]);

    assert_eq!(sorted!(map.overlaps(span(8, 9))), [(span(4, 12), &3), (span(8, 12), &7)]);
    assert_eq!(sorted!(map.overlaps(span(9, 9))), [(span(4, 12), &3), (span(8, 12), &7)]);
    // this still does not include [9, 9) because the target is an empty span
    assert_eq!(sorted!(map.overlaps(span(9, 10))), [(span(4, 12), &3), (span(8, 12), &7)]);
    // but this one does
    assert_eq!(sorted!(map.overlaps(span(8, 10))), [(span(4, 12), &3), (span(8, 12), &7),
                                                    (span(9, 9), &9)]);

    assert_eq!(sorted!(map.adjacencies(span(8, 9))), [(span(1, 8), &1), (span(4, 12), &3),
                                                      (span(8, 12), &7), (span(9, 9), &9)]);
    assert_eq!(sorted!(map.adjacencies(span(9, 9))), [(span(4, 12), &3), (span(8, 12), &7),
                                                      (span(9, 9), &9)]);
    assert_eq!(sorted!(map.adjacencies(span(9, 10))), [(span(4, 12), &3), (span(8, 12), &7),
                                                       (span(9, 9), &9)]);

    // multiple different units can be in the place
    assert!(map.insert(10.with_loc(spanx(2, 3, 7))));
    assert!(map.insert((-3).with_loc(spanx(3, 2, 3))));
    assert!(!map.insert((-2).with_loc(spanx(2, 3, 7))));
    assert!(map.insert((-4).with_loc(spanx(2, 3, 8))));
    assert!(map.insert((-5).with_loc(spanx(3, 1, 3))));

    assert_eq!(sorted!(map.contains(pos(5))), [(span(1, 8), &1), (span(3, 7), &4),
                                               (span(4, 6), &6), (span(4, 12), &3)]);
    assert_eq!(sorted!(map.contains(posx(2, 5))), [(spanx(2, 3, 7), &-2), (spanx(2, 3, 8), &-4)]);
    assert_eq!(sorted!(map.contains(posx(3, 2))), [(spanx(3, 1, 3), &-5), (spanx(3, 2, 3), &-3)]);
    assert_eq!(sorted!(map.contains(posx(3, 3))), []);

    assert_eq!(sorted!(map.overlaps(span(3, 5))), [(span(1, 8), &1), (span(3, 7), &4),
                                                   (span(4, 6), &6), (span(4, 12), &3)]);
    assert_eq!(sorted!(map.overlaps(spanx(2, 3, 5))), [(spanx(2, 3, 7), &-2),
                                                       (spanx(2, 3, 8), &-4)]);
    assert_eq!(sorted!(map.overlaps(spanx(3, 3, 5))), []);

    assert_eq!(sorted!(map.adjacencies(span(3, 5))), [(span(1, 8), &1), (span(2, 3), &5),
                                                      (span(3, 7), &4), (span(4, 6), &6),
                                                      (span(4, 12), &3), (span(5, 5), &8)]);
    assert_eq!(sorted!(map.adjacencies(spanx(2, 3, 5))), [(spanx(2, 3, 7), &-2),
                                                          (spanx(2, 3, 8), &-4)]);
    assert_eq!(sorted!(map.adjacencies(spanx(3, 3, 5))), [(spanx(3, 1, 3), &-5),
                                                          (spanx(3, 2, 3), &-3)]);

    assert_eq!(sorted!(map.iter()),
               [(span(1, 8), &1), (span(2, 3), &5), (span(3, 7), &4), (span(4, 6), &6),
                (span(4, 12), &3), (span(5, 5), &8), (span(8, 12), &7), (span(9, 9), &9),
                (spanx(2, 3, 7), &-2), (spanx(2, 3, 8), &-4),
                (spanx(3, 1, 3), &-5), (spanx(3, 2, 3), &-3)]);
    assert_eq!(map.len(), 12);
}

