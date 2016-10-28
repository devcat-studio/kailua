use std::fmt;
use std::slice;
use std::hash::Hash;
use std::borrow::Borrow;
use std::collections::HashMap;
use loc::{Unit, Pos, Span};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Scope {
    // unlike the unit, scope #0 is reserved and never instantiated.
    // it is reserved to allow more efficient operations with scopes though.
    scope: u32,
}

impl Scope {
    pub fn to_usize(&self) -> usize {
        self.scope as usize
    }
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.scope)
    }
}

pub struct AncestorScopes<'a, Name: 'a> {
    scopes: &'a [ScopeItem<Name>],
    current: u32,
    done: bool,
}

impl<'a, Name: 'a> Iterator for AncestorScopes<'a, Name> {
    type Item = Scope;

    fn next(&mut self) -> Option<Scope> {
        if self.done {
            None
        } else {
            let current = self.current;
            let parent = self.scopes[current as usize].parent;
            debug_assert!(parent <= current, "unordered scope list in ScopeMap::names");
            self.done = parent == current;
            self.current = parent;
            Some(Scope { scope: current })
        }
    }
}

pub struct NamesAndScopes<'a, Name: 'a> {
    scopes: &'a [ScopeItem<Name>],
    current: u32,
    iter: Option<slice::Iter<'a, Name>>,
}

impl<'a, Name: 'a> Iterator for NamesAndScopes<'a, Name> {
    type Item = (&'a Name, Scope);

    fn next(&mut self) -> Option<(&'a Name, Scope)> {
        loop {
            if let Some(ref mut iter) = self.iter {
                if let Some(name) = iter.next() {
                    return Some((name, Scope { scope: self.current }));
                }
            } else {
                return None;
            }

            let current = self.current;
            self.current = self.scopes[current as usize].parent;
            if current == self.current {
                self.iter = None;
            } else {
                self.iter = Some(self.scopes[self.current as usize].names.iter());
            }
        }
    }
}

struct ScopeItem<Name> {
    // same to itself when the scope is root (not necessarily global)
    parent: u32,

    // associated span, can be dummy but later set
    span: Span,

    // a list of immediately contained names
    names: Vec<Name>,
}

impl<Name> ScopeItem<Name> {
    fn new(parent: u32) -> ScopeItem<Name> {
        ScopeItem { parent: parent, span: Span::dummy(), names: Vec::new() }
    }
}

pub struct ScopeMap<Name> {
    // an implicit mapping from the scope to the item
    scopes: Vec<ScopeItem<Name>>,

    // names and sorted lists of containing scopes
    names: HashMap<Name, Vec<Scope>>,

    // a cache of ranges for mapping the location to the scope
    // the scope can be 0 (empty); for multiple scopes the scope is set arbitrarily
    span_ranges: HashMap<Unit, Vec<(u32, u32)>>,
}

impl<Name: Hash + Eq> ScopeMap<Name> {
    pub fn new() -> ScopeMap<Name> {
        ScopeMap {
            scopes: vec![ScopeItem::new(0)],
            names: HashMap::new(),
            span_ranges: HashMap::new(),
        }
    }

    pub fn generate_root(&mut self) -> Scope {
        let scope = self.scopes.len() as u32;
        self.scopes.push(ScopeItem::new(scope));
        Scope { scope: scope }
    }

    pub fn generate(&mut self, parent: Scope) -> Scope {
        let scope = self.scopes.len() as u32;
        assert!(parent.scope < scope, "parent scope is invalid");
        self.scopes.push(ScopeItem::new(parent.scope));
        Scope { scope: scope }
    }

    pub fn set_span(&mut self, scope: Scope, span: Span) {
        assert!((scope.scope as usize) < self.scopes.len());
        assert!(!span.is_dummy());

        self.span_ranges.clear(); // flush the range cache

        let scopespan = &mut self.scopes[scope.scope as usize].span;
        assert!(scopespan.is_dummy(), "scope {:?} has already set the span", scope);
        *scopespan = span;
    }

    pub fn add_name(&mut self, scope: Scope, name: Name) {
        let scopes = self.names.entry(name).or_insert(Vec::new());
        if let Err(idx) = scopes.binary_search(&scope) {
            scopes.insert(idx, scope);
        }
    }

    pub fn ancestor_scopes<'a>(&'a self, scope: Scope) -> AncestorScopes<'a, Name> {
        assert!((scope.scope as usize) < self.scopes.len());
        AncestorScopes { scopes: &self.scopes, current: scope.scope, done: false }
    }

    pub fn names_and_scopes<'a>(&'a self, scope: Scope) -> NamesAndScopes<'a, Name> {
        assert!((scope.scope as usize) < self.scopes.len());
        let iter = self.scopes[scope.scope as usize].names.iter();
        NamesAndScopes { scopes: &self.scopes, current: scope.scope, iter: Some(iter) }
    }

    // finds the most innermost name available at that scope
    pub fn find_name_in_scope<T: ?Sized>(&self, scope: Scope, name: &T) -> Option<Scope>
        where Name: Borrow<T>, T: Hash + Eq
    {
        let mut ancestors = self.ancestor_scopes(scope);
        let mut scopes = match self.names.get(name) {
            Some(v) => v.iter().rev(),
            None => return None,
        };

        // both iterators are sorted (in the decreasing order),
        // so the largest intersection can be quickly found
        let mut next_ancestor = ancestors.next();
        let mut next_scope = scopes.next();
        loop {
            if let (Some(a), Some(&s)) = (next_ancestor, next_scope) {
                if a < s {
                    next_scope = scopes.next();
                } else if a > s {
                    next_ancestor = ancestors.next();
                } else {
                    return Some(a); // found a match
                }
            } else {
                return None;
            }
        }
    }

    fn update_scope_ranges(&mut self) {
        if !self.span_ranges.is_empty() {
            // even when there is no non-dummy span in the map, the cache is populated somehow
            return;
        }

        let mut seqs = HashMap::new();
        for (scope, item) in self.scopes.iter().enumerate() {
            let scope = scope as u32;
            let unit = item.span.unit();
            let mut seq = seqs.entry(unit).or_insert(Vec::new());
            if !unit.is_dummy() {
                let begin = item.span.begin().to_usize() as u32;
                let end = item.span.end().to_usize() as u32;
                seq.push((begin, false, scope));
                seq.push((end, true, scope));
            }
        }

        self.span_ranges = seqs.into_iter().map(|(scope, mut seq)| {
            seq.sort_by_key(|&(pos, _, _)| pos);

            // technically the span may not nest, but we ignore that possibility
            // by only requiring this method to return any correct span in such cases
            let mut open = Vec::new();
            let mut ranges = Vec::new();
            for (pos, will_close, scope) in seq {
                let next_scope = if will_close {
                    while open.pop().unwrap_or(scope) > scope { }
                    open.last().map_or(0, |&scope| scope)
                } else {
                    open.push(scope);
                    scope
                };

                // prefer the opening of later-defined scopes
                if ranges.last().map(|&(pos, _)| pos) == Some(pos) {
                    ranges.last_mut().unwrap().1 = next_scope;
                } else {
                    ranges.push((pos, next_scope));
                }
            }

            (scope, ranges)
        }).collect();
    }

    pub fn scope_from_pos(&mut self, pos: Pos) -> Option<Scope> {
        self.update_scope_ranges();

        let ranges = match self.span_ranges.get(&pos.unit()) {
            Some(ranges) => ranges,
            None => return None,
        };

        let scope = match ranges.binary_search_by(|&(p, _)| p.cmp(&(pos.to_usize() as u32))) {
            Ok(i) => ranges[i].1,
            Err(0) => 0, // before the first item
            Err(i) => ranges[i-1].1,
        };
        if scope == 0 {
            None
        } else {
            Some(Scope { scope: scope })
        }
    }
}

#[test]
fn test_scope_map() {
    let mut m = ScopeMap::new();

    let g = m.generate_root();
    let a = m.generate(g);
    let b = m.generate(g);
    let c = m.generate(a);
    let d = m.generate(b);

    assert_eq!(m.ancestor_scopes(g).collect::<Vec<_>>(), [g]);
    assert_eq!(m.ancestor_scopes(a).collect::<Vec<_>>(), [a, g]);
    assert_eq!(m.ancestor_scopes(b).collect::<Vec<_>>(), [b, g]);
    assert_eq!(m.ancestor_scopes(c).collect::<Vec<_>>(), [c, a, g]);
    assert_eq!(m.ancestor_scopes(d).collect::<Vec<_>>(), [d, b, g]);

    m.add_name(a, "x".to_string());
    m.add_name(b, "x".to_string());
    m.add_name(g, "y".to_string());
    m.add_name(d, "x".to_string());

    assert_eq!(m.find_name_in_scope(c, "x"), Some(a));
    assert_eq!(m.find_name_in_scope(d, "x"), Some(d));
    assert_eq!(m.find_name_in_scope(b, "x"), Some(b));
    assert_eq!(m.find_name_in_scope(g, "x"), None);
    assert_eq!(m.find_name_in_scope(c, "y"), Some(g));
    assert_eq!(m.find_name_in_scope(g, "y"), Some(g));
}

#[test]
fn test_scope_map_span() {
    use loc::{unit_from_u32, pos_from_u32, span_from_u32};

    let unit1 = unit_from_u32(1);
    let unit2 = unit_from_u32(2);
    let pos = |pos| pos_from_u32(unit1, pos);
    let span = |begin, end| span_from_u32(unit1, begin, end);

    let mut m = ScopeMap::<()>::new();
    let g = m.generate_root();
    let a = m.generate(g); m.set_span(a, span(0, 100));
    let b = m.generate(a); m.set_span(b, span(30, 70));
    let c = m.generate(a); m.set_span(c, span(70, 80));
    let d = m.generate(a); m.set_span(d, span(80, 100));
    let e = m.generate(g); m.set_span(e, span(100, 110));
    let f = m.generate(e); m.set_span(f, span(100, 110));
    let x = m.generate(g); m.set_span(x, span(999000, 999999));
    assert_eq!(m.scope_from_pos(pos(0)), Some(a));
    assert_eq!(m.scope_from_pos(pos(20)), Some(a));
    assert_eq!(m.scope_from_pos(pos(30)), Some(b));
    assert_eq!(m.scope_from_pos(pos(69)), Some(b));
    assert_eq!(m.scope_from_pos(pos(70)), Some(c));
    assert_eq!(m.scope_from_pos(pos(80)), Some(d));
    assert_eq!(m.scope_from_pos(pos(100)), Some(f));
    assert_eq!(m.scope_from_pos(pos(109)), Some(f));
    assert_eq!(m.scope_from_pos(pos(110)), None);
    assert_eq!(m.scope_from_pos(pos(888888)), None);
    assert_eq!(m.scope_from_pos(pos(999888)), Some(x));
    assert_eq!(m.scope_from_pos(pos(1000000)), None);

    // check for dummies
    let mut m = ScopeMap::<()>::new();
    let g = m.generate_root();
    let a = m.generate(g); m.set_span(a, span(30, 60));
    let b = m.generate(a); // no span attached
    let c = m.generate(b); m.set_span(c, span(40, 50));
    let d = m.generate(g); // no span attached
    let e = m.generate(d); m.set_span(e, span(80, 90));
    assert_eq!(m.scope_from_pos(pos(0)), None);
    assert_eq!(m.scope_from_pos(pos(30)), Some(a));
    assert_eq!(m.scope_from_pos(pos(35)), Some(a));
    assert_eq!(m.scope_from_pos(pos(40)), Some(c));
    assert_eq!(m.scope_from_pos(pos(50)), Some(a));
    assert_eq!(m.scope_from_pos(pos(55)), Some(a));
    assert_eq!(m.scope_from_pos(pos(60)), None);
    assert_eq!(m.scope_from_pos(pos(85)), Some(e));

    // multiple units & cache invalidation
    let mut m = ScopeMap::<()>::new();
    let g = m.generate_root();
    let a = m.generate(g); m.set_span(a, span_from_u32(unit1, 10, 30));
    let b = m.generate(g); m.set_span(b, span_from_u32(unit2, 20, 40));
    assert_eq!(m.scope_from_pos(pos_from_u32(unit1, 10)), Some(a));
    assert_eq!(m.scope_from_pos(pos_from_u32(unit1, 20)), Some(a));
    assert_eq!(m.scope_from_pos(pos_from_u32(unit1, 30)), None);
    assert_eq!(m.scope_from_pos(pos_from_u32(unit1, 40)), None);
    assert_eq!(m.scope_from_pos(pos_from_u32(unit2, 10)), None);
    assert_eq!(m.scope_from_pos(pos_from_u32(unit2, 20)), Some(b));
    assert_eq!(m.scope_from_pos(pos_from_u32(unit2, 30)), Some(b));
    assert_eq!(m.scope_from_pos(pos_from_u32(unit2, 40)), None);
    let c = m.generate(b); m.set_span(c, span_from_u32(unit2, 25, 35));
    assert_eq!(m.scope_from_pos(pos_from_u32(unit2, 30)), Some(c));
}

