//! Scope identifiers and a location-to-scope map.

use std::fmt;
use std::ops;
use std::slice;
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::borrow::Borrow;
use std::collections::HashMap;
use loc::{Pos, Span, Spanned};
use spanmap::SpanMap;

/// A scope identifier, unique in the originating `ScopeMap`.
///
/// A *scope* is a collection of names (whatever they mean) available to some span.
/// A scope is nested: scopes can contain other scopes (hopefully with smaller spans),
/// and names in inner scopes can shadow names in the outer scope.
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

/// In the debugging output the scope is denoted <code>$<i>scope</i></code>.
impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.scope)
    }
}

/// A combined identifier from a unique name and its scope, unique in the originating `ScopeMap`.
#[derive(Clone)]
pub struct ScopedId {
    id: u32, // can be zero
}

impl ScopedId {
    pub fn to_usize(&self) -> usize {
        self.id as usize
    }

    pub fn name<'a, Name>(&self, map: &'a ScopeMap<Name>) -> &'a Name
        where Name: Clone + Hash + Eq + fmt::Debug
    {
        map.find_id(self).0
    }

    pub fn scope<'a, Name>(&self, map: &'a ScopeMap<Name>) -> Scope
        where Name: Clone + Hash + Eq + fmt::Debug
    {
        map.find_id(self).1
    }
}

impl PartialEq for ScopedId {
    fn eq(&self, other: &ScopedId) -> bool {
        self.id == other.id
    }
}

impl Eq for ScopedId {}

impl PartialOrd for ScopedId {
    fn partial_cmp(&self, other: &ScopedId) -> Option<Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl Ord for ScopedId {
    fn cmp(&self, other: &ScopedId) -> Ordering {
        self.id.cmp(&other.id)
    }
}

impl Hash for ScopedId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

/// In the debugging output the scope is denoted <code>&lt;<i>id</i>&gt;</code>.
///
/// Note that `kailua_test` will automatically convert it to the more readable form,
/// <code>`<i>Name</i>`$<i>scope</i></code>.
impl fmt::Debug for ScopedId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>", self.id)
    }
}

/// Yields all scopes in the `ScopeMap`.
pub struct AllScopes<'a, Name: 'a> {
    scopes: &'a [ScopeItem<Name>],
    range: ops::Range<u32>,
}

impl<'a, Name: 'a> Iterator for AllScopes<'a, Name> {
    type Item = Spanned<Scope>;

    fn next(&mut self) -> Option<Spanned<Scope>> {
        self.range.next().map(|scope| {
            let span = self.scopes[scope as usize].span;
            Spanned { span: span, base: Scope { scope: scope } }
        })
    }
}

/// Yields all ancestor scopes for given scope in the `ScopeMap`.
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

/// Yields all names and corresponding scoped identifiers,
/// visible from given scope in the `ScopeMap`.
pub struct Names<'a, Name: 'a> {
    iter: slice::Iter<'a, (Name, u32)>,
}

impl<'a, Name: 'a + fmt::Debug> Iterator for Names<'a, Name> {
    type Item = (&'a Name, ScopedId);

    fn next(&mut self) -> Option<(&'a Name, ScopedId)> {
        self.iter.next().map(|&(ref name, id)| (name, ScopedId { id: id }))
    }
}

/// Yields all names, corresponding scoped identifiers and the containing scope
/// visible from given scope in the `ScopeMap`.
pub struct NamesAndScopes<'a, Name: 'a> {
    scopes: &'a [ScopeItem<Name>],
    current: u32,
    iter: Option<slice::Iter<'a, (Name, u32)>>,
}

impl<'a, Name: 'a + fmt::Debug> Iterator for NamesAndScopes<'a, Name> {
    type Item = (&'a Name, Scope, ScopedId);

    fn next(&mut self) -> Option<(&'a Name, Scope, ScopedId)> {
        loop {
            if let Some(ref mut iter) = self.iter {
                if let Some(&(ref name, id)) = iter.next() {
                    let scope = Scope { scope: self.current };
                    let scoped_id = ScopedId { id: id };
                    return Some((name, scope, scoped_id));
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

#[derive(Debug, Clone)]
struct ScopeItem<Name> {
    // same to itself when the scope is root (not necessarily global)
    parent: u32,

    // associated span, can be dummy but later set
    span: Span,

    // a list of immediately contained names and ids
    names: Vec<(Name, u32)>,
}

impl<Name> ScopeItem<Name> {
    fn new(parent: u32) -> ScopeItem<Name> {
        ScopeItem { parent: parent, span: Span::dummy(), names: Vec::new() }
    }
}

/// A mapping from the position to the innermost scope containing it.
/// Also manages the names in each scope.
#[derive(Debug, Clone)]
pub struct ScopeMap<Name: Clone + Hash + Eq> {
    // an implicit mapping from the scope to the item
    scopes: Vec<ScopeItem<Name>>,

    // names and sorted lists of containing scopes and ids.
    names: HashMap<Name, Vec<(Scope, u32)>>,

    // a list of name and scope for given scoped ids
    ids: Vec<(Name, Scope)>,

    // mappings from the location to the scope
    spans: SpanMap<Scope>,
}

impl<Name: Clone + Hash + Eq + fmt::Debug> ScopeMap<Name> {
    pub fn new() -> ScopeMap<Name> {
        ScopeMap {
            scopes: vec![ScopeItem::new(0)],
            names: HashMap::new(),
            ids: Vec::new(),
            spans: SpanMap::new(),
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

    pub fn set_span(&mut self, scope: Spanned<Scope>) {
        assert!((scope.base.scope as usize) < self.scopes.len());
        assert!(scope.span.is_source_dependent());

        let scopespan = &mut self.scopes[scope.base.scope as usize].span;
        assert!(scopespan.is_dummy(), "scope {:?} has already set the span", scope.base);
        *scopespan = scope.span;

        self.spans.insert(scope);
    }

    pub fn add_name(&mut self, scope: Scope, name: Name) -> ScopedId {
        assert!((scope.scope as usize) < self.scopes.len());
        let scopes = self.names.entry(name.clone()).or_insert(Vec::new());
        match scopes.binary_search_by(|&(scope_, _)| scope.cmp(&scope_)) {
            Ok(idx) => ScopedId { id: scopes[idx].1 },
            Err(idx) => {
                assert!(self.ids.len() <= 0xffffffff);
                let id = self.ids.len() as u32;
                let scoped_id = ScopedId { id: id };
                self.ids.push((name.clone(), scope));
                scopes.insert(idx, (scope, id));
                // avoids the duplicate insertion by pushing after checking
                self.scopes[scope.scope as usize].names.push((name, id));
                scoped_id
            },
        }
    }

    pub fn parent_scope(&self, scope: Scope) -> Option<Scope> {
        assert!((scope.scope as usize) < self.scopes.len());
        let parent = self.scopes[scope.scope as usize].parent;
        if parent == scope.scope {
            None
        } else {
            Some(Scope { scope: parent })
        }
    }

    pub fn all_scopes<'a>(&'a self) -> AllScopes<'a, Name> {
        AllScopes { scopes: &self.scopes, range: 1..(self.scopes.len() as u32) }
    }

    pub fn ancestor_scopes<'a>(&'a self, scope: Scope) -> AncestorScopes<'a, Name> {
        assert!((scope.scope as usize) < self.scopes.len());
        AncestorScopes { scopes: &self.scopes, current: scope.scope, done: false }
    }

    pub fn names<'a>(&'a self, scope: Scope) -> Names<'a, Name> {
        assert!((scope.scope as usize) < self.scopes.len());
        let iter = self.scopes[scope.scope as usize].names.iter();
        Names { iter: iter }
    }

    // that this may include duplicates from shadowed names. the caller should do
    // its own duplicate removal to avoid them---they always appear after the visible name.
    pub fn names_and_scopes<'a>(&'a self, scope: Scope) -> NamesAndScopes<'a, Name> {
        assert!((scope.scope as usize) < self.scopes.len());
        let iter = self.scopes[scope.scope as usize].names.iter();
        NamesAndScopes { scopes: &self.scopes, current: scope.scope, iter: Some(iter) }
    }

    pub fn find_id<'a>(&'a self, scoped_id: &ScopedId) -> (&'a Name, Scope) {
        self.find_id_with_index(scoped_id.id as usize).expect("ScopedId used on a wrong map")
    }

    pub fn find_id_with_index<'a>(&'a self, id: usize) -> Option<(&'a Name, Scope)> {
        if id < self.ids.len() {
            let (ref name, scope) = self.ids[id];
            Some((name, scope))
        } else {
            None
        }
    }

    // finds the most innermost name available at that scope
    pub fn find_name_in_scope<T: ?Sized>(&self, scope: Scope, name: &T) -> Option<(Scope, ScopedId)>
        where Name: Borrow<T>, for<'a> Name: From<&'a T>, T: Hash + Eq
    {
        let mut ancestors = self.ancestor_scopes(scope);
        let mut scopes = match self.names.get(name) {
            Some(v) => v.iter(),
            None => return None,
        };

        // both iterators are sorted (in the same order),
        // so the largest intersection can be quickly found
        let mut next_ancestor = ancestors.next();
        let mut next_scope = scopes.next();
        loop {
            if let (Some(a), Some(&(s, id))) = (next_ancestor, next_scope) {
                if a < s {
                    next_scope = scopes.next();
                } else if a > s {
                    next_ancestor = ancestors.next();
                } else {
                    // found a match
                    return Some((s, ScopedId { id: id }));
                }
            } else {
                return None;
            }
        }
    }

    pub fn scope_from_pos(&self, pos: Pos) -> Option<Scope> {
        // there might be multiple scopes intersecting pos;
        // prefer (one of) the innermost scopes by using the later-defined scope.
        // (multiple innermost scopes are possible, but probably impossible in the parser)
        self.spans.adjacencies(Span::from(pos)).map(|scope| *scope.base).max()
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

    let id = |n| ScopedId { id: n };

    assert_eq!(m.ancestor_scopes(g).collect::<Vec<_>>(), [g]);
    assert_eq!(m.ancestor_scopes(a).collect::<Vec<_>>(), [a, g]);
    assert_eq!(m.ancestor_scopes(b).collect::<Vec<_>>(), [b, g]);
    assert_eq!(m.ancestor_scopes(c).collect::<Vec<_>>(), [c, a, g]);
    assert_eq!(m.ancestor_scopes(d).collect::<Vec<_>>(), [d, b, g]);

    assert_eq!(m.add_name(a, "x".to_string()), id(0));
    assert_eq!(m.add_name(b, "x".to_string()), id(1));
    assert_eq!(m.add_name(g, "y".to_string()), id(2));
    assert_eq!(m.add_name(d, "x".to_string()), id(3));
    assert_eq!(m.add_name(b, "z".to_string()), id(4));
    assert_eq!(m.add_name(a, "x".to_string()), id(0)); // duplicate

    assert_eq!(m.find_name_in_scope(c, "x"), Some((a, id(0))));
    assert_eq!(m.find_name_in_scope(d, "x"), Some((d, id(3))));
    assert_eq!(m.find_name_in_scope(b, "x"), Some((b, id(1))));
    assert_eq!(m.find_name_in_scope(b, "z"), Some((b, id(4))));
    assert_eq!(m.find_name_in_scope(g, "x"), None);
    assert_eq!(m.find_name_in_scope(c, "y"), Some((g, id(2))));
    assert_eq!(m.find_name_in_scope(g, "y"), Some((g, id(2))));
}

#[test]
fn test_scope_map_span() {
    use loc::{unit_from_u32, pos_from_u32, span_from_u32, WithLoc};

    let unit1 = unit_from_u32(1);
    let unit2 = unit_from_u32(2);
    let pos = |pos| pos_from_u32(unit1, pos);
    let span = |begin, end| span_from_u32(unit1, begin, end);

    let mut m = ScopeMap::<()>::new();
    let g = m.generate_root();
    let a = m.generate(g); m.set_span(a.with_loc(span(0, 100)));
    let b = m.generate(a); m.set_span(b.with_loc(span(30, 70)));
    let c = m.generate(a); m.set_span(c.with_loc(span(70, 80)));
    let d = m.generate(a); m.set_span(d.with_loc(span(80, 100)));
    let e = m.generate(g); m.set_span(e.with_loc(span(100, 110)));
    let f = m.generate(e); m.set_span(f.with_loc(span(100, 110)));
    let x = m.generate(g); m.set_span(x.with_loc(span(999000, 999999)));
    assert_eq!(m.scope_from_pos(pos(0)), Some(a));
    assert_eq!(m.scope_from_pos(pos(20)), Some(a));
    assert_eq!(m.scope_from_pos(pos(30)), Some(b));
    assert_eq!(m.scope_from_pos(pos(69)), Some(b));
    assert_eq!(m.scope_from_pos(pos(70)), Some(c));
    assert_eq!(m.scope_from_pos(pos(80)), Some(d));
    assert_eq!(m.scope_from_pos(pos(100)), Some(f));
    assert_eq!(m.scope_from_pos(pos(109)), Some(f));
    assert_eq!(m.scope_from_pos(pos(110)), Some(f));
    assert_eq!(m.scope_from_pos(pos(111)), None);
    assert_eq!(m.scope_from_pos(pos(888888)), None);
    assert_eq!(m.scope_from_pos(pos(999888)), Some(x));
    assert_eq!(m.scope_from_pos(pos(1000000)), None);

    // check for dummies
    let mut m = ScopeMap::<()>::new();
    let g = m.generate_root();
    let a = m.generate(g); m.set_span(a.with_loc(span(30, 60)));
    let b = m.generate(a); // no span attached
    let c = m.generate(b); m.set_span(c.with_loc(span(40, 50)));
    let d = m.generate(g); // no span attached
    let e = m.generate(d); m.set_span(e.with_loc(span(80, 90)));
    assert_eq!(m.scope_from_pos(pos(0)), None);
    assert_eq!(m.scope_from_pos(pos(30)), Some(a));
    assert_eq!(m.scope_from_pos(pos(39)), Some(a));
    assert_eq!(m.scope_from_pos(pos(40)), Some(c)); // prefer c over a (inclusive)
    assert_eq!(m.scope_from_pos(pos(50)), Some(c)); // prefer c over a (exclusive)
    assert_eq!(m.scope_from_pos(pos(51)), Some(a));
    assert_eq!(m.scope_from_pos(pos(60)), Some(a));
    assert_eq!(m.scope_from_pos(pos(61)), None);
    assert_eq!(m.scope_from_pos(pos(85)), Some(e));

    // multiple units & cache invalidation
    let mut m = ScopeMap::<()>::new();
    let g = m.generate_root();
    let a = m.generate(g); m.set_span(a.with_loc(span_from_u32(unit1, 10, 30)));
    let b = m.generate(g); m.set_span(b.with_loc(span_from_u32(unit2, 20, 40)));
    assert_eq!(m.scope_from_pos(pos_from_u32(unit1, 10)), Some(a));
    assert_eq!(m.scope_from_pos(pos_from_u32(unit1, 20)), Some(a));
    assert_eq!(m.scope_from_pos(pos_from_u32(unit1, 30)), Some(a));
    assert_eq!(m.scope_from_pos(pos_from_u32(unit1, 40)), None);
    assert_eq!(m.scope_from_pos(pos_from_u32(unit2, 10)), None);
    assert_eq!(m.scope_from_pos(pos_from_u32(unit2, 20)), Some(b));
    assert_eq!(m.scope_from_pos(pos_from_u32(unit2, 30)), Some(b));
    assert_eq!(m.scope_from_pos(pos_from_u32(unit2, 40)), Some(b));
    assert_eq!(m.scope_from_pos(pos_from_u32(unit2, 50)), None);
    let c = m.generate(b); m.set_span(c.with_loc(span_from_u32(unit2, 25, 35)));
    assert_eq!(m.scope_from_pos(pos_from_u32(unit2, 30)), Some(c));
}

