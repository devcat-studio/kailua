use std::ops;
use vec_map::VecMap;

pub trait Partition {
    fn create(parent: usize, rank: usize) -> Self;
    fn read(&self) -> (usize /*parent*/, usize /*rank*/);
    fn write_parent(&self, parent: usize);
    fn increment_rank(&mut self);
}

#[derive(Debug)]
pub struct Partitions<T> {
    map: VecMap<T>,
}

impl<T: Partition> Partitions<T> {
    pub fn new() -> Partitions<T> {
        Partitions { map: VecMap::new() }
    }

    pub fn find(&self, i: usize) -> usize {
        if let Some(u) = self.map.get(&i) {
            let (mut parent, _) = u.read();
            if parent != i { // path compression
                while let Some(v) = self.map.get(&parent) {
                    let (newparent, _) = v.read();
                    if newparent == parent { break; }
                    parent = newparent;
                }
                u.write_parent(parent);
            }
            parent
        } else {
            i
        }
    }

    pub fn union(&mut self, lhs: usize, rhs: usize) -> usize {
        use std::cmp::Ordering;

        let lhs = self.find(lhs);
        let rhs = self.find(rhs);
        if lhs == rhs { return rhs; }

        let (_, lrank) = self.map.entry(lhs).or_insert_with(|| Partition::create(lhs, 0)).read();
        let (_, rrank) = self.map.entry(rhs).or_insert_with(|| Partition::create(rhs, 0)).read();
        match lrank.cmp(&rrank) {
            Ordering::Less => {
                self.map.get_mut(&lhs).unwrap().write_parent(rhs);
                rhs
            }
            Ordering::Greater => {
                self.map.get_mut(&rhs).unwrap().write_parent(lhs);
                lhs
            }
            Ordering::Equal => {
                self.map.get_mut(&rhs).unwrap().write_parent(lhs);
                self.map.get_mut(&lhs).unwrap().increment_rank();
                lhs
            }
        }
    }
}

impl<T> ops::Deref for Partitions<T> {
    type Target = VecMap<T>;
    fn deref(&self) -> &VecMap<T> { &self.map }
}

impl<T> ops::DerefMut for Partitions<T> {
    fn deref_mut(&mut self) -> &mut VecMap<T> { &mut self.map }
}

