#[macro_export]
macro_rules! idx {
    ($name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $name {
            index: usize,
        }

        impl Idx for $name {
            fn as_index(&self) -> usize {
                self.index
            }

            fn new(index: usize) -> Self {
                Self { index }
            }
        }
    };
}

pub trait Idx {
    fn as_index(&self) -> usize;
    fn new(index: usize) -> Self;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IdxVec<Index, T>
where
    Index: Idx,
{
    vec: Vec<T>,
    _marker: std::marker::PhantomData<Index>,
}

impl<Index, T> IdxVec<Index, T>
where
    Index: Idx,
{
    pub fn new() -> Self {
        Self {
            vec: vec![],
            _marker: std::marker::PhantomData,
        }
    }

    pub fn push(&mut self, value: T) -> Index {
        let next_index = Index::new(self.vec.len());
        self.vec.push(value);
        next_index
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.vec.iter()
    }

    pub fn get(&self, index: Index) -> &T {
        &self[index]
    }

    pub fn indexed_iter(&self) -> impl Iterator<Item = (Index, &T)> {
        self.vec
            .iter()
            .enumerate()
            .map(|(index, value)| (Index::new(index), value))
    }

    pub fn cloned_indices(&self) -> Vec<Index> {
        self.vec
            .iter()
            .enumerate()
            .map(|(index, _)| Index::new(index))
            .collect()
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }
}

impl<Index, T> Default for IdxVec<Index, T>
where
    Index: Idx,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<Index, T> std::ops::Index<Index> for IdxVec<Index, T>
where
    Index: Idx,
{
    type Output = T;

    fn index(&self, index: Index) -> &T {
        &self.vec[index.as_index()]
    }
}

impl<Index, T> std::ops::IndexMut<Index> for IdxVec<Index, T>
where
    Index: Idx,
{
    fn index_mut(&mut self, index: Index) -> &mut T {
        &mut self.vec[index.as_index()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    idx!(NodeId);
    idx!(EdgeId);

    #[test]
    fn idx_new_and_as_index_roundtrip() {
        let id = NodeId::new(42);
        assert_eq!(id.as_index(), 42);
    }

    #[test]
    fn idx_is_copy_clone_eq_hash_derives_compile() {
        use std::collections::HashSet;
        let a = NodeId::new(1);
        let b = a; // Copy
        let c = a.clone();
        assert_eq!(a, b);
        assert_eq!(a, c);

        let mut set = HashSet::new();
        set.insert(a);
        assert!(set.contains(&NodeId::new(1)));
    }

    #[test]
    fn idxvec_new_default_are_empty() {
        let v1: IdxVec<NodeId, i32> = IdxVec::new();
        let v2: IdxVec<NodeId, i32> = Default::default();

        assert_eq!(v1.len(), 0);
        assert!(v1.is_empty());
        assert_eq!(v2.len(), 0);
        assert!(v2.is_empty());
    }

    #[test]
    fn push_returns_sequential_indices() {
        let mut v: IdxVec<NodeId, &str> = IdxVec::new();
        let i0 = v.push("a");
        let i1 = v.push("b");
        let i2 = v.push("c");

        assert_eq!(i0.as_index(), 0);
        assert_eq!(i1.as_index(), 1);
        assert_eq!(i2.as_index(), 2);

        assert_eq!(v[i0], "a");
        assert_eq!(v[i1], "b");
        assert_eq!(v[i2], "c");
    }

    #[test]
    fn iter_and_get_work() {
        let mut v: IdxVec<NodeId, i32> = IdxVec::new();
        let i0 = v.push(10);
        let i1 = v.push(20);

        assert_eq!(*v.get(i0), 10);
        assert_eq!(*v.get(i1), 20);

        let items: Vec<_> = v.iter().copied().collect();
        assert_eq!(items, vec![10, 20]);
    }

    #[test]
    fn indexed_iter_yields_index_and_ref() {
        let mut v: IdxVec<EdgeId, &str> = IdxVec::new();
        let e0 = v.push("e0");
        let e1 = v.push("e1");

        let out: Vec<(usize, &str)> = v
            .indexed_iter()
            .map(|(idx, val)| (idx.as_index(), *val))
            .collect();

        assert_eq!(out, vec![(e0.as_index(), "e0"), (e1.as_index(), "e1")]);
    }

    #[test]
    fn cloned_indices_match_length_and_values() {
        let mut v: IdxVec<NodeId, char> = IdxVec::new();
        for c in ['x', 'y', 'z'] {
            v.push(c);
        }

        let ids = v.cloned_indices();
        assert_eq!(ids.len(), 3);
        assert_eq!(
            ids.into_iter().map(|i| i.as_index()).collect::<Vec<_>>(),
            vec![0, 1, 2]
        );
    }

    #[test]
    fn index_mut_allows_in_place_update() {
        let mut v: IdxVec<NodeId, i32> = IdxVec::new();
        let i = v.push(5);
        v[i] += 1;
        assert_eq!(v[i], 6);
    }

    #[test]
    #[should_panic]
    fn get_with_invalid_manual_index_panics() {
        let v: IdxVec<NodeId, i32> = IdxVec::new();
        let bogus = NodeId::new(0);
        let _ = v.get(bogus);
    }

    #[test]
    #[should_panic]
    fn index_trait_out_of_bounds_panics() {
        let mut v: IdxVec<NodeId, i32> = IdxVec::new();
        let _ = v.push(1);
        let invalid = NodeId::new(10);
        let _ = v[invalid];
    }

    #[test]
    fn different_index_types_are_not_interchangeable() {
        let mut nodes: IdxVec<NodeId, &str> = IdxVec::new();
        let mut edges: IdxVec<EdgeId, &str> = IdxVec::new();

        let n0 = nodes.push("n0");
        let e0 = edges.push("e0");

        assert_eq!(nodes[n0], "n0");
        assert_eq!(edges[e0], "e0");
    }
}
