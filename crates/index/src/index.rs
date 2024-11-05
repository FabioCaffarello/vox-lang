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
