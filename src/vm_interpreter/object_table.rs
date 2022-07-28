use std::marker::PhantomData;

#[derive(Debug)]
pub struct ObjectTable<T> {
    table: Vec<T>,
}
impl<T> Default for ObjectTable<T> {
    fn default() -> Self {
        Self { table: Vec::new() }
    }
}
impl<T> ObjectTable<T> {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn allocate(&mut self, value: T) -> ObjectReference<T> {
        let index = self.table.len();
        self.table.push(value);
        ObjectReference::new(index)
    }
    pub fn get(&self, reference: &ObjectReference<T>) -> &T {
        &self.table[reference.index]
    }
}

#[derive(Debug, Clone)]
pub struct ObjectReference<T> {
    index: usize,
    _phantom: PhantomData<T>,
}
impl<T> ObjectReference<T> {
    fn new(index: usize) -> Self {
        Self {
            index,
            _phantom: PhantomData,
        }
    }
}
