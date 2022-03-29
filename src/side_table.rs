use std::{
    collections::HashMap,
    marker::PhantomData,
    sync::atomic::{AtomicUsize, Ordering},
};

static ID_COUNTER: AtomicUsize = AtomicUsize::new(1);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct UniqueId(usize);
impl UniqueId {
    pub fn new() -> Self {
        UniqueId(ID_COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}
impl Default for UniqueId {
    fn default() -> Self {
        Self::new()
    }
}

pub trait Unique {
    fn id(&self) -> UniqueId;
}

#[derive(Debug)]
pub struct SideTable<Owner: Unique, T> {
    values: HashMap<UniqueId, T>,
    owner: PhantomData<Owner>,
}
impl<Owner: Unique, T> SideTable<Owner, T> {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            owner: PhantomData,
        }
    }
    pub fn get(&self, owner: &Owner) -> Option<&T> {
        self.values.get(&owner.id())
    }
    pub fn set(&mut self, owner: &Owner, value: T) -> Option<T> {
        self.values.insert(owner.id(), value)
    }
}
