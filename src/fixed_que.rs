use std::{fmt::Debug, mem};

#[derive(Debug)]
pub struct FixedQueue<T: Debug, const N: usize> {
    head: usize,
    len: usize,
    data: [Option<T>; N],
}

impl<T: Debug, const N: usize> FixedQueue<T, N> {
    pub fn new() -> Self {
        Self {
            head: 0,
            len: 0,
            data: [(); N].map(|_| None),
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    fn idx(&self, idx: usize) -> usize {
        (self.head + idx) % N
    }

    pub fn push_back(&mut self, item: T) {
        assert!(self.len < N);
        self.data[self.idx(self.len)] = Some(item);
        self.len += 1;
    }

    pub fn pop_front(&mut self) -> Option<T> {
        if self.len < 1 {
            None
        } else {
            let item = mem::replace(&mut self.data[self.idx(0)], None);
            self.head += 1;
            self.len -= 1;
            Some(item.unwrap())
        }
    }

    pub fn get(&self, idx: usize) -> Option<&T> {
        if idx < self.len {
            Some(self.data[self.idx(idx)].as_ref().unwrap())
        } else {
            None
        }
    }
}
