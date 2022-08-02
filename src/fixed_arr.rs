use std::{fmt::Debug, mem::MaybeUninit};

#[derive(Debug)]
pub struct FixedArr<T: Debug, const N: usize> {
    len: usize,
    arr: [MaybeUninit<T>; N],
}
impl<T: Debug, const N: usize> FixedArr<T, N> {
    pub fn new() -> Self {
        Self {
            len: 0,
            arr: unsafe { MaybeUninit::uninit().assume_init() },
        }
    }
    pub fn capacity(&self) -> usize {
        N
    }
    pub fn len(&self) -> usize {
        self.len
    }
    #[must_use]
    pub fn push(&mut self, value: T) -> Option<&mut T> {
        if self.len < N {
            let written = self.arr[self.len].write(value);
            self.len += 1;
            Some(written)
        } else {
            None
        }
    }
    #[must_use]
    pub fn pop(&mut self) -> Option<T> {
        if self.len > 0 {
            self.len -= 1;
            Some(unsafe { self.arr[self.len].assume_init_read() })
        } else {
            None
        }
    }
}
impl<T: Debug, const N: usize> Drop for FixedArr<T, N> {
    fn drop(&mut self) {
        for i in 0..self.len {
            unsafe { self.arr[i].assume_init_drop() }
        }
    }
}
