use std::{
    fmt::Debug,
    mem::MaybeUninit,
    ops::{Index, IndexMut},
};

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
    pub fn _pop(&mut self) -> Option<T> {
        if self.len > 0 {
            self.len -= 1;
            Some(unsafe { self.arr[self.len].assume_init_read() })
        } else {
            None
        }
    }
    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.len {
            Some(unsafe { self.arr[index].assume_init_ref() })
        } else {
            None
        }
    }
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index < self.len {
            Some(unsafe { self.arr[index].assume_init_mut() })
        } else {
            None
        }
    }
    pub fn _last(&self) -> Option<&T> {
        self.get(self.len.saturating_sub(1))
    }
    pub fn _last_mut(&mut self) -> Option<&mut T> {
        self.get_mut(self.len.saturating_sub(1))
    }
}
impl<T: Debug, const N: usize> Drop for FixedArr<T, N> {
    fn drop(&mut self) {
        for i in 0..self.len {
            unsafe { self.arr[i].assume_init_drop() }
        }
    }
}
impl<T: Debug, const N: usize> Index<usize> for FixedArr<T, N> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).unwrap()
    }
}
impl<T: Debug, const N: usize> IndexMut<usize> for FixedArr<T, N> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}
