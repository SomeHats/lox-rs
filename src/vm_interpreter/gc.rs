use std::{
    borrow::Borrow,
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt::{Debug, Display},
    ops::Deref,
    ptr,
};

use crate::backtrace::minitrace;

pub trait Trace: Debug + 'static {
    fn trace(&self);
    fn root(&self);
    fn unroot(&self);
    fn finalize(&self) {}
}
macro_rules! empty_trace_impl {
    () => {
        #[inline]
        fn trace(&self) {}
        #[inline]
        fn root(&self) {}
        #[inline]
        fn unroot(&self) {}
    };
}

struct GcState {
    boxes: Cell<Option<GcBoxPtr>>,
    strings: RefCell<HashMap<String, ptr::NonNull<GcBox<GcStringInner>>>>,
}
impl GcState {
    fn iter_boxes(&self) -> impl Iterator<Item = &GcBox<dyn Trace>> {
        let mut current = self.boxes.get();
        std::iter::from_fn(move || {
            if let Some(box_ptr) = current {
                let boxed = unsafe { box_ptr.as_ref() };
                current = boxed.next.get();
                Some(boxed)
            } else {
                None
            }
        })
    }
}
thread_local! {
    static GC_STATE: GcState = GcState { boxes: Cell::new(None), strings: RefCell::new(HashMap::new()) };
}

type GcBoxPtr = ptr::NonNull<GcBox<dyn Trace>>;

struct GcBox<T: Trace + ?Sized> {
    root_count: Cell<usize>,
    next: Cell<Option<GcBoxPtr>>,
    value: T,
}
impl<T: Trace> GcBox<T> {
    fn new(value: T) -> ptr::NonNull<Self> {
        GC_STATE.with(|state| {
            if cfg!(feature = "debug_gc_roots") {
                println!("[gc] Created & rooted: {:?} at:", &value);
                minitrace();
            }
            let boxed: GcBox<T> = GcBox {
                root_count: Cell::new(1),
                next: Cell::new(state.boxes.take()),
                value,
            };
            let allocated: ptr::NonNull<Self> = unsafe { allocate(boxed) };
            state.boxes.set(Some(allocated));
            allocated
        })
    }
    fn value(&self) -> &T {
        &self.value
    }
    fn trace(&self) {
        self.value.trace();
    }
    fn root(&self) {
        let roots = self.root_count.get() + 1;
        self.root_count.set(roots);
        if roots == 1 {
            if cfg!(feature = "debug_gc_roots") {
                println!("[gc] Rooted: {:?} at:", self.value,);
                minitrace();
            }
        } else if cfg!(feature = "debug_gc_roots") {
            println!(
                "[gc] Root count for {:?} increased to {} at:",
                self.value, roots,
            );
            minitrace()
        }
    }
    fn unroot(&self) {
        // TODO: handle underflow?
        let roots = self.root_count.get() - 1;
        self.root_count.set(roots);
        if roots == 0 {
            if cfg!(feature = "debug_gc_roots") {
                println!("[gc] un-rooted: {:?} at:", self.value,);
                minitrace();
            }
        } else if cfg!(feature = "debug_gc_roots") {
            println!(
                "[gc] Root count for {:?} decreased to {} at:",
                self.value, roots,
            );
            minitrace()
        }
    }
}

pub struct Gc<T: Trace> {
    ptr: ptr::NonNull<GcBox<T>>,
}
impl<T: Trace> Gc<T> {
    pub fn _new(value: T) -> Self {
        let gc = Gc {
            ptr: GcBox::new(value),
        };
        // The thing which we are storing internally is no longer rooted!
        gc.inner().value().unroot();
        gc
    }
    fn inner(&self) -> &GcBox<T> {
        unsafe { self.ptr.as_ref() }
    }
}
impl<T: Trace> Trace for Gc<T> {
    fn trace(&self) {
        self.inner().trace();
    }
    fn root(&self) {
        self.inner().root();
    }
    fn unroot(&self) {
        self.inner().unroot();
    }
}
impl<T: Trace> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        self.inner().value()
    }
}
impl<T: Trace> Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.as_ref()
    }
}
impl<T: Trace> Clone for Gc<T> {
    fn clone(&self) -> Self {
        self.root();
        Self { ptr: self.ptr }
    }
}
impl<T: Trace> Drop for Gc<T> {
    fn drop(&mut self) {
        self.unroot();
    }
}
impl<T: Trace> Debug for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Gc").field(self.as_ref()).finish()
    }
}

#[derive(Debug)]
struct GcStringInner(String);
impl Trace for GcStringInner {
    empty_trace_impl!();

    fn finalize(&self) {
        GC_STATE.with(|state| {
            let mut strings = state.strings.borrow_mut();
            strings.remove(&self.0);
        });
    }
}

#[derive(PartialEq, Eq, Hash)]
pub struct GcString {
    ptr: ptr::NonNull<GcBox<GcStringInner>>,
}
impl GcString {
    pub fn new(value: String) -> Self {
        let ptr = GC_STATE.with(|state| match state.strings.borrow_mut().entry(value) {
            std::collections::hash_map::Entry::Occupied(entry) => {
                let ptr = entry.get();
                unsafe {
                    ptr.as_ref().root();
                }
                *ptr
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                if cfg!(feature = "debug_gc_roots") {
                    println!("[gc] interning string {:?}", entry.key());
                }
                let ptr = GcBox::new(GcStringInner(entry.key().clone()));
                entry.insert(ptr);
                ptr
            }
        });
        GcString { ptr }
    }
    fn inner(&self) -> &GcBox<GcStringInner> {
        unsafe { self.ptr.as_ref() }
    }
    fn as_str(&self) -> &str {
        self.inner().value().0.as_str()
    }
}
impl Trace for GcString {
    fn trace(&self) {
        self.inner().trace();
    }
    fn root(&self) {
        self.inner().root();
    }
    fn unroot(&self) {
        self.inner().unroot();
    }
}
impl Drop for GcString {
    fn drop(&mut self) {
        self.unroot();
    }
}
impl Clone for GcString {
    fn clone(&self) -> Self {
        self.root();
        Self { ptr: self.ptr }
    }
}
impl Deref for GcString {
    type Target = str;
    fn deref(&self) -> &str {
        self.as_str()
    }
}
impl Debug for GcString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.as_str())
    }
}
impl Display for GcString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

unsafe fn allocate<T>(value: T) -> ptr::NonNull<T> {
    let allocated = std::alloc::alloc(std::alloc::Layout::new::<T>()) as *mut T;
    allocated.write(value);
    ptr::NonNull::new(allocated).unwrap()
}

macro_rules! empty_trace_impl_for {
    ($($ty:ty),*) => {
        $(
            impl Trace for $ty {
                empty_trace_impl!();
            }
        )*
    };
}

empty_trace_impl_for![
    (),
    bool,
    isize,
    usize,
    i8,
    u8,
    i16,
    u16,
    i32,
    u32,
    i64,
    u64,
    i128,
    u128,
    f32,
    f64,
    char,
    String,
    Box<str>,
    std::rc::Rc<str>,
    std::path::Path,
    std::path::PathBuf,
    std::num::NonZeroIsize,
    std::num::NonZeroUsize,
    std::num::NonZeroI8,
    std::num::NonZeroU8,
    std::num::NonZeroI16,
    std::num::NonZeroU16,
    std::num::NonZeroI32,
    std::num::NonZeroU32,
    std::num::NonZeroI64,
    std::num::NonZeroU64,
    std::num::NonZeroI128,
    std::num::NonZeroU128,
    std::sync::atomic::AtomicBool,
    std::sync::atomic::AtomicIsize,
    std::sync::atomic::AtomicUsize,
    std::sync::atomic::AtomicI8,
    std::sync::atomic::AtomicU8,
    std::sync::atomic::AtomicI16,
    std::sync::atomic::AtomicU16,
    std::sync::atomic::AtomicI32,
    std::sync::atomic::AtomicU32,
    std::sync::atomic::AtomicI64,
    std::sync::atomic::AtomicU64
];

pub mod gc_stats {
    use super::*;

    pub fn total_root_count() -> usize {
        GC_STATE.with(|state| state.iter_boxes().map(|boxed| boxed.root_count.get()).sum())
    }
}
