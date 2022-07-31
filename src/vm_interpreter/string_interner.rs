use super::gc::{Gc, Trace};
use crate::{custom_trace, empty_trace_impl};
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    fmt::{Debug, Display},
    hash::Hash,
    ops::Deref,
};

struct InternedState {
    references: usize,
    value: Gc<InternedStringBox>,
}
thread_local! {
    static INTERNED_STRINGS: RefCell<HashMap<String, InternedState>> = RefCell::new(HashMap::new());
}

#[derive(Debug)]
struct InternedStringBox {
    value: String,
}
impl Trace for InternedStringBox {
    empty_trace_impl!();
}

pub struct InternedString {
    value: Gc<InternedStringBox>,
}
impl InternedString {
    pub fn new(value: String) -> Self {
        INTERNED_STRINGS.with(|strings| {
            let mut strings = strings.borrow_mut();
            match strings.entry(value) {
                Entry::Occupied(entry) => {
                    let entry = entry.get_mut();
                    entry.references += 1;
                    Self {
                        value: entry.value.clone(),
                    }
                }
                Entry::Vacant(entry) => {
                    let value = Gc::new(InternedStringBox {
                        value: entry.key().clone(),
                    });
                    entry.insert(InternedState {
                        value: value.clone(),
                        references: 1,
                    });
                    Self { value }
                }
            }
        })
    }
    fn as_str(&self) -> &str {
        self.value.value.as_str()
    }
}
impl Trace for InternedString {
    custom_trace!(|this| {
        mark(&this.value);
    });
}
impl Drop for InternedString {
    fn drop(&mut self) {
        INTERNED_STRINGS.with(|strings| {
            let mut strings = strings.borrow_mut();
            let state = strings.get_mut(self.as_str()).unwrap();
            state.references -= 1;
            if state.references == 0 {
                strings.remove(self.as_str());
            }
        })
    }
}
impl Clone for InternedString {
    fn clone(&self) -> Self {
        INTERNED_STRINGS.with(|strings| {
            strings
                .borrow_mut()
                .get_mut(self.as_str())
                .unwrap()
                .references += 1
        });
        Self {
            value: self.value.clone(),
        }
    }
}
impl Deref for InternedString {
    type Target = str;
    fn deref(&self) -> &str {
        self.as_str()
    }
}
impl Debug for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.as_str())
    }
}
impl Display for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
impl PartialEq for InternedString {
    fn eq(&self, other: &Self) -> bool {
        self.value.ref_eq(&other.value)
    }
}
impl Eq for InternedString {}
impl Hash for InternedString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash_ref(state);
    }
}
