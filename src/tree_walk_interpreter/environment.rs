use super::RuntimeValue;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type EnvironmentRef = Rc<RefCell<Environment>>;

#[derive(Debug, Default)]

pub struct Environment {
    values: HashMap<String, RuntimeValue>,
    parent: Option<EnvironmentRef>,
    this_value: Option<RuntimeValue>,
}
impl Environment {
    pub fn wrap(self) -> EnvironmentRef {
        Rc::new(RefCell::new(self))
    }
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            parent: None,
            this_value: None,
        }
    }
    pub fn new_with_parent(parent: EnvironmentRef) -> Self {
        Environment {
            parent: Some(parent),
            ..Default::default()
        }
    }
    fn is_global(&self) -> bool {
        self.parent.is_none()
    }
    fn is_local(&self) -> bool {
        !self.is_global()
    }
    pub fn define(&mut self, name: &str, value: RuntimeValue) -> Result<RuntimeValue, ()> {
        if self.values.contains_key(name) && self.is_local() {
            Err(())
        } else {
            self.values.insert(name.to_string(), value);
            Ok(self.get(name).unwrap())
        }
    }
    pub fn get(&self, name: &str) -> Option<RuntimeValue> {
        self.values.get(name).map(Clone::clone).or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().get(name))
        })
    }
    pub fn assign(&mut self, name: &str, value: RuntimeValue) -> Option<RuntimeValue> {
        if let Some(target) = self.values.get_mut(name) {
            *target = value;
            Some(self.get(name).unwrap())
        } else if let Some(parent) = &mut self.parent {
            parent.as_ref().borrow_mut().assign(name, value)
        } else {
            None
        }
    }
    pub fn ancestor<T, F: Fn(&Self) -> T>(&self, depth: usize, cb: F) -> Option<T> {
        if depth == 0 {
            Some(cb(self))
        } else {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().ancestor(depth - 1, cb))
        }
    }
    pub fn ancestor_mut<T, F: Fn(&mut Self) -> T>(&mut self, depth: usize, cb: F) -> Option<T> {
        if depth == 0 {
            Some(cb(self))
        } else {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow_mut().ancestor_mut(depth - 1, cb))
        }
    }
    pub fn get_this(&self) -> Option<RuntimeValue> {
        self.this_value.as_ref().cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().get_this())
        })
    }
    pub fn set_this(&mut self, this_value: Option<RuntimeValue>) {
        self.this_value = this_value;
    }
}
