use super::RuntimeValue;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type EnvironmentRef = Rc<RefCell<Environment>>;

#[derive(Debug, Default)]

pub struct Environment {
    values: HashMap<String, RuntimeValue>,
    parent: Option<EnvironmentRef>,
}
impl Environment {
    pub fn wrap(self) -> EnvironmentRef {
        Rc::new(RefCell::new(self))
    }
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            parent: None,
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
    pub fn define_local(
        &mut self,
        name: &str,
        value: impl Into<RuntimeValue>,
    ) -> Result<RuntimeValue, ()> {
        let value = value.into();
        if self.values.contains_key(name) && self.is_local() {
            Err(())
        } else {
            self.values.insert(name.to_string(), value.clone());
            Ok(value)
        }
    }
    pub fn get_local(&self, name: &str) -> Result<RuntimeValue, ()> {
        self.values.get(name).cloned().ok_or(())
    }
    pub fn assign_local(
        &mut self,
        name: &str,
        value: impl Into<RuntimeValue>,
    ) -> Result<RuntimeValue, ()> {
        let value = value.into();
        if let Some(target) = self.values.get_mut(name) {
            *target = value.clone();
            Ok(value)
        } else {
            Err(())
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
    pub fn parent(&self) -> &Option<EnvironmentRef> {
        &self.parent
    }
}
