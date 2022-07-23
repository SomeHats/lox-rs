use super::{lox_class::LoxClass, lox_function::LoxFunction, RuntimeValue};
use crate::side_table::UniqueId;
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

#[derive(Clone)]
pub struct LoxObject(Rc<LoxObjectImpl>);
struct LoxObjectImpl {
    id: UniqueId,
    class: LoxClass,
    values: RefCell<HashMap<String, RuntimeValue>>,
}

impl LoxObject {
    pub fn new(class: LoxClass) -> Self {
        Self(Rc::new(LoxObjectImpl {
            id: UniqueId::new(),
            class,
            values: RefCell::new(HashMap::new()),
        }))
    }
    pub fn get(&self, name: &str) -> Option<RuntimeValue> {
        self.0
            .values
            .borrow()
            .get(name)
            .cloned()
            .or_else(|| self.get_method(name).map(RuntimeValue::Function))
    }
    pub fn get_method(&self, name: &str) -> Option<LoxFunction> {
        self.0
            .class
            .lookup_method(name)
            .map(|method| method.bind(self.clone()))
    }
    pub fn set(&self, name: &str, value: RuntimeValue) {
        self.0.values.borrow_mut().insert(name.to_string(), value);
    }
}
impl Display for LoxObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<object {}>", self.0.class.name())
    }
}
impl Debug for LoxObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}
impl PartialEq for LoxObject {
    fn eq(&self, other: &Self) -> bool {
        self.0.id == other.0.id
    }
}
