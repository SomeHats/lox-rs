use super::{
    environment::EnvironmentRef, lox_callable::LoxCallable, lox_object::LoxObject, Ctx,
    Interpreter, RuntimeValue,
};
use crate::{ast, side_table::UniqueId, RuntimeError};
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    io::Write,
    rc::Rc,
};

pub struct LoxClass {
    id: UniqueId,
    name: String,
    closure: EnvironmentRef,
    methods: HashMap<String, Rc<ast::Fun>>,
    ctx: Ctx,
}
impl LoxClass {
    pub fn new(name: &str, closure: EnvironmentRef, methods: Vec<Rc<ast::Fun>>, ctx: Ctx) -> Self {
        Self {
            id: UniqueId::new(),
            name: name.to_string(),
            closure,
            methods: methods
                .into_iter()
                .map(|method| (method.name.name.clone(), method))
                .collect(),
            ctx,
        }
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn lookup_method(&self, name: &str) -> Option<Rc<ast::Fun>> {
        self.methods.get(name).cloned()
    }
    pub fn closure(&self) -> &EnvironmentRef {
        &self.closure
    }
    pub fn ctx(&self) -> &Ctx {
        &self.ctx
    }
}
impl Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<class {}>", self.name)
    }
}
impl Debug for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}
impl PartialEq for LoxClass {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl LoxCallable for Rc<LoxClass> {
    fn arity(&self) -> usize {
        0
    }

    fn call<W: Write>(
        &self,
        _: &mut Interpreter<W>,
        _: &[RuntimeValue],
    ) -> Result<RuntimeValue, RuntimeError> {
        Ok(RuntimeValue::Object(LoxObject::new(self.clone())))
    }
}
