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

#[derive(Clone)]
pub struct LoxClass(Rc<LoxClassImpl>);
struct LoxClassImpl {
    id: UniqueId,
    name: String,
    super_class: Option<LoxClass>,
    closure: EnvironmentRef,
    methods: HashMap<String, Rc<ast::Fun>>,
    ctx: Ctx,
}
impl LoxClass {
    pub fn new(
        name: &str,
        super_class: Option<LoxClass>,
        closure: EnvironmentRef,
        methods: Vec<Rc<ast::Fun>>,
        ctx: Ctx,
    ) -> Self {
        Self(Rc::new(LoxClassImpl {
            id: UniqueId::new(),
            name: name.to_string(),
            super_class,
            closure,
            methods: methods
                .into_iter()
                .map(|method| (method.name.name.clone(), method))
                .collect(),
            ctx,
        }))
    }
    pub fn name(&self) -> &str {
        &self.0.name
    }
    pub fn lookup_method(&self, name: &str) -> Option<Rc<ast::Fun>> {
        self.0.methods.get(name).cloned().or_else(|| {
            self.0
                .super_class
                .as_ref()
                .and_then(|super_class| super_class.lookup_method(name))
        })
    }
    pub fn closure(&self) -> &EnvironmentRef {
        &self.0.closure
    }
    pub fn ctx(&self) -> &Ctx {
        &self.0.ctx
    }
}
impl Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<class {}>", self.0.name)
    }
}
impl Debug for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}
impl PartialEq for LoxClass {
    fn eq(&self, other: &Self) -> bool {
        self.0.id == other.0.id
    }
}
impl LoxCallable for LoxClass {
    fn arity(&self) -> usize {
        self.lookup_method("init")
            .map_or(0, |init| init.parameters.len())
    }

    fn call<W: Write>(
        &self,
        interpreter: &mut Interpreter<W>,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, RuntimeError> {
        let object = LoxObject::new(self.clone());
        if let Some(init) = object.get_method("init") {
            init.call(interpreter, args)?;
        }
        Ok(RuntimeValue::Object(object))
    }
}
