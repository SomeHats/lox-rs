use super::{
    environment::{Environment, EnvironmentRef},
    lox_callable::LoxCallable,
    lox_function::LoxFunction,
    lox_object::LoxObject,
    Ctx, Interpreter, RuntimeValue,
};
use crate::{
    ast,
    keywords::{INIT, SUPER},
    side_table::UniqueId,
    RuntimeError,
};
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
    methods: HashMap<String, LoxFunction>,
    ctx: Ctx,
}
impl LoxClass {
    pub fn new(
        name: &str,
        super_class: Option<LoxClass>,
        mut closure: EnvironmentRef,
        methods: Vec<Rc<ast::Fun>>,
        ctx: Ctx,
    ) -> Self {
        if let Some(ref super_class) = super_class {
            closure = {
                let mut env = Environment::new_with_parent(closure);
                env.define_local(SUPER, super_class.clone()).unwrap();
                env.wrap()
            }
        }
        Self(Rc::new(LoxClassImpl {
            id: UniqueId::new(),
            name: name.to_string(),
            super_class,
            methods: methods
                .into_iter()
                .map(|method| {
                    let method_name = method.name.name.clone();
                    let fun =
                        LoxFunction::new(method, closure.clone(), ctx.clone(), method_name == INIT);

                    (method_name, fun)
                })
                .collect(),
            closure,
            ctx,
        }))
    }
    pub fn name(&self) -> &str {
        &self.0.name
    }
    pub fn lookup_method(&self, name: &str) -> Option<LoxFunction> {
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
        self.lookup_method(INIT).map_or(0, |init| init.arity())
    }

    fn call<W: Write>(
        &self,
        interpreter: &mut Interpreter<W>,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, RuntimeError> {
        let object = LoxObject::new(self.clone());
        if let Some(init) = object.get_method(INIT) {
            init.call(interpreter, args)?;
        }
        Ok(object.into())
    }
}
