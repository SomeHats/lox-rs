use super::{lox_callable::LoxCallable, Interpreter, RuntimeValue};
use crate::RuntimeError;
use std::{
    fmt::{Debug, Display},
    io::Write,
    rc::Rc,
};

#[derive(Clone)]
pub struct LoxNativeFunction(Rc<LoxNativeFunctionImpl>);
struct LoxNativeFunctionImpl {
    id: usize,
    name: String,
    arity: usize,
    implementation: fn(&[RuntimeValue]) -> Result<RuntimeValue, RuntimeError>,
}
impl LoxNativeFunction {
    pub fn new(
        id: usize,
        name: String,
        arity: usize,
        implementation: fn(&[RuntimeValue]) -> Result<RuntimeValue, RuntimeError>,
    ) -> Self {
        Self(Rc::new(LoxNativeFunctionImpl {
            id,
            name,
            arity,
            implementation,
        }))
    }
}
impl LoxCallable for LoxNativeFunction {
    fn arity(&self) -> usize {
        self.0.arity
    }

    fn call<W: Write>(
        &self,
        _: &mut Interpreter<W>,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, RuntimeError> {
        (self.0.implementation)(args)
    }
}
impl Display for LoxNativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {}() {{ <native code> }}", self.0.name)
    }
}
impl Debug for LoxNativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {}() {{ <native code> }}", self.0.name)
    }
}
impl PartialEq for LoxNativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.0.id == other.0.id
    }
}
