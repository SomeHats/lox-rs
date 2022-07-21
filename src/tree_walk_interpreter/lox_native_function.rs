use super::{lox_callable::LoxCallable, Interpreter, RuntimeValue};
use crate::RuntimeError;
use std::{
    fmt::{Debug, Display},
    io::Write,
};

pub struct LoxNativeFunction {
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
        Self {
            id,
            name,
            arity,
            implementation,
        }
    }
}
impl LoxCallable for LoxNativeFunction {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call<W: Write>(
        &self,
        _: &mut Interpreter<W>,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, RuntimeError> {
        (self.implementation)(args)
    }
}
impl Display for LoxNativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {}() {{ <native code> }}", self.name)
    }
}
impl Debug for LoxNativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {}() {{ <native code> }}", self.name)
    }
}
impl PartialEq for LoxNativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
