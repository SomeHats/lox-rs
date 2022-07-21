use super::{Interpreter, RuntimeValue};
use crate::RuntimeError;
use std::io::Write;

pub trait LoxCallable {
    fn arity(&self) -> usize;
    fn call<W: Write>(
        &self,
        interpreter: &mut Interpreter<W>,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, RuntimeError>;
}
