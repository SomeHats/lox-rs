use super::{RuntimeError, RuntimeValue};
use std::ops::{ControlFlow, FromResidual, Try};

pub enum Completion {
    Normal(RuntimeValue),
    Return(RuntimeValue),
    Error(RuntimeError),
}

#[derive(Debug)]
pub enum AbruptCompletion {
    Return(RuntimeValue),
    Error(RuntimeError),
}
impl FromResidual for Completion {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        match residual {
            AbruptCompletion::Return(value) => Completion::Return(value),
            AbruptCompletion::Error(err) => Completion::Error(err),
        }
    }
}
impl Try for Completion {
    type Output = RuntimeValue;

    type Residual = AbruptCompletion;

    fn from_output(output: Self::Output) -> Self {
        Completion::Normal(output)
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            Completion::Normal(value) => ControlFlow::Continue(value),
            Completion::Return(value) => ControlFlow::Break(AbruptCompletion::Return(value)),
            Completion::Error(err) => ControlFlow::Break(AbruptCompletion::Error(err)),
        }
    }
}
impl From<Result<RuntimeValue, RuntimeError>> for Completion {
    fn from(result: Result<RuntimeValue, RuntimeError>) -> Self {
        match result {
            Ok(value) => Completion::Normal(value),
            Err(err) => Completion::Error(err),
        }
    }
}
