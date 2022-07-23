use super::{RuntimeError, RuntimeValue};
use std::{
    convert::Infallible,
    ops::{ControlFlow, FromResidual, Try},
};

#[derive(Debug)]
pub enum AbruptCompletion {
    Return(RuntimeValue),
    Error(RuntimeError),
}
impl FromResidual<AbruptCompletion> for Result<RuntimeValue, RuntimeError> {
    fn from_residual(residual: AbruptCompletion) -> Self {
        match residual {
            AbruptCompletion::Return(value) => Ok(value),
            AbruptCompletion::Error(error) => Err(error),
        }
    }
}

pub enum Completion {
    Normal(RuntimeValue),
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
impl FromResidual<Result<Infallible, RuntimeError>> for Completion {
    fn from_residual(residual: Result<Infallible, RuntimeError>) -> Self {
        match residual {
            Ok(_) => unreachable!(),
            Err(error) => Completion::Error(error),
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
