use super::{
    chunk::{Chunk, CodeReadError, OpCode},
    value::Value,
};
use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
pub enum InterpreterError {
    #[error("Error reading bytecode: {0}")]
    CodeReadError(#[from] CodeReadError),
    #[error("Stack underflow")]
    StackUnderflow,
}

pub struct Vm {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
}
impl Vm {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: vec![],
        }
    }
    pub fn run(&mut self) -> Result<(), InterpreterError> {
        loop {
            if cfg!(debug_assertions) {
                self.chunk.disassemble_instruction_at(self.ip)?;
            }

            let op_code = self.next(Chunk::read_op_code)?;

            match op_code {
                OpCode::Return => {
                    let value = self.stack_pop()?;
                    println!("return: {:?}", value);

                    return Ok(());
                }
                OpCode::Constant => {
                    let constant = self.next(|a, b| a.read_constant_value(b))?.clone();
                    self.stack_push(constant.clone());
                }
                OpCode::Negate => {
                    let value = -self.stack_pop()?.cast_float();
                    self.stack_push(value.into());
                }
                OpCode::Add => {
                    let b = self.stack_pop()?.cast_float();
                    let a = self.stack_pop()?.cast_float();
                    let value = a + b;
                    self.stack_push(value.into());
                }
                OpCode::Subtract => {
                    let b = self.stack_pop()?.cast_float();
                    let a = self.stack_pop()?.cast_float();
                    let value = a - b;
                    self.stack_push(value.into());
                }
                OpCode::Multiply => {
                    let b = self.stack_pop()?.cast_float();
                    let a = self.stack_pop()?.cast_float();
                    let value = a * b;
                    self.stack_push(value.into());
                }
                OpCode::Divide => {
                    let b = self.stack_pop()?.cast_float();
                    let a = self.stack_pop()?.cast_float();
                    let value = a / b;
                    self.stack_push(value.into());
                }
            };

            if cfg!(debug_assertions) {
                println!("     | stack: {:?}", self.stack);
            }
        }
    }
    fn stack_push(&mut self, value: Value) {
        self.stack.push(value);
    }
    fn stack_pop(&mut self) -> Result<Value, InterpreterError> {
        self.stack.pop().ok_or(InterpreterError::StackUnderflow)
    }
    fn next<'a, T: 'a, F: FnOnce(&'a Chunk, usize) -> Result<(usize, T), CodeReadError>>(
        &'a mut self,
        read: F,
    ) -> Result<T, CodeReadError> {
        let (next_ip, value) = read(&self.chunk, self.ip)?;
        self.ip = next_ip;
        Ok(value)
    }
}
