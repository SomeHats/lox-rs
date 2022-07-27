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
    current_chunk: Option<Chunk>,
    ip: usize,
    stack: Vec<Value>,
}
impl Vm {
    pub fn new() -> Self {
        Self {
            current_chunk: None,
            ip: 0,
            stack: vec![],
        }
    }
    pub fn run(&mut self, chunk: Chunk) -> Result<Value, InterpreterError> {
        self.current_chunk = Some(chunk);
        self.ip = 0;

        while self.ip < self.current_chunk.as_ref().unwrap().code().len() {
            if cfg!(debug_assertions) {
                self.current_chunk
                    .as_ref()
                    .unwrap()
                    .disassemble_instruction_at(self.ip)?;
            }

            let op_code = self.next(Chunk::read_op_code)?;

            match op_code {
                OpCode::Return => {
                    let value = self.stack_pop()?;
                    println!("return: {:?}", value);

                    return Ok(value);
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
                OpCode::Print => {
                    let value = self.stack_pop()?;
                    println!("{:?}", value);
                }
            };

            // if cfg!(debug_assertions) {
            //     println!("     | stack: {:?}", self.stack);
            // }
        }

        if self.stack.len() == 0 {
            Ok(0.0.into())
        } else if self.stack.len() == 1 {
            Ok(self.stack.pop().unwrap())
        } else {
            panic!("too many values left on stack!");
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
        let (next_ip, value) = read(self.current_chunk.as_ref().unwrap(), self.ip)?;
        self.ip = next_ip;
        Ok(value)
    }
}
