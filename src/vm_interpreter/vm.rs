use super::{
    chunk::{Chunk, CodeReadError, OpCode},
    value::{Value, ValueDescriptor, ValueType},
};
use crate::{SourceReference, SourceSpan};
use miette::Diagnostic;
use std::mem::replace;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
pub enum InterpreterError {
    #[error("Error reading bytecode: {0}")]
    CodeReadError(#[from] CodeReadError),
    #[error("Stack underflow")]
    StackUnderflow,
    #[error("Operand must be {}, but found {}", .expected_type.fmt_a(), .actual_type.fmt_a())]
    OperandTypeError {
        expected_type: ValueDescriptor,
        actual_type: ValueType,
        #[label("{} was found here", .actual_type.fmt_a())]
        operand_loc: SourceSpan,
        operator: String,
        #[label("the '{operator}' operator expected {}", .expected_type.fmt_a())]
        operator_loc: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
}

pub struct Vm {
    current_chunk: Option<Chunk>,
    ip: usize,
    stack: Vec<(usize, Value)>,
    ip_at_op_start: usize,
    last_popped: Value,
}
impl Default for Vm {
    fn default() -> Self {
        Self {
            current_chunk: None,
            ip: 0,
            stack: vec![],
            ip_at_op_start: 0,
            last_popped: Value::Nil,
        }
    }
}
impl Vm {
    pub fn new() -> Self {
        Self::default()
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

            self.ip_at_op_start = self.ip;
            let op_code = self.next(Chunk::read_op_code)?;

            match op_code {
                OpCode::Return => {
                    let value = self.stack_pop()?.1;
                    println!("return: {:?}", value);

                    return Ok(value);
                }
                OpCode::Constant => {
                    let constant = self.next(|a, b| a.read_constant_value(b))?.clone();
                    self.stack_push(constant.clone());
                }
                OpCode::Negate => {
                    let value = -self.stack_pop()?.1.cast_float();
                    self.stack_push(value);
                }
                OpCode::Add => {
                    let b = self.stack_pop()?.1.cast_float();
                    let a = self.stack_pop()?.1.cast_float();
                    let value = a + b;
                    self.stack_push(value);
                }
                OpCode::Subtract => {
                    let b = self.stack_pop()?.1.cast_float();
                    let a = self.stack_pop()?.1.cast_float();
                    let value = a - b;
                    self.stack_push(value);
                }
                OpCode::Multiply => {
                    let b = self.stack_pop()?.1.cast_float();
                    let a = self.stack_pop()?.1.cast_float();
                    let value = a * b;
                    self.stack_push(value);
                }
                OpCode::Divide => {
                    let b = self.stack_pop()?.1.cast_float();
                    let a = self.stack_pop()?.1.cast_float();
                    let value = a / b;
                    self.stack_push(value);
                }
                OpCode::Print => {
                    let value = self.stack_pop()?;
                    println!("{:?}", value);
                }
                OpCode::Pop => {
                    self.stack_pop()?;
                }
            };

            if cfg!(debug_assertions) {
                println!("     | stack: {:?}", self.stack);
            }
        }

        if self.stack.is_empty() {
            let last_popped = replace(&mut self.last_popped, Value::Nil);
            Ok(last_popped)
        } else {
            panic!("too many values left on stack")
        }
    }
    fn stack_push(&mut self, value: impl Into<Value>) {
        self.stack.push((self.ip_at_op_start, value.into()));
    }
    fn stack_pop(&mut self) -> Result<(usize, Value), InterpreterError> {
        let (addr, value) = self.stack.pop().ok_or(InterpreterError::StackUnderflow)?;
        self.last_popped = value.clone();
        Ok((addr, value))
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
