use super::{
    chunk::{Chunk, CodeReadError, OpCode, OpDebug},
    value::{Value, ValueDescriptor, ValueTables, ValueType},
};
use crate::{ast::LiteralValue, SourceReference, SourceSpan};
use itertools::Itertools;
use miette::Diagnostic;
use std::{io::Write, mem::replace};
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

pub struct Vm<'a, Stdout: Write> {
    current_chunk: Option<Chunk>,
    ip: usize,
    stack: Vec<(usize, Value)>,
    ip_at_op_start: usize,
    last_popped: Value,
    stdout: &'a mut Stdout,
    tables: ValueTables,
}
impl<'vm, Stdout: Write> Vm<'vm, Stdout> {
    pub fn new(stdout: &'vm mut Stdout) -> Self {
        Self {
            current_chunk: None,
            ip: 0,
            stack: vec![],
            ip_at_op_start: 0,
            last_popped: Value::Nil,
            stdout,
            tables: ValueTables::default(),
        }
    }
    pub fn run(&mut self, chunk: Chunk) -> Result<String, InterpreterError> {
        self.current_chunk = Some(chunk);
        self.ip = 0;

        macro_rules! next {
            ($name:ident) => {{
                let (next_ip, value) = self.current_chunk.as_ref().unwrap().$name(self.ip)?;
                self.ip = next_ip;
                value
            }};
        }

        while self.ip < self.current_chunk.as_ref().unwrap().code().len() {
            if cfg!(feature = "debug") {
                self.current_chunk().disassemble_instruction_at(self.ip)?;
            }

            self.ip_at_op_start = self.ip;
            let op_code = next!(read_op_code);

            match op_code {
                OpCode::Return => {
                    let value = self.stack_pop()?.1;
                    println!("return: {:?}", value);

                    return Ok(value.to_debug_string(&self.tables));
                }
                OpCode::Constant => {
                    let constant = next!(read_constant_value);
                    let value = match constant {
                        LiteralValue::Nil => Value::Nil,
                        LiteralValue::Boolean(value) => Value::Boolean(*value),
                        LiteralValue::Number(value) => Value::Number(*value),
                        LiteralValue::String(value) => {
                            self.tables.strings.allocate(value.clone()).into()
                        }
                    };
                    self.stack_push(value);
                }
                OpCode::Print => {
                    let value = self.stack_pop()?.1;
                    writeln!(self.stdout, "{}", value.to_string(&self.tables)).unwrap();
                }
                OpCode::Pop => {
                    self.stack_pop()?;
                }
                OpCode::Negate => {
                    let (operand_loc, operand) = self.stack_pop()?;
                    let operand = operand.as_number().ok_or_else(|| {
                        self.operand_type_error(
                            ValueType::Number.into(),
                            &operand,
                            operand_loc,
                            "-",
                        )
                    })?;
                    self.stack_push(-operand);
                }
                OpCode::Add => {
                    let (rhs_loc, rhs) = self.stack_pop()?;
                    let (lhs_loc, lhs) = self.stack_pop()?;

                    let result: Value = match lhs {
                        Value::Number(lhs) => {
                            let rhs = rhs.as_number().ok_or_else(|| {
                                self.operand_type_error(
                                    ValueType::Number.into(),
                                    &rhs,
                                    rhs_loc,
                                    "+",
                                )
                            })?;
                            (lhs + rhs).into()
                        }
                        Value::String(lhs) => {
                            let rhs = rhs.as_string().ok_or_else(|| {
                                self.operand_type_error(
                                    ValueType::String.into(),
                                    &rhs,
                                    rhs_loc,
                                    "+",
                                )
                            })?;
                            let result = format!(
                                "{}{}",
                                self.tables.strings.get(&lhs),
                                self.tables.strings.get(rhs)
                            );
                            self.tables.strings.allocate(result).into()
                        }
                        _ => {
                            return Err(self.operand_type_error(
                                ValueDescriptor::AnyOf(vec![ValueType::String, ValueType::Number]),
                                &lhs,
                                lhs_loc,
                                "+",
                            ));
                        }
                    };

                    self.stack_push(result);
                }
                OpCode::Subtract => {
                    let (rhs_loc, rhs) = self.stack_pop()?;
                    let (lhs_loc, lhs) = self.stack_pop()?;

                    let lhs = lhs.as_number().ok_or_else(|| {
                        self.operand_type_error(ValueType::Number.into(), &lhs, lhs_loc, "-")
                    })?;
                    let rhs = rhs.as_number().ok_or_else(|| {
                        self.operand_type_error(ValueType::Number.into(), &rhs, rhs_loc, "-")
                    })?;

                    self.stack_push(lhs - rhs);
                }
                OpCode::Multiply => {
                    let (rhs_loc, rhs) = self.stack_pop()?;
                    let (lhs_loc, lhs) = self.stack_pop()?;

                    let lhs = lhs.as_number().ok_or_else(|| {
                        self.operand_type_error(ValueType::Number.into(), &lhs, lhs_loc, "*")
                    })?;
                    let rhs = rhs.as_number().ok_or_else(|| {
                        self.operand_type_error(ValueType::Number.into(), &rhs, rhs_loc, "*")
                    })?;

                    self.stack_push(lhs * rhs);
                }
                OpCode::Divide => {
                    let (rhs_loc, rhs) = self.stack_pop()?;
                    let (lhs_loc, lhs) = self.stack_pop()?;

                    let lhs = lhs.as_number().ok_or_else(|| {
                        self.operand_type_error(ValueType::Number.into(), &lhs, lhs_loc, "/")
                    })?;
                    let rhs = rhs.as_number().ok_or_else(|| {
                        self.operand_type_error(ValueType::Number.into(), &rhs, rhs_loc, "/")
                    })?;

                    self.stack_push(lhs / rhs);
                }
                OpCode::Not => {
                    let (_, operand) = self.stack_pop()?;
                    self.stack_push(!operand.cast_boolean())
                }
                OpCode::EqualTo => {
                    let (_, rhs) = self.stack_pop()?;
                    let (_, lhs) = self.stack_pop()?;

                    self.stack_push(lhs.eq(&rhs, &self.tables));
                }
                OpCode::NotEqualTo => {
                    let (_, rhs) = self.stack_pop()?;
                    let (_, lhs) = self.stack_pop()?;

                    self.stack_push(!lhs.eq(&rhs, &self.tables));
                }
                OpCode::LessThan => {
                    let (rhs_loc, rhs) = self.stack_pop()?;
                    let (lhs_loc, lhs) = self.stack_pop()?;

                    let lhs = lhs.as_number().ok_or_else(|| {
                        self.operand_type_error(ValueType::Number.into(), &lhs, lhs_loc, "<")
                    })?;
                    let rhs = rhs.as_number().ok_or_else(|| {
                        self.operand_type_error(ValueType::Number.into(), &rhs, rhs_loc, "<")
                    })?;

                    self.stack_push(lhs < rhs);
                }
                OpCode::LessThanOrEqualTo => {
                    let (rhs_loc, rhs) = self.stack_pop()?;
                    let (lhs_loc, lhs) = self.stack_pop()?;

                    let lhs = lhs.as_number().ok_or_else(|| {
                        self.operand_type_error(ValueType::Number.into(), &lhs, lhs_loc, "<=")
                    })?;
                    let rhs = rhs.as_number().ok_or_else(|| {
                        self.operand_type_error(ValueType::Number.into(), &rhs, rhs_loc, "<=")
                    })?;

                    self.stack_push(lhs <= rhs);
                }
                OpCode::GreaterThan => {
                    let (rhs_loc, rhs) = self.stack_pop()?;
                    let (lhs_loc, lhs) = self.stack_pop()?;

                    let lhs = lhs.as_number().ok_or_else(|| {
                        self.operand_type_error(ValueType::Number.into(), &lhs, lhs_loc, ">")
                    })?;
                    let rhs = rhs.as_number().ok_or_else(|| {
                        self.operand_type_error(ValueType::Number.into(), &rhs, rhs_loc, ">")
                    })?;

                    self.stack_push(lhs > rhs);
                }
                OpCode::GreaterThanOrEqualTo => {
                    let (rhs_loc, rhs) = self.stack_pop()?;
                    let (lhs_loc, lhs) = self.stack_pop()?;

                    let lhs = lhs.as_number().ok_or_else(|| {
                        self.operand_type_error(ValueType::Number.into(), &lhs, lhs_loc, ">=")
                    })?;
                    let rhs = rhs.as_number().ok_or_else(|| {
                        self.operand_type_error(ValueType::Number.into(), &rhs, rhs_loc, ">=")
                    })?;

                    self.stack_push(lhs >= rhs);
                }
                OpCode::LogicalAnd => {
                    let (_, rhs) = self.stack_pop()?;
                    let (_, lhs) = self.stack_pop()?;

                    self.stack_push(lhs.cast_boolean() && rhs.cast_boolean());
                }
                OpCode::LogicalOr => {
                    let (_, rhs) = self.stack_pop()?;
                    let (_, lhs) = self.stack_pop()?;

                    self.stack_push(lhs.cast_boolean() || rhs.cast_boolean());
                }
            };

            if cfg!(feature = "debug_stack") {
                println!(
                    "     | stack: {}",
                    self.stack
                        .iter()
                        .map(|(_, value)| value.to_debug_string(&self.tables))
                        .join(", ")
                );
            }
        }

        if self.stack.is_empty() {
            let last_popped = replace(&mut self.last_popped, Value::Nil);
            Ok(last_popped.to_debug_string(&self.tables))
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
    fn current_chunk(&self) -> &Chunk {
        self.current_chunk.as_ref().unwrap()
    }
    fn current_op_debug(&self) -> &OpDebug {
        self.current_chunk()
            .read_op_debug(self.ip_at_op_start)
            .unwrap()
    }
    fn operand_type_error(
        &self,
        expected: ValueDescriptor,
        actual_value: &Value,
        value_loc: usize,
        operator: &str,
    ) -> InterpreterError {
        InterpreterError::OperandTypeError {
            expected_type: expected,
            actual_type: actual_value.type_of(),
            operand_loc: self.current_chunk().read_op_debug(value_loc).unwrap().outer,
            operator: operator.to_string(),
            operator_loc: self.current_op_debug().inner,
            source_code: self.current_chunk().source().clone(),
        }
    }
}
