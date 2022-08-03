use super::{
    chunk::{CodeReadError, ConstantValue, OpCode, OpDebug},
    function::FunctionObj,
    gc::GcString,
    value::{Value, ValueDescriptor, ValueType},
};
use crate::{fixed_arr::FixedArr, vm_interpreter::disassembler, SourceReference, SourceSpan};
use itertools::Itertools;
use miette::Diagnostic;
use std::{
    collections::HashMap,
    io::Write,
    mem::replace,
    ops::{Deref, DerefMut},
};
use thiserror::Error;

const FRAMES_MAX: usize = 64;

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
    #[error("Undefined variable {name}")]
    UndefinedVariable {
        name: String,
        #[label("found here")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
}

struct Location {
    fun: FunctionObj,
    instruction_index: usize,
}

pub struct Vm<'a, Stdout: Write> {
    stack: Vec<(Location, Value)>,
    last_pushed: Value,
    stdout: &'a mut Stdout,
    globals: HashMap<GcString, Value>,
    call_frames: FixedArr<CallFrame, FRAMES_MAX>,
}
impl<'vm, Stdout: Write> Vm<'vm, Stdout> {
    pub fn new(stdout: &'vm mut Stdout) -> Self {
        Self {
            stack: vec![],
            last_pushed: Value::Nil,
            stdout,
            globals: HashMap::new(),
            call_frames: FixedArr::new(),
        }
    }
    pub fn run_script<'a>(&'a mut self, fun: FunctionObj) -> Result<Value, InterpreterError>
    where
        'vm: 'a,
    {
        assert!(fun.is_script());

        self.call_frames
            .push(CallFrame {
                fun: fun.clone(),
                ip: 0,
                stack_offset: 0,
            })
            .unwrap();

        RunningCallFrame::new(self, 0).run()?;

        if self.stack.is_empty() {
            let last_popped = replace(&mut self.last_pushed, Value::Nil);
            Ok(last_popped)
        } else {
            panic!("too many values left on stack")
        }
    }
}

#[derive(Debug)]
struct CallFrame {
    fun: FunctionObj,
    ip: usize,
    stack_offset: usize,
}

struct RunningCallFrame<'a, 'vm: 'a, Stdout: Write> {
    interpreter: &'a mut Vm<'vm, Stdout>,
    frame_idx: usize,
    ip_at_op_start: usize,
}
impl<'a, 'vm: 'a, Stdout: Write> RunningCallFrame<'a, 'vm, Stdout> {
    fn new(interpreter: &'a mut Vm<'vm, Stdout>, frame_idx: usize) -> Self {
        Self {
            ip_at_op_start: interpreter.call_frames[frame_idx].ip,
            interpreter,
            frame_idx,
        }
    }
    fn run(mut self) -> Result<Value, InterpreterError> {
        while self.ip < self.fun.chunk().code().len() {
            self.step()?;
        }
        Ok(Value::Nil)
    }
    fn step(&mut self) -> Result<Option<Value>, InterpreterError> {
        macro_rules! next {
            ($name:ident) => {{
                let (next_ip, value) = self.interpreter.call_frames[self.frame_idx]
                    .fun
                    .chunk()
                    .$name(self.ip)?;
                self.ip = next_ip;
                value
            }};
        }

        if cfg!(feature = "debug") {
            disassembler::disassemble_instruction_at(&self.fun, self.ip)?;
        }

        self.ip_at_op_start = self.ip;
        let op_code = next!(read_op_code);

        match op_code {
            OpCode::Return => {
                let value = self.stack_pop()?.1;
                println!("return: {:?}", value);

                return Ok(Some(value));
            }
            OpCode::Jump => {
                let offset = next!(read_u16);
                self.ip = self.ip_at_op_start + usize::from(offset);
            }
            OpCode::JumpIfTrue => {
                let offset = next!(read_u16);
                let (_, value) = self.stack_peek()?;
                if value.cast_boolean() {
                    self.ip = self.ip_at_op_start + usize::from(offset);
                }
            }
            OpCode::JumpIfFalse => {
                let offset = next!(read_u16);
                let (_, value) = self.stack_peek()?;
                if !value.cast_boolean() {
                    self.ip = self.ip_at_op_start + usize::from(offset);
                }
            }
            OpCode::Loop => {
                let offset = next!(read_u16);
                self.ip = self.ip_at_op_start - usize::from(offset);
            }
            OpCode::Nil => self.stack_push(Value::Nil),
            OpCode::Constant => {
                let constant = next!(read_constant_value);
                let value: Value = match constant.clone() {
                    ConstantValue::Boolean(value) => value.into(),
                    ConstantValue::Number(value) => value.into(),
                    ConstantValue::String(value) => value.into(),
                };
                self.stack_push(value);
            }
            OpCode::DefineGlobal => {
                let value = self.stack_pop()?.1;
                let name = next!(read_global_name);
                self.interpreter.globals.insert(name.clone(), value);
            }
            OpCode::ReadGlobal => {
                let name = next!(read_global_name);
                let value = self.interpreter.globals.get(&name).ok_or_else(|| {
                    InterpreterError::UndefinedVariable {
                        name: name.to_string(),
                        found_at: self.current_op_debug().inner,
                        source_code: self.fun.chunk().source().clone(),
                    }
                })?;
                self.stack_push(value.clone());
            }
            OpCode::SetGlobal => {
                let name = next!(read_global_name);
                let value = self.stack_peek()?.1;
                if self.interpreter.globals.contains_key(&name) {
                    self.interpreter.globals.insert(name.clone(), value.clone());
                } else {
                    return Err(InterpreterError::UndefinedVariable {
                        name: name.to_string(),
                        found_at: self.current_op_debug().inner,
                        source_code: self.fun.chunk().source().clone(),
                    });
                }
            }
            OpCode::ReadLocal => {
                let index = next!(read_u8);
                self.stack_push(self.stack_get(usize::from(index)).1.clone())
            }
            OpCode::SetLocal => {
                let index = next!(read_u8);
                let value = self.stack_peek()?.1;
                *self.stack_get_mut(usize::from(index)) = (self.current_location(), value.clone());
            }
            OpCode::Print => {
                let value = self.stack_pop()?.1;
                writeln!(self.interpreter.stdout, "{}", value).unwrap();
            }
            OpCode::Pop => {
                self.stack_pop()?;
            }
            OpCode::Negate => {
                let (operand_loc, operand) = self.stack_pop()?;
                let operand = operand.as_number().ok_or_else(|| {
                    self.make_operand_type_error(
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
                            self.make_operand_type_error(
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
                            self.make_operand_type_error(
                                ValueType::String.into(),
                                &rhs,
                                rhs_loc,
                                "+",
                            )
                        })?;
                        GcString::new(format!("{}{}", lhs, rhs)).into()
                    }
                    _ => {
                        return Err(self.make_operand_type_error(
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
                    self.make_operand_type_error(ValueType::Number.into(), &lhs, lhs_loc, "-")
                })?;
                let rhs = rhs.as_number().ok_or_else(|| {
                    self.make_operand_type_error(ValueType::Number.into(), &rhs, rhs_loc, "-")
                })?;

                self.stack_push(lhs - rhs);
            }
            OpCode::Multiply => {
                let (rhs_loc, rhs) = self.stack_pop()?;
                let (lhs_loc, lhs) = self.stack_pop()?;

                let lhs = lhs.as_number().ok_or_else(|| {
                    self.make_operand_type_error(ValueType::Number.into(), &lhs, lhs_loc, "*")
                })?;
                let rhs = rhs.as_number().ok_or_else(|| {
                    self.make_operand_type_error(ValueType::Number.into(), &rhs, rhs_loc, "*")
                })?;

                self.stack_push(lhs * rhs);
            }
            OpCode::Divide => {
                let (rhs_loc, rhs) = self.stack_pop()?;
                let (lhs_loc, lhs) = self.stack_pop()?;

                let lhs = lhs.as_number().ok_or_else(|| {
                    self.make_operand_type_error(ValueType::Number.into(), &lhs, lhs_loc, "/")
                })?;
                let rhs = rhs.as_number().ok_or_else(|| {
                    self.make_operand_type_error(ValueType::Number.into(), &rhs, rhs_loc, "/")
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

                self.stack_push(lhs == rhs);
            }
            OpCode::NotEqualTo => {
                let (_, rhs) = self.stack_pop()?;
                let (_, lhs) = self.stack_pop()?;

                self.stack_push(lhs != rhs);
            }
            OpCode::LessThan => {
                let (rhs_loc, rhs) = self.stack_pop()?;
                let (lhs_loc, lhs) = self.stack_pop()?;

                let lhs = lhs.as_number().ok_or_else(|| {
                    self.make_operand_type_error(ValueType::Number.into(), &lhs, lhs_loc, "<")
                })?;
                let rhs = rhs.as_number().ok_or_else(|| {
                    self.make_operand_type_error(ValueType::Number.into(), &rhs, rhs_loc, "<")
                })?;

                self.stack_push(lhs < rhs);
            }
            OpCode::LessThanOrEqualTo => {
                let (rhs_loc, rhs) = self.stack_pop()?;
                let (lhs_loc, lhs) = self.stack_pop()?;

                let lhs = lhs.as_number().ok_or_else(|| {
                    self.make_operand_type_error(ValueType::Number.into(), &lhs, lhs_loc, "<=")
                })?;
                let rhs = rhs.as_number().ok_or_else(|| {
                    self.make_operand_type_error(ValueType::Number.into(), &rhs, rhs_loc, "<=")
                })?;

                self.stack_push(lhs <= rhs);
            }
            OpCode::GreaterThan => {
                let (rhs_loc, rhs) = self.stack_pop()?;
                let (lhs_loc, lhs) = self.stack_pop()?;

                let lhs = lhs.as_number().ok_or_else(|| {
                    self.make_operand_type_error(ValueType::Number.into(), &lhs, lhs_loc, ">")
                })?;
                let rhs = rhs.as_number().ok_or_else(|| {
                    self.make_operand_type_error(ValueType::Number.into(), &rhs, rhs_loc, ">")
                })?;

                self.stack_push(lhs > rhs);
            }
            OpCode::GreaterThanOrEqualTo => {
                let (rhs_loc, rhs) = self.stack_pop()?;
                let (lhs_loc, lhs) = self.stack_pop()?;

                let lhs = lhs.as_number().ok_or_else(|| {
                    self.make_operand_type_error(ValueType::Number.into(), &lhs, lhs_loc, ">=")
                })?;
                let rhs = rhs.as_number().ok_or_else(|| {
                    self.make_operand_type_error(ValueType::Number.into(), &rhs, rhs_loc, ">=")
                })?;

                self.stack_push(lhs >= rhs);
            }
        };

        if cfg!(feature = "debug_stack") {
            println!(
                "     | stack after: {}",
                self.interpreter
                    .stack
                    .iter()
                    .map(|(_, value)| format!("{:?}", value))
                    .join(", ")
            );
        }

        Ok(None)
    }
    fn current_location(&self) -> Location {
        Location {
            fun: self.fun.clone(),
            instruction_index: self.ip_at_op_start,
        }
    }
    fn current_op_debug(&self) -> &OpDebug {
        self.fun.chunk().read_op_debug(self.ip_at_op_start).unwrap()
    }
    fn stack_push(&mut self, value: impl Into<Value>) {
        let value = value.into();
        self.interpreter.last_pushed = value.clone();
        self.interpreter
            .stack
            .push((self.current_location(), value));
    }
    fn stack_pop(&mut self) -> Result<(Location, Value), InterpreterError> {
        self.interpreter
            .stack
            .pop()
            .ok_or(InterpreterError::StackUnderflow)
    }
    fn stack_peek(&self) -> Result<(&Location, &Value), InterpreterError> {
        self.interpreter
            .stack
            .last()
            .map(|(a, b)| (a, b))
            .ok_or(InterpreterError::StackUnderflow)
    }
    fn stack_get(&self, offset: usize) -> &(Location, Value) {
        &self.interpreter.stack[self.stack_offset + offset]
    }
    fn stack_get_mut(&mut self, offset: usize) -> &mut (Location, Value) {
        let idx = self.stack_offset + offset;
        &mut self.interpreter.stack[idx]
    }

    fn make_operand_type_error(
        &self,
        expected: ValueDescriptor,
        actual_value: &Value,
        value_loc: Location,
        operator: &str,
    ) -> InterpreterError {
        InterpreterError::OperandTypeError {
            expected_type: expected,
            actual_type: actual_value.type_of(),
            operand_loc: self
                .fun
                .chunk()
                .read_op_debug(value_loc.instruction_index)
                .unwrap()
                .outer,
            operator: operator.to_string(),
            operator_loc: self.current_op_debug().inner,
            source_code: self.fun.chunk().source().clone(),
        }
    }
}
impl<Stdout: Write> Deref for RunningCallFrame<'_, '_, Stdout> {
    type Target = CallFrame;
    fn deref(&self) -> &Self::Target {
        &self.interpreter.call_frames[self.frame_idx]
    }
}
impl<Stdout: Write> DerefMut for RunningCallFrame<'_, '_, Stdout> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.interpreter.call_frames[self.frame_idx]
    }
}
