use super::gc::{GcString, Trace};
use crate::{custom_trace_impl, empty_trace_impl, SourceReference, SourceSpan};
use miette::Diagnostic;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use ordered_float::OrderedFloat;
use std::{
    collections::HashMap,
    convert::{TryFrom, TryInto},
    fmt::Display,
};
use thiserror::Error;

#[derive(Debug, Clone, Copy)]
pub struct ConstantAddress(u8);
impl ConstantAddress {
    pub fn value(&self) -> u8 {
        self.0
    }
}
impl Display for ConstantAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}
impl Trace for ConstantAddress {
    empty_trace_impl!();
}

#[derive(Debug)]
#[must_use]
pub struct JumpPatchHandle(usize);
impl Display for JumpPatchHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub struct LoopTarget(usize);
impl Display for LoopTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantValue {
    Number(OrderedFloat<f64>),
    Boolean(bool),
    String(GcString),
}
impl Trace for ConstantValue {
    custom_trace_impl!(|value| match value {
        ConstantValue::Boolean(value) => mark(value),
        ConstantValue::Number(value) => mark(&value.0),
        ConstantValue::String(value) => mark(value),
    });
}

#[derive(Debug, IntoPrimitive, TryFromPrimitive, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    Return,
    Jump,
    JumpIfTrue,
    JumpIfFalse,
    Loop,
    Pop,
    Nil,
    Constant,
    DefineGlobal,
    ReadGlobal,
    SetGlobal,
    ReadLocal,
    SetLocal,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Print,
    Not,
    NotEqualTo,
    EqualTo,
    LessThan,
    LessThanOrEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
}

#[derive(Debug, Clone)]
pub struct OpDebug {
    pub inner: SourceSpan,
    pub outer: SourceSpan,
}
impl OpDebug {
    pub fn new(inner: SourceSpan, outer: SourceSpan) -> Self {
        Self { inner, outer }
    }
    pub fn single(inner: SourceSpan) -> Self {
        Self::new(inner, inner)
    }
}

#[derive(Debug)]
pub struct Chunk {
    code: Vec<u8>,
    constant_indexes: HashMap<ConstantValue, ConstantAddress>,
    constants: Vec<ConstantValue>,
    debug_data: Vec<Option<OpDebug>>,
    source: SourceReference,
}

impl Chunk {
    pub fn new(source: SourceReference) -> Self {
        Self {
            code: vec![],
            constants: vec![],
            constant_indexes: HashMap::new(),
            debug_data: vec![],
            source,
        }
    }
    pub fn code(&self) -> &[u8] {
        &self.code[..]
    }
    pub fn get_constant_value(
        &self,
        address: ConstantAddress,
    ) -> Result<&ConstantValue, CodeReadError> {
        self.constants
            .get(address.0 as usize)
            .ok_or(CodeReadError::InvalidConstantAddress(address))
    }
    pub fn register_constant(&mut self, constant: ConstantValue) -> ConstantAddress {
        let constant_indexes = &mut self.constant_indexes;
        let constants = &mut self.constants;
        *constant_indexes
            .entry(constant)
            .or_insert_with_key(|constant| {
                let address = ConstantAddress(constants.len() as u8);
                constants.push(constant.clone());
                address
            })
    }
    fn write_u8(&mut self, byte: u8, op_debug: impl Into<Option<OpDebug>>) {
        self.code.push(byte);
        self.debug_data.push(op_debug.into());
    }
    fn write_u16(&mut self, value: u16, op_debug: impl Into<Option<OpDebug>>) {
        let [byte1, byte2] = u16_to_bytes(value);
        self.write_u8(byte1, op_debug);
        self.write_u8(byte2, None);
    }
    pub fn write_basic_op(&mut self, op: OpCode, op_debug: OpDebug) {
        self.write_u8(op.into(), op_debug);
    }
    pub fn write_constant(&mut self, value: ConstantValue, op_debug: OpDebug) {
        let address = self.register_constant(value);
        self.write_u8(OpCode::Constant.into(), op_debug);
        self.write_u8(address.0, None);
    }
    pub fn write_global_op(&mut self, op_code: OpCode, name: GcString, op_debug: OpDebug) {
        let address = self.register_constant(ConstantValue::String(name));
        self.write_u8(op_code.into(), op_debug);
        self.write_u8(address.0, None);
    }
    pub fn write_local_op(&mut self, op_code: OpCode, index: u8, op_debug: OpDebug) {
        self.write_u8(op_code.into(), op_debug);
        self.write_u8(index, None);
    }
    pub fn write_jump_op(&mut self, op_code: OpCode, op_debug: OpDebug) -> JumpPatchHandle {
        let target = JumpPatchHandle(self.code.len());
        self.write_u8(op_code.into(), op_debug);
        self.write_u16(0, None);
        target
    }
    pub fn write_loop_op(&mut self, target: LoopTarget, op_debug: OpDebug) {
        let offset = (self.code.len() - target.0).try_into().unwrap();
        self.write_u8(OpCode::Loop.into(), op_debug);
        self.write_u16(offset, None);
    }
    pub fn patch_jump_op_to_here(&mut self, jump_target: JumpPatchHandle) {
        let (target_idx, op_code) = self.read_op_code(jump_target.0).unwrap();
        let arg = match op_code {
            OpCode::Jump | OpCode::JumpIfTrue | OpCode::JumpIfFalse => {
                self.code.len() - jump_target.0
            }
            _ => panic!("Tried to patch a non-jump opcode"),
        };

        let [b1, b2] = u16_to_bytes(arg.try_into().expect("jump over too much code"));
        self.code[target_idx] = b1;
        self.code[target_idx + 1] = b2;
    }

    pub fn source(&self) -> &SourceReference {
        &self.source
    }
    fn read_byte(&self, offset: usize) -> Result<u8, CodeReadError> {
        self.code
            .get(offset)
            .cloned()
            .ok_or(CodeReadError::UnexpectedEnd)
    }
    pub fn read_u8(&self, offset: usize) -> Result<(usize, u8), CodeReadError> {
        let byte = self.read_byte(offset)?;
        Ok((offset + 1, byte))
    }
    pub fn read_u16(&self, offset: usize) -> Result<(usize, u16), CodeReadError> {
        let byte1 = self.read_byte(offset)?;
        let byte2 = self.read_byte(offset + 1)?;
        Ok((offset + 2, u16_from_bytes([byte1, byte2])))
    }
    pub fn read_op_debug(&self, offset: usize) -> Option<&OpDebug> {
        self.debug_data.get(offset)?.as_ref()
    }
    pub fn read_op_code(&self, offset: usize) -> Result<(usize, OpCode), CodeReadError> {
        let (offset, op_code) = self.read_u8(offset)?;
        Ok((
            offset,
            OpCode::try_from(op_code).map_err(|_| CodeReadError::InvalidOpCode(offset, op_code))?,
        ))
    }
    pub fn read_constant_address(
        &self,
        offset: usize,
    ) -> Result<(usize, ConstantAddress), CodeReadError> {
        let (offset, address) = self.read_u8(offset)?;
        Ok((offset, ConstantAddress(address)))
    }
    pub fn read_constant_value(
        &self,
        offset: usize,
    ) -> Result<(usize, ConstantValue), CodeReadError> {
        let (offset, address) = self.read_constant_address(offset)?;
        Ok((offset, self.get_constant_value(address)?.clone()))
    }
    pub fn read_global_name(&self, offset: usize) -> Result<(usize, GcString), CodeReadError> {
        let (offset, name) = self.read_constant_value(offset)?;
        match name {
            ConstantValue::String(name) => Ok((offset, name)),
            _ => Err(CodeReadError::InvalidGlobalName(offset)),
        }
    }
    pub fn get_loop_target(&self) -> LoopTarget {
        LoopTarget(self.code.len())
    }
}
impl Trace for Chunk {
    custom_trace_impl!(|this| {
        mark(&this.constants);
        mark(&this.constant_indexes);
    });
}

#[derive(Debug)]
pub struct ChunkDebugData {
    pub source_code: SourceReference,
    pub op_debugs: Vec<Option<OpDebug>>,
}

#[derive(Debug, Error, Diagnostic)]
pub enum CodeReadError {
    #[error("Unexpected end of code")]
    UnexpectedEnd,
    #[error("Unexpected opcode {1} at index {0}")]
    InvalidOpCode(usize, u8),
    #[error("Unknown constant {0}")]
    InvalidConstantAddress(ConstantAddress),
    #[error("Invalid global name at index {0}")]
    InvalidGlobalName(usize),
}

fn u16_to_bytes(value: u16) -> [u8; 2] {
    value.to_le_bytes()
}
fn u16_from_bytes(bytes: [u8; 2]) -> u16 {
    u16::from_le_bytes(bytes)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn read_write_u16() {
        for i in 0..=0xFFFF {
            assert_eq!(i, u16_from_bytes(u16_to_bytes(i)));
        }
    }
}
