use std::{convert::TryFrom, fmt::Display};

use miette::Diagnostic;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use thiserror::Error;

use crate::{SourceReference, SourceSpan};

use super::value::Value;

#[derive(Debug, Clone, Copy)]
pub struct ConstantAddress(u8);
impl Display for ConstantAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}

#[derive(Debug, IntoPrimitive, TryFromPrimitive, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    Return,
    Constant,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Print,
    Pop,
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
    constants: Vec<Value>,
    debug_data: Option<ChunkDebugData>,
}

impl Chunk {
    pub fn new(code: Vec<u8>, debug_source: Option<SourceReference>) -> Self {
        Self {
            code,
            constants: vec![],
            debug_data: debug_source.map(|source| ChunkDebugData {
                source_code: source,
                op_debugs: vec![],
            }),
        }
    }
    pub fn code(&self) -> &[u8] {
        &self.code[..]
    }
    pub fn get_constant_value(&self, address: ConstantAddress) -> Result<&Value, CodeReadError> {
        self.constants
            .get(address.0 as usize)
            .ok_or(CodeReadError::InvalidConstantAddress(address))
    }
    pub fn register_constant(&mut self, constant: impl Into<Value>) -> ConstantAddress {
        self.constants.push(constant.into());
        ConstantAddress(self.constants.len() as u8 - 1)
    }
    fn write_debug_data(&mut self, op_debug: impl Into<Option<OpDebug>>) {
        if let Some(debug_data) = &mut self.debug_data {
            debug_data.op_debugs.push(op_debug.into());
        }
    }
    pub fn write_basic_op(&mut self, op: OpCode, op_debug: impl Into<Option<OpDebug>>) {
        self.code.push(op.into());
        self.write_debug_data(op_debug);
    }
    pub fn write_constant(
        &mut self,
        value: impl Into<Value>,
        op_debug: impl Into<Option<OpDebug>>,
    ) {
        let address = self.register_constant(value);
        self.code.push(OpCode::Constant.into());
        self.code.push(address.0);
        self.write_debug_data(op_debug);
        self.write_debug_data(None);
    }
    pub fn read_byte(&self, offset: usize) -> Result<u8, CodeReadError> {
        self.code
            .get(offset)
            .cloned()
            .ok_or(CodeReadError::UnexpectedEnd)
    }
    pub fn read_op_debug(&self, offset: usize) -> Option<(&SourceReference, OpDebug)> {
        let debug_data = self.debug_data.as_ref()?;
        let source_span = debug_data.op_debugs.get(offset)?.clone()?;
        Some((&debug_data.source_code, source_span))
    }
    pub fn read_op_code(&self, offset: usize) -> Result<(usize, OpCode), CodeReadError> {
        let op_code = self.read_byte(offset)?;
        Ok((
            offset + 1,
            OpCode::try_from(op_code).map_err(|_| CodeReadError::InvalidOpCode(offset, op_code))?,
        ))
    }
    pub fn read_constant_address(
        &self,
        offset: usize,
    ) -> Result<(usize, ConstantAddress), CodeReadError> {
        let address = self.read_byte(offset)?;
        Ok((offset + 1, ConstantAddress(address)))
    }
    pub fn read_constant_value(&self, offset: usize) -> Result<(usize, &Value), CodeReadError> {
        let (offset, address) = self.read_constant_address(offset)?;
        Ok((offset, self.get_constant_value(address)?))
    }
    pub fn has_debug_data(&self) -> bool {
        self.debug_data.is_some()
    }
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
}
