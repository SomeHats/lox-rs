use crate::custom_trace_impl;

use super::{
    chunk::ConstantValue,
    function::FunctionObj,
    gc::{GcString, Trace},
};
use itertools::Itertools;
use ordered_float::OrderedFloat;
use std::fmt::{Debug, Display};

#[derive(Clone)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    String(GcString),
    Function(FunctionObj),
}
impl Trace for Value {
    custom_trace_impl!(|value| match value {
        Value::Nil | Value::Number(_) | Value::Boolean(_) => (),
        Value::String(value) => mark(value),
        Value::Function(value) => mark(value),
    });
}
impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}
impl From<OrderedFloat<f64>> for Value {
    fn from(value: OrderedFloat<f64>) -> Self {
        Self::Number(value.into())
    }
}
impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}
impl From<()> for Value {
    fn from(_: ()) -> Self {
        Self::Nil
    }
}
impl From<GcString> for Value {
    fn from(value: GcString) -> Self {
        Self::String(value)
    }
}
impl From<FunctionObj> for Value {
    fn from(value: FunctionObj) -> Self {
        Self::Function(value)
    }
}
impl From<ConstantValue> for Value {
    fn from(value: ConstantValue) -> Self {
        match value {
            ConstantValue::Number(value) => value.into(),
            ConstantValue::Boolean(value) => value.into(),
            ConstantValue::String(value) => value.into(),
        }
    }
}
impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::String(value) => write!(f, "\"{}\"", value),
            value => write!(f, "{}", value),
        }
    }
}
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Number(value) => write!(f, "{}", value),
            Value::Boolean(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{}", value),
            Value::Function(value) => write!(f, "fun {}() {{ ... }}", value.name()),
        }
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Function(a), Value::Function(b)) => a == b,
            _ => false,
        }
    }
}
impl Value {
    pub fn as_number(&self) -> Option<f64> {
        match self {
            Self::Number(value) => Some(*value),
            _ => None,
        }
    }
    pub fn as_string(&self) -> Option<&GcString> {
        match self {
            Self::String(value) => Some(value),
            _ => None,
        }
    }
    pub fn type_of(&self) -> ValueType {
        match self {
            Self::Nil => ValueType::Nil,
            Self::Number(_) => ValueType::Number,
            Self::Boolean(_) => ValueType::Boolean,
            Self::String(_) => ValueType::String,
            Self::Function(_) => ValueType::Function,
        }
    }
    pub fn cast_boolean(&self) -> bool {
        match self {
            Self::Nil => false,
            Self::Boolean(value) => *value,
            _ => true,
        }
    }
}

#[derive(Debug)]
pub enum ValueType {
    String,
    Number,
    Boolean,
    Nil,
    Function,
    Class,
    Object,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ValueType::String => "string",
            ValueType::Number => "number",
            ValueType::Boolean => "boolean",
            ValueType::Nil => "nil",
            ValueType::Function => "function",
            ValueType::Class => "class",
            ValueType::Object => "object",
        })
    }
}

impl ValueType {
    pub fn fmt_a(&self) -> &str {
        match self {
            ValueType::String => "a string",
            ValueType::Number => "a number",
            ValueType::Boolean => "a boolean",
            ValueType::Nil => "nil",
            ValueType::Function => "a function",
            ValueType::Class => "a class",
            ValueType::Object => "an object",
        }
    }
}

#[derive(Debug)]
pub enum ValueDescriptor {
    Single(ValueType),
    AnyOf(Vec<ValueType>),
}
impl From<ValueType> for ValueDescriptor {
    fn from(value_type: ValueType) -> Self {
        ValueDescriptor::Single(value_type)
    }
}
impl ValueDescriptor {
    pub fn fmt_a(&self) -> String {
        match self {
            Self::Single(value_type) => value_type.fmt_a().to_string(),
            Self::AnyOf(types) => {
                Itertools::intersperse(types.iter().map(ValueType::fmt_a), " or ").collect()
            }
        }
    }
}
