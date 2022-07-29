use super::{
    chunk::ConstantValue,
    gc::{Gc, Trace},
};
use itertools::Itertools;
use std::fmt::{Debug, Display};

#[derive(Clone)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    String(Gc<String>),
}
impl Trace for Value {
    fn trace(&self) {
        match self {
            Value::Nil | Value::Number(_) | Value::Boolean(_) => (),
            Value::String(s) => s.trace(),
        }
    }

    fn root(&self) {
        match self {
            Value::Nil | Value::Number(_) | Value::Boolean(_) => (),
            Value::String(s) => s.root(),
        }
    }

    fn unroot(&self) {
        match self {
            Value::Nil | Value::Number(_) | Value::Boolean(_) => (),
            Value::String(s) => s.unroot(),
        }
    }
}
impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Number(value)
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
impl From<Gc<String>> for Value {
    fn from(value: Gc<String>) -> Self {
        Self::String(value)
    }
}
impl From<ConstantValue> for Value {
    fn from(value: ConstantValue) -> Self {
        match value {
            ConstantValue::Nil => Self::Nil,
            ConstantValue::Number(value) => value.into(),
            ConstantValue::Boolean(value) => value.into(),
            ConstantValue::String(value) => value.into(),
        }
    }
}
impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Number(value) => write!(f, "{}", value),
            Value::Boolean(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "\"{}\"", value.as_ref()),
        }
    }
}
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Number(value) => write!(f, "{}", value),
            Value::Boolean(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{}", value.as_ref()),
        }
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a.as_ref() == b.as_ref(),
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
    pub fn as_string(&self) -> Option<&Gc<String>> {
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
        }
    }
    pub fn cast_boolean(self) -> bool {
        match self {
            Self::Nil => false,
            Self::Boolean(value) => value,
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
