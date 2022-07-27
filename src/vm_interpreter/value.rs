use itertools::Itertools;
use std::fmt::{Debug, Display};

#[derive(Clone)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
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
impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Number(value) => write!(f, "{}", value),
            Self::Boolean(value) => write!(f, "{}", value),
        }
    }
}
impl Value {
    pub fn into_number(self) -> Option<f64> {
        match self {
            Self::Number(value) => Some(value),
            _ => None,
        }
    }
    pub fn into_boolean(self) -> Option<bool> {
        match self {
            Self::Boolean(value) => Some(value),
            _ => None,
        }
    }
    pub fn type_of(&self) -> ValueType {
        match self {
            Self::Nil => ValueType::Nil,
            Self::Number(_) => ValueType::Number,
            Self::Boolean(_) => ValueType::Boolean,
        }
    }
    pub fn cast_float(self) -> f64 {
        self.into_number().unwrap()
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
