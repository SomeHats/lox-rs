use super::object_table::{ObjectReference, ObjectTable};
use itertools::Itertools;
use std::fmt::{Debug, Display};

#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    String(ObjectReference<String>),
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
impl From<ObjectReference<String>> for Value {
    fn from(value: ObjectReference<String>) -> Self {
        Self::String(value)
    }
}
impl Value {
    pub fn as_number(&self) -> Option<f64> {
        match self {
            Self::Number(value) => Some(*value),
            _ => None,
        }
    }
    pub fn as_string(&self) -> Option<&ObjectReference<String>> {
        match self {
            Self::String(reference) => Some(reference),
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
    pub fn to_string(&self, tables: &ValueTables) -> String {
        match self {
            Self::Nil => "nil".to_string(),
            Self::Number(value) => value.to_string(),
            Self::Boolean(value) => value.to_string(),
            Self::String(reference) => tables.strings.get(reference).to_string(),
        }
    }
    pub fn to_debug_string(&self, tables: &ValueTables) -> String {
        match self {
            Self::Nil => "nil".to_string(),
            Self::Number(value) => value.to_string(),
            Self::Boolean(value) => value.to_string(),
            Self::String(reference) => {
                format!("\"{}\"", tables.strings.get(reference))
            }
        }
    }
    pub fn eq(&self, other: &Self, tables: &ValueTables) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Number(value), Self::Number(other)) => value == other,
            (Self::Boolean(value), Self::Boolean(other)) => value == other,
            (Self::String(reference), Self::String(other)) => {
                tables.strings.get(reference) == tables.strings.get(other)
            }
            _ => false,
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

#[derive(Debug, Default)]
pub struct ValueTables {
    pub strings: ObjectTable<String>,
}
