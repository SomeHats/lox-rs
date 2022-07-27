use super::{
    lox_class::LoxClass, lox_function::LoxFunction, lox_native_function::LoxNativeFunction,
    lox_object::LoxObject, RuntimeError,
};
use crate::ast;
use itertools::Itertools;
use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

#[derive(PartialEq, Clone)]
pub enum RuntimeValue {
    String(Rc<String>),
    Number(f64),
    Boolean(bool),
    NativeFunction(LoxNativeFunction),
    Function(LoxFunction),
    Class(LoxClass),
    Object(LoxObject),
    Nil,
}

impl Debug for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(string) => {
                f.write_str("\"")?;
                f.write_str(string.replace('\n', "\\n").replace('\"', "\\\"").as_str())?;
                f.write_str("\"")
            }
            Self::Number(n) => write!(f, "{}", n),
            Self::Boolean(b) => match b {
                true => f.write_str("true"),
                false => f.write_str("false"),
            },
            Self::NativeFunction(value) => Debug::fmt(value, f),
            Self::Function(value) => Debug::fmt(value, f),
            Self::Class(value) => Debug::fmt(value, f),
            Self::Object(value) => Debug::fmt(value, f),
            Self::Nil => f.write_str("nil"),
        }
    }
}
impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(string) => f.write_str(string),
            Self::Number(n) => write!(f, "{}", n),
            Self::Boolean(b) => match b {
                true => f.write_str("true"),
                false => f.write_str("false"),
            },
            Self::NativeFunction(value) => Display::fmt(value, f),
            Self::Function(value) => Display::fmt(value, f),
            Self::Class(value) => Display::fmt(value, f),
            Self::Object(value) => Display::fmt(value, f),
            Self::Nil => f.write_str("nil"),
        }
    }
}
impl From<f64> for RuntimeValue {
    fn from(value: f64) -> Self {
        Self::number(value)
    }
}
impl From<bool> for RuntimeValue {
    fn from(value: bool) -> Self {
        Self::boolean(value)
    }
}
impl From<String> for RuntimeValue {
    fn from(value: String) -> Self {
        Self::string(value)
    }
}
impl From<&ast::LiteralValue> for RuntimeValue {
    fn from(value: &ast::LiteralValue) -> Self {
        use ast::LiteralValue::*;
        match value {
            String(value) => value.clone().into(),
            Number(value) => (*value).into(),
            Boolean(value) => (*value).into(),
            Nil => Self::nil(),
        }
    }
}
impl From<LoxClass> for RuntimeValue {
    fn from(value: LoxClass) -> Self {
        Self::Class(value)
    }
}
impl From<LoxFunction> for RuntimeValue {
    fn from(value: LoxFunction) -> Self {
        Self::Function(value)
    }
}
impl From<LoxNativeFunction> for RuntimeValue {
    fn from(value: LoxNativeFunction) -> Self {
        Self::NativeFunction(value)
    }
}
impl From<LoxObject> for RuntimeValue {
    fn from(value: LoxObject) -> Self {
        Self::Object(value)
    }
}

impl RuntimeValue {
    pub fn nil() -> Self {
        RuntimeValue::Nil
    }
    pub fn number(value: f64) -> Self {
        RuntimeValue::Number(value)
    }
    pub fn boolean(value: bool) -> Self {
        RuntimeValue::Boolean(value)
    }
    pub fn string(value: String) -> Self {
        RuntimeValue::String(Rc::new(value))
    }
    pub fn type_of(&self) -> ValueType {
        match self {
            RuntimeValue::String(_) => ValueType::String,
            RuntimeValue::Number(_) => ValueType::Number,
            RuntimeValue::Boolean(_) => ValueType::Boolean,
            RuntimeValue::NativeFunction(_) => ValueType::Function,
            RuntimeValue::Function(_) => ValueType::Function,
            RuntimeValue::Class(_) => ValueType::Class,
            RuntimeValue::Object(_) => ValueType::Object,
            RuntimeValue::Nil => ValueType::Nil,
        }
    }
    pub fn cast_number<F: Fn(ValueDescriptor, ValueType) -> RuntimeError>(
        &self,
        make_error: F,
    ) -> Result<f64, RuntimeError> {
        match self {
            RuntimeValue::Number(value) => Ok(*value),
            other => Err(make_error(ValueType::Number.into(), other.type_of())),
        }
    }
    pub fn cast_boolean(&self) -> bool {
        match self {
            RuntimeValue::Boolean(val) => *val,
            RuntimeValue::Nil => false,
            _ => true,
        }
    }
    pub fn cast_string<F: Fn(ValueDescriptor, ValueType) -> RuntimeError>(
        &self,
        make_error: F,
    ) -> Result<&str, RuntimeError> {
        match self {
            RuntimeValue::String(string) => Ok(string.as_str()),
            other => Err(make_error(ValueType::String.into(), other.type_of())),
        }
    }
    pub fn into_string(self) -> Option<Rc<String>> {
        match self {
            RuntimeValue::String(string) => Some(string),
            _ => None,
        }
    }
    pub fn into_number(self) -> Option<f64> {
        match self {
            RuntimeValue::Number(value) => Some(value),
            _ => None,
        }
    }
    pub fn into_boolean(self) -> Option<bool> {
        match self {
            RuntimeValue::Boolean(value) => Some(value),
            _ => None,
        }
    }
    pub fn into_native_function(self) -> Option<LoxNativeFunction> {
        match self {
            RuntimeValue::NativeFunction(value) => Some(value),
            _ => None,
        }
    }
    pub fn into_function(self) -> Option<LoxFunction> {
        match self {
            RuntimeValue::Function(value) => Some(value),
            _ => None,
        }
    }
    pub fn into_class(self) -> Option<LoxClass> {
        match self {
            RuntimeValue::Class(value) => Some(value),
            _ => None,
        }
    }
    pub fn into_object(self) -> Option<LoxObject> {
        match self {
            RuntimeValue::Object(value) => Some(value),
            _ => None,
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::size_of;

    #[test]
    fn runtime_value_size() {
        assert_eq!(size_of::<RuntimeValue>(), 16);
    }
}
