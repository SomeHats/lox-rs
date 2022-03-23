use std::{fmt::Display, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    String(Rc<String>),
    Number(f64),
    Boolean(bool),
    Nil,
}

impl Value {
    pub fn type_of(&self) -> ValueType {
        match self {
            Value::String(_) => ValueType::String,
            Value::Number(_) => ValueType::Number,
            Value::Boolean(_) => ValueType::Boolean,
            Value::Nil => ValueType::Nil,
        }
    }
}

impl Display for Value {
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
            Self::Nil => f.write_str("nil"),
        }
    }
}

#[derive(Debug)]
pub enum ValueType {
    String,
    Number,
    Boolean,
    Nil,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ValueType::String => "string",
            ValueType::Number => "number",
            ValueType::Boolean => "boolean",
            ValueType::Nil => "nil",
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
        }
    }
}
