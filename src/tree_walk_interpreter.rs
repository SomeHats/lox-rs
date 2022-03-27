use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    io::Write,
    rc::Rc,
};

use crate::{
    ast::*,
    source::SourceSpan,
    value::{Value, ValueDescriptor, ValueType},
};
use miette::{Diagnostic, Result};
use replace_with::replace_with_or_default;
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum RuntimeError {
    #[error("Operand must be {}, but found {}", .expected_type.fmt_a(), .actual_type.fmt_a())]
    OperandTypeError {
        expected_type: ValueDescriptor,
        actual_type: ValueType,
        #[label("{} was found here", .actual_type.fmt_a())]
        operand_loc: SourceSpan,
        operator: String,
        #[label("the '{operator}' operator expected {}", .expected_type.fmt_a())]
        operator_loc: SourceSpan,
    },
    #[error("Undefined variable {name}")]
    UndefinedVariable {
        name: String,
        #[label("found here")]
        found_at: SourceSpan,
    },
    #[error("Already a variable named {name} in this scope")]
    AlreadyDefinedVariable {
        name: String,
        #[label("'{name}' here is already a variable")]
        found_at: SourceSpan,
    },
    #[error("Expected {expected_arity} arguments but got {actual_arity}")]
    UnexpectedCallArity {
        expected_arity: usize,
        actual_arity: usize,
        #[label("On this function call")]
        found_at: SourceSpan,
    },
    #[error("Can only call functions and classes")]
    UncallableValue {
        actual_type: ValueType,
        #[label("Attempted to call {} here", .actual_type.fmt_a())]
        found_at: SourceSpan,
    },
}

pub struct Interpreter<'a, Stdout: Write> {
    environment: Environment,
    stdout: &'a mut Stdout,
    next_value_id: usize,
}

impl<'a, Stdout: Write> Interpreter<'a, Stdout> {
    pub fn new(stdout: &'a mut Stdout) -> Self {
        let mut interpreter = Self {
            environment: Environment::new(),
            stdout,
            next_value_id: 0,
        };
        interpreter.define_native_fn("clock", 0, lox_native_fns::clock);
        interpreter.define_native_fn("type_of", 1, lox_native_fns::type_of);
        interpreter
    }
    pub fn interpret(&mut self, program: &Program) -> Result<RuntimeValue, RuntimeError> {
        try_for_each_and_return_last(&program.statements, RuntimeValue::nil(), |stmt| {
            self.eval_decl_or_stmt(stmt)
        })
    }
    fn eval_decl_or_stmt(
        &mut self,
        decl_or_stmt: &DeclOrStmt,
    ) -> Result<RuntimeValue, RuntimeError> {
        match decl_or_stmt {
            DeclOrStmt::Decl(decl) => self.eval_decl(decl),
            DeclOrStmt::Stmt(stmt) => self.eval_stmt(stmt),
        }
    }
    fn eval_decl(&mut self, decl: &Decl) -> Result<RuntimeValue, RuntimeError> {
        match decl {
            Decl::Var(decl) => self.eval_var_decl(decl),
        }
    }
    fn eval_var_decl(&mut self, decl: &VarDecl) -> Result<RuntimeValue, RuntimeError> {
        let initial_value = decl
            .initializer
            .as_ref()
            .map(|expr| self.eval_expr(expr))
            .transpose()?
            .unwrap_or(RuntimeValue::nil());

        Ok(self
            .environment
            .define(&decl.identifier.name, initial_value)
            .map_err(|_| RuntimeError::AlreadyDefinedVariable {
                name: decl.identifier.name.clone(),
                found_at: decl.identifier.source_span(),
            })?
            .clone())
    }
    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<RuntimeValue, RuntimeError> {
        match stmt {
            Stmt::Expr(stmt) => self.eval_expr(&stmt.expression),
            Stmt::Print(stmt) => {
                let value = self.eval_expr(&stmt.expression)?;
                writeln!(self.stdout, "{}", value).unwrap();
                Ok(value)
            }
            Stmt::Block(stmt) => self.run_with_new_child_environment(|this| {
                try_for_each_and_return_last(&stmt.body, RuntimeValue::nil(), |stmt| {
                    this.eval_decl_or_stmt(stmt)
                })
            }),
            Stmt::If(stmt) => {
                if self.eval_expr(&stmt.condition)?.cast_boolean() {
                    self.eval_stmt(&stmt.then_branch)?;
                } else if let Some(else_branch) = &stmt.else_branch {
                    self.eval_stmt(else_branch)?;
                }
                Ok(RuntimeValue::nil())
            }
            Stmt::While(stmt) => {
                while self.eval_expr(&stmt.condition)?.cast_boolean() {
                    self.eval_stmt(&stmt.body)?;
                }
                Ok(RuntimeValue::nil())
            }
        }
    }
    pub fn eval_expr(&mut self, expr: &Expr) -> Result<RuntimeValue, RuntimeError> {
        match expr {
            Expr::Binary(BinaryExpr {
                left,
                right,
                operator,
            }) => {
                let left_val = self.eval_expr(left)?;
                let mut right_val = || self.eval_expr(right);
                let make_left_err =
                    |expected: ValueDescriptor, actual: ValueType| RuntimeError::OperandTypeError {
                        expected_type: expected,
                        actual_type: actual,
                        operand_loc: left.source_span(),
                        operator: operator.to_string(),
                        operator_loc: operator.source_span(),
                    };
                let make_right_err =
                    |expected: ValueDescriptor, actual: ValueType| RuntimeError::OperandTypeError {
                        expected_type: expected,
                        actual_type: actual,
                        operand_loc: right.source_span(),
                        operator: operator.to_string(),
                        operator_loc: operator.source_span(),
                    };
                Ok(match operator.inner() {
                    BinaryOperator::Plus => match left_val {
                        RuntimeValue::Basic(Value::String(left_str)) => {
                            let right_val = right_val()?;
                            let right_str = right_val.cast_string(make_right_err)?;
                            let mut new_str =
                                String::with_capacity(left_str.len() + right_str.len());
                            new_str.push_str(left_str.as_str());
                            new_str.push_str(right_str);
                            new_str.into()
                        }
                        RuntimeValue::Basic(Value::Number(left_num)) => {
                            (left_num + right_val()?.cast_number(make_right_err)?).into()
                        }
                        value => {
                            return Err(make_left_err(
                                ValueDescriptor::AnyOf(vec![ValueType::String, ValueType::Number]),
                                value.type_of(),
                            ))
                        }
                    },
                    BinaryOperator::Minus => (left_val.cast_number(make_left_err)?
                        - right_val()?.cast_number(make_right_err)?)
                    .into(),
                    BinaryOperator::Multiply => (left_val.cast_number(make_left_err)?
                        * right_val()?.cast_number(make_right_err)?)
                    .into(),
                    BinaryOperator::Divide => (left_val.cast_number(make_left_err)?
                        / right_val()?.cast_number(make_right_err)?)
                    .into(),
                    BinaryOperator::NotEqualTo => (left_val != right_val()?).into(),
                    BinaryOperator::EqualTo => (left_val == right_val()?).into(),
                    BinaryOperator::LessThan => (left_val.cast_number(make_left_err)?
                        < right_val()?.cast_number(make_right_err)?)
                    .into(),
                    BinaryOperator::LessThanOrEqualTo => (left_val.cast_number(make_left_err)?
                        <= right_val()?.cast_number(make_right_err)?)
                    .into(),
                    BinaryOperator::GreaterThan => (left_val.cast_number(make_left_err)?
                        > right_val()?.cast_number(make_right_err)?)
                    .into(),
                    BinaryOperator::GreaterThanOrEqualTo => (left_val
                        .cast_number(make_left_err)?
                        >= right_val()?.cast_number(make_right_err)?)
                    .into(),
                    BinaryOperator::LogicalAnd => {
                        if !left_val.cast_boolean() {
                            left_val
                        } else {
                            right_val()?
                        }
                    }
                    BinaryOperator::LogicalOr => {
                        if left_val.cast_boolean() {
                            left_val
                        } else {
                            right_val()?
                        }
                    }
                })
            }
            Expr::Unary(UnaryExpr { operator, right }) => {
                let right_val = self.eval_expr(right)?;
                Ok(match operator.inner() {
                    UnaryOperator::Minus => (-right_val.cast_number(|expected, actual| {
                        RuntimeError::OperandTypeError {
                            expected_type: expected,
                            actual_type: actual,
                            operand_loc: right.source_span(),
                            operator: operator.inner().to_string(),
                            operator_loc: operator.source_span(),
                        }
                    })?)
                    .into(),
                    UnaryOperator::Not => (!right_val.cast_boolean()).into(),
                })
            }
            Expr::Literal(LiteralExpr { value, .. }) => Ok(value.clone().into()),
            Expr::Variable(VariableExpr { identifier }) => self
                .environment
                .get(&identifier.name)
                .map(Clone::clone)
                .ok_or_else(|| RuntimeError::UndefinedVariable {
                    name: identifier.name.clone(),
                    found_at: identifier.source_span(),
                }),
            Expr::Grouping(GroupingExpr { expr }) => self.eval_expr(expr),
            Expr::Assignment(AssignmentExpr { target, value }) => {
                let value = self.eval_expr(value)?;
                self.environment
                    .assign(&target.name, value)
                    .map(Clone::clone)
                    .ok_or_else(|| RuntimeError::UndefinedVariable {
                        name: target.name.clone(),
                        found_at: target.source_span(),
                    })
            }
            Expr::Call(call_expr) => {
                let callee_val = self.eval_expr(&call_expr.callee)?;
                match callee_val {
                    RuntimeValue::NativeFunction(native_fn) => {
                        let argument_vals = call_expr
                            .arguments
                            .iter()
                            .map(|arg| self.eval_expr(arg))
                            .collect::<Result<Vec<_>, _>>()?;

                        if argument_vals.len() != native_fn.arity {
                            Err(RuntimeError::UnexpectedCallArity {
                                expected_arity: native_fn.arity,
                                actual_arity: argument_vals.len(),
                                found_at: call_expr.source_span(),
                            })
                        } else {
                            (native_fn.implementation)(&argument_vals)
                        }
                    }
                    other => Err(RuntimeError::UncallableValue {
                        actual_type: other.type_of(),
                        found_at: call_expr.source_span(),
                    }),
                }
            }
        }
    }
    fn run_with_new_child_environment<T, F: Fn(&mut Self) -> T>(&mut self, run: F) -> T {
        replace_with_or_default(&mut self.environment, |old_env| {
            Environment::new_with_parent(old_env)
        });
        let result = run(self);
        replace_with_or_default(&mut self.environment, |old_env| {
            *old_env
                .parent
                .expect("popped environment more times than pushed")
        });
        result
    }
    fn get_next_value_id(&mut self) -> usize {
        let next_id = self.next_value_id;
        self.next_value_id += 1;
        next_id
    }
    fn define_native_fn(
        &mut self,
        name: &str,
        arity: usize,
        implementation: fn(&[RuntimeValue]) -> Result<RuntimeValue, RuntimeError>,
    ) {
        let id = self.get_next_value_id();
        self.environment
            .define(
                &name,
                RuntimeValue::NativeFunction(Rc::new(LoxNativeFunction {
                    id,
                    name: name.to_string(),
                    arity,
                    implementation,
                })),
            )
            .unwrap();
    }
}

pub struct LoxNativeFunction {
    id: usize,
    name: String,
    arity: usize,
    implementation: fn(&[RuntimeValue]) -> Result<RuntimeValue, RuntimeError>,
}
impl Display for LoxNativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {}() {{ <native code> }}", self.name)
    }
}
impl Debug for LoxNativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {}() {{ <native code> }}", self.name)
    }
}
impl PartialEq for LoxNativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

#[derive(Clone, PartialEq)]
pub enum RuntimeValue {
    Basic(Value),
    NativeFunction(Rc<LoxNativeFunction>),
}
impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeValue::Basic(value) => Display::fmt(value, f),
            RuntimeValue::NativeFunction(value) => Display::fmt(value, f),
        }
    }
}
impl Debug for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Basic(value) => Debug::fmt(value, f),
            Self::NativeFunction(value) => Debug::fmt(value, f),
        }
    }
}
impl From<Value> for RuntimeValue {
    fn from(value: Value) -> Self {
        Self::Basic(value)
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
impl RuntimeValue {
    fn nil() -> Self {
        RuntimeValue::Basic(Value::Nil)
    }
    fn number(value: f64) -> Self {
        RuntimeValue::Basic(Value::Number(value))
    }
    fn boolean(value: bool) -> Self {
        RuntimeValue::Basic(Value::Boolean(value))
    }
    fn string(value: String) -> Self {
        RuntimeValue::Basic(Value::String(Rc::new(value)))
    }
    fn type_of(&self) -> ValueType {
        match self {
            RuntimeValue::Basic(value) => value.type_of(),
            RuntimeValue::NativeFunction(_) => ValueType::Function,
        }
    }
    fn cast_number<F: Fn(ValueDescriptor, ValueType) -> RuntimeError>(
        &self,
        make_error: F,
    ) -> Result<f64, RuntimeError> {
        match self {
            Self::Basic(Value::Number(value)) => Ok(*value),
            other => Err(make_error(ValueType::Number.into(), other.type_of())),
        }
    }
    fn cast_boolean(&self) -> bool {
        match self {
            Self::Basic(Value::Boolean(val)) => *val,
            Self::Basic(Value::Nil) => false,
            _ => true,
        }
    }
    fn cast_string<F: Fn(ValueDescriptor, ValueType) -> RuntimeError>(
        &self,
        make_error: F,
    ) -> Result<&str, RuntimeError> {
        match self {
            Self::Basic(Value::String(string)) => Ok(string.as_str()),
            other => Err(make_error(ValueType::String.into(), other.type_of())),
        }
    }
}

#[derive(Debug, Default)]

struct Environment {
    values: HashMap<String, RuntimeValue>,
    parent: Option<Box<Environment>>,
}
impl Environment {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
            parent: None,
        }
    }
    fn new_with_parent(parent: Self) -> Self {
        Environment {
            parent: Some(Box::new(parent)),
            ..Default::default()
        }
    }
    fn is_global(&self) -> bool {
        self.parent.is_none()
    }
    fn is_local(&self) -> bool {
        !self.is_global()
    }
    fn define(&mut self, name: &str, value: RuntimeValue) -> Result<&RuntimeValue, ()> {
        if self.values.contains_key(name) && self.is_local() {
            Err(())
        } else {
            self.values.insert(name.to_string(), value);
            Ok(self.get(name).unwrap())
        }
    }
    fn get(&self, name: &str) -> Option<&RuntimeValue> {
        self.values
            .get(name)
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get(name)))
    }
    fn assign(&mut self, name: &str, value: RuntimeValue) -> Option<&RuntimeValue> {
        if let Some(target) = self.values.get_mut(name) {
            *target = value;
            Some(self.get(name).unwrap())
        } else if let Some(parent) = &mut self.parent {
            parent.assign(name, value)
        } else {
            None
        }
    }
}

fn try_for_each_and_return_last<In, Out, Err, F: FnMut(&In) -> Result<Out, Err>>(
    items: &[In],
    default: Out,
    mut run: F,
) -> Result<Out, Err> {
    for item in &items[..items.len().max(1) - 1] {
        run(item)?;
    }
    match items.last() {
        Some(item) => run(item),
        None => Ok(default),
    }
}

mod lox_native_fns {
    use super::{RuntimeError, RuntimeValue};
    use std::time::SystemTime;

    type In = [RuntimeValue];
    type Out = Result<RuntimeValue, RuntimeError>;

    pub fn clock(_: &In) -> Out {
        Ok(RuntimeValue::number(
            SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_secs_f64(),
        ))
    }
    pub fn type_of(args: &In) -> Out {
        Ok(args.get(0).unwrap().type_of().to_string().into())
    }
}
