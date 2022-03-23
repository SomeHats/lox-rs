use std::{collections::HashMap, io::Write, rc::Rc};

use crate::{
    ast::*,
    source::SourceSpan,
    value::{Value, ValueType},
};
use miette::{Diagnostic, Result};
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum RuntimeError {
    #[error("Operand must be {}, but found {}", .expected_type.fmt_a(), .actual_type.fmt_a())]
    OperandTypeError {
        expected_type: ValueType,
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
}

pub trait InterpreterContext<Stdout: Write> {
    fn stdout(&mut self) -> &mut Stdout;
}

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Environment::new(),
        }
    }
}
impl Interpreter {
    pub fn interpret<Stdout: Write, Ctx: InterpreterContext<Stdout>>(
        &mut self,
        program: &Program,
        ctx: &mut Ctx,
    ) -> Result<Value, RuntimeError> {
        let preceding_statements = &program.statements[..program.statements.len() - 1];
        let last_statement = program.statements.last();

        for stmt in preceding_statements {
            self.eval_decl_or_stmt(stmt, ctx)?;
        }

        if let Some(stmt) = last_statement {
            self.eval_decl_or_stmt(stmt, ctx)
        } else {
            Ok(Value::Nil.into())
        }
    }
    fn eval_decl_or_stmt<Stdout: Write, Ctx: InterpreterContext<Stdout>>(
        &mut self,
        decl_or_stmt: &DeclOrStmt,
        ctx: &mut Ctx,
    ) -> Result<Value, RuntimeError> {
        match decl_or_stmt {
            DeclOrStmt::Decl(decl) => self.eval_decl(decl),
            DeclOrStmt::Stmt(stmt) => self.eval_stmt(stmt, ctx),
        }
    }
    fn eval_decl(&mut self, decl: &Decl) -> Result<Value, RuntimeError> {
        match decl {
            Decl::Var(decl) => self.eval_var_decl(decl),
        }
    }
    fn eval_var_decl(&mut self, decl: &VarDecl) -> Result<Value, RuntimeError> {
        let initial_value = decl
            .initializer
            .as_ref()
            .map(|expr| self.eval_expr(expr))
            .transpose()?
            .unwrap_or(Value::Nil);

        Ok(self
            .environment
            .define(&decl.identifier, initial_value)
            .clone())
    }
    fn eval_stmt<Stdout: Write, Ctx: InterpreterContext<Stdout>>(
        &mut self,
        stmt: &Stmt,
        ctx: &mut Ctx,
    ) -> Result<Value, RuntimeError> {
        match stmt {
            Stmt::Expr(stmt) => Ok(self.eval_expr(&stmt.expression)?),
            Stmt::Print(stmt) => {
                let value = self.eval_expr(&stmt.expression)?;
                writeln!(ctx.stdout(), "{}", value).unwrap();
                Ok(value)
            }
        }
    }
    pub fn eval_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Binary(BinaryExpr {
                left,
                right,
                operator,
            }) => {
                let left_val = self.eval_expr(left)?;
                let right_val = self.eval_expr(right)?;
                let make_left_err = |expected, actual| RuntimeError::OperandTypeError {
                    expected_type: expected,
                    actual_type: actual,
                    operand_loc: left.source_span(),
                    operator: operator.to_string(),
                    operator_loc: operator.source_span(),
                };
                let make_right_err = |expected, actual| RuntimeError::OperandTypeError {
                    expected_type: expected,
                    actual_type: actual,
                    operand_loc: right.source_span(),
                    operator: operator.to_string(),
                    operator_loc: operator.source_span(),
                };
                Ok(match operator.inner() {
                    BinaryOperator::Plus => match left_val {
                        Value::String(left_str) => {
                            let right_str = right_val.cast_string(make_right_err)?;
                            let mut new_str =
                                String::with_capacity(left_str.len() + right_str.len());
                            new_str.push_str(left_str.as_str());
                            new_str.push_str(right_str);
                            Value::String(Rc::new(new_str)).into()
                        }
                        Value::Number(left_num) => {
                            Value::Number(left_num + right_val.cast_number(make_right_err)?).into()
                        }
                        _ => todo!(),
                    },
                    BinaryOperator::Minus => Value::Number(
                        left_val.cast_number(make_left_err)?
                            - right_val.cast_number(make_right_err)?,
                    )
                    .into(),
                    BinaryOperator::Multiply => Value::Number(
                        left_val.cast_number(make_left_err)?
                            * right_val.cast_number(make_right_err)?,
                    )
                    .into(),
                    BinaryOperator::Divide => Value::Number(
                        left_val.cast_number(make_left_err)?
                            / right_val.cast_number(make_right_err)?,
                    )
                    .into(),
                    BinaryOperator::NotEqualTo => Value::Boolean(left_val != right_val).into(),
                    BinaryOperator::EqualTo => Value::Boolean(left_val == right_val).into(),
                    BinaryOperator::LessThan => Value::Boolean(
                        left_val.cast_number(make_left_err)?
                            < right_val.cast_number(make_right_err)?,
                    )
                    .into(),
                    BinaryOperator::LessThanOrEqualTo => Value::Boolean(
                        left_val.cast_number(make_left_err)?
                            <= right_val.cast_number(make_right_err)?,
                    )
                    .into(),
                    BinaryOperator::GreaterThan => Value::Boolean(
                        left_val.cast_number(make_left_err)?
                            > right_val.cast_number(make_right_err)?,
                    )
                    .into(),
                    BinaryOperator::GreaterThanOrEqualTo => Value::Boolean(
                        left_val.cast_number(make_left_err)?
                            >= right_val.cast_number(make_right_err)?,
                    )
                    .into(),
                })
            }
            Expr::Unary(UnaryExpr { operator, right }) => {
                let right_val = self.eval_expr(right)?;
                Ok(match operator.inner() {
                    UnaryOperator::Minus => {
                        Value::Number(-right_val.cast_number(|expected, actual| {
                            RuntimeError::OperandTypeError {
                                expected_type: expected,
                                actual_type: actual,
                                operand_loc: right.source_span(),
                                operator: operator.inner().to_string(),
                                operator_loc: operator.source_span(),
                            }
                        })?)
                        .into()
                    }
                    UnaryOperator::Not => Value::Boolean(!right_val.cast_boolean()).into(),
                })
            }
            Expr::Literal(LiteralExpr { value, .. }) => Ok(value.clone().into()),
            Expr::Variable(VariableExpr { identifier }) => self
                .environment
                .get(identifier)
                .map(Clone::clone)
                .ok_or_else(|| RuntimeError::UndefinedVariable {
                    name: identifier.name.clone(),
                    found_at: identifier.source_span(),
                }),
            Expr::Assignment(AssignmentExpr { target, value }) => {
                let value = self.eval_expr(value)?.to_owned();
                self.environment
                    .assign(target, value.to_owned())
                    .map(Clone::clone)
                    .ok_or_else(|| RuntimeError::UndefinedVariable {
                        name: target.name.clone(),
                        found_at: target.source_span(),
                    })
            }
        }
    }
}

impl Value {
    fn cast_number<F: Fn(ValueType, ValueType) -> RuntimeError>(
        &self,
        make_error: F,
    ) -> Result<f64, RuntimeError> {
        match self {
            Value::Number(value) => Ok(*value),
            other => Err(make_error(ValueType::Number, other.type_of())),
        }
    }
    fn cast_boolean(&self) -> bool {
        match self {
            Value::Boolean(val) => *val,
            Value::Nil => false,
            _ => true,
        }
    }
    fn cast_string<F: Fn(ValueType, ValueType) -> RuntimeError>(
        &self,
        make_error: F,
    ) -> Result<&str, RuntimeError> {
        match self {
            Value::String(string) => Ok(string.as_str()),
            other => Err(make_error(ValueType::String, other.type_of())),
        }
    }
}

struct Environment {
    values: HashMap<String, Value>,
}
impl Environment {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }
    fn define(&mut self, identifier: &Identifier, value: Value) -> &Value {
        self.values.insert(identifier.name.clone(), value);
        self.get(identifier).unwrap()
    }
    fn get(&self, identifier: &Identifier) -> Option<&Value> {
        self.values.get(&identifier.name)
    }
    fn assign(&mut self, identifier: &Identifier, value: Value) -> Option<&Value> {
        if let Some(target) = self.values.get_mut(&identifier.name) {
            *target = value;
            Some(self.get(identifier).unwrap())
        } else {
            None
        }
    }
}
