mod completion;
mod environment;
mod error;
mod lox_callable;
mod lox_class;
mod lox_function;
mod lox_native_function;
mod lox_object;
mod runtime_value;

use crate::{
    ast::*,
    keywords::{SUPER, THIS},
    resolver::{Resolutions, Resolver, ResolverError},
    side_table::{SideTable, UniqueId},
    source::SourceSpan,
    SourceReference,
};
use completion::Completion;
use environment::{Environment, EnvironmentRef};
pub use error::RuntimeError;
use miette::Result;
pub use runtime_value::*;
use std::{cell::RefCell, fmt::Debug, io::Write, mem, rc::Rc};

#[derive(Debug, Clone)]
pub struct Ctx {
    source_code: SourceReference,
}

pub struct Interpreter<'a, Stdout: Write> {
    environment: EnvironmentRef,
    globals: EnvironmentRef,
    stdout: &'a mut Stdout,
    next_value_id: usize,
    resolutions: Resolutions,
    id: UniqueId,
}

pub struct PreparedProgram(UniqueId, Program);
impl PreparedProgram {
    pub fn program(&self) -> &Program {
        &self.1
    }
}

impl<'out, Stdout: Write> Interpreter<'out, Stdout> {
    pub fn new(stdout: &'out mut Stdout) -> Self {
        let globals = Rc::new(RefCell::new(Environment::new()));
        let mut interpreter = Self {
            environment: globals.clone(),
            globals,
            stdout,
            next_value_id: 0,
            id: UniqueId::new(),
            resolutions: SideTable::new(),
        };
        interpreter.define_native_fn("clock", 0, lox_native_fns::clock);
        interpreter.define_native_fn("type_of", 1, lox_native_fns::type_of);
        interpreter
    }
    pub fn prepare(&mut self, program: Program) -> Result<PreparedProgram, Vec<ResolverError>> {
        Resolver::resolve(&program, &mut self.resolutions)?;
        Ok(PreparedProgram(self.id, program))
    }
    pub fn interpret(&mut self, program: &PreparedProgram) -> Result<RuntimeValue, RuntimeError> {
        assert_eq!(program.0, self.id);
        let ctx = Ctx {
            source_code: program.1.source_reference.clone(),
        };
        let result =
            try_for_each_and_return_last(&program.1.statements, RuntimeValue::nil(), |stmt| {
                self.eval_decl_or_stmt(stmt, &ctx)
            });
        match result {
            Completion::Normal(value) => Ok(value),
            Completion::Return(value) => Ok(value),
            Completion::Error(err) => Err(err),
        }
    }
    fn eval_decl_or_stmt(&mut self, decl_or_stmt: &DeclOrStmt, ctx: &Ctx) -> Completion {
        match decl_or_stmt {
            DeclOrStmt::Decl(decl) => self.eval_decl(decl, ctx).into(),
            DeclOrStmt::Stmt(stmt) => self.eval_stmt(stmt, ctx),
        }
    }
    fn eval_decl(&mut self, decl: &Decl, ctx: &Ctx) -> Result<RuntimeValue, RuntimeError> {
        match decl {
            Decl::Var(decl) => self.eval_var_decl(decl, ctx),
            Decl::Fun(decl) => self
                .environment
                .as_ref()
                .borrow_mut()
                .define_local(
                    &decl.fun.name.name,
                    RuntimeValue::Function(lox_function::LoxFunction::new(
                        decl.fun.clone(),
                        self.environment.clone(),
                        ctx.clone(),
                        false,
                    )),
                )
                .map_err(|_| RuntimeError::AlreadyDefinedVariable {
                    name: decl.fun.name.name.clone(),
                    found_at: SourceSpan::range(
                        decl.source_span.start(),
                        decl.fun.name.source_span.end(),
                    ),
                    source_code: ctx.source_code.clone(),
                }),
            Decl::Class(decl) => {
                let super_class = if let Some(super_class_id) = &decl.super_class {
                    let super_class = self.eval_variable(super_class_id, ctx)?;
                    if let RuntimeValue::Class(super_class) = super_class {
                        Some(super_class)
                    } else {
                        return Err(RuntimeError::NonClassExtend {
                            class_name: decl.name.name.clone(),
                            super_class_name: super_class_id.name.clone(),
                            actual_type: super_class.type_of(),
                            found_at: super_class_id.source_span(),
                            source_code: ctx.source_code.clone(),
                        });
                    }
                } else {
                    None
                };
                self.environment
                    .as_ref()
                    .borrow_mut()
                    .define_local(
                        &decl.name.name,
                        RuntimeValue::Class(lox_class::LoxClass::new(
                            &decl.name.name,
                            super_class,
                            self.environment.clone(),
                            decl.methods.clone(),
                            ctx.clone(),
                        )),
                    )
                    .map_err(|_| RuntimeError::AlreadyDefinedVariable {
                        name: decl.name.name.clone(),
                        found_at: SourceSpan::range(
                            decl.source_span.start(),
                            decl.name.source_span.end(),
                        ),
                        source_code: ctx.source_code.clone(),
                    })
            }
        }
    }
    fn eval_var_decl(&mut self, decl: &VarDecl, ctx: &Ctx) -> Result<RuntimeValue, RuntimeError> {
        let initial_value = decl
            .initializer
            .as_ref()
            .map(|expr| self.eval_expr(expr, ctx))
            .transpose()?
            .unwrap_or_else(RuntimeValue::nil);

        self.environment
            .as_ref()
            .borrow_mut()
            .define_local(&decl.identifier.name, initial_value)
            .map_err(|_| RuntimeError::AlreadyDefinedVariable {
                name: decl.identifier.name.clone(),
                found_at: decl.identifier.source_span(),
                source_code: ctx.source_code.clone(),
            })
    }
    fn eval_stmt(&mut self, stmt: &Stmt, ctx: &Ctx) -> Completion {
        match stmt {
            Stmt::Expr(stmt) => self.eval_expr(&stmt.expression, ctx).into(),
            Stmt::Print(stmt) => {
                let value = Completion::from(self.eval_expr(&stmt.expression, ctx))?;
                writeln!(self.stdout, "{}", value).unwrap();
                Completion::Normal(value)
            }
            Stmt::Block(stmt) => self.run_with_env(
                Environment::new_with_parent(self.environment.clone()).wrap(),
                |this| {
                    try_for_each_and_return_last(&stmt.body, RuntimeValue::nil(), |stmt| {
                        this.eval_decl_or_stmt(stmt, ctx)
                    })
                },
            ),
            Stmt::If(stmt) => {
                if self.eval_expr(&stmt.condition, ctx)?.cast_boolean() {
                    self.eval_stmt(&stmt.then_branch, ctx)?;
                } else if let Some(else_branch) = &stmt.else_branch {
                    self.eval_stmt(else_branch, ctx)?;
                }
                Completion::Normal(RuntimeValue::nil())
            }
            Stmt::While(stmt) => {
                while self.eval_expr(&stmt.condition, ctx)?.cast_boolean() {
                    self.eval_stmt(&stmt.body, ctx)?;
                }
                Completion::Normal(RuntimeValue::nil())
            }
            Stmt::Return(stmt) => {
                let value = match &stmt.expression {
                    Some(expression) => self.eval_expr(expression, ctx)?,
                    None => RuntimeValue::nil(),
                };
                Completion::Return(value)
            }
        }
    }
    fn eval_expr(&mut self, expr: &Expr, ctx: &Ctx) -> Result<RuntimeValue, RuntimeError> {
        match expr {
            Expr::Binary(BinaryExpr {
                left,
                right,
                operator,
            }) => {
                let left_val = self.eval_expr(left, ctx)?;
                let mut right_val = || self.eval_expr(right, ctx);
                let make_left_err =
                    |expected: ValueDescriptor, actual: ValueType| RuntimeError::OperandTypeError {
                        expected_type: expected,
                        actual_type: actual,
                        operand_loc: left.source_span(),
                        operator: operator.to_string(),
                        operator_loc: operator.source_span(),
                        source_code: ctx.source_code.clone(),
                    };
                let make_right_err =
                    |expected: ValueDescriptor, actual: ValueType| RuntimeError::OperandTypeError {
                        expected_type: expected,
                        actual_type: actual,
                        operand_loc: right.source_span(),
                        operator: operator.to_string(),
                        operator_loc: operator.source_span(),
                        source_code: ctx.source_code.clone(),
                    };
                Ok(match operator.inner() {
                    BinaryOperator::Plus => match left_val {
                        RuntimeValue::String(left_str) => {
                            let right_val = right_val()?;
                            let right_str = right_val.cast_string(make_right_err)?;
                            let mut new_str =
                                String::with_capacity(left_str.len() + right_str.len());
                            new_str.push_str(left_str.as_str());
                            new_str.push_str(right_str);
                            new_str.into()
                        }
                        RuntimeValue::Number(left_num) => {
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
                let right_val = self.eval_expr(right, ctx)?;
                Ok(match operator.inner() {
                    UnaryOperator::Minus => (-right_val.cast_number(|expected, actual| {
                        RuntimeError::OperandTypeError {
                            expected_type: expected,
                            actual_type: actual,
                            operand_loc: right.source_span(),
                            operator: operator.inner().to_string(),
                            operator_loc: operator.source_span(),
                            source_code: ctx.source_code.clone(),
                        }
                    })?)
                    .into(),
                    UnaryOperator::Not => (!right_val.cast_boolean()).into(),
                })
            }
            Expr::Literal(LiteralExpr { value, .. }) => Ok(value.into()),
            Expr::Variable(VariableExpr { identifier }) => self.eval_variable(identifier, ctx),
            Expr::Grouping(GroupingExpr { expr }) => self.eval_expr(expr, ctx),
            Expr::Assignment(AssignmentExpr { target, value }) => {
                let value = self.eval_expr(value, ctx)?;
                match target {
                    AssignmentTargetExpr::Variable(target) => {
                        self.lookup_identifier_mut(&target.identifier, |environment| {
                            environment
                                .assign_local(&target.identifier.name, value.clone())
                                .map_err(|_| RuntimeError::UndefinedVariable {
                                    name: target.identifier.name.clone(),
                                    found_at: target.source_span(),
                                    source_code: ctx.source_code.clone(),
                                })
                        })
                    }
                    AssignmentTargetExpr::PropertyAccess(target) => {
                        match self.eval_expr(&target.object, ctx)? {
                            RuntimeValue::Object(object) => {
                                object.set(&target.property.name, value.clone());
                                Ok(value)
                            }
                            value => Err(RuntimeError::PropertyAccessOnNonObject {
                                actual_type: value.type_of(),
                                property_name: target.property.name.clone(),
                                found_at: target.property.source_span(),
                                source_code: ctx.source_code.clone(),
                            }),
                        }
                    }
                }
            }
            Expr::Call(call_expr) => match self.eval_expr(&call_expr.callee, ctx)? {
                RuntimeValue::NativeFunction(native_fn) => self.eval_call(
                    call_expr.source_span(),
                    &native_fn,
                    &call_expr.arguments,
                    ctx,
                ),
                RuntimeValue::Function(fun) => {
                    self.eval_call(call_expr.source_span(), &fun, &call_expr.arguments, ctx)
                }
                RuntimeValue::Class(class) => {
                    self.eval_call(call_expr.source_span(), &class, &call_expr.arguments, ctx)
                }
                other => Err(RuntimeError::UncallableValue {
                    actual_type: other.type_of(),
                    found_at: call_expr.source_span(),
                    source_code: ctx.source_code.clone(),
                }),
            },
            Expr::PropertyAccess(access_expr) => {
                match self.eval_expr(&access_expr.object, ctx)? {
                    RuntimeValue::Object(object) => object
                        .get(&access_expr.property.name)
                        .ok_or_else(|| RuntimeError::UnknownProperty {
                            name: access_expr.property.name.clone(),
                            found_at: access_expr.source_span(),
                            source_code: ctx.source_code.clone(),
                        }),
                    value => Err(RuntimeError::PropertyAccessOnNonObject {
                        actual_type: value.type_of(),
                        property_name: access_expr.property.name.clone(),
                        found_at: access_expr.source_span(),
                        source_code: ctx.source_code.clone(),
                    }),
                }
            }
            Expr::This(this_expr) => self.lookup_identifier(&this_expr.keyword, |environment| {
                Ok(environment
                    .get_local(THIS)
                    .expect("this should be defined according to resolver"))
            }),
            Expr::Super(super_expr) => {
                let super_class = self.lookup_identifier_mut(&super_expr.keyword, |environment| {
                    environment
                        .get_local(SUPER)
                        .expect("super must be defined according to resolver")
                });

                let super_class = match super_class {
                    RuntimeValue::Class(class) => class,
                    _ => panic!("Super is {} not a class", super_class.type_of()),
                };

                let object_distance = self.resolutions.get(&super_expr.keyword).unwrap() - 1;
                let object = self
                    .environment
                    .borrow()
                    .ancestor(object_distance, |environment| {
                        environment.get_local(THIS).unwrap()
                    })
                    .unwrap();

                let object = match object {
                    RuntimeValue::Object(object) => object,
                    _ => panic!("This is {} not an object", object.type_of()),
                };

                let method = super_class
                    .lookup_method(&super_expr.method.name)
                    .ok_or_else(|| RuntimeError::UnknownProperty {
                        name: super_expr.method.name.clone(),
                        found_at: super_expr.method.source_span(),
                        source_code: ctx.source_code.clone(),
                    })?
                    .bind(object);

                Ok(RuntimeValue::Function(method))
            }
        }
    }
    fn eval_variable(
        &mut self,
        identifier: &Identifier,
        ctx: &Ctx,
    ) -> Result<RuntimeValue, RuntimeError> {
        self.lookup_identifier(identifier, |environment| {
            environment
                .get_local(&identifier.name)
                .map_err(|_| RuntimeError::UndefinedVariable {
                    name: identifier.name.clone(),
                    found_at: identifier.source_span(),
                    source_code: ctx.source_code.clone(),
                })
        })
    }
    fn eval_call<Callable: lox_callable::LoxCallable>(
        &mut self,
        callable_source_span: SourceSpan,
        callable: &Callable,
        arguments: &[Expr],
        ctx: &Ctx,
    ) -> Result<RuntimeValue, RuntimeError> {
        let argument_vals = arguments
            .iter()
            .map(|arg| self.eval_expr(arg, ctx))
            .collect::<Result<Vec<_>, _>>()?;

        if argument_vals.len() != callable.arity() {
            Err(RuntimeError::UnexpectedCallArity {
                expected_arity: callable.arity(),
                actual_arity: argument_vals.len(),
                found_at: callable_source_span,
                source_code: ctx.source_code.clone(),
            })
        } else {
            callable.call(self, &argument_vals)
        }
    }
    fn lookup_identifier<T, F: Fn(&Environment) -> T>(&self, identifier: &Identifier, cb: F) -> T {
        match self.resolutions.get(identifier) {
            Some(distance) => self.environment.borrow().ancestor(*distance, cb).unwrap(),
            None => cb(&self.globals.borrow()),
        }
    }
    fn lookup_identifier_mut<T, F: Fn(&mut Environment) -> T>(
        &self,
        identifier: &Identifier,
        cb: F,
    ) -> T {
        match self.resolutions.get(identifier) {
            Some(distance) => self
                .environment
                .borrow_mut()
                .ancestor_mut(*distance, cb)
                .unwrap(),
            None => cb(&mut self.globals.borrow_mut()),
        }
    }
    fn run_with_env<T, F: Fn(&mut Self) -> T>(&mut self, new_env: EnvironmentRef, run: F) -> T {
        let old_env = mem::replace(&mut self.environment, new_env);
        let result = run(self);
        self.environment = old_env;
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
            .as_ref()
            .borrow_mut()
            .define_local(
                name,
                RuntimeValue::NativeFunction(lox_native_function::LoxNativeFunction::new(
                    id,
                    name.to_string(),
                    arity,
                    implementation,
                )),
            )
            .unwrap();
    }
}

fn try_for_each_and_return_last<In, F: FnMut(&In) -> Completion>(
    items: &[In],
    default: RuntimeValue,
    mut run: F,
) -> Completion {
    for item in &items[..items.len().max(1) - 1] {
        run(item)?;
    }
    match items.last() {
        Some(item) => run(item),
        None => Completion::Normal(default),
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
