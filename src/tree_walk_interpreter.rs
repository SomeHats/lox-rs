use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    io::Write,
    mem,
    ops::{ControlFlow, FromResidual, Try},
    rc::Rc,
};

use crate::{
    ast::*,
    resolver::{Resolutions, Resolver, ResolverError},
    side_table::{SideTable, UniqueId},
    source::SourceSpan,
    value::{Value, ValueDescriptor, ValueType},
    SourceReference,
};
use itertools::Itertools;
use miette::{Diagnostic, Result};
use thiserror::Error;

type EnvironmentRef = Rc<RefCell<Environment>>;

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
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Undefined variable {name}")]
    UndefinedVariable {
        name: String,
        #[label("found here")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Already a variable named {name} in this scope")]
    AlreadyDefinedVariable {
        name: String,
        #[label("'{name}' here is already a variable")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Expected {expected_arity} arguments but got {actual_arity}")]
    UnexpectedCallArity {
        expected_arity: usize,
        actual_arity: usize,
        #[label("On this function call")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Can only call functions and classes")]
    UncallableValue {
        actual_type: ValueType,
        #[label("Attempted to call {} here", .actual_type.fmt_a())]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Only objects have properties")]
    PropertyAccessOnNonObject {
        actual_type: ValueType,
        property_name: String,
        #[label("Attempted to access {property_name} on {} here", .actual_type.fmt_a())]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Unknown property {name}")]
    UnknownProperty {
        name: String,
        #[label("This property is unknown")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
}

#[derive(Debug)]
pub enum Completion {
    Normal(RuntimeValue),
    Return(RuntimeValue),
    Error(RuntimeError),
}

#[derive(Debug)]
pub enum AbruptCompletion {
    Return(RuntimeValue),
    Error(RuntimeError),
}
impl FromResidual for Completion {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        match residual {
            AbruptCompletion::Return(value) => Completion::Return(value),
            AbruptCompletion::Error(err) => Completion::Error(err),
        }
    }
}
impl Try for Completion {
    type Output = RuntimeValue;

    type Residual = AbruptCompletion;

    fn from_output(output: Self::Output) -> Self {
        Completion::Normal(output)
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            Completion::Normal(value) => ControlFlow::Continue(value),
            Completion::Return(value) => ControlFlow::Break(AbruptCompletion::Return(value)),
            Completion::Error(err) => ControlFlow::Break(AbruptCompletion::Error(err)),
        }
    }
}
impl From<Result<RuntimeValue, RuntimeError>> for Completion {
    fn from(result: Result<RuntimeValue, RuntimeError>) -> Self {
        match result {
            Ok(value) => Completion::Normal(value),
            Err(err) => Completion::Error(err),
        }
    }
}

#[derive(Debug, Clone)]
struct Ctx {
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
                .define(
                    &decl.fun.name.name,
                    RuntimeValue::Function(Rc::new(LoxFunction {
                        id: UniqueId::new(),
                        fun: decl.fun.clone(),
                        closure: self.environment.clone(),
                        ctx: ctx.clone(),
                    })),
                )
                .map_err(|_| RuntimeError::AlreadyDefinedVariable {
                    name: decl.fun.name.name.clone(),
                    found_at: SourceSpan::range(
                        decl.source_span.start(),
                        decl.fun.name.source_span.end(),
                    ),
                    source_code: ctx.source_code.clone(),
                }),
            Decl::Class(decl) => self
                .environment
                .as_ref()
                .borrow_mut()
                .define(
                    &decl.name.name,
                    RuntimeValue::Class(Rc::new(LoxClass {
                        id: UniqueId::new(),
                        name: decl.name.name.clone(),
                        closure: self.environment.clone(),
                        methods: decl.methods.clone(),
                        ctx: ctx.clone(),
                    })),
                )
                .map_err(|_| RuntimeError::AlreadyDefinedVariable {
                    name: decl.name.name.clone(),
                    found_at: SourceSpan::range(
                        decl.source_span.start(),
                        decl.name.source_span.end(),
                    ),
                    source_code: ctx.source_code.clone(),
                }),
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
            .define(&decl.identifier.name, initial_value)
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
                if Completion::from(self.eval_expr(&stmt.condition, ctx))?.cast_boolean() {
                    self.eval_stmt(&stmt.then_branch, ctx)?;
                } else if let Some(else_branch) = &stmt.else_branch {
                    self.eval_stmt(else_branch, ctx)?;
                }
                Completion::Normal(RuntimeValue::nil())
            }
            Stmt::While(stmt) => {
                while Completion::from(self.eval_expr(&stmt.condition, ctx))?.cast_boolean() {
                    self.eval_stmt(&stmt.body, ctx)?;
                }
                Completion::Normal(RuntimeValue::nil())
            }
            Stmt::Return(stmt) => {
                let value = match &stmt.expression {
                    Some(expression) => Completion::from(self.eval_expr(expression, ctx))?,
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
            Expr::Literal(LiteralExpr { value, .. }) => Ok(value.clone().into()),
            Expr::Variable(VariableExpr { identifier }) => {
                self.lookup_identifier(identifier, |environment| {
                    environment.get(&identifier.name).ok_or_else(|| {
                        RuntimeError::UndefinedVariable {
                            name: identifier.name.clone(),
                            found_at: identifier.source_span(),
                            source_code: ctx.source_code.clone(),
                        }
                    })
                })
            }
            Expr::Grouping(GroupingExpr { expr }) => self.eval_expr(expr, ctx),
            Expr::Assignment(AssignmentExpr { target, value }) => {
                let value = self.eval_expr(value, ctx)?;
                match target {
                    AssignmentTargetExpr::Variable(target) => {
                        self.lookup_identifier_mut(&target.identifier, |environment| {
                            environment
                                .assign(&target.identifier.name, value.clone())
                                .ok_or_else(|| RuntimeError::UndefinedVariable {
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
                    &*native_fn,
                    &call_expr.arguments,
                    ctx,
                ),
                RuntimeValue::Function(fun) => {
                    self.eval_call(call_expr.source_span(), &*fun, &call_expr.arguments, ctx)
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
            Expr::This(this_expr) => {
                self.environment
                    .borrow()
                    .get_this()
                    .ok_or_else(|| RuntimeError::UnknownProperty {
                        name: "this".to_string(),
                        found_at: this_expr.source_span(),
                        source_code: ctx.source_code.clone(),
                    })
            }
        }
    }
    fn eval_call<Callable: LoxCallable>(
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
            .define(
                name,
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

trait LoxCallable {
    fn arity(&self) -> usize;
    fn call<W: Write>(
        &self,
        interpreter: &mut Interpreter<W>,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, RuntimeError>;
}

pub struct LoxNativeFunction {
    id: usize,
    name: String,
    arity: usize,
    implementation: fn(&[RuntimeValue]) -> Result<RuntimeValue, RuntimeError>,
}
impl LoxCallable for LoxNativeFunction {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call<W: Write>(
        &self,
        _: &mut Interpreter<W>,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, RuntimeError> {
        (self.implementation)(args)
    }
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

pub struct LoxFunction {
    id: UniqueId,
    fun: Rc<Fun>,
    closure: EnvironmentRef,
    ctx: Ctx,
}
impl LoxCallable for LoxFunction {
    fn arity(&self) -> usize {
        self.fun.parameters.len()
    }

    fn call<W: Write>(
        &self,
        interpreter: &mut Interpreter<W>,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, RuntimeError> {
        let mut call_env = Environment::new_with_parent(self.closure.clone());
        for (name, value) in self.fun.parameters.iter().zip_eq(args) {
            call_env.define(&name.name, value.clone()).map_err(|_| {
                RuntimeError::AlreadyDefinedVariable {
                    name: name.name.clone(),
                    found_at: name.source_span(),
                    source_code: self.ctx.source_code.clone(),
                }
            })?;
        }
        interpreter.run_with_env(
            call_env.wrap(),
            |interpreter| -> Result<RuntimeValue, RuntimeError> {
                for stmt in self.fun.body.iter() {
                    match interpreter.eval_decl_or_stmt(stmt, &self.ctx) {
                        Completion::Normal(_) => continue,
                        Completion::Return(value) => return Ok(value),
                        Completion::Error(err) => return Err(err),
                    }
                }
                Ok(RuntimeValue::nil())
            },
        )
    }
}
impl Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {}() {{ ... }}", self.fun.name.name,)
    }
}
impl Debug for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}
impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

pub struct LoxClass {
    id: UniqueId,
    name: String,
    closure: EnvironmentRef,
    methods: Vec<Rc<Fun>>,
    ctx: Ctx,
}
impl Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<class {}>", self.name)
    }
}
impl Debug for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}
impl PartialEq for LoxClass {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl LoxCallable for Rc<LoxClass> {
    fn arity(&self) -> usize {
        0
    }

    fn call<W: Write>(
        &self,
        _: &mut Interpreter<W>,
        _: &[RuntimeValue],
    ) -> Result<RuntimeValue, RuntimeError> {
        let object = Rc::new(LoxObject {
            id: UniqueId::new(),
            class: self.clone(),
            values: RefCell::new(HashMap::new()),
        });
        let object_closure = Environment::new_with_parent(self.closure.clone()).wrap();

        for method in self.methods.iter() {
            let fun = LoxFunction {
                id: UniqueId::new(),
                fun: method.clone(),
                closure: object_closure.clone(),
                ctx: self.ctx.clone(),
            };
            object.values.borrow_mut().insert(
                method.name.name.clone(),
                RuntimeValue::Function(Rc::new(fun)),
            );
        }

        object_closure.borrow_mut().this_value = Some(RuntimeValue::Object(object.clone()));
        Ok(RuntimeValue::Object(object))
    }
}

pub struct LoxObject {
    id: UniqueId,
    class: Rc<LoxClass>,
    values: RefCell<HashMap<String, RuntimeValue>>,
}
impl LoxObject {
    fn get(&self, name: &str) -> Option<RuntimeValue> {
        self.values.borrow().get(name).cloned()
    }
    fn set(&self, name: &str, value: RuntimeValue) {
        self.values.borrow_mut().insert(name.to_string(), value);
    }
}
impl Display for LoxObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<object {}>", self.class.name)
    }
}
impl Debug for LoxObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}
impl PartialEq for LoxObject {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

#[derive(Clone, PartialEq)]
pub enum RuntimeValue {
    Basic(Value),
    NativeFunction(Rc<LoxNativeFunction>),
    Function(Rc<LoxFunction>),
    Class(Rc<LoxClass>),
    Object(Rc<LoxObject>),
}
impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeValue::Basic(value) => Display::fmt(value, f),
            RuntimeValue::NativeFunction(value) => Display::fmt(value, f),
            RuntimeValue::Function(value) => Display::fmt(value, f),
            RuntimeValue::Class(value) => Display::fmt(value, f),
            RuntimeValue::Object(value) => Display::fmt(value, f),
        }
    }
}
impl Debug for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Basic(value) => Debug::fmt(value, f),
            Self::NativeFunction(value) => Debug::fmt(value, f),
            Self::Function(value) => Debug::fmt(value, f),
            Self::Class(value) => Debug::fmt(value, f),
            Self::Object(value) => Debug::fmt(value, f),
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
            RuntimeValue::Function(_) => ValueType::Function,
            RuntimeValue::Class(_) => ValueType::Class,
            RuntimeValue::Object(_) => ValueType::Object,
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
    parent: Option<EnvironmentRef>,
    this_value: Option<RuntimeValue>,
}
impl Environment {
    fn wrap(self) -> EnvironmentRef {
        Rc::new(RefCell::new(self))
    }
    fn new() -> Self {
        Self {
            values: HashMap::new(),
            parent: None,
            this_value: None,
        }
    }
    fn new_with_parent(parent: EnvironmentRef) -> Self {
        Environment {
            parent: Some(parent),
            ..Default::default()
        }
    }
    fn is_global(&self) -> bool {
        self.parent.is_none()
    }
    fn is_local(&self) -> bool {
        !self.is_global()
    }
    fn define(&mut self, name: &str, value: RuntimeValue) -> Result<RuntimeValue, ()> {
        if self.values.contains_key(name) && self.is_local() {
            Err(())
        } else {
            self.values.insert(name.to_string(), value);
            Ok(self.get(name).unwrap())
        }
    }
    fn get(&self, name: &str) -> Option<RuntimeValue> {
        self.values.get(name).map(Clone::clone).or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().get(name))
        })
    }
    fn assign(&mut self, name: &str, value: RuntimeValue) -> Option<RuntimeValue> {
        if let Some(target) = self.values.get_mut(name) {
            *target = value;
            Some(self.get(name).unwrap())
        } else if let Some(parent) = &mut self.parent {
            parent.as_ref().borrow_mut().assign(name, value)
        } else {
            None
        }
    }
    fn ancestor<T, F: Fn(&Self) -> T>(&self, depth: usize, cb: F) -> Option<T> {
        if depth == 0 {
            Some(cb(self))
        } else {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().ancestor(depth - 1, cb))
        }
    }
    fn ancestor_mut<T, F: Fn(&mut Self) -> T>(&mut self, depth: usize, cb: F) -> Option<T> {
        if depth == 0 {
            Some(cb(self))
        } else {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow_mut().ancestor_mut(depth - 1, cb))
        }
    }
    fn get_this(&self) -> Option<RuntimeValue> {
        self.this_value.as_ref().cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().get_this())
        })
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::size_of;

    #[test]
    fn runtime_value_size() {
        assert_eq!(size_of::<RuntimeValue>(), 24);
    }
}
