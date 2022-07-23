use super::{
    environment::Environment, lox_callable::LoxCallable, Ctx, EnvironmentRef, Interpreter,
    RuntimeError, RuntimeValue,
};
use crate::{
    ast::{self, AstNode},
    side_table::UniqueId,
};
use itertools::Itertools;
use std::{
    fmt::{Debug, Display},
    io::Write,
    rc::Rc,
};

#[derive(Clone)]
pub struct LoxFunction(Rc<LoxFunctionImpl>);
struct LoxFunctionImpl {
    id: UniqueId,
    fun: Rc<ast::Fun>,
    closure: EnvironmentRef,
    ctx: Ctx,
    is_initializer: bool,
}
impl LoxFunction {
    pub fn new(fun: Rc<ast::Fun>, closure: EnvironmentRef, ctx: Ctx, is_initializer: bool) -> Self {
        Self(Rc::new(LoxFunctionImpl {
            id: UniqueId::new(),
            fun,
            closure,
            ctx,
            is_initializer,
        }))
    }
}
impl LoxCallable for LoxFunction {
    fn arity(&self) -> usize {
        self.0.fun.parameters.len()
    }

    fn call<W: Write>(
        &self,
        interpreter: &mut Interpreter<W>,
        args: &[RuntimeValue],
    ) -> Result<RuntimeValue, RuntimeError> {
        let mut call_env = Environment::new_with_parent(self.0.closure.clone());
        for (name, value) in self.0.fun.parameters.iter().zip_eq(args) {
            call_env.define(&name.name, value.clone()).map_err(|_| {
                RuntimeError::AlreadyDefinedVariable {
                    name: name.name.clone(),
                    found_at: name.source_span(),
                    source_code: self.0.ctx.source_code.clone(),
                }
            })?;
        }
        let return_value = interpreter.run_with_env(
            call_env.wrap(),
            |interpreter| -> Result<RuntimeValue, RuntimeError> {
                for stmt in self.0.fun.body.iter() {
                    interpreter.eval_decl_or_stmt(stmt, &self.0.ctx)?;
                }

                Ok(RuntimeValue::nil())
            },
        )?;

        if self.0.is_initializer {
            Ok(self
                .0
                .closure
                .borrow()
                .get_this()
                .expect("must have this if is_initializer"))
        } else {
            Ok(return_value)
        }
    }
}
impl Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {}() {{ ... }}", self.0.fun.name.name,)
    }
}
impl Debug for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}
impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        self.0.id == other.0.id
    }
}
