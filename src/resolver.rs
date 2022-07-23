use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use crate::{ast::*, side_table::SideTable, SourceReference, SourceSpan};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScopeEntryStatus {
    Declared,
    Defined,
}

#[derive(Debug, Clone)]
struct ScopeEntry {
    declared_at: SourceSpan,
    status: ScopeEntryStatus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScopeType {
    Class,
    Function(FunctionType),
    Block,
    Global,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FunctionType {
    Function,
    Initializer,
    Method,
}

#[derive(Debug)]
struct Scope {
    scope_type: ScopeType,
    entries: HashMap<String, ScopeEntry>,
    has_this: bool,
}

#[derive(Error, Diagnostic, Debug)]
pub enum ResolverError {
    #[error("Cannot use variable in own initializer")]
    VariableUsedInOwnInitializer {
        #[label("Variable is being declared here")]
        declared_at: SourceSpan,
        #[label("Variable is being used here")]
        used_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Already a variable named {name} in this scope")]
    VariableAlreadyDeclared {
        name: String,
        #[label("'{name}' is declared in the same scope again here")]
        found_at: SourceSpan,
        #[label("'{name}' was first declared here")]
        first_found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Cannot return from top-level code")]
    ReturnFromTopLevel {
        #[label("Return outside of function found here")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Cannot use `this` outside of a class")]
    ThisOutsideOfClass {
        #[label("Found here")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Cannot return a value from an initializer")]
    ReturnValueFromInit {
        #[label("Value found here")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Class can't inherit from itself")]
    ClassInheritFromSelf {
        name: String,
        #[label("Class {name} is trying to inherit from itself")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
}

pub type Resolutions = SideTable<Identifier, usize>;

pub struct Resolver<'a> {
    scopes: Vec<Scope>,
    errors: Vec<ResolverError>,
    source_reference: SourceReference,
    resolutions: &'a mut Resolutions,
    current_function: Option<FunctionType>,
}

impl Resolver<'_> {
    pub fn resolve(
        program: &Program,
        resolutions: &mut Resolutions,
    ) -> Result<(), Vec<ResolverError>> {
        let mut resolver = Resolver {
            scopes: Vec::new(),
            errors: Vec::new(),
            source_reference: program.source_reference.clone(),
            resolutions,
            current_function: None,
        };
        resolver.resolve_program(program);
        if resolver.errors.is_empty() {
            Ok(())
        } else {
            Err(resolver.errors)
        }
    }
    fn resolve_program(&mut self, program: &Program) {
        self.begin_scope(ScopeType::Global);
        self.resolve_block(&program.statements);
        self.end_scope();
    }
    fn resolve_block(&mut self, body: &[DeclOrStmt]) {
        for stmt in body {
            match stmt {
                DeclOrStmt::Decl(decl) => self.resolve_decl(decl),
                DeclOrStmt::Stmt(stmt) => self.resolve_stmt(stmt),
            }
        }
    }
    fn resolve_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Var(decl) => {
                self.declare(&decl.identifier);
                if let Some(initializer) = &decl.initializer {
                    self.resolve_expr(initializer);
                }
                self.define(&decl.identifier);
            }
            Decl::Fun(decl) => {
                self.declare(&decl.fun.name);
                self.define(&decl.fun.name);
                self.resolve_fun(&decl.fun, FunctionType::Function);
            }
            Decl::Class(decl) => {
                self.declare(&decl.name);
                self.define(&decl.name);

                if let Some(super_class) = &decl.super_class {
                    if super_class.name == decl.name.name {
                        self.errors.push(ResolverError::ClassInheritFromSelf {
                            name: decl.name.name.clone(),
                            found_at: SourceSpan::range(
                                decl.name.source_span.start(),
                                super_class.source_span().end(),
                            ),
                            source_code: self.source_reference.clone(),
                        });
                    }
                    self.resolve_variable(super_class)
                }

                self.begin_scope(ScopeType::Class);
                self.scopes.last_mut().unwrap().has_this = true;
                for method in &decl.methods {
                    self.resolve_fun(
                        method,
                        if method.name.name == "init" {
                            FunctionType::Initializer
                        } else {
                            FunctionType::Method
                        },
                    );
                }
                self.end_scope();
            }
        }
    }
    fn resolve_fun(&mut self, fun: &Fun, function_type: FunctionType) {
        let enclosing_function = self.current_function;
        self.current_function = Some(function_type);
        self.begin_scope(ScopeType::Function(function_type));
        for parameter in &fun.parameters {
            self.declare(parameter);
            self.define(parameter);
        }
        self.resolve_block(&fun.body);
        self.end_scope();
        self.current_function = enclosing_function;
    }
    fn resolve_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(stmt) => {
                self.resolve_expr(&stmt.expression);
            }
            Stmt::Print(stmt) => {
                self.resolve_expr(&stmt.expression);
            }
            Stmt::Block(stmt) => {
                self.begin_scope(ScopeType::Block);
                self.resolve_block(&stmt.body);
                self.end_scope();
            }
            Stmt::If(stmt) => {
                self.resolve_expr(&stmt.condition);
                self.resolve_stmt(&stmt.then_branch);
                if let Some(else_branch) = &stmt.else_branch {
                    self.resolve_stmt(else_branch);
                }
            }
            Stmt::While(stmt) => {
                self.resolve_expr(&stmt.condition);
                self.resolve_stmt(&stmt.body);
            }
            Stmt::Return(stmt) => {
                if let Some(expression) = &stmt.expression {
                    self.resolve_expr(expression);

                    if self.current_function == Some(FunctionType::Initializer) {
                        self.errors.push(ResolverError::ReturnValueFromInit {
                            found_at: expression.source_span(),
                            source_code: self.source_reference.clone(),
                        })
                    }
                }

                if self.current_function == None {
                    self.errors.push(ResolverError::ReturnFromTopLevel {
                        found_at: stmt.return_span,
                        source_code: self.source_reference.clone(),
                    })
                }
            }
        }
    }
    fn resolve_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Binary(expr) => {
                self.resolve_expr(&expr.left);
                self.resolve_expr(&expr.right);
            }
            Expr::Unary(expr) => {
                self.resolve_expr(&expr.right);
            }
            Expr::Literal(_) => {}
            Expr::Variable(expr) => {
                self.resolve_variable(&expr.identifier);
            }
            Expr::Assignment(expr) => {
                self.resolve_expr(&expr.value);
                match &expr.target {
                    AssignmentTargetExpr::Variable(expr) => self.resolve_local(&expr.identifier),
                    AssignmentTargetExpr::PropertyAccess(expr) => self.resolve_expr(&expr.object),
                }
            }
            Expr::Grouping(expr) => {
                self.resolve_expr(&expr.expr);
            }
            Expr::Call(expr) => {
                self.resolve_expr(&expr.callee);
                for argument in expr.arguments.iter() {
                    self.resolve_expr(argument);
                }
            }
            Expr::PropertyAccess(expr) => {
                self.resolve_expr(&expr.object);
            }
            Expr::This(expr) => {
                if !self.scopes.iter().rev().any(|scope| scope.has_this) {
                    self.errors.push(ResolverError::ThisOutsideOfClass {
                        found_at: expr.source_span(),
                        source_code: self.source_reference.clone(),
                    })
                }
            }
        }
    }
    fn resolve_variable(&mut self, identifier: &Identifier) {
        if let Some(ScopeEntry{status, declared_at}) = self.get_entry(&identifier.name) && status == ScopeEntryStatus::Declared {
            self.errors
                .push(ResolverError::VariableUsedInOwnInitializer {
                    declared_at,
                    used_at: identifier.source_span(),
                    source_code: self.source_reference.clone(),
                })
        }

        self.resolve_local(identifier);
    }
    fn resolve_local(&mut self, identifier: &Identifier) {
        let indexed_scope = self
            .scopes
            .iter()
            .rev()
            .enumerate()
            .find(|(_, scope)| scope.entries.contains_key(&identifier.name));

        if let Some((index, _)) = indexed_scope {
            self.resolutions.set(identifier, index);
        }
    }
    fn begin_scope(&mut self, scope_type: ScopeType) {
        self.scopes.push(Scope {
            scope_type,
            entries: HashMap::new(),
            has_this: false,
        });
    }
    fn end_scope(&mut self) {
        self.scopes.pop();
    }
    fn declare(&mut self, identifier: &Identifier) {
        if let Some(current_scope) = self.scopes.last_mut() {
            if let Some(entry) = current_scope.entries.get(&identifier.name) {
                if current_scope.scope_type != ScopeType::Global {
                    self.errors.push(ResolverError::VariableAlreadyDeclared {
                        name: identifier.name.clone(),
                        found_at: identifier.source_span(),
                        first_found_at: entry.declared_at,
                        source_code: self.source_reference.clone(),
                    })
                }
                return;
            }
            current_scope.entries.insert(
                identifier.name.clone(),
                ScopeEntry {
                    status: ScopeEntryStatus::Declared,
                    declared_at: identifier.source_span(),
                },
            );
        }
    }
    fn define(&mut self, identifier: &Identifier) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope
                .entries
                .get_mut(&identifier.name)
                .unwrap()
                .status = ScopeEntryStatus::Defined;
        }
    }
    fn get_entry(&self, name: &str) -> Option<ScopeEntry> {
        self.scopes
            .last()
            .and_then(|scope| scope.entries.get(name).cloned())
    }
}
