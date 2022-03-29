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
    Function,
    Block,
    Global,
}

#[derive(Debug)]
struct Scope {
    scope_type: ScopeType,
    entries: HashMap<String, ScopeEntry>,
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
}

pub type Resolutions = SideTable<Identifier, usize>;

pub struct Resolver<'a> {
    scopes: Vec<Scope>,
    errors: Vec<ResolverError>,
    source_reference: SourceReference,
    resolutions: &'a mut Resolutions,
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
                self.declare(&decl.name);
                self.define(&decl.name);

                self.begin_scope(ScopeType::Function);
                for parameter in decl.parameters.iter() {
                    self.declare(parameter);
                    self.define(parameter);
                }
                self.resolve_block(&decl.body);
                self.end_scope();
            }
        }
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
                }
                if !self.is_in_function() {
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
                if let Some(ScopeEntry {
                    status: ScopeEntryStatus::Declared,
                    declared_at,
                }) = self.get_entry(&expr.identifier.name)
                {
                    self.errors
                        .push(ResolverError::VariableUsedInOwnInitializer {
                            declared_at,
                            used_at: expr.source_span(),
                            source_code: self.source_reference.clone(),
                        })
                }
                self.resolve_local(&expr.identifier);
            }
            Expr::Assignment(expr) => {
                self.resolve_expr(&expr.value);
                self.resolve_local(&expr.target);
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
        }
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

        // TODO: unresolved global?
    }
    fn is_in_function(&self) -> bool {
        self.scopes
            .iter()
            .any(|scope| scope.scope_type == ScopeType::Function)
    }
    fn begin_scope(&mut self, scope_type: ScopeType) {
        self.scopes.push(Scope {
            scope_type,
            entries: HashMap::new(),
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
