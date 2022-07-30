use super::{
    chunk::{Chunk, ConstantValue, OpCode, OpDebug},
    gc::GcString,
};
use crate::{ast::*, SourceReference, SourceSpan};
use miette::Diagnostic;
use std::convert::TryFrom;
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum CompilerError {
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
    #[error("Cannot use variable in own initializer")]
    VariableUsedInOwnInitializer {
        #[label("Variable is being declared here")]
        declared_at: SourceSpan,
        #[label("Variable is being used here")]
        used_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
}

#[derive(Error, Diagnostic, Debug)]
#[error("CompilerErrors")]
pub struct CompilerErrors {
    #[related]
    pub errors: Vec<CompilerError>,
}

pub struct Compiler {
    chunk: Chunk,
    source_reference: SourceReference,
    locals: Vec<Local>,
    scope_depth: usize,
    errors: Vec<CompilerError>,
}
impl Compiler {
    pub fn compile(
        Program {
            statements,
            source_reference,
        }: Program,
    ) -> Result<Chunk, CompilerErrors> {
        let mut compiler = Self {
            chunk: Chunk::new(source_reference.clone()),
            source_reference,
            locals: Vec::new(),
            scope_depth: 0,
            errors: Vec::new(),
        };

        for decl_or_stmt in statements {
            compiler.compile_decl_or_stmt(decl_or_stmt);
        }

        if compiler.errors.is_empty() {
            Ok(compiler.chunk)
        } else {
            Err(CompilerErrors {
                errors: compiler.errors,
            })
        }
    }
    fn compile_decl_or_stmt(&mut self, decl_or_stmt: DeclOrStmt) {
        match decl_or_stmt {
            DeclOrStmt::Decl(decl) => self.compile_decl(decl),
            DeclOrStmt::Stmt(stmt) => self.compile_stmt(stmt),
        }
    }
    fn compile_decl(&mut self, decl: Decl) {
        match decl {
            Decl::Var(decl) => self.compile_var_decl(decl),
            Decl::Fun(_) => todo!(),
            Decl::Class(_) => todo!(),
        }
    }

    fn compile_var_decl(&mut self, decl: VarDecl) {
        let span = decl.source_span();
        let local_index = self.declare_variable(decl.identifier.clone());

        if let Some(initializer) = decl.initializer {
            self.compile_expr(initializer);
        } else {
            self.chunk
                .write_basic_op(OpCode::Nil, OpDebug::single(decl.source_span()));
        }

        if let Some(local_index) = local_index {
            self.locals[local_index].mark_initialized(self.scope_depth);
            return;
        }

        let debug = OpDebug::new(
            SourceSpan::range(decl.var_span.start(), decl.identifier.source_span().end()),
            span,
        );
        self.chunk.write_global_op(
            OpCode::DefineGlobal,
            GcString::new(decl.identifier.name),
            debug,
        )
    }
    fn compile_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Print(stmt) => self.compile_print_stmt(stmt),
            Stmt::Expr(stmt) => self.compile_expr_stmt(stmt),
            Stmt::Block(stmt) => self.compile_block_stmt(stmt),
            Stmt::If(stmt) => self.compile_if_stmt(stmt),
            Stmt::While(stmt) => self.compile_while_stmt(stmt),
            _ => unimplemented!("{:?}", stmt),
        }
    }
    fn compile_print_stmt(&mut self, stmt: PrintStmt) {
        let span = stmt.source_span();
        self.compile_expr(stmt.expression);
        self.chunk
            .write_basic_op(OpCode::Print, OpDebug::new(stmt.print_span, span));
    }
    fn compile_expr_stmt(&mut self, stmt: ExprStmt) {
        let span = stmt.source_span();
        self.compile_expr(stmt.expression);
        self.chunk
            .write_basic_op(OpCode::Pop, OpDebug::single(span));
    }
    fn compile_block_stmt(&mut self, stmt: BlockStmt) {
        self.begin_scope();
        for stmt in stmt.body {
            self.compile_decl_or_stmt(stmt);
        }
        self.end_scope(stmt.close_span);
    }
    fn compile_if_stmt(&mut self, stmt: IfStmt) {
        let span = stmt.source_span();
        self.compile_expr(stmt.condition);
        let jump_to_else = self
            .chunk
            .write_jump_op(OpCode::JumpIfFalse, OpDebug::new(stmt.if_span, span));

        self.chunk
            .write_basic_op(OpCode::Pop, OpDebug::new(stmt.if_span, span));
        self.compile_stmt(*stmt.then_branch);
        let jump_to_after = self
            .chunk
            .write_jump_op(OpCode::Jump, OpDebug::new(stmt.if_span, span));

        self.chunk.patch_jump_op_to_here(jump_to_else);
        self.chunk
            .write_basic_op(OpCode::Pop, OpDebug::new(stmt.if_span, span));

        if let Some(else_branch) = stmt.else_branch {
            self.compile_stmt(*else_branch);
        }

        self.chunk.patch_jump_op_to_here(jump_to_after);
    }
    fn compile_while_stmt(&mut self, stmt: WhileStmt) {
        let span = stmt.source_span();
        let keyword_span = stmt.while_span;
        let op_debug = OpDebug::new(keyword_span, span);

        let loop_start = self.chunk.get_loop_target();
        self.compile_expr(stmt.condition);
        let jump_to_after = self
            .chunk
            .write_jump_op(OpCode::JumpIfFalse, op_debug.clone());
        self.chunk.write_basic_op(OpCode::Pop, op_debug.clone());

        self.compile_stmt(*stmt.body);
        self.chunk.write_loop_op(loop_start, op_debug.clone());
        self.chunk.patch_jump_op_to_here(jump_to_after);
        self.chunk.write_basic_op(OpCode::Pop, op_debug);
    }
    fn compile_expr(&mut self, expr: Expr) {
        let span = expr.source_span();
        match expr {
            Expr::Literal(expr) => {
                match expr.value {
                    LiteralValue::String(value) => self.chunk.write_constant(
                        ConstantValue::String(GcString::new(value)),
                        OpDebug::single(span),
                    ),
                    LiteralValue::Number(value) => self
                        .chunk
                        .write_constant(ConstantValue::Number(value.into()), OpDebug::single(span)),
                    LiteralValue::Boolean(value) => self
                        .chunk
                        .write_constant(ConstantValue::Boolean(value), OpDebug::single(span)),
                    LiteralValue::Nil => self
                        .chunk
                        .write_basic_op(OpCode::Nil, OpDebug::single(span)),
                };
            }
            Expr::Unary(expr) => self.compile_unary_expr(expr),
            Expr::Binary(expr) => self.compile_binary_expr(expr),
            Expr::Grouping(expr) => self.compile_expr(*expr.expr),
            Expr::Variable(expr) => {
                if let Some((index, _)) = self.resolve_variable(&expr.identifier) {
                    self.chunk.write_local_op(
                        OpCode::ReadLocal,
                        u8::try_from(index).unwrap(),
                        OpDebug::single(span),
                    );
                } else {
                    self.chunk.write_global_op(
                        OpCode::ReadGlobal,
                        GcString::new(expr.identifier.name),
                        OpDebug::single(span),
                    );
                }
            }
            Expr::Assignment(expr) => match expr.target {
                AssignmentTargetExpr::Variable(target) => {
                    self.compile_expr(*expr.value);
                    let id_span = target.identifier.source_span();
                    if let Some((index, _)) = self.resolve_variable(&target.identifier) {
                        self.chunk.write_local_op(
                            OpCode::SetLocal,
                            u8::try_from(index).unwrap(),
                            OpDebug::new(id_span, span),
                        )
                    } else {
                        self.chunk.write_global_op(
                            OpCode::SetGlobal,
                            GcString::new(target.identifier.name),
                            OpDebug::new(id_span, span),
                        );
                    }
                }
                AssignmentTargetExpr::PropertyAccess(_) => todo!(),
            },
            _ => unimplemented!("{:?}", expr),
        }
    }
    fn compile_unary_expr(&mut self, expr: UnaryExpr) {
        let outer_span = expr.source_span();
        self.compile_expr(*expr.right);
        let op_code = match expr.operator.inner() {
            UnaryOperator::Minus => OpCode::Negate,
            UnaryOperator::Not => OpCode::Not,
        };
        self.chunk.write_basic_op(
            op_code,
            OpDebug::new(expr.operator.source_span(), outer_span),
        );
    }
    fn compile_binary_expr(&mut self, expr: BinaryExpr) {
        let outer_span = expr.source_span();

        let op_code = match expr.operator.inner() {
            BinaryOperator::LogicalAnd => {
                self.compile_expr(*expr.left);
                let jump_to_after = self.chunk.write_jump_op(
                    OpCode::JumpIfFalse,
                    OpDebug::new(expr.operator.source_span(), outer_span),
                );
                self.chunk.write_basic_op(
                    OpCode::Pop,
                    OpDebug::new(expr.operator.source_span(), outer_span),
                );
                self.compile_expr(*expr.right);
                self.chunk.patch_jump_op_to_here(jump_to_after);
                return;
            }
            BinaryOperator::LogicalOr => {
                self.compile_expr(*expr.left);
                let jump_to_after = self.chunk.write_jump_op(
                    OpCode::JumpIfTrue,
                    OpDebug::new(expr.operator.source_span(), outer_span),
                );
                self.chunk.write_basic_op(
                    OpCode::Pop,
                    OpDebug::new(expr.operator.source_span(), outer_span),
                );
                self.compile_expr(*expr.right);
                self.chunk.patch_jump_op_to_here(jump_to_after);
                return;
            }
            BinaryOperator::Plus => OpCode::Add,
            BinaryOperator::Minus => OpCode::Subtract,
            BinaryOperator::Multiply => OpCode::Multiply,
            BinaryOperator::Divide => OpCode::Divide,
            BinaryOperator::NotEqualTo => OpCode::NotEqualTo,
            BinaryOperator::EqualTo => OpCode::EqualTo,
            BinaryOperator::LessThan => OpCode::LessThan,
            BinaryOperator::LessThanOrEqualTo => OpCode::LessThanOrEqualTo,
            BinaryOperator::GreaterThan => OpCode::GreaterThan,
            BinaryOperator::GreaterThanOrEqualTo => OpCode::GreaterThanOrEqualTo,
        };

        self.compile_expr(*expr.left);
        self.compile_expr(*expr.right);

        self.chunk.write_basic_op(
            op_code,
            OpDebug::new(expr.operator.source_span(), outer_span),
        );
    }
    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }
    fn end_scope(&mut self, ending_span: SourceSpan) {
        let locals_to_remove = self.iter_locals_from_current_scope().count();
        for _ in 0..locals_to_remove {
            self.chunk
                .write_basic_op(OpCode::Pop, OpDebug::single(ending_span));
            self.locals.pop();
        }

        self.scope_depth -= 1;
    }
    fn declare_variable(&mut self, id: Identifier) -> Option<usize> {
        if self.scope_depth == 0 {
            return None;
        }

        let already_defined_error = self
            .iter_locals_from_current_scope()
            .find(|(_, local)| local.id.name == id.name)
            .map(
                |(_, existing_local)| CompilerError::VariableAlreadyDeclared {
                    name: id.name.clone(),
                    found_at: id.source_span(),
                    first_found_at: existing_local.id.source_span(),
                    source_code: self.source_reference.clone(),
                },
            );
        self.report_error(already_defined_error);

        let index = self.locals.len();
        self.locals.push(Local::new(id));
        Some(index)
    }
    fn resolve_variable(&mut self, id: &Identifier) -> Option<(usize, Local)> {
        let local = self
            .iter_locals()
            .find(|(_, local)| local.id.name == id.name)
            .map(|(idx, local)| (idx, local.clone()));

        if let Some((_, ref local)) = local {
            if local.depth.is_none() {
                self.report_error(CompilerError::VariableUsedInOwnInitializer {
                    declared_at: local.id.source_span(),
                    used_at: id.source_span(),
                    source_code: self.source_reference.clone(),
                })
            }
        }

        local
    }
    fn report_error(&mut self, error: impl Into<Option<CompilerError>>) {
        if let Some(error) = error.into() {
            self.errors.push(error);
        }
    }
    fn iter_locals(&self) -> impl Iterator<Item = (usize, &Local)> {
        self.locals.iter().enumerate().rev()
    }
    fn iter_locals_from_current_scope(&self) -> impl Iterator<Item = (usize, &Local)> {
        let scope_depth = self.scope_depth;
        self.iter_locals().take_while(move |(_, local)| {
            local
                .depth
                .map(|depth| depth >= scope_depth)
                .unwrap_or(true)
        })
    }
}

#[derive(Debug, Clone)]
struct Local {
    id: Identifier,
    depth: Option<usize>,
}
impl Local {
    fn new(id: Identifier) -> Self {
        Self { id, depth: None }
    }
    fn mark_initialized(&mut self, depth: usize) {
        self.depth = Some(depth);
    }
}
