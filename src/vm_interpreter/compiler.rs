use super::chunk::{Chunk, OpCode, OpDebug};
use crate::ast::*;

pub struct Compiler {
    chunk: Chunk,
}
impl Compiler {
    pub fn compile(
        Program {
            statements,
            source_reference,
        }: Program,
    ) -> Chunk {
        let mut compiler = Self {
            chunk: Chunk::new(source_reference),
            // source_reference: source_reference,
        };

        for decl_or_stmt in statements {
            compiler.compile_decl_or_stmt(decl_or_stmt);
        }

        compiler.chunk
    }
    fn compile_decl_or_stmt(&mut self, decl_or_stmt: DeclOrStmt) {
        match decl_or_stmt {
            DeclOrStmt::Decl(decl) => self.compile_decl(decl),
            DeclOrStmt::Stmt(stmt) => self.compile_stmt(stmt),
        }
    }
    fn compile_decl(&mut self, _decl: Decl) {
        unimplemented!();
    }
    fn compile_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Print(stmt) => self.compile_print_stmt(stmt),
            Stmt::Expr(stmt) => self.compile_expr_stmt(stmt),
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
    fn compile_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Literal(expr) => {
                let span = expr.source_span();
                self.chunk.write_constant(expr.value, OpDebug::single(span))
            }
            Expr::Unary(expr) => self.compile_unary_expr(expr),
            Expr::Binary(expr) => self.compile_binary_expr(expr),
            Expr::Grouping(expr) => self.compile_expr(*expr.expr),
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
        self.compile_expr(*expr.left);
        self.compile_expr(*expr.right);
        let op_code = match expr.operator.inner() {
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
            BinaryOperator::LogicalAnd => OpCode::LogicalAnd,
            BinaryOperator::LogicalOr => OpCode::LogicalOr,
        };
        self.chunk.write_basic_op(
            op_code,
            OpDebug::new(expr.operator.source_span(), outer_span),
        );
    }
}
