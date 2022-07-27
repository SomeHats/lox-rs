use super::{
    chunk::{Chunk, OpCode, OpDebug},
    value::Value,
};
use crate::ast::*;

pub struct Compiler {
    chunk: Chunk,
    // source_reference: SourceReference,
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

        for decl_or_stmt in &statements {
            compiler.compile_decl_or_stmt(decl_or_stmt);
        }

        compiler.chunk
    }
    fn compile_decl_or_stmt(&mut self, decl_or_stmt: &DeclOrStmt) {
        match decl_or_stmt {
            DeclOrStmt::Decl(decl) => self.compile_decl(decl),
            DeclOrStmt::Stmt(stmt) => self.compile_stmt(stmt),
        }
    }
    fn compile_decl(&mut self, _decl: &Decl) {
        unimplemented!();
    }
    fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Print(stmt) => self.compile_print_stmt(stmt),
            Stmt::Expr(stmt) => self.compile_expr_stmt(stmt),
            _ => unimplemented!("{:?}", stmt),
        }
    }
    fn compile_print_stmt(&mut self, stmt: &PrintStmt) {
        self.compile_expr(&stmt.expression);
        self.chunk.write_basic_op(
            OpCode::Print,
            OpDebug::new(stmt.print_span, stmt.source_span()),
        );
    }
    fn compile_expr_stmt(&mut self, stmt: &ExprStmt) {
        self.compile_expr(&stmt.expression);
        self.chunk
            .write_basic_op(OpCode::Pop, OpDebug::single(stmt.source_span()));
    }
    fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(expr) => self.compile_literal_expr(expr),
            Expr::Unary(expr) => self.compile_unary_expr(expr),
            Expr::Binary(expr) => self.compile_binary_expr(expr),
            Expr::Grouping(expr) => self.compile_expr(&expr.expr),
            _ => unimplemented!("{:?}", expr),
        }
    }
    fn compile_literal_expr(&mut self, expr: &LiteralExpr) {
        let value: Value = match expr.value {
            LiteralValue::Nil => Value::Nil,
            LiteralValue::Number(value) => value.into(),
            LiteralValue::Boolean(value) => value.into(),
            _ => unimplemented!("{:?}", expr),
        };
        self.chunk
            .write_constant(value, OpDebug::single(expr.source_span()))
    }
    fn compile_unary_expr(&mut self, expr: &UnaryExpr) {
        self.compile_expr(&expr.right);
        let op_code = match expr.operator.inner() {
            UnaryOperator::Minus => OpCode::Negate,
            _ => unimplemented!("{:?}", expr),
        };
        self.chunk.write_basic_op(
            op_code,
            OpDebug::new(expr.operator.source_span(), expr.source_span()),
        );
    }
    fn compile_binary_expr(&mut self, expr: &BinaryExpr) {
        self.compile_expr(&expr.left);
        self.compile_expr(&expr.right);
        let op_code = match expr.operator.inner() {
            BinaryOperator::Plus => OpCode::Add,
            BinaryOperator::Minus => OpCode::Subtract,
            BinaryOperator::Multiply => OpCode::Multiply,
            BinaryOperator::Divide => OpCode::Divide,
            _ => unimplemented!("{:?}", expr),
        };
        self.chunk.write_basic_op(
            op_code,
            OpDebug::new(expr.operator.source_span(), expr.source_span()),
        );
    }
}
