use std::{
    fmt::{Debug, Display},
    ops::Deref,
};

use crate::{source::SourceSpan, value::Value};

#[derive(Debug)]
pub struct WithSpan<T> {
    inner: T,
    source_span: SourceSpan,
}

impl<T> WithSpan<T> {
    pub fn new(inner: T, source_span: SourceSpan) -> Self {
        Self { inner, source_span }
    }
    pub fn source_span(&self) -> SourceSpan {
        self.source_span
    }
    pub fn inner(&self) -> &T {
        &self.inner
    }
}
impl<T> Deref for WithSpan<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

pub trait AstNode {
    fn source_span(&self) -> SourceSpan;
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<DeclOrStmt>,
}
impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in self.statements.iter() {
            writeln!(f, "{}", stmt)?;
        }
        Ok(())
    }
}
impl AstNode for Program {
    fn source_span(&self) -> SourceSpan {
        match (self.statements.first(), self.statements.last()) {
            (Some(first), Some(last)) => {
                SourceSpan::range(first.source_span().start(), last.source_span().end())
            }
            _ => 0.into(),
        }
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub source_span: SourceSpan,
    pub name: String,
}

impl AstNode for Identifier {
    fn source_span(&self) -> SourceSpan {
        self.source_span
    }
}
impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

#[derive(Debug)]
pub struct VarDecl {
    pub var_span: SourceSpan,
    pub identifier: Identifier,
    pub initializer: Option<Expr>,
}
impl AstNode for VarDecl {
    fn source_span(&self) -> SourceSpan {
        SourceSpan::range(
            self.var_span.start(),
            self.initializer
                .as_ref()
                .map(|init| init.source_span().end())
                .unwrap_or_else(|| self.identifier.source_span().end()),
        )
    }
}
impl Display for VarDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.initializer {
            Some(init) => write!(f, "(var {} {})", self.identifier, init),
            None => write!(f, "(var {})", self.identifier),
        }
    }
}

#[derive(Debug)]
pub enum Decl {
    Var(VarDecl),
}
impl AstNode for Decl {
    fn source_span(&self) -> SourceSpan {
        match self {
            Self::Var(decl) => decl.source_span(),
        }
    }
}
impl Display for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(decl) => Display::fmt(decl, f),
        }
    }
}

#[derive(Debug)]
pub enum DeclOrStmt {
    Decl(Decl),
    Stmt(Stmt),
}
impl AstNode for DeclOrStmt {
    fn source_span(&self) -> SourceSpan {
        match self {
            Self::Decl(decl) => decl.source_span(),
            Self::Stmt(stmt) => stmt.source_span(),
        }
    }
}
impl Display for DeclOrStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Decl(decl) => Display::fmt(decl, f),
            Self::Stmt(stmt) => Display::fmt(stmt, f),
        }
    }
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expression: Expr,
}
impl Display for ExprStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.expression, f)
    }
}
impl AstNode for ExprStmt {
    fn source_span(&self) -> SourceSpan {
        self.expression.source_span()
    }
}

#[derive(Debug)]
pub struct PrintStmt {
    pub expression: Expr,
    pub print_span: SourceSpan,
}
impl Display for PrintStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(print {})", self.expression)
    }
}
impl AstNode for PrintStmt {
    fn source_span(&self) -> SourceSpan {
        SourceSpan::range(self.print_span.start(), self.expression.source_span().end())
    }
}

#[derive(Debug)]
pub struct BlockStmt {
    pub statements: Vec<DeclOrStmt>,
    pub open_span: SourceSpan,
    pub close_span: SourceSpan,
}
impl Display for BlockStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("(do ")?;
        for stmt in self.statements.iter() {
            write!(f, "{} ", stmt)?;
        }
        f.write_str(")")
    }
}
impl AstNode for BlockStmt {
    fn source_span(&self) -> SourceSpan {
        SourceSpan::range(self.open_span.start(), self.close_span.end())
    }
}

#[derive(Debug)]
pub enum Stmt {
    Expr(ExprStmt),
    Print(PrintStmt),
    Block(BlockStmt),
}
impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(stmt) => Display::fmt(stmt, f),
            Self::Print(stmt) => Display::fmt(stmt, f),
            Self::Block(stmt) => Display::fmt(stmt, f),
        }
    }
}
impl AstNode for Stmt {
    fn source_span(&self) -> SourceSpan {
        match self {
            Self::Expr(stmt) => stmt.source_span(),
            Self::Print(stmt) => stmt.source_span(),
            Self::Block(stmt) => stmt.source_span(),
        }
    }
}

#[derive(Debug)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    NotEqualTo,
    EqualTo,
    LessThan,
    LessThanOrEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
}
impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::NotEqualTo => "!=",
            Self::EqualTo => "==",
            Self::LessThan => "<",
            Self::LessThanOrEqualTo => "<=",
            Self::GreaterThan => ">",
            Self::GreaterThanOrEqualTo => ">=",
        })
    }
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub operator: WithSpan<BinaryOperator>,
}
impl Display for BinaryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.operator.inner, self.left, self.right)
    }
}
impl AstNode for BinaryExpr {
    fn source_span(&self) -> SourceSpan {
        SourceSpan::range(
            self.left.source_span().start(),
            self.right.source_span().end(),
        )
    }
}

#[derive(Debug)]
pub struct LiteralExpr {
    pub value: Value,
    pub source_span: SourceSpan,
}
impl Display for LiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.value, f)
    }
}
impl AstNode for LiteralExpr {
    fn source_span(&self) -> SourceSpan {
        self.source_span
    }
}

#[derive(Debug)]
pub enum UnaryOperator {
    Not,
    Minus,
}
impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Minus => f.write_str("-"),
            Self::Not => f.write_str("!"),
        }
    }
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub operator: WithSpan<UnaryOperator>,
    pub right: Box<Expr>,
}
impl Display for UnaryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {})", self.operator.inner, self.right)
    }
}
impl AstNode for UnaryExpr {
    fn source_span(&self) -> SourceSpan {
        SourceSpan::range(
            self.operator.source_span.start(),
            self.right.source_span().end(),
        )
    }
}

#[derive(Debug)]
pub struct VariableExpr {
    pub identifier: Identifier,
}
impl Display for VariableExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.identifier, f)
    }
}
impl AstNode for VariableExpr {
    fn source_span(&self) -> SourceSpan {
        self.identifier.source_span()
    }
}

#[derive(Debug)]
pub struct AssignmentExpr {
    pub target: Identifier,
    pub value: Box<Expr>,
}
impl Display for AssignmentExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(assign {} {})", self.target, self.value)
    }
}
impl AstNode for AssignmentExpr {
    fn source_span(&self) -> SourceSpan {
        SourceSpan::range(
            self.target.source_span().start(),
            self.value.source_span().end(),
        )
    }
}

#[derive(Debug)]
pub struct GroupingExpr {
    pub expr: Box<Expr>,
}
impl Display for GroupingExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.expr, f)
    }
}
impl AstNode for GroupingExpr {
    fn source_span(&self) -> SourceSpan {
        self.expr.source_span()
    }
}

#[derive(Debug)]
pub enum Expr {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Literal(LiteralExpr),
    Variable(VariableExpr),
    Assignment(AssignmentExpr),
    Grouping(GroupingExpr),
}
impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Binary(expr) => Display::fmt(expr, f),
            Self::Unary(expr) => Display::fmt(expr, f),
            Self::Literal(expr) => Display::fmt(expr, f),
            Self::Variable(expr) => Display::fmt(expr, f),
            Self::Assignment(expr) => Display::fmt(expr, f),
            Self::Grouping(expr) => Display::fmt(expr, f),
        }
    }
}
impl AstNode for Expr {
    fn source_span(&self) -> SourceSpan {
        match self {
            Self::Binary(expr) => expr.source_span(),
            Self::Unary(expr) => expr.source_span(),
            Self::Literal(expr) => expr.source_span(),
            Self::Variable(expr) => expr.source_span(),
            Self::Assignment(expr) => expr.source_span(),
            Self::Grouping(expr) => expr.source_span(),
        }
    }
}
