use std::{
    fmt::{Debug, Display, Write},
    ops::Deref,
    rc::Rc,
};

use colored::Colorize;

use crate::{
    side_table::{Unique, UniqueId},
    source::SourceSpan,
    value::Value,
    SourceReference,
};

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
    pub source_reference: SourceReference,
}
impl PrettyPrint for Program {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        for stmt in self.statements.iter() {
            stmt.fmt_pretty(f);
            f.break_line();
        }
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
    pub id: UniqueId,
}

impl AstNode for Identifier {
    fn source_span(&self) -> SourceSpan {
        self.source_span
    }
}
impl PrettyPrint for Identifier {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        f.push_str(&self.name);
    }
}
impl Unique for Identifier {
    fn id(&self) -> UniqueId {
        self.id
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
impl PrettyPrint for VarDecl {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        f.block_sexp(|f| {
            f.keyword_item("var");
            f.item(&self.identifier);
            if let Some(initializer) = &self.initializer {
                f.item(initializer);
            }
        });
    }
}

#[derive(Debug)]
pub struct FunDecl {
    pub source_span: SourceSpan,
    pub fun: Rc<Fun>,
}
impl PrettyPrint for FunDecl {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        f.block_sexp(|f| {
            f.keyword_item("fun");
            f.item(&self.fun.name);
            f.inline_sexp(|f| {
                for parameter in &self.fun.parameters {
                    f.item(parameter);
                }
            });
            for stmt in &self.fun.body {
                f.break_line();
                f.item(stmt);
            }
        });
    }
}
impl AstNode for FunDecl {
    fn source_span(&self) -> SourceSpan {
        self.source_span
    }
}

#[derive(Debug)]
pub struct ClassDecl {
    pub source_span: SourceSpan,
    pub name: Identifier,
    pub methods: Vec<Rc<Fun>>,
}
impl AstNode for ClassDecl {
    fn source_span(&self) -> SourceSpan {
        self.source_span
    }
}
impl PrettyPrint for ClassDecl {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        f.block_sexp(|f| {
            f.keyword_item("class");
            f.item(&self.name);
            for method in &*self.methods {
                f.break_line();
                f.block_sexp(|f| {
                    f.keyword_item("method");
                    f.item(&method.name);
                    f.inline_sexp(|f| {
                        for parameter in &method.parameters {
                            f.item(parameter);
                        }
                    });
                    for stmt in &method.body {
                        f.break_line();
                        f.item(stmt);
                    }
                })
            }
        })
    }
}

#[derive(Debug)]
pub struct Fun {
    pub source_span: SourceSpan,
    pub name: Identifier,
    pub parameters: Vec<Identifier>,
    pub body: Vec<DeclOrStmt>,
}
impl AstNode for Fun {
    fn source_span(&self) -> SourceSpan {
        self.source_span
    }
}

#[derive(Debug)]
pub enum Decl {
    Var(VarDecl),
    Fun(FunDecl),
    Class(ClassDecl),
}
impl AstNode for Decl {
    fn source_span(&self) -> SourceSpan {
        match self {
            Self::Var(decl) => decl.source_span(),
            Self::Fun(decl) => decl.source_span(),
            Self::Class(decl) => decl.source_span(),
        }
    }
}
impl PrettyPrint for Decl {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        match self {
            Self::Var(decl) => decl.fmt_pretty(f),
            Self::Fun(decl) => decl.fmt_pretty(f),
            Self::Class(decl) => decl.fmt_pretty(f),
        };
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
impl PrettyPrint for DeclOrStmt {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        match self {
            Self::Decl(decl) => decl.fmt_pretty(f),
            Self::Stmt(stmt) => stmt.fmt_pretty(f),
        }
    }
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expression: Expr,
}
impl PrettyPrint for ExprStmt {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        self.expression.fmt_pretty(f);
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
impl PrettyPrint for PrintStmt {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        f.inline_sexp(|f| {
            f.keyword_item("print");
            f.item(&self.expression);
        });
    }
}
impl AstNode for PrintStmt {
    fn source_span(&self) -> SourceSpan {
        SourceSpan::range(self.print_span.start(), self.expression.source_span().end())
    }
}

#[derive(Debug)]
pub struct BlockStmt {
    pub body: Vec<DeclOrStmt>,
    pub open_span: SourceSpan,
    pub close_span: SourceSpan,
}
impl PrettyPrint for BlockStmt {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        f.block_sexp(|f| {
            f.keyword_item("do");
            for stmt in &self.body {
                f.break_line();
                f.item(stmt);
            }
        });
    }
}
impl AstNode for BlockStmt {
    fn source_span(&self) -> SourceSpan {
        SourceSpan::range(self.open_span.start(), self.close_span.end())
    }
}

#[derive(Debug)]
pub struct IfStmt {
    pub if_span: SourceSpan,
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}
impl PrettyPrint for IfStmt {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        f.block_sexp(|f| {
            f.keyword_item("if");
            f.item(&self.condition);
            f.break_line();
            f.item(self.then_branch.as_ref());
            if let Some(else_branch) = &self.else_branch {
                f.break_line();
                f.item(else_branch.as_ref());
            }
        });
    }
}
impl AstNode for IfStmt {
    fn source_span(&self) -> SourceSpan {
        SourceSpan::range(
            self.if_span.start(),
            self.else_branch
                .as_ref()
                .map(|br| br.source_span().end())
                .unwrap_or_else(|| self.then_branch.source_span().end()),
        )
    }
}

#[derive(Debug)]
pub struct WhileStmt {
    pub while_span: SourceSpan,
    pub condition: Expr,
    pub body: Box<Stmt>,
}
impl PrettyPrint for WhileStmt {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        f.inline_sexp(|f| {
            f.keyword_item("while");
            f.item(&self.condition);
            f.item(self.body.as_ref());
        });
    }
}
impl AstNode for WhileStmt {
    fn source_span(&self) -> SourceSpan {
        SourceSpan::range(self.while_span.start(), self.body.source_span().end())
    }
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub return_span: SourceSpan,
    pub expression: Option<Expr>,
}
impl PrettyPrint for ReturnStmt {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        f.inline_sexp(|f| {
            f.keyword_item("return");
            if let Some(expression) = &self.expression {
                f.item(expression);
            }
        });
    }
}
impl AstNode for ReturnStmt {
    fn source_span(&self) -> SourceSpan {
        SourceSpan::range(
            self.return_span.start(),
            self.expression
                .as_ref()
                .map(|expr| expr.source_span().end())
                .unwrap_or_else(|| self.return_span.end()),
        )
    }
}

#[derive(Debug)]
pub enum Stmt {
    Expr(ExprStmt),
    Print(PrintStmt),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
    Return(ReturnStmt),
}
impl PrettyPrint for Stmt {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        match self {
            Self::Expr(stmt) => stmt.fmt_pretty(f),
            Self::Print(stmt) => stmt.fmt_pretty(f),
            Self::Block(stmt) => stmt.fmt_pretty(f),
            Self::If(stmt) => stmt.fmt_pretty(f),
            Self::While(stmt) => stmt.fmt_pretty(f),
            Self::Return(stmt) => stmt.fmt_pretty(f),
        }
    }
}
impl AstNode for Stmt {
    fn source_span(&self) -> SourceSpan {
        match self {
            Self::Expr(stmt) => stmt.source_span(),
            Self::Print(stmt) => stmt.source_span(),
            Self::Block(stmt) => stmt.source_span(),
            Self::If(stmt) => stmt.source_span(),
            Self::While(stmt) => stmt.source_span(),
            Self::Return(stmt) => stmt.source_span(),
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
    LogicalAnd,
    LogicalOr,
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
            Self::LogicalAnd => "and",
            Self::LogicalOr => "or",
        })
    }
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub operator: WithSpan<BinaryOperator>,
}
impl PrettyPrint for BinaryExpr {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        f.inline_sexp(|f| {
            f.item(&self.operator.inner.to_string().purple().to_string());
            f.item(self.left.as_ref());
            f.item(self.right.as_ref());
        });
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
impl PrettyPrint for LiteralExpr {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        f.push_str(&format!("{:?}", self.value).yellow());
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
        f.write_str(match self {
            Self::Minus => "-",
            Self::Not => "!",
        })
    }
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub operator: WithSpan<UnaryOperator>,
    pub right: Box<Expr>,
}
impl PrettyPrint for UnaryExpr {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        f.inline_sexp(|f| {
            f.item(&self.operator.inner.to_string().purple().to_string());
            f.item(self.right.as_ref());
        });
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
impl PrettyPrint for VariableExpr {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        self.identifier.fmt_pretty(f);
    }
}
impl AstNode for VariableExpr {
    fn source_span(&self) -> SourceSpan {
        self.identifier.source_span()
    }
}

#[derive(Debug)]
pub enum AssignmentTargetExpr {
    Variable(VariableExpr),
    PropertyAccess(PropertyAccessExpr),
}
impl AstNode for AssignmentTargetExpr {
    fn source_span(&self) -> SourceSpan {
        match self {
            Self::Variable(expr) => expr.source_span(),
            Self::PropertyAccess(expr) => expr.source_span(),
        }
    }
}
impl PrettyPrint for AssignmentTargetExpr {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        match self {
            Self::Variable(expr) => expr.fmt_pretty(f),
            Self::PropertyAccess(expr) => expr.fmt_pretty(f),
        }
    }
}

#[derive(Debug)]
pub struct AssignmentExpr {
    pub target: AssignmentTargetExpr,
    pub value: Box<Expr>,
}
impl PrettyPrint for AssignmentExpr {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        f.inline_sexp(|f| {
            f.keyword_item("assign");
            f.item(&self.target);
            f.item(self.value.as_ref());
        });
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
impl PrettyPrint for GroupingExpr {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        self.expr.fmt_pretty(f);
    }
}
impl AstNode for GroupingExpr {
    fn source_span(&self) -> SourceSpan {
        self.expr.source_span()
    }
}

#[derive(Debug)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub arguments: Vec<Expr>,
    pub close_paren_span: SourceSpan,
}
impl PrettyPrint for CallExpr {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        f.inline_sexp(|f| {
            f.item(self.callee.as_ref());
            for arg in &self.arguments {
                f.item(arg);
            }
        });
    }
}
impl AstNode for CallExpr {
    fn source_span(&self) -> SourceSpan {
        SourceSpan::range(
            self.callee.source_span().start(),
            self.close_paren_span.end(),
        )
    }
}

#[derive(Debug)]
pub struct PropertyAccessExpr {
    pub object: Box<Expr>,
    pub property: Identifier,
}
impl PrettyPrint for PropertyAccessExpr {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        self.object.fmt_pretty(f);
        f.push_str(".");
        self.property.fmt_pretty(f);
    }
}
impl AstNode for PropertyAccessExpr {
    fn source_span(&self) -> SourceSpan {
        SourceSpan::range(
            self.object.source_span().start(),
            self.property.source_span().end(),
        )
    }
}

#[derive(Debug)]
pub struct ThisExpr {
    pub source_span: SourceSpan,
}
impl PrettyPrint for ThisExpr {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        f.push_str(&"this".purple());
    }
}
impl AstNode for ThisExpr {
    fn source_span(&self) -> SourceSpan {
        self.source_span
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
    Call(CallExpr),
    PropertyAccess(PropertyAccessExpr),
    This(ThisExpr),
}
impl PrettyPrint for Expr {
    fn fmt_pretty(&self, f: &mut PrettyPrinter) {
        match self {
            Self::Binary(expr) => expr.fmt_pretty(f),
            Self::Unary(expr) => expr.fmt_pretty(f),
            Self::Literal(expr) => expr.fmt_pretty(f),
            Self::Variable(expr) => expr.fmt_pretty(f),
            Self::Assignment(expr) => expr.fmt_pretty(f),
            Self::Grouping(expr) => expr.fmt_pretty(f),
            Self::Call(expr) => expr.fmt_pretty(f),
            Self::PropertyAccess(expr) => expr.fmt_pretty(f),
            Self::This(expr) => expr.fmt_pretty(f),
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
            Self::Call(expr) => expr.source_span(),
            Self::PropertyAccess(expr) => expr.source_span(),
            Self::This(expr) => expr.source_span(),
        }
    }
}

pub struct PrettyPrinter {
    buf: String,
    indent_size: usize,
    indent: usize,
}
impl PrettyPrinter {
    fn push_str(&mut self, s: &str) {
        for (idx, line) in s.split('\n').enumerate() {
            if idx > 0 {
                write!(self.buf, "\n{}", " ".repeat(self.indent * self.indent_size)).unwrap();
            }
            self.buf.push_str(line);
        }
    }
    fn closing_paren(&mut self) {
        let last = unsafe { self.buf.as_bytes_mut().last_mut() };
        match last {
            Some(lc) if *lc == b' ' => {
                *lc = b')';
            }
            _ => {
                self.buf.push(')');
            }
        }
    }
    fn inline_sexp<F: Fn(&mut Self)>(&mut self, fmt: F) {
        self.push_str("(");
        fmt(self);
        self.closing_paren();
    }
    fn block_sexp<F: Fn(&mut Self)>(&mut self, fmt: F) {
        self.push_str("(");
        self.indent += 1;
        fmt(self);
        self.closing_paren();
        self.indent -= 1;
    }
    fn break_line(&mut self) {
        self.push_str("\n");
    }
    fn item<T: PrettyPrint>(&mut self, item: &T) {
        item.fmt_pretty(self);
        self.buf.push(' ');
    }
    fn keyword_item(&mut self, s: &str) {
        self.item(&s.blue().to_string());
    }
}
pub trait PrettyPrint {
    fn fmt_pretty(&self, printer: &mut PrettyPrinter);
    fn pretty_print(&self) -> String {
        let mut printer = PrettyPrinter {
            buf: String::new(),
            indent: 0,
            indent_size: 4,
        };
        self.fmt_pretty(&mut printer);
        printer.buf
    }
}
impl PrettyPrint for &str {
    fn fmt_pretty(&self, printer: &mut PrettyPrinter) {
        printer.push_str(self);
    }
}
impl PrettyPrint for String {
    fn fmt_pretty(&self, printer: &mut PrettyPrinter) {
        printer.push_str(self);
    }
}
