use miette::{Diagnostic, Result};
use std::{fmt::Display, iter::Peekable, rc::Rc};
use thiserror::Error;

const MAX_FUN_ARGS: usize = 254;

use crate::{
    ast::*,
    scanner::{Token, TokenType, TokenTypeName},
    side_table::UniqueId,
    source::SourceSpan,
    value::Value,
    SourceOffset, SourceReference,
};

#[derive(Error, Diagnostic, Debug)]
pub enum ParserError {
    #[error("Expected closing parenthesis")]
    UnmatchedParenthesis {
        #[label("Opening parenthesis here")]
        opener: SourceSpan,
        found_token_type: TokenTypeName,
        #[label("Found {found_token_type:?} instead")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Expected a semi colon at the end of this statement")]
    ExpectedSemicolor {
        actual: TokenTypeName,
        #[label("Found {actual:?} instead of a semicolon (;)")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Unexpected token in expression")]
    UnexpectedExpressionToken {
        actual: TokenTypeName,
        #[label("Found {actual:?} instead of a number, variable, unary, etc.")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Unexpected token")]
    UnexpectedToken {
        actual: TokenTypeName,
        expected: TokenTypeName,
        #[label("Found {actual:?} instead of {expected:?}")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Invalid assignment target")]
    InvalidAssignmentTarget {
        #[label("Cannot assign to this expression")]
        found_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Can't have more than {} arguments to a function", MAX_FUN_ARGS)]
    TooManyCallArgs {
        #[label("The function call is here")]
        callee_at: SourceSpan,
        #[label("The {}th argument is here", MAX_FUN_ARGS + 1)]
        too_many_args_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
    },
    #[error("Can't have more than {} parameters to a {fun_type}", MAX_FUN_ARGS)]
    TooManyFunParams {
        #[label("The {fun_type} is defined here")]
        decl_at: SourceSpan,
        #[label("The {}th paramter is here", MAX_FUN_ARGS + 1)]
        too_many_params_at: SourceSpan,
        #[source_code]
        source_code: SourceReference,
        fun_type: FunType,
    },
}

#[derive(Default)]
pub struct ParserOpts {
    is_repl: bool,
}
impl ParserOpts {
    pub fn for_repl(self) -> Self {
        Self { is_repl: true }
    }
}

#[derive(Debug)]
pub enum FunType {
    FunDecl,
    Method,
}
impl Display for FunType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            FunType::FunDecl => "function",
            FunType::Method => "method",
        })
    }
}

pub struct Parser<Stream: Iterator<Item = Token>> {
    opts: ParserOpts,
    token_stream: Peekable<Stream>,
    current_token: Option<Token>,
    at_end: bool,
    recovered_errors: Vec<ParserError>,
    source_reference: SourceReference,
}

impl<Stream: Iterator<Item = Token>> Parser<Stream> {
    pub fn parse(
        token_stream: Stream,
        source_reference: SourceReference,
        opts: ParserOpts,
    ) -> (Program, Vec<ParserError>) {
        let mut parser = Self::new(token_stream, source_reference, opts);
        let program = parser.parse_program();
        (program, parser.recovered_errors)
    }
    fn new(
        token_stream: Stream,
        source_reference: SourceReference,
        opts: ParserOpts,
    ) -> Parser<impl Iterator<Item = Token>> {
        Parser {
            opts,
            token_stream: token_stream
                .filter(|token| token.token_type != TokenType::LineComment)
                .peekable(),
            current_token: None,
            at_end: false,
            recovered_errors: Vec::new(),
            source_reference,
        }
    }
}
impl<Stream: Iterator<Item = Token>> Parser<Stream> {
    fn parse_program(&mut self) -> Program {
        let mut statements = Vec::new();
        while !matches!(
            self.token_stream.peek(),
            None | Some(Token {
                token_type: TokenType::Eof,
                ..
            })
        ) {
            match self.parse_decl_or_stmt() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    self.synchronize();
                    self.recovered_errors.push(err);
                }
            }
        }
        Program {
            statements,
            source_reference: self.source_reference.clone(),
        }
    }
    fn parse_decl_or_stmt(&mut self) -> Result<DeclOrStmt, ParserError> {
        if self.consume_token(TokenType::Class).is_some() {
            return Ok(DeclOrStmt::Decl(Decl::Class(self.parse_class_decl()?)));
        }
        if self.consume_token(TokenType::Var).is_some() {
            return Ok(DeclOrStmt::Decl(Decl::Var(self.parse_var_decl()?)));
        }
        if self.consume_token(TokenType::Fun).is_some() {
            return Ok(DeclOrStmt::Decl(Decl::Fun(self.parse_fun_decl()?)));
        }
        Ok(DeclOrStmt::Stmt(self.parse_stmt()?))
    }
    fn parse_class_decl(&mut self) -> Result<ClassDecl, ParserError> {
        let class_span = self.extract_current_token_span(TokenType::Class);
        let name = self.parse_identifier()?;
        self.consume_token_or_default_error(&TokenType::OpenBrace)?;
        let mut methods = Vec::new();
        loop {
            match self.consume_token(TokenType::CloseBrace) {
                Some(tok) => {
                    break Ok(ClassDecl {
                        source_span: SourceSpan::range(class_span.start(), tok.span.end()),
                        name,
                        methods,
                    })
                }
                None => methods.push(Rc::new(self.parse_fun(FunType::Method, None)?)),
            }
        }
    }
    fn parse_var_decl(&mut self) -> Result<VarDecl, ParserError> {
        let var_span = self.extract_current_token_span(TokenType::Var);
        let identifier = self.parse_identifier()?;

        let initializer = match self.consume_token(TokenType::Equal) {
            Some(_) => Some(self.parse_expr()?),
            _ => None,
        };

        self.consume_statement_end_semicolon()?;

        Ok(VarDecl {
            var_span,
            identifier,
            initializer,
        })
    }
    fn parse_fun_decl(&mut self) -> Result<FunDecl, ParserError> {
        let fun_span = self.extract_current_token_span(TokenType::Fun);
        let fun = self.parse_fun(FunType::FunDecl, Some(fun_span.start()))?;
        Ok(FunDecl {
            source_span: SourceSpan::range(fun_span.start(), fun.source_span().end()),
            fun: Rc::new(fun),
        })
    }
    fn parse_fun(
        &mut self,
        fun_type: FunType,
        start_offset: Option<SourceOffset>,
    ) -> Result<Fun, ParserError> {
        let name = self.parse_identifier()?;
        self.consume_token_or_default_error(&TokenType::OpenParen)?;
        let parameters = match self.consume_token(TokenType::CloseParen) {
            Some(_) => Vec::new(),
            None => {
                let mut parameters = Vec::new();
                loop {
                    let parameter = self.parse_identifier()?;
                    parameters.push(parameter);
                    if self.consume_token(TokenType::Comma).is_none() {
                        break;
                    }
                }
                self.consume_token_or_default_error(&TokenType::CloseParen)?;
                parameters
            }
        };

        if let Some(param) = parameters.get(MAX_FUN_ARGS + 1) {
            self.recovered_errors.push(ParserError::TooManyFunParams {
                decl_at: SourceSpan::range(
                    start_offset.unwrap_or_else(|| name.source_span().start()),
                    name.source_span().end(),
                ),
                too_many_params_at: param.source_span(),
                source_code: self.source_reference.clone(),
                fun_type,
            });
        }

        self.consume_token_or_default_error(&TokenType::OpenBrace)?;
        let body_block = self.parse_block_stmt()?;

        Ok(Fun {
            source_span: SourceSpan::range(
                start_offset.unwrap_or_else(|| name.source_span().start()),
                body_block.source_span().end(),
            ),
            name,
            parameters,
            body: body_block.body,
        })
    }
    fn parse_stmt(&mut self) -> Result<Stmt, ParserError> {
        if self.consume_token(TokenType::If).is_some() {
            return Ok(Stmt::If(self.parse_if_stmt()?));
        }
        if self.consume_token(TokenType::While).is_some() {
            return Ok(Stmt::While(self.parse_while_stmt()?));
        }
        if self.consume_token(TokenType::For).is_some() {
            return self.parse_for_stmt();
        }
        if self.consume_token(TokenType::Print).is_some() {
            return Ok(Stmt::Print(self.parse_print_stmt()?));
        }
        if self.consume_token(TokenType::Return).is_some() {
            return Ok(Stmt::Return(self.parse_return_stmt()?));
        }
        if self.consume_token(TokenType::OpenBrace).is_some() {
            return Ok(Stmt::Block(self.parse_block_stmt()?));
        }

        Ok(Stmt::Expr(self.parse_expr_stmt()?))
    }

    fn parse_expr_stmt(&mut self) -> Result<ExprStmt, ParserError> {
        let expression = self.parse_expr()?;
        self.consume_statement_end_semicolon()?;
        Ok(ExprStmt { expression })
    }
    fn parse_if_stmt(&mut self) -> Result<IfStmt, ParserError> {
        let if_span = self.extract_current_token_span(TokenType::If);

        self.consume_token_or_default_error(&TokenType::OpenParen)?;
        let condition = self.parse_expr()?;
        self.consume_token_or_default_error(&TokenType::CloseParen)?;

        let then_branch = Box::new(self.parse_stmt()?);
        let else_branch = if self.consume_token(TokenType::Else).is_some() {
            Some(Box::new(self.parse_stmt()?))
        } else {
            None
        };

        Ok(IfStmt {
            if_span,
            condition,
            then_branch,
            else_branch,
        })
    }
    fn parse_print_stmt(&mut self) -> Result<PrintStmt, ParserError> {
        let print_span = self.extract_current_token_span(TokenType::Print);
        let expression = self.parse_expr()?;
        self.consume_statement_end_semicolon()?;
        Ok(PrintStmt {
            expression,
            print_span,
        })
    }
    fn parse_return_stmt(&mut self) -> Result<ReturnStmt, ParserError> {
        let return_span = self.extract_current_token_span(TokenType::Return);
        let expression = match self.consume_token(TokenType::Semicolon) {
            Some(_) => None,
            None => {
                let expression = self.parse_expr()?;
                self.consume_statement_end_semicolon()?;
                Some(expression)
            }
        };
        Ok(ReturnStmt {
            expression,
            return_span,
        })
    }
    fn parse_while_stmt(&mut self) -> Result<WhileStmt, ParserError> {
        let while_span = self.extract_current_token_span(TokenType::While);
        self.consume_token_or_default_error(&TokenType::OpenParen)?;
        let condition = self.parse_expr()?;
        self.consume_token_or_default_error(&TokenType::CloseParen)?;
        let body = Box::new(self.parse_stmt()?);

        Ok(WhileStmt {
            while_span,
            condition,
            body,
        })
    }
    fn parse_for_stmt(&mut self) -> Result<Stmt, ParserError> {
        let for_span = self.extract_current_token_span(TokenType::For);
        self.consume_token_or_default_error(&TokenType::OpenParen)?;

        let initializer = if self.consume_token(TokenType::Semicolon).is_some() {
            None
        } else if self.consume_token(TokenType::Var).is_some() {
            Some(DeclOrStmt::Decl(Decl::Var(self.parse_var_decl()?)))
        } else {
            Some(DeclOrStmt::Stmt(Stmt::Expr(self.parse_expr_stmt()?)))
        };

        let condition = match self.consume_token(TokenType::Semicolon) {
            Some(_) => None,
            None => {
                let expr = self.parse_expr()?;
                self.consume_token_or_default_error(&TokenType::Semicolon)?;
                Some(expr)
            }
        };

        let increment = match self.consume_token(TokenType::CloseParen) {
            Some(_) => None,
            None => {
                let expr = self.parse_expr()?;
                self.consume_token_or_default_error(&TokenType::CloseParen)?;
                Some(expr)
            }
        };

        let body = self.parse_stmt()?;

        let mut body = BlockStmt {
            open_span: body.source_span().start().into(),
            close_span: body.source_span().end().into(),
            body: vec![DeclOrStmt::Stmt(body)],
        };
        if let Some(expression) = increment {
            body.body
                .push(DeclOrStmt::Stmt(Stmt::Expr(ExprStmt { expression })));
        }

        let body = Stmt::While(WhileStmt {
            while_span: for_span,
            condition: condition.unwrap_or(Expr::Literal(LiteralExpr {
                value: Value::Boolean(true),
                source_span: for_span,
            })),
            body: Box::new(Stmt::Block(body)),
        });

        Ok(if let Some(initializer) = initializer {
            Stmt::Block(BlockStmt {
                open_span: initializer.source_span(),
                close_span: body.source_span().end().into(),
                body: vec![initializer, DeclOrStmt::Stmt(body)],
            })
        } else {
            body
        })
    }
    fn parse_block_stmt(&mut self) -> Result<BlockStmt, ParserError> {
        let open_span = self.extract_current_token_span(TokenType::OpenBrace);
        let mut statements = Vec::new();
        loop {
            if let Some(close_span) = self.consume_token_to_span(TokenType::CloseBrace) {
                return Ok(BlockStmt {
                    body: statements,
                    open_span,
                    close_span,
                });
            } else {
                statements.push(self.parse_decl_or_stmt()?);
            }
        }
    }
    fn consume_statement_end_semicolon(&mut self) -> Result<(), ParserError> {
        let is_repl = self.opts.is_repl;
        let result = self.consume_token_or_error(&TokenType::Semicolon, |token, source_code| {
            ParserError::ExpectedSemicolor {
                actual: (&token.token_type).into(),
                found_at: token.span,
                source_code,
            }
        });
        if is_repl && result.is_err() {
            self.consume_token_or_error(&TokenType::Eof, |token, source_code| {
                ParserError::ExpectedSemicolor {
                    actual: (&token.token_type).into(),
                    found_at: token.span,
                    source_code,
                }
            })?;
            Ok(())
        } else {
            result?;
            Ok(())
        }
    }
    fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        self.parse_assignment_expr()
    }
    fn parse_assignment_expr(&mut self) -> Result<Expr, ParserError> {
        fn expr_to_assignment_target(
            expr: Expr,
            source_reference: &SourceReference,
        ) -> Result<AssignmentTargetExpr, ParserError> {
            match expr {
                Expr::Variable(expr) => Ok(AssignmentTargetExpr::Variable(expr)),
                Expr::PropertyAccess(expr) => Ok(AssignmentTargetExpr::PropertyAccess(expr)),
                other => Err(ParserError::InvalidAssignmentTarget {
                    found_at: other.source_span(),
                    source_code: source_reference.clone(),
                }),
            }
        }

        let expr = self.parse_or_expr()?;
        if self.consume_token(TokenType::Equal).is_some() {
            let value = self.parse_expr()?;
            let target = expr_to_assignment_target(expr, &self.source_reference)?;
            Ok(Expr::Assignment(AssignmentExpr {
                target,
                value: Box::new(value),
            }))
        } else {
            Ok(expr)
        }
    }
    fn parse_or_expr(&mut self) -> Result<Expr, ParserError> {
        let mut last_expr = self.parse_and_expr()?;

        while let Some(or_span) = self.consume_token_to_span(TokenType::Or) {
            last_expr = Expr::Binary(BinaryExpr {
                left: Box::new(last_expr),
                right: Box::new(self.parse_and_expr()?),
                operator: WithSpan::new(BinaryOperator::LogicalOr, or_span),
            });
        }

        Ok(last_expr)
    }
    fn parse_and_expr(&mut self) -> Result<Expr, ParserError> {
        let mut last_expr = self.parse_equality_expr()?;

        while let Some(or_span) = self.consume_token_to_span(TokenType::And) {
            last_expr = Expr::Binary(BinaryExpr {
                left: Box::new(last_expr),
                right: Box::new(self.parse_equality_expr()?),
                operator: WithSpan::new(BinaryOperator::LogicalAnd, or_span),
            });
        }

        Ok(last_expr)
    }
    fn parse_equality_expr(&mut self) -> Result<Expr, ParserError> {
        let mut last_expr = self.parse_comparison_expr()?;

        while let Some(operator) = self.consume_match(|token| match token.token_type {
            TokenType::BangEqual => Some(WithSpan::new(BinaryOperator::NotEqualTo, token.span)),
            TokenType::EqualEqual => Some(WithSpan::new(BinaryOperator::EqualTo, token.span)),
            _ => None,
        }) {
            last_expr = Expr::Binary(BinaryExpr {
                left: Box::new(last_expr),
                right: Box::new(self.parse_comparison_expr()?),
                operator,
            });
        }

        Ok(last_expr)
    }
    fn parse_comparison_expr(&mut self) -> Result<Expr, ParserError> {
        let mut last_expr = self.parse_term_expr()?;

        while let Some(operator) = self.consume_match(|token| match token.token_type {
            TokenType::Less => Some(WithSpan::new(BinaryOperator::LessThan, token.span)),
            TokenType::LessEqual => {
                Some(WithSpan::new(BinaryOperator::LessThanOrEqualTo, token.span))
            }
            TokenType::Greater => Some(WithSpan::new(BinaryOperator::GreaterThan, token.span)),
            TokenType::GreaterEqual => Some(WithSpan::new(
                BinaryOperator::GreaterThanOrEqualTo,
                token.span,
            )),
            _ => None,
        }) {
            last_expr = Expr::Binary(BinaryExpr {
                left: Box::new(last_expr),
                right: Box::new(self.parse_term_expr()?),
                operator,
            });
        }

        Ok(last_expr)
    }
    fn parse_term_expr(&mut self) -> Result<Expr, ParserError> {
        let mut last_expr = self.parse_factor_expr()?;

        while let Some(operator) = self.consume_match(|token| match token.token_type {
            TokenType::Minus => Some(WithSpan::new(BinaryOperator::Minus, token.span)),
            TokenType::Plus => Some(WithSpan::new(BinaryOperator::Plus, token.span)),
            _ => None,
        }) {
            last_expr = Expr::Binary(BinaryExpr {
                left: Box::new(last_expr),
                right: Box::new(self.parse_factor_expr()?),
                operator,
            });
        }

        Ok(last_expr)
    }
    fn parse_factor_expr(&mut self) -> Result<Expr, ParserError> {
        let mut last_expr = self.parse_unary_expr()?;

        while let Some(operator) = self.consume_match(|token| match token.token_type {
            TokenType::Star => Some(WithSpan::new(BinaryOperator::Multiply, token.span)),
            TokenType::Slash => Some(WithSpan::new(BinaryOperator::Divide, token.span)),
            _ => None,
        }) {
            last_expr = Expr::Binary(BinaryExpr {
                left: Box::new(last_expr),
                right: Box::new(self.parse_unary_expr()?),
                operator,
            });
        }

        Ok(last_expr)
    }
    fn parse_unary_expr(&mut self) -> Result<Expr, ParserError> {
        if let Some(operator) = self.consume_match(|token| match token.token_type {
            TokenType::Minus => Some(WithSpan::new(UnaryOperator::Minus, token.span)),
            TokenType::Bang => Some(WithSpan::new(UnaryOperator::Not, token.span)),
            _ => None,
        }) {
            Ok(Expr::Unary(UnaryExpr {
                operator,
                right: Box::new(self.parse_unary_expr()?),
            }))
        } else {
            self.parse_call_expr()
        }
    }
    fn parse_call_expr(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_primary_expr()?;
        loop {
            expr = if self.consume_token(TokenType::OpenParen).is_some() {
                let mut arguments = Vec::new();
                let close_paren_span = match self.consume_token_to_span(TokenType::CloseParen) {
                    Some(span) => span,
                    None => {
                        loop {
                            let argument = self.parse_expr()?;
                            arguments.push(argument);
                            if self.consume_token(TokenType::Comma).is_none() {
                                break;
                            }
                        }
                        self.consume_token_or_default_error(&TokenType::CloseParen)?
                            .span
                    }
                };
                if let Some(argument) = arguments.get(MAX_FUN_ARGS + 1) {
                    self.recovered_errors.push(ParserError::TooManyCallArgs {
                        callee_at: expr.source_span(),
                        too_many_args_at: argument.source_span(),
                        source_code: self.source_reference.clone(),
                    })
                }
                Expr::Call(CallExpr {
                    callee: Box::new(expr),
                    arguments,
                    close_paren_span,
                })
            } else if self.consume_token(TokenType::Dot).is_some() {
                let property = self.parse_identifier()?;
                Expr::PropertyAccess(PropertyAccessExpr {
                    object: Box::new(expr),
                    property,
                })
            } else {
                break;
            }
        }
        Ok(expr)
    }
    fn parse_primary_expr(&mut self) -> Result<Expr, ParserError> {
        let literal = self.consume_match(|token| {
            Some(LiteralExpr {
                source_span: token.span,
                value: match &token.token_type {
                    TokenType::False => Value::Boolean(false),
                    TokenType::True => Value::Boolean(true),
                    TokenType::Nil => Value::Nil,
                    TokenType::Number(number) => Value::Number(*number),
                    TokenType::String(string) => Value::String(Rc::new(string.clone())),
                    _ => return None,
                },
            })
        });

        if let Some(literal) = literal {
            return Ok(Expr::Literal(literal));
        }

        if let Some(this) = self.consume_token(TokenType::This) {
            return Ok(Expr::This(ThisExpr {
                source_span: this.span,
            }));
        }

        if let Some(identifier) = self.consume_match(|token| match &token.token_type {
            TokenType::Identifier(name) => Some(Identifier {
                id: UniqueId::new(),
                name: name.clone(),
                source_span: token.span,
            }),
            _ => None,
        }) {
            return Ok(Expr::Variable(VariableExpr { identifier }));
        }

        if let Some(opening_span) = self.consume_token_to_span(TokenType::OpenParen) {
            let expr = self.parse_expr()?;
            self.consume_token_or_error(&TokenType::CloseParen, |tok, source_code| {
                ParserError::UnmatchedParenthesis {
                    found_token_type: (&tok.token_type).into(),
                    found_at: tok.span,
                    opener: opening_span,
                    source_code,
                }
            })?;
            return Ok(Expr::Grouping(GroupingExpr {
                expr: Box::new(expr),
            }));
        }

        let unknown_tok = self
            .token_stream
            .peek()
            .or(self.current_token.as_ref())
            .unwrap();
        Err(ParserError::UnexpectedExpressionToken {
            actual: (&unknown_tok.token_type).into(),
            found_at: unknown_tok.span,
            source_code: self.source_reference.clone(),
        })
    }
    fn parse_identifier(&mut self) -> Result<Identifier, ParserError> {
        let identifier = {
            let next = self.peek_or_eof();
            match &next.token_type {
                TokenType::Identifier(name) => Ok(Identifier {
                    id: UniqueId::new(),
                    name: name.to_string(),
                    source_span: next.span,
                }),
                other => Err(ParserError::UnexpectedToken {
                    actual: other.into(),
                    expected: TokenTypeName::Identifier,
                    found_at: next.span,
                    source_code: self.source_reference.clone(),
                }),
            }
        };
        if identifier.is_ok() {
            self.advance();
        }
        identifier
    }
    fn synchronize(&mut self) {
        while self.advance() {
            if self
                .current_token
                .as_ref()
                .map_or(false, |t| t.token_type == TokenType::Semicolon)
            {
                break;
            }
            if let Some(Token {
                token_type:
                    TokenType::For
                    | TokenType::Class
                    | TokenType::Fun
                    | TokenType::Var
                    | TokenType::If
                    | TokenType::While
                    | TokenType::Print
                    | TokenType::Return,
                ..
            }) = self.token_stream.peek()
            {
                break;
            }
        }
    }
    fn advance(&mut self) -> bool {
        match self.token_stream.next() {
            Some(token) => {
                self.current_token = Some(token);
                true
            }
            None => {
                self.at_end = true;
                false
            }
        }
    }
    fn peek_or_eof(&mut self) -> &Token {
        self.token_stream
            .peek()
            .or(self.current_token.as_ref())
            .unwrap()
    }
    fn consume_match<T: Sized, F: Fn(&Token) -> Option<T>>(&mut self, check: F) -> Option<T> {
        match self.token_stream.peek() {
            None => None,
            Some(token) => match check(token) {
                Some(value) => {
                    self.advance();
                    Some(value)
                }
                None => None,
            },
        }
    }
    fn consume_token(&mut self, token_type: TokenType) -> Option<&Token> {
        match self.token_stream.peek() {
            Some(token) if token.token_type == token_type => {
                self.advance();
                self.current_token.as_ref()
            }
            Some(_) | None => None,
        }
    }
    fn consume_token_to_span(&mut self, token_type: TokenType) -> Option<SourceSpan> {
        self.consume_token(token_type).map(|token| token.span)
    }
    fn consume_token_or_error<F: Fn(&Token, SourceReference) -> ParserError>(
        &mut self,
        token_type: &TokenType,
        make_err: F,
    ) -> Result<&Token, ParserError> {
        match self.token_stream.peek() {
            Some(token) if token.token_type == *token_type => {
                self.advance();
                Ok(self.current_token.as_ref().unwrap())
            }
            Some(other_token) => Err(make_err(other_token, self.source_reference.clone())),
            None => Err(make_err(
                self.current_token.as_ref().unwrap(),
                self.source_reference.clone(),
            )),
        }
    }
    fn consume_token_or_default_error(
        &mut self,
        token_type: &TokenType,
    ) -> Result<&Token, ParserError> {
        self.consume_token_or_error(token_type, |actual, source_code| {
            ParserError::UnexpectedToken {
                actual: (&actual.token_type).into(),
                expected: token_type.into(),
                found_at: actual.span,
                source_code,
            }
        })
    }
    fn extract_current_token_span(&self, token_type: TokenType) -> SourceSpan {
        let token = self.current_token.as_ref().unwrap();
        assert_eq!(token.token_type, token_type);
        token.span
    }
}
