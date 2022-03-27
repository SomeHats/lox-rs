use miette::{Diagnostic, Result};
use std::{iter::Peekable, rc::Rc};
use thiserror::Error;

use crate::{
    ast::*,
    scanner::{Token, TokenType, TokenTypeName},
    source::SourceSpan,
    value::Value,
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
    },
    #[error("Expected a semi colon at the end of this statement")]
    ExpectedSemicolor {
        actual: TokenTypeName,
        #[label("Found {actual:?} instead of a semicolon (;)")]
        found_at: SourceSpan,
    },
    #[error("Unexpected token in expression")]
    UnexpectedExpressionToken {
        actual: TokenTypeName,
        #[label("Found {actual:?} instead of a number, variable, unary, etc.")]
        found_at: SourceSpan,
    },
    #[error("Unexpected token")]
    UnexpectedToken {
        actual: TokenTypeName,
        expected: TokenTypeName,
        #[label("Found {actual:?} instead of {expected:?}")]
        found_at: SourceSpan,
    },
    #[error("Invalid assignment target")]
    InvalidAssignmentTarget {
        #[label("Cannot assign to this expression")]
        found_at: SourceSpan,
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

pub struct Parser<Stream: Iterator<Item = Token>> {
    opts: ParserOpts,
    token_stream: Peekable<Stream>,
    current_token: Option<Token>,
    at_end: bool,
    recovered_errors: Vec<ParserError>,
}

impl<Stream: Iterator<Item = Token>> Parser<Stream> {
    pub fn parse(token_stream: Stream, opts: ParserOpts) -> (Program, Vec<ParserError>) {
        let mut parser = Self::new(token_stream, opts);
        let program = parser.parse_program();
        (program, parser.recovered_errors)
    }
    fn new(token_stream: Stream, opts: ParserOpts) -> Parser<impl Iterator<Item = Token>> {
        Parser {
            opts,
            token_stream: token_stream
                .filter(|token| token.token_type != TokenType::LineComment)
                .peekable(),
            current_token: None,
            at_end: false,
            recovered_errors: Vec::new(),
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
        Program { statements }
    }
    fn parse_decl_or_stmt(&mut self) -> Result<DeclOrStmt, ParserError> {
        if self.consume_token(TokenType::Var).is_some() {
            return Ok(DeclOrStmt::Decl(Decl::Var(self.parse_var_decl()?)));
        }
        Ok(DeclOrStmt::Stmt(self.parse_stmt()?))
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
    fn parse_stmt(&mut self) -> Result<Stmt, ParserError> {
        if self.consume_token(TokenType::If).is_some() {
            return Ok(Stmt::If(self.parse_if_stmt()?));
        }
        if self.consume_token(TokenType::While).is_some() {
            return Ok(Stmt::While(self.parse_while_stmt()?));
        }
        if let Some(print_span) = self.consume_token_to_span(TokenType::Print) {
            let expression = self.parse_expr()?;
            self.consume_statement_end_semicolon()?;
            return Ok(Stmt::Print(PrintStmt {
                expression,
                print_span,
            }));
        }

        if let Some(open_span) = self.consume_token_to_span(TokenType::OpenBrace) {
            let mut statements = Vec::new();
            loop {
                if let Some(close_span) = self.consume_token_to_span(TokenType::CloseBrace) {
                    return Ok(Stmt::Block(BlockStmt {
                        statements,
                        open_span,
                        close_span,
                    }));
                } else {
                    statements.push(self.parse_decl_or_stmt()?);
                }
            }
        }

        let expression = self.parse_expr()?;
        self.consume_statement_end_semicolon()?;
        Ok(Stmt::Expr(ExprStmt { expression }))
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
    fn consume_statement_end_semicolon(&mut self) -> Result<(), ParserError> {
        let result = self.consume_token_or_error(&TokenType::Semicolon, |token| {
            ParserError::ExpectedSemicolor {
                actual: (&token.token_type).into(),
                found_at: token.span,
            }
        });
        if self.opts.is_repl {
            result.or_else(|_| {
                self.consume_token_or_error(&TokenType::Eof, |token| {
                    ParserError::ExpectedSemicolor {
                        actual: (&token.token_type).into(),
                        found_at: token.span,
                    }
                })
            })
        } else {
            result
        }
    }
    fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        self.parse_assignment_expr()
    }
    fn parse_assignment_expr(&mut self) -> Result<Expr, ParserError> {
        fn expr_to_assignment_target(expr: Expr) -> Result<Identifier, ParserError> {
            match expr {
                Expr::Variable(var) => Ok(var.identifier),
                other => Err(ParserError::InvalidAssignmentTarget {
                    found_at: other.source_span(),
                }),
            }
        }

        let expr = self.parse_or_expr()?;
        if self.consume_token(TokenType::Equal).is_some() {
            let value = self.parse_expr()?;
            let target = expr_to_assignment_target(expr)?;
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
            self.parse_primary_expr()
        }
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

        if let Some(identifier) = self.consume_match(|token| match &token.token_type {
            TokenType::Identifier(name) => Some(Identifier {
                name: name.clone(),
                source_span: token.span,
            }),
            _ => None,
        }) {
            return Ok(Expr::Variable(VariableExpr { identifier }));
        }

        if let Some(opening_span) = self.consume_token_to_span(TokenType::OpenParen) {
            let expr = self.parse_expr()?;
            self.consume_token_or_error(&TokenType::CloseParen, |tok| {
                ParserError::UnmatchedParenthesis {
                    found_token_type: (&tok.token_type).into(),
                    found_at: tok.span,
                    opener: opening_span,
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
        })
    }
    fn parse_identifier(&mut self) -> Result<Identifier, ParserError> {
        let identifier = {
            let next = self.peek_or_eof();
            match &next.token_type {
                TokenType::Identifier(name) => Ok(Identifier {
                    name: name.to_string(),
                    source_span: next.span,
                }),
                other => Err(ParserError::UnexpectedToken {
                    actual: other.into(),
                    expected: TokenTypeName::Identifier,
                    found_at: next.span,
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
    fn consume_token_or_error<F: Fn(&Token) -> ParserError>(
        &mut self,
        token_type: &TokenType,
        make_err: F,
    ) -> Result<(), ParserError> {
        match self.token_stream.peek() {
            Some(token) if token.token_type == *token_type => {
                self.advance();
                Ok(())
            }
            Some(other_token) => Err(make_err(other_token)),
            None => Err(make_err(self.current_token.as_ref().unwrap())),
        }
    }
    fn consume_token_or_default_error(
        &mut self,
        token_type: &TokenType,
    ) -> Result<(), ParserError> {
        self.consume_token_or_error(token_type, |actual| ParserError::UnexpectedToken {
            actual: (&actual.token_type).into(),
            expected: token_type.into(),
            found_at: actual.span,
        })
    }
    fn extract_current_token_span(&self, token_type: TokenType) -> SourceSpan {
        let token = self.current_token.as_ref().unwrap();
        assert_eq!(token.token_type, token_type);
        token.span
    }
}
