use miette::{Diagnostic, Result};
use std::iter::Peekable;
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
        fount_at: SourceSpan,
    },
}

pub struct Parser<Stream: Iterator<Item = Token>> {
    token_stream: Peekable<Stream>,
    current_token: Option<Token>,
    at_end: bool,
    recovered_errors: Vec<ParserError>,
}

impl<Stream: Iterator<Item = Token>> Parser<Stream> {
    pub fn parse(token_stream: Stream) -> (Program, Vec<ParserError>) {
        let mut parser = Self::new(token_stream);
        let program = parser.parse_program();
        (program, parser.recovered_errors)
    }
    fn new(token_stream: Stream) -> Self {
        Parser {
            token_stream: token_stream.peekable(),
            current_token: None,
            at_end: false,
            recovered_errors: Vec::new(),
        }
    }
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
        let var_span = {
            let var_token = self.current_token.as_ref().unwrap();
            assert_eq!(var_token.token_type, TokenType::Var);
            var_token.span
        };
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
        if let Some(print_span) = self.consume_match(|token| match token.token_type {
            TokenType::Print => Some(token.span),
            _ => None,
        }) {
            let expression = self.parse_expr()?;
            self.consume_token_or_error(TokenType::Semicolon, |token| {
                ParserError::ExpectedSemicolor {
                    actual: (&token.token_type).into(),
                    found_at: token.span,
                }
            })?;
            return Ok(Stmt::Print(PrintStmt {
                expression,
                print_span,
            }));
        }

        let expression = self.parse_expr()?;
        self.consume_statement_end_semicolon()?;
        Ok(Stmt::Expr(ExprStmt { expression }))
    }
    fn consume_statement_end_semicolon(&mut self) -> Result<(), ParserError> {
        self.consume_token_or_error(TokenType::Semicolon, |token| {
            ParserError::ExpectedSemicolor {
                actual: (&token.token_type).into(),
                found_at: token.span,
            }
        })
    }
    fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        self.parse_equality_expr()
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
                right: Box::new(self.parse_primary_expr()?),
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
                    TokenType::String(string) => Value::String(string.clone()),
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

        if let Some(opening_span) = self.consume_match(|token| match token.token_type {
            TokenType::OpenParen => Some(token.span),
            _ => None,
        }) {
            let expr = self.parse_expr()?;
            self.consume_token_or_error(TokenType::CloseParen, |tok| {
                ParserError::UnmatchedParenthesis {
                    found_token_type: (&tok.token_type).into(),
                    found_at: tok.span,
                    opener: opening_span,
                }
            })?;
            return Ok(expr);
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
                    fount_at: next.span,
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
    fn consume_token_or_error<F: Fn(&Token) -> ParserError>(
        &mut self,
        token_type: TokenType,
        make_err: F,
    ) -> Result<(), ParserError> {
        match self.token_stream.peek() {
            Some(token) if token.token_type == token_type => {
                self.advance();
                Ok(())
            }
            Some(other_token) => Err(make_err(other_token)),
            None => Err(make_err(self.current_token.as_ref().unwrap())),
        }
    }
}
