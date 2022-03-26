use crate::{
    fixed_que::FixedQueue,
    source::{SourceOffset, SourceSpan},
};
use derive_new::new;
use miette::{Diagnostic, Result};
use std::{collections::VecDeque, str::CharIndices};
use strum::EnumDiscriminants;
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum ScannerError {
    #[error("Unexpected character: {character:?}")]
    UnexpectedCharacter {
        character: char,
        #[label("Character found here")]
        at: SourceOffset,
    },
    #[error("Unterminated string")]
    UnterminatedString {
        #[label("String")]
        at: SourceSpan,
    },
    #[error("Unknown escape character {character:?}")]
    UnknownEscape {
        character: char,
        #[label("Character found here")]
        at: SourceSpan,
    },
}

#[derive(Debug, new)]
pub struct Token {
    pub span: SourceSpan,
    pub token_type: TokenType,
}

#[derive(Debug, Clone, PartialEq, EnumDiscriminants)]
#[strum_discriminants(name(TokenTypeName))]
pub enum TokenType {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier(String),
    String(String),
    Number(f64),
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    LineComment,
    Eof,
}

pub struct Scanner<'a> {
    source: &'a str,
    iterator: CharIndices<'a>,
    buffered: FixedQueue<(usize, char), 4>,
    at_end: bool,
    current_offset: usize,
    current_token_start_offset: usize,
    pending_errors: VecDeque<ScannerError>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            iterator: source.char_indices(),
            buffered: FixedQueue::new(),
            at_end: false,
            current_offset: 0,
            current_token_start_offset: 0,
            pending_errors: VecDeque::new(),
        }
    }

    fn advance(&mut self) -> Option<char> {
        if let Some((offset, ch)) = self.buffered.pop_front().or_else(|| self.iterator.next()) {
            self.current_offset = offset;
            Some(ch)
        } else {
            self.at_end = true;
            None
        }
    }

    fn advance_while<F: Fn(char) -> bool>(&mut self, check: F) {
        loop {
            match self.peek(1) {
                Some(ch) if check(ch) => {
                    self.advance();
                }
                _ => return,
            }
        }
    }

    fn peek(&mut self, offset: usize) -> Option<char> {
        assert!(offset > 0);
        for _ in self.buffered.len()..offset {
            if let Some(entry) = self.iterator.next() {
                self.buffered.push_back(entry);
            } else {
                return None;
            }
        }
        self.buffered.get(offset - 1).map(|entry| entry.1)
    }

    fn begin_token(&mut self) {
        self.current_token_start_offset = self.current_offset;
    }

    fn yield_token(&self, token_type: TokenType) -> Token {
        Token::new(
            (self.current_token_start_offset..=self.current_offset).into(),
            token_type,
        )
    }

    fn yield_conditional_token(
        &mut self,
        target_ch: char,
        token_if_found: TokenType,
        token_if_not_found: TokenType,
    ) -> Token {
        let token = match self.peek(1) {
            Some(ch) if ch == target_ch => {
                self.advance();
                token_if_found
            }
            _ => token_if_not_found,
        };
        self.yield_token(token)
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Token, ScannerError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(error) = self.pending_errors.pop_front() {
            return Some(Err(error));
        }

        if self.at_end {
            return None;
        }

        self.advance_while(|ch| ch.is_whitespace());

        let next = self.advance();
        self.begin_token();
        Some(match next {
            None => Ok(self.yield_token(TokenType::Eof)),
            Some('(') => Ok(self.yield_token(TokenType::OpenParen)),
            Some(')') => Ok(self.yield_token(TokenType::CloseParen)),
            Some('{') => Ok(self.yield_token(TokenType::OpenBrace)),
            Some('}') => Ok(self.yield_token(TokenType::CloseBrace)),
            Some(',') => Ok(self.yield_token(TokenType::Comma)),
            Some('.') => Ok(self.yield_token(TokenType::Dot)),
            Some('-') => Ok(self.yield_token(TokenType::Minus)),
            Some('+') => Ok(self.yield_token(TokenType::Plus)),
            Some(';') => Ok(self.yield_token(TokenType::Semicolon)),
            Some('*') => Ok(self.yield_token(TokenType::Star)),
            Some('!') => {
                Ok(self.yield_conditional_token('=', TokenType::BangEqual, TokenType::Bang))
            }
            Some('=') => {
                Ok(self.yield_conditional_token('=', TokenType::EqualEqual, TokenType::Equal))
            }
            Some('<') => {
                Ok(self.yield_conditional_token('=', TokenType::LessEqual, TokenType::Less))
            }
            Some('>') => {
                Ok(self.yield_conditional_token('=', TokenType::GreaterEqual, TokenType::Greater))
            }
            Some('/') => {
                // line comments
                if self.peek(1) == Some('/') {
                    loop {
                        match self.peek(1) {
                            Some('\n') | None => break,
                            _ => self.advance(),
                        };
                    }
                    Ok(self.yield_token(TokenType::LineComment))
                } else {
                    Ok(self.yield_token(TokenType::Slash))
                }
            }
            Some('"') => {
                // strings
                let mut string_value = String::new();
                loop {
                    string_value.push(match self.advance() {
                        Some('\\') => match self.advance() {
                            None => {
                                break Err(ScannerError::UnterminatedString {
                                    at: (self.current_token_start_offset..self.current_offset)
                                        .into(),
                                })
                            }
                            Some('n') => '\n',
                            Some('\\') => '\\',
                            Some('"') => '"',
                            Some('r') => '\r',
                            Some('t') => '\t',
                            Some(ch) => {
                                self.pending_errors.push_back(ScannerError::UnknownEscape {
                                    character: ch,
                                    at: self.current_offset.into(),
                                });
                                continue;
                            }
                        },
                        Some('"') => break Ok(self.yield_token(TokenType::String(string_value))),
                        None => {
                            self.pending_errors
                                .push_back(ScannerError::UnterminatedString {
                                    at: (self.current_token_start_offset..self.current_offset)
                                        .into(),
                                });
                            break Ok(self.yield_token(TokenType::String(string_value)));
                        }
                        Some(ch) => ch,
                    });
                }
            }
            Some(ch) if ch.is_digit(10) => {
                // numbers
                let num_start = self.current_offset;
                self.advance_while(|ch| ch.is_digit(10));
                if self.peek(1) == Some('.')
                    && self.peek(2).map(|ch| ch.is_digit(10)).unwrap_or(false)
                {
                    self.advance();
                    self.advance_while(|ch| ch.is_digit(10));
                }

                let number: f64 = self.source[num_start..=self.current_offset]
                    .parse()
                    .unwrap();

                Ok(self.yield_token(TokenType::Number(number)))
            }
            Some('a'..='z' | 'A'..='Z' | '_') => {
                let word_start = self.current_offset;
                self.advance_while(|ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'));
                let word = &self.source[word_start..=self.current_offset];
                match word {
                    "and" => Ok(self.yield_token(TokenType::And)),
                    "class" => Ok(self.yield_token(TokenType::Class)),
                    "else" => Ok(self.yield_token(TokenType::Else)),
                    "false" => Ok(self.yield_token(TokenType::False)),
                    "for" => Ok(self.yield_token(TokenType::For)),
                    "fun" => Ok(self.yield_token(TokenType::Fun)),
                    "if" => Ok(self.yield_token(TokenType::If)),
                    "nil" => Ok(self.yield_token(TokenType::Nil)),
                    "or" => Ok(self.yield_token(TokenType::Or)),
                    "print" => Ok(self.yield_token(TokenType::Print)),
                    "return" => Ok(self.yield_token(TokenType::Return)),
                    "super" => Ok(self.yield_token(TokenType::Super)),
                    "this" => Ok(self.yield_token(TokenType::This)),
                    "true" => Ok(self.yield_token(TokenType::True)),
                    "var" => Ok(self.yield_token(TokenType::Var)),
                    "while" => Ok(self.yield_token(TokenType::While)),
                    _ => Ok(self.yield_token(TokenType::Identifier(word.to_string()))),
                }
            }
            Some(ch) => Err(ScannerError::UnexpectedCharacter {
                character: ch,
                at: self.current_offset.into(),
            }),
        })
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use std::fs;
    use std::path::Path;

    #[test]
    fn fixtures() {
        let scanner_dir = Path::new("test_fixtures/scanner");
        println!("{:?}", std::env::current_dir().unwrap());
        fs::read_dir(scanner_dir)
            .unwrap()
            .map(Result::unwrap)
            .map(|entry| entry.file_name().into_string().unwrap())
            .filter(|name| name.ends_with("-input.lox"))
            .for_each(|name| {
                let input_str = fs::read_to_string(scanner_dir.join(&name)).unwrap();
                let expected_output_str =
                    fs::read_to_string(scanner_dir.join(name.replace("-input.lox", "-output.ron")))
                        .unwrap_or_else(|_| "".to_string());

                let actual_output_str = format!(
                    "{:#?}",
                    &super::Scanner::new(&input_str).collect::<Vec<_>>()
                );

                println!("actual output for {}:\n{}", name, actual_output_str);
                assert_eq!(expected_output_str.trim(), actual_output_str.trim());
            });
    }
}
