pub mod ast;
mod fixed_que;
mod parser;
mod scanner;
mod source;
mod tree_walk_interpreter;
mod value;

pub use parser::{Parser, ParserError};
pub use scanner::{Scanner, ScannerError};
pub use tree_walk_interpreter::{Interpreter, RuntimeError};
pub use value::{Value, ValueType};

use crate::scanner::TokenType;

pub fn parse_and_collect_errors(
    file_name: &str,
    source: &str,
) -> (ast::Program, Vec<miette::Report>) {
    let make_err_source = || miette::NamedSource::new(file_name, source.to_string());
    let mut errors = Vec::new();

    let token_stream = Scanner::new(source).filter_map(|token_or_err| match token_or_err {
        Ok(token) if token.token_type == TokenType::LineComment => None,
        Ok(token) => Some(token),
        Err(error) => {
            errors.push(miette::Report::new(error).with_source_code(make_err_source()));
            None
        }
    });

    let (program, parser_errors) = Parser::parse(token_stream);
    parser_errors
        .into_iter()
        .map(|err| miette::Report::new(err).with_source_code(make_err_source()))
        .for_each(|err| errors.push(err));

    (program, errors)
}
