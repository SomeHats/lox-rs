use std::io::Stdout;

use ast::Program;
use miette::{IntoDiagnostic, NamedSource, Report, Result};
use parser::Parser;
use rustyline::error::ReadlineError;
use tree_walk_interpreter::InterpreterContext;

use crate::tree_walk_interpreter::Interpreter;

mod ast;
mod fixed_que;
mod parser;
mod scanner;
mod source;
mod tree_walk_interpreter;
mod value;

fn main() -> Result<()> {
    let args: Vec<_> = std::env::args().collect();
    match args.as_slice() {
        [_] => run_prompt(),
        [_, script] => run_file(script.clone()),
        _ => {
            println!("Usage: lox-rs [script]");
            std::process::exit(64);
        }
    }
}

fn parse_and_report_errors(file_name: &str, source: &str) -> Option<Program> {
    let make_err_source = || NamedSource::new(file_name, source.to_string());
    let mut did_have_scanner_error = false;
    let token_stream =
        scanner::Scanner::new(source).filter_map(|token_or_err| match token_or_err {
            Ok(token) => Some(token),
            Err(error) => {
                let report = Report::new(error).with_source_code(make_err_source());
                println!("{:?}", report);
                did_have_scanner_error = true;
                None
            }
        });

    let (program, parser_errors) = Parser::parse(token_stream);
    let did_have_parser_error = !parser_errors.is_empty();
    for error in parser_errors.into_iter() {
        let report = Report::new(error).with_source_code(make_err_source());
        println!("{:?}", report);
    }

    if did_have_scanner_error || did_have_parser_error {
        None
    } else {
        Some(program)
    }
}

fn run_file(file_name: String) -> Result<()> {
    let path = std::fs::canonicalize(file_name).into_diagnostic()?;
    let source = std::fs::read_to_string(&path).into_diagnostic()?;

    match parse_and_report_errors(&path.to_string_lossy(), &source) {
        None => std::process::exit(70),
        Some(program) => {
            let mut ctx = MainInterpreterContext::new();
            let mut interpreter = Interpreter::new();
            interpreter.interpret(&program, &mut ctx).map_err(|err| {
                Report::new(err)
                    .with_source_code(NamedSource::new(path.to_string_lossy(), source.clone()))
            })?;
        }
    }

    Ok(())
}

fn run_prompt() -> Result<()> {
    let mut rl = rustyline::Editor::<()>::new();
    let mut ctx = MainInterpreterContext::new();
    let mut interpreter = Interpreter::new();
    loop {
        match rl.readline("> ") {
            Ok(line) => match parse_and_report_errors("<repl>", &line) {
                None => continue,
                Some(program) => {
                    println!("{}", program);
                    match interpreter.interpret(&program, &mut ctx) {
                        Ok(value) => println!("==> {}", value.as_ref()),
                        Err(err) => {
                            let report =
                                Report::new(err).with_source_code(NamedSource::new("<repl>", line));
                            println!("{:?}", report);
                        }
                    }
                }
            },
            Err(ReadlineError::Interrupted) => return Ok(()),
            Err(ReadlineError::Eof) => return Ok(()),
            Err(err) => return Err(err).into_diagnostic(),
        }
    }
}

struct MainInterpreterContext {
    stdout: Stdout,
}
impl MainInterpreterContext {
    fn new() -> Self {
        MainInterpreterContext {
            stdout: std::io::stdout(),
        }
    }
}
impl InterpreterContext<Stdout> for MainInterpreterContext {
    fn stdout(&mut self) -> &mut Stdout {
        &mut self.stdout
    }
}
