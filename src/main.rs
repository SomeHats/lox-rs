use std::io::stdout;

use miette::{IntoDiagnostic, NamedSource, Report, Result};
use rustyline::error::ReadlineError;

use lox_rs::{ast::Program, Interpreter, Parser, ParserOpts, Scanner, SourceReference};

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

fn parse_and_report_errors(
    file_name: &str,
    source: &str,
    parser_opts: ParserOpts,
) -> Option<Program> {
    let source_reference = SourceReference::new(file_name.to_string(), source.to_string());
    let make_err_source = || NamedSource::new(file_name, source.to_string());
    let mut did_have_scanner_error = false;
    let token_stream =
        Scanner::new(source, source_reference.clone()).filter_map(
            |token_or_err| match token_or_err {
                Ok(token) => Some(token),
                Err(error) => {
                    let report = Report::new(error).with_source_code(make_err_source());
                    println!("{:?}", report);
                    did_have_scanner_error = true;
                    None
                }
            },
        );

    let (program, parser_errors) = Parser::parse(token_stream, source_reference, parser_opts);
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

    match parse_and_report_errors(&path.to_string_lossy(), &source, ParserOpts::default()) {
        None => std::process::exit(70),
        Some(program) => {
            let mut stdout = stdout();
            let mut interpreter = Interpreter::new(&mut stdout);
            interpreter.interpret(&program).map_err(Report::new)?;
        }
    }

    Ok(())
}

fn run_prompt() -> Result<()> {
    let mut rl = rustyline::Editor::<()>::new();
    let mut stdout = stdout();
    let mut interpreter = Interpreter::new(&mut stdout);
    let mut repl_line: usize = 1;
    loop {
        match rl.readline(&format!("{}> ", repl_line)) {
            Ok(line) => {
                match parse_and_report_errors(
                    &format!("<repl-{}>", repl_line),
                    &line,
                    ParserOpts::default().for_repl(),
                ) {
                    None => continue,
                    Some(program) => {
                        println!("{}", program);
                        match interpreter.interpret(&program) {
                            Ok(value) => println!("==> {:?}", value),
                            Err(err) => {
                                println!("{:?}", Report::new(err));
                            }
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => return Ok(()),
            Err(ReadlineError::Eof) => return Ok(()),
            Err(err) => return Err(err).into_diagnostic(),
        }
        repl_line += 1;
    }
}
