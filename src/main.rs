use std::io::{stdout, Write};

use miette::{Diagnostic, IntoDiagnostic, Report, Result};
use rustyline::error::ReadlineError;

use lox_rs::{Interpreter, Parser, ParserOpts, PreparedProgram, Scanner, SourceReference};

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

fn report_all_errors<E: Diagnostic + Send + Sync + 'static>(errors: impl IntoIterator<Item = E>) {
    for error in errors {
        println!("{:?}", Report::new(error));
    }
}

fn prepare_and_report_errors<W: Write>(
    file_name: &str,
    source: &str,
    parser_opts: ParserOpts,
    interpreter: &mut Interpreter<W>,
) -> Option<PreparedProgram> {
    let source_reference = SourceReference::new(file_name.to_string(), source.to_string());
    let mut did_have_scanner_error = false;
    let token_stream =
        Scanner::new(source, source_reference.clone()).filter_map(
            |token_or_err| match token_or_err {
                Ok(token) => Some(token),
                Err(error) => {
                    println!("{:?}", Report::new(error));
                    did_have_scanner_error = true;
                    None
                }
            },
        );

    let (program, parser_errors) = Parser::parse(token_stream, source_reference, parser_opts);
    let did_have_parser_error = !parser_errors.is_empty();
    report_all_errors(parser_errors);

    match interpreter.prepare(program) {
        Ok(prepared) => {
            if did_have_scanner_error || did_have_parser_error {
                None
            } else {
                Some(prepared)
            }
        }
        Err(errors) => {
            report_all_errors(errors);
            None
        }
    }
}

fn run_file(file_name: String) -> Result<()> {
    let path = std::fs::canonicalize(file_name).into_diagnostic()?;
    let source = std::fs::read_to_string(&path).into_diagnostic()?;
    let mut stdout = stdout();
    let mut interpreter = Interpreter::new(&mut stdout);

    match prepare_and_report_errors(
        &path.to_string_lossy(),
        &source,
        ParserOpts::default(),
        &mut interpreter,
    ) {
        None => std::process::exit(70),
        Some(program) => {
            interpreter.interpret(&program)?;
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
                match prepare_and_report_errors(
                    &format!("<repl-{}>", repl_line),
                    &line,
                    ParserOpts::default().for_repl(),
                    &mut interpreter,
                ) {
                    None => {}
                    Some(prepared_program) => {
                        println!("{}", prepared_program.program());
                        match interpreter.interpret(&prepared_program) {
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
