use std::{
    io::{stdout, Write},
    path::PathBuf,
};

use miette::{Diagnostic, IntoDiagnostic, Report, Result};
use rustyline::error::ReadlineError;

use lox_rs::{
    ast::Program,
    vm_interpreter::{Compiler, Vm},
    Interpreter, Parser, ParserOpts, PreparedProgram, Scanner, SourceReference,
};

fn main() -> Result<()> {
    let mut args: Vec<_> = std::env::args().skip(1).collect();
    let use_old = consume_arg(&mut args, |arg| (arg == "--old").then_some(true)).unwrap_or(false);
    let file = consume_arg(&mut args, |arg| {
        if arg.starts_with("--") {
            None
        } else {
            Some(arg.to_string())
        }
    });
    if let Some(file) = file {
        if !use_old {
            let should_disassemble =
                consume_arg(&mut args, |arg| (arg == "--disassemble").then_some(true))
                    .unwrap_or(false);

            if should_disassemble {
                finish_args(args);
                let (path, source) = read_source_file(&file)?;
                let program = parse_or_exit(path, &source, ParserOpts::default());
                let chunk = Compiler::compile(program)?;
                chunk.disassemble(&file);
                return Ok(());
            }
        }

        finish_args(args);
        run_file(file, use_old)?;
    } else {
        finish_args(args);
        run_prompt(use_old)?;
    }

    Ok(())
}

fn finish_args(args: Vec<String>) {
    if !args.is_empty() {
        eprintln!("Unrecognized arguments: {:?}", args);
        eprintln!("Usage: lox-rs [--old] [file]");
        std::process::exit(1);
    }
}

fn consume_arg<T, F: Fn(&str) -> Option<T>>(args: &mut Vec<String>, predicate: F) -> Option<T> {
    let found = args
        .iter()
        .enumerate()
        .filter_map(|(idx, arg)| predicate(arg).map(|val| (idx, val)))
        .next();

    if let Some((idx, val)) = found {
        args.remove(idx);
        Some(val)
    } else {
        None
    }
}

fn report_all_errors<E: Diagnostic + Send + Sync + 'static>(errors: impl IntoIterator<Item = E>) {
    for error in errors {
        println!("{:?}", Report::new(error));
    }
}

fn parse_and_report_errors(
    file_name: &str,
    source: &str,
    parser_opts: ParserOpts,
) -> (Program, bool) {
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

    (program, did_have_scanner_error || did_have_parser_error)
}

fn parse_or_exit(file_name: PathBuf, source: &str, parser_opts: ParserOpts) -> Program {
    let (program, did_have_error) =
        parse_and_report_errors(file_name.to_str().unwrap_or("<stdin>"), source, parser_opts);
    if did_have_error {
        std::process::exit(1);
    }
    program
}

fn prepare_interpreter<W: Write>(
    interpreter: &mut Interpreter<W>,
    program: Program,
    did_have_error: bool,
) -> Option<PreparedProgram> {
    match interpreter.prepare(program) {
        Ok(prepared) => {
            if did_have_error {
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

fn run_file(file_name: String, use_old: bool) -> Result<()> {
    let (path, source) = read_source_file(&file_name)?;
    let mut stdout = stdout();

    let (program, did_have_error) =
        parse_and_report_errors(&path.to_string_lossy(), &source, ParserOpts::default());

    if use_old {
        let mut interpreter = Interpreter::new(&mut stdout);
        match prepare_interpreter(&mut interpreter, program, did_have_error) {
            None => std::process::exit(70),
            Some(program) => {
                interpreter.interpret(&program)?;
            }
        }
    } else {
        let mut vm = Vm::new(&mut stdout);
        if did_have_error {
            std::process::exit(70);
        } else {
            let compiled = Compiler::compile(program)?;
            vm.run(compiled)?;
        }
    }

    Ok(())
}

fn read_source_file(file_name: &str) -> Result<(std::path::PathBuf, String), Report> {
    let path = std::fs::canonicalize(file_name).into_diagnostic()?;
    let source = std::fs::read_to_string(&path).into_diagnostic()?;
    Ok((path, source))
}

fn repl_loop<F: FnMut(String, String) -> Option<Result<String, Report>>>(
    mut eval: F,
) -> Result<()> {
    let mut rl = rustyline::Editor::<()>::new();
    let mut repl_line: usize = 1;
    loop {
        match rl.readline(&format!("{}> ", repl_line)) {
            Ok(line) => match eval(format!("<repl-{}>", repl_line), format!("{}\n", line)) {
                Some(Ok(val)) => println!("==> {}", val),
                Some(Err(err)) => println!("{:?}", err),
                None => {}
            },
            Err(ReadlineError::Interrupted) => return Ok(()),
            Err(ReadlineError::Eof) => return Ok(()),
            Err(err) => return Err(err).into_diagnostic(),
        }
        repl_line += 1;
    }
}

fn run_prompt(use_old: bool) -> Result<()> {
    let mut stdout = stdout();
    if use_old {
        let mut interpreter = Interpreter::new(&mut stdout);
        repl_loop(|file_name, source| {
            let (program, did_have_error) =
                parse_and_report_errors(&file_name, &source, ParserOpts::default().for_repl());
            prepare_interpreter(&mut interpreter, program, did_have_error).map(|program| {
                interpreter
                    .interpret(&program)
                    .map(|result| format!("{:?}", result))
                    .map_err(Report::new)
            })
        })
    } else {
        let mut vm = Vm::new(&mut stdout);
        repl_loop(|file_name, source| {
            let (program, did_have_error) =
                parse_and_report_errors(&file_name, &source, ParserOpts::default().for_repl());
            if did_have_error {
                None
            } else {
                match Compiler::compile(program) {
                    Ok(chunk) => Some(
                        vm.run(chunk)
                            .map(|result| format!("{:?}", result))
                            .map_err(Report::new),
                    ),
                    Err(error) => Some(Err(Report::new(error))),
                }
            }
        })
    }
    // loop {
    //     match rl.readline(&format!("{}> ", repl_line)) {
    //         Ok(line) => {
    //             match prepare_and_report_errors(
    //                 &format!("<repl-{}>", repl_line),
    //                 &line,
    //                 ParserOpts::default().for_repl(),
    //                 &mut interpreter,
    //             ) {
    //                 None => {}
    //                 Some(prepared_program) => match interpreter.interpret(&prepared_program) {
    //                     Ok(value) => println!("==> {:?}", value),
    //                     Err(err) => {
    //                         println!("{:?}", Report::new(err));
    //                     }
    //                 },
    //             }
    //         }
    //         Err(ReadlineError::Interrupted) => return Ok(()),
    //         Err(ReadlineError::Eof) => return Ok(()),
    //         Err(err) => return Err(err).into_diagnostic(),
    //     }
    //     repl_line += 1;
    // }
}
