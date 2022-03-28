use std::{
    collections::VecDeque,
    fs::{self},
    io::Write,
    path::{Path, PathBuf},
};

use colored::Colorize;
use itertools::Itertools;
use lazy_static::lazy_static;
use libtest_mimic::{self, run_tests, Arguments, Outcome, Test};
use lox_rs::{
    Interpreter, Parser, ParserError, ParserOpts, RuntimeError, Scanner, ScannerError,
    SourceOffset, SourceReference, SourceSpan,
};
use miette::{miette, IntoDiagnostic, Result};
use regex::Regex;

lazy_static! {
    static ref IGNORE_PATTERN: Regex = Regex::new("test_fixtures/((benchmark|class|constructor|field|inheritance|limit|method|regression|return|super|this)/|(variable/(early_bound|local_from_method|use_this_as_var|use_local_in_initializer|duplicate_parameter)|operator/(not_class|equals_class|equals_method)|assignment/to_this|while/class_in_body|for/class_in_body|closure/(close_over_method_parameter|assign_to_shadowed_later)|function/local_mutual_recursion|call/object).lox)").unwrap();
}

fn main() {
    let tests = read_all_files("test_fixtures".to_string().into())
        .unwrap()
        .into_iter()
        .filter(|path| !path.starts_with("test_fixtures/scanner"))
        .map(|path| Test {
            name: path.to_string_lossy().into(),
            kind: "tree-walk".into(),
            is_bench: false,
            is_ignored: IGNORE_PATTERN.is_match(&path.to_string_lossy()),
            data: path,
        })
        .collect::<Vec<_>>();

    // Run all tests and exit the application appropriatly (in this case, the
    // test runner is a dummy runner which does nothing and says that all s
    // passed).
    run_tests(&Arguments::from_args(), tests, |test| {
        match run_test(&test.data) {
            Ok(outcome) => outcome,
            Err(err) => Outcome::Failed {
                msg: Some(format!("{:?}", err)),
            },
        }
    })
    .exit();
}

lazy_static! {
    static ref EXPECTED_OUTPUT_RE: Regex = Regex::new("// expect: (.*)\n?").unwrap();
    static ref PARSER_ERROR_RE: Regex = Regex::new("// (Scanner|Parser)Error: (.*)\n?").unwrap();
    static ref RUNTIME_ERROR_RE: Regex = Regex::new("// RuntimeError: (.*)\n?").unwrap();
}

fn run_test(path: &Path) -> Result<Outcome> {
    let test_source = fs::read_to_string(path).into_diagnostic()?;
    let source_reference =
        SourceReference::new(path.to_string_lossy().to_string(), test_source.clone());

    let expected_output = EXPECTED_OUTPUT_RE
        .captures_iter(&test_source)
        .map(|captures| format!("{}\n", &captures[1]))
        .collect::<String>();

    let mut expected_parser_errors = PARSER_ERROR_RE
        .captures_iter(&test_source)
        .map(|captures| format!("{}Error: {}", &captures[1], &captures[2]))
        .collect::<VecDeque<_>>();

    let expected_runtime_error = RUNTIME_ERROR_RE
        .captures_iter(&test_source)
        .map(|captures| format!("RuntimeError: {}", &captures[1]))
        .at_most_one()
        .map_err(|_| miette!("should have at most one expected runtime error"))?;

    let mut scanner_errors = Vec::new();
    let token_stream =
        Scanner::new(&test_source, source_reference.clone()).filter_map(|token_or_err| {
            match token_or_err {
                Ok(token) => Some(token),
                Err(error) => {
                    scanner_errors.push(error);
                    None
                }
            }
        });

    let (program, parser_errors) =
        Parser::parse(token_stream, source_reference, ParserOpts::default());

    let did_have_scan_or_parse_errors = !scanner_errors.is_empty() || !parser_errors.is_empty();
    for scanner_error in scanner_errors {
        match match_errors(
            scanner_error,
            &expected_parser_errors.pop_front(),
            &test_source,
        ) {
            Ok(_) => continue,
            Err(msg) => return Ok(Outcome::Failed { msg: Some(msg) }),
        }
    }

    for parser_error in parser_errors {
        match match_errors(
            parser_error,
            &expected_parser_errors.pop_front(),
            &test_source,
        ) {
            Ok(_) => continue,
            Err(msg) => return Ok(Outcome::Failed { msg: Some(msg) }),
        }
    }

    if !expected_parser_errors.is_empty() {
        return Ok(Outcome::Failed {
            msg: Some(format!(
                "Expected errors:\n{}",
                expected_parser_errors
                    .iter()
                    .map(|err| format!(" - {}\n", err))
                    .collect::<String>(),
            )),
        });
    }

    if did_have_scan_or_parse_errors {
        return Ok(Outcome::Passed);
    }

    let mut output_writer = StringWriter::new();
    let mut interpreter = Interpreter::new(&mut output_writer);
    if let Err(err) = interpreter.interpret(&program) {
        if let Err(err) = match_errors(err, &expected_runtime_error, &test_source) {
            return Ok(Outcome::Failed { msg: Some(err) });
        }
    } else if let Some(expected_err) = &expected_runtime_error {
        return Ok(Outcome::Failed {
            msg: Some(format!("Expected runtime error:\n{}", expected_err)),
        });
    }

    let actual_output: String = output_writer.into();

    Ok(compare_outputs(
        expected_output.split('\n').map(String::from).collect(),
        actual_output.split('\n').map(String::from).collect(),
    ))
}

fn match_errors<E: FmtError>(
    actual_error: E,
    expected_error: &Option<String>,
    source: &str,
) -> Result<(), String> {
    let actual_str = actual_error.fmt_error(source);
    match expected_error {
        Some(expected_str) if expected_str.trim() == actual_str.trim() => Ok(()),
        Some(expected_str) => Err(format!(
            "Errors do not match.\nExpected: {}\n  Actual: {}",
            expected_str, actual_str
        )),
        None => Err(format!("Unexpected error:\n{}", actual_str)),
    }
}

fn compare_outputs(expected_lines: Vec<String>, actual_lines: Vec<String>) -> Outcome {
    const EXPECTED: &str = "expected";
    const ACTUAL: &str = "actual";
    const NONE: &str = "<None>";

    fn max_len(lines: &[String], label: &str) -> usize {
        lines
            .iter()
            .map(|line| line.len())
            .max()
            .unwrap_or(0)
            .max(label.len())
    }

    let max_expected_len = max_len(&expected_lines, EXPECTED);
    let max_actual_len = max_len(&actual_lines, ACTUAL);

    let mut output_str = format!(
        "   | {:max_expected_len$} | {:max_actual_len$} \n",
        EXPECTED.bold(),
        ACTUAL.bold()
    );
    let line_count = expected_lines.len().max(actual_lines.len());
    let mut unmatched_count = 0usize;
    for i in 0..line_count {
        let expected_line = expected_lines.get(i);
        let actual_line = actual_lines.get(i);

        let is_match = expected_line == actual_line;
        if !is_match {
            unmatched_count += 1;
        }

        let colorify = |string: &str| {
            if is_match {
                string.green()
            } else {
                string.red()
            }
        };

        let result_char = if is_match { "✓" } else { "✗" };
        let result_str = &format!(
            " {} | {:max_expected_len$} | {:max_actual_len$}",
            colorify(result_char),
            expected_line
                .map(|line| colorify(line))
                .unwrap_or_else(|| NONE.dimmed()),
            actual_line
                .map(|line| colorify(line))
                .unwrap_or_else(|| NONE.dimmed()),
        );
        output_str.push_str(result_str);
        output_str.push('\n');
    }

    if unmatched_count > 0 {
        Outcome::Failed {
            msg: Some(output_str),
        }
    } else {
        Outcome::Passed
    }
}

fn read_all_files(prefix: PathBuf) -> Result<Vec<PathBuf>> {
    let mut results = Vec::<PathBuf>::new();
    read_children(prefix, &mut results)?;
    return Ok(results);

    fn read_children(prefix: PathBuf, results: &mut Vec<PathBuf>) -> Result<()> {
        for entry in fs::read_dir(prefix).into_diagnostic()? {
            let entry = entry.into_diagnostic()?;
            if entry.file_name().to_string_lossy().starts_with('.') {
                continue;
            }
            if entry.file_type().into_diagnostic()?.is_dir() {
                read_children(entry.path(), results)?;
            } else {
                results.push(entry.path())
            }
        }
        Ok(())
    }
}

struct StringWriter {
    string: String,
}
impl StringWriter {
    fn new() -> Self {
        Self {
            string: String::new(),
        }
    }
}
impl Write for StringWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let new_string_part = String::from_utf8_lossy(buf);
        self.string.push_str(&new_string_part);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
impl From<StringWriter> for String {
    fn from(string_writer: StringWriter) -> Self {
        string_writer.string
    }
}

trait FmtError {
    fn fmt_error(&self, source: &str) -> String;
}
impl FmtError for ScannerError {
    fn fmt_error(&self, source: &str) -> String {
        match self {
            ScannerError::UnexpectedCharacter { character, at, .. } => format!(
                "ScannerError: UnexpectedCharacter {} at {}",
                character,
                format_offset(at, source)
            ),
            ScannerError::UnterminatedString { at, .. } => format!(
                "ScannerError: UnterminatedString {}",
                format_span(at, source)
            ),
            ScannerError::UnknownEscape { character, at, .. } => format!(
                "ScannerError: UnknownEscape {} {}",
                character,
                format_span(at, source)
            ),
        }
    }
}
impl FmtError for ParserError {
    fn fmt_error(&self, source: &str) -> String {
        match self {
            ParserError::UnmatchedParenthesis {
                opener,
                found_token_type,
                found_at,
                ..
            } => format!(
                "ParserError: UnmatchedParenthesis opened {}. found {:?} {}. ",
                format_span(opener, source),
                found_token_type,
                format_span(found_at, source)
            ),
            ParserError::ExpectedSemicolor {
                actual, found_at, ..
            } => format!(
                "ParserError: ExpectedSemicolon found {:?} {}",
                actual,
                format_span(found_at, source),
            ),
            ParserError::UnexpectedExpressionToken {
                actual, found_at, ..
            } => format!(
                "ParserError: UnexpectedExpressionToken found {:?} {}",
                actual,
                format_span(found_at, source),
            ),
            ParserError::UnexpectedToken {
                actual,
                expected,
                found_at,
                ..
            } => format!(
                "ParserError: UnexpectedToken found {:?} {} expected {:?}",
                actual,
                format_span(found_at, source),
                expected
            ),
            ParserError::InvalidAssignmentTarget { found_at, .. } => format!(
                "ParserError: InvalidAssignmentTarget {}",
                format_span(found_at, source)
            ),
            ParserError::TooManyCallArgs {
                callee_at,
                too_many_args_at,
                ..
            } => format!(
                "ParserError: TooManyCallArgs callee {} arg {}",
                format_span(callee_at, source),
                format_span(too_many_args_at, source)
            ),
            ParserError::TooManyFunParams {
                decl_at,
                too_many_params_at,
                ..
            } => format!(
                "ParserError: TooManyFunParams decl {} param {}",
                format_span(decl_at, source),
                format_span(too_many_params_at, source)
            ),
        }
    }
}
impl FmtError for RuntimeError {
    fn fmt_error(&self, source: &str) -> String {
        match self {
            RuntimeError::OperandTypeError {
                expected_type,
                actual_type,
                operand_loc,
                operator,
                operator_loc,
                ..
            } => format!(
                "RuntimeError: Operator {} {} expected {} got {} {}",
                operator,
                format_span(operator_loc, source),
                expected_type.fmt_a(),
                actual_type.fmt_a(),
                format_span(operand_loc, source)
            ),
            RuntimeError::UndefinedVariable { name, found_at, .. } => format!(
                "RuntimeError: undefined variable {} {}",
                name,
                format_span(found_at, source)
            ),
            RuntimeError::AlreadyDefinedVariable { name, found_at, .. } => format!(
                "RuntimeError: {} AlreadyDefinedVariable {}",
                format_span(found_at, source),
                name,
            ),
            RuntimeError::UnexpectedCallArity {
                expected_arity,
                actual_arity,
                found_at,
                ..
            } => format!(
                "RuntimeError: expected call arity {} got {} {}",
                expected_arity,
                actual_arity,
                format_span(found_at, source)
            ),
            RuntimeError::UncallableValue {
                actual_type,
                found_at,
                ..
            } => format!(
                "RuntimeError: cannot call {} {}",
                actual_type.fmt_a(),
                format_span(found_at, source)
            ),
        }
    }
}

fn format_offset(loc: &SourceOffset, source: &str) -> String {
    let mut line_no: usize = 1;
    let mut char_no: usize = 0;
    let target = loc.byte_offset();
    for (byte_offset, ch) in source.char_indices() {
        char_no += 1;
        if byte_offset >= target {
            break;
        }
        if ch == '\n' {
            line_no += 1;
            char_no = 0;
        }
    }

    format!("l{}c{}", line_no, char_no)
}
fn format_span(span: &SourceSpan, source: &str) -> String {
    if span.len().byte_offset() <= 1 {
        format!("at {}", format_offset(&span.start(), source))
    } else {
        format!(
            "from {} to {}",
            format_offset(&span.start(), source),
            format_offset(&span.end(), source)
        )
    }
}
