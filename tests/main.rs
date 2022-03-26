use std::{
    fs::{self},
    io::Write,
    path::PathBuf,
};

use colored::Colorize;
use lazy_static::lazy_static;
use libtest_mimic::{self, run_tests, Arguments, Outcome, Test};
use lox_rs::{parse_and_collect_errors, Interpreter};
use miette::{IntoDiagnostic, NamedSource, Report, Result};
use regex::Regex;

fn main() {
    let tests = read_all_files("test_fixtures".to_string().into())
        .unwrap()
        .into_iter()
        .filter(|path| !path.starts_with("test_fixtures/scanner"))
        .map(|path| Test {
            name: path.to_string_lossy().into(),
            kind: "".into(),
            is_bench: false,
            is_ignored: false,
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
    static ref EXPECT_RE: Regex = Regex::new("// expect: (.*)\n?").unwrap();
}

fn run_test(path: &PathBuf) -> Result<Outcome> {
    let test_source = fs::read_to_string(path).into_diagnostic()?;

    let mut expected_output = String::new();
    EXPECT_RE.captures_iter(&test_source).for_each(|captures| {
        expected_output.push_str(&captures[1]);
        expected_output.push('\n');
    });

    let (program, parse_errors) = parse_and_collect_errors(&path.to_string_lossy(), &test_source);
    if !parse_errors.is_empty() {
        return Ok(Outcome::Failed {
            msg: Some(
                parse_errors
                    .into_iter()
                    .map(|report| format!("{:?}", report))
                    .collect::<Vec<_>>()
                    .join("\n\n"),
            ),
        });
    }

    let mut output_writer = StringWriter::new();
    let mut interpreter = Interpreter::new(&mut output_writer);
    if let Err(err) = interpreter.interpret(&program) {
        return Ok(Outcome::Failed {
            msg: Some(format!(
                "{:?}",
                Report::new(err)
                    .with_source_code(NamedSource::new(path.to_string_lossy(), test_source)),
            )),
        });
    }

    let actual_output: String = output_writer.into();

    Ok(compare_outputs(
        expected_output.split('\n').map(String::from).collect(),
        actual_output.split('\n').map(String::from).collect(),
    ))
}

fn compare_outputs(expected_lines: Vec<String>, actual_lines: Vec<String>) -> Outcome {
    const EXPECTED: &str = "expected";
    const ACTUAL: &str = "actual";
    const NONE: &str = "<None>";

    fn max_len(lines: &Vec<String>, label: &str) -> usize {
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
                .unwrap_or(NONE.dimmed()),
            actual_line
                .map(|line| colorify(line))
                .unwrap_or(NONE.dimmed()),
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
