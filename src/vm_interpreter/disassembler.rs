use super::chunk::{Chunk, CodeReadError, OpCode};
use crate::SourceSpan;
use colored::{ColoredString, Colorize};
use std::fmt::Write;

// const OP_CODE_WIDTH: usize = 12;

impl Chunk {
    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        let mut offset = 0;
        while offset < self.code().len() {
            match self.disassemble_instruction_at(offset) {
                Ok(next_offset) => offset = next_offset,
                Err(e) => {
                    println!("{}", format!("Error reading code: {}", e).red());
                }
            }
        }
    }
    pub fn disassemble_instruction_at(
        &self,
        initial_offset: usize,
    ) -> Result<usize, CodeReadError> {
        let (mut offset, op_code) = self.read_op_code(initial_offset)?;
        let source_info: Option<ColoredString> = self
            .read_debug_span(initial_offset)
            .map(|(source, span)| Colorize::clear(get_formatted_line(source.str(), span).as_str()));

        let basic_info = match op_code {
            OpCode::Return
            | OpCode::Negate
            | OpCode::Add
            | OpCode::Subtract
            | OpCode::Multiply
            | OpCode::Divide
            | OpCode::Print => {
                print_line([
                    (Some(format!("{:>4}", offset).dimmed()), 4),
                    (Some(" | ".into()), 3),
                    (Some(format!("{:?}", op_code).purple()), 50),
                    (source_info, 0),
                ]);
                // format!(
                //     "{} | {}",
                //     format!("{:04}", initial_offset).dimmed(),
                //     format!("{:?}", op_code).purple(),
                // )
            }
            OpCode::Constant => {
                let (next_offset, address) = self.read_constant_address(offset)?;
                offset = next_offset;
                print_line([
                    (Some(format!("{:>4}", offset).dimmed()), 4),
                    (Some(" | ".into()), 3),
                    (Some(format!("{:?}", op_code).purple()), 10),
                    (Some(format!(" {:>4}", address).green()), 5),
                    (Some(" = ".into()), 3),
                    (
                        Some(format!("{:?}", self.get_constant_value(address)?).blue()),
                        32,
                    ),
                    (source_info, 0),
                ]);
                // format!(
                //     "{} | {} {} = {}",
                //     format!("{:04}", initial_offset).dimmed(),
                //     format!("{:?}", op_code).purple(),
                //     format!("{}", address).green(),
                //     format!("{:?}", self.get_constant_value(address)?).blue(),
                // )
            }
        };

        Ok(offset)
    }
}

fn print_line<const N: usize>(parts: [(Option<ColoredString>, usize); N]) {
    let mut out = String::new();
    for (part, width) in parts {
        write!(out, "{:<width$}", part.unwrap_or("".into()), width = width).unwrap();
    }
    print!("{}\n", out.on_black());
}

fn get_formatted_line(source: &str, span: SourceSpan) -> String {
    let mut line_start_idx = 0;
    let mut line = 1;
    let mut line_end_idx = 0;

    let start_offset = span.start().byte_offset();
    let end_offset = span.end().byte_offset().max(start_offset);

    enum Stage {
        Before,
        Within,
        After,
    }
    let mut stage = Stage::Before;

    for (idx, ch) in source.char_indices() {
        match stage {
            Stage::Before => {
                if ch == '\n' {
                    line_start_idx = idx;
                    line += 1;
                }
                if idx == start_offset {
                    stage = Stage::Within;
                }
            }
            Stage::Within => {
                if ch == '\n' {
                    line_end_idx = idx;
                    break;
                }
                if idx >= end_offset {
                    stage = Stage::After;
                }
            }
            Stage::After => {
                line_end_idx = idx;
                if ch == '\n' {
                    break;
                }
            }
        }
    }

    format!(
        "{} {}{}{}",
        format!("{:>3}:", line).dimmed(),
        &source[line_start_idx..start_offset]
            .trim_start_matches('\n')
            .dimmed(),
        &source[start_offset..end_offset].magenta().bold(),
        &source[end_offset..line_end_idx].dimmed(),
    )
}