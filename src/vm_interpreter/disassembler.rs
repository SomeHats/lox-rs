use super::chunk::{Chunk, CodeReadError, ConstantValue, OpCode, OpDebug};
use colored::{ColoredString, Colorize};
use itertools::Itertools;
use std::fmt::Write;

const OP_CODE_WIDTH: usize = 15;

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
        let source_info = self
            .read_op_debug(initial_offset)
            .map(|op_debug| {
                Colorize::clear(get_formatted_line(self.source().str(), op_debug).as_str())
            })
            .unwrap();

        match op_code {
            OpCode::Return
            | OpCode::Print
            | OpCode::Pop
            | OpCode::Nil
            | OpCode::Negate
            | OpCode::Add
            | OpCode::Subtract
            | OpCode::Multiply
            | OpCode::Divide
            | OpCode::Not
            | OpCode::NotEqualTo
            | OpCode::EqualTo
            | OpCode::LessThan
            | OpCode::LessThanOrEqualTo
            | OpCode::GreaterThan
            | OpCode::GreaterThanOrEqualTo => {
                print_line([
                    self.prefix_data(initial_offset, offset),
                    (Some(" | ".into()), 3),
                    (Some(format!("{:?}", op_code).purple()), OP_CODE_WIDTH + 45),
                    (Some(source_info), 0),
                ]);
            }
            OpCode::Jump | OpCode::JumpIfTrue | OpCode::JumpIfFalse => {
                let (next_offset, arg) = self.read_u16(offset)?;
                offset = next_offset;

                print_line([
                    self.prefix_data(initial_offset, offset),
                    (Some(" | ".into()), 3),
                    (Some(format!("{:?}", op_code).purple()), OP_CODE_WIDTH),
                    (Some(format!(" +{}", arg).green()), 5),
                    (Some(" -> ".into()), 4),
                    (
                        Some(format!("{:<5}", initial_offset + usize::from(arg)).blue()),
                        36,
                    ),
                    (Some(source_info), 0),
                ]);
            }
            OpCode::Loop => {
                let (next_offset, arg) = self.read_u16(offset)?;
                offset = next_offset;

                print_line([
                    self.prefix_data(initial_offset, offset),
                    (Some(" | ".into()), 3),
                    (Some(format!("{:?}", op_code).purple()), OP_CODE_WIDTH),
                    (Some(format!(" -{}", arg).green()), 5),
                    (Some(" -> ".into()), 4),
                    (
                        Some(format!("{:<5}", initial_offset - usize::from(arg)).blue()),
                        36,
                    ),
                    (Some(source_info), 0),
                ]);
            }
            OpCode::ReadLocal | OpCode::SetLocal => {
                let (next_offset, local_index) = self.read_u8(offset)?;
                offset = next_offset;
                print_line([
                    self.prefix_data(initial_offset, offset),
                    (Some(" | ".into()), 3),
                    (Some(format!("{:?}", op_code).purple()), OP_CODE_WIDTH),
                    (Some(format!(" {}", local_index).green()), 45),
                    (Some(source_info), 0),
                ]);
            }
            OpCode::Constant | OpCode::DefineGlobal | OpCode::ReadGlobal | OpCode::SetGlobal => {
                let (next_offset, address) = self.read_constant_address(offset)?;
                offset = next_offset;
                let value = self.get_constant_value(address)?;
                let value_str = match value {
                    ConstantValue::Number(value) => format!("{}", value),
                    ConstantValue::Boolean(value) => format!("{}", value),
                    ConstantValue::String(value) => format!("\"{}\"", value),
                };
                print_line([
                    self.prefix_data(initial_offset, offset),
                    (Some(" | ".into()), 3),
                    (Some(format!("{:?}", op_code).purple()), OP_CODE_WIDTH),
                    (Some(format!(" {:>5}", address).green()), 6),
                    (Some(" = ".into()), 3),
                    (Some(value_str.blue()), 36),
                    (Some(source_info), 0),
                ]);
            }
        };

        Ok(offset)
    }

    fn prefix_data(
        &self,
        initial_offset: usize,
        current_offset: usize,
    ) -> (Option<ColoredString>, usize) {
        let string = format!(
            "{:>4}: {:<8}",
            initial_offset,
            (initial_offset..current_offset)
                .into_iter()
                .map(|offset| format!("{:02x}", self.read_u8(offset).unwrap().1))
                .join(" ")
        );
        (Some(string.dimmed()), 14)
    }
}

fn print_line<const N: usize>(parts: [(Option<ColoredString>, usize); N]) {
    let mut out = String::new();
    for (part, width) in parts {
        write!(
            out,
            "{:<width$}",
            part.unwrap_or_else(|| "".into()),
            width = width
        )
        .unwrap();
    }
    println!("{}", out);
}

fn get_formatted_line(source: &str, op_debug: &OpDebug) -> String {
    let mut line_start_idx = 0;
    let mut line = 1;
    let mut line_end_idx = 0;

    let inner_start_offset = op_debug.inner.start().byte_offset();
    let inner_end_offset = op_debug.inner.end().byte_offset();
    let outer_start_offset = op_debug.outer.start().byte_offset();
    let outer_end_offset = op_debug.outer.end().byte_offset();

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
                if idx == inner_start_offset {
                    stage = Stage::Within;
                }
            }
            Stage::Within => {
                if ch == '\n' {
                    line_end_idx = idx;
                    break;
                }
                if idx >= inner_end_offset {
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
        "{} {}{}{}{}{}",
        format!("{:>3}:", line).dimmed(),
        &source[line_start_idx..outer_start_offset.max(line_start_idx)]
            .trim_matches('\n')
            .dimmed(),
        &source[outer_start_offset.max(line_start_idx)..inner_start_offset.max(line_start_idx)]
            .trim_matches('\n')
            .blue()
            .to_string(),
        &source[inner_start_offset.max(line_start_idx)..inner_end_offset.min(line_end_idx)]
            .trim_matches('\n')
            .green()
            .bold(),
        &source[inner_end_offset.min(line_end_idx)..outer_end_offset.min(line_end_idx)]
            .trim_matches('\n')
            .blue()
            .to_string(),
        &source[outer_end_offset.min(line_end_idx)..line_end_idx.min(line_end_idx)]
            .trim_matches('\n')
            .dimmed()
            .to_string(),
    )
}
