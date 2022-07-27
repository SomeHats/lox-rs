use super::chunk::{Chunk, CodeReadError, OpCode, OpDebug};
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
        let source_info = self
            .read_op_debug(initial_offset)
            .map(|op_debug| {
                Colorize::clear(get_formatted_line(self.source().str(), op_debug).as_str())
            })
            .unwrap();

        match op_code {
            OpCode::Return
            | OpCode::Negate
            | OpCode::Add
            | OpCode::Subtract
            | OpCode::Multiply
            | OpCode::Divide
            | OpCode::Print
            | OpCode::Pop => {
                print_line([
                    (Some(format!("{:>4}", initial_offset).dimmed()), 4),
                    (Some(" | ".into()), 3),
                    (Some(format!("{:?}", op_code).purple()), 50),
                    (Some(source_info), 0),
                ]);
            }
            OpCode::Constant => {
                let (next_offset, address) = self.read_constant_address(offset)?;
                offset = next_offset;
                print_line([
                    (Some(format!("{:>4}", initial_offset).dimmed()), 4),
                    (Some(" | ".into()), 3),
                    (Some(format!("{:?}", op_code).purple()), 10),
                    (Some(format!(" {:>4}", address).green()), 5),
                    (Some(" = ".into()), 3),
                    (
                        Some(format!("{:?}", self.get_constant_value(address)?).blue()),
                        32,
                    ),
                    (Some(source_info), 0),
                ]);
            }
        };

        Ok(offset)
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
    println!("{}", out.on_black());
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
        "{} {}{}{}",
        format!("{:>3}:", line).dimmed(),
        if outer_start_offset <= line_start_idx {
            source[line_start_idx..inner_start_offset]
                .trim_start_matches('\n')
                .blue()
                .to_string()
        } else {
            format!(
                "{}{}",
                &source[line_start_idx..outer_start_offset]
                    .trim_start_matches('\n')
                    .dimmed(),
                &source[outer_start_offset..inner_start_offset]
                    .blue()
                    .to_string(),
            )
        },
        &source[inner_start_offset..inner_end_offset].green().bold(),
        if outer_end_offset >= line_end_idx {
            source[inner_end_offset..line_end_idx].blue().to_string()
        } else {
            format!(
                "{}{}",
                &source[inner_end_offset..outer_end_offset]
                    .blue()
                    .to_string(),
                &source[outer_end_offset..line_end_idx].dimmed().to_string(),
            )
        },
    )
}
