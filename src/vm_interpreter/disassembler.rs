use super::chunk::{Chunk, CodeReadError, OpCode};
use colored::Colorize;

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
        match op_code {
            OpCode::Return
            | OpCode::Negate
            | OpCode::Add
            | OpCode::Subtract
            | OpCode::Multiply
            | OpCode::Divide => {
                println!(
                    "{} | {}",
                    format!("{:04}", initial_offset).dimmed(),
                    format!("{:?}", op_code).purple(),
                )
            }
            OpCode::Constant => {
                let (next_offset, address) = self.read_constant_address(offset)?;
                offset = next_offset;
                println!(
                    "{} | {} {} = {}",
                    format!("{:04}", initial_offset).dimmed(),
                    format!("{:?}", op_code).purple(),
                    format!("{}", address).green(),
                    format!("{:?}", self.get_constant_value(address)?).blue(),
                )
            }
        }
        Ok(offset)
    }
}
