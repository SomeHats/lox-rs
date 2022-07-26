mod chunk;
mod disassembler;
mod value;
mod vm;

use chunk::*;
use miette::Result;
use vm::Vm;

pub fn main() -> Result<()> {
    let mut chunk = Chunk::new(vec![], None);

    chunk.write_constant(1.2, None);
    chunk.write_constant(3.4, None);
    chunk.write_basic_op(OpCode::Add, None);
    chunk.write_basic_op(OpCode::Negate, None);
    chunk.write_basic_op(OpCode::Return, None);

    // chunk.disassemble("test");
    Vm::new(chunk).run()?;

    Ok(())
}
