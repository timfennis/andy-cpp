/// A single bytecode instruction.
#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    Return,
}

/// A chunk of bytecode along with the constants it references.
#[derive(Debug, Default)]
pub struct Chunk {
    pub code: Vec<OpCode>,
}

impl Chunk {
    pub fn write(&mut self, op: OpCode) {
        self.code.push(op);
    }
}
