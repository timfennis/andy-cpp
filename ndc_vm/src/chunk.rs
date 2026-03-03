use crate::Value;
use ndc_lexer::Span;

/// A single bytecode instruction.
#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    /// Pushes a constant value on the stack
    Constant(usize),
    /// Reads local variable at the given slot and pushes it on the stack
    GetLocal(usize),
    /// Pops the top of the stack and stores it in the given local slot
    SetLocal(usize),
    /// Stop execution
    Halt,
    Return,
}

/// A chunk of bytecode along with the constants it references.
#[derive(Debug, Default)]
pub struct Chunk {
    constants: Vec<Value>,
    code: Vec<OpCode>,
    spans: Vec<Span>,
}

impl Chunk {
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn write(&mut self, op: OpCode, span: Span) {
        self.code.push(op);
        (self.spans).push(span);
    }

    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }
    #[inline(always)]
    pub fn opcode(&self, idx: usize) -> &OpCode {
        &self.code[idx]
    }

    pub fn constant(&self, idx: usize) -> &Value {
        &self.constants[idx]
    }
}
