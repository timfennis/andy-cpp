use crate::Value;
use ndc_lexer::Span;

/// A single bytecode instruction.
#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    /// Removes the top of the stack
    Pop,
    /// Always jumps
    Jump(isize),
    /// Conditionally jumps if the top of the stack is true
    JumpIfTrue(usize),
    /// Conditionally jumps if the top of the stack is false
    JumpIfFalse(usize),
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
    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn write(&mut self, op: OpCode, span: Span) -> usize {
        self.code.push(op);
        (self.spans).push(span);
        self.code.len() - 1
    }

    pub fn patch_jump(&mut self, op_idx: usize) {
        let len = self.code.len();
        match self.code.get_mut(op_idx) {
            Some(OpCode::JumpIfFalse(offset) | OpCode::JumpIfTrue(offset)) => {
                *offset = len - op_idx - 1
            }
            Some(OpCode::Jump(offset)) => {
                *offset = isize::try_from(len - op_idx - 1).expect("usize underflow")
            }
            _ => {
                panic!("expected to backpatch JumpIfFalse")
            }
        }
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
