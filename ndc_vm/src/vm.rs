use crate::Value;
use crate::chunk::{Chunk, OpCode};
use ndc_core::num::Number;

pub struct Vm {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
}

#[derive(thiserror::Error, Debug)]
pub enum VmError {
    #[error("runtime error")]
    RuntimeError,
}

impl Vm {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: Vec::default(),
        }
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        eprintln!("[DBG] Value bytes: {}", size_of::<Value>());
        eprintln!("[DBG] Number bytes: {}", size_of::<Number>());

        if self.chunk.is_empty() {
            return Ok(());
        }

        loop {
            let op = self.chunk.opcode(self.ip);
            self.ip += 1;

            match op {
                OpCode::Return => {
                    println!("{:?}", self.stack.pop().expect("stack underflow"));
                }
                OpCode::Constant(idx) => {
                    // TODO: assuming constants can be referenced multiple times we'll have to clone here
                    self.stack.push(self.chunk.constant(*idx).clone());
                }
            }
        }
    }
}
