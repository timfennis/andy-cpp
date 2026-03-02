use crate::chunk::{Chunk, OpCode};

pub struct Vm {
    chunk: Chunk,
    ip: usize,
}

#[derive(thiserror::Error, Debug)]
pub enum VmError {
    #[error("runtime error")]
    RuntimeError,
}

impl Vm {
    pub fn new(chunk: Chunk) -> Self {
        Self { chunk, ip: 0 }
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        loop {
            let op = &self.chunk.code[self.ip];
            self.ip += 1;

            match op {
                OpCode::Return => return Ok(()),
            }
        }
    }
}
