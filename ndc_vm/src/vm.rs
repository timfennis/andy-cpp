use crate::Value;
use crate::chunk::{Chunk, OpCode};
use ndc_core::num::Number;

pub struct Vm {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    locals: Vec<Value>,
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
            locals: Vec::default(),
        }
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        eprintln!("[VM] Value bytes: {}", size_of::<Value>());
        eprintln!("[VM] OpCode bytes: {}", size_of::<OpCode>());

        if self.chunk.is_empty() {
            return Ok(());
        }

        loop {
            let op = self.chunk.opcode(self.ip);
            self.ip += 1;

            eprintln!("[VM] Running: {:?}", op);

            match op {
                OpCode::Halt => {
                    eprintln!("[VM] stack-dump\n{:?}", self.stack);
                    eprintln!("[VM] locals-dump\n{:?}", self.locals);
                    return Ok(());
                }
                OpCode::Return => {
                    println!("{:?}", self.stack.pop().expect("stack underflow"));
                }
                OpCode::Constant(idx) => {
                    self.stack.push(self.chunk.constant(*idx).clone());
                }
                OpCode::GetLocal(slot) => {
                    self.stack.push(self.locals[*slot].clone());
                }
                OpCode::SetLocal(slot) => {
                    let value = self.stack.pop().expect("stack underflow");
                    if *slot < self.locals.len() {
                        self.locals[*slot] = value;
                    } else {
                        self.locals.resize(*slot, Value::None);
                        self.locals.push(value);
                    }
                }
            }
        }
    }
}
