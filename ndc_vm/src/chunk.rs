use crate::Value;
use ndc_lexer::Span;
use std::rc::Rc;

/// A single bytecode instruction.
// NOTE: For now we just derive Copy for OpCode since it makes our live easier and it probably won't cost THAT much performance. In the future we might want to do some proper byte packing and dive into unsafe land to optimize further.
#[derive(Clone, PartialEq, Eq)]
pub enum OpCode {
    /// Call the function with `usize` arguments
    Call(usize),
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
    /// Read a value from a parent scope
    GetUpvalue { slot: usize, depth: usize },
    /// Pops the top of the stack and stores it in the given local slot
    SetLocal(usize),
    /// Reads global variable at the given slot and pushes it on the stack
    GetGlobal(usize),
    /// Create a list using n arguments on the stack
    MakeList(usize),
    /// Create a tuple using n arguments on the stack
    MakeTuple(usize),
    /// Create a closure by capturing some values
    Closure {
        constant_idx: usize,
        values: Rc<[CaptureFrom]>,
    },
    /// Stop execution
    Halt,
    /// Return from function call
    Return,
}

impl std::fmt::Debug for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Call(n) => write!(f, "Call({n})"),
            Self::Pop => write!(f, "Pop"),
            Self::Jump(n) => write!(f, "Jump({n})"),
            Self::JumpIfTrue(n) => write!(f, "JumpIfTrue({n})"),
            Self::JumpIfFalse(n) => write!(f, "JumpIfFalse({n})"),
            Self::Constant(n) => write!(f, "Constant({n})"),
            Self::GetLocal(n) => write!(f, "GetLocal({n})"),
            Self::SetLocal(n) => write!(f, "SetLocal({n})"),
            Self::GetGlobal(n) => write!(f, "GetGlobal({n})"),
            Self::GetUpvalue { slot, depth } => write!(f, "GetUpvalue({slot}, {depth})"),
            Self::MakeList(n) => write!(f, "MakeList({n})"),
            Self::MakeTuple(n) => write!(f, "MakeTuple({n})"),
            Self::Closure {
                constant_idx,
                values,
            } => {
                write!(f, "Closure({constant_idx}")?;
                for cap in values.iter() {
                    match cap {
                        CaptureFrom::Local(n) => write!(f, ", local({n})")?,
                        CaptureFrom::Upvalue(n) => write!(f, ", upvalue({n})")?,
                    }
                }
                write!(f, ")")
            }
            Self::Halt => write!(f, "Halt"),
            Self::Return => write!(f, "Return"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CaptureFrom {
    Local(usize),
    Upvalue(usize),
}

/// A chunk of bytecode along with the constants it references.
#[derive(Default)]
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

    pub fn capture_upvalues(&mut self) -> Vec<CaptureFrom> {
        let mut upvalues = vec![];
        for op in &mut self.code {
            let OpCode::GetUpvalue { slot, depth } = op else {
                continue;
            };

            // Create a capture
            let capture = if *depth == 1 {
                CaptureFrom::Local(*slot)
            } else {
                CaptureFrom::Upvalue(*slot)
            };

            // If an equal capture already exists we just use that index instead, otherwise we add it to the capture list
            let idx = upvalues
                .iter()
                .position(|c| c == &capture)
                .unwrap_or_else(|| {
                    upvalues.push(capture);
                    upvalues.len() - 1
                });

            *depth = 0;
            *slot = idx;
        }
        upvalues
    }

    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }
    #[inline(always)]
    pub fn opcode(&self, idx: usize) -> OpCode {
        self.code[idx].clone()
    }

    pub fn opcodes(&self) -> &[OpCode] {
        &self.code
    }

    pub fn constant(&self, idx: usize) -> &Value {
        &self.constants[idx]
    }

    pub fn span(&self, ip: usize) -> Span {
        self.spans[ip]
    }

    /// Iterates opcodes as `(index, opcode, constant_value)` where `constant_value`
    /// is `Some` for `Constant(idx)` and `Closure(idx)` opcodes.
    pub fn iter(&self) -> impl Iterator<Item = (usize, OpCode, Option<&Value>)> {
        self.code.iter().cloned().enumerate().map(|(i, op)| {
            let val = match op {
                OpCode::Constant(idx)
                | OpCode::Closure {
                    constant_idx: idx, ..
                } => Some(&self.constants[idx]),
                _ => None,
            };
            (i, op, val)
        })
    }
}
