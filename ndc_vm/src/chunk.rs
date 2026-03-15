use crate::Value;
use ndc_lexer::Span;
use ndc_parser::CaptureSource;
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
    JumpIfTrue(isize),
    /// Conditionally jumps if the top of the stack is false
    JumpIfFalse(isize),
    /// Pushes a constant value on the stack
    Constant(usize),
    /// Reads local variable at the given slot and pushes it on the stack
    GetLocal(usize),
    /// Read a value from a parent scope
    GetUpvalue(usize),
    /// Pops the top of the stack and stores it in the given local slot
    SetLocal(usize),
    /// Pops the top of the stack and stores it in the given upvalue slot
    SetUpvalue(usize),
    /// Reads global variable at the given slot and pushes it on the stack
    GetGlobal(usize),
    /// Create a list using n arguments on the stack
    MakeList(usize),
    /// Create a tuple using n arguments on the stack
    MakeTuple(usize),
    /// Create a map from n key-value pairs on the stack. If has_default is true, an extra default value is on top of the pairs.
    MakeMap { pairs: usize, has_default: bool },
    /// Create a closure by capturing some values
    Closure {
        constant_idx: usize,
        values: Rc<[CaptureSource]>,
    },
    /// Convert top-of-stack to an iterator. No-op if already an iterator.
    GetIterator,
    /// Peek at iterator on stack (don't pop). If value: push it. If done: jump by offset.
    IterNext(isize),
    /// Pop top-of-stack, append to list at local slot.
    ListPush(usize),
    /// Pop value then key from stack, insert into map at local slot.
    MapInsert(usize),
    /// Pop end, pop start (both i64), push exclusive range iterator.
    MakeRange,
    /// Pop end, pop start (both i64), push inclusive range iterator.
    MakeRangeInclusive,
    /// Pop a tuple/list of exactly `usize` elements, push them in reverse order (first on top)
    Unpack(usize),
    /// Stop execution
    Halt,
    /// Return from function call
    Return,
    /// Close all open upvalues whose absolute stack slot is >= frame_pointer + slot.
    /// Used at the end of each loop iteration to give each iteration's closures their own copy.
    CloseUpvalue(usize),
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
            Self::GetUpvalue(n) => write!(f, "GetUpvalue({n})"),
            Self::SetUpvalue(n) => write!(f, "SetUpvalue({n})"),
            Self::MakeList(n) => write!(f, "MakeList({n})"),
            Self::MakeTuple(n) => write!(f, "MakeTuple({n})"),
            Self::MakeMap { pairs, has_default } => {
                write!(f, "MakeMap({pairs}, default={has_default})")
            }
            Self::Closure {
                constant_idx,
                values,
            } => {
                write!(f, "Closure({constant_idx}")?;
                for cap in values.iter() {
                    match cap {
                        CaptureSource::Local(n) => write!(f, ", local({n})")?,
                        CaptureSource::Upvalue(n) => write!(f, ", upvalue({n})")?,
                    }
                }
                write!(f, ")")
            }
            Self::GetIterator => write!(f, "GetIterator"),
            Self::IterNext(n) => write!(f, "IterNext({n})"),
            Self::ListPush(n) => write!(f, "ListPush({n})"),
            Self::MapInsert(n) => write!(f, "MapInsert({n})"),
            Self::MakeRange => write!(f, "MakeRange"),
            Self::MakeRangeInclusive => write!(f, "MakeRangeInclusive"),
            Self::Halt => write!(f, "Halt"),
            Self::Return => write!(f, "Return"),
            Self::Unpack(n) => write!(f, "Unpack({n})"),
            Self::CloseUpvalue(n) => write!(f, "CloseUpvalue({n})"),
        }
    }
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
        let offset =
            isize::try_from(self.code.len() - op_idx - 1).expect("jump too large to patch");
        match self.code.get_mut(op_idx) {
            Some(
                OpCode::JumpIfFalse(n)
                | OpCode::JumpIfTrue(n)
                | OpCode::Jump(n)
                | OpCode::IterNext(n),
            ) => *n = offset,
            _ => panic!("expected a patchable jump instruction at index {op_idx}"),
        }
    }

    /// Emits a `Jump` that goes back to `target` (a previously recorded chunk offset).
    pub fn write_jump_back(&mut self, target: usize, span: Span) -> usize {
        let offset =
            -isize::try_from(self.len() - target + 1).expect("loop too large to jump back");
        self.write(OpCode::Jump(offset), span)
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
