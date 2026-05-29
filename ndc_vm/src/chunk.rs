use crate::Value;
use ahash::AHashSet;
use ndc_core::hash_map::HashMap;
use ndc_lexer::Span;
use ndc_parser::CaptureSource;
use std::rc::Rc;

/// A signed displacement applied to the instruction pointer to perform a jump.
///
/// Wraps `isize` so the contract — "relative offset, in instructions, from the
/// instruction *after* the jump opcode" — has a single definition site. Future
/// stages of a compile pipeline may swap this for an `enum JumpTarget { … }`
/// without touching every call site.

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum JumpTarget {
    Offset(isize),
    Label(LabelId),
}

impl JumpTarget {
    pub const PLACEHOLDER: Self = Self::Label(LabelId(usize::MAX));

    #[inline]
    pub fn new(offset: isize) -> Self {
        Self::Offset(offset)
    }

    #[inline]
    pub fn raw(self) -> isize {
        match self {
            JumpTarget::Offset(i) => i,
            JumpTarget::Label(_) => panic!("cannot get raw offset of unresolved label"),
        }
    }

    /// Advance `ip` by this offset using wrapping signed arithmetic.
    #[inline]
    pub fn apply(self, ip: usize) -> usize {
        match self {
            JumpTarget::Offset(o) => ip.wrapping_add_signed(o),
            JumpTarget::Label(_) => panic!("cannot apply instruction pointer to unresolved label"),
        }
    }
}

impl std::fmt::Debug for JumpTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JumpTarget::Offset(offset) => std::fmt::Debug::fmt(&offset, f),
            JumpTarget::Label(label_id) => std::fmt::Debug::fmt(&label_id.0, f),
        }
    }
}

/// A single bytecode instruction.
///
/// ## Stack effects
///
/// Each instruction documents its net stack effect as `[before → after]`.
/// Variable-size operands use `n` for the instruction's argument.
///
/// | Instruction   | Stack effect                            | Notes                                      |
/// |---------------|-----------------------------------------|--------------------------------------------|
/// | `Constant`    | `[… → … value]`                        | +1                                         |
/// | `Pop`         | `[… value → …]`                        | −1                                         |
/// | `GetLocal`    | `[… → … value]`                        | +1 (copies from slot)                      |
/// | `GetUpvalue`  | `[… → … value]`                        | +1 (reads upvalue cell)                    |
/// | `GetGlobal`   | `[… → … value]`                        | +1 (copies from globals)                   |
/// | `SetLocal`    | `[… value → …]`                        | −1 (pops, writes to slot†)                 |
/// | `SetUpvalue`  | `[… value → …]`                        | −1 (pops, writes to upvalue cell)          |
/// | `Call`        | `[… callee a1…an → … result]`          | −n (pops callee + args, pushes result)     |
/// | `Return`      | `[… retval → …]`                       | pops retval, truncates frame, pushes retval|
/// | `Halt`        | `[…]`                                   | terminates execution                       |
/// | `Jump`        | `[…]`                                   | 0 (unconditional jump)                     |
/// | `JumpIfTrue`  | `[… bool → … bool]`                    | 0 (peeks, jumps if true)                   |
/// | `JumpIfFalse` | `[… bool → … bool]`                    | 0 (peeks, jumps if false)                  |
/// | `MakeList`    | `[… v1…vn → … list]`                   | −(n−1)                                     |
/// | `MakeTuple`   | `[… v1…vn → … tuple]`                  | −(n−1)                                     |
/// | `MakeMap`     | `[… k1 v1…kn vn (default?) → … map]`   | −(2n−1) or −2n if `has_default`              |
/// | `MakeRange`   | `[… start (end?) → … iter]`            | −1 if bounded, 0 if unbounded              |
/// | `Closure`     | `[… → … closure]`                      | +1                                         |
/// | `GetIterator` | `[… value → … iter]`                   | 0 (pops value, pushes iterator)            |
/// | `IterNext`    | `[… iter → … iter value]` or jump      | +1 if has next, 0 + jump if exhausted      |
/// | `ListPush`    | `[… value → …]`                        | −1 (pops value, mutates list in slot)      |
/// | `MapInsert`   | `[… key value → …]`                    | −2 (pops both, mutates map in slot)        |
/// | `Unpack`      | `[… compound → … v1…vn]`               | +(n−1) (pops 1, pushes n)                  |
/// | `CloseUpvalue`| `[…]`                                   | 0 (closes upvalue cells, no stack change)  |
/// | `Memoize`     | `[… fn → … memoized_fn]`               | 0 (pops and pushes)                        |
///
/// † `SetLocal` for a **declaration** (slot == stack top) is effectively a no-op on the
///   stack: it pops then immediately pushes to extend. For a **reassignment** (slot < top)
///   it truly shrinks the stack by 1.
// NOTE: OpCode cannot be Copy because the Closure variant holds Rc<[CaptureSource]>.
// The dispatch loop accesses opcodes by reference to avoid cloning the 32-byte enum on
// every iteration; see Vm::run_to_depth.
#[derive(Clone, PartialEq, Eq)]
pub enum OpCode {
    /// Pops callee and `n` arguments, pushes result. `[… callee a1…an → … result]`
    Call(usize),
    /// Vec-dispatches the callee on top of the stack across tuple arguments.
    /// The callee is a single scalar `Function` (loaded directly, not wrapped
    /// in an `OverloadSet`): the analyser pinned it at compile time.
    /// `[… callee a1…an → … tuple]` where each `ai` is either a tuple of the
    /// shared axis length or a scalar that broadcasts unchanged.
    CallVec(usize),
    /// Pops top of stack. `[… value → …]`
    Pop,
    /// Unconditional jump. `[…] → […]`
    Jump(JumpTarget),
    /// Peeks top; jumps if true. `[… bool → … bool]`
    JumpIfTrue(JumpTarget),
    /// Peeks top; jumps if false. `[… bool → … bool]`
    JumpIfFalse(JumpTarget),
    /// Pushes a constant. `[… → … value]`
    Constant(usize),
    /// Copies local slot onto stack. `[… → … value]`
    GetLocal(usize),
    /// Reads upvalue cell onto stack. `[… → … value]`
    GetUpvalue(usize),
    /// Pops top and writes to local slot. `[… value → …]`
    SetLocal(usize),
    /// Pops top and writes to upvalue cell. `[… value → …]`
    SetUpvalue(usize),
    /// Copies global slot onto stack. `[… → … value]`
    GetGlobal(usize),
    /// Pops `n` values, pushes a list. `[… v1…vn → … list]`
    MakeList(usize),
    /// Pops `n` values, pushes a tuple. `[… v1…vn → … tuple]`
    MakeTuple(usize),
    /// Pops `2n` values (+ optional default), pushes a map.
    MakeMap { pairs: usize, has_default: bool },
    /// Pushes a closure, capturing values from locals/upvalues. `[… → … closure]`
    Closure {
        constant_idx: usize,
        values: Rc<[CaptureSource]>,
    },
    /// Pops value, pushes iterator. No-op if already an iterator. `[… value → … iter]`
    GetIterator,
    /// Peeks iterator; pushes next value or jumps if exhausted. `[… iter → … iter value]`
    IterNext(JumpTarget),
    /// Pops value, appends to list at local slot. `[… value → …]`
    ListPush(usize),
    /// Pops value then key, inserts into map at local slot. `[… key value → …]`
    MapInsert(usize),
    /// Pops start (and end if bounded), pushes range iterator.
    MakeRange { inclusive: bool, bounded: bool },
    /// Pops a compound value, pushes `n` elements. `[… compound → … v1…vn]`
    Unpack(usize),
    /// Terminates execution.
    Halt,
    /// Returns from function call. Pops return value, truncates frame, pushes return value.
    Return,
    /// Closes open upvalues at or above `frame_pointer + slot`. No stack change.
    CloseUpvalue(usize),
    /// Pops function, pushes memoized wrapper. `[… fn → … memoized_fn]`
    Memoize,
}

impl std::fmt::Debug for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Call(n) => write!(f, "Call({n})"),
            Self::CallVec(n) => write!(f, "CallVec({n})"),
            Self::Pop => write!(f, "Pop"),
            Self::Jump(n) => write!(f, "Jump({n:?})"),
            Self::JumpIfTrue(n) => write!(f, "JumpIfTrue({n:?})"),
            Self::JumpIfFalse(n) => write!(f, "JumpIfFalse({n:?})"),
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
            Self::IterNext(n) => write!(f, "IterNext({n:?})"),
            Self::ListPush(n) => write!(f, "ListPush({n})"),
            Self::MapInsert(n) => write!(f, "MapInsert({n})"),
            Self::MakeRange { inclusive, bounded } => {
                write!(f, "MakeRange(inclusive={inclusive}, bounded={bounded})")
            }
            Self::Halt => write!(f, "Halt"),
            Self::Return => write!(f, "Return"),
            Self::Unpack(n) => write!(f, "Unpack({n})"),
            Self::CloseUpvalue(n) => write!(f, "CloseUpvalue({n})"),
            Self::Memoize => write!(f, "Memoize"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LabelId(usize);

#[derive(Default, Clone)]
pub(crate) struct OptimizerIr {
    constants: Vec<Value>,
    code: Vec<(LabelId, OpCode, Span)>,
}

impl OptimizerIr {
    pub(crate) fn len(&self) -> usize {
        self.code.len()
    }

    pub(crate) fn next_label(&self) -> LabelId {
        LabelId(self.len())
    }

    pub(crate) fn into_chunk(self) -> Chunk {
        let label_to_pos: HashMap<LabelId, usize> = self
            .code
            .iter()
            .enumerate()
            .map(|(idx, (a, _, _))| (*a, idx))
            .collect();

        let mut op_codes = Vec::with_capacity(self.code.len());
        let mut spans = Vec::with_capacity(self.code.len());

        // Jump offsets are relative to the instruction *after* the jump: the
        // VM dispatch loop increments `ip` before calling `apply`. So the
        // offset is `dest - (cur + 1)`.
        let resolve = |cur_label: LabelId, dest_label: LabelId| -> JumpTarget {
            let cur = label_to_pos[&cur_label] as isize;
            let dest = label_to_pos[&dest_label] as isize;
            JumpTarget::Offset(dest - cur - 1)
        };

        for (cur_label, opcode, span) in self.code.into_iter() {
            spans.push(span);
            let lowered = match opcode {
                OpCode::Jump(JumpTarget::Label(d)) => OpCode::Jump(resolve(cur_label, d)),
                OpCode::JumpIfFalse(JumpTarget::Label(d)) => {
                    OpCode::JumpIfFalse(resolve(cur_label, d))
                }
                OpCode::JumpIfTrue(JumpTarget::Label(d)) => {
                    OpCode::JumpIfTrue(resolve(cur_label, d))
                }
                OpCode::IterNext(JumpTarget::Label(d)) => OpCode::IterNext(resolve(cur_label, d)),
                op => op,
            };
            op_codes.push(lowered);
        }

        Chunk {
            constants: self.constants,
            code: op_codes,
            spans,
        }
    }

    pub(crate) fn peephole(&mut self) {
        let targets: AHashSet<LabelId> = self
            .code
            .iter()
            .filter_map(|(_, op, _)| match op {
                OpCode::Jump(JumpTarget::Label(l))
                | OpCode::JumpIfFalse(JumpTarget::Label(l))
                | OpCode::JumpIfTrue(JumpTarget::Label(l))
                | OpCode::IterNext(JumpTarget::Label(l)) => Some(*l),
                _ => None,
            })
            .collect();

        loop {
            let mut out = Vec::with_capacity(self.code.len());
            let mut i = 0;
            let mut removed = 0;
            while i < self.code.len() {
                let elide = i + 1 < self.code.len()
                    && matches!(
                        self.code[i].1,
                        OpCode::Constant(_) | OpCode::GetGlobal(_) | OpCode::GetLocal(_)
                    )
                    && matches!(self.code[i + 1].1, OpCode::Pop)
                    && !targets.contains(&self.code[i].0)
                    && !targets.contains(&self.code[i + 1].0);
                if elide {
                    i += 2;
                    removed += 2;
                } else {
                    out.push(self.code[i].clone());
                    i += 1;
                }
            }

            self.code = out;

            if removed == 0 {
                break;
            }
        }
    }

    pub(crate) fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    /// Overwrites the jump operand of a `Jump`, `JumpIfTrue`, `JumpIfFalse`, or
    /// `IterNext` already written at `idx`. Panics on any other opcode.
    pub(crate) fn set_jump_offset(&mut self, label: LabelId, target: JumpTarget) {
        let idx = self
            .code
            .iter()
            .position(|(l, _, _)| *l == label)
            .expect("invalid label");

        match self.code.get_mut(idx) {
            Some((
                _label,
                OpCode::JumpIfFalse(n)
                | OpCode::JumpIfTrue(n)
                | OpCode::Jump(n)
                | OpCode::IterNext(n),
                _span,
            )) => *n = target,
            _ => panic!("expected a patchable jump instruction at index {idx}"),
        }
    }

    pub(crate) fn write(&mut self, op: OpCode, span: Span) -> LabelId {
        let label = self.next_label();
        self.code.push((label, op, span));
        label
    }
}

/// A chunk of bytecode along with the constants it references.
#[derive(Default, Clone)]
pub struct Chunk {
    constants: Vec<Value>,
    code: Vec<OpCode>,
    spans: Vec<Span>,
}

impl Chunk {
    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn write(&mut self, op: OpCode, span: Span) -> usize {
        self.code.push(op);
        self.spans.push(span);
        self.code.len() - 1
    }

    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }
    #[inline(always)]
    pub fn opcode(&self, idx: usize) -> &OpCode {
        &self.code[idx]
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
    pub fn iter(&self) -> impl Iterator<Item = (usize, &OpCode, Option<&Value>)> {
        self.code.iter().enumerate().map(|(i, op)| {
            let val = match op {
                OpCode::Constant(idx)
                | OpCode::Closure {
                    constant_idx: idx, ..
                } => Some(&self.constants[*idx]),
                _ => None,
            };
            (i, op, val)
        })
    }
}
