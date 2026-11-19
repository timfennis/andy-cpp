use crate::native_op::NativeOp;
use std::fmt;

/// How many registers a frame can address: one per value of a [`Reg`]. The
/// VM borrows this many values of the stack from a frame's first register
/// on, so indexing them with a `Reg` needs no bounds check.
pub const WINDOW: usize = 1 << 16;

/// A register of the running frame: `r0` is the frame's first value. A
/// function's parameters are its first registers, then its other locals,
/// then the temporaries its expressions need.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Reg(pub u16);

impl Reg {
    /// The register's position in the frame's window.
    #[inline(always)]
    pub fn index(self) -> usize {
        usize::from(self.0)
    }

    /// The register `n` after this one: `r3.offset(2)` is `r5`, or `None`
    /// past the last register.
    pub fn offset(self, n: usize) -> Option<Self> {
        u16::try_from(self.index() + n).ok().map(Self)
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "r{}", self.0)
    }
}

/// One instruction of the register VM. Each names the registers it reads
/// and the one it writes: `AddInt { dst: r2, left: r0, right: r1 }` is
/// `r2 = r0 + r1`. An instruction reads all its operands before it writes
/// its destination, so the destination may be one of them.
///
/// A jump's `offset` counts instructions from the one after the jump:
/// `Jump { offset: -3 }` goes back to the instruction two before it.
///
/// Instructions that store a value somewhere else (`SetGlobal`, `Return`,
/// calls) take it out of its register and leave `None` there; the others
/// only read their operands.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Instr {
    /// `dst = constants[k]`.
    LoadK { dst: Reg, k: u32 },
    /// `dst = ()`.
    LoadUnit { dst: Reg },
    /// `dst` = a new string with the text of the constant string `k`. A
    /// string literal makes a new string each time, as strings can change.
    MakeString { dst: Reg, k: u32 },
    /// `dst = src`, which both hold afterwards.
    Copy { dst: Reg, src: Reg },
    /// `dst = src`, leaving `None` in `src`.
    Move { dst: Reg, src: Reg },
    /// `dst = globals[slot]`.
    GetGlobal { dst: Reg, slot: u32 },
    /// `globals[slot] = src`, leaving `None` in `src`.
    SetGlobal { slot: u32, src: Reg },
    /// [`NativeOp::AddInt`]: `dst = left + right`.
    AddInt { dst: Reg, left: Reg, right: Reg },
    /// [`NativeOp::SubInt`]: `dst = left - right`.
    SubInt { dst: Reg, left: Reg, right: Reg },
    /// [`NativeOp::MulInt`]: `dst = left * right`.
    MulInt { dst: Reg, left: Reg, right: Reg },
    /// [`NativeOp::AddFloat`]: `dst = left + right`.
    AddFloat { dst: Reg, left: Reg, right: Reg },
    /// [`NativeOp::SubFloat`]: `dst = left - right`.
    SubFloat { dst: Reg, left: Reg, right: Reg },
    /// [`NativeOp::MulFloat`]: `dst = left * right`.
    MulFloat { dst: Reg, left: Reg, right: Reg },
    /// [`NativeOp::DivFloat`]: `dst = left / right`.
    DivFloat { dst: Reg, left: Reg, right: Reg },
    /// [`NativeOp::Less`]: `dst = left < right`.
    Less { dst: Reg, left: Reg, right: Reg },
    /// [`NativeOp::LessEqual`]: `dst = left <= right`.
    LessEqual { dst: Reg, left: Reg, right: Reg },
    /// [`NativeOp::Greater`]: `dst = left > right`.
    Greater { dst: Reg, left: Reg, right: Reg },
    /// [`NativeOp::GreaterEqual`]: `dst = left >= right`.
    GreaterEqual { dst: Reg, left: Reg, right: Reg },
    /// [`NativeOp::Equal`]: `dst = left == right`.
    Equal { dst: Reg, left: Reg, right: Reg },
    /// [`NativeOp::NotEqual`]: `dst = left != right`.
    NotEqual { dst: Reg, left: Reg, right: Reg },
    /// [`NativeOp::NegInt`]: `dst = -src`.
    NegInt { dst: Reg, src: Reg },
    /// [`NativeOp::NegFloat`]: `dst = -src`.
    NegFloat { dst: Reg, src: Reg },
    /// [`NativeOp::Not`]: `dst = !src`.
    Not { dst: Reg, src: Reg },
    /// Goes `offset` instructions on.
    Jump { offset: i32 },
    /// Goes `offset` instructions on if `cond` is `false`, and fails if it
    /// isn't a `Bool`.
    JumpIfFalse { cond: Reg, offset: i32 },
    /// Calls the function in `base` with the `args` values after it, and
    /// puts what it returns in `base`. The callee's frame starts at
    /// `base + 1`, so the arguments are its first registers.
    Call { base: Reg, args: u16 },
    /// Calls the native in global slot `slot` with the `args` values from
    /// `base` on, for a call the analyser resolved to that native, and puts
    /// what it returns in `dst`. The argument registers are `None` after.
    CallNative {
        dst: Reg,
        base: Reg,
        slot: u16,
        args: u8,
    },
    /// Ends the call, giving the caller the value in `src`.
    Return { src: Reg },
    /// Ends the program, whose value is the one in `src`.
    Halt { src: Reg },
}

impl Instr {
    /// The instruction that does the binary `op` of `left` and `right` into
    /// `dst`: `SubInt` for `NativeOp::SubInt`.
    pub fn binary(op: NativeOp, dst: Reg, left: Reg, right: Reg) -> Self {
        match op {
            NativeOp::AddInt => Self::AddInt { dst, left, right },
            NativeOp::SubInt => Self::SubInt { dst, left, right },
            NativeOp::MulInt => Self::MulInt { dst, left, right },
            NativeOp::AddFloat => Self::AddFloat { dst, left, right },
            NativeOp::SubFloat => Self::SubFloat { dst, left, right },
            NativeOp::MulFloat => Self::MulFloat { dst, left, right },
            NativeOp::DivFloat => Self::DivFloat { dst, left, right },
            NativeOp::Less => Self::Less { dst, left, right },
            NativeOp::LessEqual => Self::LessEqual { dst, left, right },
            NativeOp::Greater => Self::Greater { dst, left, right },
            NativeOp::GreaterEqual => Self::GreaterEqual { dst, left, right },
            NativeOp::Equal => Self::Equal { dst, left, right },
            NativeOp::NotEqual => Self::NotEqual { dst, left, right },
            NativeOp::NegInt | NativeOp::NegFloat | NativeOp::Not => {
                unreachable!("internal error: {op:?} takes one operand")
            }
        }
    }

    /// The instruction that does the unary `op` of `src` into `dst`:
    /// `NegInt` for `NativeOp::NegInt`.
    pub fn unary(op: NativeOp, dst: Reg, src: Reg) -> Self {
        match op {
            NativeOp::NegInt => Self::NegInt { dst, src },
            NativeOp::NegFloat => Self::NegFloat { dst, src },
            NativeOp::Not => Self::Not { dst, src },
            _ => unreachable!("internal error: {op:?} takes two operands"),
        }
    }

    /// The variant's name, as the disassembler prints it: `"AddInt"`.
    pub fn name(self) -> &'static str {
        match self {
            Self::LoadK { .. } => "LoadK",
            Self::LoadUnit { .. } => "LoadUnit",
            Self::MakeString { .. } => "MakeString",
            Self::Copy { .. } => "Copy",
            Self::Move { .. } => "Move",
            Self::GetGlobal { .. } => "GetGlobal",
            Self::SetGlobal { .. } => "SetGlobal",
            Self::AddInt { .. } => "AddInt",
            Self::SubInt { .. } => "SubInt",
            Self::MulInt { .. } => "MulInt",
            Self::AddFloat { .. } => "AddFloat",
            Self::SubFloat { .. } => "SubFloat",
            Self::MulFloat { .. } => "MulFloat",
            Self::DivFloat { .. } => "DivFloat",
            Self::Less { .. } => "Less",
            Self::LessEqual { .. } => "LessEqual",
            Self::Greater { .. } => "Greater",
            Self::GreaterEqual { .. } => "GreaterEqual",
            Self::Equal { .. } => "Equal",
            Self::NotEqual { .. } => "NotEqual",
            Self::NegInt { .. } => "NegInt",
            Self::NegFloat { .. } => "NegFloat",
            Self::Not { .. } => "Not",
            Self::Jump { .. } => "Jump",
            Self::JumpIfFalse { .. } => "JumpIfFalse",
            Self::Call { .. } => "Call",
            Self::CallNative { .. } => "CallNative",
            Self::Return { .. } => "Return",
            Self::Halt { .. } => "Halt",
        }
    }

    /// The jump's offset, for an instruction that jumps.
    pub fn jump_offset(self) -> Option<i32> {
        match self {
            Self::Jump { offset } | Self::JumpIfFalse { offset, .. } => Some(offset),
            _ => None,
        }
    }

    /// The same jump going `offset` instructions on. Panics for an
    /// instruction that doesn't jump.
    pub(crate) fn with_jump_offset(self, offset: i32) -> Self {
        match self {
            Self::Jump { .. } => Self::Jump { offset },
            Self::JumpIfFalse { cond, .. } => Self::JumpIfFalse { cond, offset },
            other => panic!("internal error: {} doesn't jump", other.name()),
        }
    }
}

/// The operands as the disassembler prints them, without the name:
/// `r2, r0, r1` for `AddInt { dst: r2, left: r0, right: r1 }`. A
/// constant is `k3`, a global `g12`, a native `n25`, and a jump's
/// offset is signed: `+4`.
impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::LoadK { dst, k } | Self::MakeString { dst, k } => write!(f, "{dst}, k{k}"),
            Self::LoadUnit { dst } => write!(f, "{dst}"),
            Self::Copy { dst, src }
            | Self::Move { dst, src }
            | Self::NegInt { dst, src }
            | Self::NegFloat { dst, src }
            | Self::Not { dst, src } => write!(f, "{dst}, {src}"),
            Self::GetGlobal { dst, slot } => write!(f, "{dst}, g{slot}"),
            Self::SetGlobal { slot, src } => write!(f, "g{slot}, {src}"),
            Self::AddInt { dst, left, right }
            | Self::SubInt { dst, left, right }
            | Self::MulInt { dst, left, right }
            | Self::AddFloat { dst, left, right }
            | Self::SubFloat { dst, left, right }
            | Self::MulFloat { dst, left, right }
            | Self::DivFloat { dst, left, right }
            | Self::Less { dst, left, right }
            | Self::LessEqual { dst, left, right }
            | Self::Greater { dst, left, right }
            | Self::GreaterEqual { dst, left, right }
            | Self::Equal { dst, left, right }
            | Self::NotEqual { dst, left, right } => write!(f, "{dst}, {left}, {right}"),
            Self::Jump { offset } => write!(f, "{offset:+}"),
            Self::JumpIfFalse { cond, offset } => write!(f, "{cond}, {offset:+}"),
            Self::Call { base, args } => write!(f, "{base}, {args}"),
            Self::CallNative {
                dst,
                base,
                slot,
                args,
            } => write!(f, "{dst}, n{slot}, {base}, {args}"),
            Self::Return { src } | Self::Halt { src } => write!(f, "{src}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_instruction_is_8_bytes() {
        assert_eq!(size_of::<Instr>(), 8);
    }

    #[test]
    fn operands_print_with_register_names() {
        let add = Instr::AddInt {
            dst: Reg(2),
            left: Reg(0),
            right: Reg(1),
        };
        assert_eq!(format!("{} {add}", add.name()), "AddInt r2, r0, r1");
        let jump = Instr::JumpIfFalse {
            cond: Reg(3),
            offset: -4,
        };
        assert_eq!(jump.to_string(), "r3, -4");
    }
}
