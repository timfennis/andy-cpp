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
    /// `dst` = the value of the local in register `src`, which closures
    /// capture: what its cell holds once a closure captured it, and before
    /// that, what the register holds.
    GetCell { dst: Reg, src: Reg },
    /// Writes `src` to the local in register `cell`, which closures capture:
    /// into its cell once a closure captured it, and before that, into the
    /// register. Leaves `None` in `src`.
    SetCell { cell: Reg, src: Reg },
    /// `reg` = a new cell holding unit, for a function that captures itself
    /// and needs its cell before its value exists.
    NewCell { reg: Reg },
    /// `dst` = what the running closure's captured variable `index` holds.
    GetUpvalue { dst: Reg, index: u16 },
    /// Writes `src` to the running closure's captured variable `index`,
    /// leaving `None` in `src`.
    SetUpvalue { index: u16, src: Reg },
    /// `dst` = a closure of the chunk's closure `index`: its function, over
    /// what it captures. A local it captures moves into a cell on its first
    /// capture, which takes its place in the register.
    Closure { dst: Reg, index: u32 },
    /// Puts the function in `reg` in a memoising wrapper with a new cache:
    /// a `pure fn`.
    Memoize { reg: Reg },
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
    /// [`NativeOp::AddInt`] of a register and an `Int` written in the code:
    /// `dst = left + imm`, as in `i + 1`.
    AddIntI { dst: Reg, left: Reg, imm: i16 },
    /// [`NativeOp::SubInt`]: `dst = left - imm`, as in `n - 2`.
    SubIntI { dst: Reg, left: Reg, imm: i16 },
    /// [`NativeOp::MulInt`]: `dst = left * imm`, as in `i * 2`.
    MulIntI { dst: Reg, left: Reg, imm: i16 },
    /// Goes `offset` instructions on.
    Jump { offset: i32 },
    /// Goes `offset` instructions on if `cond` is `false`, and fails if it
    /// isn't a `Bool`.
    JumpIfFalse { cond: Reg, offset: i32 },
    /// Goes `offset` instructions on unless `left < right`: the condition
    /// `i < n` of a loop, as `Less` then `JumpIfFalse` would, without making
    /// a `Bool`. An offset of [`FAR`] is in the chunk's far jumps.
    JumpIfNotLess { left: Reg, right: Reg, offset: i16 },
    /// As [`Self::JumpIfNotLess`], unless `left <= right`.
    JumpIfNotLessEqual { left: Reg, right: Reg, offset: i16 },
    /// As [`Self::JumpIfNotLess`], unless `left > right`.
    JumpIfNotGreater { left: Reg, right: Reg, offset: i16 },
    /// As [`Self::JumpIfNotLess`], unless `left >= right`.
    JumpIfNotGreaterEqual { left: Reg, right: Reg, offset: i16 },
    /// As [`Self::JumpIfNotLess`], unless `left == right`.
    JumpIfNotEqual { left: Reg, right: Reg, offset: i16 },
    /// As [`Self::JumpIfNotLess`], unless `left != right`.
    JumpIfNotNotEqual { left: Reg, right: Reg, offset: i16 },
    /// As [`Self::JumpIfNotLess`] with an `Int` written in the code on the
    /// right: unless `left < imm`, as in `n < 10`.
    JumpIfNotLessI { left: Reg, imm: i16, offset: i16 },
    /// As [`Self::JumpIfNotLessI`], unless `left <= imm`.
    JumpIfNotLessEqualI { left: Reg, imm: i16, offset: i16 },
    /// As [`Self::JumpIfNotLessI`], unless `left > imm`.
    JumpIfNotGreaterI { left: Reg, imm: i16, offset: i16 },
    /// As [`Self::JumpIfNotLessI`], unless `left >= imm`.
    JumpIfNotGreaterEqualI { left: Reg, imm: i16, offset: i16 },
    /// As [`Self::JumpIfNotLessI`], unless `left == imm`.
    JumpIfNotEqualI { left: Reg, imm: i16, offset: i16 },
    /// As [`Self::JumpIfNotLessI`], unless `left != imm`.
    JumpIfNotNotEqualI { left: Reg, imm: i16, offset: i16 },
    /// Calls the function in `base` with the `args` values after it, and
    /// puts what it returns in `base`. The callee's frame starts at
    /// `base + 1`, so the arguments are its first registers.
    Call { base: Reg, args: u16 },
    /// Calls the function in `base`, an operator, with the `args` values
    /// after it element by element, and puts the tuple of the results in
    /// `base`: `(1, 2) + (3, 4)` is `(1 + 3, 2 + 4)`. The function in `base`
    /// is one for every position, or a tuple of one for each.
    CallVec { base: Reg, args: u16 },
    /// Calls the native in global slot `slot` with the `args` values from
    /// `base` on, for a call the analyser resolved to that native, and puts
    /// what it returns in `dst`. The argument registers are `None` after.
    CallNative {
        dst: Reg,
        base: Reg,
        slot: u16,
        args: u8,
    },
    /// `dst` = the tuple of the `len` values from `base` on, which are
    /// `None` afterwards.
    MakeTuple { dst: Reg, base: Reg, len: u16 },
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

    /// The instruction that does the binary `op` of `left` and the `Int`
    /// `imm` into `dst`, if there's one for `op`: `SubIntI` for
    /// `NativeOp::SubInt`.
    pub fn binary_immediate(op: NativeOp, dst: Reg, left: Reg, imm: i16) -> Option<Self> {
        match op {
            NativeOp::AddInt => Some(Self::AddIntI { dst, left, imm }),
            NativeOp::SubInt => Some(Self::SubIntI { dst, left, imm }),
            NativeOp::MulInt => Some(Self::MulIntI { dst, left, imm }),
            _ => None,
        }
    }

    /// The jump taken unless the comparison `op` of `left` and `right`
    /// holds, if `op` is a comparison: `JumpIfNotLess` for `NativeOp::Less`.
    pub fn jump_unless(op: NativeOp, left: Reg, right: Reg) -> Option<Self> {
        let offset = 0;
        Some(match op {
            NativeOp::Less => Self::JumpIfNotLess {
                left,
                right,
                offset,
            },
            NativeOp::LessEqual => Self::JumpIfNotLessEqual {
                left,
                right,
                offset,
            },
            NativeOp::Greater => Self::JumpIfNotGreater {
                left,
                right,
                offset,
            },
            NativeOp::GreaterEqual => Self::JumpIfNotGreaterEqual {
                left,
                right,
                offset,
            },
            NativeOp::Equal => Self::JumpIfNotEqual {
                left,
                right,
                offset,
            },
            NativeOp::NotEqual => Self::JumpIfNotNotEqual {
                left,
                right,
                offset,
            },
            _ => return None,
        })
    }

    /// [`Self::jump_unless`] with the `Int` `imm` on the right:
    /// `JumpIfNotLessI` for `NativeOp::Less`.
    pub fn jump_unless_immediate(op: NativeOp, left: Reg, imm: i16) -> Option<Self> {
        let offset = 0;
        Some(match op {
            NativeOp::Less => Self::JumpIfNotLessI { left, imm, offset },
            NativeOp::LessEqual => Self::JumpIfNotLessEqualI { left, imm, offset },
            NativeOp::Greater => Self::JumpIfNotGreaterI { left, imm, offset },
            NativeOp::GreaterEqual => Self::JumpIfNotGreaterEqualI { left, imm, offset },
            NativeOp::Equal => Self::JumpIfNotEqualI { left, imm, offset },
            NativeOp::NotEqual => Self::JumpIfNotNotEqualI { left, imm, offset },
            _ => return None,
        })
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
            Self::GetCell { .. } => "GetCell",
            Self::SetCell { .. } => "SetCell",
            Self::NewCell { .. } => "NewCell",
            Self::GetUpvalue { .. } => "GetUpvalue",
            Self::SetUpvalue { .. } => "SetUpvalue",
            Self::Closure { .. } => "Closure",
            Self::Memoize { .. } => "Memoize",
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
            Self::AddIntI { .. } => "AddIntI",
            Self::SubIntI { .. } => "SubIntI",
            Self::MulIntI { .. } => "MulIntI",
            Self::Jump { .. } => "Jump",
            Self::JumpIfFalse { .. } => "JumpIfFalse",
            Self::JumpIfNotLess { .. } => "JumpIfNotLess",
            Self::JumpIfNotLessEqual { .. } => "JumpIfNotLessEqual",
            Self::JumpIfNotGreater { .. } => "JumpIfNotGreater",
            Self::JumpIfNotGreaterEqual { .. } => "JumpIfNotGreaterEqual",
            Self::JumpIfNotEqual { .. } => "JumpIfNotEqual",
            Self::JumpIfNotNotEqual { .. } => "JumpIfNotNotEqual",
            Self::JumpIfNotLessI { .. } => "JumpIfNotLessI",
            Self::JumpIfNotLessEqualI { .. } => "JumpIfNotLessEqualI",
            Self::JumpIfNotGreaterI { .. } => "JumpIfNotGreaterI",
            Self::JumpIfNotGreaterEqualI { .. } => "JumpIfNotGreaterEqualI",
            Self::JumpIfNotEqualI { .. } => "JumpIfNotEqualI",
            Self::JumpIfNotNotEqualI { .. } => "JumpIfNotNotEqualI",
            Self::Call { .. } => "Call",
            Self::CallVec { .. } => "CallVec",
            Self::CallNative { .. } => "CallNative",
            Self::MakeTuple { .. } => "MakeTuple",
            Self::Return { .. } => "Return",
            Self::Halt { .. } => "Halt",
        }
    }

    /// The jump's offset as it's written, for an instruction that jumps.
    /// For one whose offset is [`FAR`], [`super::Chunk::jump_target`] says
    /// where it goes.
    pub fn jump_offset(self) -> Option<i32> {
        match self {
            Self::Jump { offset } | Self::JumpIfFalse { offset, .. } => Some(offset),
            Self::JumpIfNotLess { offset, .. }
            | Self::JumpIfNotLessEqual { offset, .. }
            | Self::JumpIfNotGreater { offset, .. }
            | Self::JumpIfNotGreaterEqual { offset, .. }
            | Self::JumpIfNotEqual { offset, .. }
            | Self::JumpIfNotNotEqual { offset, .. }
            | Self::JumpIfNotLessI { offset, .. }
            | Self::JumpIfNotLessEqualI { offset, .. }
            | Self::JumpIfNotGreaterI { offset, .. }
            | Self::JumpIfNotGreaterEqualI { offset, .. }
            | Self::JumpIfNotEqualI { offset, .. }
            | Self::JumpIfNotNotEqualI { offset, .. } => Some(i32::from(offset)),
            _ => None,
        }
    }

    /// Whether this is a jump whose offset is [`FAR`], so that where it
    /// goes is in the chunk's far jumps.
    pub fn is_far_jump(self) -> bool {
        !matches!(self, Self::Jump { .. } | Self::JumpIfFalse { .. })
            && self.jump_offset() == Some(i32::from(FAR))
    }

    /// The same jump going `offset` instructions on, or `None` if `offset`
    /// doesn't fit the jump's offset, which is then [`FAR`]. Panics for an
    /// instruction that doesn't jump.
    pub(crate) fn with_jump_offset(self, offset: i32) -> Option<Self> {
        if let Self::Jump { .. } = self {
            return Some(Self::Jump { offset });
        }
        if let Self::JumpIfFalse { cond, .. } = self {
            return Some(Self::JumpIfFalse { cond, offset });
        }
        let offset = i16::try_from(offset).ok().filter(|&offset| offset != FAR)?;
        Some(self.with_short_offset(offset))
    }

    /// The same jump, whose offset is an `i16`, going `offset` on: [`FAR`]
    /// for one that goes where the chunk's far jumps say.
    pub(crate) fn with_short_offset(self, offset: i16) -> Self {
        match self {
            Self::JumpIfNotLess { left, right, .. } => Self::JumpIfNotLess {
                left,
                right,
                offset,
            },
            Self::JumpIfNotLessEqual { left, right, .. } => Self::JumpIfNotLessEqual {
                left,
                right,
                offset,
            },
            Self::JumpIfNotGreater { left, right, .. } => Self::JumpIfNotGreater {
                left,
                right,
                offset,
            },
            Self::JumpIfNotGreaterEqual { left, right, .. } => Self::JumpIfNotGreaterEqual {
                left,
                right,
                offset,
            },
            Self::JumpIfNotEqual { left, right, .. } => Self::JumpIfNotEqual {
                left,
                right,
                offset,
            },
            Self::JumpIfNotNotEqual { left, right, .. } => Self::JumpIfNotNotEqual {
                left,
                right,
                offset,
            },
            Self::JumpIfNotLessI { left, imm, .. } => Self::JumpIfNotLessI { left, imm, offset },
            Self::JumpIfNotLessEqualI { left, imm, .. } => {
                Self::JumpIfNotLessEqualI { left, imm, offset }
            }
            Self::JumpIfNotGreaterI { left, imm, .. } => {
                Self::JumpIfNotGreaterI { left, imm, offset }
            }
            Self::JumpIfNotGreaterEqualI { left, imm, .. } => {
                Self::JumpIfNotGreaterEqualI { left, imm, offset }
            }
            Self::JumpIfNotEqualI { left, imm, .. } => Self::JumpIfNotEqualI { left, imm, offset },
            Self::JumpIfNotNotEqualI { left, imm, .. } => {
                Self::JumpIfNotNotEqualI { left, imm, offset }
            }
            other => panic!("internal error: {} has no short jump offset", other.name()),
        }
    }
}

/// The offset of a jump whose offset is an `i16` that goes further than one
/// can say: where it goes is in [`super::Chunk`]'s far jumps instead.
pub const FAR: i16 = i16::MIN;

/// The operands as the disassembler prints them, without the name:
/// `r2, r0, r1` for `AddInt { dst: r2, left: r0, right: r1 }`. A
/// constant is `k3`, a global `g12`, a native `n25`, one of the chunk's
/// closures `c0`, a closure's captured variable `u1`, and a jump's offset is
/// signed: `+4`.
impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::LoadK { dst, k } | Self::MakeString { dst, k } => write!(f, "{dst}, k{k}"),
            Self::LoadUnit { dst } => write!(f, "{dst}"),
            Self::Copy { dst, src }
            | Self::Move { dst, src }
            | Self::NegInt { dst, src }
            | Self::NegFloat { dst, src }
            | Self::Not { dst, src }
            | Self::GetCell { dst, src } => write!(f, "{dst}, {src}"),
            Self::GetGlobal { dst, slot } => write!(f, "{dst}, g{slot}"),
            Self::SetGlobal { slot, src } => write!(f, "g{slot}, {src}"),
            Self::SetCell { cell, src } => write!(f, "{cell}, {src}"),
            Self::NewCell { reg } | Self::Memoize { reg } => write!(f, "{reg}"),
            Self::GetUpvalue { dst, index } => write!(f, "{dst}, u{index}"),
            Self::SetUpvalue { index, src } => write!(f, "u{index}, {src}"),
            Self::Closure { dst, index } => write!(f, "{dst}, c{index}"),
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
            Self::AddIntI { dst, left, imm }
            | Self::SubIntI { dst, left, imm }
            | Self::MulIntI { dst, left, imm } => write!(f, "{dst}, {left}, #{imm}"),
            Self::Jump { offset } => write!(f, "{offset:+}"),
            Self::JumpIfFalse { cond, offset } => write!(f, "{cond}, {offset:+}"),
            Self::JumpIfNotLess {
                left,
                right,
                offset,
            }
            | Self::JumpIfNotLessEqual {
                left,
                right,
                offset,
            }
            | Self::JumpIfNotGreater {
                left,
                right,
                offset,
            }
            | Self::JumpIfNotGreaterEqual {
                left,
                right,
                offset,
            }
            | Self::JumpIfNotEqual {
                left,
                right,
                offset,
            }
            | Self::JumpIfNotNotEqual {
                left,
                right,
                offset,
            } => write!(f, "{left}, {right}, {}", ShortOffset(offset)),
            Self::JumpIfNotLessI { left, imm, offset }
            | Self::JumpIfNotLessEqualI { left, imm, offset }
            | Self::JumpIfNotGreaterI { left, imm, offset }
            | Self::JumpIfNotGreaterEqualI { left, imm, offset }
            | Self::JumpIfNotEqualI { left, imm, offset }
            | Self::JumpIfNotNotEqualI { left, imm, offset } => {
                write!(f, "{left}, #{imm}, {}", ShortOffset(offset))
            }
            Self::Call { base, args } | Self::CallVec { base, args } => {
                write!(f, "{base}, {args}")
            }
            Self::CallNative {
                dst,
                base,
                slot,
                args,
            } => write!(f, "{dst}, n{slot}, {base}, {args}"),
            Self::MakeTuple { dst, base, len } => write!(f, "{dst}, {base}, {len}"),
            Self::Return { src } | Self::Halt { src } => write!(f, "{src}"),
        }
    }
}

/// A short jump offset as the disassembler prints it: `+4`, or `far`.
struct ShortOffset(i16);

impl fmt::Display for ShortOffset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 == FAR {
            write!(f, "far")
        } else {
            write!(f, "{:+}", self.0)
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
