//! The register VM, which is to replace the stack VM (see
//! `docs/register-vm/plan.md`). Both exist side by side while it's built:
//! the stack VM stays the one programs run on unless a register VM is asked
//! for.
//!
//! A frame's values are registers, numbered from 0: the function's
//! parameters, its other locals, then the temporaries its expressions need.
//! An instruction names the registers it reads and writes, so `i = i + 1`
//! is one instruction, `AddInt r0, r0, r1`, rather than four that push and
//! pop.

pub mod chunk;
mod compiler;
pub mod disassemble;
pub mod instr;
mod vm;

pub use chunk::{Builder, Chunk, Label};
pub use compiler::Compiler;
pub use instr::{Instr, Reg, WINDOW};
pub use vm::Vm;
