//! Throwaway measurement for docs/register-vm/plan.md, not part of the build.
//!
//! Runs one loop (`total = total + i * 2 - 1; i = i + 1`, 20 million times) on
//! a stack VM shaped like ndc_vm's and on register VMs of a few shapes. Safe
//! Rust only, a 16-byte `Value` with `Rc` variants so drops are real, checked
//! arithmetic and type checks that return errors.
//!
//! ```sh
//! rustc --edition 2024 -C opt-level=3 -C codegen-units=1 -C lto=fat \
//!     -o /tmp/prototype docs/register-vm/prototype.rs
//! /tmp/prototype all            # or: stack | plain | constreg | fused
//! ```
#![deny(unsafe_code)]
use std::cell::RefCell;
use std::hint::black_box;
use std::rc::Rc;
use std::time::Instant;

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Number(Rc<String>),
    Bool(bool),
    None,
    Object(Rc<Vec<Value>>),
    Cell(Rc<RefCell<Value>>),
}

#[derive(Debug)]
pub struct VmError(String, u32);

#[cold]
#[inline(never)]
fn type_error(ip: usize) -> VmError {
    VmError("type".into(), ip as u32)
}
#[cold]
#[inline(never)]
fn overflow(ip: usize) -> VmError {
    VmError("overflow".into(), ip as u32)
}

// ---------------------------------------------------------------- stack VM
#[derive(Clone, Copy, Debug)]
pub enum SOp {
    Constant(u32),
    GetLocal(u32),
    SetLocal(u32),
    AddInt,
    SubInt,
    MulInt,
    Less,
    JumpIfFalse(i32),
    Jump(i32),
    Pop,
    Return,
}

#[inline(never)]
pub fn run_stack(code: &[SOp], constants: &[Value], num_locals: usize) -> Result<Value, VmError> {
    let mut stack: Vec<Value> = Vec::with_capacity(256);
    for _ in 0..num_locals {
        stack.push(Value::None);
    }
    let fp = 0usize;
    let mut ip = 0usize;
    loop {
        let op = code[ip];
        ip += 1;
        match op {
            SOp::Constant(k) => stack.push(constants[k as usize].clone()),
            SOp::GetLocal(s) => {
                let v = stack[fp + s as usize].clone();
                stack.push(v);
            }
            SOp::SetLocal(s) => {
                let v = stack.pop().expect("underflow");
                stack[fp + s as usize] = v;
            }
            SOp::AddInt | SOp::SubInt | SOp::MulInt => {
                let right = stack.pop().expect("underflow");
                let left = stack.last_mut().expect("underflow");
                let (Value::Int(l), Value::Int(r)) = (&*left, &right) else {
                    return Err(type_error(ip));
                };
                let result = match op {
                    SOp::AddInt => l.checked_add(*r),
                    SOp::SubInt => l.checked_sub(*r),
                    _ => l.checked_mul(*r),
                };
                *left = Value::Int(result.ok_or_else(|| overflow(ip))?);
            }
            SOp::Less => {
                let right = stack.pop().expect("underflow");
                let left = stack.last_mut().expect("underflow");
                let (Value::Int(l), Value::Int(r)) = (&*left, &right) else {
                    return Err(type_error(ip));
                };
                *left = Value::Bool(l < r);
            }
            SOp::JumpIfFalse(o) => match stack.last().expect("underflow") {
                Value::Bool(false) => ip = ip.wrapping_add_signed(o as isize),
                Value::Bool(true) => {}
                _ => return Err(type_error(ip)),
            },
            SOp::Jump(o) => ip = ip.wrapping_add_signed(o as isize),
            SOp::Pop => {
                stack.pop();
            }
            SOp::Return => return Ok(stack.pop().expect("underflow")),
        }
    }
}

/// The same stack code with the stack pointer in a local and the stack a
/// slice: no `Vec` length to keep in memory, values written in place.
#[inline(never)]
pub fn run_stack_sp(
    code: &[SOp],
    constants: &[Value],
    stack: &mut [Value],
    num_locals: usize,
) -> Result<Value, VmError> {
    let fp = 0usize;
    let mut sp = num_locals;
    let mut ip = 0usize;
    loop {
        let op = code[ip];
        ip += 1;
        match op {
            SOp::Constant(k) => {
                stack[sp] = constants[k as usize].clone();
                sp += 1;
            }
            SOp::GetLocal(s) => {
                stack[sp] = stack[fp + s as usize].clone();
                sp += 1;
            }
            SOp::SetLocal(s) => {
                sp -= 1;
                stack[fp + s as usize] = std::mem::replace(&mut stack[sp], Value::None);
            }
            SOp::AddInt | SOp::SubInt | SOp::MulInt => {
                let (Value::Int(l), Value::Int(r)) = (&stack[sp - 2], &stack[sp - 1]) else {
                    return Err(type_error(ip));
                };
                let result = match op {
                    SOp::AddInt => l.checked_add(*r),
                    SOp::SubInt => l.checked_sub(*r),
                    _ => l.checked_mul(*r),
                };
                stack[sp - 2] = Value::Int(result.ok_or_else(|| overflow(ip))?);
                sp -= 1;
            }
            SOp::Less => {
                let (Value::Int(l), Value::Int(r)) = (&stack[sp - 2], &stack[sp - 1]) else {
                    return Err(type_error(ip));
                };
                stack[sp - 2] = Value::Bool(l < r);
                sp -= 1;
            }
            SOp::JumpIfFalse(o) => match &stack[sp - 1] {
                Value::Bool(false) => ip = ip.wrapping_add_signed(o as isize),
                Value::Bool(true) => {}
                _ => return Err(type_error(ip)),
            },
            SOp::Jump(o) => ip = ip.wrapping_add_signed(o as isize),
            SOp::Pop => {
                sp -= 1;
                stack[sp] = Value::None;
            }
            SOp::Return => return Ok(std::mem::replace(&mut stack[sp - 1], Value::None)),
        }
    }
}

// ------------------------------------------------------------- register VM
#[derive(Clone, Copy, Debug)]
pub enum ROp {
    LoadK {
        dst: u16,
        k: u32,
    },
    Move {
        dst: u16,
        src: u16,
    },
    AddInt {
        dst: u16,
        l: u16,
        r: u16,
    },
    SubInt {
        dst: u16,
        l: u16,
        r: u16,
    },
    MulInt {
        dst: u16,
        l: u16,
        r: u16,
    },
    AddIntI {
        dst: u16,
        l: u16,
        imm: i16,
    },
    SubIntI {
        dst: u16,
        l: u16,
        imm: i16,
    },
    MulIntI {
        dst: u16,
        l: u16,
        imm: i16,
    },
    Less {
        dst: u16,
        l: u16,
        r: u16,
    },
    JumpIfFalse {
        cond: u16,
        off: i16,
    },
    /// Fused compare and branch: jump unless `l < r`.
    JumpIfNotLess {
        l: u16,
        r: u16,
        off: i16,
    },
    Jump {
        off: i32,
    },
    Return {
        src: u16,
    },
}

macro_rules! register_loop {
    ($regs:ident, $code:ident, $constants:ident) => {{
        let mut ip = 0usize;
        loop {
            let op = $code[ip];
            ip += 1;
            match op {
                ROp::LoadK { dst, k } => $regs[dst as usize] = $constants[k as usize].clone(),
                ROp::Move { dst, src } => $regs[dst as usize] = $regs[src as usize].clone(),
                ROp::AddInt { dst, l, r } => {
                    let (Value::Int(a), Value::Int(b)) = (&$regs[l as usize], &$regs[r as usize])
                    else {
                        return Err(type_error(ip));
                    };
                    $regs[dst as usize] =
                        Value::Int(a.checked_add(*b).ok_or_else(|| overflow(ip))?);
                }
                ROp::SubInt { dst, l, r } => {
                    let (Value::Int(a), Value::Int(b)) = (&$regs[l as usize], &$regs[r as usize])
                    else {
                        return Err(type_error(ip));
                    };
                    $regs[dst as usize] =
                        Value::Int(a.checked_sub(*b).ok_or_else(|| overflow(ip))?);
                }
                ROp::MulInt { dst, l, r } => {
                    let (Value::Int(a), Value::Int(b)) = (&$regs[l as usize], &$regs[r as usize])
                    else {
                        return Err(type_error(ip));
                    };
                    $regs[dst as usize] =
                        Value::Int(a.checked_mul(*b).ok_or_else(|| overflow(ip))?);
                }
                ROp::AddIntI { dst, l, imm } => {
                    let Value::Int(a) = &$regs[l as usize] else {
                        return Err(type_error(ip));
                    };
                    $regs[dst as usize] =
                        Value::Int(a.checked_add(i64::from(imm)).ok_or_else(|| overflow(ip))?);
                }
                ROp::SubIntI { dst, l, imm } => {
                    let Value::Int(a) = &$regs[l as usize] else {
                        return Err(type_error(ip));
                    };
                    $regs[dst as usize] =
                        Value::Int(a.checked_sub(i64::from(imm)).ok_or_else(|| overflow(ip))?);
                }
                ROp::MulIntI { dst, l, imm } => {
                    let Value::Int(a) = &$regs[l as usize] else {
                        return Err(type_error(ip));
                    };
                    $regs[dst as usize] =
                        Value::Int(a.checked_mul(i64::from(imm)).ok_or_else(|| overflow(ip))?);
                }
                ROp::Less { dst, l, r } => {
                    let (Value::Int(a), Value::Int(b)) = (&$regs[l as usize], &$regs[r as usize])
                    else {
                        return Err(type_error(ip));
                    };
                    $regs[dst as usize] = Value::Bool(a < b);
                }
                ROp::JumpIfFalse { cond, off } => match &$regs[cond as usize] {
                    Value::Bool(false) => ip = ip.wrapping_add_signed(off as isize),
                    Value::Bool(true) => {}
                    _ => return Err(type_error(ip)),
                },
                ROp::JumpIfNotLess { l, r, off } => {
                    let (Value::Int(a), Value::Int(b)) = (&$regs[l as usize], &$regs[r as usize])
                    else {
                        return Err(type_error(ip));
                    };
                    if a >= b {
                        ip = ip.wrapping_add_signed(off as isize);
                    }
                }
                ROp::Jump { off } => ip = ip.wrapping_add_signed(off as isize),
                ROp::Return { src } => return Ok($regs[src as usize].clone()),
            }
        }
    }};
}

/// Registers are a slice of the value stack: every access is bounds-checked.
#[inline(never)]
pub fn run_register_slice(
    code: &[ROp],
    constants: &[Value],
    stack: &mut Vec<Value>,
    base: usize,
    frame: usize,
) -> Result<Value, VmError> {
    let regs = &mut stack[base..base + frame];
    register_loop!(regs, code, constants)
}

/// Registers are a 256-slot window: a `u8` operand can't be out of bounds.
#[inline(never)]
pub fn run_register_window(
    code: &[ROp],
    constants: &[Value],
    stack: &mut Vec<Value>,
    base: usize,
) -> Result<Value, VmError> {
    let regs: &mut [Value; 65536] = (&mut stack[base..base + 65536]).try_into().expect("window");
    register_loop!(regs, code, constants)
}

fn main() {
    let n: i64 = std::env::args()
        .nth(2)
        .and_then(|s| s.parse().ok())
        .unwrap_or(20_000_000);
    let which = std::env::args().nth(1).unwrap_or_default();
    println!(
        "size_of Value = {}, SOp = {}, ROp = {}",
        size_of::<Value>(),
        size_of::<SOp>(),
        size_of::<ROp>()
    );

    // ndc's exact instruction sequence for the loop (18 per iteration).
    let s_consts = vec![Value::Int(0), Value::Int(n), Value::Int(2), Value::Int(1)];
    let s_code = vec![
        SOp::Constant(0),
        SOp::SetLocal(0),
        SOp::Constant(0),
        SOp::SetLocal(1),
        // 4:
        SOp::GetLocal(0),
        SOp::Constant(1),
        SOp::Less,
        SOp::JumpIfFalse(14),
        SOp::Pop,
        SOp::GetLocal(1),
        SOp::GetLocal(0),
        SOp::Constant(2),
        SOp::MulInt,
        SOp::AddInt,
        SOp::Constant(3),
        SOp::SubInt,
        SOp::SetLocal(1),
        SOp::GetLocal(0),
        SOp::Constant(3),
        SOp::AddInt,
        SOp::SetLocal(0),
        SOp::Jump(-18),
        // 22:
        SOp::Pop,
        SOp::GetLocal(1),
        SOp::Return,
    ];

    // r0 = i, r1 = total, r2 = temp, r3.. = constants / temps
    // (b) plain three-address, constants loaded into registers each time (11/iter)
    let r_consts = vec![Value::Int(0), Value::Int(n), Value::Int(2), Value::Int(1)];
    let r_plain = vec![
        ROp::LoadK { dst: 0, k: 0 },
        ROp::LoadK { dst: 1, k: 0 },
        // 2:
        ROp::LoadK { dst: 3, k: 1 },
        ROp::Less { dst: 4, l: 0, r: 3 },
        ROp::JumpIfFalse { cond: 4, off: 8 },
        ROp::LoadK { dst: 3, k: 2 },
        ROp::MulInt { dst: 2, l: 0, r: 3 },
        ROp::AddInt { dst: 2, l: 1, r: 2 },
        ROp::LoadK { dst: 3, k: 3 },
        ROp::SubInt { dst: 1, l: 2, r: 3 },
        ROp::LoadK { dst: 3, k: 3 },
        ROp::AddInt { dst: 0, l: 0, r: 3 },
        ROp::Jump { off: -11 },
        // 13:
        ROp::Return { src: 1 },
    ];
    // (c) constants live in registers for the whole function (7/iter)
    let r_constreg = vec![
        ROp::LoadK { dst: 0, k: 0 },
        ROp::LoadK { dst: 1, k: 0 },
        ROp::LoadK { dst: 5, k: 1 },
        ROp::LoadK { dst: 6, k: 2 },
        ROp::LoadK { dst: 7, k: 3 },
        // 5:
        ROp::Less { dst: 4, l: 0, r: 5 },
        ROp::JumpIfFalse { cond: 4, off: 5 },
        ROp::MulInt { dst: 2, l: 0, r: 6 },
        ROp::AddInt { dst: 2, l: 1, r: 2 },
        ROp::SubInt { dst: 1, l: 2, r: 7 },
        ROp::AddInt { dst: 0, l: 0, r: 7 },
        ROp::Jump { off: -7 },
        // 12:
        ROp::Return { src: 1 },
    ];
    // (d) immediates and a fused compare-and-branch (6/iter)
    let r_fused = vec![
        ROp::LoadK { dst: 0, k: 0 },
        ROp::LoadK { dst: 1, k: 0 },
        ROp::LoadK { dst: 5, k: 1 },
        // 3:
        ROp::JumpIfNotLess { l: 0, r: 5, off: 5 },
        ROp::MulIntI {
            dst: 2,
            l: 0,
            imm: 2,
        },
        ROp::AddInt { dst: 2, l: 1, r: 2 },
        ROp::SubIntI {
            dst: 1,
            l: 2,
            imm: 1,
        },
        ROp::AddIntI {
            dst: 0,
            l: 0,
            imm: 1,
        },
        ROp::Jump { off: -6 },
        // 9:
        ROp::Return { src: 1 },
    ];

    let time = |name: &str, f: &mut dyn FnMut() -> Result<Value, VmError>| {
        let mut best = u128::MAX;
        let mut out = None;
        for _ in 0..5 {
            let start = Instant::now();
            out = Some(f());
            best = best.min(start.elapsed().as_millis());
        }
        println!("{name:<34} {best:>6} ms   {:?}", out.unwrap());
    };
    let fresh = || vec![Value::None; 70000];
    let all = which.is_empty() || which == "all";
    if all || which == "stack" {
        time("stack (ndc's 18 instr/iter)", &mut || {
            run_stack(black_box(&s_code), black_box(&s_consts), 2)
        });
    }
    if all || which == "stack" {
        time("stack, sp local + slice (18/iter)", &mut || {
            run_stack_sp(black_box(&s_code), black_box(&s_consts), &mut fresh(), 2)
        });
    }
    if all || which == "plain" {
        time("register plain, slice (11/iter)", &mut || {
            run_register_slice(black_box(&r_plain), &r_consts, &mut fresh(), 8, 16)
        });
        time("register plain, window (11/iter)", &mut || {
            run_register_window(black_box(&r_plain), &r_consts, &mut fresh(), 8)
        });
    }
    if all || which == "constreg" {
        time("register const-regs, slice (7/iter)", &mut || {
            run_register_slice(black_box(&r_constreg), &r_consts, &mut fresh(), 8, 16)
        });
        time("register const-regs, window (7/iter)", &mut || {
            run_register_window(black_box(&r_constreg), &r_consts, &mut fresh(), 8)
        });
    }
    if all || which == "fused" {
        time("register fused+imm, slice (6/iter)", &mut || {
            run_register_slice(black_box(&r_fused), &r_consts, &mut fresh(), 8, 16)
        });
        time("register fused+imm, window (6/iter)", &mut || {
            run_register_window(black_box(&r_fused), &r_consts, &mut fresh(), 8)
        });
    }
}
