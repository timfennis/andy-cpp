//! The VM, which runs what [`crate::compiler`] compiles.
//!
//! Every running call has a frame: a window of the VM's one value stack,
//! `stack[base..base + frame_size]`, whose values are the function's
//! registers. A call's frame starts at the register after the one its
//! function is in, so the caller's argument registers are the callee's
//! parameters, and `Return` puts the result where the function was.
//! Nothing is pushed or popped while code runs; [`WINDOW`] values past the
//! top frame's first register are always there, so an instruction's
//! registers are indexed without a bounds check.
//!
//! An instruction either borrows its operands, as `AddInt` and the jumps
//! do, or takes them and leaves `None` behind, as everything that stores a
//! value somewhere else does (`MakeList`, `SetGlobal`, `Return`, calls).
//!
//! A temporary that an instruction only borrowed, as both sides of
//! `f(x) == g(y)`, stays in its register until the register is written
//! again or the call returns, which empties its frame. Nothing clears it
//! sooner: an instruction to do so costs more than the memory it would
//! free earlier. So a reference count can read one higher than the values
//! a program can still reach, which may only ever cost a copy that wasn't
//! needed, as it does for `++` and for casts.
//!
//! A call doesn't clear the registers of the frame it makes, which may hold
//! what the caller left in its temporaries. Its code writes each local
//! before reading it: a parameter is written by the call, and any other
//! local by its declaration (see [`crate::compiler`]).

use crate::chunk::{ClosureCode, far_target};
use crate::error::VmError;
use crate::hash_map::HashMap;
use crate::instr::{FAR, Instr, Reg, WINDOW};
use crate::io::{InputSource, OutputSink};
use crate::iterator::ValueIter;
use crate::memo::{MemoCache, MemoKey};
use crate::native_op::{NativeOp, list_element, list_index_error, list_set};
use crate::value::{
    ClosureFunction, CompiledFunction, Function, NativeFunc, NativeFunction, Range,
};
use crate::{Natives, Object, Value};
use ndc_analysis::CaptureSource;
use ndc_core::StaticType;
use ndc_lexer::Span;
use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

/// Runs compiled code. The natives that take a VM are given this one, so a
/// native that calls back into the program, as `map` does, runs the
/// callback on the same VM.
pub struct Vm {
    /// The registers of every running call, one frame above the other, with
    /// [`WINDOW`] values past the top frame's first register.
    stack: Vec<Value>,
    globals: Vec<Value>,
    /// The running calls.
    frames: Frames,
    /// What the program's last expression gave, once it halts.
    result: Value,
    /// Output sink for `print`, `dbg`, and similar I/O built-ins.
    output: OutputSink,
    /// Input source for `read_stdin`.
    input: InputSource,
    #[cfg(feature = "trace")]
    source: Option<String>,
    #[cfg(feature = "trace")]
    tracer: Option<Box<dyn crate::tracer::VmTracer>>,
}

/// The cache of a memoised function's results, and the key of the call
/// whose result goes in it when the call returns.
type PendingMemo = (Rc<RefCell<MemoCache>>, MemoKey);

/// How many calls deep a program can go. A frame takes about a hundred
/// bytes, so this is about 100 MB of frames.
pub const MAX_CALL_DEPTH: usize = 1_000_000;

/// The VM's globals for `natives`: native `i` is global slot `i`, and the
/// intrinsics follow. The analyser numbers them the same way. Each holds unit
/// until [`Vm::new`] puts the native there for code that reads it: a native
/// nothing uses is never made. An intrinsic's slot keeps unit: the compiler
/// writes it as bytecode where it's used and never reads its slot.
pub fn native_globals(natives: &Natives) -> Vec<Value> {
    vec![Value::unit(); natives.len() + natives.intrinsics().len()]
}

/// Puts each native `code` reads in its global slot, making the ones that
/// aren't made yet, before `code` runs at the top level.
fn fill_natives(globals: &mut [Value], natives: &Natives, code: &CompiledFunction) {
    for &slot in &code.natives {
        // An intrinsic's slot is never read: see `native_globals`.
        if slot >= natives.len() {
            continue;
        }
        let filled = matches!(&globals[slot], Value::Object(object)
            if matches!(object.as_ref(), Object::Function(Function::Native(_))));
        if !filled {
            globals[slot] = Value::Object(Rc::new(Object::Function(Function::Native(Rc::clone(
                natives.function(slot),
            )))));
        }
    }
}

/// A running call. Its registers are
/// `stack[base..base + frame_size]`, and nothing is pushed or popped while
/// code runs. A call's frame starts at the register after the callee, so the
/// caller's argument registers are the callee's parameters.
struct Frame {
    function: Rc<CompiledFunction>,
    /// The closure called, if the function is one, whose upvalues are the
    /// cells of the variables it captures. The closure itself rather than a
    /// copy of its upvalues, which keeps frames small and calls cheap.
    closure: Option<Rc<Object>>,
    /// The next instruction to run, kept here only while another frame runs
    /// or the inner loop is left. This and `base` are `u32`s, which keeps a
    /// frame to 32 bytes: calls take measurably longer with bigger frames.
    ip: u32,
    /// The index in the stack of the frame's first register.
    base: u32,
    /// What the call does with what it returns besides giving it to the
    /// caller, for the few calls that do something: boxed, so that frames
    /// stay small.
    on_return: Option<Box<OnReturn>>,
}

/// The running calls, the one on top last. A call's frame is
/// written field by field into a slot kept from an earlier call that went
/// as deep, rather than made and moved in: moving a new frame in reads it
/// back from where it was made before its fields are stored, which stalls,
/// and took a quarter of the time of a call. A slot past the running calls
/// holds nothing but the function its call ran.
struct Frames {
    slots: Vec<Frame>,
    /// How many of the slots are running calls.
    depth: usize,
}

/// What a call does with what it returns before it gives it to its caller.
#[derive(Default)]
struct OnReturn {
    /// For a call of a `pure fn`: store it in the function's cache.
    memo: Option<PendingMemo>,
    /// For a call of a [`Function::Checked`]: check that it's a value of
    /// this type, and make it one.
    returns: Option<StaticType>,
}

impl Frame {
    /// The frame of a program run at the top level, `function`.
    fn top_level(function: Rc<CompiledFunction>) -> Self {
        Self {
            function,
            closure: None,
            ip: 0,
            base: 0,
            on_return: None,
        }
    }

    /// The cells of the variables the function captures, if it's a
    /// closure.
    fn upvalues(&self) -> &[Rc<RefCell<Value>>] {
        match self.closure.as_deref() {
            Some(Object::Function(Function::Closure(closure))) => &closure.upvalues,
            _ => &[],
        }
    }
}

impl Frames {
    /// The call of a program run at the top level, `function`.
    fn top_level(function: Rc<CompiledFunction>) -> Self {
        Self {
            slots: vec![Frame::top_level(function)],
            depth: 1,
        }
    }

    /// How many calls are running.
    fn len(&self) -> usize {
        self.depth
    }

    /// The call on top.
    fn last(&self) -> Option<&Frame> {
        self.slots[..self.depth].last()
    }

    fn last_mut(&mut self) -> Option<&mut Frame> {
        self.slots[..self.depth].last_mut()
    }

    /// Starts a call of `function`, a closure's if `closure` is one, whose
    /// first register is `base`, at its first instruction.
    fn push(
        &mut self,
        function: Rc<CompiledFunction>,
        closure: Option<Rc<Object>>,
        base: u32,
        on_return: Option<Box<OnReturn>>,
    ) {
        if let Some(slot) = self.slots.get_mut(self.depth) {
            slot.function = function;
            slot.closure = closure;
            slot.ip = 0;
            slot.base = base;
            slot.on_return = on_return;
        } else {
            self.slots.push(Frame {
                function,
                closure,
                ip: 0,
                base,
                on_return,
            });
        }
        self.depth += 1;
    }

    /// Ends the call on top, and returns how many registers its function
    /// takes and what it does with what it returns. Its closure is let go of
    /// now, as a popped frame would be.
    fn pop(&mut self) -> (usize, Option<Box<OnReturn>>) {
        self.depth = self
            .depth
            .checked_sub(1)
            .expect("internal error: a running VM has a frame");
        let slot = &mut self.slots[self.depth];
        slot.closure = None;
        (slot.function.frame_size, slot.on_return.take())
    }

    /// Ends the calls above the first `depth`.
    fn truncate(&mut self, depth: usize) {
        while self.depth > depth {
            self.pop();
        }
    }
}

/// Why the inner loop of [`Vm::run_to_depth`] stopped: something it doesn't
/// do itself because it changes frames or needs the whole VM.
enum Exit {
    Call {
        base: Reg,
        args: u16,
    },
    CallVec {
        base: Reg,
        args: u16,
    },
    /// A native that takes the VM.
    CallNative {
        dst: Reg,
        base: Reg,
        slot: u16,
        args: u8,
    },
    Return {
        src: Reg,
    },
    Halt {
        src: Reg,
    },
    Error(VmError),
}

impl Vm {
    /// A VM that runs `function` at the top level with `globals`, which
    /// [`native_globals`] starts, and which a VM that ran earlier code hands
    /// on. Each native `function` reads is put in its slot first, made from
    /// `natives` if it isn't made yet.
    pub fn new(function: CompiledFunction, mut globals: Vec<Value>, natives: &Natives) -> Self {
        // Every global the program declares has a slot, holding unit until its
        // declaration runs, like a local.
        if globals.len() < function.num_globals {
            globals.resize(function.num_globals, Value::unit());
        }
        fill_natives(&mut globals, natives, &function);
        Self {
            stack: vec![Value::None; WINDOW],
            globals,
            frames: Frames::top_level(Rc::new(function)),
            result: Value::unit(),
            output: OutputSink::Stdout,
            input: InputSource::Stdin,
            #[cfg(feature = "trace")]
            source: None,
            #[cfg(feature = "trace")]
            tracer: None,
        }
    }

    /// Replace the output sink used by I/O built-ins (e.g. `print`, `dbg`).
    /// Call this before `run()` to redirect output away from stdout.
    pub fn with_output(mut self, output: OutputSink) -> Self {
        self.output = output;
        self
    }

    /// Returns the captured output bytes, or `None` if this VM writes to stdout.
    pub fn get_output(&self) -> Option<&[u8]> {
        match &self.output {
            OutputSink::Stdout => None,
            OutputSink::Buffer(buf) => Some(buf),
        }
    }

    /// Replace the input source used by `read_stdin`. Call this before
    /// `run()` to read from somewhere other than stdin.
    pub fn with_input(mut self, input: InputSource) -> Self {
        self.input = input;
        self
    }

    /// The input source, with what is left of it to read.
    pub fn take_input(&mut self) -> InputSource {
        std::mem::replace(&mut self.input, InputSource::None)
    }

    /// Reads everything left on the VM's input source.
    pub fn read_input(&mut self) -> Result<String, VmError> {
        self.input.read_to_end()
    }

    /// Write a pre-formatted string to the VM's output sink.
    pub fn write_output(&mut self, s: &str) -> Result<(), VmError> {
        self.output
            .write_all(s.as_bytes())
            .map_err(|e| VmError::native(e.to_string()))
    }

    #[cfg(feature = "trace")]
    pub fn with_source(mut self, source: impl Into<String>) -> Self {
        self.source = Some(source.into());
        self
    }

    #[cfg(feature = "trace")]
    pub fn set_source(&mut self, source: impl Into<String>) {
        self.source = Some(source.into());
    }

    #[cfg(feature = "trace")]
    pub fn with_tracer(mut self, tracer: Box<dyn crate::tracer::VmTracer>) -> Self {
        self.tracer = Some(tracer);
        self
    }

    /// Runs the program until it ends or fails, and then tells the tracer,
    /// if there is one, that it's done.
    pub fn run(&mut self) -> Result<(), VmError> {
        let result = self.run_to_depth(0);
        #[cfg(feature = "trace")]
        if let Some(tracer) = &mut self.tracer {
            tracer.on_complete();
        }
        result
    }

    /// Runs the code until the program halts or fails, or until a call
    /// returns to `target_depth` frames, which is how a callback runs.
    ///
    /// Two loops: the inner one runs the instructions of one frame with what
    /// it needs in locals, the frame's code, its instruction pointer and its
    /// registers, and leaves for anything that enters or leaves a frame; the
    /// outer one does that and enters the inner loop again for the frame on
    /// top.
    ///
    /// The work of a rarer instruction, such as making a closure or an
    /// iterator, is in a function marked `#[inline(never)]`: inlined, it
    /// makes the loop bigger and the integer loop and calls measurably
    /// slower.
    fn run_to_depth(&mut self, target_depth: usize) -> Result<(), VmError> {
        loop {
            let frame = self
                .frames
                .last()
                .expect("internal error: a running VM has a frame");
            let chunk = &frame.function.body;
            let code = chunk.code();
            let constants = chunk.constants();
            let base = frame.base as usize;
            let mut ip = frame.ip as usize;
            let regs: &mut [Value; WINDOW] = (&mut self.stack[base..base + WINDOW])
                .try_into()
                .expect("internal error: the stack reaches a window past every frame");
            let globals = &mut self.globals;
            let far_jumps = chunk.far_jumps();

            // A compare-and-branch: goes `offset` on unless `holds` of the
            // two values, as the comparison `op` finds them.
            macro_rules! jump_unless {
                ($op:expr, $left:expr, $right:expr, $holds:expr, $offset:expr) => {{
                    let holds = match ($left, $right) {
                        (Value::Int(l), Value::Int(r)) => $holds(l, r),
                        (left, right) => match compare($op, left, right) {
                            Ok(holds) => holds,
                            Err(error) => break Exit::Error(error),
                        },
                    };
                    if !holds {
                        ip = short_jump(ip, $offset, far_jumps);
                    }
                }};
            }

            let exit = loop {
                let instr = code[ip];
                #[cfg(feature = "trace")]
                crate::tracer::trace_instruction(
                    &mut self.tracer,
                    self.source.as_deref(),
                    chunk,
                    ip,
                );
                ip += 1;
                match instr {
                    Instr::LoadK { dst, k } => regs[dst.index()] = constants[k as usize].clone(),
                    Instr::LoadUnit { dst } => regs[dst.index()] = Value::unit(),
                    Instr::MakeString { dst, k } => {
                        regs[dst.index()] = new_string(&constants[k as usize]);
                    }
                    Instr::Copy { dst, src } => regs[dst.index()] = regs[src.index()].clone(),
                    Instr::Move { dst, src } => {
                        regs[dst.index()] = std::mem::replace(&mut regs[src.index()], Value::None);
                    }
                    Instr::GetGlobal { dst, slot } => {
                        regs[dst.index()] = globals[slot as usize].clone();
                    }
                    Instr::SetGlobal { slot, src } => {
                        globals[slot as usize] =
                            std::mem::replace(&mut regs[src.index()], Value::None);
                    }
                    Instr::AddInt { dst, left, right } => {
                        let value = match (&regs[left.index()], &regs[right.index()]) {
                            (Value::Int(l), Value::Int(r)) => l.checked_add(*r).map(Value::Int),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                break Exit::Error(binary_error(
                                    NativeOp::AddInt,
                                    &regs[left.index()],
                                    &regs[right.index()],
                                ));
                            }
                        }
                    }
                    Instr::SubInt { dst, left, right } => {
                        let value = match (&regs[left.index()], &regs[right.index()]) {
                            (Value::Int(l), Value::Int(r)) => l.checked_sub(*r).map(Value::Int),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                break Exit::Error(binary_error(
                                    NativeOp::SubInt,
                                    &regs[left.index()],
                                    &regs[right.index()],
                                ));
                            }
                        }
                    }
                    Instr::MulInt { dst, left, right } => {
                        let value = match (&regs[left.index()], &regs[right.index()]) {
                            (Value::Int(l), Value::Int(r)) => l.checked_mul(*r).map(Value::Int),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                break Exit::Error(binary_error(
                                    NativeOp::MulInt,
                                    &regs[left.index()],
                                    &regs[right.index()],
                                ));
                            }
                        }
                    }
                    Instr::AddFloat { dst, left, right } => {
                        if let Err(error) = binary(regs, NativeOp::AddFloat, dst, left, right) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::SubFloat { dst, left, right } => {
                        if let Err(error) = binary(regs, NativeOp::SubFloat, dst, left, right) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::MulFloat { dst, left, right } => {
                        if let Err(error) = binary(regs, NativeOp::MulFloat, dst, left, right) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::DivFloat { dst, left, right } => {
                        if let Err(error) = binary(regs, NativeOp::DivFloat, dst, left, right) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::Less { dst, left, right } => {
                        let value = match (&regs[left.index()], &regs[right.index()]) {
                            (Value::Int(l), Value::Int(r)) => Some(Value::Bool(l < r)),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                if let Err(error) = binary(regs, NativeOp::Less, dst, left, right) {
                                    break Exit::Error(error);
                                }
                            }
                        }
                    }
                    Instr::LessEqual { dst, left, right } => {
                        let value = match (&regs[left.index()], &regs[right.index()]) {
                            (Value::Int(l), Value::Int(r)) => Some(Value::Bool(l <= r)),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                if let Err(error) =
                                    binary(regs, NativeOp::LessEqual, dst, left, right)
                                {
                                    break Exit::Error(error);
                                }
                            }
                        }
                    }
                    Instr::Greater { dst, left, right } => {
                        let value = match (&regs[left.index()], &regs[right.index()]) {
                            (Value::Int(l), Value::Int(r)) => Some(Value::Bool(l > r)),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                if let Err(error) =
                                    binary(regs, NativeOp::Greater, dst, left, right)
                                {
                                    break Exit::Error(error);
                                }
                            }
                        }
                    }
                    Instr::GreaterEqual { dst, left, right } => {
                        let value = match (&regs[left.index()], &regs[right.index()]) {
                            (Value::Int(l), Value::Int(r)) => Some(Value::Bool(l >= r)),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                if let Err(error) =
                                    binary(regs, NativeOp::GreaterEqual, dst, left, right)
                                {
                                    break Exit::Error(error);
                                }
                            }
                        }
                    }
                    Instr::Equal { dst, left, right } => {
                        if let Err(error) = binary(regs, NativeOp::Equal, dst, left, right) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::NotEqual { dst, left, right } => {
                        if let Err(error) = binary(regs, NativeOp::NotEqual, dst, left, right) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::NegInt { dst, src } => {
                        if let Err(error) = unary(regs, NativeOp::NegInt, dst, src) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::NegFloat { dst, src } => {
                        if let Err(error) = unary(regs, NativeOp::NegFloat, dst, src) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::Not { dst, src } => {
                        if let Err(error) = unary(regs, NativeOp::Not, dst, src) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::AddIntI { dst, left, imm } => {
                        let value = match &regs[left.index()] {
                            Value::Int(l) => l.checked_add(i64::from(imm)).map(Value::Int),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                break Exit::Error(binary_error(
                                    NativeOp::AddInt,
                                    &regs[left.index()],
                                    &Value::Int(i64::from(imm)),
                                ));
                            }
                        }
                    }
                    Instr::SubIntI { dst, left, imm } => {
                        let value = match &regs[left.index()] {
                            Value::Int(l) => l.checked_sub(i64::from(imm)).map(Value::Int),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                break Exit::Error(binary_error(
                                    NativeOp::SubInt,
                                    &regs[left.index()],
                                    &Value::Int(i64::from(imm)),
                                ));
                            }
                        }
                    }
                    Instr::MulIntI { dst, left, imm } => {
                        let value = match &regs[left.index()] {
                            Value::Int(l) => l.checked_mul(i64::from(imm)).map(Value::Int),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                break Exit::Error(binary_error(
                                    NativeOp::MulInt,
                                    &regs[left.index()],
                                    &Value::Int(i64::from(imm)),
                                ));
                            }
                        }
                    }
                    Instr::JumpIfNotLess {
                        left,
                        right,
                        offset,
                    } => jump_unless!(
                        NativeOp::Less,
                        &regs[left.index()],
                        &regs[right.index()],
                        |l: &i64, r: &i64| l < r,
                        offset
                    ),
                    Instr::JumpIfNotLessEqual {
                        left,
                        right,
                        offset,
                    } => jump_unless!(
                        NativeOp::LessEqual,
                        &regs[left.index()],
                        &regs[right.index()],
                        |l: &i64, r: &i64| l <= r,
                        offset
                    ),
                    Instr::JumpIfNotGreater {
                        left,
                        right,
                        offset,
                    } => jump_unless!(
                        NativeOp::Greater,
                        &regs[left.index()],
                        &regs[right.index()],
                        |l: &i64, r: &i64| l > r,
                        offset
                    ),
                    Instr::JumpIfNotGreaterEqual {
                        left,
                        right,
                        offset,
                    } => jump_unless!(
                        NativeOp::GreaterEqual,
                        &regs[left.index()],
                        &regs[right.index()],
                        |l: &i64, r: &i64| l >= r,
                        offset
                    ),
                    Instr::JumpIfNotEqual {
                        left,
                        right,
                        offset,
                    } => jump_unless!(
                        NativeOp::Equal,
                        &regs[left.index()],
                        &regs[right.index()],
                        |l: &i64, r: &i64| l == r,
                        offset
                    ),
                    Instr::JumpIfNotNotEqual {
                        left,
                        right,
                        offset,
                    } => jump_unless!(
                        NativeOp::NotEqual,
                        &regs[left.index()],
                        &regs[right.index()],
                        |l: &i64, r: &i64| l != r,
                        offset
                    ),
                    Instr::JumpIfNotLessI { left, imm, offset } => jump_unless!(
                        NativeOp::Less,
                        &regs[left.index()],
                        &Value::Int(i64::from(imm)),
                        |l: &i64, r: &i64| l < r,
                        offset
                    ),
                    Instr::JumpIfNotLessEqualI { left, imm, offset } => jump_unless!(
                        NativeOp::LessEqual,
                        &regs[left.index()],
                        &Value::Int(i64::from(imm)),
                        |l: &i64, r: &i64| l <= r,
                        offset
                    ),
                    Instr::JumpIfNotGreaterI { left, imm, offset } => jump_unless!(
                        NativeOp::Greater,
                        &regs[left.index()],
                        &Value::Int(i64::from(imm)),
                        |l: &i64, r: &i64| l > r,
                        offset
                    ),
                    Instr::JumpIfNotGreaterEqualI { left, imm, offset } => jump_unless!(
                        NativeOp::GreaterEqual,
                        &regs[left.index()],
                        &Value::Int(i64::from(imm)),
                        |l: &i64, r: &i64| l >= r,
                        offset
                    ),
                    Instr::JumpIfNotEqualI { left, imm, offset } => jump_unless!(
                        NativeOp::Equal,
                        &regs[left.index()],
                        &Value::Int(i64::from(imm)),
                        |l: &i64, r: &i64| l == r,
                        offset
                    ),
                    Instr::JumpIfNotNotEqualI { left, imm, offset } => jump_unless!(
                        NativeOp::NotEqual,
                        &regs[left.index()],
                        &Value::Int(i64::from(imm)),
                        |l: &i64, r: &i64| l != r,
                        offset
                    ),
                    Instr::Jump { offset } => ip = ip.wrapping_add_signed(offset as isize),
                    Instr::JumpIfFalse { cond, offset } => match &regs[cond.index()] {
                        Value::Bool(true) => {}
                        Value::Bool(false) => ip = ip.wrapping_add_signed(offset as isize),
                        value => break Exit::Error(VmError::native(not_a_bool(value))),
                    },
                    Instr::JumpIfTrue { cond, offset } => match &regs[cond.index()] {
                        Value::Bool(true) => ip = ip.wrapping_add_signed(offset as isize),
                        Value::Bool(false) => {}
                        value => break Exit::Error(VmError::native(not_a_bool(value))),
                    },
                    Instr::Call { base, args } => break Exit::Call { base, args },
                    Instr::CallVec { base, args } => break Exit::CallVec { base, args },
                    Instr::CallNative {
                        dst,
                        base,
                        slot,
                        args,
                    } => {
                        let native = global_native(globals, usize::from(slot));
                        let NativeFunc::Simple(function) = &native.func else {
                            break Exit::CallNative {
                                dst,
                                base,
                                slot,
                                args,
                            };
                        };
                        let arguments = &mut regs[base.index()..base.index() + usize::from(args)];
                        let result = function(arguments);
                        clear(arguments);
                        match result {
                            Ok(value) => regs[dst.index()] = value,
                            Err(error) => break Exit::Error(error),
                        }
                    }
                    Instr::GetCell { dst, src } => {
                        regs[dst.index()] = match &regs[src.index()] {
                            Value::Cell(cell) => cell.borrow().clone(),
                            value => value.clone(),
                        };
                    }
                    Instr::SetCell { cell, src } => {
                        let value = std::mem::replace(&mut regs[src.index()], Value::None);
                        match &regs[cell.index()] {
                            Value::Cell(cell) => *cell.borrow_mut() = value,
                            _ => regs[cell.index()] = value,
                        }
                    }
                    Instr::NewCell { reg } => {
                        regs[reg.index()] = Value::Cell(Rc::new(RefCell::new(Value::unit())));
                    }
                    Instr::GetUpvalue { dst, index } => {
                        regs[dst.index()] = frame.upvalues()[usize::from(index)].borrow().clone();
                    }
                    Instr::SetUpvalue { index, src } => {
                        *frame.upvalues()[usize::from(index)].borrow_mut() =
                            std::mem::replace(&mut regs[src.index()], Value::None);
                    }
                    Instr::Closure { dst, index } => {
                        regs[dst.index()] =
                            make_closure(&chunk.closures()[index as usize], regs, frame.upvalues());
                    }
                    Instr::Memoize { reg } => memoize(&mut regs[reg.index()]),
                    Instr::MakeTuple { dst, base, len } => {
                        let elements =
                            take_all(&mut regs[base.index()..base.index() + usize::from(len)]);
                        regs[dst.index()] = Value::Object(Rc::new(Object::Tuple(elements)));
                    }
                    Instr::MakeList { dst, base, len } => {
                        let elements =
                            take_all(&mut regs[base.index()..base.index() + usize::from(len)]);
                        regs[dst.index()] = Value::list(elements);
                    }
                    Instr::MakeMap {
                        dst,
                        base,
                        pairs,
                        default,
                    } => regs[dst.index()] = make_map(regs, base, pairs, default),
                    Instr::ListPush { list, src } => list_push(regs, list, src),
                    Instr::MapInsert { map, key, value } => map_insert(regs, map, key, value),
                    Instr::MakeRange { dst, start, end } => {
                        match make_range(&regs[start.index()], Some(&regs[end.index()]), false) {
                            Ok(range) => regs[dst.index()] = range,
                            Err(message) => break Exit::Error(VmError::native(message)),
                        }
                    }
                    Instr::MakeRangeInclusive { dst, start, end } => {
                        match make_range(&regs[start.index()], Some(&regs[end.index()]), true) {
                            Ok(range) => regs[dst.index()] = range,
                            Err(message) => break Exit::Error(VmError::native(message)),
                        }
                    }
                    Instr::MakeRangeFrom { dst, start } => {
                        match make_range(&regs[start.index()], None, false) {
                            Ok(range) => regs[dst.index()] = range,
                            Err(message) => break Exit::Error(VmError::native(message)),
                        }
                    }
                    Instr::TupleIndex { dst, src, index } => {
                        let (src, dst, index) = (src.index(), dst.index(), i64::from(index));
                        let result = if src == dst {
                            tuple_element(&regs[src], index)
                                .cloned()
                                .map(|element| regs[dst] = element)
                        } else {
                            let (tuple, out) = read_and_write(regs, src, dst);
                            tuple_element(tuple, index).map(|element| out.clone_from(element))
                        };
                        if let Err(message) = result {
                            break Exit::Error(VmError::native(message));
                        }
                    }
                    Instr::ListIndex { dst, list, index } => {
                        match list_element(&regs[list.index()], &regs[index.index()]) {
                            Some(element) => regs[dst.index()] = element,
                            None => {
                                break Exit::Error(list_index_error(
                                    &regs[list.index()],
                                    &regs[index.index()],
                                ));
                            }
                        }
                    }
                    Instr::ListSet { list, index, value } => {
                        let value = std::mem::replace(&mut regs[value.index()], Value::None);
                        if let Err(error) =
                            list_set(&regs[list.index()], &regs[index.index()], value)
                        {
                            break Exit::Error(error);
                        }
                    }
                    Instr::CheckType { reg, typ } => {
                        let target = &chunk.types()[typ as usize];
                        if !convert(&mut regs[reg.index()], target) {
                            let message = cannot_cast(&regs[reg.index()], target);
                            break Exit::Error(VmError::native(message));
                        }
                    }
                    Instr::TryCast { reg, typ } => {
                        try_cast_value(&mut regs[reg.index()], &chunk.types()[typ as usize]);
                    }
                    Instr::GetIterator { dst, src } => {
                        let src = &regs[src.index()];
                        let iterated = if is_list_or_tuple(src) {
                            Ok(src.clone())
                        } else {
                            iterator_of(src)
                        };
                        match iterated {
                            Ok(iterated) => {
                                regs[dst.index()] = iterated;
                                regs[dst.index() + 1] = Value::Int(0);
                            }
                            Err(message) => break Exit::Error(VmError::native(message)),
                        }
                    }
                    Instr::IterNext { iter, dst, offset } => {
                        let (iter, position) = (iter.index(), iter.index() + 1);
                        // A position is never negative.
                        let index = match regs[position] {
                            Value::Int(index) => index as usize,
                            _ => unreachable!("internal error: GetIterator writes a position"),
                        };
                        let (iterated, out) = read_and_write(regs, iter, dst.index());
                        let Value::Object(object) = iterated else {
                            unreachable!("internal error: GetIterator writes an object");
                        };
                        // A list or tuple is read at the position after it,
                        // which moves on; any other iterator counts itself.
                        let (found, counted) = match object.as_ref() {
                            Object::Iterator(iterator) => (
                                iterator.borrow_mut().next().map(|value| *out = value),
                                false,
                            ),
                            Object::Tuple(elements) => (
                                elements.get(index).map(|element| out.clone_from(element)),
                                true,
                            ),
                            Object::List(elements) => (
                                elements
                                    .borrow()
                                    .get(index)
                                    .map(|element| out.clone_from(element)),
                                true,
                            ),
                            _ => unreachable!("internal error: GetIterator writes an iterable"),
                        };
                        if found.is_none() {
                            ip = short_jump(ip, offset, far_jumps);
                        } else if counted {
                            regs[position] = Value::Int(index as i64 + 1);
                        }
                    }
                    Instr::ForRange { counter, inclusive } => {
                        if let Err(message) = start_range(regs, counter.index(), inclusive) {
                            break Exit::Error(VmError::native(message));
                        }
                    }
                    Instr::ForRangeNext {
                        counter,
                        dst,
                        offset,
                    } => match (&regs[counter.index()], &regs[counter.index() + 1]) {
                        (&Value::Int(next), &Value::Int(last)) if next <= last => {
                            regs[dst.index()] = Value::Int(next);
                            regs[counter.index()] = match next.checked_add(1) {
                                Some(after) => Value::Int(after),
                                None => Value::None,
                            };
                        }
                        _ => ip = short_jump(ip, offset, far_jumps),
                    },
                    Instr::Unpack { src, base, len } => {
                        let (src, base) = (src.index(), base.index());
                        let end = base + usize::from(len);
                        let result = if src < base {
                            let (below, rest) = regs.split_at_mut(base);
                            unpack_into(&below[src], &mut rest[..end - base])
                        } else if src >= end {
                            let (below, rest) = regs.split_at_mut(end);
                            unpack_into(&rest[src - end], &mut below[base..])
                        } else {
                            // The value is in one of the registers it's
                            // unpacked to.
                            let value = regs[src].clone();
                            unpack_into(&value, &mut regs[base..end])
                        };
                        if let Err(message) = result {
                            break Exit::Error(VmError::native(message));
                        }
                    }
                    Instr::Return { src } => break Exit::Return { src },
                    Instr::Halt { src } => break Exit::Halt { src },
                }
            };

            // The one place the instruction pointer is written back.
            let frame = self
                .frames
                .last_mut()
                .expect("internal error: a running VM has a frame");
            frame.ip = ip as u32;
            let base = frame.base as usize;
            let result = match exit {
                Exit::Call { base: callee, args } => {
                    self.call_at(base + callee.index(), usize::from(args))
                }
                Exit::CallVec { base: callee, args } => {
                    let span = frame.function.body.span(ip - 1);
                    self.call_vec_at(base + callee.index(), usize::from(args), span)
                }
                Exit::CallNative {
                    dst,
                    base: first,
                    slot,
                    args,
                } => {
                    let native = Rc::clone(global_native(&self.globals, usize::from(slot)));
                    self.run_native(&native, base + first.index(), usize::from(args))
                        .map(|value| self.stack[base + dst.index()] = value)
                }
                Exit::Return { src } => {
                    let result = self.return_from_call(base, src);
                    if result.is_ok() && self.frames.len() == target_depth {
                        return Ok(());
                    }
                    result
                }
                Exit::Halt { src } => {
                    self.result =
                        std::mem::replace(&mut self.stack[base + src.index()], Value::None);
                    return Ok(());
                }
                Exit::Error(error) => Err(error),
            };
            if let Err(mut error) = result {
                let frame = self
                    .frames
                    .last()
                    .expect("internal error: a running VM has a frame");
                error
                    .span
                    .get_or_insert_with(|| frame.function.body.span(frame.ip as usize - 1));
                return Err(error);
            }
        }
    }

    /// Ends the call whose frame is on top, which starts at `base`, giving
    /// its caller the value in `src`: the caller's callee register, the one
    /// below the frame, gets it. A checked call's result is checked here, so
    /// that a wrong one fails at the call, the caller's instruction.
    fn return_from_call(&mut self, base: usize, src: Reg) -> Result<(), VmError> {
        let (frame_size, on_return) = self.frames.pop();
        let mut value = std::mem::replace(&mut self.stack[base + src.index()], Value::None);
        clear(&mut self.stack[base..base + frame_size]);
        if let Some(on_return) = on_return {
            if let Some((cache, key)) = on_return.memo {
                cache.borrow_mut().insert(key, value.clone());
            }
            if let Some(returns) = on_return.returns {
                let result = convert_returned(&mut value, &returns);
                self.stack[base - 1] = value;
                return result.map_err(VmError::native);
            }
        }
        // The top level halts, so a frame that returns has a caller, whose
        // callee register is the one below it.
        self.stack[base - 1] = value;
        Ok(())
    }

    /// Calls the function at `callee`, an index in the stack, with the `args`
    /// values after it: pushes its frame, or for a function that has no code
    /// of its own, as a native, runs it and puts what it returns at `callee`.
    fn call_at(&mut self, callee: usize, args: usize) -> Result<(), VmError> {
        let function = match &self.stack[callee] {
            Value::Object(object) => match object.as_ref() {
                // The usual calls, which the frame takes the function of
                // without copying anything.
                Object::Function(Function::Compiled(function)) => {
                    let function = Rc::clone(function);
                    return self.enter_frame(function, None, callee, None);
                }
                Object::Function(Function::Closure(closure)) => {
                    let function = Rc::clone(&closure.prototype);
                    let closure = Some(Rc::clone(object));
                    return self.enter_frame(function, closure, callee, None);
                }
                Object::Function(function) => function.clone(),
                other => return Err(unable_to_invoke(&other.static_type())),
            },
            other => return Err(unable_to_invoke(&other.static_type())),
        };
        self.dispatch_call(function, callee, args, None)
    }

    /// Calls `function`, whose callee register is `callee`, an index in the
    /// stack, with the `args` values after it. `memo` is where the result
    /// goes too, for a call of a `pure fn` whose result isn't cached yet.
    fn dispatch_call(
        &mut self,
        function: Function,
        callee: usize,
        args: usize,
        memo: Option<PendingMemo>,
    ) -> Result<(), VmError> {
        match function {
            Function::Compiled(function) => self.enter_frame(function, None, callee, memo),
            Function::Closure(closure) => {
                let function = Rc::clone(&closure.prototype);
                let closure = Rc::new(Object::Function(Function::Closure(closure)));
                self.enter_frame(function, Some(closure), callee, memo)
            }
            Function::Native(native) => {
                let value = self.run_native(&native, callee + 1, args)?;
                if let Some((cache, key)) = memo {
                    cache.borrow_mut().insert(key, value.clone());
                }
                self.stack[callee] = value;
                Ok(())
            }
            Function::Memoized { cache, function } => {
                let arguments = callee + 1..callee + 1 + args;
                let key = MemoKey::of(&self.stack[arguments.clone()]);
                let cached = cache.borrow().get(key).cloned();
                if let Some(cached) = cached {
                    clear(&mut self.stack[arguments]);
                    self.stack[callee] = cached;
                    return Ok(());
                }
                self.dispatch_call(
                    Rc::unwrap_or_clone(function),
                    callee,
                    args,
                    Some((cache, key)),
                )
            }
            Function::Checked { function, typ } => self.call_checked(function, *typ, callee, args),
        }
    }

    /// Calls the function a [`Function::Checked`] checks, and checks that
    /// what it returns is what `typ`, the function type it's used as, says:
    /// right away when the call completes here, or when its frame returns.
    #[cold]
    fn call_checked(
        &mut self,
        function: Rc<Function>,
        typ: StaticType,
        callee: usize,
        args: usize,
    ) -> Result<(), VmError> {
        let StaticType::Function { return_type, .. } = typ else {
            unreachable!("internal error: a checked function is used as a function type");
        };
        // The function type it's used as may take more than the function
        // does, as `fn(Any) -> Int` would of a function taking an Int.
        let arguments = &self.stack[callee + 1..callee + 1 + args];
        if !function.conforms_value_args(arguments) {
            return Err(refused_arguments(arguments));
        }
        let depth = self.frames.len();
        self.dispatch_call(Rc::unwrap_or_clone(function), callee, args, None)?;
        if self.frames.len() > depth {
            let frame = self
                .frames
                .last_mut()
                .expect("internal error: a frame was pushed");
            frame.on_return.get_or_insert_default().returns = Some(*return_type);
            return Ok(());
        }
        convert_returned(&mut self.stack[callee], &return_type).map_err(VmError::native)
    }

    /// Pushes the frame of a call of `function`, which `closure` is a
    /// closure of if it's one, whose callee register is `callee`, an index
    /// in the stack: its parameters are the registers after it. The
    /// registers after its parameters keep what they hold, as the
    /// function's code writes each of its locals before reading it.
    fn enter_frame(
        &mut self,
        function: Rc<CompiledFunction>,
        closure: Option<Rc<Object>>,
        callee: usize,
        memo: Option<PendingMemo>,
    ) -> Result<(), VmError> {
        // The frames live on the heap, so without a limit a function that
        // calls itself without end would take memory until the system stops
        // the program.
        if self.frames.len() >= MAX_CALL_DEPTH {
            return Err(calls_too_deep());
        }
        let base = callee + 1;
        // More registers than a `u32` counts would take more memory than a
        // machine has, deep in a recursion.
        let Ok(first) = u32::try_from(base) else {
            return Err(calls_too_deep());
        };
        if self.stack.len() < base + WINDOW {
            self.stack.resize(base + WINDOW, Value::None);
        }
        let on_return = memo.map(|memo| {
            Box::new(OnReturn {
                memo: Some(memo),
                returns: None,
            })
        });
        self.frames.push(function, closure, first, on_return);
        Ok(())
    }

    /// Calls `callee`'s function, an operator applied to each element of
    /// tuples (see [`Vm::call_vec`]), with the `args` values after it, and
    /// puts the tuple of the results at `callee`.
    fn call_vec_at(&mut self, callee: usize, args: usize, span: Span) -> Result<(), VmError> {
        let arguments = take_all(&mut self.stack[callee + 1..callee + 1 + args]);
        let function = std::mem::replace(&mut self.stack[callee], Value::None);
        self.stack[callee] = self.call_vec(&function, &arguments, span)?;
        Ok(())
    }

    /// Runs `native` with the `args` values from `first` on in the stack,
    /// which are `None` afterwards, and returns what it returns.
    fn run_native(
        &mut self,
        native: &NativeFunction,
        first: usize,
        args: usize,
    ) -> Result<Value, VmError> {
        let arguments = &mut self.stack[first..first + args];
        match &native.func {
            NativeFunc::Simple(function) => {
                let result = function(arguments);
                clear(arguments);
                result
            }
            NativeFunc::WithVm(function) => {
                let arguments = take_all(arguments);
                function(&arguments, self)
            }
        }
    }

    /// Calls `function` with `args` for a native that calls back into the
    /// program, as `map` does, and returns what it returns. The call runs
    /// on this VM, above the frame on top, which is the one that called the
    /// native: its callee register first, then the arguments, which are
    /// copies of `args`.
    pub fn call_callback(&mut self, function: Function, args: &[Value]) -> Result<Value, VmError> {
        // A native runs on `args` where they are: `(a, b) - (c, d)` calls
        // the native `-` for each element.
        if let Function::Native(native) = &function {
            return match &native.func {
                NativeFunc::Simple(function) => function(args),
                NativeFunc::WithVm(function) => function(args, self),
            };
        }
        let top = self
            .frames
            .last()
            .expect("internal error: a native runs in a frame");
        let callee = top.base as usize + top.function.frame_size;
        let end = callee + 1 + args.len();
        if self.stack.len() < end {
            self.stack.resize(end, Value::None);
        }
        self.stack[callee + 1..end].clone_from_slice(args);
        let depth = self.frames.len();
        let result = self
            .dispatch_call(function, callee, args.len(), None)
            .and_then(|()| {
                if self.frames.len() > depth {
                    self.run_to_depth(depth)
                } else {
                    Ok(())
                }
            });
        if result.is_err() {
            // A native may go on after a callback failed, from the frame
            // that called it.
            self.frames.truncate(depth);
        }
        result?;
        Ok(std::mem::replace(&mut self.stack[callee], Value::None))
    }

    /// The value of the program's last expression, once it ran: unit if
    /// that's a `;`-statement or a declaration.
    pub fn last_value(&self) -> Value {
        self.result.clone()
    }

    /// The globals as the program left them, for the next REPL line, and
    /// what is left on the stack: the registers of the top-level function,
    /// and of every function the program was in if it failed.
    pub fn into_globals_and_stack(self) -> (Vec<Value>, Vec<Value>) {
        (self.globals, self.stack)
    }

    /// Calls `callee`, an operator the analyser picked for each element
    /// position of the tuples among `arguments`, element by element, and
    /// returns the tuple of the results: `(1, 2) + (3, 4)` is
    /// `(1 + 3, 2 + 4)`. The callee is one function for every position, or
    /// a tuple of one for each. An argument that isn't a tuple is used at
    /// every position: `(1, 2) * 3` is `(3, 6)`. An error is at `span`, the
    /// call's, unless it has a place of its own.
    #[inline(never)]
    fn call_vec(
        &mut self,
        callee: &Value,
        arguments: &[Value],
        span: Span,
    ) -> Result<Value, VmError> {
        let unable = |callee: StaticType| {
            VmError::new(format!("Unable to invoke {callee} as a function."), span)
        };
        let Value::Object(callee) = callee else {
            return Err(unable(callee.static_type()));
        };
        let at_position = |i: usize| -> Option<&Function> {
            match callee.as_ref() {
                Object::Function(f) => Some(f),
                Object::Tuple(overloads) => match overloads.get(i)? {
                    Value::Object(obj) => match obj.as_ref() {
                        Object::Function(f) => Some(f),
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            }
        };
        let Some(first) = at_position(0) else {
            return Err(unable(callee.as_ref().static_type()));
        };
        let name = first.name();
        let Some(axis_len) = vec_axis_len(arguments) else {
            return Err(VmError::new(
                format!(
                    "vec call '{}' requires tuple arguments of equal non-zero length",
                    name.unwrap_or("?")
                ),
                span,
            ));
        };

        let mut elem_args: Vec<Value> = Vec::with_capacity(arguments.len());
        let mut results: Vec<Value> = Vec::with_capacity(axis_len);
        for i in 0..axis_len {
            elem_args.clear();
            elem_args.extend(arguments.iter().map(|arg| vec_element_at(arg, i)));
            let scalar = at_position(i)
                .cloned()
                .ok_or_else(|| VmError::new(format!("no function for tuple position {i}"), span))?;
            let result = self.call_callback(scalar, &elem_args).map_err(|mut e| {
                let prefix = match name {
                    Some(name) => format!("while vectorising '{name}' at index {i}: "),
                    None => format!("while vectorising at index {i}: "),
                };
                e.message = format!("{prefix}{}", e.message);
                e.span.get_or_insert(span);
                e
            })?;
            results.push(result);
        }
        Ok(Value::Object(Rc::new(Object::Tuple(results))))
    }
}

/// Writes the `out.len()` elements of `value` to `out`, in order, or says
/// why it can't: `(1, 2)` unpacks into two values, but not into three.
#[inline(never)]
fn unpack_into(value: &Value, out: &mut [Value]) -> Result<(), String> {
    let size = out.len();
    let Value::Object(obj) = value else {
        return Err("cannot unpack: expected a list, tuple, or string".to_string());
    };
    match obj.as_ref() {
        Object::List(_) | Object::Tuple(_) => {
            let type_name = match obj.as_ref() {
                Object::List(_) => "list",
                Object::Tuple(_) => "tuple",
                _ => unreachable!(),
            };
            let len = match obj.as_ref() {
                Object::List(seq) => seq.borrow().len(),
                Object::Tuple(seq) => seq.len(),
                _ => unreachable!(),
            };
            if len != size {
                return Err(format!(
                    "cannot unpack a {type_name} of length {len} into {size} variables",
                ));
            }
            match obj.as_ref() {
                Object::List(seq) => out.clone_from_slice(&seq.borrow()),
                Object::Tuple(seq) => out.clone_from_slice(seq),
                _ => unreachable!(),
            }
        }
        Object::String(s) => {
            let s = s.borrow();
            let mut iter = s.chars();
            for (unpacked, slot) in out.iter_mut().enumerate() {
                match iter.next() {
                    Some(c) => *slot = Value::string(c.to_string()),
                    None => {
                        return Err(format!(
                            "cannot unpack a string of length {unpacked} into {size} variables",
                        ));
                    }
                }
            }
            if iter.next().is_some() {
                return Err(format!(
                    "cannot unpack a string into {size} variables: string is too long",
                ));
            }
        }
        // A deque, heap or iterator unpacks the elements it gives, one
        // past the variables at most, as an iterator may not end.
        _ => {
            let Some(iter) = value.iter() else {
                return Err(
                    "cannot unpack: expected a list, tuple, string, deque, heap or iterator"
                        .to_string(),
                );
            };
            let elements: Vec<Value> = iter.take(size + 1).collect();
            if elements.len() != size {
                return Err(if elements.len() > size {
                    format!("cannot unpack more than {size} values into {size} variables")
                } else {
                    format!(
                        "cannot unpack {} values into {size} variables",
                        elements.len()
                    )
                });
            }
            for (slot, element) in out.iter_mut().zip(elements) {
                *slot = element;
            }
        }
    }
    Ok(())
}

/// The register `read`, to read, and the register `write`, a different one,
/// to write.
#[inline(always)]
fn read_and_write(regs: &mut [Value; WINDOW], read: usize, write: usize) -> (&Value, &mut Value) {
    if read < write {
        let (below, rest) = regs.split_at_mut(write);
        (&below[read], &mut rest[0])
    } else {
        let (below, rest) = regs.split_at_mut(read);
        (&rest[0], &mut below[write])
    }
}

/// Starts [`Instr::ForRange`]: turns the range in register `counter`, a
/// `Range` or `RangeFrom`, or the range from the `Int` there to the `Int`
/// in the one after it, which includes that end if `inclusive`, into its
/// first value, or `None` if it has none, and its last. It runs once per
/// loop, so it stays out of the dispatch loop's code.
#[inline(never)]
fn start_range(regs: &mut [Value; WINDOW], counter: usize, inclusive: bool) -> Result<(), String> {
    if let Value::Object(object) = &regs[counter] {
        let bounds = match object.as_ref() {
            Object::Range(range) => range.bounds(),
            Object::RangeFrom(start) => Some((*start, i64::MAX)),
            _ => return Err(bad_range_bound(&regs[counter])),
        };
        (regs[counter], regs[counter + 1]) = match bounds {
            Some((first, last)) => (Value::Int(first), Value::Int(last)),
            None => (Value::None, Value::None),
        };
        return Ok(());
    }
    let (start, end) = (&regs[counter], &regs[counter + 1]);
    let (&Value::Int(start), &Value::Int(end)) = (start, end) else {
        let bad = if matches!(end, Value::Int(_)) {
            start
        } else {
            end
        };
        return Err(bad_range_bound(bad));
    };
    let last = if inclusive {
        Some(end)
    } else {
        end.checked_sub(1)
    };
    (regs[counter], regs[counter + 1]) = match last {
        Some(last) => (Value::Int(start), Value::Int(last)),
        None => (Value::None, Value::Int(end)),
    };
    Ok(())
}

/// Whether `value` is a list or tuple, which `IterNext` iterates over in
/// place.
fn is_list_or_tuple(value: &Value) -> bool {
    matches!(value, Value::Object(object) if matches!(object.as_ref(), Object::List(_) | Object::Tuple(_)))
}

/// An iterator over `value`: the value itself if it's an iterator, and a
/// cursor over its elements (see [`ValueIter`]) if it's a deque, string,
/// map, heap or range.
#[inline(never)]
fn iterator_of(value: &Value) -> Result<Value, String> {
    match value.iter() {
        Some(ValueIter::Shared(_)) => Ok(value.clone()),
        Some(cursor) => Ok(Value::iterator(Rc::new(RefCell::new(cursor)))),
        None => Err(format!("{} is not iterable", value.static_type())),
    }
}

/// Find the broadcast-axis length for a vec call: the shared length of every
/// tuple-shaped argument. Empty tuples and length mismatches return `None`,
/// which `CallVec` reports as an error.
fn vec_axis_len(args: &[Value]) -> Option<usize> {
    let mut axis: Option<usize> = None;
    for arg in args {
        if let Value::Object(obj) = arg
            && let Object::Tuple(elems) = obj.as_ref()
        {
            if elems.is_empty() {
                return None;
            }
            match axis {
                None => axis = Some(elems.len()),
                Some(n) if n == elems.len() => {}
                _ => return None,
            }
        }
    }
    axis
}

/// Pick the per-element value at vec position `i`. Tuple args contribute
/// `tuple[i]`; non-tuple args broadcast unchanged.
fn vec_element_at(arg: &Value, i: usize) -> Value {
    if let Value::Object(obj) = arg
        && let Object::Tuple(elems) = obj.as_ref()
    {
        return elems[i].clone();
    }
    arg.clone()
}

/// A callable VM function bound to the parent VM, for natives that call back
/// into the program ([`NativeFunc::WithVm`]). Produced by the
/// `&mut VmCallable` input-type handler in `ndc_macros::vm_convert`.
///
/// Calling `call()` runs the function inline on the parent VM via
/// [`Vm::call_callback`].
pub struct VmCallable<'a> {
    pub function: Function,
    pub vm: &'a mut Vm,
}

impl VmCallable<'_> {
    /// Call this function with the given arguments, running inline on the
    /// parent VM.
    pub fn call(&mut self, args: &[Value]) -> Result<Value, VmError> {
        self.vm.call_callback(self.function.clone(), args)
    }
}

/// What a call fails with when it would go more than [`MAX_CALL_DEPTH`]
/// calls deep.
#[cold]
fn calls_too_deep() -> VmError {
    VmError::native(format!(
        "calls go more than {MAX_CALL_DEPTH} deep, which usually means a function calls itself without end"
    ))
}

/// What a call of a [`Function::Checked`] fails with when the function it
/// checks doesn't take `arguments`.
#[cold]
fn refused_arguments(arguments: &[Value]) -> VmError {
    VmError::native(format!(
        "the function doesn't take the arguments ({})",
        arguments
            .iter()
            .map(|argument| argument.diagnostic_type().to_string())
            .collect::<Vec<_>>()
            .join(", ")
    ))
}

/// The analyser makes a condition a `Bool`; this is what a VM that's given
/// something else says.
#[cold]
fn not_a_bool(value: &Value) -> String {
    format!(
        "mismatched types: expected Bool, found {}",
        value.diagnostic_type()
    )
}

/// The analyser makes a range bound an `Int`; this is what a VM that's
/// given something else says.
#[cold]
fn bad_range_bound(value: &Value) -> String {
    format!("range bounds must be Int, got {}", value.diagnostic_type())
}

/// The range from `start` to `end`, which includes `end` if `inclusive`,
/// or which doesn't end without one: `1..4`, `1..=3`, `1..`. The bounds
/// are `Int`s; `end` is checked first.
fn make_range(start: &Value, end: Option<&Value>, inclusive: bool) -> Result<Value, String> {
    let end = match end {
        Some(Value::Int(end)) => Some(*end),
        Some(end) => return Err(bad_range_bound(end)),
        None => None,
    };
    let Value::Int(start) = *start else {
        return Err(bad_range_bound(start));
    };
    Ok(match end {
        Some(end) => Value::range(Range::new(start, end, inclusive)),
        None => Value::range_from(start),
    })
}

/// Replaces `value` with `Some` of it, made the `target` as
/// [`Value::converted_to`] does, or with `None` when it isn't one.
#[cold]
fn try_cast_value(value: &mut Value, target: &StaticType) {
    let taken = std::mem::replace(value, Value::None);
    *value = Value::option(taken.converted_to(target).ok());
}

/// The element of `tuple` at `index`, counting a negative index from the
/// end: `-1` is the last element.
#[inline(always)]
fn tuple_element(tuple: &Value, index: i64) -> Result<&Value, String> {
    if let Value::Object(object) = tuple
        && let Object::Tuple(elements) = object.as_ref()
    {
        let position = if index < 0 {
            elements.len().checked_sub(index.unsigned_abs() as usize)
        } else {
            usize::try_from(index).ok()
        };
        return match position.and_then(|position| elements.get(position)) {
            Some(element) => Ok(element),
            None => Err(tuple_index_out_of_bounds(index, elements.len())),
        };
    }
    Err(cannot_index_into(tuple))
}

#[cold]
fn cannot_index_into(value: &Value) -> String {
    format!("cannot index into {}", value.static_type())
}

/// The analyser rejects an index written in the code that's outside the
/// tuple; this is what a VM that's given one says.
#[cold]
fn tuple_index_out_of_bounds(index: i64, length: usize) -> String {
    let plural = if length == 1 { "" } else { "s" };
    format!("index {index} is out of bounds for a tuple of {length} element{plural}")
}

/// Checks that `value` is a `target`, and makes it one: a function that may
/// be a function `target` gets its calls checked, and a container something
/// else holds is copied (see [`Value::converted_to`]). A value that isn't a
/// `target` is left as it was.
fn convert(value: &mut Value, target: &StaticType) -> bool {
    let taken = std::mem::replace(value, Value::None);
    let (converted, is_a) = match taken.converted_to(target) {
        Ok(converted) => (converted, true),
        Err(unchanged) => (unchanged, false),
    };
    *value = converted;
    is_a
}

/// Checks that `returned`, the result of a call of a [`Function::Checked`],
/// is a `returns`, and makes it one, as [`convert`] does: a function it
/// returns may get its own calls checked.
fn convert_returned(returned: &mut Value, returns: &StaticType) -> Result<(), String> {
    if convert(returned, returns) {
        return Ok(());
    }
    Err(format!(
        "expected the function to return {returns} but it returned {}",
        returned.diagnostic_type()
    ))
}

/// What a cast of `value` that fails says: `cannot cast String to Int`.
#[cold]
fn cannot_cast(value: &Value, target: &StaticType) -> String {
    format!("cannot cast {} to {target}", value.diagnostic_type())
}

/// A closure of `code`, made in a frame whose registers are `regs` and
/// which is a closure over `upvalues`: it shares the cells of the locals it
/// captures, moving a local into a new cell on its first capture, and its
/// maker's own captured variables.
#[inline(never)]
fn make_closure(
    code: &ClosureCode,
    regs: &mut [Value; WINDOW],
    upvalues: &[Rc<RefCell<Value>>],
) -> Value {
    let upvalues = code
        .captures
        .iter()
        .map(|capture| match *capture {
            CaptureSource::Local(slot) => capture_local(&mut regs[slot]),
            CaptureSource::Upvalue(index) => Rc::clone(&upvalues[index]),
        })
        .collect();
    Value::function(Function::Closure(ClosureFunction {
        prototype: Rc::clone(&code.function),
        upvalues,
    }))
}

/// The cell a closure shares with the local in `register`. On the first
/// capture the local's value moves into a new cell, which takes its place,
/// so the frame and every closure see one variable.
fn capture_local(register: &mut Value) -> Rc<RefCell<Value>> {
    if let Value::Cell(cell) = register {
        return Rc::clone(cell);
    }
    let cell = Rc::new(RefCell::new(std::mem::replace(register, Value::None)));
    *register = Value::Cell(Rc::clone(&cell));
    cell
}

/// A new map of the `pairs` keys and values from `base` on in `regs`, a key
/// then its value, with the value after them as its default if it has a
/// `default`. The registers are `None` afterwards.
#[inline(never)]
fn make_map(regs: &mut [Value; WINDOW], base: Reg, pairs: u16, default: bool) -> Value {
    let pairs = usize::from(pairs);
    let values = &mut regs[base.index()..base.index() + 2 * pairs + usize::from(default)];
    let mut entries = HashMap::with_capacity_and_hasher(pairs, Default::default());
    for [key, value] in values[..2 * pairs].as_chunks_mut::<2>().0 {
        let key = std::mem::replace(key, Value::None);
        entries.insert(key, std::mem::replace(value, Value::None));
    }

    let default = default.then(|| std::mem::replace(&mut values[2 * pairs], Value::None));
    Value::Object(Rc::new(Object::map(entries, default)))
}

/// Appends the value in `src` to the list in `list`, leaving `None` in
/// `src`.
#[inline(never)]
fn list_push(regs: &mut [Value; WINDOW], list: Reg, src: Reg) {
    debug_assert_ne!(list, src, "internal error: a list is pushed onto itself");
    let value = std::mem::replace(&mut regs[src.index()], Value::None);
    let Value::Object(object) = &regs[list.index()] else {
        unreachable!("internal error: ListPush's register holds a list");
    };
    let Object::List(list) = object.as_ref() else {
        unreachable!("internal error: ListPush's register holds a list");
    };
    list.borrow_mut().push(value);
}

/// Puts the value in `value` under the key in `key` in the map in `map`,
/// leaving `None` in both.
#[inline(never)]
fn map_insert(regs: &mut [Value; WINDOW], map: Reg, key: Reg, value: Reg) {
    debug_assert!(
        map != key && map != value && key != value,
        "internal error: MapInsert's registers are different"
    );
    let key = std::mem::replace(&mut regs[key.index()], Value::None);
    let value = std::mem::replace(&mut regs[value.index()], Value::None);
    let Value::Object(object) = &regs[map.index()] else {
        unreachable!("internal error: MapInsert's register holds a map");
    };
    let Object::Map { entries, .. } = object.as_ref() else {
        unreachable!("internal error: MapInsert's register holds a map");
    };
    entries.borrow_mut().insert(key, value);
}

/// Puts the function in `register` in a memoising wrapper with a new cache.
#[inline(never)]
fn memoize(register: &mut Value) {
    let Value::Object(object) = register else {
        unreachable!("internal error: Memoize's register holds a function");
    };
    let Object::Function(function) = object.as_ref() else {
        unreachable!("internal error: Memoize's register holds a function");
    };
    *register = Value::function(Function::Memoized {
        cache: Rc::default(),
        function: Rc::new(function.clone()),
    });
}

/// Leaves `None` in each of `registers`, dropping what they held: the
/// arguments of a native once it returns, or the frame a call leaves. A
/// loop rather than `fill`, which clones a `None` into each in a function
/// of its own.
#[inline(always)]
fn clear(registers: &mut [Value]) {
    for register in registers {
        *register = Value::None;
    }
}

/// The values of `registers`, which are `None` afterwards.
fn take_all(registers: &mut [Value]) -> Vec<Value> {
    registers
        .iter_mut()
        .map(|register| std::mem::replace(register, Value::None))
        .collect()
}

/// What a call of something that isn't a function, of type `typ`, fails
/// with.
#[cold]
fn unable_to_invoke(typ: &StaticType) -> VmError {
    VmError::native(format!("Unable to invoke {typ} as a function."))
}

/// The native in global slot `slot`, where the compiler only writes a
/// `CallNative` for a native.
fn global_native(globals: &[Value], slot: usize) -> &Rc<NativeFunction> {
    match &globals[slot] {
        Value::Object(object) => match object.as_ref() {
            Object::Function(Function::Native(native)) => native,
            _ => unreachable!("internal error: global slot {slot} holds no native"),
        },
        _ => unreachable!("internal error: global slot {slot} holds no native"),
    }
}

/// A new string with the text of `constant`, a string.
fn new_string(constant: &Value) -> Value {
    let Value::Object(object) = constant else {
        unreachable!("internal error: MakeString's constant is a string");
    };
    let Object::String(text) = object.as_ref() else {
        unreachable!("internal error: MakeString's constant is a string");
    };
    Value::string(text.borrow().as_str())
}

/// Does the binary `op` of the registers `left` and `right` into `dst`.
#[inline(never)]
fn binary(
    regs: &mut [Value; WINDOW],
    op: NativeOp,
    dst: Reg,
    left: Reg,
    right: Reg,
) -> Result<(), VmError> {
    regs[dst.index()] = op.binary(&regs[left.index()], &regs[right.index()])?;
    Ok(())
}

/// What the binary `op` of the registers `left` and `right` fails with, when
/// the instruction's own fast path couldn't do it: an `Int` operation that
/// overflowed, or operands of the wrong type.
#[cold]
#[inline(never)]
fn binary_error(op: NativeOp, left: &Value, right: &Value) -> VmError {
    match op.binary(left, right) {
        Err(error) => error,
        Ok(_) => unreachable!("internal error: {op:?} failed on its fast path only"),
    }
}

/// Whether the comparison `op` of `left` and `right` holds, for values its
/// instruction's own fast path doesn't compare.
#[inline(never)]
fn compare(op: NativeOp, left: &Value, right: &Value) -> Result<bool, VmError> {
    match op.binary(left, right)? {
        Value::Bool(holds) => Ok(holds),
        _ => unreachable!("internal error: {op:?} is a comparison"),
    }
}

/// Where a jump whose offset is an `i16` goes from `ip`, the instruction
/// after it: `offset` on, or for [`FAR`], where `far_jumps` says.
#[inline(always)]
fn short_jump(ip: usize, offset: i16, far_jumps: &[(usize, usize)]) -> usize {
    if offset == FAR {
        far_target(far_jumps, ip - 1)
    } else {
        ip.wrapping_add_signed(offset as isize)
    }
}

/// Does the unary `op` of the register `src` into `dst`.
#[inline(never)]
fn unary(regs: &mut [Value; WINDOW], op: NativeOp, dst: Reg, src: Reg) -> Result<(), VmError> {
    regs[dst.index()] = op.unary(&regs[src.index()])?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chunk::Builder;
    use crate::{NativeSpec, Natives};
    use ndc_core::FunctionRegistry;

    /// The top-level function `build` writes, using `frame_size` registers.
    fn program(
        frame_size: usize,
        natives: Vec<usize>,
        build: impl FnOnce(&mut Builder),
    ) -> CompiledFunction {
        let mut builder = Builder::default();
        build(&mut builder);
        CompiledFunction {
            name: None,
            static_type: StaticType::Any,
            num_globals: 0,
            natives,
            body: builder.finish(),
            frame_size,
        }
    }

    /// Natives with one, `sum`, in slot 0, which adds up its Int arguments.
    fn natives() -> Natives {
        let mut registry = FunctionRegistry::default();
        registry.declare_global_fn(NativeSpec::with_signature(
            "sum",
            "Adds up Ints.",
            "(Int, Int) -> Int",
            || {
                NativeFunc::Simple(Box::new(|args| {
                    Ok(Value::Int(
                        args.iter()
                            .map(|arg| match arg {
                                Value::Int(n) => *n,
                                _ => 0,
                            })
                            .sum(),
                    ))
                }))
            },
        ));
        let mut natives = Natives::default();
        natives.extend(registry);
        natives
    }

    /// `fn double(n) => n + n`.
    fn double() -> CompiledFunction {
        program(1, vec![], |b| {
            emit(
                b,
                Instr::AddInt {
                    dst: Reg(0),
                    left: Reg(0),
                    right: Reg(0),
                },
            );
            emit(b, Instr::Return { src: Reg(0) });
        })
    }

    fn tuple(values: &[i64]) -> Value {
        Value::tuple(values.iter().map(|&n| Value::Int(n)).collect())
    }

    /// A checked function that returns something else than the type it's
    /// used as says fails at the call.
    #[test]
    fn a_checked_function_that_returns_the_wrong_type() {
        let span = Span::new(ndc_lexer::SourceId::SYNTHETIC, 7, 2);
        let mut double = double();
        double.static_type = StaticType::Function {
            parameters: Some(vec![StaticType::Int]),
            return_type: Box::new(StaticType::Any),
        };
        let checked = Value::function(Function::Checked {
            function: Rc::new(Function::Compiled(Rc::new(double))),
            typ: Box::new(StaticType::Function {
                parameters: Some(vec![StaticType::Int]),
                return_type: Box::new(StaticType::String),
            }),
        });
        let function = program(2, vec![], |b| {
            let k = b.constant(checked);
            emit(b, Instr::LoadK { dst: Reg(0), k });
            load_int(b, Reg(1), 2);
            b.emit(
                Instr::Call {
                    base: Reg(0),
                    args: 1,
                },
                span,
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        let error = run(&Natives::default(), function).unwrap_err();
        assert_eq!(
            error.message,
            "expected the function to return String but it returned Int"
        );
        assert_eq!(error.span, Some(span));
    }

    /// A captured local is its register's value until a closure captures
    /// it, and its cell's after; reading or writing it into its own
    /// register leaves it as it was.
    #[test]
    fn a_captured_local_before_and_after_it_has_a_cell() {
        let function = program(5, vec![], |b| {
            load_int(b, Reg(0), 5);
            emit(
                b,
                Instr::GetCell {
                    dst: Reg(1),
                    src: Reg(0),
                },
            );
            load_int(b, Reg(2), 7);
            emit(
                b,
                Instr::SetCell {
                    cell: Reg(0),
                    src: Reg(2),
                },
            );
            emit(
                b,
                Instr::GetCell {
                    dst: Reg(2),
                    src: Reg(0),
                },
            );
            emit(
                b,
                Instr::GetCell {
                    dst: Reg(0),
                    src: Reg(0),
                },
            );
            emit(b, Instr::NewCell { reg: Reg(3) });
            load_int(b, Reg(4), 9);
            emit(
                b,
                Instr::SetCell {
                    cell: Reg(3),
                    src: Reg(4),
                },
            );
            emit(
                b,
                Instr::SetCell {
                    cell: Reg(3),
                    src: Reg(3),
                },
            );
            emit(
                b,
                Instr::GetCell {
                    dst: Reg(3),
                    src: Reg(3),
                },
            );
            emit(
                b,
                Instr::MakeTuple {
                    dst: Reg(0),
                    base: Reg(0),
                    len: 4,
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        assert_eq!(
            run(&Natives::default(), function).unwrap(),
            tuple(&[7, 5, 7, 9])
        );
    }

    /// A closure shares the cell of the local it captures with the frame
    /// that made it, even when it's made in the local's own register.
    #[test]
    fn a_closure_shares_the_local_it_captures() {
        // `fn() { n = n + 1; n }`, over `n`.
        let increment = Rc::new(program(1, vec![], |b| {
            emit(
                b,
                Instr::GetUpvalue {
                    dst: Reg(0),
                    index: 0,
                },
            );
            emit(
                b,
                Instr::AddIntI {
                    dst: Reg(0),
                    left: Reg(0),
                    imm: 1,
                },
            );
            emit(
                b,
                Instr::SetUpvalue {
                    index: 0,
                    src: Reg(0),
                },
            );
            emit(
                b,
                Instr::GetUpvalue {
                    dst: Reg(0),
                    index: 0,
                },
            );
            emit(b, Instr::Return { src: Reg(0) });
        }));
        // r0 is `n`, r1 the first closure, r2 to r4 the results, and r5
        // where the calls are.
        let call = |b: &mut Builder, closure: Reg, result: Reg| {
            emit(
                b,
                Instr::Copy {
                    dst: Reg(5),
                    src: closure,
                },
            );
            emit(
                b,
                Instr::Call {
                    base: Reg(5),
                    args: 0,
                },
            );
            emit(
                b,
                Instr::Move {
                    dst: result,
                    src: Reg(5),
                },
            );
        };
        let function = program(6, vec![], |b| {
            let index = b.closure(Rc::clone(&increment), &[CaptureSource::Local(0)]);
            load_int(b, Reg(0), 1);
            emit(b, Instr::Closure { dst: Reg(1), index });
            call(b, Reg(1), Reg(2));
            // A closure made in the register of the local it captures: the
            // register holds the closure, and the closure the cell.
            emit(b, Instr::Closure { dst: Reg(0), index });
            call(b, Reg(0), Reg(3));
            call(b, Reg(1), Reg(4));
            emit(
                b,
                Instr::MakeTuple {
                    dst: Reg(0),
                    base: Reg(2),
                    len: 3,
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        assert_eq!(
            run(&Natives::default(), function).unwrap(),
            tuple(&[2, 3, 4])
        );
    }

    #[test]
    fn a_range_bound_that_isnt_an_int() {
        let function = program(2, vec![], |b| {
            load_int(b, Reg(0), 1);
            let k = b.constant(Value::Bool(true));
            emit(b, Instr::LoadK { dst: Reg(1), k });
            emit(
                b,
                Instr::MakeRange {
                    dst: Reg(0),
                    start: Reg(1),
                    end: Reg(1),
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        let error = run(&Natives::default(), function).unwrap_err();
        assert_eq!(error.message, "range bounds must be Int, got Bool");
    }

    /// Sums the elements of two tuples with `GetIterator` and `IterNext`,
    /// each iterated over in its own register, with its position in the one
    /// after it, and each element read into a register below one tuple and
    /// above the other.
    #[test]
    fn iterating_in_the_registers_of_the_operands() {
        let function = program(6, vec![], |b| {
            load_int(b, Reg(0), 0);
            for (iterated, values) in [(Reg(1), &[1, 2, 3][..]), (Reg(4), &[10, 20][..])] {
                let k = b.constant(tuple(values));
                emit(b, Instr::LoadK { dst: iterated, k });
                emit(
                    b,
                    Instr::GetIterator {
                        dst: iterated,
                        src: iterated,
                    },
                );
                let start = b.label();
                let end = b.label();
                b.bind(start);
                b.jump(
                    Instr::IterNext {
                        iter: iterated,
                        dst: Reg(3),
                        offset: 0,
                    },
                    end,
                    Span::synthetic(),
                );
                emit(
                    b,
                    Instr::AddInt {
                        dst: Reg(0),
                        left: Reg(0),
                        right: Reg(3),
                    },
                );
                b.jump(Instr::Jump { offset: 0 }, start, Span::synthetic());
                b.bind(end);
            }
            emit(b, Instr::Halt { src: Reg(0) });
        });
        assert_eq!(run(&Natives::default(), function).unwrap(), Value::Int(36));
    }

    #[test]
    fn unpack_into_the_register_of_the_value() {
        let function = program(3, vec![], |b| {
            let k = b.constant(tuple(&[4, 5, 6]));
            emit(b, Instr::LoadK { dst: Reg(1), k });
            emit(
                b,
                Instr::Unpack {
                    src: Reg(1),
                    base: Reg(0),
                    len: 3,
                },
            );
            emit(
                b,
                Instr::MakeTuple {
                    dst: Reg(0),
                    base: Reg(0),
                    len: 3,
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        assert_eq!(
            run(&Natives::default(), function).unwrap(),
            tuple(&[4, 5, 6])
        );
    }

    #[test]
    fn something_that_isnt_iterable() {
        let function = program(2, vec![], |b| {
            load_int(b, Reg(0), 3);
            emit(
                b,
                Instr::GetIterator {
                    dst: Reg(0),
                    src: Reg(0),
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        let error = run(&Natives::default(), function).unwrap_err();
        assert_eq!(error.message, "Int is not iterable");
    }

    fn run(natives: &Natives, function: CompiledFunction) -> Result<Value, VmError> {
        let mut vm = Vm::new(function, native_globals(natives), natives);
        vm.run()?;
        Ok(vm.last_value())
    }

    fn emit(builder: &mut Builder, instr: Instr) {
        builder.emit(instr, Span::synthetic());
    }

    fn load_int(builder: &mut Builder, dst: Reg, n: i64) {
        let k = builder.constant(Value::Int(n));
        emit(builder, Instr::LoadK { dst, k });
    }

    #[test]
    fn a_move_or_copy_to_its_own_register_keeps_the_value() {
        for instr in [
            Instr::Move {
                dst: Reg(0),
                src: Reg(0),
            },
            Instr::Copy {
                dst: Reg(0),
                src: Reg(0),
            },
        ] {
            let function = program(1, vec![], |b| {
                load_int(b, Reg(0), 7);
                emit(b, instr);
                emit(b, Instr::Halt { src: Reg(0) });
            });
            assert_eq!(run(&Natives::default(), function).unwrap(), Value::Int(7));
        }
    }

    #[test]
    fn a_native_empties_its_argument_registers() {
        let natives = natives();
        let function = program(3, vec![0], |b| {
            load_int(b, Reg(0), 40);
            load_int(b, Reg(1), 2);
            emit(
                b,
                Instr::CallNative {
                    dst: Reg(2),
                    base: Reg(0),
                    slot: 0,
                    args: 2,
                },
            );
            emit(b, Instr::Halt { src: Reg(1) });
        });
        assert_eq!(run(&natives, function).unwrap(), Value::None);
    }

    /// A compare-and-branch over more instructions than its offset can say
    /// goes where the chunk's far jumps say.
    #[test]
    fn a_compare_and_branch_further_than_its_offset() {
        for (n, expected) in [(5, 1), (50, 2)] {
            let function = program(2, vec![], |b| {
                let far = b.label();
                load_int(b, Reg(0), n);
                load_int(b, Reg(1), 1);
                b.jump(
                    Instr::JumpIfNotLessI {
                        left: Reg(0),
                        imm: 10,
                        offset: 0,
                    },
                    far,
                    Span::synthetic(),
                );
                for _ in 0..40_000 {
                    emit(
                        b,
                        Instr::Copy {
                            dst: Reg(1),
                            src: Reg(1),
                        },
                    );
                }
                emit(b, Instr::Halt { src: Reg(1) });
                b.bind(far);
                load_int(b, Reg(1), 2);
                emit(b, Instr::Halt { src: Reg(1) });
            });
            assert!(function.body.code()[2].is_far_jump());
            assert_eq!(function.body.jump_target(2), Some(40_004));
            assert_eq!(
                run(&Natives::default(), function).unwrap(),
                Value::Int(expected)
            );
        }
    }

    /// The analyser makes every condition a `Bool`, so no program gets
    /// here.
    #[test]
    fn a_condition_that_isnt_a_bool() {
        let function = program(1, vec![], |b| {
            let end = b.label();
            load_int(b, Reg(0), 1);
            b.jump(
                Instr::JumpIfFalse {
                    cond: Reg(0),
                    offset: 0,
                },
                end,
                Span::synthetic(),
            );
            b.bind(end);
            emit(b, Instr::Halt { src: Reg(0) });
        });
        let error = run(&Natives::default(), function).unwrap_err();
        assert_eq!(error.message, "mismatched types: expected Bool, found Int");
    }
}
