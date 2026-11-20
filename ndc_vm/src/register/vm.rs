use super::chunk::{ClosureCode, far_target};
use super::instr::{FAR, Instr, Reg, WINDOW};
use crate::error::VmError;
use crate::hash_map::HashMap;
use crate::native_op::NativeOp;
use crate::value::{ClosureFunction, CompiledFunction, Function, NativeFunc, NativeFunction};
use crate::vm::{
    MAX_CALL_DEPTH, MemoCache, calls_too_deep, cannot_cast, convert, convert_returned, iterator_of,
    make_range, memo_key, not_a_bool, refused_arguments, try_cast_value, tuple_element,
    unpack_into,
};
use crate::{Object, Value, Vm};
use ndc_analysis::CaptureSource;
use ndc_core::StaticType;
use ndc_lexer::Span;
use std::cell::RefCell;
use std::rc::Rc;

/// A running call of register code. Its registers are
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

/// The register VM's running calls, the one on top last. A call's frame is
/// written field by field into a slot kept from an earlier call that went
/// as deep, rather than made and moved in: moving a new frame in reads it
/// back from where it was made before its fields are stored, which stalls,
/// and took a quarter of the time of a call. A slot past the running calls
/// holds nothing but the function its call ran.
#[derive(Default)]
pub(crate) struct Frames {
    slots: Vec<Frame>,
    /// How many of the slots are running calls.
    depth: usize,
}

/// What a call does with what it returns before it gives it to its caller.
#[derive(Default)]
struct OnReturn {
    /// For a call of a `pure fn`: store it in the function's cache.
    memo: Option<MemoCache>,
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
    pub(crate) fn top_level(function: Rc<CompiledFunction>) -> Self {
        Self {
            slots: vec![Frame::top_level(function)],
            depth: 1,
        }
    }

    /// How many calls are running.
    pub(crate) fn len(&self) -> usize {
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

/// Why the inner loop of [`Vm::run_registers`] stopped: something it doesn't
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
    /// Runs register code until the program halts or fails, or until a call
    /// returns to `target_depth` frames, which is how a callback runs.
    ///
    /// Two loops: the inner one runs the instructions of one frame with what
    /// it needs in locals, the frame's code, its instruction pointer and its
    /// registers, and leaves for anything that enters or leaves a frame; the
    /// outer one does that and enters the inner loop again for the frame on
    /// top.
    pub(crate) fn run_registers(&mut self, target_depth: usize) -> Result<(), VmError> {
        loop {
            let frame = self
                .register_frames
                .last()
                .expect("internal error: a running VM has a frame");
            let chunk = &frame.function.registers;
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
                crate::tracer::on_register_instruction(
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
                        match tuple_element(&regs[src.index()], i64::from(index)) {
                            Ok(element) => regs[dst.index()] = element,
                            Err(message) => break Exit::Error(VmError::native(message)),
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
                    Instr::GetIterator { dst, src } => match iterator_of(&regs[src.index()]) {
                        Ok(iterator) => regs[dst.index()] = iterator,
                        Err(message) => break Exit::Error(VmError::native(message)),
                    },
                    Instr::IterNext { iter, dst, offset } => {
                        let Value::Object(object) = &regs[iter.index()] else {
                            unreachable!("internal error: IterNext's register holds an iterator");
                        };
                        let Object::Iterator(iterator) = object.as_ref() else {
                            unreachable!("internal error: IterNext's register holds an iterator");
                        };
                        let next = iterator.borrow_mut().next();
                        match next {
                            Some(value) => regs[dst.index()] = value,
                            None => ip = short_jump(ip, offset, far_jumps),
                        }
                    }
                    Instr::Unpack { src, base, len } => {
                        // The value may be in one of the registers it's
                        // unpacked to.
                        let value = regs[src.index()].clone();
                        let out = &mut regs[base.index()..base.index() + usize::from(len)];
                        if let Err(message) = unpack_into(&value, out) {
                            break Exit::Error(VmError::native(message));
                        }
                    }
                    Instr::Return { src } => break Exit::Return { src },
                    Instr::Halt { src } => break Exit::Halt { src },
                }
            };

            // The one place the instruction pointer is written back.
            let frame = self
                .register_frames
                .last_mut()
                .expect("internal error: a running VM has a frame");
            frame.ip = ip as u32;
            let base = frame.base as usize;
            let result = match exit {
                Exit::Call { base: callee, args } => {
                    self.call_register(base + callee.index(), usize::from(args))
                }
                Exit::CallVec { base: callee, args } => {
                    let span = frame.function.registers.span(ip - 1);
                    self.call_vec_in_registers(base + callee.index(), usize::from(args), span)
                }
                Exit::CallNative {
                    dst,
                    base: first,
                    slot,
                    args,
                } => {
                    let native = Rc::clone(global_native(&self.globals, usize::from(slot)));
                    self.run_native_in_registers(&native, base + first.index(), usize::from(args))
                        .map(|value| self.stack[base + dst.index()] = value)
                }
                Exit::Return { src } => {
                    let result = self.return_from_register_call(base, src);
                    if result.is_ok() && self.register_frames.len() == target_depth {
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
                    .register_frames
                    .last()
                    .expect("internal error: a running VM has a frame");
                error
                    .span
                    .get_or_insert_with(|| frame.function.registers.span(frame.ip as usize - 1));
                return Err(error);
            }
        }
    }

    /// Ends the call whose frame is on top, which starts at `base`, giving
    /// its caller the value in `src`: the caller's callee register, the one
    /// below the frame, gets it. A checked call's result is checked here, so
    /// that a wrong one fails at the call, the caller's instruction.
    fn return_from_register_call(&mut self, base: usize, src: Reg) -> Result<(), VmError> {
        let (frame_size, on_return) = self.register_frames.pop();
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
    /// values after it: pushes its frame, or for a function that runs no
    /// register code of its own, as a native, runs it and puts what it
    /// returns at `callee`.
    fn call_register(&mut self, callee: usize, args: usize) -> Result<(), VmError> {
        let function = match &self.stack[callee] {
            Value::Object(object) => match object.as_ref() {
                // The usual calls, which the frame takes the function of
                // without copying anything.
                Object::Function(Function::Compiled(function)) => {
                    let function = Rc::clone(function);
                    return self.enter_register_frame(function, None, callee, None);
                }
                Object::Function(Function::Closure(closure)) => {
                    let function = Rc::clone(&closure.prototype);
                    let closure = Some(Rc::clone(object));
                    return self.enter_register_frame(function, closure, callee, None);
                }
                Object::Function(function) => function.clone(),
                other => return Err(unable_to_invoke(&other.static_type())),
            },
            other => return Err(unable_to_invoke(&other.static_type())),
        };
        self.dispatch_register_call(function, callee, args, None)
    }

    /// Calls `function`, whose callee register is `callee`, an index in the
    /// stack, with the `args` values after it. `memo` is where the result
    /// goes too, for a call of a `pure fn` whose result isn't cached yet.
    fn dispatch_register_call(
        &mut self,
        function: Function,
        callee: usize,
        args: usize,
        memo: Option<MemoCache>,
    ) -> Result<(), VmError> {
        match function {
            Function::Compiled(function) => self.enter_register_frame(function, None, callee, memo),
            Function::Closure(closure) => {
                let function = Rc::clone(&closure.prototype);
                let closure = Rc::new(Object::Function(Function::Closure(closure)));
                self.enter_register_frame(function, Some(closure), callee, memo)
            }
            Function::Native(native) => {
                let value = self.run_native_in_registers(&native, callee + 1, args)?;
                if let Some((cache, key)) = memo {
                    cache.borrow_mut().insert(key, value.clone());
                }
                self.stack[callee] = value;
                Ok(())
            }
            Function::Memoized { cache, function } => {
                let arguments = callee + 1..callee + 1 + args;
                let key = memo_key(&self.stack[arguments.clone()]);
                let cached = cache.borrow().get(&key).cloned();
                if let Some(cached) = cached {
                    clear(&mut self.stack[arguments]);
                    self.stack[callee] = cached;
                    return Ok(());
                }
                self.dispatch_register_call(
                    Rc::unwrap_or_clone(function),
                    callee,
                    args,
                    Some((cache, key)),
                )
            }
            Function::Checked { function, typ } => {
                self.call_checked_in_registers(function, *typ, callee, args)
            }
        }
    }

    /// Calls the function a [`Function::Checked`] checks, and checks that
    /// what it returns is what `typ`, the function type it's used as, says:
    /// right away when the call completes here, or when its frame returns.
    #[cold]
    fn call_checked_in_registers(
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
        let depth = self.register_frames.len();
        self.dispatch_register_call(Rc::unwrap_or_clone(function), callee, args, None)?;
        if self.register_frames.len() > depth {
            let frame = self
                .register_frames
                .last_mut()
                .expect("internal error: a frame was pushed");
            frame.on_return.get_or_insert_default().returns = Some(*return_type);
            return Ok(());
        }
        convert_returned(&mut self.stack[callee], &return_type).map_err(VmError::native)
    }

    /// Pushes the frame of a call of `function`, which `closure` is a
    /// closure of if it's one, whose callee register is `callee`, an index
    /// in the stack: its parameters are the registers after it.
    fn enter_register_frame(
        &mut self,
        function: Rc<CompiledFunction>,
        closure: Option<Rc<Object>>,
        callee: usize,
        memo: Option<MemoCache>,
    ) -> Result<(), VmError> {
        // The frames live on the heap, so without a limit a function that
        // calls itself without end would take memory until the system stops
        // the program.
        if self.register_frames.len() >= MAX_CALL_DEPTH {
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
        self.register_frames
            .push(function, closure, first, on_return);
        Ok(())
    }

    /// Calls `callee`'s function, an operator applied to each element of
    /// tuples (see [`Vm::call_vec`]), with the `args` values after it, and
    /// puts the tuple of the results at `callee`.
    fn call_vec_in_registers(
        &mut self,
        callee: usize,
        args: usize,
        span: Span,
    ) -> Result<(), VmError> {
        let arguments = take_all(&mut self.stack[callee + 1..callee + 1 + args]);
        let function = std::mem::replace(&mut self.stack[callee], Value::None);
        self.stack[callee] = self.call_vec(&function, &arguments, span)?;
        Ok(())
    }

    /// Runs `native` with the `args` values from `first` on in the stack,
    /// which are `None` afterwards, and returns what it returns.
    fn run_native_in_registers(
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
    /// program, as `map` does, and returns what it returns. The call's
    /// registers are above those of the frame on top, which is the one that
    /// called the native: its callee register first, then the arguments.
    pub(crate) fn register_callback(
        &mut self,
        function: Function,
        args: &[Value],
    ) -> Result<Value, VmError> {
        // A native runs on `args` where they are, as the stack VM runs it:
        // `(a, b) - (c, d)` calls the native `-` for each element.
        if let Function::Native(native) = &function {
            return match &native.func {
                NativeFunc::Simple(function) => function(args),
                NativeFunc::WithVm(function) => function(args, self),
            };
        }
        let top = self
            .register_frames
            .last()
            .expect("internal error: a native runs in a frame");
        let callee = top.base as usize + top.function.frame_size;
        let end = callee + 1 + args.len();
        if self.stack.len() < end {
            self.stack.resize(end, Value::None);
        }
        self.stack[callee + 1..end].clone_from_slice(args);
        let depth = self.register_frames.len();
        let result = self
            .dispatch_register_call(function, callee, args.len(), None)
            .and_then(|()| {
                if self.register_frames.len() > depth {
                    self.run_registers(depth)
                } else {
                    Ok(())
                }
            });
        if result.is_err() {
            // A native may go on after a callback failed, from the frame
            // that called it.
            self.register_frames.truncate(depth);
        }
        result?;
        Ok(std::mem::replace(&mut self.stack[callee], Value::None))
    }
}

/// A closure of `code`, made in a frame whose registers are `regs` and
/// which is a closure over `upvalues`: it shares the cells of the locals it
/// captures, moving a local into a new cell on its first capture, and its
/// maker's own captured variables.
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
    use crate::register::Builder;
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
            registers: builder.finish(),
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
        // `twice(f, x)`, which calls back into the program: `f(f(x))`.
        registry.declare_global_fn(NativeSpec::with_signature(
            "twice",
            "Calls a function on what it returns.",
            "(Any, Int) -> Int",
            || {
                NativeFunc::WithVm(Box::new(|args, vm| {
                    let [Value::Object(function), x] = args else {
                        unreachable!("twice takes a function and an Int");
                    };
                    let Object::Function(function) = function.as_ref() else {
                        unreachable!("twice takes a function and an Int");
                    };
                    let once = vm.call_callback(function.clone(), std::slice::from_ref(x))?;
                    vm.call_callback(function.clone(), &[once])
                }))
            },
        ));
        let mut natives = Natives::default();
        natives.extend(registry);
        natives
    }

    /// A function value of `function`, a body `program` wrote.
    fn function_value(function: CompiledFunction) -> Value {
        Value::function(Function::Compiled(Rc::new(function)))
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

    /// The program that calls `function` with the `Int`s `args` and halts
    /// with what it returns.
    fn call_with(function: Value, args: &[i64]) -> CompiledFunction {
        program(1 + args.len(), vec![], |b| {
            let k = b.constant(function);
            emit(b, Instr::LoadK { dst: Reg(0), k });
            for (i, &arg) in args.iter().enumerate() {
                load_int(b, Reg(1 + i as u16), arg);
            }
            emit(
                b,
                Instr::Call {
                    base: Reg(0),
                    args: args.len() as u16,
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        })
    }

    fn tuple(values: &[i64]) -> Value {
        Value::tuple(values.iter().map(|&n| Value::Int(n)).collect())
    }

    #[test]
    fn a_tuple_made_in_its_first_element() {
        let function = program(2, vec![], |b| {
            load_int(b, Reg(0), 1);
            load_int(b, Reg(1), 2);
            emit(
                b,
                Instr::MakeTuple {
                    dst: Reg(0),
                    base: Reg(0),
                    len: 2,
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        assert_eq!(run(&Natives::default(), function).unwrap(), tuple(&[1, 2]));
    }

    /// `(1, 2) + (10, 20)` with the native `sum` applied to each element.
    #[test]
    fn a_vectorised_call_puts_the_tuple_where_the_function_was() {
        let natives = natives();
        let function = program(3, vec![0], |b| {
            emit(
                b,
                Instr::GetGlobal {
                    dst: Reg(0),
                    slot: 0,
                },
            );
            let k = b.constant(tuple(&[1, 2]));
            emit(b, Instr::LoadK { dst: Reg(1), k });
            let k = b.constant(tuple(&[10, 20]));
            emit(b, Instr::LoadK { dst: Reg(2), k });
            emit(
                b,
                Instr::CallVec {
                    base: Reg(0),
                    args: 2,
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        assert_eq!(run(&natives, function).unwrap(), tuple(&[11, 22]));
    }

    /// A native that calls back into the program runs the callback on the
    /// VM it's given, above the frame that called the native.
    #[test]
    fn a_native_calls_back_into_the_program() {
        let natives = natives();
        let function = program(3, vec![1], |b| {
            let k = b.constant(function_value(double()));
            emit(b, Instr::LoadK { dst: Reg(0), k });
            load_int(b, Reg(1), 5);
            emit(
                b,
                Instr::CallNative {
                    dst: Reg(2),
                    base: Reg(0),
                    slot: 1,
                    args: 2,
                },
            );
            emit(b, Instr::Halt { src: Reg(2) });
        });
        assert_eq!(run(&natives, function).unwrap(), Value::Int(20));
    }

    /// `fn quadruple(n) => twice(double, n)`, which a callback calls: a
    /// callback that calls a native that calls back.
    #[test]
    fn a_callback_calls_a_native_that_calls_back() {
        let natives = natives();
        let double = function_value(double());
        let quadruple = program(3, vec![], |b| {
            emit(
                b,
                Instr::Copy {
                    dst: Reg(2),
                    src: Reg(0),
                },
            );
            let k = b.constant(double.clone());
            emit(b, Instr::LoadK { dst: Reg(1), k });
            emit(
                b,
                Instr::CallNative {
                    dst: Reg(0),
                    base: Reg(1),
                    slot: 1,
                    args: 2,
                },
            );
            emit(b, Instr::Return { src: Reg(0) });
        });
        let function = program(3, vec![1], |b| {
            let k = b.constant(function_value(quadruple));
            emit(b, Instr::LoadK { dst: Reg(0), k });
            load_int(b, Reg(1), 3);
            emit(
                b,
                Instr::CallNative {
                    dst: Reg(0),
                    base: Reg(0),
                    slot: 1,
                    args: 2,
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        // quadruple(quadruple(3))
        assert_eq!(run(&natives, function).unwrap(), Value::Int(48));
    }

    /// A memoised function runs once for arguments it has a result for.
    #[test]
    fn a_memoised_function_runs_once_per_argument() {
        // `fn f(n) { calls = calls + 1; n + n }`, `calls` in global 0.
        let counted = program(2, vec![], |b| {
            emit(
                b,
                Instr::GetGlobal {
                    dst: Reg(1),
                    slot: 0,
                },
            );
            emit(
                b,
                Instr::AddIntI {
                    dst: Reg(1),
                    left: Reg(1),
                    imm: 1,
                },
            );
            emit(
                b,
                Instr::SetGlobal {
                    slot: 0,
                    src: Reg(1),
                },
            );
            emit(
                b,
                Instr::AddInt {
                    dst: Reg(0),
                    left: Reg(0),
                    right: Reg(0),
                },
            );
            emit(b, Instr::Return { src: Reg(0) });
        });
        let memoised = Value::function(Function::Memoized {
            cache: Rc::default(),
            function: Rc::new(Function::Compiled(Rc::new(counted))),
        });
        let function = program(4, vec![], |b| {
            load_int(b, Reg(3), 0);
            emit(
                b,
                Instr::SetGlobal {
                    slot: 0,
                    src: Reg(3),
                },
            );
            for n in [4, 4, 5] {
                let k = b.constant(memoised.clone());
                emit(b, Instr::LoadK { dst: Reg(0), k });
                load_int(b, Reg(1), n);
                emit(
                    b,
                    Instr::Call {
                        base: Reg(0),
                        args: 1,
                    },
                );
            }
            emit(
                b,
                Instr::GetGlobal {
                    dst: Reg(2),
                    slot: 0,
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
        let mut function = function;
        function.num_globals = 1;
        // f(5), what is left of the last call's argument, and two runs.
        assert_eq!(
            run(&Natives::default(), function).unwrap().to_string(),
            "(10,None,2)"
        );
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
    fn memoize_wraps_the_function_in_its_register() {
        let function = program(2, vec![], |b| {
            let k = b.constant(function_value(double()));
            emit(b, Instr::LoadK { dst: Reg(0), k });
            emit(b, Instr::Memoize { reg: Reg(0) });
            load_int(b, Reg(1), 4);
            emit(
                b,
                Instr::Call {
                    base: Reg(0),
                    args: 1,
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        assert_eq!(run(&Natives::default(), function).unwrap(), Value::Int(8));
    }

    /// Lists, maps and ranges made in the register of their first operand.
    #[test]
    fn collections_made_in_their_first_operand() {
        let function = program(5, vec![], |b| {
            load_int(b, Reg(0), 1);
            load_int(b, Reg(1), 2);
            emit(
                b,
                Instr::MakeList {
                    dst: Reg(0),
                    base: Reg(0),
                    len: 2,
                },
            );
            load_int(b, Reg(1), 3);
            emit(
                b,
                Instr::ListPush {
                    list: Reg(0),
                    src: Reg(1),
                },
            );
            load_int(b, Reg(1), 10);
            load_int(b, Reg(2), 20);
            load_int(b, Reg(3), 0);
            emit(
                b,
                Instr::MakeMap {
                    dst: Reg(1),
                    base: Reg(1),
                    pairs: 1,
                    default: true,
                },
            );
            load_int(b, Reg(2), 30);
            load_int(b, Reg(3), 40);
            emit(
                b,
                Instr::MapInsert {
                    map: Reg(1),
                    key: Reg(2),
                    value: Reg(3),
                },
            );
            load_int(b, Reg(2), 5);
            load_int(b, Reg(3), 7);
            emit(
                b,
                Instr::MakeRangeInclusive {
                    dst: Reg(2),
                    start: Reg(2),
                    end: Reg(3),
                },
            );
            load_int(b, Reg(3), 1);
            emit(
                b,
                Instr::MakeRange {
                    dst: Reg(3),
                    start: Reg(3),
                    end: Reg(3),
                },
            );
            load_int(b, Reg(4), 9);
            emit(
                b,
                Instr::MakeRangeFrom {
                    dst: Reg(4),
                    start: Reg(4),
                },
            );
            emit(
                b,
                Instr::MakeTuple {
                    dst: Reg(0),
                    base: Reg(0),
                    len: 5,
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        let value = run(&Natives::default(), function).unwrap();
        let Value::Object(tuple) = &value else {
            panic!("a tuple");
        };
        let Object::Tuple(values) = tuple.as_ref() else {
            panic!("a tuple");
        };
        assert_eq!(values[0].to_string(), "[1,2,3]");
        let Value::Object(map) = &values[1] else {
            panic!("a map");
        };
        let Object::Map { entries, default } = map.as_ref() else {
            panic!("a map");
        };
        assert_eq!(entries.borrow().len(), 2);
        assert_eq!(entries.borrow()[&Value::Int(30)], Value::Int(40));
        assert_eq!(default.as_ref(), Some(&Value::Int(0)));
        let range = |value: &Value| {
            value
                .clone()
                .try_into_iter()
                .unwrap()
                .take(5)
                .collect::<Vec<_>>()
        };
        assert_eq!(range(&values[2]), [5, 6, 7].map(Value::Int));
        assert!(range(&values[3]).is_empty());
        assert_eq!(range(&values[4]), [9, 10, 11, 12, 13].map(Value::Int));
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

    #[test]
    fn jump_if_true() {
        for (condition, expected) in [(true, 2), (false, 1)] {
            let function = program(2, vec![], |b| {
                let end = b.label();
                let k = b.constant(Value::Bool(condition));
                emit(b, Instr::LoadK { dst: Reg(0), k });
                load_int(b, Reg(1), 2);
                b.jump(
                    Instr::JumpIfTrue {
                        cond: Reg(0),
                        offset: 0,
                    },
                    end,
                    Span::synthetic(),
                );
                load_int(b, Reg(1), 1);
                b.bind(end);
                emit(b, Instr::Halt { src: Reg(1) });
            });
            assert_eq!(
                run(&Natives::default(), function).unwrap(),
                Value::Int(expected)
            );
        }
    }

    /// Sums the elements of a tuple with `GetIterator` and `IterNext`, the
    /// iterator made in the tuple's register and each element read into the
    /// iterator's.
    #[test]
    fn iterating_in_the_registers_of_the_operands() {
        let function = program(3, vec![], |b| {
            let k = b.constant(tuple(&[1, 2, 3]));
            emit(b, Instr::LoadK { dst: Reg(0), k });
            emit(
                b,
                Instr::GetIterator {
                    dst: Reg(0),
                    src: Reg(0),
                },
            );
            load_int(b, Reg(1), 0);
            let start = b.label();
            let end = b.label();
            b.bind(start);
            b.jump(
                Instr::IterNext {
                    iter: Reg(0),
                    dst: Reg(2),
                    offset: 0,
                },
                end,
                Span::synthetic(),
            );
            emit(
                b,
                Instr::AddInt {
                    dst: Reg(1),
                    left: Reg(1),
                    right: Reg(2),
                },
            );
            b.jump(Instr::Jump { offset: 0 }, start, Span::synthetic());
            b.bind(end);
            // The last element read into the iterator's own register.
            let k = b.constant(tuple(&[7]));
            emit(b, Instr::LoadK { dst: Reg(2), k });
            emit(
                b,
                Instr::GetIterator {
                    dst: Reg(2),
                    src: Reg(2),
                },
            );
            b.jump(
                Instr::IterNext {
                    iter: Reg(2),
                    dst: Reg(2),
                    offset: 0,
                },
                start,
                Span::synthetic(),
            );
            emit(
                b,
                Instr::MakeTuple {
                    dst: Reg(0),
                    base: Reg(1),
                    len: 2,
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        assert_eq!(run(&Natives::default(), function).unwrap(), tuple(&[6, 7]));
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
        let function = program(1, vec![], |b| {
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

    #[test]
    fn a_tuple_element_read_into_the_tuples_register() {
        let function = program(2, vec![], |b| {
            let k = b.constant(tuple(&[4, 5, 6]));
            emit(b, Instr::LoadK { dst: Reg(0), k });
            emit(
                b,
                Instr::Copy {
                    dst: Reg(1),
                    src: Reg(0),
                },
            );
            emit(
                b,
                Instr::TupleIndex {
                    dst: Reg(0),
                    src: Reg(0),
                    index: -1,
                },
            );
            emit(
                b,
                Instr::TupleIndex {
                    dst: Reg(1),
                    src: Reg(1),
                    index: 0,
                },
            );
            emit(
                b,
                Instr::MakeTuple {
                    dst: Reg(0),
                    base: Reg(0),
                    len: 2,
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        assert_eq!(run(&Natives::default(), function).unwrap(), tuple(&[6, 4]));
    }

    /// A cast checks the value in its register, and an optional one makes
    /// it `Some` of the value or `None` there.
    #[test]
    fn casts_in_place() {
        let function = program(3, vec![], |b| {
            load_int(b, Reg(0), 1);
            let int = b.static_type(StaticType::Int);
            let string = b.static_type(StaticType::String);
            emit(
                b,
                Instr::CheckType {
                    reg: Reg(0),
                    typ: int,
                },
            );
            load_int(b, Reg(1), 2);
            emit(
                b,
                Instr::TryCast {
                    reg: Reg(1),
                    typ: int,
                },
            );
            load_int(b, Reg(2), 3);
            emit(
                b,
                Instr::TryCast {
                    reg: Reg(2),
                    typ: string,
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
            emit(
                b,
                Instr::CheckType {
                    reg: Reg(0),
                    typ: string,
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        let error = run(&Natives::default(), function).unwrap_err();
        assert_eq!(
            error.message,
            "cannot cast (Int, Option<Int>, Option<Any>) to String"
        );
    }

    #[test]
    fn calling_something_that_isnt_a_function() {
        let error = run(&Natives::default(), call_with(Value::Int(3), &[1])).unwrap_err();
        assert_eq!(error.message, "Unable to invoke Int as a function.");
    }

    fn run(natives: &Natives, function: CompiledFunction) -> Result<Value, VmError> {
        let mut vm = Vm::new(function, crate::native_globals(natives), natives);
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
    fn an_op_whose_destination_is_its_operands() {
        let function = program(1, vec![], |b| {
            load_int(b, Reg(0), 21);
            emit(
                b,
                Instr::AddInt {
                    dst: Reg(0),
                    left: Reg(0),
                    right: Reg(0),
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        assert_eq!(run(&Natives::default(), function).unwrap(), Value::Int(42));
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
    fn a_native_whose_result_goes_to_its_first_argument() {
        let natives = natives();
        let function = program(2, vec![0], |b| {
            load_int(b, Reg(0), 40);
            load_int(b, Reg(1), 2);
            emit(
                b,
                Instr::CallNative {
                    dst: Reg(0),
                    base: Reg(0),
                    slot: 0,
                    args: 2,
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        assert_eq!(run(&natives, function).unwrap(), Value::Int(42));
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

    #[test]
    fn a_call_puts_the_result_where_the_callee_was() {
        // fn double(n) => n + n, called as `double(21)`.
        let double = program(1, vec![], |b| {
            emit(
                b,
                Instr::AddInt {
                    dst: Reg(0),
                    left: Reg(0),
                    right: Reg(0),
                },
            );
            emit(b, Instr::Return { src: Reg(0) });
        });
        let function = program(3, vec![], |b| {
            let k = b.constant(Value::function(Function::Compiled(Rc::new(double))));
            emit(b, Instr::LoadK { dst: Reg(1), k });
            load_int(b, Reg(2), 21);
            emit(
                b,
                Instr::Call {
                    base: Reg(1),
                    args: 1,
                },
            );
            emit(b, Instr::Halt { src: Reg(1) });
        });
        assert_eq!(run(&Natives::default(), function).unwrap(), Value::Int(42));
    }

    #[test]
    fn an_error_has_the_span_of_its_instruction() {
        let span = Span::new(ndc_lexer::SourceId::SYNTHETIC, 3, 4);
        let function = program(2, vec![], |b| {
            load_int(b, Reg(0), i64::MAX);
            load_int(b, Reg(1), 1);
            b.emit(
                Instr::AddInt {
                    dst: Reg(0),
                    left: Reg(0),
                    right: Reg(1),
                },
                span,
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        let error = run(&Natives::default(), function).unwrap_err();
        assert_eq!(error.span, Some(span));
        assert!(error.message.contains("overflowed"), "{}", error.message);
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
            assert!(function.registers.code()[2].is_far_jump());
            assert_eq!(function.registers.jump_target(2), Some(40_004));
            assert_eq!(
                run(&Natives::default(), function).unwrap(),
                Value::Int(expected)
            );
        }
    }

    /// The analyser makes every condition a `Bool`, so no program in the
    /// language the register VM compiles so far gets here.
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
