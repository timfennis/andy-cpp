use crate::chunk::OpCode;
use crate::error::VmError;
use crate::hash_map::{HashMap, hasher};
use crate::io::{InputSource, OutputSink};
use crate::iterator::{
    HeapIter, MapIter, RangeInclusiveIter, RangeIter, SeqIter, StringIter, UnboundedRangeIter,
    VmIterator,
};
use crate::native_op::NativeOp;
use crate::value::{CompiledFunction, Function, NativeFunc, NativeFunction};
use crate::{ClosureFunction, Natives, Object, Value};
use ndc_analysis::CaptureSource;
use ndc_core::StaticType;
use ndc_lexer::Span;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::rc::Rc;

/// Runs compiled code: stack code on a value stack, or register code (see
/// [`crate::register`]) on a register stack, which a VM made for register
/// code holds instead. The natives that take a VM are given this one, so a
/// native that calls back into the program, as `map` does, runs the
/// callback on the same VM, whichever code the program was compiled to.
pub struct Vm {
    /// The stack VM's value stack, or the register VM's registers: every
    /// running call's frame, one above the other, with
    /// [`crate::register::WINDOW`] values past the top frame's first
    /// register.
    pub(crate) stack: Vec<Value>,
    pub(crate) globals: Vec<Value>,
    frames: Vec<CallFrame>,
    /// The register VM's running calls, empty for stack code.
    pub(crate) register_frames: Vec<crate::register::Frame>,
    /// What the program's last expression gave, once register code halts.
    pub(crate) result: Value,
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
pub(crate) type MemoCache = (Rc<RefCell<HashMap<u64, Value>>>, u64);

/// What a dispatched call left behind. Callees split into two kinds: compiled
/// functions and closures push a frame that still has to be executed, while
/// natives, struct constructors, field accessors, and memoization cache hits
/// finish during dispatch and push their result straight onto the value stack.
/// Callers that drive execution themselves (`call_callback`) must know which
/// happened; running a frame that was never pushed resumes the *caller's*
/// instruction stream instead.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Dispatch {
    /// A new frame sits on top of the frame stack; run it to completion.
    FramePushed,
    /// The call is already done and its result is on the value stack.
    Completed,
}

/// How many calls deep a program can go. A frame takes about a hundred
/// bytes, so this is about 100 MB of frames.
pub const MAX_CALL_DEPTH: usize = 1_000_000;

pub struct CallFrame {
    closure: ClosureFunction,
    ip: usize,
    /// Absolute index of the first local slot for this frame.
    frame_pointer: usize,
    /// If this frame was called from a `pure` (memoized) function, holds the
    /// cache to write into and the hash key when the frame returns.  `None`
    /// for ordinary (non-memoized) calls.
    memo: Option<MemoCache>,
    /// What the frame's call has to return, when it's a call of a
    /// [`Function::Checked`]. Boxed to keep frames small.
    returns: Option<Box<StaticType>>,
}

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
pub(crate) fn fill_natives(globals: &mut [Value], natives: &Natives, code: &CompiledFunction) {
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

impl Vm {
    /// A VM that runs `function` at the top level with `globals`, which
    /// [`native_globals`] starts, and which a VM that ran earlier code hands
    /// on. Each native `function` reads is put in its slot first, made from
    /// `natives` if it isn't made yet.
    pub fn new(function: CompiledFunction, mut globals: Vec<Value>, natives: &Natives) -> Self {
        let num_locals = function.num_locals;
        // Every global the program declares has a slot, holding unit until its
        // declaration runs, like a local.
        if globals.len() < function.num_globals {
            globals.resize(function.num_globals, Value::unit());
        }
        fill_natives(&mut globals, natives, &function);
        let mut vm = Self {
            stack: Vec::new(),
            globals,
            frames: Vec::new(),
            register_frames: Vec::new(),
            result: Value::unit(),
            output: OutputSink::Stdout,
            input: InputSource::Stdin,
            #[cfg(feature = "trace")]
            source: None,
            #[cfg(feature = "trace")]
            tracer: None,
        };
        if function.is_register_code() {
            vm.stack = vec![Value::None; crate::register::WINDOW];
            vm.register_frames
                .push(crate::register::Frame::top_level(Rc::new(function)));
            return vm;
        }
        vm.stack = Vec::with_capacity(256);
        vm.frames.push(CallFrame {
            closure: ClosureFunction {
                prototype: Rc::new(function),
                upvalues: Vec::new(),
            },
            ip: 0,
            frame_pointer: 0,
            memo: None,
            returns: None,
        });
        for _ in 0..num_locals {
            vm.stack.push(Value::unit());
        }
        vm
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

    pub fn run(&mut self) -> Result<(), VmError> {
        if self.runs_registers() {
            return self.run_registers(0);
        }
        self.run_to_depth(0)
    }

    /// Whether the VM runs register code.
    fn runs_registers(&self) -> bool {
        !self.register_frames.is_empty()
    }

    /// Run the VM until the frame stack drops back to `target_depth`.
    /// Used by `call_callback` to execute a single callback frame inline
    /// on the parent VM, stopping when that frame's `Return` opcode fires.
    pub fn run_to_depth(&mut self, target_depth: usize) -> Result<(), VmError> {
        if self.frames.is_empty() {
            panic!("no call frames")
        }

        let result = loop {
            let frame = self.frames.last_mut().expect("must not be empty");
            let ip = frame.ip;
            let span = frame.closure.prototype.body.span(ip);
            let op = frame.closure.prototype.body.opcode(ip);
            frame.ip += 1;

            #[cfg(feature = "trace")]
            if let Some(tracer) = &mut self.tracer {
                tracer.on_instruction(&crate::tracer::InstructionContext {
                    ip,
                    opcode: op,
                    span,
                    source: self.source.as_deref(),
                });
            }

            match op {
                OpCode::Halt => {
                    break Ok(());
                }
                OpCode::Return => {
                    let ret = self.stack.pop().expect("stack underflow");
                    let frame = self.frames.last_mut().expect("no frame");
                    let frame_pointer = frame.frame_pointer;
                    // If this was a memoized call, cache the return value now,
                    // before the frame is torn down.
                    if let Some((cache, key)) = &frame.memo {
                        cache.borrow_mut().insert(*key, ret.clone());
                    }
                    let returns = frame.returns.take();
                    if frame_pointer == 0 {
                        return Err(VmError::new("Return at top level", span));
                    }
                    self.stack.truncate(frame_pointer - 1);
                    self.frames.pop().expect("no frame to pop");
                    self.stack.push(ret);
                    // A checked call's result is checked where it was called.
                    if let Some(returns) = returns {
                        let caller = self.frames.last().expect("a call has a caller");
                        let call = caller.closure.prototype.body.span(caller.ip - 1);
                        convert_result(&mut self.stack, &returns)
                            .map_err(|message| VmError::new(message, call))?;
                    }
                    if self.frames.len() == target_depth {
                        break Ok(());
                    }
                }
                OpCode::Constant(idx) => {
                    let idx = *idx;
                    self.stack
                        .push(frame.closure.prototype.body.constant(idx).clone());
                }
                OpCode::MakeString(idx) => {
                    let Value::Object(object) = frame.closure.prototype.body.constant(*idx) else {
                        unreachable!("MakeString's constant is a string");
                    };
                    let Object::String(text) = object.as_ref() else {
                        unreachable!("MakeString's constant is a string");
                    };
                    let string = Value::string(text.borrow().as_str());
                    self.stack.push(string);
                }
                OpCode::CheckType(target) => {
                    if !convert_top(&mut self.stack, target) {
                        let value = self.stack.last().expect("stack underflow");
                        return Err(VmError::new(cannot_cast(value, target), span));
                    }
                }
                OpCode::TryCast(target) => try_cast(&mut self.stack, target),
                OpCode::TupleIndex(index) => {
                    let tuple = self.stack.last_mut().expect("stack underflow");
                    match tuple_element(tuple, *index) {
                        Ok(element) => *tuple = element,
                        Err(message) => return Err(VmError::new(message, span)),
                    }
                }
                OpCode::GetLocalTupleIndex { slot, index } => {
                    match tuple_element(&self.stack[frame.slot(*slot)], *index) {
                        Ok(element) => self.stack.push(element),
                        Err(message) => return Err(VmError::new(message, span)),
                    }
                }
                OpCode::AddInt => run_native_op(&mut self.stack, NativeOp::AddInt, span)?,
                OpCode::SubInt => run_native_op(&mut self.stack, NativeOp::SubInt, span)?,
                OpCode::MulInt => run_native_op(&mut self.stack, NativeOp::MulInt, span)?,
                OpCode::AddFloat => run_native_op(&mut self.stack, NativeOp::AddFloat, span)?,
                OpCode::SubFloat => run_native_op(&mut self.stack, NativeOp::SubFloat, span)?,
                OpCode::MulFloat => run_native_op(&mut self.stack, NativeOp::MulFloat, span)?,
                OpCode::DivFloat => run_native_op(&mut self.stack, NativeOp::DivFloat, span)?,
                OpCode::Less => run_native_op(&mut self.stack, NativeOp::Less, span)?,
                OpCode::LessEqual => run_native_op(&mut self.stack, NativeOp::LessEqual, span)?,
                OpCode::Greater => run_native_op(&mut self.stack, NativeOp::Greater, span)?,
                OpCode::GreaterEqual => {
                    run_native_op(&mut self.stack, NativeOp::GreaterEqual, span)?
                }
                OpCode::Equal => run_native_op(&mut self.stack, NativeOp::Equal, span)?,
                OpCode::NotEqual => run_native_op(&mut self.stack, NativeOp::NotEqual, span)?,
                OpCode::NegInt => run_native_op(&mut self.stack, NativeOp::NegInt, span)?,
                OpCode::NegFloat => run_native_op(&mut self.stack, NativeOp::NegFloat, span)?,
                OpCode::Not => run_native_op(&mut self.stack, NativeOp::Not, span)?,
                OpCode::CallNative { slot, args } => {
                    let (slot, args) = (*slot, *args);
                    if let Err(mut e) = self.call_native(slot, args) {
                        e.span.get_or_insert(span);
                        return Err(e);
                    }
                }
                OpCode::GetLocal(slot) => {
                    let slot = *slot;
                    self.stack.push(self.stack[frame.slot(slot)].clone());
                }
                OpCode::GetCell(slot) => {
                    let value = &self.stack[frame.slot(*slot)];
                    let value = match value.as_cell() {
                        Some(cell) => cell.borrow().clone(),
                        None => value.clone(),
                    };
                    self.stack.push(value);
                }
                OpCode::SetCell(slot) => {
                    let index = frame.slot(*slot);
                    let value = self.stack.pop().expect("stack underflow");
                    match self.stack[index].as_cell() {
                        Some(cell) => *cell.borrow_mut() = value,
                        None => self.stack[index] = value,
                    }
                }
                OpCode::NewCell(slot) => {
                    let index = frame.slot(*slot);
                    let cell = Value::Cell(Rc::new(RefCell::new(Value::unit())));
                    if index < self.stack.len() {
                        self.stack[index] = cell;
                    } else {
                        self.stack.push(cell);
                    }
                }
                OpCode::GetGlobal(slot) => {
                    let slot = *slot;
                    self.stack.push(self.globals[slot].clone());
                }
                OpCode::SetGlobal(slot) => {
                    let slot = *slot;
                    let value = self.stack.pop().expect("stack underflow");
                    self.globals[slot] = value;
                }
                OpCode::SetLocal(slot) => {
                    let slot = *slot;
                    let value = self.stack.pop().expect("stack underflow");
                    if frame.slot(slot) < self.stack.len() {
                        self.stack[frame.slot(slot)] = value;
                    } else {
                        self.stack.push(value);
                    }
                }
                OpCode::JumpIfFalse(offset) => {
                    let offset = *offset;
                    let top = self.stack.last().expect("stack underflow");
                    match top {
                        Value::Bool(false) => {
                            frame.ip = offset.apply(frame.ip);
                        }
                        Value::Bool(true) => {}
                        value => return Err(VmError::new(not_a_bool(value), span)),
                    }
                }
                OpCode::JumpIfTrue(offset) => {
                    let offset = *offset;
                    let top = self.stack.last().expect("stack underflow");
                    match top {
                        Value::Bool(true) => {
                            frame.ip = offset.apply(frame.ip);
                        }
                        Value::Bool(false) => {}
                        value => return Err(VmError::new(not_a_bool(value), span)),
                    }
                }
                OpCode::Jump(offset) => {
                    frame.ip = offset.apply(frame.ip);
                }
                OpCode::Pop => {
                    self.stack.pop();
                }
                OpCode::Call(args) => {
                    let args = *args;
                    let func = self
                        .resolve_callee(args)
                        .map_err(|msg| VmError::new(msg, span))?;
                    if let Err(mut e) = self.dispatch_call(func, args) {
                        e.span.get_or_insert(span);
                        return Err(e);
                    }
                }
                OpCode::CallVec(args) => {
                    let args = *args;
                    let arguments = self.stack.split_off(self.stack.len() - args);
                    let callee = self.stack.pop().expect("stack underflow");
                    let result = self.call_vec(&callee, &arguments, span)?;
                    self.stack.push(result);
                }
                OpCode::MakeList(size) => {
                    let size = *size;
                    let data = self.stack.split_off(self.stack.len() - size);
                    self.stack.push(Value::Object(Rc::new(Object::list(data))));
                }
                OpCode::MakeTuple(size) => {
                    let size = *size;
                    let data = self.stack.split_off(self.stack.len() - size);
                    self.stack.push(Value::Object(Rc::new(Object::Tuple(data))));
                }
                OpCode::MakeMap { pairs, has_default } => {
                    let (pairs, has_default) = (*pairs, *has_default);
                    let default = if has_default {
                        Some(self.stack.pop().expect("expected default value on stack"))
                    } else {
                        None
                    };
                    let flat = self.stack.split_off(self.stack.len() - pairs * 2);
                    let mut map = HashMap::with_capacity_and_hasher(pairs, Default::default());
                    let mut flat_iter = flat.into_iter();
                    for _ in 0..pairs {
                        let key = flat_iter.next().expect("expected key");
                        let value = flat_iter.next().expect("expected value");
                        map.insert(key, value);
                    }
                    self.stack
                        .push(Value::Object(Rc::new(Object::map(map, default))));
                }
                OpCode::GetUpvalue(slot) => {
                    let value = frame.closure.upvalues[*slot].borrow().clone();
                    self.stack.push(value);
                }
                OpCode::SetUpvalue(slot) => {
                    let value = self.stack.pop().expect("stack underflow");
                    *frame.closure.upvalues[*slot].borrow_mut() = value;
                }
                OpCode::GetIterator => {
                    let val = self.stack.pop().expect("stack underflow");

                    // Check if already an iterator
                    if matches!(val, Value::Object(ref obj) if matches!(**obj, Object::Iterator(_)))
                    {
                        self.stack.push(val);
                        continue;
                    }

                    let iter_val = match &val {
                        Value::Object(rc) => match rc.as_ref() {
                            Object::List(_) | Object::Tuple(_) | Object::Deque(_) => Some(
                                Value::iterator(Rc::new(RefCell::new(SeqIter::new(Rc::clone(rc))))),
                            ),
                            Object::String(s) => Some(Value::iterator(Rc::new(RefCell::new(
                                StringIter::new(Rc::clone(s)),
                            )))),
                            Object::Map { .. } => Some(Value::iterator(Rc::new(RefCell::new(
                                MapIter::new(Rc::clone(rc)),
                            )))),
                            Object::MinHeap(h) => {
                                Some(Value::iterator(Rc::new(RefCell::new(HeapIter::new_min(h)))))
                            }
                            Object::MaxHeap(h) => {
                                Some(Value::iterator(Rc::new(RefCell::new(HeapIter::new_max(h)))))
                            }
                            _ => None,
                        },
                        _ => None,
                    };

                    match iter_val {
                        Some(iter) => self.stack.push(iter),
                        None => {
                            return Err(VmError::new(
                                format!("{} is not iterable", val.static_type()),
                                span,
                            ));
                        }
                    }
                }
                OpCode::IterNext(offset) => {
                    let offset = *offset;
                    let top = self.stack.last().expect("stack underflow");
                    let Value::Object(obj) = top else {
                        panic!("IterNext expects an iterator on the stack")
                    };
                    let Object::Iterator(iter_rc) = &**obj else {
                        panic!("IterNext expects an iterator on the stack")
                    };
                    let next = iter_rc.borrow_mut().next();
                    match next {
                        Some(value) => {
                            self.stack.push(value);
                        }
                        None => {
                            frame.ip = offset.apply(frame.ip);
                        }
                    }
                }
                OpCode::ListPush(slot) => {
                    let slot = *slot;
                    let value = self.stack.pop().expect("stack underflow");
                    let frame = self.frames.last().expect("no frame");
                    let list_val = &self.stack[frame.slot(slot)];
                    let Value::Object(obj) = list_val else {
                        return Err(VmError::new("ListPush expects a list", span));
                    };
                    let Object::List(rc) = &**obj else {
                        return Err(VmError::new("ListPush expects a list", span));
                    };
                    rc.borrow_mut().push(value);
                }
                OpCode::MapInsert(slot) => {
                    let slot = *slot;
                    let value = self.stack.pop().expect("stack underflow");
                    let key = self.stack.pop().expect("stack underflow");
                    let frame = self.frames.last().expect("no frame");
                    let map_val = &self.stack[frame.slot(slot)];
                    let Value::Object(obj) = map_val else {
                        return Err(VmError::new("MapInsert expects a map", span));
                    };
                    let Object::Map { entries, .. } = &**obj else {
                        return Err(VmError::new("MapInsert expects a map", span));
                    };
                    entries.borrow_mut().insert(key, value);
                }
                OpCode::MakeRange { inclusive, bounded } => {
                    let (inclusive, bounded) = (*inclusive, *bounded);
                    let end = if bounded {
                        let v = self.stack.pop().expect("stack underflow");
                        let Value::Int(n) = v else {
                            return Err(VmError::new(bad_range_bound(&v), span));
                        };
                        Some(n)
                    } else {
                        None
                    };
                    let start = self.stack.pop().expect("stack underflow");
                    let Value::Int(start) = start else {
                        return Err(VmError::new(bad_range_bound(&start), span));
                    };
                    let iter: Rc<RefCell<dyn VmIterator>> = match (inclusive, end) {
                        (_, None) => Rc::new(RefCell::new(UnboundedRangeIter::new(start))),
                        (false, Some(end)) => Rc::new(RefCell::new(RangeIter::new(start, end))),
                        (true, Some(end)) => {
                            Rc::new(RefCell::new(RangeInclusiveIter::new(start, end)))
                        }
                    };
                    self.stack.push(Value::iterator(iter));
                }
                OpCode::Closure {
                    constant_idx: idx,
                    values,
                } => {
                    // Clone the Rc<[CaptureSource]> (just a refcount bump) and copy the index
                    // so that `op`'s borrow of `self.frames` ends before we re-borrow the frame
                    // and before `capture_local` needs `&mut self`.
                    let idx = *idx;
                    let values = Rc::clone(values);
                    let (compiled, frame_pointer) = {
                        let frame = self.frames.last().expect("no frame");
                        let Value::Object(obj) = frame.closure.prototype.body.constant(idx) else {
                            panic!("Closure opcode: constant at index {idx} is not an Object");
                        };
                        let Object::Function(Function::Compiled(compiled)) = &**obj else {
                            panic!(
                                "Closure opcode: constant at index {idx} is not a Compiled function"
                            );
                        };
                        (Rc::clone(compiled), frame.frame_pointer)
                    };
                    // Share the cells of captured locals, moving a local into a new
                    // cell on its first capture, and the parent's own upvalues.
                    let upvalues = values
                        .iter()
                        .map(|c| match c {
                            CaptureSource::Local(slot) => self.capture_local(frame_pointer + slot),
                            CaptureSource::Upvalue(slot) => {
                                let frame = self.frames.last().expect("no frame");
                                Rc::clone(&frame.closure.upvalues[*slot])
                            }
                        })
                        .collect();
                    let closure = Value::function(Function::Closure(ClosureFunction {
                        prototype: compiled,
                        upvalues,
                    }));

                    self.stack.push(closure);
                }
                OpCode::Unpack(size) => {
                    let size = *size;
                    self.exec_unpack(size, span)?;
                }
                OpCode::UnpackLocals { first, count } => {
                    let (first, count) = (*first, *count);
                    let frame_pointer = self.frames.last().expect("no frame").frame_pointer;
                    self.exec_unpack_locals(frame_pointer + first, count, span)?;
                }
                // Wraps the function on top of the stack in a fresh memoization
                // cache.  Emitted by the compiler for every `pure fn` declaration.
                // On subsequent calls with the same arguments, the cached result
                // is returned without executing the function body.
                OpCode::Memoize => {
                    let val = self.stack.pop().expect("stack underflow");
                    let Value::Object(obj) = val else {
                        panic!("Memoize: expected a function on the stack");
                    };
                    let Object::Function(func) = obj.as_ref() else {
                        panic!("Memoize: expected a function on the stack");
                    };
                    let func = func.clone();
                    self.stack.push(Value::function(Function::Memoized {
                        cache: Rc::new(RefCell::new(HashMap::default())),
                        function: Rc::new(func),
                    }));
                }
            }
        };

        #[cfg(feature = "trace")]
        if target_depth == 0
            && let Some(tracer) = &mut self.tracer
        {
            tracer.on_complete();
        }

        result
    }

    /// The cell a closure shares with the local at `stack_slot`. On the
    /// first capture the local's value moves into a new cell, which takes its
    /// place in the slot, so the frame and every closure see one variable.
    fn capture_local(&mut self, stack_slot: usize) -> Rc<RefCell<Value>> {
        if let Some(cell) = self.stack[stack_slot].as_cell() {
            return Rc::clone(cell);
        }
        let value = std::mem::replace(&mut self.stack[stack_slot], Value::None);
        let cell = Rc::new(RefCell::new(value));
        self.stack[stack_slot] = Value::Cell(Rc::clone(&cell));
        cell
    }

    fn dispatch_call(&mut self, func: Function, args: usize) -> Result<Dispatch, VmError> {
        // Memoized functions check the cache first.  On a hit we short-circuit
        // without pushing a new frame.  On a miss we dispatch the inner
        // function normally and tag the new frame so `Return` will populate
        // the cache once the call finishes.
        if let Function::Memoized { cache, function } = func {
            let start = self.stack.len() - args;
            let key = memo_key(&self.stack[start..]);

            if let Some(cached) = cache.borrow().get(&key).cloned() {
                // Cache hit: discard the callee slot and arguments, push result.
                self.stack.truncate(start - 1);
                self.stack.push(cached);
                return Ok(Dispatch::Completed);
            }

            // Cache miss: dispatch the inner function and remember to store
            // the result in the cache when this frame returns.
            self.dispatch_call_with_memo(Rc::unwrap_or_clone(function), args, Some((cache, key)))
        } else {
            self.dispatch_call_with_memo(func, args, None)
        }
    }

    /// Calls the function a [`Function::Checked`] checks, and checks that
    /// what it returns is what the function type it's used as says: right
    /// away when the call completes here, or when its frame returns.
    #[cold]
    fn call_checked(
        &mut self,
        function: Rc<Function>,
        typ: StaticType,
        args: usize,
    ) -> Result<Dispatch, VmError> {
        let StaticType::Function { return_type, .. } = typ else {
            unreachable!("a checked function is used as a function type");
        };
        // The function type it's used as may take more than the function
        // does, as `fn(Any) -> Int` would of a function taking an Int.
        let arguments = &self.stack[self.stack.len() - args..];
        if !function.conforms_value_args(arguments) {
            return Err(refused_arguments(arguments));
        }
        match self.dispatch_call(Rc::unwrap_or_clone(function), args)? {
            Dispatch::FramePushed => {
                self.frames.last_mut().expect("a frame was pushed").returns = Some(return_type);
                Ok(Dispatch::FramePushed)
            }
            Dispatch::Completed => {
                convert_result(&mut self.stack, &return_type).map_err(VmError::native)?;
                Ok(Dispatch::Completed)
            }
        }
    }

    /// Inner helper that pushes a new call frame.  `memo` is `Some` when the
    /// call originates from a memoized function and the return value should be
    /// stored in the cache.
    fn dispatch_call_with_memo(
        &mut self,
        func: Function,
        args: usize,
        memo: Option<MemoCache>,
    ) -> Result<Dispatch, VmError> {
        let closure = match func {
            Function::Native(native) => {
                let result = self.run_native(&native, args)?;
                self.stack.pop(); // callee slot
                if let Some((cache, key)) = memo {
                    cache.borrow_mut().insert(key, result.clone());
                }
                self.stack.push(result);
                return Ok(Dispatch::Completed);
            }
            Function::Closure(c) => c,
            Function::Compiled(f) => ClosureFunction {
                prototype: f,
                upvalues: Vec::new(),
            },
            Function::Checked { function, typ } => return self.call_checked(function, *typ, args),
            Function::Memoized { .. } => {
                // Already unwrapped in dispatch_call; this branch is unreachable.
                unreachable!(
                    "Memoized function should have been unwrapped before dispatch_call_with_memo"
                )
            }
        };
        // The frames live on the heap, so without a limit a function that
        // calls itself without end would take memory until the system stops
        // the program.
        if self.frames.len() >= MAX_CALL_DEPTH {
            return Err(calls_too_deep());
        }
        let num_locals = closure.prototype.num_locals;
        self.frames.push(CallFrame {
            closure,
            ip: 0,
            frame_pointer: self.stack.len() - args,
            memo,
            returns: None,
        });
        for _ in args..num_locals {
            self.stack.push(Value::unit());
        }
        Ok(Dispatch::FramePushed)
    }

    /// Calls the native in global slot `slot` with the `args` values on top
    /// of the stack, and puts its result in their place.
    fn call_native(&mut self, slot: usize, args: usize) -> Result<(), VmError> {
        let native = match &self.globals[slot] {
            Value::Object(object) => match object.as_ref() {
                Object::Function(Function::Native(native)) => native,
                _ => unreachable!("internal error: global slot {slot} holds no native"),
            },
            _ => unreachable!("internal error: global slot {slot} holds no native"),
        };
        let result = match &native.func {
            // Borrowed from the globals, as it doesn't take the VM.
            NativeFunc::Simple(f) => run_simple_native(&mut self.stack, f, args)?,
            NativeFunc::WithVm(_) => {
                let native = Rc::clone(native);
                self.run_native(&native, args)?
            }
        };
        self.stack.push(result);
        Ok(())
    }

    /// Runs `native` with the `args` values on top of the stack, which it
    /// takes off, and returns what it returns.
    fn run_native(&mut self, native: &NativeFunction, args: usize) -> Result<Value, VmError> {
        let start = self.stack.len() - args;
        match &native.func {
            NativeFunc::Simple(f) => run_simple_native(&mut self.stack, f, args),
            NativeFunc::WithVm(f) => {
                // The native takes the VM, so its arguments leave the stack
                // first.
                let call_args: Vec<Value> = self.stack.drain(start..).collect();
                f(&call_args, self)
            }
        }
    }

    /// Call a function inline on this VM, without spawning a child VM.
    /// Used by `VmCallable::call` so stdlib HOFs run their predicates/mappers
    /// directly on the parent stack — zero allocation per callback invocation.
    pub fn call_callback(&mut self, func: Function, args: &[Value]) -> Result<Value, VmError> {
        if self.runs_registers() {
            return self.register_callback(func, args);
        }
        if let Function::Native(native) = func {
            match &native.func {
                NativeFunc::Simple(f) => f(args),
                NativeFunc::WithVm(f) => f(args, self),
            }
        } else {
            let depth = self.frames.len();
            let n = args.len();
            self.stack.push(Value::unit()); // dummy callee slot
            self.stack.extend(args.iter().cloned());
            if self.dispatch_call(func, n)? == Dispatch::FramePushed {
                self.run_to_depth(depth)?;
            }
            Ok(self.stack.pop().expect("callback must produce a value"))
        }
    }

    /// Returns the expression result left on top of the stack after `run()`.
    /// If the last top-level expression was a statement (semicolon-terminated)
    /// its value was already popped, so the stack is exactly `num_locals` deep
    /// and this returns `Value::unit()`.
    /// For register code, it's the value `Halt` gave.
    pub fn last_value(&self, num_locals: usize) -> Value {
        if self.runs_registers() {
            return self.result.clone();
        }
        if self.stack.len() > num_locals {
            self.stack.last().cloned().unwrap_or_else(Value::unit)
        } else {
            Value::unit()
        }
    }

    /// The globals as the program left them, for the next REPL line, and
    /// what is left on the stack: the locals of the top-level function, and
    /// of every function and block the program was in if it failed.
    pub fn into_globals_and_stack(self) -> (Vec<Value>, Vec<Value>) {
        (self.globals, self.stack)
    }

    /// The function on the stack below the `args` arguments of a call.
    fn resolve_callee(&self, args: usize) -> Result<Function, String> {
        match &self.stack[self.stack.len() - args - 1] {
            Value::Object(obj) => match obj.as_ref() {
                Object::Function(f) => Ok(f.clone()),
                other => Err(format!(
                    "Unable to invoke {} as a function.",
                    other.static_type()
                )),
            },
            callee => Err(format!(
                "Unable to invoke {} as a function.",
                callee.static_type()
            )),
        }
    }

    /// Calls `callee`, an operator the analyser picked for each element
    /// position of the tuples among `arguments`, element by element, and
    /// returns the tuple of the results: `(1, 2) + (3, 4)` is
    /// `(1 + 3, 2 + 4)`. The callee is one function for every position, or
    /// a tuple of one for each. An argument that isn't a tuple is used at
    /// every position: `(1, 2) * 3` is `(3, 6)`. An error is at `span`, the
    /// call's, unless it has a place of its own.
    pub(crate) fn call_vec(
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

    /// Pops a value from the stack and pushes `size` unpacked elements back.
    /// Writes the elements of the compound on top of the stack to the
    /// `count` stack slots from `base`. A list or tuple is written in place;
    /// anything else, or slots past the top of the stack, go through
    /// `exec_unpack` and one write per slot, as `SetLocal` does.
    fn exec_unpack_locals(&mut self, base: usize, count: usize, span: Span) -> Result<(), VmError> {
        let len = self.stack.len();
        if base + count < len
            && let Value::Object(obj) = &self.stack[len - 1]
        {
            let obj = Rc::clone(obj);
            let in_place = match obj.as_ref() {
                Object::Tuple(seq) if seq.len() == count => {
                    self.stack[base..base + count].clone_from_slice(seq);
                    true
                }
                Object::List(seq) if seq.borrow().len() == count => {
                    self.stack[base..base + count].clone_from_slice(&seq.borrow());
                    true
                }
                _ => false,
            };
            if in_place {
                self.stack.pop();
                return Ok(());
            }
        }
        self.exec_unpack(count, span)?;
        for slot in base..base + count {
            let value = self.stack.pop().expect("stack underflow");
            if slot < self.stack.len() {
                self.stack[slot] = value;
            } else {
                self.stack.push(value);
            }
        }
        Ok(())
    }

    fn exec_unpack(&mut self, size: usize, span: Span) -> Result<(), VmError> {
        let top = self.stack.pop().expect("stack underflow");
        let Value::Object(obj) = top else {
            return Err(VmError::new(
                "cannot unpack: expected a list, tuple, or string",
                span,
            ));
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
                    return Err(VmError::new(
                        format!(
                            "cannot unpack a {type_name} of length {len} into {size} variables",
                        ),
                        span,
                    ));
                }
                // Elements are pushed in reverse order so that the first element
                // ends up on top of the stack after all pushes complete.
                match obj.as_ref() {
                    Object::List(seq) => self.stack.extend(seq.borrow().iter().rev().cloned()),
                    Object::Tuple(seq) => self.stack.extend(seq.iter().rev().cloned()),
                    _ => unreachable!(),
                }
            }
            Object::String(s) => {
                let s = s.borrow();
                let mut iter = s.chars();
                let mut chars: Vec<Value> = Vec::with_capacity(size);
                for _ in 0..size {
                    match iter.next() {
                        Some(c) => chars.push(Value::string(c.to_string())),
                        None => {
                            return Err(VmError::new(
                                format!(
                                    "cannot unpack a string of length {} into {} variables",
                                    chars.len(),
                                    size
                                ),
                                span,
                            ));
                        }
                    }
                }
                if iter.next().is_some() {
                    return Err(VmError::new(
                        format!(
                            "cannot unpack a string into {} variables: string is too long",
                            size
                        ),
                        span,
                    ));
                }
                chars.reverse();
                self.stack.append(&mut chars);
            }
            // A deque, heap or iterator unpacks the elements it gives, one
            // past the variables at most, as an iterator may not end.
            _ => {
                let Some(iter) = Value::Object(obj).try_into_iter() else {
                    return Err(VmError::new(
                        "cannot unpack: expected a list, tuple, string, deque, heap or iterator",
                        span,
                    ));
                };
                let mut elements: Vec<Value> = iter.take(size + 1).collect();
                if elements.len() != size {
                    return Err(VmError::new(
                        if elements.len() > size {
                            format!("cannot unpack more than {size} values into {size} variables")
                        } else {
                            format!(
                                "cannot unpack {} values into {size} variables",
                                elements.len()
                            )
                        },
                        span,
                    ));
                }
                elements.reverse();
                self.stack.append(&mut elements);
            }
        }
        Ok(())
    }
}

/// The key a memoised function's result for `arguments` is cached under.
/// Arguments `==` calls equal share a result: a `pure fn` takes only values
/// it compares by what they are, which hash alike when they're equal.
pub(crate) fn memo_key(arguments: &[Value]) -> u64 {
    let mut hasher = hasher();
    for argument in arguments {
        argument.hash(&mut hasher);
    }
    hasher.finish()
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

impl CallFrame {
    #[inline(always)]
    fn slot(&self, local: usize) -> usize {
        self.frame_pointer + local
    }
}

/// A callable VM function bound to the parent VM, for use in `VmNative` HOF
/// implementations. Produced by the `&mut VmCallable` input-type handler in
/// `ndc_macros::vm_convert`.
///
/// Calling `call()` runs the function inline on the parent VM via
/// `Vm::call_callback` — no child VM is allocated.
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
pub(crate) fn calls_too_deep() -> VmError {
    VmError::native(format!(
        "calls go more than {MAX_CALL_DEPTH} deep, which usually means a function calls itself without end"
    ))
}

/// What a call of a [`Function::Checked`] fails with when the function it
/// checks doesn't take `arguments`.
#[cold]
pub(crate) fn refused_arguments(arguments: &[Value]) -> VmError {
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
pub(crate) fn not_a_bool(value: &Value) -> String {
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

/// Replaces the value on top of `stack` with `Some` of it, made the
/// `target` as [`Value::converted_to`] does, or with `None` when it isn't
/// one.
#[cold]
fn try_cast(stack: &mut [Value], target: &StaticType) {
    try_cast_value(stack.last_mut().expect("stack underflow"), target);
}

/// Replaces `value` with `Some` of it, made the `target` as
/// [`Value::converted_to`] does, or with `None` when it isn't one.
#[cold]
pub(crate) fn try_cast_value(value: &mut Value, target: &StaticType) {
    *value = Value::option(value.converted_to(target));
}

/// The element of `tuple` at `index`, counting a negative index from the
/// end: `-1` is the last element.
pub(crate) fn tuple_element(tuple: &Value, index: i64) -> Result<Value, String> {
    if let Value::Object(object) = tuple
        && let Object::Tuple(elements) = object.as_ref()
    {
        let position = if index < 0 {
            elements.len().checked_sub(index.unsigned_abs() as usize)
        } else {
            usize::try_from(index).ok()
        };
        return match position.and_then(|position| elements.get(position)) {
            Some(element) => Ok(element.clone()),
            None => Err(tuple_index_out_of_bounds(index, elements.len())),
        };
    }
    Err(format!("cannot index into {}", tuple.static_type()))
}

/// The analyser rejects an index written in the code that's outside the
/// tuple; this is what a VM that's given one says.
#[cold]
fn tuple_index_out_of_bounds(index: i64, length: usize) -> String {
    let plural = if length == 1 { "" } else { "s" };
    format!("index {index} is out of bounds for a tuple of {length} element{plural}")
}

/// Checks that the value on top of `stack` is a `target`, and makes it one,
/// as [`convert`] does.
fn convert_top(stack: &mut [Value], target: &StaticType) -> bool {
    convert(stack.last_mut().expect("stack underflow"), target)
}

/// Checks that `value` is a `target`, and makes it one: a function that may
/// be a function `target` gets its calls checked (see
/// [`Value::converted_to`]).
pub(crate) fn convert(value: &mut Value, target: &StaticType) -> bool {
    if !matches!(target, StaticType::Function { .. }) {
        return value.conforms_to(target);
    }
    match value.converted_to(target) {
        Some(converted) => {
            *value = converted;
            true
        }
        None => false,
    }
}

/// Checks that the result of a call of a [`Function::Checked`], on top of
/// `stack`, is a `returns`, and makes it one, as [`convert_returned`] does.
fn convert_result(stack: &mut [Value], returns: &StaticType) -> Result<(), String> {
    convert_returned(stack.last_mut().expect("a call leaves its result"), returns)
}

/// Checks that `returned`, the result of a call of a [`Function::Checked`],
/// is a `returns`, and makes it one, as [`convert`] does: a function it
/// returns may get its own calls checked.
pub(crate) fn convert_returned(returned: &mut Value, returns: &StaticType) -> Result<(), String> {
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
pub(crate) fn cannot_cast(value: &Value, target: &StaticType) -> String {
    format!("cannot cast {} to {target}", value.diagnostic_type())
}

/// Runs the native function `f`, which doesn't take the VM, with the `args`
/// values on top of `stack`, which it takes off, and returns what it
/// returns. The arguments are a slice of the stack, copied nowhere.
#[inline(always)]
fn run_simple_native(
    stack: &mut Vec<Value>,
    f: impl Fn(&[Value]) -> Result<Value, VmError>,
    args: usize,
) -> Result<Value, VmError> {
    let start = stack.len() - args;
    let result = f(&stack[start..])?;
    stack.truncate(start);
    Ok(result)
}

/// Replaces the operands of `op` on top of `stack` with what it makes of
/// them, failing at `span`, the instruction's.
#[inline(always)]
fn run_native_op(stack: &mut Vec<Value>, op: NativeOp, span: Span) -> Result<(), VmError> {
    let result = if op.arity() == 1 {
        let value = stack.last_mut().expect("stack underflow");
        op.unary(value).map(|result| *value = result)
    } else {
        let right = stack.pop().expect("stack underflow");
        let left = stack.last_mut().expect("stack underflow");
        op.binary(left, &right).map(|result| *left = result)
    };
    result.map_err(|error| VmError::new(error.message, span))
}
