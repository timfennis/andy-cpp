use crate::chunk::OpCode;
use crate::error::VmError;
use crate::hash_map::{DefaultHasher, HashMap};
use crate::io::{InputSource, OutputSink};
use crate::iterator::{
    HeapIter, MapIter, RangeInclusiveIter, RangeIter, SeqIter, StringIter, UnboundedRangeIter,
    VmIterator,
};
use crate::value::{CompiledFunction, Function, NativeFunc, NativeFunction};
use crate::{ClosureFunction, Object, Value};
use ndc_analysis::CaptureSource;
use ndc_core::StaticType;
use ndc_lexer::Span;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::rc::Rc;

pub struct Vm {
    stack: Vec<Value>,
    globals: Vec<Value>,
    frames: Vec<CallFrame>,
    /// Output sink for `print`, `dbg`, and similar I/O built-ins.
    output: OutputSink,
    /// Input source for `read_stdin`.
    input: InputSource,
    #[cfg(feature = "trace")]
    source: Option<String>,
    #[cfg(feature = "trace")]
    tracer: Option<Box<dyn crate::tracer::VmTracer>>,
}

type MemoCache = (Rc<RefCell<HashMap<u64, Value>>>, u64);

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

/// The VM's globals for `natives`: native `i` is global slot `i`. The analyser
/// numbers natives the same way, given them in the same order.
pub fn native_globals<'a>(natives: impl IntoIterator<Item = &'a Rc<NativeFunction>>) -> Vec<Value> {
    natives
        .into_iter()
        .map(|native| {
            Value::Object(Rc::new(Object::Function(Function::Native(Rc::clone(
                native,
            )))))
        })
        .collect()
}

impl Vm {
    pub fn new(function: CompiledFunction, mut globals: Vec<Value>) -> Self {
        let num_locals = function.num_locals;
        // Every global the program declares has a slot, holding unit until its
        // declaration runs, like a local.
        if globals.len() < function.num_globals {
            globals.resize(function.num_globals, Value::unit());
        }
        let mut vm = Self {
            stack: Vec::with_capacity(256),
            globals,
            frames: vec![CallFrame {
                closure: ClosureFunction {
                    prototype: Rc::new(function),
                    upvalues: Vec::new(),
                },
                ip: 0,
                frame_pointer: 0,
                memo: None,
                returns: None,
            }],
            output: OutputSink::Stdout,
            input: InputSource::Stdin,
            #[cfg(feature = "trace")]
            source: None,
            #[cfg(feature = "trace")]
            tracer: None,
        };
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
        self.run_to_depth(0)
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
                        return Err(VmError::new(
                            format!("cannot cast {} to {target}", value.diagnostic_type()),
                            span,
                        ));
                    }
                }
                OpCode::CheckUnshared(target) => {
                    // A value that isn't a `target` fails the cast's own test
                    // instead, whether it's shared or not.
                    let value = self.stack.last().expect("stack underflow");
                    if let Some(kind) = value.shared_container(target)
                        && value.conforms_to(target)
                    {
                        return Err(VmError::new(
                            format!(
                                "cannot cast to {target}: the {kind} is also held by something else, \
                                 which would still expect what it held before"
                            ),
                            span,
                        ));
                    }
                }
                OpCode::TryCast(target) => try_cast(&mut self.stack, target),
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
                    // The analyser picked the function for each position: one
                    // for all of them, or a tuple with one for each.
                    let callee = match &self.stack[self.stack.len() - args - 1] {
                        Value::Object(obj) => Rc::clone(obj),
                        callee => {
                            return Err(VmError::new(
                                format!("Unable to invoke {} as a function.", callee.static_type()),
                                span,
                            ));
                        }
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
                        return Err(VmError::new(
                            format!(
                                "Unable to invoke {} as a function.",
                                callee.as_ref().static_type()
                            ),
                            span,
                        ));
                    };
                    let name = first.name();
                    let Some(axis_len) = vec_axis_len(&self.stack[self.stack.len() - args..])
                    else {
                        return Err(VmError::new(
                            format!(
                                "vec call '{}' requires tuple arguments of equal non-zero length",
                                name.unwrap_or("?")
                            ),
                            span,
                        ));
                    };
                    if let Err(mut e) = self.call_elementwise(args, axis_len, name, span, |i| {
                        at_position(i).cloned().ok_or_else(|| {
                            VmError::new(format!("no function for tuple position {i}"), span)
                        })
                    }) {
                        e.span.get_or_insert(span);
                        return Err(e);
                    }
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
                    let mut map = HashMap::with_capacity(pairs);
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
            let mut hasher = DefaultHasher::default();
            // Arguments `==` calls equal share a result: a `pure fn` takes
            // only values it compares by what they are, which hash alike when
            // they're equal.
            for arg in &self.stack[start..] {
                arg.hash(&mut hasher);
            }
            let key = hasher.finish();

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
        // does, as `Fn(Any) -> Int` would of a function taking an Int.
        let arguments = &self.stack[self.stack.len() - args..];
        if !function.conforms_value_args(arguments) {
            return Err(VmError::native(format!(
                "the function doesn't take the arguments ({})",
                arguments
                    .iter()
                    .map(|argument| argument.diagnostic_type().to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )));
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
                let start = self.stack.len() - args;
                let result = match &native.func {
                    NativeFunc::Simple(f) => {
                        // Zero-copy: args are a slice directly into the stack.
                        let result = f(&self.stack[start..])?;
                        self.stack.truncate(start);
                        self.stack.pop(); // callee slot
                        result
                    }
                    NativeFunc::WithVm(f) => {
                        // Drain args so we can pass `&mut self` to the HOF.
                        let call_args: Vec<Value> = self.stack.drain(start..).collect();
                        self.stack.pop(); // callee slot
                        f(&call_args, self)?
                    }
                };
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
            return Err(VmError::native(format!(
                "calls go more than {MAX_CALL_DEPTH} deep, which usually means a function calls itself without end"
            )));
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

    /// Call a function inline on this VM, without spawning a child VM.
    /// Used by `VmCallable::call` so stdlib HOFs run their predicates/mappers
    /// directly on the parent stack — zero allocation per callback invocation.
    pub fn call_callback(&mut self, func: Function, args: &[Value]) -> Result<Value, VmError> {
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
    pub fn last_value(&self, num_locals: usize) -> Value {
        if self.stack.len() > num_locals {
            self.stack.last().cloned().unwrap_or_else(Value::unit)
        } else {
            Value::unit()
        }
    }

    /// The globals as the program left them, for the next REPL line.
    pub fn into_globals(self) -> Vec<Value> {
        self.globals
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

    /// Calls the function `pick` gives for each element position of the
    /// tuple arguments on the stack, and replaces the callee and arguments
    /// with the tuple of the results: `(1, 2) + (3, 4)` is `(1 + 3, 2 + 4)`.
    fn call_elementwise(
        &mut self,
        args: usize,
        axis_len: usize,
        callee_name: Option<&str>,
        span: Span,
        pick: impl Fn(usize) -> Result<Function, VmError>,
    ) -> Result<(), VmError> {
        // Materialise the broadcast arguments up front so the inner
        // call_callback can hold &mut self without conflicting with stack
        // borrows. Element values clone Rcs only.
        let arg_values: Vec<Value> = self.stack.split_off(self.stack.len() - args);
        self.stack.pop(); // discard the callee slot

        let mut elem_args: Vec<Value> = Vec::with_capacity(args);
        let mut results: Vec<Value> = Vec::with_capacity(axis_len);
        for i in 0..axis_len {
            elem_args.clear();
            elem_args.extend(arg_values.iter().map(|arg| vec_element_at(arg, i)));
            let scalar = pick(i)?;
            let result = self.call_callback(scalar, &elem_args).map_err(|mut e| {
                let prefix = match callee_name {
                    Some(name) => format!("while vectorising '{name}' at index {i}: "),
                    None => format!("while vectorising at index {i}: "),
                };
                e.message = format!("{prefix}{}", e.message);
                e.span.get_or_insert(span);
                e
            })?;
            results.push(result);
        }

        self.stack
            .push(Value::Object(Rc::new(Object::Tuple(results))));
        Ok(())
    }

    /// Pops a value from the stack and pushes `size` unpacked elements back.
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
                // Try to avoid a clone when we're the sole owner.
                // Elements are pushed in reverse order so that the first element
                // ends up on top of the stack after all pushes complete.
                let mut elements = match Rc::try_unwrap(obj) {
                    Ok(Object::List(seq)) => seq.into_inner(),
                    Ok(Object::Tuple(seq)) => seq,
                    Ok(_) => unreachable!(),
                    Err(rc) => match rc.as_ref() {
                        Object::List(seq) => seq.borrow().to_vec(),
                        Object::Tuple(seq) => seq.clone(),
                        _ => unreachable!(),
                    },
                };
                elements.reverse();
                self.stack.append(&mut elements);
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

/// Replaces the value on top of `stack` with `Some` of it, made the
/// `target` as [`Value::converted_to`] does, or with `None` when it isn't
/// one.
#[cold]
fn try_cast(stack: &mut [Value], target: &StaticType) {
    let value = stack.last_mut().expect("stack underflow");
    *value = Value::option(value.converted_to(target));
}

/// Checks that the value on top of `stack` is a `target`, and makes it one:
/// a function that may be a function `target` gets its calls checked (see
/// [`Value::converted_to`]).
fn convert_top(stack: &mut [Value], target: &StaticType) -> bool {
    let value = stack.last_mut().expect("stack underflow");
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
/// `stack`, is a `returns`, and makes it one, as [`convert_top`] does: a
/// function it returns may get its own calls checked.
fn convert_result(stack: &mut [Value], returns: &StaticType) -> Result<(), String> {
    if convert_top(stack, returns) {
        return Ok(());
    }
    let returned = stack.last().expect("a call leaves its result");
    Err(format!(
        "expected the function to return {returns} but it returned {}",
        returned.diagnostic_type()
    ))
}
