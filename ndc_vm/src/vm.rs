use crate::chunk::OpCode;
use crate::error::VmError;
use crate::hash_map::{DefaultHasher, HashMap};
use crate::iterator::{
    HeapIter, MapIter, RangeInclusiveIter, RangeIter, SeqIter, StringIter, UnboundedRangeIter,
    VmIterator,
};
use crate::value::{CompiledFunction, Function, NativeFunc, NativeFunction};
use crate::{ClosureFunction, Object, Value};
use ndc_analysis::{CaptureSource, ResolvedVar};
use ndc_core::StaticType;
use ndc_lexer::Span;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::rc::Rc;

/// Output destination for the VM's I/O built-ins (`print`, `dbg`, etc.).
pub enum OutputSink {
    Stdout,
    Buffer(Vec<u8>),
}

impl Write for OutputSink {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Self::Stdout => std::io::stdout().write(buf),
            Self::Buffer(vec) => vec.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Self::Stdout => std::io::stdout().flush(),
            Self::Buffer(vec) => vec.flush(),
        }
    }
}

pub struct Vm {
    stack: Vec<Value>,
    globals: Vec<Value>,
    frames: Vec<CallFrame>,
    /// Output sink for `print`, `dbg`, and similar I/O built-ins.
    output: OutputSink,
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
                OpCode::CheckType(target) => {
                    if !convert_top(&mut self.stack, target) {
                        let value = self.stack.last().expect("stack underflow");
                        return Err(VmError::new(
                            format!("cannot cast {} to {target}", value.diagnostic_type()),
                            span,
                        ));
                    }
                }
                OpCode::ExpectType(target) => {
                    if !convert_top(&mut self.stack, target) {
                        let value = self.stack.last().expect("stack underflow");
                        return Err(VmError::new(
                            format!("expected {target} but found {}", value.diagnostic_type()),
                            span,
                        ));
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
                        value => {
                            let type_str = value.static_type().to_string();
                            return Err(VmError::new(
                                format!("mismatched types: expected Bool, found {type_str}"),
                                span,
                            ));
                        }
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
                        value => {
                            let type_str = value.static_type().to_string();
                            return Err(VmError::new(
                                format!("mismatched types: expected Bool, found {type_str}"),
                                span,
                            ));
                        }
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
                    if let Some(func) = self
                        .resolve_callee(args)
                        .map_err(|msg| VmError::new(msg, span))?
                    {
                        if let Err(mut e) = self.dispatch_call(func, args) {
                            e.span.get_or_insert(span);
                            return Err(e);
                        }
                    } else if let Some((overload_set, axis_len)) = self.try_vec_dispatch(args) {
                        if let Err(mut e) =
                            self.dispatch_vec_call_dynamic(&overload_set, args, axis_len, span)
                        {
                            e.span.get_or_insert(span);
                            return Err(e);
                        }
                    } else {
                        let arg_types: Vec<_> = self.stack[self.stack.len() - args..]
                            .iter()
                            .map(Value::static_type)
                            .collect();
                        let callee_name = self.callee_name(args);
                        return Err(VmError::new(
                            format!(
                                "no function called '{}' found matches the arguments: ({})",
                                callee_name.as_deref().unwrap_or("?"),
                                arg_types
                                    .iter()
                                    .map(|t| t.to_string())
                                    .collect::<Vec<_>>()
                                    .join(", "),
                            ),
                            span,
                        ));
                    }
                }
                OpCode::CallVec(args) => {
                    let args = *args;
                    // The analyser picked the scalar function, which the
                    // compiler loaded directly: no overload set to probe.
                    let scalar = {
                        let callee = &self.stack[self.stack.len() - args - 1];
                        let Value::Object(obj) = callee else {
                            return Err(VmError::new(
                                format!("Unable to invoke {} as a function.", callee.static_type()),
                                span,
                            ));
                        };
                        let Object::Function(f) = obj.as_ref() else {
                            return Err(VmError::new(
                                format!(
                                    "Unable to invoke {} as a function.",
                                    obj.as_ref().static_type()
                                ),
                                span,
                            ));
                        };
                        f.clone()
                    };
                    let axis_len = match vec_axis_len(&self.stack[self.stack.len() - args..]) {
                        Some(n) => n,
                        None => {
                            return Err(VmError::new(
                                format!(
                                    "vec call '{}' requires tuple arguments of equal non-zero length",
                                    scalar.name().unwrap_or("?")
                                ),
                                span,
                            ));
                        }
                    };
                    if let Err(mut e) =
                        self.call_elementwise(args, axis_len, scalar.name(), span, |_, _, _| {
                            Ok(scalar.clone())
                        })
                    {
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
                OpCode::MakeOverloads(size) => {
                    let size = *size;
                    self.make_overloads(size, span)?;
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
            for arg in &self.stack[start..] {
                arg.hash(&mut hasher);
                // A function checked as another function type is the same
                // function, but a call may return it, so the cache tells
                // them apart.
                if let Value::Object(object) = arg
                    && let Object::Function(Function::Checked { typ, .. }) = object.as_ref()
                {
                    typ.hash(&mut hasher);
                }
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

    /// Replaces the `size` functions on top of the stack with a
    /// [`Function::Overloads`] of them.
    #[cold]
    fn make_overloads(&mut self, size: usize, span: Span) -> Result<(), VmError> {
        let candidates = self
            .stack
            .split_off(self.stack.len() - size)
            .into_iter()
            .map(|candidate| match candidate {
                Value::Object(object) => match object.as_ref() {
                    Object::Function(function) => Ok(function.clone()),
                    other => Err(other.static_type()),
                },
                other => Err(other.static_type()),
            })
            .collect::<Result<Rc<[Function]>, _>>()
            .map_err(|found| {
                VmError::new(format!("Unable to invoke {found} as a function."), span)
            })?;
        self.stack
            .push(Value::function(Function::Overloads(candidates)));
        Ok(())
    }

    /// Calls the first of `candidates`, a [`Function::Overloads`], that
    /// accepts the arguments on the stack.
    #[cold]
    fn call_overloads(
        &mut self,
        candidates: &[Function],
        args: usize,
    ) -> Result<Dispatch, VmError> {
        let arguments = &self.stack[self.stack.len() - args..];
        let picked = candidates
            .iter()
            .find(|candidate| candidate.matches_value_args(arguments))
            .or_else(|| {
                candidates
                    .iter()
                    .find(|candidate| candidate.conforms_value_args(arguments))
            })
            .cloned();
        let Some(picked) = picked else {
            let types = arguments
                .iter()
                .map(|argument| argument.static_type().to_string())
                .collect::<Vec<_>>()
                .join(", ");
            let name = candidates.first().and_then(Function::name).unwrap_or("?");
            return Err(VmError::native(format!(
                "no function called '{name}' found matches the arguments: ({types})"
            )));
        };
        self.dispatch_call(picked, args)
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
            Function::Overloads(candidates) => return self.call_overloads(&candidates, args),
            Function::Memoized { .. } => {
                // Already unwrapped in dispatch_call; this branch is unreachable.
                unreachable!(
                    "Memoized function should have been unwrapped before dispatch_call_with_memo"
                )
            }
        };
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

    /// Resolves the callee on the stack to a scalar `Function`. Direct
    /// `Object::Function` and `OverloadSet` scalars both go through this
    /// path — for an `OverloadSet`, the scalar list is walked first-match-
    /// wins (no vec dispatch yet). `Ok(None)` means no scalar matched; the
    /// caller falls through to vec dispatch.
    ///
    /// Returns `Option<Function>` (not the wider `Callable` enum) so the
    /// arm body in the dispatch loop stays the same shape as master — that
    /// matters: earlier drafts that widened the return type or split this
    /// into two helpers regressed numerics-heavy benches by ~10%.
    fn resolve_callee(&self, args: usize) -> Result<Option<Function>, String> {
        match &self.stack[self.stack.len() - args - 1] {
            Value::Object(obj) => match obj.as_ref() {
                Object::Function(f) => Ok(Some(f.clone())),
                Object::OverloadSet {
                    scalars,
                    vec_candidates,
                } => {
                    let start = self.stack.len() - args;
                    let args = &self.stack[start..];
                    // An operator on tuples applies to each element, which
                    // comes before looking inside containers for a scalar
                    // overload: that's only before the call fails.
                    let elementwise = !vec_candidates.is_empty() && vec_axis_len(args).is_some();
                    Ok(self.find_scalar_overload(scalars, args, !elementwise))
                }
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

    /// Probes `OpCode::Call`'s callee for vec dispatch eligibility without
    /// allocating: returns the `OverloadSet` Rc and broadcast axis length
    /// when vec applies. `dispatch_vec_call_dynamic` resolves the candidates
    /// lazily from there, avoiding a per-call `Vec<Function>` allocation.
    fn try_vec_dispatch(&self, args: usize) -> Option<(Rc<Object>, usize)> {
        let Value::Object(obj) = &self.stack[self.stack.len() - args - 1] else {
            return None;
        };
        match obj.as_ref() {
            Object::OverloadSet { vec_candidates, .. } if !vec_candidates.is_empty() => {
                let start = self.stack.len() - args;
                let axis_len = vec_axis_len(&self.stack[start..])?;
                Some((Rc::clone(obj), axis_len))
            }
            _ => None,
        }
    }

    /// Returns the name of the callee at the given stack position, if known.
    /// For a direct `Function` this is its own name; for an `OverloadSet` it
    /// reads the name off the first scalar (or vec) candidate.
    fn callee_name(&self, args: usize) -> Option<String> {
        let frame_pointer = self.frames.last()?.frame_pointer;
        match &self.stack[self.stack.len() - args - 1] {
            Value::Object(obj) => match obj.as_ref() {
                Object::Function(f) => f.name().map(str::to_string),
                Object::OverloadSet {
                    scalars,
                    vec_candidates,
                } => {
                    let first = scalars.first().or_else(|| vec_candidates.first())?;
                    let value = self.resolve_var(first, frame_pointer);
                    let Value::Object(obj) = value else {
                        return None;
                    };
                    let Object::Function(f) = obj.as_ref() else {
                        return None;
                    };
                    f.name().map(str::to_string)
                }
                _ => None,
            },
            _ => None,
        }
    }

    /// Walks the `OverloadSet`'s scalar candidates in priority order,
    /// returning the first whose parameter types accept `args`. Mirrors
    /// master's `find_overload` body so the hot path on numerics-heavy
    /// code stays at the same shape and footprint. A parameter like
    /// `List<Int>` isn't checked element by element on that path, so when no
    /// candidate accepts the arguments that way, they're checked again
    /// element by element before the call fails, if `look_inside`: a struct
    /// with a `List<Int>` field can be made through `Any` with a list of
    /// Ints.
    fn find_scalar_overload(
        &self,
        scalars: &[ResolvedVar],
        args: &[Value],
        look_inside: bool,
    ) -> Option<Function> {
        let frame_pointer = self.frames.last().expect("no frame").frame_pointer;
        let function = |var| {
            let value = self.resolve_var(var, frame_pointer);
            let Value::Object(obj) = value else {
                return None;
            };
            let Object::Function(f) = obj.as_ref() else {
                return None;
            };
            Some(f.clone())
        };
        scalars
            .iter()
            .find_map(|var| function(var).filter(|f| f.matches_value_args(args)))
            .or_else(|| {
                look_inside.then(|| {
                    scalars
                        .iter()
                        .find_map(|var| function(var).filter(|f| f.conforms_value_args(args)))
                })?
            })
    }

    /// Vec dispatch for `Binding::Dynamic` operator calls: the program picks
    /// the scalar overload for each element position, trying the one the
    /// previous position used first, so a tuple whose elements all have one
    /// type picks once.
    fn dispatch_vec_call_dynamic(
        &mut self,
        overload_set: &Rc<Object>,
        args: usize,
        axis_len: usize,
        span: Span,
    ) -> Result<(), VmError> {
        let Object::OverloadSet { vec_candidates, .. } = overload_set.as_ref() else {
            unreachable!("dispatch_vec_call_dynamic invoked with non-OverloadSet callee");
        };
        debug_assert!(!vec_candidates.is_empty());
        let frame_pointer = self.frames.last().expect("no frame").frame_pointer;

        // Resolve one candidate up front so the error paths can borrow its name
        // as `&str` instead of allocating a `String` on every successful call.
        let callee_fn: Option<Rc<Object>> =
            vec_candidates
                .first()
                .and_then(|var| match self.resolve_var(var, frame_pointer) {
                    Value::Object(obj) if matches!(obj.as_ref(), Object::Function(_)) => Some(obj),
                    _ => None,
                });
        let callee_name: Option<&str> = callee_fn.as_ref().and_then(|obj| {
            let Object::Function(f) = obj.as_ref() else {
                return None;
            };
            f.name()
        });

        let mut last_match: Option<Function> = None;
        self.call_elementwise(args, axis_len, callee_name, span, |vm, elements, i| {
            if let Some(f) = &last_match
                && f.matches_value_args(elements)
            {
                return Ok(f.clone());
            }
            let found = vec_candidates.iter().find_map(|var| {
                let Value::Object(obj) = vm.resolve_var(var, frame_pointer) else {
                    return None;
                };
                let Object::Function(f) = obj.as_ref() else {
                    return None;
                };
                f.matches_value_args(elements).then(|| f.clone())
            });
            let Some(f) = found else {
                let element_types = elements
                    .iter()
                    .map(|v| v.static_type().to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                let name = callee_name.unwrap_or("?");
                return Err(VmError::new(
                    format!("no overload of '{name}' accepts element {i}: ({element_types})"),
                    span,
                ));
            };
            last_match = Some(f.clone());
            Ok(f)
        })
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
        mut pick: impl FnMut(&mut Self, &[Value], usize) -> Result<Function, VmError>,
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
            let scalar = pick(self, &elem_args, i)?;
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
            _ => {
                return Err(VmError::new(
                    "cannot unpack: expected a list, tuple, or string",
                    span,
                ));
            }
        }
        Ok(())
    }

    fn resolve_var(&self, var: &ResolvedVar, frame_pointer: usize) -> Value {
        match var {
            ResolvedVar::Global { slot } => self.globals[*slot].clone(),
            ResolvedVar::Local { slot } => self.stack[frame_pointer + slot].clone(),
            ResolvedVar::Cell { slot } => {
                let value = &self.stack[frame_pointer + slot];
                match value.as_cell() {
                    Some(cell) => cell.borrow().clone(),
                    None => value.clone(),
                }
            }
            ResolvedVar::Upvalue { slot } => {
                let frame = self.frames.last().expect("no frame");
                frame.closure.upvalues[*slot].borrow().clone()
            }
        }
    }
}

/// Find the broadcast-axis length for a vec call: the shared length of every
/// tuple-shaped argument. Empty tuples and length mismatches return `None`,
/// which is interpreted upstream as "vec doesn't apply" and falls through to
/// the regular `no function found` error.
pub(crate) fn vec_axis_len(args: &[Value]) -> Option<usize> {
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
pub(crate) fn vec_element_at(arg: &Value, i: usize) -> Value {
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

/// Explain why a range bound was rejected. Only an integer too big for an
/// `Int` is a size problem; every other non-`Int` bound is a type problem, and
/// since `n` literals are the only way to write a big integer, a small
/// `Number` bound is now the common case.
fn bad_range_bound(value: &Value) -> String {
    if let Some(number) = value.as_number()
        && matches!(number, crate::value::AdvancedNumber::Int(_))
        && number.as_exact_i64().is_none()
    {
        return "Integer too large for range bounds".to_string();
    }
    format!("range bounds must be Int, got {}", value.diagnostic_type())
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
