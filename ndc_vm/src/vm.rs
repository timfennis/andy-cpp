use crate::chunk::OpCode;
use crate::error::VmError;
use crate::iterator::{
    HeapIter, MapIter, RangeInclusiveIter, RangeIter, SeqIter, StringIter, UnboundedRangeIter,
    VmIterator,
};
use crate::value::{CompiledFunction, Function, NativeFunc};
use crate::{ClosureFunction, Object, UpvalueCell, Value};
use ndc_core::hash_map::{DefaultHasher, HashMap};
use ndc_lexer::Span;
use ndc_parser::{CaptureSource, ResolvedVar};
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::ops::Deref;
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
    /// All currently-open upvalue cells, keyed by absolute stack slot.
    /// When a frame exits, cells pointing into that frame's stack region are
    /// "closed" — the live value is copied out of the stack into the cell so
    /// the closure can still read/write it after the frame is gone.
    /// Two closures capturing the same local share one cell via this list.
    open_upvalues: Vec<Rc<RefCell<UpvalueCell>>>,
    /// Output sink for `print`, `dbg`, and similar I/O built-ins.
    output: OutputSink,
    #[cfg(feature = "trace")]
    source: Option<String>,
    #[cfg(feature = "trace")]
    tracer: Option<Box<dyn crate::tracer::VmTracer>>,
}

type MemoCache = (Rc<RefCell<HashMap<u64, Value>>>, u64);

pub struct CallFrame {
    closure: ClosureFunction,
    ip: usize,
    /// Absolute index of the first local slot for this frame.
    frame_pointer: usize,
    /// If this frame was called from a `pure` (memoized) function, holds the
    /// cache to write into and the hash key when the frame returns.  `None`
    /// for ordinary (non-memoized) calls.
    memo: Option<MemoCache>,
}

impl Vm {
    pub fn new(function: CompiledFunction, globals: Vec<Value>) -> Self {
        let num_locals = function.num_locals;
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
            }],
            open_upvalues: Vec::new(),
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
                    let frame = self.frames.last().expect("no frame");
                    let frame_pointer = frame.frame_pointer;
                    // If this was a memoized call, cache the return value now,
                    // before the frame is torn down.
                    if let Some((cache, key)) = &frame.memo {
                        cache.borrow_mut().insert(*key, ret.clone());
                    }
                    if !self.open_upvalues.is_empty() {
                        self.close_upvalues(frame_pointer);
                    }
                    if frame_pointer == 0 {
                        return Err(VmError::new("Return at top level", span));
                    }
                    self.stack.truncate(frame_pointer - 1);
                    self.frames.pop().expect("no frame to pop");
                    self.stack.push(ret);
                    if self.frames.len() == target_depth {
                        break Ok(());
                    }
                }
                OpCode::Constant(idx) => {
                    let idx = *idx;
                    self.stack
                        .push(frame.closure.prototype.body.constant(idx).clone());
                }
                OpCode::GetLocal(slot) => {
                    let slot = *slot;
                    self.stack.push(self.stack[frame.slot(slot)].clone());
                }
                OpCode::GetGlobal(slot) => {
                    let slot = *slot;
                    self.stack.push(self.globals[slot].clone());
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
                            frame.ip = frame.ip.wrapping_add_signed(offset);
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
                            frame.ip = frame.ip.wrapping_add_signed(offset);
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
                    let offset = *offset;
                    frame.ip = frame.ip.wrapping_add_signed(offset);
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
                    } else if let Some((scalars, axis_len)) = self.try_vec_dispatch(args) {
                        if let Err(mut e) = self.dispatch_vec_call(&scalars, args, axis_len, span) {
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
                    // Compiler emitted a directly-loaded scalar function — no
                    // OverloadSet probing needed. We still verify it's callable
                    // because the analyser may have widened its slot to Any
                    // somewhere along the way.
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
                    let scalars = [scalar];
                    if let Err(mut e) = self.dispatch_vec_call(&scalars, args, axis_len, span) {
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
                    let mut map = HashMap::new();
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
                    let slot = *slot;
                    match frame.closure.upvalues[slot].borrow().deref() {
                        UpvalueCell::Open(slot) => self.stack.push(self.stack[*slot].clone()),
                        UpvalueCell::Closed(value) => self.stack.push(value.clone()),
                    }
                }
                OpCode::SetUpvalue(slot) => {
                    let slot = *slot;
                    let value = self.stack.pop().expect("stack underflow");
                    let mut cell = frame.closure.upvalues[slot].borrow_mut();
                    match &mut *cell {
                        UpvalueCell::Open(stack_slot) => self.stack[*stack_slot] = value,
                        UpvalueCell::Closed(stored) => *stored = value,
                    }
                }
                OpCode::GetIterator => {
                    let val = self.stack.pop().expect("stack underflow");

                    // Check if already an iterator
                    if matches!(val, Value::Object(ref obj) if matches!(**obj, Object::Iterator(_)))
                    {
                        self.stack.push(val);
                        continue;
                    }

                    // Get type string before moving val
                    let type_str = format!("{}", val.static_type());

                    // Try to create an iterator
                    let iter_val = match val {
                        Value::Object(rc) => match rc.as_ref() {
                            Object::List(_) | Object::Tuple(_) | Object::Deque(_) => {
                                Some(Value::iterator(Rc::new(RefCell::new(SeqIter::new(
                                    Rc::clone(&rc),
                                )))))
                            }
                            Object::String(s) => Some(Value::iterator(Rc::new(RefCell::new(
                                StringIter::new(Rc::clone(s)),
                            )))),
                            Object::Map { .. } => Some(Value::iterator(Rc::new(RefCell::new(
                                MapIter::new(Rc::clone(&rc)),
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
                                format!("{} is not iterable", type_str),
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
                            frame.ip = frame.ip.wrapping_add_signed(offset);
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
                            return Err(VmError::new("Integer too large for range bounds", span));
                        };
                        Some(n)
                    } else {
                        None
                    };
                    let start = self.stack.pop().expect("stack underflow");
                    let Value::Int(start) = start else {
                        return Err(VmError::new("Integer too large for range bounds", span));
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
                    // and before `capture_upvalue` needs `&mut self`.
                    let idx = *idx;
                    let values = Rc::clone(values);
                    let frame = self.frames.last().expect("no frame");
                    let Value::Object(obj) = frame.closure.prototype.body.constant(idx) else {
                        panic!("Closure opcode: constant at index {idx} is not an Object");
                    };
                    let Object::Function(Function::Compiled(compiled)) = &**obj else {
                        panic!(
                            "Closure opcode: constant at index {idx} is not a Compiled function"
                        );
                    };
                    let compiled = Rc::clone(compiled);
                    let frame_pointer = frame.frame_pointer;
                    // Pre-clone parent upvalue Rcs so we can drop the frame borrow
                    // before calling capture_upvalue (which needs &mut self).
                    let parent_upvalues: Vec<_> =
                        frame.closure.upvalues.iter().map(Rc::clone).collect();
                    let upvalues = values
                        .iter()
                        .map(|c| match c {
                            CaptureSource::Local(slot) => {
                                self.capture_upvalue(frame_pointer + slot)
                            }
                            CaptureSource::Upvalue(slot) => Rc::clone(&parent_upvalues[*slot]),
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
                OpCode::CloseUpvalue(slot) => {
                    let slot = *slot;
                    let frame_pointer = self.frames.last().expect("no frame").frame_pointer;
                    self.close_upvalues(frame_pointer + slot);
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
                        function: Box::new(func),
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

    /// Returns a shared upvalue cell for the given absolute stack slot, creating
    /// one if it doesn't exist yet. Two closures capturing the same local get the
    /// same `Rc` so mutations from either side are immediately visible to both.
    fn capture_upvalue(&mut self, stack_slot: usize) -> Rc<RefCell<UpvalueCell>> {
        if let Some(cell) = self
            .open_upvalues
            .iter()
            .find(|c| matches!(*c.borrow(), UpvalueCell::Open(s) if s == stack_slot))
        {
            return Rc::clone(cell);
        }
        let cell = Rc::new(RefCell::new(UpvalueCell::Open(stack_slot)));
        self.open_upvalues.push(Rc::clone(&cell));
        cell
    }

    /// Closes every open upvalue whose stack slot falls within the region
    /// starting at `frame_pointer`. Called just before a frame's stack window
    /// is reclaimed so that closures retaining those cells keep live copies.
    fn close_upvalues(&mut self, frame_pointer: usize) {
        self.open_upvalues.retain_mut(|cell| {
            let mut borrow = cell.borrow_mut();
            if let UpvalueCell::Open(slot) = *borrow
                && slot >= frame_pointer
            {
                *borrow = UpvalueCell::Closed(self.stack[slot].clone());
                return false;
            }
            true
        });
    }

    fn dispatch_call(&mut self, func: Function, args: usize) -> Result<(), VmError> {
        // Memoized functions check the cache first.  On a hit we short-circuit
        // without pushing a new frame.  On a miss we dispatch the inner
        // function normally and tag the new frame so `Return` will populate
        // the cache once the call finishes.
        if let Function::Memoized { cache, function } = func {
            let start = self.stack.len() - args;
            let mut hasher = DefaultHasher::default();
            for arg in &self.stack[start..] {
                arg.hash(&mut hasher);
            }
            let key = hasher.finish();

            if let Some(cached) = cache.borrow().get(&key).cloned() {
                // Cache hit: discard the callee slot and arguments, push result.
                self.stack.truncate(start - 1);
                self.stack.push(cached);
                return Ok(());
            }

            // Cache miss: dispatch the inner function and remember to store
            // the result in the cache when this frame returns.
            self.dispatch_call_with_memo(*function, args, Some((cache, key)))
        } else {
            self.dispatch_call_with_memo(func, args, None)
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
    ) -> Result<(), VmError> {
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
                return Ok(());
            }
            Function::Closure(c) => c,
            Function::Compiled(f) => ClosureFunction {
                prototype: f,
                upvalues: Vec::new(),
            },
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
        });
        for _ in args..num_locals {
            self.stack.push(Value::unit());
        }
        Ok(())
    }

    /// Call a VM function with the given arguments, using a fresh VM instance.
    /// Used by the interpreter bridge (`vm_to_interp_callable`) to re-enter the
    /// VM from the tree-walk interpreter side. The hot path for VM-native HOFs
    /// uses `call_callback` instead, which runs inline on the parent VM.
    pub fn call_function(
        func: Function,
        args: Vec<Value>,
        globals: Vec<Value>,
    ) -> Result<Value, VmError> {
        let mut vm = Self {
            stack: Vec::new(),
            globals,
            frames: Vec::new(),
            open_upvalues: Vec::new(),
            output: OutputSink::Stdout,
            #[cfg(feature = "trace")]
            source: None,
            #[cfg(feature = "trace")]
            tracer: None,
        };
        vm.call_callback(func, args)
    }

    /// Call a function inline on this VM, without spawning a child VM.
    /// Used by `VmCallable::call` so stdlib HOFs run their predicates/mappers
    /// directly on the parent stack — zero allocation per callback invocation.
    pub fn call_callback(&mut self, func: Function, args: Vec<Value>) -> Result<Value, VmError> {
        if let Function::Native(native) = func {
            match &native.func {
                NativeFunc::Simple(f) => f(&args),
                NativeFunc::WithVm(f) => f(&args, self),
            }
        } else {
            let depth = self.frames.len();
            let n = args.len();
            self.stack.push(Value::unit()); // dummy callee slot
            self.stack.extend(args);
            self.dispatch_call_with_memo(func, n, None)?;
            self.run_to_depth(depth)?;
            Ok(self.stack.pop().expect("callback must produce a value"))
        }
    }

    /// Expose the globals slice for use by the interpreter bridge.
    pub fn globals(&self) -> &[Value] {
        &self.globals
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

    /// Resume execution with a new (extended) compiled function, starting at
    /// `resume_ip` — the instruction index where the previous `Halt` was.
    /// Used by the REPL to keep a single VM alive across lines.
    ///
    /// The stack is trimmed to `prev_num_locals` (discarding any leftover
    /// expression result from the previous run) and then extended with
    /// `Value::unit()` for any new local slots the new function introduces.
    /// Upvalues for preserved locals remain open; any pointing above
    /// `prev_num_locals` are closed before the truncation.
    pub fn resume_from_halt(
        &mut self,
        function: CompiledFunction,
        globals: Vec<Value>,
        resume_ip: usize,
        prev_num_locals: usize,
    ) {
        let new_num_locals = function.num_locals;
        self.globals = globals;
        self.close_upvalues(prev_num_locals);
        self.stack.truncate(prev_num_locals);
        for _ in prev_num_locals..new_num_locals {
            self.stack.push(Value::unit());
        }
        self.frames = vec![CallFrame {
            closure: ClosureFunction {
                prototype: Rc::new(function),
                upvalues: Vec::new(),
            },
            ip: resume_ip,
            frame_pointer: 0,
            memo: None,
        }];
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
                Object::OverloadSet { scalars, .. } => {
                    let start = self.stack.len() - args;
                    Ok(self.find_scalar_overload(scalars, &self.stack[start..]))
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

    /// Vec-dispatch fallback for `OpCode::Call`. Returns the matched vec
    /// scalars and the broadcast axis length when the callee is an
    /// `OverloadSet` with at least one vec candidate *and* the args have
    /// a consistent tuple axis length (`vec_axis_len`).
    ///
    /// Returns `None` to mean "vec doesn't apply" — caller errors out.
    fn try_vec_dispatch(&self, args: usize) -> Option<(Vec<Function>, usize)> {
        let Value::Object(obj) = &self.stack[self.stack.len() - args - 1] else {
            return None;
        };
        let Object::OverloadSet { vec_candidates, .. } = obj.as_ref() else {
            return None;
        };
        if vec_candidates.is_empty() {
            return None;
        }
        let start = self.stack.len() - args;
        let axis_len = vec_axis_len(&self.stack[start..])?;
        let frame_pointer = self.frames.last().expect("no frame").frame_pointer;
        let mut scalars: Vec<Function> = Vec::with_capacity(vec_candidates.len());
        for var in vec_candidates {
            let value = self.resolve_var(var, frame_pointer);
            let Value::Object(obj) = value else { continue };
            let Object::Function(f) = obj.as_ref() else {
                continue;
            };
            scalars.push(f.clone());
        }
        if scalars.is_empty() {
            None
        } else {
            Some((scalars, axis_len))
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
    /// code stays at the same shape and footprint.
    fn find_scalar_overload(&self, scalars: &[ResolvedVar], args: &[Value]) -> Option<Function> {
        let frame_pointer = self.frames.last().expect("no frame").frame_pointer;
        scalars.iter().find_map(|var| {
            let value = self.resolve_var(var, frame_pointer);
            let Value::Object(obj) = value else {
                return None;
            };
            let Object::Function(f) = obj.as_ref() else {
                return None;
            };
            f.matches_value_args(args).then(|| f.clone())
        })
    }

    /// Broadcast `scalars` across `axis_len` element positions, looking up the
    /// matching scalar per pair when more than one is in play. Non-tuple args
    /// broadcast unchanged.
    ///
    /// **Fast path**: when `scalars` has exactly one entry — which is the
    /// `CallVec` opcode case and the "pinned single scalar" `Resolved(Vec)`
    /// case at runtime — the per-element overload probe is skipped and the
    /// scalar is called directly. This is the bulk of the win over the PR's
    /// always-probe dispatcher.
    fn dispatch_vec_call(
        &mut self,
        scalars: &[Function],
        args: usize,
        axis_len: usize,
        span: Span,
    ) -> Result<(), VmError> {
        let arg_start = self.stack.len() - args;
        let callee_name = self.callee_name(args);

        // Materialise the broadcast arguments up front so the inner
        // call_callback can hold &mut self without conflicting with stack
        // borrows. Element values clone Rcs only.
        let arg_values: Vec<Value> = self.stack.split_off(arg_start);
        self.stack.pop(); // discard the callee slot

        let pinned: Option<&Function> = if scalars.len() == 1 {
            Some(&scalars[0])
        } else {
            None
        };

        let mut elem_args: Vec<Value> = Vec::with_capacity(args);
        let mut results: Vec<Value> = Vec::with_capacity(axis_len);

        for i in 0..axis_len {
            elem_args.clear();
            for arg in &arg_values {
                elem_args.push(vec_element_at(arg, i));
            }

            let scalar: Function = if let Some(f) = pinned {
                f.clone()
            } else {
                let Some(found) = scalars.iter().find(|f| f.matches_value_args(&elem_args)) else {
                    let element_types = elem_args
                        .iter()
                        .map(|v| v.static_type().to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    let name = callee_name.as_deref().unwrap_or("?");
                    return Err(VmError::new(
                        format!("no overload of '{name}' accepts element {i}: ({element_types})"),
                        span,
                    ));
                };
                found.clone()
            };

            let call_args = std::mem::replace(&mut elem_args, Vec::with_capacity(args));
            let result = self.call_callback(scalar, call_args).map_err(|mut e| {
                let prefix = match &callee_name {
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
            ResolvedVar::Upvalue { slot } => {
                let frame = self.frames.last().expect("no frame");
                match &*frame.closure.upvalues[*slot].borrow() {
                    UpvalueCell::Open(stack_slot) => self.stack[*stack_slot].clone(),
                    UpvalueCell::Closed(value) => value.clone(),
                }
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
    pub fn call(&mut self, args: Vec<Value>) -> Result<Value, VmError> {
        self.vm.call_callback(self.function.clone(), args)
    }
}
