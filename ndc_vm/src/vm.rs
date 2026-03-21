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
            OutputSink::Stdout => std::io::stdout().write(buf),
            OutputSink::Buffer(vec) => vec.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            OutputSink::Stdout => std::io::stdout().flush(),
            OutputSink::Buffer(vec) => vec.flush(),
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
    #[cfg(feature = "vm-trace")]
    source: Option<String>,
}

pub struct CallFrame {
    closure: ClosureFunction,
    ip: usize,
    /// Absolute index of the first local slot for this frame.
    frame_pointer: usize,
    /// If this frame was called from a `pure` (memoized) function, holds the
    /// cache to write into and the hash key when the frame returns.  `None`
    /// for ordinary (non-memoized) calls.
    memo: Option<(Rc<RefCell<HashMap<u64, Value>>>, u64)>,
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
                    upvalues: vec![],
                },
                ip: 0,
                frame_pointer: 0,
                memo: None,
            }],
            open_upvalues: Vec::new(),
            output: OutputSink::Stdout,
            #[cfg(feature = "vm-trace")]
            source: None,
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

    #[cfg(feature = "vm-trace")]
    pub fn with_source(mut self, source: impl Into<String>) -> Self {
        self.source = Some(source.into());
        self
    }

    #[cfg(feature = "vm-trace")]
    pub fn set_source(&mut self, source: impl Into<String>) {
        self.source = Some(source.into());
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

        loop {
            let frame = self.frames.last_mut().expect("must not be empty");
            let ip = frame.ip;
            let span = frame.closure.prototype.body.span(ip);
            let op = frame.closure.prototype.body.opcode(ip);
            frame.ip += 1;

            #[cfg(feature = "vm-trace")]
            {
                let excerpt = self
                    .source
                    .as_deref()
                    .and_then(|src| src.get(span.range()).map(|s| s.trim().replace('\n', "↵")));
                let op_str = format!("{op:?}");
                match excerpt {
                    Some(s) => eprintln!("[VM] {ip:04} {op_str:<30}  {s}"),
                    None => eprintln!("[VM] {ip:04} {op_str}"),
                }
            }

            match op {
                OpCode::Halt => {
                    return Ok(());
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
                    self.close_upvalues(frame_pointer);
                    if frame_pointer == 0 {
                        return Err(VmError::new("Return at top level", span));
                    }
                    self.stack.truncate(frame_pointer - 1);
                    self.frames.pop().expect("no frame to pop");
                    self.stack.push(ret);
                    if self.frames.len() == target_depth {
                        return Ok(());
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
                    } else if let Some(result) = self.try_vectorized_call(args, span)? {
                        self.stack.push(result);
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
                        panic!("ListPush expects a list")
                    };
                    let Object::List(rc) = &**obj else {
                        panic!("ListPush expects a list")
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
                        panic!("MapInsert expects a map")
                    };
                    let Object::Map { entries, .. } = &**obj else {
                        panic!("MapInsert expects a map")
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

            #[cfg(feature = "vm-trace")]
            {
                dbg!(&self.stack);
            }
        }
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
        memo: Option<(Rc<RefCell<HashMap<u64, Value>>>, u64)>,
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
                upvalues: vec![],
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
            #[cfg(feature = "vm-trace")]
            source: None,
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
                upvalues: vec![],
            },
            ip: resume_ip,
            frame_pointer: 0,
            memo: None,
        }];
    }

    /// Resolves the callee on the stack to a concrete `Function`. Returns `Ok(None)`
    /// when the callee is an overload set and no candidate matches the argument
    /// types — the caller should then try a vectorized fallback. Returns `Err` when
    /// the callee is not callable at all.
    fn resolve_callee(&self, args: usize) -> Result<Option<Function>, String> {
        match &self.stack[self.stack.len() - args - 1] {
            Value::Object(obj) => match obj.as_ref() {
                Object::Function(f) => Ok(Some(f.clone())),
                Object::OverloadSet(candidates) => {
                    let start = self.stack.len() - args;
                    Ok(self.find_overload(candidates, &self.stack[start..]))
                }
                obj => Err(format!(
                    "Unable to invoke {} as a function.",
                    obj.static_type()
                )),
            },
            callee => Err(format!(
                "Unable to invoke {} as a function.",
                callee.static_type()
            )),
        }
    }

    /// Returns the name of the callee at the given stack position, if known.
    /// For a direct `Function` this is its own name; for an `OverloadSet` it
    /// reads the name off the first resolved candidate.
    fn callee_name(&self, args: usize) -> Option<String> {
        let frame_pointer = self.frames.last()?.frame_pointer;
        match &self.stack[self.stack.len() - args - 1] {
            Value::Object(obj) => match obj.as_ref() {
                Object::Function(f) => f.name().map(str::to_string),
                Object::OverloadSet(candidates) => candidates.first().and_then(|var| {
                    let value = self.resolve_var(var, frame_pointer);
                    let Value::Object(obj) = value else {
                        return None;
                    };
                    let Object::Function(f) = obj.as_ref() else {
                        return None;
                    };
                    f.name().map(str::to_string)
                }),
                _ => None,
            },
            _ => None,
        }
    }

    /// Searches an overload set for the first candidate whose type signature
    /// accepts the given argument types.
    fn find_overload(&self, candidates: &[ResolvedVar], args: &[Value]) -> Option<Function> {
        let frame_pointer = self.frames.last().expect("no frame").frame_pointer;
        candidates.iter().find_map(|var| {
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

    /// Applies a binary operator element-wise over numeric tuples.
    ///
    /// Returns `Some(result_tuple)` when both arguments (or one argument and
    /// one scalar) are numeric tuples of compatible shape and an inner function
    /// can be found for the element types.  Returns `None` when vectorization
    /// does not apply.
    fn try_vectorized_call(&mut self, args: usize, span: Span) -> Result<Option<Value>, VmError> {
        if args != 2 {
            return Ok(None);
        }

        let callee_idx = self.stack.len() - args - 1;

        // Use a block to scope all shared borrows of self.stack so they are
        // dropped before the &mut self calls (call_callback, truncate) below.
        let (inner_fn, pairs) = {
            // P5: borrow candidates rather than cloning the Vec.
            let candidates: &[ResolvedVar] = match &self.stack[callee_idx] {
                Value::Object(obj) => match obj.as_ref() {
                    Object::OverloadSet(candidates) => candidates,
                    _ => return Ok(None),
                },
                _ => return Ok(None),
            };

            let left = &self.stack[self.stack.len() - 2];
            let right = &self.stack[self.stack.len() - 1];

            // Check shape, build a two-element probe for overload lookup, and
            // extract the element pairs all in one pass — avoiding the redundant
            // as_numeric_tuple calls that a separate vectorization_pairs would do.
            let left_tup = as_numeric_tuple(left);
            let right_tup = as_numeric_tuple(right);
            let (probe, pairs): ([Value; 2], Vec<(Value, Value)>) = match (left_tup, right_tup) {
                (Some(ls), Some(rs)) if ls.len() == rs.len() => (
                    [ls[0].clone(), rs[0].clone()],
                    ls.iter().cloned().zip(rs.iter().cloned()).collect(),
                ),
                (None, Some(rs)) if left.is_number() => (
                    [left.clone(), rs[0].clone()],
                    rs.iter().map(|r| (left.clone(), r.clone())).collect(),
                ),
                (Some(ls), None) if right.is_number() => (
                    [ls[0].clone(), right.clone()],
                    ls.iter().map(|l| (l.clone(), right.clone())).collect(),
                ),
                _ => return Ok(None),
            };

            let Some(inner_fn) = self.find_overload(candidates, &probe) else {
                return Ok(None);
            };

            (inner_fn, pairs)
        };

        let mut results = Vec::with_capacity(pairs.len());
        for (l, r) in pairs {
            let v = self
                .call_callback(inner_fn.clone(), vec![l, r])
                .map_err(|mut e| {
                    e.span.get_or_insert(span);
                    e
                })?;
            results.push(v);
        }

        // Replace callee + args on the stack with the result tuple.
        self.stack.truncate(callee_idx);
        Ok(Some(Value::Object(Rc::new(Object::Tuple(results)))))
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

/// If `value` is a tuple whose elements are all numeric, returns a reference to
/// its element vec.  Returns `None` for empty tuples, non-tuples, or tuples
/// that contain non-numeric elements.
fn as_numeric_tuple(value: &Value) -> Option<&Vec<Value>> {
    let Value::Object(obj) = value else {
        return None;
    };
    let Object::Tuple(elems) = obj.as_ref() else {
        return None;
    };
    if !elems.is_empty() && elems.iter().all(|e| e.is_number()) {
        Some(elems)
    } else {
        None
    }
}

impl CallFrame {
    #[inline(always)]
    fn slot(&self, local: usize) -> usize {
        self.frame_pointer + local
    }
}

/// A callable VM function bound to the parent VM, for use in VmNative HOF
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
