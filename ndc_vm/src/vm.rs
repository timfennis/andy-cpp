use crate::chunk::OpCode;
use crate::error::VmError;
use crate::iterator::{
    MapIter, MaxHeapIter, MinHeapIter, RangeInclusiveIter, RangeIter, SeqIter, StringIter,
    UnboundedRangeIter, VmIterator,
};
use crate::value::{CompiledFunction, Function, NativeFunc};
use crate::{ClosureFunction, Object, UpvalueCell, Value};
use ndc_core::StaticType;
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

            let op = frame.opcode();

            #[cfg(feature = "vm-trace")]
            {
                let ip = frame.ip;
                let span = frame.closure.prototype.body.span(ip);
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

            let span = frame.closure.prototype.body.span(frame.ip);
            frame.ip += 1;

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
                    self.stack.truncate(frame_pointer - 1);
                    self.frames.pop().expect("no frame to pop");
                    self.stack.push(ret);
                    if self.frames.len() == target_depth {
                        return Ok(());
                    }
                }
                OpCode::Constant(idx) => {
                    self.stack
                        .push(frame.closure.prototype.body.constant(idx).clone());
                }
                OpCode::GetLocal(slot) => {
                    self.stack.push(self.stack[frame.slot(slot)].clone());
                }
                OpCode::GetGlobal(slot) => {
                    self.stack.push(self.globals[slot].clone());
                }
                OpCode::SetLocal(slot) => {
                    let value = self.stack.pop().expect("stack underflow");
                    if frame.slot(slot) < self.stack.len() {
                        self.stack[frame.slot(slot)] = value;
                    } else {
                        self.stack.push(value);
                    }
                }
                OpCode::JumpIfFalse(offset) => {
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
                    frame.ip = frame.ip.wrapping_add_signed(offset);
                }
                OpCode::Pop => {
                    self.stack.pop();
                }
                OpCode::Call(args) => {
                    if let Some(func) = self
                        .resolve_callee(args)
                        .map_err(|msg| VmError::new(msg, span))?
                    {
                        // Native functions bridge to the tree-walk interpreter, which may call
                        // closures back via `Vm::call_function` in a fresh VM context. In that
                        // context, Open upvalue cells (stack-slot references) are invalid.
                        // Materialize them now while the current stack is still live.
                        if func.needs_arg_materialization() {
                            self.materialize_upvalues_in_args(args);
                        }
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
                    let data = self.stack.split_off(self.stack.len() - size);
                    self.stack.push(Value::Object(Rc::new(Object::list(data))));
                }
                OpCode::MakeTuple(size) => {
                    let data = self.stack.split_off(self.stack.len() - size);
                    self.stack.push(Value::Object(Rc::new(Object::Tuple(data))));
                }
                OpCode::MakeMap { pairs, has_default } => {
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
                OpCode::GetUpvalue(slot) => match frame.closure.upvalues[slot].borrow().deref() {
                    UpvalueCell::Open(slot) => self.stack.push(self.stack[*slot].clone()),
                    UpvalueCell::Closed(value) => self.stack.push(value.clone()),
                },
                OpCode::SetUpvalue(slot) => {
                    let value = self.stack.last().expect("stack underflow").clone();
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
                            Object::Map { entries, .. } => Some(Value::iterator(Rc::new(
                                RefCell::new(MapIter::new(entries)),
                            ))),
                            Object::MinHeap(h) => {
                                Some(Value::iterator(Rc::new(RefCell::new(MinHeapIter::new(h)))))
                            }
                            Object::MaxHeap(h) => {
                                Some(Value::iterator(Rc::new(RefCell::new(MaxHeapIter::new(h)))))
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
                    let frame = self.frames.last().expect("no frame");
                    let Value::Object(obj) = frame.closure.prototype.body.constant(idx) else {
                        panic!("invalid type");
                    };
                    let Object::Function(Function::Compiled(compiled)) = &**obj else {
                        panic!("invalid type 2");
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
                    self.exec_unpack(size, span)?;
                }
                OpCode::CloseUpvalue(slot) => {
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
        for cell in &self.open_upvalues {
            let mut borrow = cell.borrow_mut();
            if let UpvalueCell::Open(slot) = *borrow
                && slot >= frame_pointer
            {
                *borrow = UpvalueCell::Closed(self.stack[slot].clone());
            }
        }
        // Remove the cells we just closed; outer-frame Open cells stay.
        self.open_upvalues
            .retain(|c| matches!(*c.borrow(), UpvalueCell::Open(_)));
    }

    /// Materializes (closes) any Open upvalues in the immediate arguments.
    /// This is necessary when closures are about to be passed to functions
    /// that may call them in a different VM context (e.g., stdlib HOFs
    /// that bridge back to the tree-walk interpreter). We only materialize
    /// direct function arguments, not nested closures.
    fn materialize_upvalues_in_args(&mut self, arg_count: usize) {
        let start = self.stack.len() - arg_count;

        // Capture the stack state before any mutations
        let current_stack_len = self.stack.len();
        let mut materialized = Vec::new();

        // Identify closures and their open upvalues that need materializing
        for i in start..current_stack_len {
            if let Value::Object(obj) = &self.stack[i] {
                if let Object::Function(Function::Closure(closure)) = &**obj {
                    for (j, cell) in closure.upvalues.iter().enumerate() {
                        if let UpvalueCell::Open(slot) = *cell.borrow() {
                            materialized.push((i, j, slot));
                        }
                    }
                }
            }
        }

        // Now materialize them
        for (arg_idx, upvalue_idx, slot) in materialized {
            if slot < self.stack.len() {
                let value = self.stack[slot].clone();
                if let Value::Object(obj) = &self.stack[arg_idx] {
                    if let Object::Function(Function::Closure(closure)) = obj.as_ref() {
                        let mut cell_borrow = closure.upvalues[upvalue_idx].borrow_mut();
                        *cell_borrow = UpvalueCell::Closed(value);
                    }
                }
            }
        }

        // Remove cells we just closed from open_upvalues; they're no longer open.
        self.open_upvalues
            .retain(|c| matches!(*c.borrow(), UpvalueCell::Open(_)));
    }

    fn dispatch_call(&mut self, func: Function, args: usize) -> Result<(), VmError> {
        // Memoized functions check the cache first.  On a hit we short-circuit
        // without pushing a new frame.  On a miss we dispatch the inner
        // function normally and tag the new frame so `Return` will populate
        // the cache once the call finishes.
        let memo = if let Function::Memoized { cache, function } = func {
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
            return self.dispatch_call_with_memo(*function, args, Some((cache, key)));
        } else {
            None
        };

        self.dispatch_call_with_memo(func, args, memo)
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
                    let arg_types: Vec<_> = self.stack[self.stack.len() - args..]
                        .iter()
                        .map(Value::static_type)
                        .collect();
                    Ok(self.find_overload(candidates, &arg_types))
                }
                obj => panic!(
                    "callee is unexpected object type: {:?}",
                    std::mem::discriminant(obj)
                ),
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
    fn find_overload(
        &self,
        candidates: &[ResolvedVar],
        arg_types: &[StaticType],
    ) -> Option<Function> {
        let frame_pointer = self.frames.last().expect("no frame").frame_pointer;
        candidates.iter().find_map(|var| {
            let value = self.resolve_var(var, frame_pointer);
            let Value::Object(obj) = value else {
                return None;
            };
            let Object::Function(f) = obj.as_ref() else {
                return None;
            };
            f.matches_arg_types(arg_types).then(|| f.clone())
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
        let candidates = match &self.stack[callee_idx] {
            Value::Object(obj) => match obj.as_ref() {
                Object::OverloadSet(candidates) => candidates.clone(),
                _ => return Ok(None),
            },
            _ => return Ok(None),
        };

        let left = self.stack[self.stack.len() - 2].clone();
        let right = self.stack[self.stack.len() - 1].clone();

        let Some(pairs) = vectorization_pairs(&left, &right) else {
            return Ok(None);
        };

        // Use the first pair's element types to find a matching inner function.
        let first_elem_types = [pairs[0].0.static_type(), pairs[0].1.static_type()];
        let Some(inner_fn) = self.find_overload(&candidates, &first_elem_types) else {
            return Ok(None);
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
            panic!("expected a tuple or list to unpack");
        };
        match obj.as_ref() {
            Object::List(seq) => {
                let len = seq.borrow().len();
                if len != size {
                    return Err(VmError::new(
                        format!(
                            "cannot unpack a list of length {} into {} variables",
                            len, size
                        ),
                        span,
                    ));
                }
                // Try to avoid a clone when we're the sole owner.
                let mut elements = match Rc::try_unwrap(obj) {
                    Ok(Object::List(seq)) => seq.into_inner(),
                    Ok(_) => unreachable!(),
                    Err(rc) => {
                        let Object::List(seq) = rc.as_ref() else {
                            unreachable!()
                        };
                        seq.borrow().to_vec()
                    }
                };
                elements.reverse();
                self.stack.append(&mut elements);
            }
            Object::Tuple(seq) => {
                if seq.len() != size {
                    return Err(VmError::new(
                        format!(
                            "cannot unpack a tuple of length {} into {} variables",
                            seq.len(),
                            size
                        ),
                        span,
                    ));
                }
                // Try to avoid a clone when we're the sole owner.
                let mut elements = match Rc::try_unwrap(obj) {
                    Ok(Object::Tuple(seq)) => seq,
                    Ok(_) => unreachable!(),
                    Err(rc) => {
                        let Object::Tuple(seq) = rc.as_ref() else {
                            unreachable!()
                        };
                        seq.clone()
                    }
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
            _ => panic!("expected a tuple or list to unpack"),
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
    if !elems.is_empty() && elems.iter().all(|e| e.static_type().is_number()) {
        Some(elems)
    } else {
        None
    }
}

/// Returns the element pairs to apply a binary op to when vectorizing over
/// numeric tuples, or `None` when the arguments do not support vectorization.
///
/// Three shapes are supported:
///   - `(tuple, tuple)` — same-length numeric tuples: paired element-wise
///   - `(scalar, tuple)` — scalar broadcast over each tuple element
///   - `(tuple, scalar)` — tuple element paired with scalar
fn vectorization_pairs(left: &Value, right: &Value) -> Option<Vec<(Value, Value)>> {
    let left_tuple = as_numeric_tuple(left);
    let right_tuple = as_numeric_tuple(right);
    let left_is_scalar = left.static_type().is_number();
    let right_is_scalar = right.static_type().is_number();

    match (left_tuple, right_tuple) {
        (Some(ls), Some(rs)) if ls.len() == rs.len() => {
            Some(ls.iter().cloned().zip(rs.iter().cloned()).collect())
        }
        (None, Some(rs)) if left_is_scalar => {
            Some(rs.iter().map(|r| (left.clone(), r.clone())).collect())
        }
        (Some(ls), None) if right_is_scalar => {
            Some(ls.iter().map(|l| (l.clone(), right.clone())).collect())
        }
        _ => None,
    }
}

impl CallFrame {
    #[inline(always)]
    fn opcode(&mut self) -> OpCode {
        self.closure.prototype.body.opcode(self.ip)
    }

    #[inline(always)]
    fn slot(&self, slot: usize) -> usize {
        self.frame_pointer + slot
    }
}

/// A callable VM function bound to the parent VM, for use in VmNative HOF
/// implementations. Produced by the `&VmCallable` input-type handler in
/// `ndc_macros::vm_convert`.
///
/// Calling `call()` runs the function inline on the parent VM via
/// `Vm::call_callback` — no child VM is allocated. `RefCell` gives interior
/// mutability so `call(&self)` works without changing any HOF signatures.
pub struct VmCallable<'a> {
    pub function: Function,
    pub vm: RefCell<&'a mut Vm>,
}

impl VmCallable<'_> {
    /// Call this function with the given arguments, running inline on the
    /// parent VM.
    pub fn call(&self, args: Vec<Value>) -> Result<Value, VmError> {
        self.vm
            .borrow_mut()
            .call_callback(self.function.clone(), args)
    }
}
