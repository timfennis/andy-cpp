use crate::chunk::OpCode;
use crate::iterator::{ListIter, RangeInclusiveIter, RangeIter, StringIter, TupleIter};
use crate::value::{CompiledFunction, Function};
use crate::{ClosureFunction, Object, UpvalueCell, Value};
use ndc_core::hash_map::HashMap;
use ndc_parser::{CaptureSource, ResolvedVar};
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

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
    #[cfg(feature = "vm-trace")]
    source: Option<String>,
}

#[derive(thiserror::Error, Debug)]
pub enum VmError {
    #[error("runtime error")]
    RuntimeError,
}

pub struct CallFrame {
    closure: ClosureFunction,
    ip: usize,
    // offset into the locals array for this call frame
    frame_pointer: usize,
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
            }],
            open_upvalues: Vec::new(),
            #[cfg(feature = "vm-trace")]
            source: None,
        };
        for _ in 0..num_locals {
            vm.stack.push(Value::unit());
        }
        vm
    }

    #[cfg(feature = "vm-trace")]
    pub fn with_source(mut self, source: impl Into<String>) -> Self {
        self.source = Some(source.into());
        self
    }

    pub fn run(&mut self) -> Result<(), VmError> {
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

            frame.ip += 1;

            match op {
                OpCode::Halt => {
                    return Ok(());
                }
                OpCode::Return => {
                    let ret = self.stack.pop().expect("stack underflow");
                    let frame_pointer = self.frames.last().expect("no frame").frame_pointer;
                    self.close_upvalues(frame_pointer);
                    self.stack.truncate(frame_pointer - 1);
                    self.frames.pop().expect("no frame to pop");
                    self.stack.push(ret);
                    if self.frames.is_empty() {
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
                    if let Value::Bool(false) = top {
                        frame.ip = frame.ip.wrapping_add_signed(offset);
                    }
                }
                OpCode::JumpIfTrue(offset) => {
                    let top = self.stack.last().expect("stack underflow");
                    if let Value::Bool(true) = top {
                        frame.ip = frame.ip.wrapping_add_signed(offset);
                    }
                }
                OpCode::Jump(offset) => {
                    frame.ip = frame.ip.wrapping_add_signed(offset);
                }
                OpCode::Pop => {
                    self.stack.pop();
                }
                OpCode::Call(args) => {
                    let callee = &self.stack[self.stack.len() - args - 1];
                    let func =
                        match callee {
                            Value::Object(obj) => match obj.as_ref() {
                                Object::Function(f) => f.clone(),
                                Object::OverloadSet(candidates) => {
                                    let candidates = candidates.clone();
                                    let fp = frame.frame_pointer;
                                    let arg_types: Vec<_> = self.stack[self.stack.len() - args..]
                                        .iter()
                                        .map(Value::static_type)
                                        .collect();
                                    candidates
                                    .iter()
                                    .find_map(|var| {
                                        let value = self.resolve_var(var, fp);
                                        let Value::Object(obj) = value else { return None };
                                        let Object::Function(f) = obj.as_ref() else {
                                            return None;
                                        };
                                        f.static_type()
                                            .is_fn_and_matches(&arg_types)
                                            .then(|| f.clone())
                                    })
                                    .unwrap_or_else(|| {
                                        panic!(
                                            "no matching overload found for argument types {:?}",
                                            arg_types
                                        )
                                    })
                                }
                                _ => panic!("callee is unexpected object type"),
                            },
                            _ => panic!("callee is unexpected value type"),
                        };
                    self.dispatch_call(func, args);
                }
                OpCode::MakeList(size) => {
                    let data = self.stack.split_off(self.stack.len() - size);
                    self.stack.push(Value::Object(Box::new(Object::list(data))));
                }
                OpCode::MakeTuple(size) => {
                    let data = self.stack.split_off(self.stack.len() - size);
                    self.stack
                        .push(Value::Object(Box::new(Object::Tuple(data))));
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
                    self.stack.push(Value::Object(Box::new(Object::map(map, default))));
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
                    let iter_val = match val {
                        Value::Object(ref obj) if matches!(**obj, Object::Iterator(_)) => val,
                        Value::Object(obj) => match *obj {
                            Object::List(rc) => {
                                Value::iterator(Rc::new(RefCell::new(ListIter::new(rc))))
                            }
                            Object::Tuple(vec) => {
                                Value::iterator(Rc::new(RefCell::new(TupleIter::new(vec))))
                            }
                            Object::String(rc) => {
                                Value::iterator(Rc::new(RefCell::new(StringIter::new(rc))))
                            }
                            other => panic!("value is not iterable: {other}"),
                        },
                        other => panic!("value is not iterable: {:?}", other),
                    };
                    self.stack.push(iter_val);
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
                OpCode::MakeRange => {
                    let end = self.stack.pop().expect("stack underflow");
                    let start = self.stack.pop().expect("stack underflow");
                    let (Value::Int(start), Value::Int(end)) = (start, end) else {
                        panic!("range bounds must be integers")
                    };
                    self.stack
                        .push(Value::iterator(Rc::new(RefCell::new(RangeIter::new(
                            start, end,
                        )))));
                }
                OpCode::MakeRangeInclusive => {
                    let end = self.stack.pop().expect("stack underflow");
                    let start = self.stack.pop().expect("stack underflow");
                    let (Value::Int(start), Value::Int(end)) = (start, end) else {
                        panic!("range bounds must be integers")
                    };
                    self.stack.push(Value::iterator(Rc::new(RefCell::new(
                        RangeInclusiveIter::new(start, end),
                    ))));
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
                    let top = self.stack.pop().expect("stack underflow");
                    let Value::Object(obj) = top else {
                        panic!("expected a tuple or list to unpack");
                    };

                    match *obj {
                        Object::List(seq) => {
                            let mut seq = std::mem::take(&mut *seq.borrow_mut());
                            assert_eq!(seq.len(), size, "unpack length mismatch");
                            seq.reverse();
                            self.stack.append(&mut seq);
                        }
                        Object::Tuple(mut seq) => {
                            assert_eq!(seq.len(), size, "unpack length mismatch");
                            seq.reverse();
                            self.stack.append(&mut seq);
                        }
                        _ => panic!("expected a tuple or list to unpack"),
                    }
                }
            }

            #[cfg(feature = "vm-trace")]
            {
                dbg!(&self.stack);
                // eprintln!("[VM] stack: {:?}", self.stack);
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
        self.open_upvalues
            .retain(|c| matches!(*c.borrow(), UpvalueCell::Open(_)));
    }

    fn dispatch_call(&mut self, func: Function, args: usize) {
        match func {
            Function::Closure(c) => {
                let num_locals = c.prototype.num_locals;
                self.frames.push(CallFrame {
                    closure: c,
                    ip: 0,
                    frame_pointer: self.stack.len() - args,
                });
                for _ in args..num_locals {
                    self.stack.push(Value::unit());
                }
            }
            Function::Compiled(f) => {
                let num_locals = f.num_locals;
                self.frames.push(CallFrame {
                    closure: ClosureFunction {
                        prototype: f,
                        upvalues: vec![],
                    },
                    ip: 0,
                    frame_pointer: self.stack.len() - args,
                });
                for _ in args..num_locals {
                    self.stack.push(Value::unit());
                }
            }
            Function::Native(native) => {
                let start = self.stack.len() - args;
                let result = (native.func)(&self.stack[start..]);
                self.stack.truncate(start - 1);
                self.stack.push(result);
            }
        }
    }

    /// Call a VM function with the given arguments, using a fresh VM instance.
    /// Used to enable callbacks from stdlib HOFs into user-defined VM closures.
    pub fn call_function(func: Function, args: Vec<Value>, globals: Vec<Value>) -> Value {
        match func {
            Function::Native(native) => (native.func)(&args),
            func => {
                let n_args = args.len();
                let mut vm = Vm {
                    stack: vec![Value::unit()], // dummy callee slot so frame_pointer = 1
                    globals,
                    frames: Vec::new(),
                    open_upvalues: Vec::new(),
                    #[cfg(feature = "vm-trace")]
                    source: None,
                };
                for arg in args {
                    vm.stack.push(arg);
                }
                vm.dispatch_call(func, n_args);
                vm.run().expect("callback execution failed");
                vm.stack.pop().expect("callback must produce a value")
            }
        }
    }

    fn resolve_var(&self, var: &ResolvedVar, frame_pointer: usize) -> &Value {
        match var {
            ResolvedVar::Global { slot } => &self.globals[*slot],
            ResolvedVar::Local { slot } => &self.stack[frame_pointer + slot],
            ResolvedVar::Upvalue { .. } => todo!("upvalue resolution in overload sets"),
        }
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
