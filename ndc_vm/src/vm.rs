use crate::chunk::{CaptureFrom, OpCode};
use crate::value::{CompiledFunction, Function};
use crate::{ClosureFunction, Object, UpvalueCell, Value};
use ndc_parser::ResolvedVar;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

pub struct Vm {
    stack: Vec<Value>,
    globals: Vec<Value>,
    frames: Vec<CallFrame>,
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
                    self.stack.truncate(frame.frame_pointer - 1);
                    self.frames.pop().expect("no frame to pop");
                    self.stack.push(ret)
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
                        frame.ip = frame.ip.wrapping_add(offset);
                    }
                }
                OpCode::JumpIfTrue(offset) => {
                    let top = self.stack.last().expect("stack underflow");
                    if let Value::Bool(true) = top {
                        frame.ip = frame.ip.wrapping_add(offset);
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
                OpCode::GetUpvalue { depth, slot } => {
                    debug_assert_eq!(
                        depth, 0,
                        "compiler must have captured the closed over variables"
                    );
                    match frame.closure.upvalues[slot].borrow().deref() {
                        UpvalueCell::Open(slot) => self.stack.push(self.stack[*slot].clone()),
                        UpvalueCell::Closed(value) => self.stack.push(value.clone()),
                    }
                }
                OpCode::SetUpvalue { depth, slot } => {
                    debug_assert_eq!(
                        depth, 0,
                        "compiler must have captured the closed over variables"
                    );
                    let value = self.stack.last().expect("stack underflow").clone();
                    let mut cell = frame.closure.upvalues[slot].borrow_mut();
                    match &mut *cell {
                        UpvalueCell::Open(stack_slot) => self.stack[*stack_slot] = value,
                        UpvalueCell::Closed(stored) => *stored = value,
                    }
                }
                OpCode::Closure {
                    constant_idx: idx,
                    values,
                } => {
                    let Value::Object(obj) = frame.closure.prototype.body.constant(idx) else {
                        panic!("invalid type");
                    };
                    let Object::Function(Function::Compiled(compiled)) = &**obj else {
                        panic!("invalid type 2");
                    };
                    let closure = Value::function(Function::Closure(ClosureFunction {
                        prototype: Rc::clone(compiled),
                        upvalues: values
                            .iter()
                            .map(|c| match c {
                                CaptureFrom::Local(slot) => Rc::new(RefCell::new(
                                    UpvalueCell::Open(frame.frame_pointer + slot),
                                )),
                                CaptureFrom::Upvalue(slot) => {
                                    Rc::clone(&frame.closure.upvalues[*slot])
                                }
                            })
                            .collect(),
                    }));

                    self.stack.push(closure);
                }
            }

            #[cfg(feature = "vm-trace")]
            {
                dbg!(&self.stack);
                // eprintln!("[VM] stack: {:?}", self.stack);
            }
        }
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
