use crate::chunk::OpCode;
use crate::value::{CompiledFunction, Function};
use crate::{Object, Value};
use ndc_parser::ResolvedVar;
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
    function: Rc<CompiledFunction>,
    ip: usize,
    // offset into the locals array for this call frame
    frame_pointer: usize,
}

impl Vm {
    pub fn new(function: CompiledFunction, globals: Vec<Value>) -> Self {
        let function = Rc::new(function);
        Self {
            stack: Vec::with_capacity(256),
            globals,
            frames: vec![CallFrame {
                function,
                ip: 0,
                frame_pointer: 0,
            }],
            #[cfg(feature = "vm-trace")]
            source: None,
        }
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
                let span = frame.function.body.span(ip);
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
                    self.stack.push(frame.function.body.constant(idx).clone());
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
                    self.stack.push(Value::Object(Box::new(Object::List(data))));
                }
                OpCode::MakeTuple(size) => {
                    let data = self.stack.split_off(self.stack.len() - size);
                    self.stack
                        .push(Value::Object(Box::new(Object::Tuple(data))));
                }
            }
        }
    }

    fn dispatch_call(&mut self, func: Function, args: usize) {
        match func {
            Function::Compiled(f) => {
                self.frames.push(CallFrame {
                    function: f,
                    ip: 0,
                    frame_pointer: self.stack.len() - args,
                });
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
        self.function.body.opcode(self.ip)
    }

    #[inline(always)]
    fn slot(&self, slot: usize) -> usize {
        self.frame_pointer + slot
    }
}
