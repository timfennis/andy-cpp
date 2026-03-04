use crate::chunk::OpCode;
use crate::value::{CompiledFunction, Function};
use crate::{Object, Value};
use ndc_parser::ResolvedVar;
use std::rc::Rc;

pub struct Vm {
    stack: Vec<Value>,
    globals: Vec<Value>,
    frames: Vec<CallFrame>,
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
            stack: vec![Value::Object(Box::new(Object::Function(
                Function::Compiled(Rc::clone(&function)),
            )))],
            globals,
            frames: vec![CallFrame {
                function,
                ip: 0,
                frame_pointer: 0,
            }],
        }
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        eprintln!("[VM] Value bytes: {}", size_of::<Value>());
        eprintln!("[VM] OpCode bytes: {}", size_of::<OpCode>());

        if self.frames.is_empty() {
            panic!("no call frames")
        }

        loop {
            let frame = self.frames.last_mut().expect("must not be empty");

            let op = frame.opcode();
            frame.ip += 1;

            eprintln!("[VM] Running: {:?}", op);

            match op {
                OpCode::Halt => {
                    let ret = self.stack.pop().expect("stack underflow");
                    self.stack.truncate(frame.frame_pointer - 1);
                    self.frames.pop().expect("no frame to pop");
                    self.stack.push(ret)
                }
                OpCode::Return => {
                    println!("{:?}", self.stack.pop().expect("stack underflow"));
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
                    match callee {
                        Value::Object(obj) => match obj.as_ref() {
                            Object::Function(Function::Compiled(f)) => {
                                let f = Rc::clone(f);
                                self.frames.push(CallFrame {
                                    function: f,
                                    ip: 0,
                                    frame_pointer: self.stack.len() - args,
                                });
                            }
                            Object::Function(Function::Native(native)) => {
                                let native = Rc::clone(native);
                                let args_slice = self.stack[self.stack.len() - args..].to_vec();
                                let result = (native.func)(&args_slice);
                                self.stack.truncate(self.stack.len() - args - 1);
                                self.stack.push(result);
                            }
                            Object::OverloadSet(candidates) => {
                                let candidates = candidates.clone();
                                let fp = frame.frame_pointer;
                                let arg_types: Vec<_> = self.stack[self.stack.len() - args..]
                                    .iter()
                                    .map(Value::static_type)
                                    .collect();

                                let resolved = candidates.iter().find_map(|var| {
                                    let value = self.resolve_var(var, fp);
                                    let Value::Object(obj) = &value else {
                                        return None;
                                    };
                                    let Object::Function(func) = obj.as_ref() else {
                                        return None;
                                    };
                                    if func.static_type().is_fn_and_matches(&arg_types) {
                                        Some(value.clone())
                                    } else {
                                        None
                                    }
                                });

                                match resolved {
                                    Some(Value::Object(obj)) => match obj.as_ref() {
                                        Object::Function(Function::Compiled(f)) => {
                                            let f = Rc::clone(f);
                                            self.frames.push(CallFrame {
                                                function: f,
                                                ip: 0,
                                                frame_pointer: self.stack.len() - args,
                                            });
                                        }
                                        Object::Function(Function::Native(native)) => {
                                            let native = Rc::clone(native);
                                            let args_slice =
                                                self.stack[self.stack.len() - args..].to_vec();
                                            let result = (native.func)(&args_slice);
                                            self.stack.truncate(self.stack.len() - args - 1);
                                            self.stack.push(result);
                                        }
                                        _ => unreachable!(),
                                    },
                                    _ => panic!(
                                        "no matching overload found for argument types {:?}",
                                        arg_types
                                    ),
                                }
                            }
                            _ => panic!("callee is unexpected object type"),
                        },
                        _ => panic!("callee is unexpected value type"),
                    }
                }
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
