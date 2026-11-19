use super::instr::{Instr, Reg, WINDOW};
use crate::error::VmError;
use crate::io::{InputSource, OutputSink};
use crate::native_op::NativeOp;
use crate::value::{CompiledFunction, Function, NativeFunc, NativeFunction};
use crate::vm::{MAX_CALL_DEPTH, calls_too_deep, fill_natives, not_a_bool};
use crate::{Natives, Object, Value};
use std::rc::Rc;

/// The register VM. Its stack is the registers of every running call, one
/// frame above the other: a frame is `stack[base..base + frame_size]`, and
/// nothing is pushed or popped while code runs. A call's frame starts at the
/// register after the callee, so the caller's argument registers are the
/// callee's parameters.
pub struct Vm {
    /// Every frame's registers, with [`WINDOW`] values past the top frame's
    /// first register, so a frame's registers can be indexed by a [`Reg`]
    /// without bounds checks. It only grows.
    stack: Vec<Value>,
    globals: Vec<Value>,
    frames: Vec<Frame>,
    /// The stack VM the natives that take a VM are given, which holds the
    /// output `print` writes to and the input `read_stdin` reads.
    host: crate::Vm,
    /// What the program's last expression gave, once it halts.
    result: Value,
}

/// A running call.
struct Frame {
    function: Rc<CompiledFunction>,
    /// The next instruction to run, kept here only while another frame runs
    /// or the inner loop is left.
    ip: usize,
    /// The index in the stack of the frame's first register.
    base: usize,
}

/// Why the inner loop of [`Vm::run`] stopped: something it doesn't do itself
/// because it changes frames or needs the whole VM.
enum Exit {
    Call {
        base: Reg,
        args: u16,
    },
    /// A native that takes the VM.
    CallNative {
        dst: Reg,
        base: Reg,
        slot: u16,
        args: u8,
    },
    Return {
        src: Reg,
    },
    Halt {
        src: Reg,
    },
    Error(VmError),
}

impl Vm {
    /// A VM that runs `function`, compiled by the register compiler, at the
    /// top level with `globals`, as [`crate::Vm::new`] does.
    pub fn new(function: CompiledFunction, mut globals: Vec<Value>, natives: &Natives) -> Self {
        assert!(
            function.is_register_code(),
            "internal error: the register VM runs register code"
        );
        if globals.len() < function.num_globals {
            globals.resize(function.num_globals, Value::unit());
        }
        fill_natives(&mut globals, natives, &function);
        Self {
            stack: vec![Value::None; WINDOW],
            globals,
            frames: vec![Frame {
                function: Rc::new(function),
                ip: 0,
                base: 0,
            }],
            host: crate::Vm::host(),
            result: Value::unit(),
        }
    }

    /// Replace the output sink used by I/O built-ins (e.g. `print`, `dbg`).
    #[must_use]
    pub fn with_output(mut self, output: OutputSink) -> Self {
        self.host = self.host.with_output(output);
        self
    }

    /// Returns the captured output bytes, or `None` if this VM writes to stdout.
    pub fn get_output(&self) -> Option<&[u8]> {
        self.host.get_output()
    }

    /// Replace the input source used by `read_stdin`.
    #[must_use]
    pub fn with_input(mut self, input: InputSource) -> Self {
        self.host = self.host.with_input(input);
        self
    }

    /// The input source, with what is left of it to read.
    pub fn take_input(&mut self) -> InputSource {
        self.host.take_input()
    }

    /// What the program's last expression gave, unit until it halts.
    pub fn last_value(&self) -> Value {
        self.result.clone()
    }

    /// The globals as the program left them, and the stack.
    pub fn into_globals_and_stack(self) -> (Vec<Value>, Vec<Value>) {
        (self.globals, self.stack)
    }

    /// Runs the program until it halts or fails.
    ///
    /// Two loops: the inner one runs the instructions of one frame with what
    /// it needs in locals, the frame's code, its instruction pointer and its
    /// registers, and leaves for anything that enters or leaves a frame; the
    /// outer one does that and enters the inner loop again for the frame on
    /// top.
    pub fn run(&mut self) -> Result<(), VmError> {
        loop {
            let frame = self
                .frames
                .last()
                .expect("internal error: a running VM has a frame");
            let chunk = &frame.function.registers;
            let code = chunk.code();
            let constants = chunk.constants();
            let base = frame.base;
            let mut ip = frame.ip;
            let regs: &mut [Value; WINDOW] = (&mut self.stack[base..base + WINDOW])
                .try_into()
                .expect("internal error: the stack reaches a window past every frame");
            let globals = &mut self.globals;

            let exit = loop {
                let instr = code[ip];
                ip += 1;
                match instr {
                    Instr::LoadK { dst, k } => regs[dst.index()] = constants[k as usize].clone(),
                    Instr::LoadUnit { dst } => regs[dst.index()] = Value::unit(),
                    Instr::MakeString { dst, k } => {
                        regs[dst.index()] = new_string(&constants[k as usize]);
                    }
                    Instr::Copy { dst, src } => regs[dst.index()] = regs[src.index()].clone(),
                    Instr::Move { dst, src } => {
                        regs[dst.index()] = std::mem::replace(&mut regs[src.index()], Value::None);
                    }
                    Instr::GetGlobal { dst, slot } => {
                        regs[dst.index()] = globals[slot as usize].clone();
                    }
                    Instr::SetGlobal { slot, src } => {
                        globals[slot as usize] =
                            std::mem::replace(&mut regs[src.index()], Value::None);
                    }
                    Instr::AddInt { dst, left, right } => {
                        let value = match (&regs[left.index()], &regs[right.index()]) {
                            (Value::Int(l), Value::Int(r)) => l.checked_add(*r).map(Value::Int),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                break Exit::Error(binary_error(
                                    regs,
                                    NativeOp::AddInt,
                                    left,
                                    right,
                                ));
                            }
                        }
                    }
                    Instr::SubInt { dst, left, right } => {
                        let value = match (&regs[left.index()], &regs[right.index()]) {
                            (Value::Int(l), Value::Int(r)) => l.checked_sub(*r).map(Value::Int),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                break Exit::Error(binary_error(
                                    regs,
                                    NativeOp::SubInt,
                                    left,
                                    right,
                                ));
                            }
                        }
                    }
                    Instr::MulInt { dst, left, right } => {
                        let value = match (&regs[left.index()], &regs[right.index()]) {
                            (Value::Int(l), Value::Int(r)) => l.checked_mul(*r).map(Value::Int),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                break Exit::Error(binary_error(
                                    regs,
                                    NativeOp::MulInt,
                                    left,
                                    right,
                                ));
                            }
                        }
                    }
                    Instr::AddFloat { dst, left, right } => {
                        if let Err(error) = binary(regs, NativeOp::AddFloat, dst, left, right) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::SubFloat { dst, left, right } => {
                        if let Err(error) = binary(regs, NativeOp::SubFloat, dst, left, right) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::MulFloat { dst, left, right } => {
                        if let Err(error) = binary(regs, NativeOp::MulFloat, dst, left, right) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::DivFloat { dst, left, right } => {
                        if let Err(error) = binary(regs, NativeOp::DivFloat, dst, left, right) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::Less { dst, left, right } => {
                        let value = match (&regs[left.index()], &regs[right.index()]) {
                            (Value::Int(l), Value::Int(r)) => Some(Value::Bool(l < r)),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                if let Err(error) = binary(regs, NativeOp::Less, dst, left, right) {
                                    break Exit::Error(error);
                                }
                            }
                        }
                    }
                    Instr::LessEqual { dst, left, right } => {
                        let value = match (&regs[left.index()], &regs[right.index()]) {
                            (Value::Int(l), Value::Int(r)) => Some(Value::Bool(l <= r)),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                if let Err(error) =
                                    binary(regs, NativeOp::LessEqual, dst, left, right)
                                {
                                    break Exit::Error(error);
                                }
                            }
                        }
                    }
                    Instr::Greater { dst, left, right } => {
                        let value = match (&regs[left.index()], &regs[right.index()]) {
                            (Value::Int(l), Value::Int(r)) => Some(Value::Bool(l > r)),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                if let Err(error) =
                                    binary(regs, NativeOp::Greater, dst, left, right)
                                {
                                    break Exit::Error(error);
                                }
                            }
                        }
                    }
                    Instr::GreaterEqual { dst, left, right } => {
                        let value = match (&regs[left.index()], &regs[right.index()]) {
                            (Value::Int(l), Value::Int(r)) => Some(Value::Bool(l >= r)),
                            _ => None,
                        };
                        match value {
                            Some(value) => regs[dst.index()] = value,
                            None => {
                                if let Err(error) =
                                    binary(regs, NativeOp::GreaterEqual, dst, left, right)
                                {
                                    break Exit::Error(error);
                                }
                            }
                        }
                    }
                    Instr::Equal { dst, left, right } => {
                        if let Err(error) = binary(regs, NativeOp::Equal, dst, left, right) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::NotEqual { dst, left, right } => {
                        if let Err(error) = binary(regs, NativeOp::NotEqual, dst, left, right) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::NegInt { dst, src } => {
                        if let Err(error) = unary(regs, NativeOp::NegInt, dst, src) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::NegFloat { dst, src } => {
                        if let Err(error) = unary(regs, NativeOp::NegFloat, dst, src) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::Not { dst, src } => {
                        if let Err(error) = unary(regs, NativeOp::Not, dst, src) {
                            break Exit::Error(error);
                        }
                    }
                    Instr::Jump { offset } => ip = ip.wrapping_add_signed(offset as isize),
                    Instr::JumpIfFalse { cond, offset } => match &regs[cond.index()] {
                        Value::Bool(true) => {}
                        Value::Bool(false) => ip = ip.wrapping_add_signed(offset as isize),
                        value => break Exit::Error(VmError::native(not_a_bool(value))),
                    },
                    Instr::Call { base, args } => break Exit::Call { base, args },
                    Instr::CallNative {
                        dst,
                        base,
                        slot,
                        args,
                    } => {
                        let native = global_native(globals, usize::from(slot));
                        let NativeFunc::Simple(function) = &native.func else {
                            break Exit::CallNative {
                                dst,
                                base,
                                slot,
                                args,
                            };
                        };
                        let arguments = &mut regs[base.index()..base.index() + usize::from(args)];
                        let result = function(arguments);
                        arguments.fill(Value::None);
                        match result {
                            Ok(value) => regs[dst.index()] = value,
                            Err(error) => break Exit::Error(error),
                        }
                    }
                    Instr::Return { src } => break Exit::Return { src },
                    Instr::Halt { src } => break Exit::Halt { src },
                }
            };

            // The one place the instruction pointer is written back.
            let frame = self
                .frames
                .last_mut()
                .expect("internal error: a running VM has a frame");
            frame.ip = ip;
            let base = frame.base;
            let result = match exit {
                Exit::Call { base: callee, args } => self.call(base + callee.index(), args),
                Exit::CallNative {
                    dst,
                    base: first,
                    slot,
                    args,
                } => {
                    let native = Rc::clone(global_native(&self.globals, usize::from(slot)));
                    self.run_native(&native, base + first.index(), usize::from(args))
                        .map(|value| self.stack[base + dst.index()] = value)
                }
                Exit::Return { src } => {
                    let frame = self
                        .frames
                        .pop()
                        .expect("internal error: a running VM has a frame");
                    let value = std::mem::replace(&mut self.stack[base + src.index()], Value::None);
                    self.stack[base..base + frame.function.frame_size].fill(Value::None);
                    // The top level halts, so a frame that returns has a
                    // caller, whose callee register is the one below it.
                    self.stack[base - 1] = value;
                    Ok(())
                }
                Exit::Halt { src } => {
                    self.result =
                        std::mem::replace(&mut self.stack[base + src.index()], Value::None);
                    return Ok(());
                }
                Exit::Error(error) => Err(error),
            };
            if let Err(mut error) = result {
                let frame = self
                    .frames
                    .last()
                    .expect("internal error: a running VM has a frame");
                error
                    .span
                    .get_or_insert_with(|| frame.function.registers.span(frame.ip - 1));
                return Err(error);
            }
        }
    }

    /// Calls the function at `callee`, an index in the stack, with the `args`
    /// values after it: pushes its frame, or for a native, runs it and puts
    /// what it returns at `callee`.
    fn call(&mut self, callee: usize, args: u16) -> Result<(), VmError> {
        let args = usize::from(args);
        let function = match &self.stack[callee] {
            Value::Object(object) => match object.as_ref() {
                Object::Function(Function::Compiled(function)) => Rc::clone(function),
                Object::Function(Function::Native(native)) => {
                    let native = Rc::clone(native);
                    let value = self.run_native(&native, callee + 1, args)?;
                    self.stack[callee] = value;
                    return Ok(());
                }
                Object::Function(_) => {
                    return Err(VmError::native(
                        "not yet supported on the register VM: calling a closure, a pure function or a checked function",
                    ));
                }
                other => {
                    return Err(VmError::native(format!(
                        "Unable to invoke {} as a function.",
                        other.static_type()
                    )));
                }
            },
            other => {
                return Err(VmError::native(format!(
                    "Unable to invoke {} as a function.",
                    other.static_type()
                )));
            }
        };
        debug_assert!(
            function.is_register_code(),
            "internal error: the register VM runs register code"
        );
        if self.frames.len() >= MAX_CALL_DEPTH {
            return Err(calls_too_deep());
        }
        let base = callee + 1;
        if self.stack.len() < base + WINDOW {
            self.stack.resize(base + WINDOW, Value::None);
        }
        self.frames.push(Frame {
            function,
            ip: 0,
            base,
        });
        Ok(())
    }

    /// Runs `native` with the `args` values from `first` on in the stack,
    /// which are `None` afterwards, and returns what it returns.
    fn run_native(
        &mut self,
        native: &NativeFunction,
        first: usize,
        args: usize,
    ) -> Result<Value, VmError> {
        let arguments = &mut self.stack[first..first + args];
        match &native.func {
            NativeFunc::Simple(function) => {
                let result = function(arguments);
                arguments.fill(Value::None);
                result
            }
            NativeFunc::WithVm(function) => {
                let arguments: Vec<Value> = arguments
                    .iter_mut()
                    .map(|argument| std::mem::replace(argument, Value::None))
                    .collect();
                function(&arguments, &mut self.host)
            }
        }
    }
}

/// The native in global slot `slot`, where the compiler only writes a
/// `CallNative` for a native.
fn global_native(globals: &[Value], slot: usize) -> &Rc<NativeFunction> {
    match &globals[slot] {
        Value::Object(object) => match object.as_ref() {
            Object::Function(Function::Native(native)) => native,
            _ => unreachable!("internal error: global slot {slot} holds no native"),
        },
        _ => unreachable!("internal error: global slot {slot} holds no native"),
    }
}

/// A new string with the text of `constant`, a string.
fn new_string(constant: &Value) -> Value {
    let Value::Object(object) = constant else {
        unreachable!("internal error: MakeString's constant is a string");
    };
    let Object::String(text) = object.as_ref() else {
        unreachable!("internal error: MakeString's constant is a string");
    };
    Value::string(text.borrow().as_str())
}

/// Does the binary `op` of the registers `left` and `right` into `dst`.
#[inline(never)]
fn binary(
    regs: &mut [Value; WINDOW],
    op: NativeOp,
    dst: Reg,
    left: Reg,
    right: Reg,
) -> Result<(), VmError> {
    regs[dst.index()] = op.binary(&regs[left.index()], &regs[right.index()])?;
    Ok(())
}

/// What the binary `op` of the registers `left` and `right` fails with, when
/// the instruction's own fast path couldn't do it: an `Int` operation that
/// overflowed, or operands of the wrong type.
#[cold]
#[inline(never)]
fn binary_error(regs: &[Value; WINDOW], op: NativeOp, left: Reg, right: Reg) -> VmError {
    match op.binary(&regs[left.index()], &regs[right.index()]) {
        Err(error) => error,
        Ok(_) => unreachable!("internal error: {op:?} failed on its fast path only"),
    }
}

/// Does the unary `op` of the register `src` into `dst`.
#[inline(never)]
fn unary(regs: &mut [Value; WINDOW], op: NativeOp, dst: Reg, src: Reg) -> Result<(), VmError> {
    regs[dst.index()] = op.unary(&regs[src.index()])?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::NativeSpec;
    use crate::register::Builder;
    use ndc_core::{FunctionRegistry, StaticType};
    use ndc_lexer::Span;

    /// The top-level function `build` writes, using `frame_size` registers.
    fn program(
        frame_size: usize,
        natives: Vec<usize>,
        build: impl FnOnce(&mut Builder),
    ) -> CompiledFunction {
        let mut builder = Builder::default();
        build(&mut builder);
        CompiledFunction {
            name: None,
            static_type: StaticType::Any,
            body: crate::chunk::Chunk::default(),
            num_locals: 0,
            num_globals: 0,
            natives,
            registers: builder.finish(),
            frame_size,
        }
    }

    /// Natives with one, `sum`, in slot 0, which adds up its Int arguments.
    fn natives() -> Natives {
        let mut registry = FunctionRegistry::default();
        registry.declare_global_fn(NativeSpec::with_signature(
            "sum",
            "Adds up Ints.",
            "(Int, Int) -> Int",
            || {
                NativeFunc::Simple(Box::new(|args| {
                    Ok(Value::Int(
                        args.iter()
                            .map(|arg| match arg {
                                Value::Int(n) => *n,
                                _ => 0,
                            })
                            .sum(),
                    ))
                }))
            },
        ));
        let mut natives = Natives::default();
        natives.extend(registry);
        natives
    }

    fn run(natives: &Natives, function: CompiledFunction) -> Result<Value, VmError> {
        let mut vm = Vm::new(function, crate::native_globals(natives), natives);
        vm.run()?;
        Ok(vm.last_value())
    }

    fn emit(builder: &mut Builder, instr: Instr) {
        builder.emit(instr, Span::synthetic());
    }

    fn load_int(builder: &mut Builder, dst: Reg, n: i64) {
        let k = builder.constant(Value::Int(n));
        emit(builder, Instr::LoadK { dst, k });
    }

    #[test]
    fn an_op_whose_destination_is_its_operands() {
        let function = program(1, vec![], |b| {
            load_int(b, Reg(0), 21);
            emit(
                b,
                Instr::AddInt {
                    dst: Reg(0),
                    left: Reg(0),
                    right: Reg(0),
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        assert_eq!(run(&Natives::default(), function).unwrap(), Value::Int(42));
    }

    #[test]
    fn a_move_or_copy_to_its_own_register_keeps_the_value() {
        for instr in [
            Instr::Move {
                dst: Reg(0),
                src: Reg(0),
            },
            Instr::Copy {
                dst: Reg(0),
                src: Reg(0),
            },
        ] {
            let function = program(1, vec![], |b| {
                load_int(b, Reg(0), 7);
                emit(b, instr);
                emit(b, Instr::Halt { src: Reg(0) });
            });
            assert_eq!(run(&Natives::default(), function).unwrap(), Value::Int(7));
        }
    }

    #[test]
    fn a_native_whose_result_goes_to_its_first_argument() {
        let natives = natives();
        let function = program(2, vec![0], |b| {
            load_int(b, Reg(0), 40);
            load_int(b, Reg(1), 2);
            emit(
                b,
                Instr::CallNative {
                    dst: Reg(0),
                    base: Reg(0),
                    slot: 0,
                    args: 2,
                },
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        assert_eq!(run(&natives, function).unwrap(), Value::Int(42));
    }

    #[test]
    fn a_native_empties_its_argument_registers() {
        let natives = natives();
        let function = program(3, vec![0], |b| {
            load_int(b, Reg(0), 40);
            load_int(b, Reg(1), 2);
            emit(
                b,
                Instr::CallNative {
                    dst: Reg(2),
                    base: Reg(0),
                    slot: 0,
                    args: 2,
                },
            );
            emit(b, Instr::Halt { src: Reg(1) });
        });
        assert_eq!(run(&natives, function).unwrap(), Value::None);
    }

    #[test]
    fn a_call_puts_the_result_where_the_callee_was() {
        // fn double(n) => n + n, called as `double(21)`.
        let double = program(1, vec![], |b| {
            emit(
                b,
                Instr::AddInt {
                    dst: Reg(0),
                    left: Reg(0),
                    right: Reg(0),
                },
            );
            emit(b, Instr::Return { src: Reg(0) });
        });
        let function = program(3, vec![], |b| {
            let k = b.constant(Value::function(Function::Compiled(Rc::new(double))));
            emit(b, Instr::LoadK { dst: Reg(1), k });
            load_int(b, Reg(2), 21);
            emit(
                b,
                Instr::Call {
                    base: Reg(1),
                    args: 1,
                },
            );
            emit(b, Instr::Halt { src: Reg(1) });
        });
        assert_eq!(run(&Natives::default(), function).unwrap(), Value::Int(42));
    }

    #[test]
    fn an_error_has_the_span_of_its_instruction() {
        let span = Span::new(ndc_lexer::SourceId::SYNTHETIC, 3, 4);
        let function = program(2, vec![], |b| {
            load_int(b, Reg(0), i64::MAX);
            load_int(b, Reg(1), 1);
            b.emit(
                Instr::AddInt {
                    dst: Reg(0),
                    left: Reg(0),
                    right: Reg(1),
                },
                span,
            );
            emit(b, Instr::Halt { src: Reg(0) });
        });
        let error = run(&Natives::default(), function).unwrap_err();
        assert_eq!(error.span, Some(span));
        assert!(error.message.contains("overflowed"), "{}", error.message);
    }
}
