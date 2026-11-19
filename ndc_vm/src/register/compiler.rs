//! Compiles analysed code to register code, for the register VM.
//!
//! Each expression is compiled with a [`Dest`]: where its value has to go.
//! `let x = a + b` compiles `a + b` with `x`'s register as its destination,
//! which is one instruction, `AddInt x, a, b`. A local variable is its
//! register; temporaries take the registers after the locals, a watermark
//! that goes up while an expression needs them and back down when it's done.
//!
//! It compiles part of the language so far; anything else fails with a
//! "not yet supported on the register VM" error that names it.

use super::chunk::{Builder, Label};
use super::instr::{Instr, Reg, WINDOW};
use crate::compiler::CompileError;
use crate::native_op::NativeOp;
use crate::value::{CompiledFunction, Function};
use crate::{Natives, Value};
use ndc_analysis::{Analysis, Binding, Callee, Candidate, Instance, ResolvedVar};
use ndc_core::collections::HashSet;
use ndc_core::{NodeId, StaticType};
use ndc_lexer::{NumericLiteral, Span};
use ndc_parser::{AssignmentTarget, BindingPattern, Expression, ExpressionLocation};
use std::cell::RefCell;
use std::rc::Rc;

/// Where an expression's value has to go.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Dest {
    /// Nowhere: the value isn't used, as in `f(x);`.
    Discard,
    /// In this register: an argument of a call, or `x` in `let x = ...`.
    Reg(Reg),
    /// Wherever is cheapest: a local variable's own register, or a new
    /// temporary.
    Any,
}

/// Where a compiled expression left its value.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Operand {
    /// In a local variable's register, which an instruction may read but
    /// not take: the variable still holds it.
    Local(Reg),
    /// In a temporary register, which nothing else holds, so an instruction
    /// may take it.
    Temp(Reg),
    /// Nowhere, as asked for with [`Dest::Discard`] or [`Dest::Reg`], which
    /// says where it is.
    Placed,
}

/// Where an instance of a generic function is in [`Analysis::instances`]:
/// the declaration it's an instance of, and which of its instances it is.
#[derive(Clone, Copy)]
struct InstanceAt {
    declaration: NodeId,
    index: usize,
}

/// What a call resolved to a native compiles to.
enum NativeCall {
    /// The op's own instruction: `SubInt` for `n - 1`.
    Op(NativeOp),
    /// `CallNative` of the native in this global slot: `len(xs)`.
    Native(usize),
}

/// The labels of the loop being compiled, which `break` and `continue` jump
/// to.
#[derive(Clone, Copy)]
struct Loop {
    start: Label,
    end: Label,
}

/// Compiles one function to register code.
pub struct Compiler {
    builder: Builder,
    /// What the analyser worked out about the batch being compiled.
    analysis: Rc<Analysis>,
    /// The instance of a generic function whose body is being compiled,
    /// whose picks take the place of the function's own.
    instance: Option<InstanceAt>,
    /// The global slots of the natives the code reads or calls, shared with
    /// the compilers of the functions it declares, which the VM puts in its
    /// globals.
    natives: Rc<RefCell<HashSet<usize>>>,
    /// The op each native does, if it's one, in global slot order.
    native_ops: Rc<[Option<NativeOp>]>,
    /// The declarations whose instances the batch compiled.
    compiled_instances: HashSet<NodeId>,
    /// How many registers the function's own variables take, parameters
    /// first. Temporaries come after them.
    locals: usize,
    /// The first free temporary.
    next_temp: usize,
    /// The most registers the function has used at once.
    frame_size: usize,
    loops: Vec<Loop>,
}

impl Compiler {
    /// Compiles a program into a function the register VM runs at the top
    /// level, as [`crate::compiler::Compiler::compile`] does for the stack VM.
    pub fn compile(
        expressions: impl Iterator<Item = ExpressionLocation>,
        analysis: Analysis,
        natives: &Natives,
    ) -> Result<CompiledFunction, CompileError> {
        let locals = analysis.top_level_frame_size;
        let mut compiler = Self {
            builder: Builder::default(),
            analysis: Rc::new(analysis),
            instance: None,
            natives: Rc::default(),
            native_ops: natives.ops(),
            compiled_instances: HashSet::default(),
            locals,
            next_temp: locals,
            frame_size: locals,
            loops: Vec::new(),
        };
        let expressions = expressions.collect::<Vec<_>>();
        // The program's value is its last expression's that isn't a
        // `;`-statement, kept in a register of its own while the statements
        // after it run.
        let last_value = expressions
            .iter()
            .rposition(|expression| !matches!(expression.expression, Expression::Statement(_)));
        let result = compiler.alloc_temp(Span::synthetic())?;
        for (index, expression) in expressions.into_iter().enumerate() {
            let dest = if Some(index) == last_value {
                Dest::Reg(result)
            } else {
                Dest::Discard
            };
            compiler.compile_expr(expression, dest)?;
        }
        if last_value.is_none() {
            compiler.emit(Instr::LoadUnit { dst: result }, Span::synthetic());
        }
        compiler.emit(Instr::Halt { src: result }, Span::synthetic());

        if compiler
            .analysis
            .instances
            .keys()
            .any(|declaration| !compiler.compiled_instances.contains(declaration))
        {
            return Err(CompileError::not_yet_supported(
                "a generic function declared by an earlier REPL line",
                Span::synthetic(),
            ));
        }

        let mut natives = compiler.natives.take().into_iter().collect::<Vec<_>>();
        natives.sort_unstable();
        let num_globals = compiler.analysis.global_count;
        Ok(compiler.finish(
            None,
            StaticType::Function {
                parameters: Some(vec![]),
                return_type: Box::new(StaticType::Any),
            },
            num_globals,
            natives,
        ))
    }

    /// A compiler for the body of a function the code this one compiles
    /// declares, whose own variables take `locals` registers, compiled with
    /// the picks of `instance`.
    fn nested(&self, locals: usize, instance: Option<InstanceAt>) -> Self {
        Self {
            builder: Builder::default(),
            analysis: Rc::clone(&self.analysis),
            instance,
            natives: Rc::clone(&self.natives),
            native_ops: Rc::clone(&self.native_ops),
            compiled_instances: HashSet::default(),
            locals,
            next_temp: locals,
            frame_size: locals,
            loops: Vec::new(),
        }
    }

    fn finish(
        self,
        name: Option<String>,
        static_type: StaticType,
        num_globals: usize,
        natives: Vec<usize>,
    ) -> CompiledFunction {
        CompiledFunction {
            name,
            static_type,
            body: crate::chunk::Chunk::default(),
            num_locals: 0,
            num_globals,
            natives,
            registers: self.builder.finish(),
            frame_size: self.frame_size,
        }
    }

    fn emit(&mut self, instr: Instr, span: Span) {
        self.builder.emit(instr, span);
    }

    /// The register at `index`, if a frame has one.
    fn register(index: usize, span: Span) -> Result<Reg, CompileError> {
        u16::try_from(index)
            .map(Reg)
            .map_err(|_too_many| CompileError::too_many_registers(span))
    }

    /// The register of the local variable in `slot`.
    fn local(&self, slot: usize, span: Span) -> Result<Reg, CompileError> {
        debug_assert!(
            slot < self.locals,
            "internal error: slot {slot} is outside the frame's {} locals",
            self.locals
        );
        Self::register(slot, span)
    }

    /// A new temporary, the first free one.
    fn alloc_temp(&mut self, span: Span) -> Result<Reg, CompileError> {
        let reg = Self::register(self.next_temp, span)?;
        self.next_temp += 1;
        self.frame_size = self.frame_size.max(self.next_temp);
        debug_assert!(self.frame_size <= WINDOW);
        Ok(reg)
    }

    /// The register an expression compiled with `dest` writes its value to:
    /// the one asked for, or a new temporary.
    fn place(&mut self, dest: Dest, span: Span) -> Result<Reg, CompileError> {
        match dest {
            Dest::Reg(reg) => Ok(reg),
            Dest::Any | Dest::Discard => self.alloc_temp(span),
        }
    }

    /// What an expression compiled with `dest` that wrote its value to
    /// `reg`, which [`Self::place`] gave, returns.
    fn placed(dest: Dest, reg: Reg) -> Operand {
        match dest {
            Dest::Any => Operand::Temp(reg),
            Dest::Reg(_) | Dest::Discard => Operand::Placed,
        }
    }

    /// Compiles `expression`, putting its value where `dest` says. With
    /// [`Dest::Any`], the value is in a local's register, which takes no
    /// temporary, or in the first free temporary, which stays taken.
    fn compile_expr(
        &mut self,
        expression: ExpressionLocation,
        dest: Dest,
    ) -> Result<Operand, CompileError> {
        let mark = self.next_temp;
        let operand = self.compile_expr_inner(expression, dest)?;
        match operand {
            Operand::Temp(reg) => {
                debug_assert_eq!(dest, Dest::Any);
                debug_assert_eq!(
                    reg.index(),
                    mark,
                    "internal error: a value is in the first free temporary"
                );
                self.next_temp = mark + 1;
            }
            Operand::Local(_) | Operand::Placed => self.next_temp = mark,
        }
        Ok(operand)
    }

    fn compile_expr_inner(
        &mut self,
        expression: ExpressionLocation,
        dest: Dest,
    ) -> Result<Operand, CompileError> {
        let ExpressionLocation {
            id,
            expression,
            span,
        } = expression;
        match expression {
            Expression::BoolLiteral(b) => self.load_constant(Value::Bool(b), dest, span),
            Expression::NoneLiteral => self.load_constant(Value::None, dest, span),
            Expression::NumericLiteral(literal) => {
                let value = match literal {
                    // An integer literal is what its use made it: `1` in
                    // `[1.5, 1]` is a Float.
                    NumericLiteral::Int64(i) => match self.analysis.expr_types.get(&id) {
                        Some(StaticType::Float) => Value::float(i as f64),
                        Some(StaticType::Number) => {
                            Value::number(crate::value::AdvancedNumber::Int(i.into()))
                        }
                        _ => Value::int(i),
                    },
                    NumericLiteral::Float64(f) => Value::float(f),
                    NumericLiteral::NumberInt(i) => {
                        Value::number(crate::value::AdvancedNumber::Int(i))
                    }
                    NumericLiteral::NumberFloat(f) => {
                        Value::number(crate::value::AdvancedNumber::Float(f))
                    }
                    NumericLiteral::Complex(c) => Value::complex(c),
                };
                self.load_constant(value, dest, span)
            }
            Expression::StringLiteral(text) => {
                if dest == Dest::Discard {
                    return Ok(Operand::Placed);
                }
                let k = self.builder.constant(Value::string(text));
                let dst = self.place(dest, span)?;
                self.emit(Instr::MakeString { dst, k }, span);
                Ok(Self::placed(dest, dst))
            }
            Expression::Identifier { .. } => match self.binding(id) {
                Binding::Resolved(Candidate::Scalar(Callee::Var(var))) => {
                    self.read_variable(var, dest, span)
                }
                Binding::None => Err(CompileError::unresolved_binding(span)),
                Binding::Resolved(Candidate::Scalar(Callee::Intrinsic(_))) => Err(
                    CompileError::not_yet_supported("an intrinsic used as a value", span),
                ),
                Binding::Resolved(Candidate::Vec(_)) | Binding::PerPosition(_) => {
                    Err(CompileError::not_yet_supported(
                        "an operator applied to each element of tuples",
                        span,
                    ))
                }
            },
            Expression::Statement(statement) => {
                self.compile_expr(*statement, Dest::Discard)?;
                self.unit(dest, span)
            }
            Expression::Grouping(inner) => self.compile_expr(*inner, dest),
            Expression::VariableDeclaration { l_value, value, .. } => {
                let BindingPattern::Identifier { .. } = l_value.pattern else {
                    return Err(CompileError::not_yet_supported(
                        "declaring more than one variable at once",
                        span,
                    ));
                };
                let variable = self.analysis.variable(l_value.id).expect(
                    "internal error: every binding pattern is analysed before it's compiled",
                );
                self.store(variable, *value, span)?;
                self.unit(dest, span)
            }
            Expression::Assignment { l_value, r_value } => {
                let AssignmentTarget::Identifier { .. } = l_value.target else {
                    return Err(CompileError::not_yet_supported(
                        "assigning to anything but a variable",
                        span,
                    ));
                };
                let variable = self.analysis.variable(l_value.id).expect(
                    "internal error: every assignment target is analysed before it's compiled",
                );
                self.store(variable, *r_value, span)?;
                self.unit(dest, span)
            }
            Expression::FunctionDeclaration {
                name, body, pure, ..
            } => {
                self.compile_function_declaration(id, name, *body, pure, span)?;
                self.unit(dest, span)
            }
            Expression::Block { statements } => self.compile_block(statements, dest, span),
            Expression::If {
                condition,
                on_true,
                on_false,
            } => self.compile_if(*condition, *on_true, on_false.map(|e| *e), dest, span),
            Expression::While {
                expression: condition,
                loop_body,
            } => {
                self.compile_while(*condition, *loop_body)?;
                self.unit(dest, span)
            }
            Expression::Call {
                function,
                arguments,
            }
            | Expression::OperatorCall {
                function,
                arguments,
            }
            | Expression::MethodCall {
                function,
                arguments,
            } => self.compile_call(*function, arguments, dest),
            Expression::Return { value } => {
                let mark = self.next_temp;
                let src = match self.compile_expr(*value, Dest::Any)? {
                    Operand::Local(reg) | Operand::Temp(reg) => reg,
                    Operand::Placed => unreachable!("internal error: Dest::Any gives a register"),
                };
                self.emit(Instr::Return { src }, span);
                self.next_temp = mark;
                self.diverged(dest, span)
            }
            Expression::Break => {
                let end = self.current_loop().end;
                self.builder.jump(Instr::Jump { offset: 0 }, end, span);
                self.diverged(dest, span)
            }
            Expression::Continue => {
                let start = self.current_loop().start;
                self.builder.jump(Instr::Jump { offset: 0 }, start, span);
                self.diverged(dest, span)
            }
            Expression::Logical { .. } => {
                Err(CompileError::not_yet_supported("`and` and `or`", span))
            }
            Expression::OpAssignment { .. } => Err(CompileError::not_yet_supported(
                "an assignment with an operator, such as `+=`",
                span,
            )),
            Expression::StructDeclaration { .. } => {
                Err(CompileError::not_yet_supported("structs", span))
            }
            Expression::For { .. } => Err(CompileError::not_yet_supported("`for` loops", span)),
            Expression::MemberAccess { .. } => {
                Err(CompileError::not_yet_supported("member access", span))
            }
            Expression::Tuple { .. } => Err(CompileError::not_yet_supported("tuples", span)),
            Expression::List { .. } => Err(CompileError::not_yet_supported("lists", span)),
            Expression::Map { .. } => Err(CompileError::not_yet_supported("maps", span)),
            Expression::RangeInclusive { .. } | Expression::RangeExclusive { .. } => {
                Err(CompileError::not_yet_supported("ranges", span))
            }
        }
    }

    /// Puts `value`, a constant, where `dest` says.
    fn load_constant(
        &mut self,
        value: Value,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        if dest == Dest::Discard {
            return Ok(Operand::Placed);
        }
        let k = self.builder.constant(value);
        let dst = self.place(dest, span)?;
        self.emit(Instr::LoadK { dst, k }, span);
        Ok(Self::placed(dest, dst))
    }

    /// Puts unit where `dest` says: the value of a declaration, an
    /// assignment, a loop or a statement.
    fn unit(&mut self, dest: Dest, span: Span) -> Result<Operand, CompileError> {
        if dest == Dest::Discard {
            return Ok(Operand::Placed);
        }
        let dst = self.place(dest, span)?;
        self.emit(Instr::LoadUnit { dst }, span);
        Ok(Self::placed(dest, dst))
    }

    /// What an expression after which the code doesn't go on, as `return`,
    /// returns: a register nothing writes, as nothing reads it either.
    fn diverged(&mut self, dest: Dest, span: Span) -> Result<Operand, CompileError> {
        let dst = self.place(dest, span)?;
        Ok(Self::placed(dest, dst))
    }

    /// Puts the value of `variable` where `dest` says.
    fn read_variable(
        &mut self,
        variable: ResolvedVar,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        match variable {
            ResolvedVar::Local { slot } => {
                let reg = self.local(slot, span)?;
                match dest {
                    Dest::Any => Ok(Operand::Local(reg)),
                    Dest::Reg(dst) => {
                        if dst != reg {
                            self.emit(Instr::Copy { dst, src: reg }, span);
                        }
                        Ok(Operand::Placed)
                    }
                    Dest::Discard => Ok(Operand::Placed),
                }
            }
            ResolvedVar::Global { slot } => {
                if slot < self.analysis.builtin_count {
                    self.natives.borrow_mut().insert(slot);
                }
                if dest == Dest::Discard {
                    return Ok(Operand::Placed);
                }
                let dst = self.place(dest, span)?;
                self.emit(
                    Instr::GetGlobal {
                        dst,
                        slot: global(slot),
                    },
                    span,
                );
                Ok(Self::placed(dest, dst))
            }
            ResolvedVar::Cell { .. } | ResolvedVar::Upvalue { .. } => Err(
                CompileError::not_yet_supported("a variable a closure captures", span),
            ),
        }
    }

    /// Compiles `value` and stores it in `variable`, which a declaration or
    /// an assignment at `span` writes.
    fn store(
        &mut self,
        variable: ResolvedVar,
        value: ExpressionLocation,
        span: Span,
    ) -> Result<(), CompileError> {
        match variable {
            ResolvedVar::Local { slot } => {
                let dst = self.local(slot, span)?;
                self.compile_expr(value, Dest::Reg(dst))?;
            }
            ResolvedVar::Global { slot } => {
                let mark = self.next_temp;
                let src = match self.compile_expr(value, Dest::Any)? {
                    Operand::Temp(reg) => reg,
                    // `SetGlobal` takes its value, which a local keeps.
                    Operand::Local(reg) => {
                        let temp = self.alloc_temp(span)?;
                        self.emit(
                            Instr::Copy {
                                dst: temp,
                                src: reg,
                            },
                            span,
                        );
                        temp
                    }
                    Operand::Placed => unreachable!("internal error: Dest::Any gives a register"),
                };
                self.emit(
                    Instr::SetGlobal {
                        slot: global(slot),
                        src,
                    },
                    span,
                );
                self.next_temp = mark;
            }
            ResolvedVar::Cell { .. } | ResolvedVar::Upvalue { .. } => {
                return Err(CompileError::not_yet_supported(
                    "a variable a closure captures",
                    span,
                ));
            }
        }
        Ok(())
    }

    fn compile_block(
        &mut self,
        statements: Vec<ExpressionLocation>,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        let Some(last) = statements.len().checked_sub(1) else {
            return self.unit(dest, span);
        };
        let mut operand = Operand::Placed;
        for (index, statement) in statements.into_iter().enumerate() {
            let statement_dest = if index == last { dest } else { Dest::Discard };
            operand = self.compile_expr(statement, statement_dest)?;
        }
        Ok(operand)
    }

    fn compile_if(
        &mut self,
        condition: ExpressionLocation,
        on_true: ExpressionLocation,
        on_false: Option<ExpressionLocation>,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        let condition_span = condition.span;
        let otherwise = self.builder.label();
        let cond = self.condition(condition)?;
        self.builder.jump(
            Instr::JumpIfFalse { cond, offset: 0 },
            otherwise,
            condition_span,
        );
        let Some(on_false) = on_false else {
            // An `if` without an `else` is `()`, whatever its branch gives.
            self.compile_expr(on_true, Dest::Discard)?;
            self.builder.bind(otherwise);
            return self.unit(dest, span);
        };
        let end = self.builder.label();
        // Both branches put their value in the same register.
        let (branch_dest, dst) = match dest {
            Dest::Discard => (Dest::Discard, None),
            Dest::Reg(reg) => (Dest::Reg(reg), None),
            Dest::Any => {
                let reg = self.alloc_temp(span)?;
                (Dest::Reg(reg), Some(reg))
            }
        };
        self.compile_expr(on_true, branch_dest)?;
        self.builder
            .jump(Instr::Jump { offset: 0 }, end, Span::synthetic());
        self.builder.bind(otherwise);
        self.compile_expr(on_false, branch_dest)?;
        self.builder.bind(end);
        Ok(dst.map_or(Operand::Placed, Operand::Temp))
    }

    fn compile_while(
        &mut self,
        condition: ExpressionLocation,
        body: ExpressionLocation,
    ) -> Result<(), CompileError> {
        let condition_span = condition.span;
        let start = self.builder.label();
        let end = self.builder.label();
        self.builder.bind(start);
        // A `break` in the condition leaves this loop too.
        self.loops.push(Loop { start, end });
        let cond = self.condition(condition)?;
        self.builder
            .jump(Instr::JumpIfFalse { cond, offset: 0 }, end, condition_span);
        self.compile_expr(body, Dest::Discard)?;
        self.loops.pop();
        self.builder
            .jump(Instr::Jump { offset: 0 }, start, Span::synthetic());
        self.builder.bind(end);
        Ok(())
    }

    /// Compiles a condition, and returns the register it's in. Its
    /// temporary is free again, as the jump that reads it comes right after.
    fn condition(&mut self, condition: ExpressionLocation) -> Result<Reg, CompileError> {
        let mark = self.next_temp;
        let cond = match self.compile_expr(condition, Dest::Any)? {
            Operand::Local(reg) | Operand::Temp(reg) => reg,
            Operand::Placed => unreachable!("internal error: Dest::Any gives a register"),
        };
        self.next_temp = mark;
        Ok(cond)
    }

    fn current_loop(&self) -> Loop {
        *self
            .loops
            .last()
            .expect("internal error: the analyser rejects break and continue outside of a loop")
    }

    /// Compiles the call of `function` with `arguments`: the instruction of
    /// the op it resolves to, as `SubInt` for `n - 1`, a `CallNative` of the
    /// native it resolves to, or a `Call` of the function value.
    fn compile_call(
        &mut self,
        function: ExpressionLocation,
        arguments: Vec<ExpressionLocation>,
        dest: Dest,
    ) -> Result<Operand, CompileError> {
        let span = function.span;
        let binding = match &function.expression {
            Expression::Identifier { .. } => Some(self.binding(function.id)),
            _ => None,
        };
        match &binding {
            Some(Binding::Resolved(Candidate::Scalar(Callee::Intrinsic(_)))) => {
                return Err(CompileError::not_yet_supported(
                    "intrinsics, such as `cast`",
                    span,
                ));
            }
            Some(Binding::Resolved(Candidate::Vec(_)) | Binding::PerPosition(_)) => {
                return Err(CompileError::not_yet_supported(
                    "an operator applied to each element of tuples",
                    span,
                ));
            }
            _ => {}
        }
        match binding
            .as_ref()
            .and_then(|binding| self.native_call(binding))
        {
            Some(NativeCall::Op(op)) => self.compile_op(op, arguments, dest, span),
            Some(NativeCall::Native(slot)) => {
                match (u16::try_from(slot), u8::try_from(arguments.len())) {
                    (Ok(slot), Ok(args)) => {
                        self.compile_native_call(slot, args, arguments, dest, span)
                    }
                    // Too many to say in a `CallNative`: called as a value.
                    _ => self.compile_value_call(function, arguments, dest, span),
                }
            }
            None => self.compile_value_call(function, arguments, dest, span),
        }
    }

    /// What a call `binding` resolves to compiles to, if it picks one
    /// native and applies it to the arguments as they are: not `+` in
    /// `(1, 2) + (3, 4)`, which applies it to each pair of elements, nor a
    /// function a program declares, even one named like a native.
    fn native_call(&self, binding: &Binding) -> Option<NativeCall> {
        let Binding::Resolved(Candidate::Scalar(Callee::Var(ResolvedVar::Global { slot }))) =
            *binding
        else {
            return None;
        };
        // Only natives have an entry; a program's globals come after them.
        Some(match *self.native_ops.get(slot)? {
            Some(op) => NativeCall::Op(op),
            None => NativeCall::Native(slot),
        })
    }

    /// Compiles `op` of `arguments` into one instruction that reads them
    /// where they are.
    fn compile_op(
        &mut self,
        op: NativeOp,
        arguments: Vec<ExpressionLocation>,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        debug_assert_eq!(
            op.arity(),
            arguments.len(),
            "the analyser picks an overload by arity"
        );
        let mark = self.next_temp;
        let operands = self.compile_operands(arguments)?;
        // The instruction reads its operands before it writes its
        // destination, which may be one of them.
        self.next_temp = mark;
        let dst = self.place(dest, span)?;
        let instr = match operands[..] {
            [src] => Instr::unary(op, dst, src),
            [left, right] => Instr::binary(op, dst, left, right),
            _ => unreachable!("internal error: an op takes one or two operands"),
        };
        self.emit(instr, span);
        Ok(Self::placed(dest, dst))
    }

    /// Compiles `arguments` in order into registers an instruction reads,
    /// and returns them: a local's own register, or a temporary. A local
    /// read before an argument that may assign it is copied first, so the
    /// instruction sees it as it was: in `x + { x = 5; x }`, `x` before the
    /// block.
    fn compile_operands(
        &mut self,
        arguments: Vec<ExpressionLocation>,
    ) -> Result<Vec<Reg>, CompileError> {
        let mut assigned_later = arguments
            .iter()
            .rev()
            .scan(false, |later, argument| {
                let before = *later;
                *later |= may_assign(argument);
                Some(before)
            })
            .collect::<Vec<_>>();
        assigned_later.reverse();
        let mut operands = Vec::with_capacity(arguments.len());
        for (argument, assigned_later) in arguments.into_iter().zip(assigned_later) {
            let span = argument.span;
            let reg = match self.compile_expr(argument, Dest::Any)? {
                Operand::Local(reg) if assigned_later => {
                    let temp = self.alloc_temp(span)?;
                    self.emit(
                        Instr::Copy {
                            dst: temp,
                            src: reg,
                        },
                        span,
                    );
                    temp
                }
                Operand::Local(reg) | Operand::Temp(reg) => reg,
                Operand::Placed => unreachable!("internal error: Dest::Any gives a register"),
            };
            operands.push(reg);
        }
        Ok(operands)
    }

    /// Compiles `arguments` into registers of their own, one after the
    /// other from the first free temporary, which is returned. A local is
    /// copied there: an argument register is the call's own copy, which the
    /// callee may take or change.
    fn compile_arguments(
        &mut self,
        arguments: Vec<ExpressionLocation>,
        span: Span,
    ) -> Result<Reg, CompileError> {
        let first = Self::register(self.next_temp, span)?;
        let registers = arguments
            .iter()
            .map(|argument| self.alloc_temp(argument.span))
            .collect::<Result<Vec<_>, _>>()?;
        for (argument, reg) in arguments.into_iter().zip(registers) {
            self.compile_expr(argument, Dest::Reg(reg))?;
        }
        Ok(first)
    }

    /// Compiles the call of the native in global slot `slot` with
    /// `arguments`, of which there are `args`.
    fn compile_native_call(
        &mut self,
        slot: u16,
        args: u8,
        arguments: Vec<ExpressionLocation>,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        self.natives.borrow_mut().insert(usize::from(slot));
        let mark = self.next_temp;
        let base = self.compile_arguments(arguments, span)?;
        // The native reads its arguments before its result is written.
        self.next_temp = mark;
        let dst = self.place(dest, span)?;
        self.emit(
            Instr::CallNative {
                dst,
                base,
                slot,
                args,
            },
            span,
        );
        Ok(Self::placed(dest, dst))
    }

    /// Compiles the call of the value of `function` with `arguments`: the
    /// function goes in the first free temporary, the arguments after it,
    /// and the result where the function was.
    fn compile_value_call(
        &mut self,
        function: ExpressionLocation,
        arguments: Vec<ExpressionLocation>,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        let args = u16::try_from(arguments.len())
            .map_err(|_too_many| CompileError::too_many_registers(span))?;
        let base = self.alloc_temp(span)?;
        self.compile_expr(function, Dest::Reg(base))?;
        self.compile_arguments(arguments, span)?;
        self.emit(Instr::Call { base, args }, span);
        self.next_temp = base.index() + 1;
        match dest {
            Dest::Any => Ok(Operand::Temp(base)),
            Dest::Reg(dst) => {
                self.emit(Instr::Move { dst, src: base }, span);
                Ok(Operand::Placed)
            }
            Dest::Discard => Ok(Operand::Placed),
        }
    }

    /// Compiles a named function declared at the top level, which is stored
    /// in a global: its body, or for a generic function, the body of each
    /// of its instances, stored in globals of their own.
    fn compile_function_declaration(
        &mut self,
        id: NodeId,
        name: Option<String>,
        body: ExpressionLocation,
        pure: bool,
        span: Span,
    ) -> Result<(), CompileError> {
        if pure {
            return Err(CompileError::not_yet_supported("`pure fn`", span));
        }
        let Some(ResolvedVar::Global { slot }) = self.analysis.variable(id) else {
            return Err(CompileError::not_yet_supported(
                "a function that isn't declared by name at the top level",
                span,
            ));
        };
        let analysis = Rc::clone(&self.analysis);
        if let Some(instances) = analysis.instances.get(&id) {
            for (index, instance) in instances.iter().enumerate() {
                let at = InstanceAt {
                    declaration: id,
                    index,
                };
                let function = self.compile_function(id, name.clone(), body.clone(), Some(at))?;
                let Instance {
                    variable: ResolvedVar::Global { slot },
                    ..
                } = *instance
                else {
                    unreachable!("internal error: an instance is stored in a global");
                };
                self.store_function(function, slot, span)?;
            }
            self.compiled_instances.insert(id);
            return Ok(());
        }
        let function = self.compile_function(id, name, body, None)?;
        self.store_function(function, slot, span)
    }

    /// Stores `function` in global slot `slot`.
    fn store_function(
        &mut self,
        function: CompiledFunction,
        slot: usize,
        span: Span,
    ) -> Result<(), CompileError> {
        let k = self
            .builder
            .constant(Value::function(Function::Compiled(Rc::new(function))));
        let mark = self.next_temp;
        let src = self.alloc_temp(span)?;
        self.emit(Instr::LoadK { dst: src, k }, span);
        self.emit(
            Instr::SetGlobal {
                slot: global(slot),
                src,
            },
            span,
        );
        self.next_temp = mark;
        Ok(())
    }

    /// Compiles the body of the function declared at `id`, with the picks
    /// of `instance` if it's an instance of a generic function.
    fn compile_function(
        &self,
        id: NodeId,
        name: Option<String>,
        body: ExpressionLocation,
        instance: Option<InstanceAt>,
    ) -> Result<CompiledFunction, CompileError> {
        let shared = self.analysis.function_type(id);
        let static_type = instance
            .and_then(|at| {
                self.analysis.instances[&at.declaration][at.index]
                    .function_types
                    .get(&id)
            })
            .unwrap_or(shared)
            .clone();
        let locals = self.analysis.frame_size(id);
        if locals > WINDOW {
            return Err(CompileError::too_many_registers(body.span));
        }
        let mut compiler = self.nested(locals, instance);
        let src = match compiler.compile_expr(body, Dest::Any)? {
            Operand::Local(reg) | Operand::Temp(reg) => reg,
            Operand::Placed => unreachable!("internal error: Dest::Any gives a register"),
        };
        compiler.emit(Instr::Return { src }, Span::synthetic());
        Ok(compiler.finish(name, static_type, 0, Vec::new()))
    }

    /// What the identifier `id` refers to, in the instance being compiled.
    fn binding(&self, id: NodeId) -> Binding {
        self.instance
            .map(|at| &self.analysis.instances[&at.declaration][at.index])
            .and_then(|instance| instance.bindings.get(&id))
            .unwrap_or_else(|| self.analysis.binding(id))
            .clone()
    }
}

/// The `u32` a global slot is written as in an instruction.
fn global(slot: usize) -> u32 {
    u32::try_from(slot).expect("internal error: a program has fewer than 2^32 globals")
}

/// Whether evaluating `expression` may write a local variable: whether it
/// assigns or declares one, anywhere inside it. A declaration may reuse the
/// register of a block's variable that went out of scope.
fn may_assign(expression: &ExpressionLocation) -> bool {
    match &expression.expression {
        Expression::Assignment { .. }
        | Expression::OpAssignment { .. }
        | Expression::VariableDeclaration { .. }
        | Expression::FunctionDeclaration { .. }
        | Expression::StructDeclaration { .. }
        | Expression::For { .. } => true,
        _ => expression.children().into_iter().any(may_assign),
    }
}
