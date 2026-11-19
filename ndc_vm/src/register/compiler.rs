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
use ndc_analysis::{Analysis, Binding, Callee, Candidate, CaptureSource, Instance, ResolvedVar};
use ndc_core::collections::{HashMap, HashSet};
use ndc_core::{NodeId, StaticType};
use ndc_lexer::{NumericLiteral, Span};
use ndc_parser::{
    AssignmentTarget, BindingPattern, BindingPatternLocation, Expression, ExpressionLocation,
    ForBody, ForIteration, LogicalOperator,
};
use std::cell::{Cell, RefCell};
use std::rc::Rc;

/// How many elements of a list literal, or pairs of a map literal, are made
/// at once from registers of their own; the ones after them are added one by
/// one, so a long literal takes no more registers than a short one.
const LITERAL_CHUNK: usize = 64;

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

/// The right operand of a binary op.
enum Right {
    Reg(Reg),
    /// An `Int` written in the code: `2` in `i * 2`.
    Immediate(i16),
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
    /// Whether the code compiled so far used a pick of `instance` that
    /// differs from the function's own, so it differs between instances.
    used_instance: Cell<bool>,
    /// The functions compiled the same wherever they're declared, by
    /// declaration: outside of any instance, or in an instance's body
    /// without using its picks, so `fn f() => 42` in a generic body is one
    /// function across its instances and keeps its identity.
    prototypes: Rc<RefCell<HashMap<NodeId, Rc<CompiledFunction>>>>,
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
            used_instance: Cell::new(false),
            prototypes: Rc::default(),
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
            used_instance: Cell::new(false),
            prototypes: Rc::clone(&self.prototypes),
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
            Expression::Identifier { .. } => {
                let binding = self.binding(id);
                self.compile_callee(&binding, dest, span)
            }
            Expression::Statement(statement) => {
                self.compile_expr(*statement, Dest::Discard)?;
                self.unit(dest, span)
            }
            Expression::Grouping(inner) => self.compile_expr(*inner, dest),
            Expression::VariableDeclaration { l_value, value, .. } => {
                if let BindingPattern::Identifier { .. } = l_value.pattern {
                    let variable = self.analysis.variable(l_value.id).expect(
                        "internal error: every binding pattern is analysed before it's compiled",
                    );
                    self.store(variable, *value, true, span)?;
                } else {
                    // The whole value is read before any variable is
                    // declared.
                    let mark = self.next_temp;
                    let src = match self.compile_expr(*value, Dest::Any)? {
                        Operand::Local(reg) | Operand::Temp(reg) => reg,
                        Operand::Placed => {
                            unreachable!("internal error: Dest::Any gives a register")
                        }
                    };
                    self.declare_pattern(l_value, src, span)?;
                    self.next_temp = mark;
                }
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
                self.store(variable, *r_value, false, span)?;
                self.unit(dest, span)
            }
            Expression::FunctionDeclaration {
                name, body, pure, ..
            } => self.compile_function_declaration(id, name, *body, pure, dest, span),
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
            Expression::Logical {
                left,
                operator,
                right,
            } => self.compile_logical(*left, operator, *right, dest),
            Expression::OpAssignment { .. } => Err(CompileError::not_yet_supported(
                "an assignment with an operator, such as `+=`",
                span,
            )),
            Expression::StructDeclaration { .. } => {
                self.compile_struct_declaration(id, span)?;
                self.unit(dest, span)
            }
            Expression::For { iterations, body } => {
                self.compile_for(&iterations, *body, dest, span)
            }
            Expression::MemberAccess {
                receiver,
                member_span,
                ..
            } => {
                let getter = self
                    .getter(id)
                    .expect("internal error: every member access is analysed before it's compiled");
                self.compile_binding_call(&getter, vec![*receiver], dest, member_span)
            }
            Expression::Tuple { values } => self.compile_tuple(values, dest, span),
            Expression::List { values } => self.compile_list(values, dest, span),
            Expression::Map { values, default } => {
                self.compile_map(values, default.map(|default| *default), dest, span)
            }
            Expression::RangeInclusive { start, end } => {
                self.compile_range(start, end, true, dest, span)
            }
            Expression::RangeExclusive { start, end } => {
                self.compile_range(start, end, false, dest, span)
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
            // A captured local is read through its cell, into a register of
            // its own: a call may change it, as can a closure it calls.
            ResolvedVar::Cell { slot } => {
                let src = self.local(slot, span)?;
                if dest == Dest::Discard {
                    return Ok(Operand::Placed);
                }
                let dst = self.place(dest, span)?;
                self.emit(Instr::GetCell { dst, src }, span);
                Ok(Self::placed(dest, dst))
            }
            ResolvedVar::Upvalue { slot } => {
                let index = upvalue(slot, span)?;
                if dest == Dest::Discard {
                    return Ok(Operand::Placed);
                }
                let dst = self.place(dest, span)?;
                self.emit(Instr::GetUpvalue { dst, index }, span);
                Ok(Self::placed(dest, dst))
            }
        }
    }

    /// Compiles `value` and stores it in `variable`, which a declaration,
    /// when `declaring`, or an assignment at `span` writes. A declaration of
    /// a local that closures capture writes its register, which replaces
    /// the cell a closure made of an earlier run of it, as in a loop, so
    /// each run makes a new variable; an assignment writes into the cell.
    fn store(
        &mut self,
        variable: ResolvedVar,
        value: ExpressionLocation,
        declaring: bool,
        span: Span,
    ) -> Result<(), CompileError> {
        let mark = self.next_temp;
        match variable {
            ResolvedVar::Local { slot } => {
                let dst = self.local(slot, span)?;
                self.compile_expr(value, Dest::Reg(dst))?;
            }
            ResolvedVar::Cell { slot } if declaring => {
                let dst = self.local(slot, span)?;
                self.compile_expr(value, Dest::Reg(dst))?;
            }
            ResolvedVar::Cell { slot } => {
                let cell = self.local(slot, span)?;
                let src = self.compile_owned(value)?;
                self.emit(Instr::SetCell { cell, src }, span);
            }
            ResolvedVar::Upvalue { slot } => {
                let index = upvalue(slot, span)?;
                let src = self.compile_owned(value)?;
                self.emit(Instr::SetUpvalue { index, src }, span);
            }
            ResolvedVar::Global { slot } => {
                let src = self.compile_owned(value)?;
                self.emit(
                    Instr::SetGlobal {
                        slot: global(slot),
                        src,
                    },
                    span,
                );
            }
        }
        self.next_temp = mark;
        Ok(())
    }

    /// Compiles `expression` into a register that nothing else holds, for
    /// an instruction that takes the value out of it: a temporary, or a
    /// copy of a local's value. The register stays taken until the caller
    /// frees it.
    fn compile_owned(&mut self, expression: ExpressionLocation) -> Result<Reg, CompileError> {
        let span = expression.span;
        match self.compile_expr(expression, Dest::Any)? {
            Operand::Temp(reg) => Ok(reg),
            Operand::Local(reg) => {
                let temp = self.alloc_temp(span)?;
                self.emit(
                    Instr::Copy {
                        dst: temp,
                        src: reg,
                    },
                    span,
                );
                Ok(temp)
            }
            Operand::Placed => unreachable!("internal error: Dest::Any gives a register"),
        }
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
        let otherwise = self.builder.label();
        self.jump_unless(condition, otherwise)?;
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
        let start = self.builder.label();
        let end = self.builder.label();
        self.builder.bind(start);
        // A `break` in the condition leaves this loop too.
        self.loops.push(Loop { start, end });
        self.jump_unless(condition, end)?;
        self.compile_expr(body, Dest::Discard)?;
        self.loops.pop();
        self.builder
            .jump(Instr::Jump { offset: 0 }, start, Span::synthetic());
        self.builder.bind(end);
        Ok(())
    }

    /// Compiles `condition` and a jump to `target` taken when it's false. A
    /// condition that compares two values is one instruction that compares
    /// and jumps, which makes no `Bool`: `i < n` is `JumpIfNotLess i, n`.
    fn jump_unless(
        &mut self,
        condition: ExpressionLocation,
        target: Label,
    ) -> Result<(), CompileError> {
        let mark = self.next_temp;
        let ExpressionLocation {
            id,
            expression,
            span,
        } = condition;
        match expression {
            Expression::Grouping(inner) => return self.jump_unless(*inner, target),
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
            } if let Some(op) = self.comparison(&function, arguments.len()) => {
                let jump = match self.compile_binary_operands(arguments, true)? {
                    (left, Right::Reg(right)) => Instr::jump_unless(op, left, right),
                    (left, Right::Immediate(imm)) => Instr::jump_unless_immediate(op, left, imm),
                }
                .expect("internal error: a comparison has a compare-and-branch");
                self.builder.jump(jump, target, function.span);
            }
            expression => {
                let condition = ExpressionLocation {
                    id,
                    expression,
                    span,
                };
                let cond = match self.compile_expr(condition, Dest::Any)? {
                    Operand::Local(reg) | Operand::Temp(reg) => reg,
                    Operand::Placed => unreachable!("internal error: Dest::Any gives a register"),
                };
                self.builder
                    .jump(Instr::JumpIfFalse { cond, offset: 0 }, target, span);
            }
        }
        // The jump has read its operands.
        self.next_temp = mark;
        Ok(())
    }

    /// The comparison a call of `function` with `args` arguments does, if
    /// it resolves to one of the comparison ops, as `<` of two Ints does.
    fn comparison(&self, function: &ExpressionLocation, args: usize) -> Option<NativeOp> {
        let Expression::Identifier { .. } = function.expression else {
            return None;
        };
        match self.native_call(&self.binding(function.id))? {
            NativeCall::Op(op) if args == 2 && Instr::jump_unless(op, Reg(0), Reg(0)).is_some() => {
                Some(op)
            }
            _ => None,
        }
    }

    fn current_loop(&self) -> Loop {
        *self
            .loops
            .last()
            .expect("internal error: the analyser rejects break and continue outside of a loop")
    }

    /// Compiles the call of `function` with `arguments`: of what the name
    /// refers to (see [`Self::compile_binding_call`]), or of the function
    /// value any other expression gives, as `make()(1)`.
    fn compile_call(
        &mut self,
        function: ExpressionLocation,
        arguments: Vec<ExpressionLocation>,
        dest: Dest,
    ) -> Result<Operand, CompileError> {
        let span = function.span;
        if let Expression::Identifier { .. } = function.expression {
            let binding = self.binding(function.id);
            return self.compile_binding_call(&binding, arguments, dest, span);
        }
        let base = self.alloc_temp(span)?;
        self.compile_expr(function, Dest::Reg(base))?;
        self.finish_call(base, arguments, false, dest, span)
    }

    /// Compiles the call at `span` of what `binding` refers to with
    /// `arguments`: the instruction of the op it resolves to, as `SubInt`
    /// for `n - 1`, a `CallNative` of the native it resolves to, a `CallVec`
    /// of an operator applied to each element of tuples, or a `Call` of the
    /// function value.
    fn compile_binding_call(
        &mut self,
        binding: &Binding,
        arguments: Vec<ExpressionLocation>,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        if let Binding::Resolved(Candidate::Scalar(Callee::Intrinsic(_))) = binding {
            return Err(CompileError::not_yet_supported(
                "intrinsics, such as `cast`",
                span,
            ));
        }
        match self.native_call(binding) {
            Some(NativeCall::Op(op)) => return self.compile_op(op, arguments, dest, span),
            Some(NativeCall::Native(slot)) => {
                // A call with too many to say in a `CallNative` calls the
                // native as a value.
                if let (Ok(slot), Ok(args)) = (u16::try_from(slot), u8::try_from(arguments.len())) {
                    return self.compile_native_call(slot, args, arguments, dest, span);
                }
            }
            None => {}
        }
        let vectorised = matches!(
            binding,
            Binding::Resolved(Candidate::Vec(_)) | Binding::PerPosition(_)
        );
        let base = self.alloc_temp(span)?;
        self.compile_callee(binding, Dest::Reg(base), span)?;
        self.finish_call(base, arguments, vectorised, dest, span)
    }

    /// Puts the value of what `binding` refers to where `dest` says: the
    /// variable's value, or for an operator whose tuple positions take
    /// different overloads, the tuple of them, which `CallVec` applies
    /// position by position.
    fn compile_callee(
        &mut self,
        binding: &Binding,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        match binding {
            Binding::None => Err(CompileError::unresolved_binding(span)),
            Binding::Resolved(candidate) => match candidate.var() {
                Callee::Var(variable) => self.read_variable(variable, dest, span),
                Callee::Intrinsic(_) => Err(CompileError::not_yet_supported(
                    "an intrinsic used as a value",
                    span,
                )),
            },
            Binding::PerPosition(overloads) => {
                let mark = self.next_temp;
                let base = Self::register(mark, span)?;
                for &overload in overloads {
                    let reg = self.alloc_temp(span)?;
                    let Callee::Var(variable) = overload else {
                        return Err(CompileError::not_yet_supported(
                            "an intrinsic used as a value",
                            span,
                        ));
                    };
                    self.read_variable(variable, Dest::Reg(reg), span)?;
                }
                let len = u16::try_from(overloads.len())
                    .map_err(|_too_many| CompileError::too_many_registers(span))?;
                // `MakeTuple` reads its elements before it writes `dst`.
                self.next_temp = mark;
                let dst = self.place(dest, span)?;
                self.emit(Instr::MakeTuple { dst, base, len }, span);
                Ok(Self::placed(dest, dst))
            }
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
        if op.arity() == 1 {
            let [src] = self.compile_operands(arguments)?[..] else {
                unreachable!("internal error: a unary op takes one operand");
            };
            self.next_temp = mark;
            let dst = self.place(dest, span)?;
            self.emit(Instr::unary(op, dst, src), span);
            return Ok(Self::placed(dest, dst));
        }
        let immediate = Instr::binary_immediate(op, Reg(0), Reg(0), 0).is_some();
        let (left, right) = self.compile_binary_operands(arguments, immediate)?;
        // The instruction reads its operands before it writes its
        // destination, which may be one of them.
        self.next_temp = mark;
        let dst = self.place(dest, span)?;
        let instr = match right {
            Right::Reg(right) => Instr::binary(op, dst, left, right),
            Right::Immediate(imm) => Instr::binary_immediate(op, dst, left, imm)
                .expect("internal error: the op has an immediate form"),
        };
        self.emit(instr, span);
        Ok(Self::placed(dest, dst))
    }

    /// Compiles the two operands of a binary op: two registers, or with
    /// `immediate`, a register and the `Int` on the right when it's a
    /// number written in the code that fits an `i16`, as `2` in `i * 2`.
    fn compile_binary_operands(
        &mut self,
        arguments: Vec<ExpressionLocation>,
        immediate: bool,
    ) -> Result<(Reg, Right), CompileError> {
        if immediate && let Some(imm) = self.small_int(&arguments[1]) {
            let [left, _] = <[_; 2]>::try_from(arguments)
                .expect("internal error: a binary op takes two operands");
            let left = match self.compile_expr(left, Dest::Any)? {
                Operand::Local(reg) | Operand::Temp(reg) => reg,
                Operand::Placed => unreachable!("internal error: Dest::Any gives a register"),
            };
            return Ok((left, Right::Immediate(imm)));
        }
        let [left, right] = self.compile_operands(arguments)?[..] else {
            unreachable!("internal error: a binary op takes two operands");
        };
        Ok((left, Right::Reg(right)))
    }

    /// The value of `expression` if it's an `Int` written in the code that
    /// fits an `i16`: `2`, but not `2` where the analyser made it a Float.
    fn small_int(&self, expression: &ExpressionLocation) -> Option<i16> {
        let Expression::NumericLiteral(NumericLiteral::Int64(value)) = expression.expression else {
            return None;
        };
        if matches!(
            self.analysis.expr_types.get(&expression.id),
            Some(StaticType::Float | StaticType::Number)
        ) {
            return None;
        }
        i16::try_from(value).ok()
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

    /// Declares the variables of `pattern`, which a declaration or a `for`
    /// loop at `span` declares, with the value in `src`: a sequence
    /// pattern's parts with the elements of the value, which `Unpack` puts
    /// in temporaries first.
    fn declare_pattern(
        &mut self,
        pattern: BindingPatternLocation,
        src: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        match pattern.pattern {
            BindingPattern::Identifier { .. } => {
                let variable = self.analysis.variable(pattern.id).expect(
                    "internal error: every binding pattern is analysed before it's compiled",
                );
                self.declare_from(variable, src, span)
            }
            BindingPattern::Sequence(patterns) => {
                let len = u16::try_from(patterns.len())
                    .map_err(|_too_many| CompileError::too_many_registers(span))?;
                let mark = self.next_temp;
                let base = Self::register(mark, span)?;
                for _ in 0..len {
                    self.alloc_temp(span)?;
                }
                self.emit(Instr::Unpack { src, base, len }, span);
                for (index, pattern) in patterns.into_iter().enumerate() {
                    let element = base
                        .offset(index)
                        .expect("internal error: the elements' registers exist");
                    self.declare_pattern(pattern, element, span)?;
                }
                self.next_temp = mark;
                Ok(())
            }
        }
    }

    /// Declares `variable` with the value in `src`, which it takes if `src`
    /// is a temporary. A declaration replaces what a captured local's
    /// register held, as [`Self::store`] does.
    fn declare_from(
        &mut self,
        variable: ResolvedVar,
        src: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        let owned = src.index() >= self.locals;
        match variable {
            ResolvedVar::Local { slot } | ResolvedVar::Cell { slot } => {
                let dst = self.local(slot, span)?;
                if dst != src {
                    let copy = if owned {
                        Instr::Move { dst, src }
                    } else {
                        Instr::Copy { dst, src }
                    };
                    self.emit(copy, span);
                }
            }
            ResolvedVar::Global { slot } => {
                let mark = self.next_temp;
                let src = if owned {
                    src
                } else {
                    let temp = self.alloc_temp(span)?;
                    self.emit(Instr::Copy { dst: temp, src }, span);
                    temp
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
            ResolvedVar::Upvalue { .. } => {
                unreachable!("internal error: the analyser never declares an upvalue")
            }
        }
        Ok(())
    }

    /// Compiles a `for` loop, whose value is unit, or a list or map
    /// comprehension, whose value is the list or map it makes.
    fn compile_for(
        &mut self,
        iterations: &[ForIteration],
        body: ForBody,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        match body {
            ForBody::Block(block) => {
                self.compile_iterations(iterations, span, &mut |this| {
                    this.compile_expr(block.clone(), Dest::Discard).map(|_| ())
                })?;
                self.unit(dest, span)
            }
            ForBody::List { expr } => {
                let list = self.alloc_temp(span)?;
                self.emit(
                    Instr::MakeList {
                        dst: list,
                        base: list,
                        len: 0,
                    },
                    span,
                );
                self.compile_iterations(iterations, span, &mut |this| {
                    let src = this.alloc_temp(span)?;
                    this.compile_expr(expr.clone(), Dest::Reg(src))?;
                    this.emit(Instr::ListPush { list, src }, span);
                    this.next_temp = src.index();
                    Ok(())
                })?;
                Ok(self.built(list, dest, span))
            }
            ForBody::Map {
                key,
                value,
                default,
            } => {
                // The default is made once, before the loop.
                let map = self.alloc_temp(span)?;
                let has_default = default.is_some();
                if let Some(default) = default {
                    let reg = self.alloc_temp(span)?;
                    self.compile_expr(*default, Dest::Reg(reg))?;
                    self.next_temp = map.index() + 1;
                }
                let base = map
                    .offset(1)
                    .ok_or_else(|| CompileError::too_many_registers(span))?;
                self.emit(
                    Instr::MakeMap {
                        dst: map,
                        base,
                        pairs: 0,
                        default: has_default,
                    },
                    span,
                );
                self.compile_iterations(iterations, span, &mut |this| {
                    let (key, value) = this.compile_pair(key.clone(), value.clone(), span)?;
                    this.emit(Instr::MapInsert { map, key, value }, span);
                    this.next_temp = key.index();
                    Ok(())
                })?;
                Ok(self.built(map, dest, span))
            }
        }
    }

    /// Compiles the `iterations` of a `for` loop at `span`, each loop inside
    /// the one before, and a guard's code inside the loop before it, with
    /// `compile_leaf` compiling the body inside them all. A `break` or
    /// `continue` in the body leaves or goes on with the innermost loop.
    fn compile_iterations(
        &mut self,
        iterations: &[ForIteration],
        span: Span,
        compile_leaf: &mut dyn FnMut(&mut Self) -> Result<(), CompileError>,
    ) -> Result<(), CompileError> {
        let Some((first, rest)) = iterations.split_first() else {
            return compile_leaf(self);
        };
        let mark = self.next_temp;
        match first {
            ForIteration::Iteration { l_value, sequence } => {
                let sequence_span = sequence.span;
                // The iterator takes the sequence's temporary, or one of its
                // own when the sequence is a local.
                let (iter, src) = match self.compile_expr(sequence.clone(), Dest::Any)? {
                    Operand::Temp(reg) => (reg, reg),
                    Operand::Local(reg) => (self.alloc_temp(sequence_span)?, reg),
                    Operand::Placed => unreachable!("internal error: Dest::Any gives a register"),
                };
                self.emit(Instr::GetIterator { dst: iter, src }, sequence_span);
                let start = self.builder.label();
                let end = self.builder.label();
                self.builder.bind(start);
                let value = self.alloc_temp(span)?;
                self.builder.jump(
                    Instr::IterNext {
                        iter,
                        dst: value,
                        offset: 0,
                    },
                    end,
                    span,
                );
                self.declare_pattern(l_value.clone(), value, span)?;
                self.next_temp = value.index();
                self.loops.push(Loop { start, end });
                self.compile_iterations(rest, span, compile_leaf)?;
                self.loops.pop();
                self.builder.jump(Instr::Jump { offset: 0 }, start, span);
                self.builder.bind(end);
            }
            ForIteration::Guard(condition) => {
                let skip = self.builder.label();
                self.jump_unless(condition.clone(), skip)?;
                self.compile_iterations(rest, span, compile_leaf)?;
                self.builder.bind(skip);
            }
        }
        self.next_temp = mark;
        Ok(())
    }

    /// Compiles `a and b` or `a or b`: `b` only runs when `a` doesn't
    /// settle it. Both go to one register, which isn't a local's: the left
    /// operand written to a local's register would change what the right
    /// one reads of it, as in `x = y and x`.
    fn compile_logical(
        &mut self,
        left: ExpressionLocation,
        operator: LogicalOperator,
        right: ExpressionLocation,
        dest: Dest,
    ) -> Result<Operand, CompileError> {
        let left_span = left.span;
        let dst = match dest {
            Dest::Reg(reg) if reg.index() >= self.locals => reg,
            Dest::Reg(_) | Dest::Any | Dest::Discard => self.alloc_temp(left_span)?,
        };
        self.compile_expr(left, Dest::Reg(dst))?;
        let end = self.builder.label();
        let jump = match operator {
            LogicalOperator::And => Instr::JumpIfFalse {
                cond: dst,
                offset: 0,
            },
            LogicalOperator::Or => Instr::JumpIfTrue {
                cond: dst,
                offset: 0,
            },
        };
        self.builder.jump(jump, end, left_span);
        self.compile_expr(right, Dest::Reg(dst))?;
        self.builder.bind(end);
        Ok(match dest {
            Dest::Any => Operand::Temp(dst),
            Dest::Reg(reg) if reg != dst => {
                self.emit(Instr::Move { dst: reg, src: dst }, left_span);
                Operand::Placed
            }
            Dest::Reg(_) | Dest::Discard => Operand::Placed,
        })
    }

    /// Compiles a tuple literal, its elements in order.
    fn compile_tuple(
        &mut self,
        values: Vec<ExpressionLocation>,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        let len = u16::try_from(values.len())
            .map_err(|_too_many| CompileError::too_many_registers(span))?;
        let mark = self.next_temp;
        let base = self.compile_arguments(values, span)?;
        // `MakeTuple` reads its elements before it writes `dst`.
        self.next_temp = mark;
        let dst = self.place(dest, span)?;
        self.emit(Instr::MakeTuple { dst, base, len }, span);
        Ok(Self::placed(dest, dst))
    }

    /// Compiles a list literal, its elements in order: the first
    /// [`LITERAL_CHUNK`] in registers of their own, which `MakeList` makes
    /// the list of, and each one after them pushed onto it, so a long list
    /// takes no more registers than a short one.
    fn compile_list(
        &mut self,
        mut values: Vec<ExpressionLocation>,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        let rest = values.split_off(values.len().min(LITERAL_CHUNK));
        let len = u16::try_from(values.len()).expect("internal error: a chunk fits a u16");
        let mark = self.next_temp;
        let base = self.compile_arguments(values, span)?;
        self.next_temp = mark;
        if rest.is_empty() {
            let dst = self.place(dest, span)?;
            self.emit(Instr::MakeList { dst, base, len }, span);
            return Ok(Self::placed(dest, dst));
        }
        let list = self.alloc_temp(span)?;
        self.emit(
            Instr::MakeList {
                dst: list,
                base,
                len,
            },
            span,
        );
        for value in rest {
            let src = self.alloc_temp(value.span)?;
            self.compile_expr(value, Dest::Reg(src))?;
            self.emit(Instr::ListPush { list, src }, span);
            self.next_temp = src.index();
        }
        Ok(self.built(list, dest, span))
    }

    /// Compiles a map literal, each key before its value, and its default
    /// after them, all in registers of their own, which `MakeMap` makes the
    /// map of. Unlike a list's elements they aren't added in chunks: a key
    /// is hashed by what it holds when it goes into the map, so a map made
    /// before its last expression ran would hash a key that expression
    /// changes, as the list `k` in `%{k: 1, …, other: k.push(0)}`, too early.
    fn compile_map(
        &mut self,
        values: Vec<(ExpressionLocation, Option<ExpressionLocation>)>,
        default: Option<ExpressionLocation>,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        let pairs = u16::try_from(values.len())
            .map_err(|_too_many| CompileError::too_many_registers(span))?;
        let mark = self.next_temp;
        let base = Self::register(mark, span)?;
        for (key, value) in values {
            self.compile_pair(key, value, span)?;
        }
        let has_default = default.is_some();
        if let Some(default) = default {
            let reg = self.alloc_temp(default.span)?;
            self.compile_expr(default, Dest::Reg(reg))?;
        }
        // `MakeMap` reads its pairs before it writes `dst`.
        self.next_temp = mark;
        let dst = self.place(dest, span)?;
        self.emit(
            Instr::MakeMap {
                dst,
                base,
                pairs,
                default: has_default,
            },
            span,
        );
        Ok(Self::placed(dest, dst))
    }

    /// Compiles a key of a map literal and its value, which is unit when
    /// there's none, as in the set `{1, 2}`, into two new temporaries.
    fn compile_pair(
        &mut self,
        key: ExpressionLocation,
        value: Option<ExpressionLocation>,
        span: Span,
    ) -> Result<(Reg, Reg), CompileError> {
        let key_reg = self.alloc_temp(key.span)?;
        let value_reg = self.alloc_temp(span)?;
        self.compile_expr(key, Dest::Reg(key_reg))?;
        match value {
            Some(value) => {
                self.compile_expr(value, Dest::Reg(value_reg))?;
            }
            None => self.emit(Instr::LoadUnit { dst: value_reg }, Span::synthetic()),
        }
        Ok((key_reg, value_reg))
    }

    /// What a literal built in `reg`, the first free temporary but one,
    /// gives for `dest`.
    fn built(&mut self, reg: Reg, dest: Dest, span: Span) -> Operand {
        debug_assert_eq!(reg.index() + 1, self.next_temp);
        match dest {
            Dest::Any => Operand::Temp(reg),
            Dest::Reg(dst) => {
                self.emit(Instr::Move { dst, src: reg }, span);
                Operand::Placed
            }
            Dest::Discard => Operand::Placed,
        }
    }

    /// Compiles the range `start..end`, or `start..=end` if `inclusive`, or
    /// `start..` without an end.
    fn compile_range(
        &mut self,
        start: Option<Box<ExpressionLocation>>,
        end: Option<Box<ExpressionLocation>>,
        inclusive: bool,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        let start = start.expect("internal error: the parser gives a range a start");
        let bounds = std::iter::once(*start).chain(end.map(|end| *end)).collect();
        let mark = self.next_temp;
        let operands = self.compile_operands(bounds)?;
        // The range reads its bounds before it writes `dst`.
        self.next_temp = mark;
        let dst = self.place(dest, span)?;
        let instr = match (&operands[..], inclusive) {
            ([start, end], false) => Instr::MakeRange {
                dst,
                start: *start,
                end: *end,
            },
            ([start, end], true) => Instr::MakeRangeInclusive {
                dst,
                start: *start,
                end: *end,
            },
            ([start], _) => Instr::MakeRangeFrom { dst, start: *start },
            _ => unreachable!("internal error: a range has one or two bounds"),
        };
        self.emit(instr, span);
        Ok(Self::placed(dest, dst))
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

    /// Compiles the rest of the call of the function in `base`, the first
    /// free temporary but one, with `arguments`: they go in the registers
    /// after it, and the result where the function was. A `vectorised` call
    /// applies the function to each element of tuples, with `CallVec`.
    fn finish_call(
        &mut self,
        base: Reg,
        arguments: Vec<ExpressionLocation>,
        vectorised: bool,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        debug_assert_eq!(base.index() + 1, self.next_temp);
        let args = u16::try_from(arguments.len())
            .map_err(|_too_many| CompileError::too_many_registers(span))?;
        self.compile_arguments(arguments, span)?;
        let call = if vectorised {
            Instr::CallVec { base, args }
        } else {
            Instr::Call { base, args }
        };
        self.emit(call, span);
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

    /// Compiles the function declared at `id`: a closure of its body over
    /// what it captures, or the body alone if it captures nothing, stored in
    /// the variable its name declares. A function without a name is the
    /// expression's value instead, which `dest` says where to put; a
    /// declaration's value is unit. A generic function, which is declared at
    /// the top level, runs as its instances, each its body compiled with
    /// the instance's picks and stored in a global of its own.
    fn compile_function_declaration(
        &mut self,
        id: NodeId,
        name: Option<String>,
        body: ExpressionLocation,
        pure: bool,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        let analysis = Rc::clone(&self.analysis);
        if let Some(instances) = analysis.instances.get(&id) {
            for (index, instance) in instances.iter().enumerate() {
                let at = InstanceAt {
                    declaration: id,
                    index,
                };
                let (function, _) =
                    self.compile_function_body(id, name.clone(), body.clone(), Some(at))?;
                // A `pure fn`'s instance has a cache of its own, as arguments
                // of different types can be equal: `f(1)` and `f(1.0)` call
                // different instances, which may return different values.
                self.store_function(Rc::new(function), &[], pure, instance.variable, span)?;
            }
            self.compiled_instances.insert(id);
            return self.unit(dest, span);
        }
        let function = self.prototype(id, name, body)?;
        let captures = analysis.captures(id);
        let Some(variable) = analysis.variable(id) else {
            let dst = self.place(dest, span)?;
            self.make_function(function, captures, pure, dst, span);
            return Ok(Self::placed(dest, dst));
        };
        self.store_function(function, captures, pure, variable, span)?;
        self.unit(dest, span)
    }

    /// Stores a function made of `function` (see [`Self::make_function`])
    /// in `variable`, which its declaration at `span` declares.
    fn store_function(
        &mut self,
        function: Rc<CompiledFunction>,
        captures: &[CaptureSource],
        pure: bool,
        variable: ResolvedVar,
        span: Span,
    ) -> Result<(), CompileError> {
        let mark = self.next_temp;
        match variable {
            ResolvedVar::Local { slot } => {
                let dst = self.local(slot, span)?;
                self.make_function(function, captures, pure, dst, span);
            }
            // A function that captures itself needs its cell before its
            // value exists.
            ResolvedVar::Cell { slot } => {
                let cell = self.local(slot, span)?;
                self.emit(Instr::NewCell { reg: cell }, span);
                let src = self.alloc_temp(span)?;
                self.make_function(function, captures, pure, src, span);
                self.emit(Instr::SetCell { cell, src }, span);
            }
            ResolvedVar::Global { slot } => {
                let src = self.alloc_temp(span)?;
                self.make_function(function, captures, pure, src, span);
                self.emit(
                    Instr::SetGlobal {
                        slot: global(slot),
                        src,
                    },
                    span,
                );
            }
            ResolvedVar::Upvalue { .. } => {
                unreachable!("internal error: the analyser never declares an upvalue")
            }
        }
        self.next_temp = mark;
        Ok(())
    }

    /// Puts a function value of `function` in `dst`: a closure over
    /// `captures` if it captures anything, the function itself if not, and
    /// for a `pure` one, that in a memoising wrapper with a cache of its
    /// own, made each time the declaration runs.
    fn make_function(
        &mut self,
        function: Rc<CompiledFunction>,
        captures: &[CaptureSource],
        pure: bool,
        dst: Reg,
        span: Span,
    ) {
        if captures.is_empty() {
            let k = self
                .builder
                .constant(Value::function(Function::Compiled(function)));
            self.emit(Instr::LoadK { dst, k }, span);
        } else {
            let index = self.builder.closure(function, captures);
            self.emit(Instr::Closure { dst, index }, span);
        }
        if pure {
            self.emit(Instr::Memoize { reg: dst }, span);
        }
    }

    /// Stores `value`, a constant, in `variable`, which a declaration at
    /// `span` declares. A declaration replaces what a captured local's
    /// register held, so a declaration that runs again, as in a loop, makes
    /// a new variable rather than writing to the cell a closure kept.
    fn declare_value(
        &mut self,
        variable: ResolvedVar,
        value: Value,
        span: Span,
    ) -> Result<(), CompileError> {
        let k = self.builder.constant(value);
        match variable {
            ResolvedVar::Local { slot } | ResolvedVar::Cell { slot } => {
                let dst = self.local(slot, span)?;
                self.emit(Instr::LoadK { dst, k }, span);
            }
            ResolvedVar::Global { slot } => {
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
            }
            ResolvedVar::Upvalue { .. } => {
                unreachable!("internal error: the analyser never declares an upvalue")
            }
        }
        Ok(())
    }

    /// Compiles the struct declaration at `id`: its constructor, and the
    /// getter and setter of each field, made here and stored where the
    /// analyser put them.
    fn compile_struct_declaration(&mut self, id: NodeId, span: Span) -> Result<(), CompileError> {
        let analysis = Rc::clone(&self.analysis);
        let declared = analysis.declared_struct(id);
        // The constructor and accessors share this one `Rc`: struct values
        // and accessor checks compare their info by identity.
        let info = Rc::new(declared.info.clone());
        let constructor = analysis
            .variable(id)
            .expect("internal error: a struct declaration declares its constructor");
        self.declare_value(
            constructor,
            Value::function(Function::struct_constructor(Rc::clone(&info))),
            span,
        )?;
        for (index, field) in declared.fields.iter().enumerate() {
            self.declare_value(
                field.getter,
                Value::function(Function::struct_getter(Rc::clone(&info), index)),
                span,
            )?;
            self.declare_value(
                field.setter,
                Value::function(Function::struct_setter(Rc::clone(&info), index)),
                span,
            )?;
        }
        Ok(())
    }

    /// The compiled function declared at `id`. Inside an instance's body, a
    /// function that compiles the same in every instance, not using any of
    /// the instance's picks, is the one the first of them compiled.
    fn prototype(
        &self,
        id: NodeId,
        name: Option<String>,
        body: ExpressionLocation,
    ) -> Result<Rc<CompiledFunction>, CompileError> {
        let (compiled, used_instance) = self.compile_function_body(id, name, body, None)?;
        if self.instance.is_none() {
            let compiled = Rc::new(compiled);
            self.prototypes
                .borrow_mut()
                .insert(id, Rc::clone(&compiled));
            return Ok(compiled);
        }
        if used_instance {
            self.used_instance.set(true);
            return Ok(Rc::new(compiled));
        }
        let prototype = self
            .prototypes
            .borrow_mut()
            .entry(id)
            .or_insert_with(|| Rc::new(compiled))
            .clone();
        Ok(prototype)
    }

    /// Compiles the body of the function declared at `id`, with the picks
    /// of `instance` if it's an instance of a generic function. The
    /// functions it declares are compiled with them too. Also returns
    /// whether the body used a pick of the instance it's compiled in.
    fn compile_function_body(
        &self,
        id: NodeId,
        name: Option<String>,
        body: ExpressionLocation,
        instance: Option<InstanceAt>,
    ) -> Result<(CompiledFunction, bool), CompileError> {
        // An instance, and a function its body declares, takes what the
        // instance's types make it take. Only a type that differs from the
        // function's own makes a declared function the instance's, which
        // makes the code around it depend on the instance too: otherwise
        // it's shared, and stays one function across instances.
        let shared = self.analysis.function_type(id);
        let static_type = match instance {
            Some(at) => self.analysis.instances[&at.declaration][at.index]
                .function_types
                .get(&id)
                .unwrap_or(shared),
            None => self
                .instance
                .and_then(|at| {
                    self.analysis.instances[&at.declaration][at.index]
                        .function_types
                        .get(&id)
                })
                .filter(|typ| *typ != shared)
                .and_then(|_| self.instance_pick(|instance| instance.function_types.get(&id)))
                .unwrap_or(shared),
        }
        .clone();
        let locals = self.analysis.frame_size(id);
        if locals > WINDOW {
            return Err(CompileError::too_many_registers(body.span));
        }
        let mut compiler = self.nested(locals, instance.or(self.instance));
        let src = match compiler.compile_expr(body, Dest::Any)? {
            Operand::Local(reg) | Operand::Temp(reg) => reg,
            Operand::Placed => unreachable!("internal error: Dest::Any gives a register"),
        };
        compiler.emit(Instr::Return { src }, Span::synthetic());
        let used_instance = compiler.used_instance.get();
        Ok((
            compiler.finish(name, static_type, 0, Vec::new()),
            used_instance,
        ))
    }

    /// What the identifier `id` refers to, in the instance being compiled.
    fn binding(&self, id: NodeId) -> Binding {
        self.instance_pick(|instance| instance.bindings.get(&id))
            .unwrap_or_else(|| self.analysis.binding(id))
            .clone()
    }

    /// The pick `pick` finds in the instance being compiled, if any, noting
    /// that the code depends on the instance.
    fn instance_pick<'a, T>(
        &'a self,
        pick: impl FnOnce(&'a Instance) -> Option<&'a T>,
    ) -> Option<&'a T> {
        let found = self
            .instance
            .map(|at| &self.analysis.instances[&at.declaration][at.index])
            .and_then(pick);
        if found.is_some() {
            self.used_instance.set(true);
        }
        found
    }

    /// The function that reads through the target at `id`, if it reads, in
    /// the instance being compiled: the getter of `p.x`.
    fn getter(&self, id: NodeId) -> Option<Binding> {
        self.instance_pick(|instance| instance.getters.get(&id))
            .or_else(|| self.analysis.getter(id))
            .cloned()
    }
}

/// The `u16` a closure's captured variable `slot` is written as in an
/// instruction.
fn upvalue(slot: usize, span: Span) -> Result<u16, CompileError> {
    u16::try_from(slot).map_err(|_too_many| CompileError::too_many_registers(span))
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
