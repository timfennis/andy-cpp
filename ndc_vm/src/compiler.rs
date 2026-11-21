//! Compiles analysed code to the VM's instructions.
//!
//! Each expression is compiled with a `Dest`: where its value has to go.
//! `let x = a + b` compiles `a + b` with `x`'s register as its destination,
//! which is one instruction, `AddInt x, a, b`. A local variable is its
//! register; temporaries take the registers after the locals, a watermark
//! that goes up while an expression needs them and back down when it's done.
//!
//! A call's arguments go in consecutive registers after the one the function
//! is in, and become the callee's first registers. An argument register is
//! the call's own copy: `f(x)` copies the local `x` into it rather than
//! handing over `x`'s register, as a native may change a value nothing else
//! holds in place, the way `a ++ b` appends to `a`'s list. For the same
//! reason an instruction that takes its operand, leaving `None` behind (one
//! that stores it somewhere else, as `MakeList` or `SetGlobal`), is only
//! ever given a temporary.
//!
//! A call doesn't clear the callee's registers, so the code compiled here
//! writes each local before reading it. A parameter is written by the call.
//! Any other local is written by its declaration, where it runs: a `let`, a
//! named `fn` and a `struct` are only ever statements, and a statement runs
//! before anything after it in its block, which is all that can read the
//! name. A loop's variables, and a comprehension's, are written before each
//! run of its body or guard.

use crate::chunk::{Builder, Label};
use crate::instr::{Instr, Reg, WINDOW};
use crate::native_op::NativeOp;
use crate::value::{CompiledFunction, Function};
use crate::{Natives, Value};
use ndc_analysis::{Analysis, Binding, Callee, Candidate, CaptureSource, Instance, ResolvedVar};
use ndc_core::collections::{HashMap, HashSet};
use ndc_core::{Intrinsic, NodeId, StaticType};
use ndc_lexer::{NumericLiteral, Span};
use ndc_parser::{
    AssignmentTarget, AssignmentTargetLocation, BindingPattern, BindingPatternLocation, Expression,
    ExpressionLocation, ForBody, ForIteration, LogicalOperator,
};
use std::cell::{Cell, RefCell};
use std::rc::Rc;

/// How many elements of a list literal are made at once from registers of
/// their own; the ones after them are added one by one, so a long list takes
/// no more registers than a short one.
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
    /// As [`Self::Any`], for an operand of an instruction that only reads
    /// it, as `==` does: a string literal there is the constant itself, as
    /// nothing can store or change it, instead of a new string.
    Read,
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

/// An argument of a call or an operand of an instruction, which is
/// compiled in its turn among the others: an expression, or what an
/// assignment with an operator has already worked out about its target, as
/// `xs[i] += 1` reads `xs[i]` with a cached `xs` and `i`.
enum Arg {
    Expr(ExpressionLocation),
    /// The value of a variable, read at `span`.
    Var {
        variable: ResolvedVar,
        span: Span,
    },
    /// The value in a temporary that stays in use, which a call copies.
    Copy(Reg),
    /// The value in a temporary that nothing reads after the call: the
    /// value `xs[i] += 1` stores. An instruction that stores it takes it
    /// from there; anything else reads it as it reads a [`Self::Copy`].
    Take(Reg),
    /// What the call of `getter` with copies of the values in `args` gives,
    /// at `span`: the read of `xs[i]` or `p.x`.
    Read {
        getter: Binding,
        args: Vec<Reg>,
        span: Span,
    },
}

/// The target of an assignment with an operator, with the parts it's read
/// and written through worked out: `xs[i] += 1` reads and writes `xs[i]`
/// with `xs` and `i` in temporaries.
enum PreparedTarget {
    Variable {
        variable: ResolvedVar,
        span: Span,
    },
    Index {
        container: Reg,
        index: Reg,
        /// The target's span, from the container to the index.
        span: Span,
        getter: Binding,
        setter: Binding,
    },
    Member {
        receiver: Reg,
        /// The member's span.
        span: Span,
        getter: Binding,
        setter: Binding,
    },
}

/// The labels of the loop being compiled, which `break` and `continue` jump
/// to.
#[derive(Clone, Copy)]
struct Loop {
    start: Label,
    end: Label,
}

impl Arg {
    /// Where the argument is in the source.
    fn span(&self) -> Span {
        match self {
            Self::Expr(expression) => expression.span,
            Self::Var { span, .. } | Self::Read { span, .. } => *span,
            Self::Copy(_) | Self::Take(_) => Span::synthetic(),
        }
    }

    /// Whether compiling the argument may write a local variable (see
    /// [`may_assign`]).
    fn may_assign(&self) -> bool {
        match self {
            Self::Expr(expression) => may_assign(expression),
            Self::Var { .. } | Self::Copy(_) | Self::Take(_) | Self::Read { .. } => false,
        }
    }
}

/// The arguments `expressions`, which are expressions.
fn exprs(expressions: Vec<ExpressionLocation>) -> Vec<Arg> {
    expressions.into_iter().map(Arg::Expr).collect()
}

/// What compiling the earlier lines of a REPL session leaves for the lines
/// after them, which can call the functions those declared with new types.
/// After `fn inc(x) => x + 1`, the line `inc(2.5)` compiles the instance
/// `inc<Float>` from `inc`'s body and what the analyser worked out about it.
#[derive(Default)]
pub struct Session {
    /// What the analyser worked out about every line so far.
    analysis: Rc<Analysis>,
    /// The functions the lines declared in globals, by declaration.
    functions: HashMap<NodeId, DeclaredFunction>,
    /// As [`Compiler::prototypes`], for every line so far.
    prototypes: Rc<RefCell<HashMap<NodeId, Rc<CompiledFunction>>>>,
}

/// A function declaration a REPL line made, which a later line may compile
/// instances of.
#[derive(Clone)]
struct DeclaredFunction {
    name: Option<String>,
    body: ExpressionLocation,
    pure: bool,
    span: Span,
}

/// Compiles one function.
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
    /// The functions the code compiled so far declared in globals, kept when
    /// compiling a REPL line, whose later lines may need instances of them.
    functions: Option<HashMap<NodeId, DeclaredFunction>>,
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
    /// Compiles a program into a function the VM runs at the top level.
    pub fn compile(
        expressions: impl Iterator<Item = ExpressionLocation>,
        analysis: Analysis,
        natives: &Natives,
    ) -> Result<CompiledFunction, CompileError> {
        let mut compiler = Self::top_level(Rc::new(analysis), natives, Rc::default(), None);
        compiler.compile_statements(expressions)?;
        compiler.finish_program()
    }

    /// Compiles one line of a REPL session, `analysis` being what the
    /// analyser worked out about it, and keeps in `session` what later lines
    /// need. The line starts by storing the instances it made of earlier
    /// lines' functions.
    pub fn compile_line(
        expressions: impl Iterator<Item = ExpressionLocation>,
        analysis: Analysis,
        natives: &Natives,
        session: &mut Session,
    ) -> Result<CompiledFunction, CompileError> {
        Rc::make_mut(&mut session.analysis).extend(analysis);
        let mut compiler = Self::top_level(
            Rc::clone(&session.analysis),
            natives,
            Rc::clone(&session.prototypes),
            Some(std::mem::take(&mut session.functions)),
        );
        let result = compiler
            .compile_earlier_instances()
            .and_then(|()| compiler.compile_statements(expressions));
        session.functions = compiler.functions.take().unwrap_or_default();
        result?;
        compiler.finish_program()
    }

    /// A compiler for code run at the top level, which `analysis` is about.
    /// `functions` keeps the functions it declares in globals, for a REPL
    /// line, whose later lines may make instances of them.
    fn top_level(
        analysis: Rc<Analysis>,
        natives: &Natives,
        prototypes: Rc<RefCell<HashMap<NodeId, Rc<CompiledFunction>>>>,
        functions: Option<HashMap<NodeId, DeclaredFunction>>,
    ) -> Self {
        let locals = analysis.top_level_frame_size;
        Self {
            builder: Builder::default(),
            analysis,
            instance: None,
            used_instance: Cell::new(false),
            prototypes,
            functions,
            natives: Rc::default(),
            native_ops: natives.ops(),
            compiled_instances: HashSet::default(),
            locals,
            next_temp: locals,
            frame_size: locals,
            loops: Vec::new(),
        }
    }

    /// Stores the instances the line made of functions earlier lines
    /// declared, each its function's body compiled with the instance's
    /// picks, as [`Self::compile_function_declaration`] stores a function's
    /// own.
    fn compile_earlier_instances(&mut self) -> Result<(), CompileError> {
        let analysis = Rc::clone(&self.analysis);
        let earlier = analysis
            .instances
            .iter()
            .filter_map(|(declaration, instances)| {
                let function = self.functions.as_ref()?.get(declaration)?;
                Some((*declaration, function.clone(), instances))
            })
            .collect::<Vec<_>>();
        for (declaration, function, instances) in earlier {
            for (index, instance) in instances.iter().enumerate() {
                let at = InstanceAt { declaration, index };
                let (body, _) = self.compile_function_body(
                    declaration,
                    function.name.clone(),
                    function.body.clone(),
                    Some(at),
                )?;
                self.store_function(
                    Rc::new(body),
                    &[],
                    function.pure,
                    instance.variable,
                    function.span,
                )?;
            }
            self.compiled_instances.insert(declaration);
        }
        Ok(())
    }

    /// Compiles the program's `expressions`, then `Halt` with the value of
    /// the last one that isn't a `;`-statement.
    fn compile_statements(
        &mut self,
        expressions: impl Iterator<Item = ExpressionLocation>,
    ) -> Result<(), CompileError> {
        let expressions = expressions.collect::<Vec<_>>();
        // The program's value is its last expression's that isn't a
        // `;`-statement, kept in a register of its own while the statements
        // after it run.
        let last_value = expressions
            .iter()
            .rposition(|expression| !matches!(expression.expression, Expression::Statement(_)));
        let result = self.alloc_temp(Span::synthetic())?;
        for (index, expression) in expressions.into_iter().enumerate() {
            let dest = if Some(index) == last_value {
                Dest::Reg(result)
            } else {
                Dest::Discard
            };
            self.compile_expr(expression, dest)?;
        }
        if last_value.is_none() {
            self.emit(Instr::LoadUnit { dst: result }, Span::synthetic());
        }
        self.emit(Instr::Halt { src: result }, Span::synthetic());
        Ok(())
    }

    /// The program compiled so far, as a function the VM runs at the top
    /// level.
    fn finish_program(self) -> Result<CompiledFunction, CompileError> {
        // Only `compile_line` makes instances of functions declared before
        // the code it compiles.
        if self
            .analysis
            .instances
            .keys()
            .any(|declaration| !self.compiled_instances.contains(declaration))
        {
            return Err(CompileError::uncompiled_instances());
        }
        let mut natives = self.natives.take().into_iter().collect::<Vec<_>>();
        natives.sort_unstable();
        let num_globals = self.analysis.global_count;
        Ok(self.finish(
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
            functions: None,
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
            num_globals,
            natives,
            body: self.builder.finish(),
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
            Dest::Any | Dest::Read | Dest::Discard => self.alloc_temp(span),
        }
    }

    /// What an expression compiled with `dest` that wrote its value to
    /// `reg`, which [`Self::place`] gave, returns.
    fn placed(dest: Dest, reg: Reg) -> Operand {
        match dest {
            Dest::Any | Dest::Read => Operand::Temp(reg),
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
        Ok(self.settle(mark, operand, dest))
    }

    /// Frees the temporaries an expression compiled with `dest` from `mark`
    /// on took, but for the one its value is in, which is the first.
    fn settle(&mut self, mark: usize, operand: Operand, dest: Dest) -> Operand {
        match operand {
            Operand::Temp(reg) => {
                debug_assert!(matches!(dest, Dest::Any | Dest::Read));
                debug_assert_eq!(
                    reg.index(),
                    mark,
                    "internal error: a value is in the first free temporary"
                );
                self.next_temp = mark + 1;
            }
            Operand::Local(_) | Operand::Placed => self.next_temp = mark,
        }
        operand
    }

    /// Compiles `arg`, putting its value where `dest` says, as
    /// [`Self::compile_expr`] does.
    fn compile_arg(&mut self, arg: Arg, dest: Dest) -> Result<Operand, CompileError> {
        let mark = self.next_temp;
        let operand = match arg {
            Arg::Expr(expression) => return self.compile_expr(expression, dest),
            Arg::Var { variable, span } => self.read_variable(variable, dest, span)?,
            // The register stays in use, so the value is read where it is.
            Arg::Copy(reg) | Arg::Take(reg) => match dest {
                Dest::Any | Dest::Read => Operand::Local(reg),
                Dest::Reg(dst) => {
                    if dst != reg {
                        self.emit(Instr::Copy { dst, src: reg }, Span::synthetic());
                    }
                    Operand::Placed
                }
                Dest::Discard => Operand::Placed,
            },
            Arg::Read { getter, args, span } => {
                let args = args.into_iter().map(Arg::Copy).collect();
                self.compile_binding_call(&getter, args, dest, span)?
            }
        };
        Ok(self.settle(mark, operand, dest))
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
                if dest == Dest::Read {
                    return self.load_constant(Value::string(text), dest, span);
                }
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
                self.compile_callee(&binding, Some(id), dest, span)
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
                self.compile_assignment(l_value, *r_value, dest, span)
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
            Expression::OpAssignment {
                l_value, r_value, ..
            } => {
                self.compile_op_assignment(id, l_value, *r_value, span)?;
                self.unit(dest, span)
            }
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
                self.compile_binding_call(&getter, vec![Arg::Expr(*receiver)], dest, member_span)
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
                    Dest::Any | Dest::Read => Ok(Operand::Local(reg)),
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
            Dest::Any | Dest::Read => {
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

    /// Compiles `condition` and a jump to `target` taken when it's false.
    fn jump_unless(
        &mut self,
        condition: ExpressionLocation,
        target: Label,
    ) -> Result<(), CompileError> {
        self.jump_when(condition, false, target)
    }

    /// Compiles `condition` and a jump to `target` taken when it's `when`,
    /// as jumps that make no `Bool` where they can. A comparison of two
    /// values is one instruction that compares and jumps: `i < n` is
    /// `JumpIfNotLess i, n`. `and`, `or` and `!` are jumps on their
    /// operands: `if a and b` goes past its branch when `a` is false,
    /// without running `b`.
    fn jump_when(
        &mut self,
        condition: ExpressionLocation,
        when: bool,
        target: Label,
    ) -> Result<(), CompileError> {
        let mark = self.next_temp;
        let ExpressionLocation {
            id,
            expression,
            span,
        } = condition;
        match expression {
            Expression::Grouping(inner) => return self.jump_when(*inner, when, target),
            Expression::BoolLiteral(value) => {
                if value == when {
                    self.builder.jump(Instr::Jump { offset: 0 }, target, span);
                }
            }
            Expression::Logical {
                left,
                operator,
                right,
            } => {
                // `a and b` is false as soon as `a` is, and `a or b` true as
                // soon as `a` is.
                let settled_by_left = match operator {
                    LogicalOperator::And => false,
                    LogicalOperator::Or => true,
                };
                if when == settled_by_left {
                    self.jump_when(*left, when, target)?;
                    self.jump_when(*right, when, target)?;
                } else {
                    let skip = self.builder.label();
                    self.jump_when(*left, settled_by_left, skip)?;
                    self.jump_when(*right, when, target)?;
                    self.builder.bind(skip);
                }
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
            } if self.condition_op(&function, arguments.len()).is_some() => {
                let op = self
                    .condition_op(&function, arguments.len())
                    .expect("internal error: the guard found the op");
                if op == NativeOp::Not {
                    let Ok([operand]) = <[_; 1]>::try_from(arguments) else {
                        unreachable!("internal error: `!` takes one operand");
                    };
                    return self.jump_when(operand, !when, target);
                }
                // A comparison that has to hold to jump jumps over a jump
                // to `target` when it doesn't.
                let skip = when.then(|| self.builder.label());
                let jump = match self.compile_binary_operands(exprs(arguments), true)? {
                    (left, Right::Reg(right)) => Instr::jump_unless(op, left, right),
                    (left, Right::Immediate(imm)) => Instr::jump_unless_immediate(op, left, imm),
                }
                .expect("internal error: a comparison has a compare-and-branch");
                self.builder
                    .jump(jump, skip.unwrap_or(target), function.span);
                if let Some(skip) = skip {
                    self.builder.jump(Instr::Jump { offset: 0 }, target, span);
                    self.builder.bind(skip);
                }
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
                let jump = if when {
                    Instr::JumpIfTrue { cond, offset: 0 }
                } else {
                    Instr::JumpIfFalse { cond, offset: 0 }
                };
                self.builder.jump(jump, target, span);
            }
        }
        // The jump has read its operands.
        self.next_temp = mark;
        Ok(())
    }

    /// The op a call of `function` with `args` arguments does, if it
    /// resolves to one a condition jumps on without making a `Bool`: a
    /// comparison, as `<` of two Ints, or `!` of a Bool.
    fn condition_op(&self, function: &ExpressionLocation, args: usize) -> Option<NativeOp> {
        let Expression::Identifier { .. } = function.expression else {
            return None;
        };
        match self.native_call(&self.binding(function.id))? {
            NativeCall::Op(NativeOp::Not) if args == 1 => Some(NativeOp::Not),
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
            if let Binding::Resolved(Candidate::Scalar(Callee::Intrinsic(intrinsic))) = binding {
                return self.compile_intrinsic_call(
                    intrinsic,
                    function.id,
                    exprs(arguments),
                    dest,
                    span,
                );
            }
            return self.compile_binding_call(&binding, exprs(arguments), dest, span);
        }
        let base = self.alloc_temp(span)?;
        self.compile_expr(function, Dest::Reg(base))?;
        self.finish_call(base, exprs(arguments), false, dest, span)
    }

    /// Compiles the call at `span` of what `binding` refers to with
    /// `arguments`: the instruction of the op it resolves to, as `SubInt`
    /// for `n - 1`, a `CallNative` of the native it resolves to, a `CallVec`
    /// of an operator applied to each element of tuples, or a `Call` of the
    /// function value.
    fn compile_binding_call(
        &mut self,
        binding: &Binding,
        arguments: Vec<Arg>,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        // A binding no name refers to, as a getter's, isn't an intrinsic.
        if let Binding::Resolved(Candidate::Scalar(Callee::Intrinsic(_))) = binding {
            return Err(CompileError::unnamed_intrinsic(span));
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
        self.compile_callee(binding, None, Dest::Reg(base), span)?;
        self.finish_call(base, arguments, vectorised, dest, span)
    }

    /// Puts the value of what `binding`, named at `site` if a name refers
    /// to it, refers to where `dest` says: the variable's value, or for an
    /// operator whose tuple positions take different overloads, the tuple
    /// of them, which `CallVec` applies position by position.
    fn compile_callee(
        &mut self,
        binding: &Binding,
        site: Option<NodeId>,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        match binding {
            Binding::None => Err(CompileError::unresolved_binding(span)),
            Binding::Resolved(candidate) => self.callee_value(candidate.var(), site, dest, span),
            Binding::PerPosition(overloads) => {
                let mark = self.next_temp;
                let base = Self::register(mark, span)?;
                for &overload in overloads {
                    let reg = self.alloc_temp(span)?;
                    self.callee_value(overload, site, Dest::Reg(reg), span)?;
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

    /// Puts `callee`, named at `site`, where `dest` says as a function
    /// value. An intrinsic is stored nowhere, so it becomes a small function
    /// around its instruction: `cast::<Int>` in `xs.map(cast::<Int>)` is
    /// `fn(x) => cast::<Int>(x)`.
    fn callee_value(
        &mut self,
        callee: Callee,
        site: Option<NodeId>,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        match (callee, site) {
            (Callee::Var(variable), _) => self.read_variable(variable, dest, span),
            (Callee::Intrinsic(intrinsic), Some(site)) => {
                let function = self.intrinsic_function(intrinsic, site, span)?;
                let value = Value::function(Function::Compiled(Rc::new(function)));
                self.load_constant(value, dest, span)
            }
            (Callee::Intrinsic(_), None) => Err(CompileError::unnamed_intrinsic(span)),
        }
    }

    /// The function value of `intrinsic` named at `site`: the intrinsic's
    /// instruction on its parameter, and a return. It's compiled in the
    /// instance being compiled, so what it reads of the instance's picks is
    /// that instance's.
    fn intrinsic_function(
        &self,
        intrinsic: Intrinsic,
        site: NodeId,
        span: Span,
    ) -> Result<CompiledFunction, CompileError> {
        let arity = intrinsic.arity();
        let static_type = self
            .analysis
            .expr_types
            .get(&site)
            .cloned()
            .unwrap_or_else(|| StaticType::Function {
                parameters: Some(vec![StaticType::Any; arity]),
                return_type: Box::new(StaticType::Any),
            });
        let mut compiler = self.nested(arity, self.instance);
        let src = Reg(0);
        match intrinsic {
            Intrinsic::Cast | Intrinsic::TryCast => {
                compiler.emit_cast(site, intrinsic == Intrinsic::TryCast, src, span)?;
            }
            // Only a read of `[]` with a number written in the code calls
            // it, which `Self::compile_intrinsic_call` writes.
            Intrinsic::TupleIndex => return Err(CompileError::unnamed_intrinsic(span)),
        }
        compiler.emit(Instr::Return { src }, Span::synthetic());
        if compiler.used_instance.get() {
            self.used_instance.set(true);
        }
        Ok(compiler.finish(None, static_type, 0, Vec::new()))
    }

    /// Compiles the call at `span` of `intrinsic`, named at `site`, with
    /// `arguments`. The tuple index takes its index, a number written in the
    /// code, as the instruction's operand: `t[1]` is `TupleIndex` of `t`'s
    /// register and 1. A cast checks its argument in the register it's put
    /// in, which is the call's own.
    fn compile_intrinsic_call(
        &mut self,
        intrinsic: Intrinsic,
        site: NodeId,
        arguments: Vec<Arg>,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        if intrinsic == Intrinsic::TupleIndex {
            let [tuple, Arg::Expr(index)] = <[_; 2]>::try_from(arguments)
                .ok()
                .expect("internal error: the tuple index takes a tuple and an index")
            else {
                unreachable!("internal error: the tuple index's index is an expression");
            };
            let index = index.integer_literal().expect(
                "internal error: the analyser picks the tuple index for a number written in the code",
            );
            let index =
                i16::try_from(index).map_err(|_too_large| CompileError::index_too_large(span))?;
            let mark = self.next_temp;
            let src = match self.compile_arg(tuple, Dest::Any)? {
                Operand::Local(reg) | Operand::Temp(reg) => reg,
                Operand::Placed => unreachable!("internal error: Dest::Any gives a register"),
            };
            // `TupleIndex` reads the tuple before it writes `dst`.
            self.next_temp = mark;
            let dst = self.place(dest, span)?;
            self.emit(Instr::TupleIndex { dst, src, index }, span);
            return Ok(Self::placed(dest, dst));
        }
        let [value] = <[_; 1]>::try_from(arguments)
            .ok()
            .expect("internal error: a cast takes one value");
        let dst = match dest {
            Dest::Reg(reg) => reg,
            Dest::Any | Dest::Read | Dest::Discard => self.alloc_temp(span)?,
        };
        self.compile_arg(value, Dest::Reg(dst))?;
        self.emit_cast(site, intrinsic == Intrinsic::TryCast, dst, span)?;
        Ok(Self::placed(dest, dst))
    }

    /// Writes the cast at `site` of the value in `reg`, in place: a check
    /// that it has the type the analyser picked for the instance being
    /// compiled, or for `optional`, `Some` of the value if it has it and
    /// `None` if not.
    fn emit_cast(
        &mut self,
        site: NodeId,
        optional: bool,
        reg: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        let check = match self
            .instance_pick(|instance| instance.cast_checks.get(&site))
            .or_else(|| self.analysis.cast_checks.get(&site))
        {
            None => return Err(CompileError::unresolved_cast(span)),
            Some(check) => check.clone(),
        };
        let instr = match (check, optional) {
            (None, false) => return Ok(()),
            (Some(target), false) => Instr::CheckType {
                reg,
                typ: self.builder.static_type(target),
            },
            // A value proven to be the type is always `Some`, as every
            // value is an `Any`.
            (None, true) => Instr::TryCast {
                reg,
                typ: self.builder.static_type(StaticType::Any),
            },
            (Some(target), true) => Instr::TryCast {
                reg,
                typ: self.builder.static_type(target),
            },
        };
        self.emit(instr, span);
        Ok(())
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
        arguments: Vec<Arg>,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        debug_assert_eq!(
            op.arity(),
            arguments.len(),
            "the analyser picks an overload by arity"
        );
        if op == NativeOp::ListSet {
            return self.compile_list_set(arguments, dest, span);
        }
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

    /// Compiles `xs[i] = v`, the call of [`NativeOp::ListSet`] with the
    /// list, the index and the value, into a `ListSet` that reads the list
    /// and the index where they are and takes the value: from an
    /// [`Arg::Take`]'s register, or from a temporary of its own, which a
    /// local's value is copied into.
    fn compile_list_set(
        &mut self,
        mut arguments: Vec<Arg>,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        let mark = self.next_temp;
        let taken = match arguments.last() {
            Some(&Arg::Take(reg)) => {
                arguments.pop();
                Some(reg)
            }
            _ => None,
        };
        // `ListSet` reads the list and the index and takes the value.
        let operands = self.compile_operands_where(arguments, taken.is_none())?;
        let reg = |operand: &Operand| match *operand {
            Operand::Local(reg) | Operand::Temp(reg) => reg,
            Operand::Placed => unreachable!("internal error: Dest::Any gives a register"),
        };
        let value = match (taken, operands.get(2)) {
            (Some(reg), None) | (None, Some(&Operand::Temp(reg))) => reg,
            (None, Some(&Operand::Local(src))) => {
                let dst = self.alloc_temp(span)?;
                self.emit(Instr::Copy { dst, src }, span);
                dst
            }
            _ => unreachable!("internal error: ListSet takes three operands"),
        };
        self.emit(
            Instr::ListSet {
                list: reg(&operands[0]),
                index: reg(&operands[1]),
                value,
            },
            span,
        );
        self.next_temp = mark;
        self.unit(dest, span)
    }

    /// Compiles the two operands of a binary op: two registers, or with
    /// `immediate`, a register and the `Int` on the right when it's a
    /// number written in the code that fits an `i16`, as `2` in `i * 2`.
    fn compile_binary_operands(
        &mut self,
        arguments: Vec<Arg>,
        immediate: bool,
    ) -> Result<(Reg, Right), CompileError> {
        if immediate
            && let Arg::Expr(right) = &arguments[1]
            && let Some(imm) = self.small_int(right)
        {
            let [left, _] = <[_; 2]>::try_from(arguments)
                .ok()
                .expect("internal error: a binary op takes two operands");
            let left = match self.compile_arg(left, Dest::Read)? {
                Operand::Local(reg) | Operand::Temp(reg) => reg,
                Operand::Placed => unreachable!("internal error: Dest::Read gives a register"),
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
    fn compile_operands(&mut self, arguments: Vec<Arg>) -> Result<Vec<Reg>, CompileError> {
        Ok(self
            .compile_operands_where(arguments, false)?
            .into_iter()
            .map(|operand| match operand {
                Operand::Local(reg) | Operand::Temp(reg) => reg,
                Operand::Placed => unreachable!("internal error: Dest::Any gives a register"),
            })
            .collect())
    }

    /// [`Self::compile_operands`] for an instruction that reads its
    /// operands, or with `takes_last`, takes its last one, saying of each
    /// register whether it's a local's own, which the instruction may only
    /// read, or a temporary. Only the last operand of an instruction that
    /// takes it is compiled to a temporary it may take; the others are
    /// compiled with [`Dest::Read`].
    fn compile_operands_where(
        &mut self,
        arguments: Vec<Arg>,
        takes_last: bool,
    ) -> Result<Vec<Operand>, CompileError> {
        let last = arguments.len().saturating_sub(1);
        let mut assigned_later = arguments
            .iter()
            .rev()
            .scan(false, |later, argument| {
                let before = *later;
                *later |= argument.may_assign();
                Some(before)
            })
            .collect::<Vec<_>>();
        assigned_later.reverse();
        let mut operands = Vec::with_capacity(arguments.len());
        for (i, (argument, assigned_later)) in arguments.into_iter().zip(assigned_later).enumerate()
        {
            let span = argument.span();
            let dest = if takes_last && i == last {
                Dest::Any
            } else {
                Dest::Read
            };
            let reg = match self.compile_arg(argument, dest)? {
                Operand::Local(reg) if assigned_later => {
                    let temp = self.alloc_temp(span)?;
                    self.emit(
                        Instr::Copy {
                            dst: temp,
                            src: reg,
                        },
                        span,
                    );
                    Operand::Temp(temp)
                }
                operand @ (Operand::Local(_) | Operand::Temp(_)) => operand,
                Operand::Placed => unreachable!("internal error: Dest::Any gives a register"),
            };
            operands.push(reg);
        }
        Ok(operands)
    }

    /// Declares the variables of `pattern`, which a declaration or a `for`
    /// loop at `span` declares, with the value in `src`: a sequence
    /// pattern's parts with the elements of the value, which `Unpack` puts
    /// straight in the variables' registers when they follow each other, and
    /// in temporaries first otherwise.
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
                self.store_from(variable, src, true, span)
            }
            BindingPattern::Sequence(patterns) => {
                let len = u16::try_from(patterns.len())
                    .map_err(|_too_many| CompileError::too_many_registers(span))?;
                // Variables in registers one after the other get their
                // elements straight away: `for (i, x) in xs` unpacks into
                // `i` and `x`.
                if let Some(base) = self.declared_registers(&patterns) {
                    self.emit(Instr::Unpack { src, base, len }, span);
                    return Ok(());
                }
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

    /// The register a declaration of `pattern` writes its value to, if it's
    /// one variable whose value is its register's: a local, or a local that
    /// closures capture, whose declaration writes its register (see
    /// [`Self::store`]).
    fn declared_register(&self, pattern: &BindingPatternLocation) -> Option<Reg> {
        let BindingPattern::Identifier { .. } = pattern.pattern else {
            return None;
        };
        match self.analysis.variable(pattern.id)? {
            ResolvedVar::Local { slot } | ResolvedVar::Cell { slot } => {
                Self::register(slot, pattern.span).ok()
            }
            ResolvedVar::Global { .. } | ResolvedVar::Upvalue { .. } => None,
        }
    }

    /// The first of the registers a declaration of `patterns` writes the
    /// elements of a value to, if each is one variable's register (see
    /// [`Self::declared_register`]) and they follow each other: `(a, b)` of
    /// the locals in `r3` and `r4` is `r3`.
    fn declared_registers(&self, patterns: &[BindingPatternLocation]) -> Option<Reg> {
        let first = self.declared_register(patterns.first()?)?;
        patterns
            .iter()
            .enumerate()
            .all(|(index, pattern)| self.declared_register(pattern) == first.offset(index))
            .then_some(first)
    }

    /// Stores the value in `src` in `variable`, which a declaration, when
    /// `declaring`, or an assignment at `span` writes, as [`Self::store`]
    /// does. The value is taken if `src` is a temporary, and copied if it's
    /// a local's register.
    fn store_from(
        &mut self,
        variable: ResolvedVar,
        src: Reg,
        declaring: bool,
        span: Span,
    ) -> Result<(), CompileError> {
        if let ResolvedVar::Local { slot } = variable {
            let dst = self.local(slot, span)?;
            if dst != src {
                let copy = if src.index() >= self.locals {
                    Instr::Move { dst, src }
                } else {
                    Instr::Copy { dst, src }
                };
                self.emit(copy, span);
            }
            return Ok(());
        }
        if let ResolvedVar::Cell { slot } = variable
            && declaring
        {
            return self.store_from(ResolvedVar::Local { slot }, src, declaring, span);
        }
        // The rest take the value, which a local keeps.
        let mark = self.next_temp;
        let src = if src.index() >= self.locals {
            src
        } else {
            let temp = self.alloc_temp(span)?;
            self.emit(Instr::Copy { dst: temp, src }, span);
            temp
        };
        let instr = match variable {
            ResolvedVar::Cell { slot } => Instr::SetCell {
                cell: self.local(slot, span)?,
                src,
            },
            ResolvedVar::Upvalue { slot } => Instr::SetUpvalue {
                index: upvalue(slot, span)?,
                src,
            },
            ResolvedVar::Global { slot } => Instr::SetGlobal {
                slot: global(slot),
                src,
            },
            ResolvedVar::Local { .. } => unreachable!("internal error: a local is stored above"),
        };
        self.emit(instr, span);
        self.next_temp = mark;
        Ok(())
    }

    /// Compiles the assignment of `value` to `target`. Through an index or
    /// a member, it's a call of the target's setter, whose result is the
    /// assignment's value: `xs[i] = v` is `[]=(xs, i, v)`. Otherwise it's
    /// unit.
    fn compile_assignment(
        &mut self,
        target: AssignmentTargetLocation,
        value: ExpressionLocation,
        dest: Dest,
        span: Span,
    ) -> Result<Operand, CompileError> {
        match target.target {
            AssignmentTarget::Identifier { .. } => {
                let variable = self.analysis.variable(target.id).expect(
                    "internal error: every assignment target is analysed before it's compiled",
                );
                self.store(variable, value, false, span)?;
            }
            AssignmentTarget::Index {
                value: container,
                index,
            } => {
                let setter = self.setter(target.id);
                let args = vec![Arg::Expr(*container), Arg::Expr(*index), Arg::Expr(value)];
                return self.compile_binding_call(&setter, args, dest, span);
            }
            AssignmentTarget::Member {
                receiver,
                member_span,
                ..
            } => {
                let setter = self.setter(target.id);
                let args = vec![Arg::Expr(*receiver), Arg::Expr(value)];
                return self.compile_binding_call(&setter, args, dest, member_span);
            }
            AssignmentTarget::Sequence(targets) => {
                // The whole value is read before any target is assigned.
                let mark = self.next_temp;
                let src = match self.compile_expr(value, Dest::Any)? {
                    Operand::Local(reg) | Operand::Temp(reg) => reg,
                    Operand::Placed => unreachable!("internal error: Dest::Any gives a register"),
                };
                self.assign_sequence(targets, src, span)?;
                self.next_temp = mark;
            }
        }
        self.unit(dest, span)
    }

    /// Assigns the elements of the value in `src` to `targets`, from the
    /// left, for the destructuring assignment at `span`.
    fn assign_sequence(
        &mut self,
        targets: Vec<AssignmentTargetLocation>,
        src: Reg,
        span: Span,
    ) -> Result<(), CompileError> {
        let len = u16::try_from(targets.len())
            .map_err(|_too_many| CompileError::too_many_registers(span))?;
        let mark = self.next_temp;
        let base = Self::register(mark, span)?;
        for _ in 0..len {
            self.alloc_temp(span)?;
        }
        self.emit(Instr::Unpack { src, base, len }, span);
        for (index, target) in targets.into_iter().enumerate() {
            let element = base
                .offset(index)
                .expect("internal error: the elements' registers exist");
            match target.target {
                AssignmentTarget::Identifier { .. } => {
                    let variable = self.analysis.variable(target.id).expect(
                        "internal error: every assignment target is analysed before it's compiled",
                    );
                    self.store_from(variable, element, false, span)?;
                }
                AssignmentTarget::Index { value, index } => {
                    let setter = self.setter(target.id);
                    let args = vec![Arg::Expr(*value), Arg::Expr(*index), Arg::Take(element)];
                    self.compile_binding_call(&setter, args, Dest::Discard, span)?;
                }
                AssignmentTarget::Sequence(targets) => {
                    self.assign_sequence(targets, element, span)?;
                }
                AssignmentTarget::Member { .. } => unreachable!(
                    "internal error: the parser rejects members inside destructuring patterns"
                ),
            }
        }
        self.next_temp = mark;
        Ok(())
    }

    /// Compiles the assignment with an operator at `span`, the one `id`
    /// resolves, of `value` to `target`: `x += 1` is `x = x + 1`, with the
    /// parts of an index or member target worked out once, so `xs[f()] += 1`
    /// calls `f` once. The target is read first, then `value`.
    fn compile_op_assignment(
        &mut self,
        id: NodeId,
        target: AssignmentTargetLocation,
        value: ExpressionLocation,
        span: Span,
    ) -> Result<(), CompileError> {
        let mark = self.next_temp;
        let target = self.prepare_target(target)?;
        let Some(operator) = self.augmented_assignment(id) else {
            return Err(CompileError::unresolved_binding(span));
        };
        let read = match &target {
            PreparedTarget::Variable { variable, span } => Arg::Var {
                variable: *variable,
                span: *span,
            },
            PreparedTarget::Index {
                container,
                index,
                span,
                getter,
                ..
            } => Arg::Read {
                getter: getter.clone(),
                args: vec![*container, *index],
                span: *span,
            },
            PreparedTarget::Member {
                receiver,
                span,
                getter,
                ..
            } => Arg::Read {
                getter: getter.clone(),
                args: vec![*receiver],
                span: *span,
            },
        };
        let args = vec![read, Arg::Expr(value)];
        // A local takes the result in its own register; any other target
        // has it stored from a temporary.
        if let PreparedTarget::Variable {
            variable: ResolvedVar::Local { slot },
            span: target_span,
        } = target
        {
            let dst = self.local(slot, target_span)?;
            self.compile_binding_call(&operator, args, Dest::Reg(dst), span)?;
        } else {
            let result = self.alloc_temp(span)?;
            self.compile_binding_call(&operator, args, Dest::Reg(result), span)?;
            self.store_target(target, result)?;
        }
        self.next_temp = mark;
        Ok(())
    }

    /// Works out the parts of `target` that an assignment with an operator
    /// reads and writes it through, each once, into temporaries that stay
    /// taken until the assignment is done.
    fn prepare_target(
        &mut self,
        target: AssignmentTargetLocation,
    ) -> Result<PreparedTarget, CompileError> {
        Ok(match target.target {
            AssignmentTarget::Identifier { span, .. } => PreparedTarget::Variable {
                variable: self.analysis.variable(target.id).expect(
                    "internal error: every assignment target is analysed before it's compiled",
                ),
                span,
            },
            AssignmentTarget::Index { value, index } => {
                let span = value.span.merge(index.span);
                let container = self.alloc_temp(value.span)?;
                self.compile_expr(*value, Dest::Reg(container))?;
                let index_reg = self.alloc_temp(index.span)?;
                self.compile_expr(*index, Dest::Reg(index_reg))?;
                PreparedTarget::Index {
                    container,
                    index: index_reg,
                    span,
                    getter: self.target_getter(target.id),
                    setter: self.setter(target.id),
                }
            }
            AssignmentTarget::Member {
                receiver,
                member_span,
                ..
            } => {
                let reg = self.alloc_temp(receiver.span)?;
                self.compile_expr(*receiver, Dest::Reg(reg))?;
                PreparedTarget::Member {
                    receiver: reg,
                    span: member_span,
                    getter: self.target_getter(target.id),
                    setter: self.setter(target.id),
                }
            }
            AssignmentTarget::Sequence(_) => unreachable!(
                "internal error: name resolution rejects a sequence as an augmented assignment target"
            ),
        })
    }

    /// Stores the value in `src`, a temporary, in `target`.
    fn store_target(&mut self, target: PreparedTarget, src: Reg) -> Result<(), CompileError> {
        match target {
            PreparedTarget::Variable { variable, span } => {
                self.store_from(variable, src, false, span)
            }
            PreparedTarget::Index {
                container,
                index,
                span,
                setter,
                ..
            } => {
                let args = vec![Arg::Copy(container), Arg::Copy(index), Arg::Take(src)];
                self.compile_binding_call(&setter, args, Dest::Discard, span)
                    .map(|_| ())
            }
            PreparedTarget::Member {
                receiver,
                span,
                setter,
                ..
            } => {
                let args = vec![Arg::Copy(receiver), Arg::Copy(src)];
                self.compile_binding_call(&setter, args, Dest::Discard, span)
                    .map(|_| ())
            }
        }
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
                // A loop variable in a register gets each value straight
                // away; a pattern is unpacked from a temporary.
                if let Some(dst) = self.declared_register(l_value) {
                    self.builder.jump(
                        Instr::IterNext {
                            iter,
                            dst,
                            offset: 0,
                        },
                        end,
                        span,
                    );
                } else {
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
                }
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
            Dest::Reg(_) | Dest::Any | Dest::Read | Dest::Discard => self.alloc_temp(left_span)?,
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
            Dest::Any | Dest::Read => Operand::Temp(dst),
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
        let base = self.compile_arguments(exprs(values), span)?;
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
        let base = self.compile_arguments(exprs(values), span)?;
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
            Dest::Any | Dest::Read => Operand::Temp(reg),
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
        let operands = self.compile_operands(exprs(bounds))?;
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
    fn compile_arguments(&mut self, arguments: Vec<Arg>, span: Span) -> Result<Reg, CompileError> {
        let first = Self::register(self.next_temp, span)?;
        let registers = arguments
            .iter()
            .map(|argument| self.alloc_temp(argument.span()))
            .collect::<Result<Vec<_>, _>>()?;
        for (argument, reg) in arguments.into_iter().zip(registers) {
            self.compile_arg(argument, Dest::Reg(reg))?;
        }
        Ok(first)
    }

    /// Compiles the call of the native in global slot `slot` with
    /// `arguments`, of which there are `args`.
    fn compile_native_call(
        &mut self,
        slot: u16,
        args: u8,
        arguments: Vec<Arg>,
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
        arguments: Vec<Arg>,
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
            Dest::Any | Dest::Read => Ok(Operand::Temp(base)),
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
        // A later REPL line may make instances of a function in a global.
        if let Some(functions) = &mut self.functions
            && self.instance.is_none()
            && let Some(ResolvedVar::Global { .. }) = analysis.variable(id)
        {
            let declared = DeclaredFunction {
                name: name.clone(),
                body: body.clone(),
                pure,
                span,
            };
            functions.insert(id, declared);
        }
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

    /// The function that writes through the target at `id`, in the
    /// instance being compiled: the setter of `p.x`, `x=`.
    fn setter(&self, id: NodeId) -> Binding {
        self.instance_pick(|instance| instance.setters.get(&id))
            .unwrap_or_else(|| self.analysis.setter(id))
            .clone()
    }

    /// The function an assignment with an operator reads its target at
    /// `target` with.
    fn target_getter(&self, target: NodeId) -> Binding {
        self.getter(target)
            .expect("internal error: an augmented assignment's target has a getter")
    }

    /// The function the assignment with an operator `id` calls, in the
    /// instance being compiled: `+` for `x += 1`.
    fn augmented_assignment(&self, id: NodeId) -> Option<Binding> {
        self.instance_pick(|instance| instance.augmented_assignments.get(&id))
            .or_else(|| self.analysis.augmented_assignments.get(&id))
            .cloned()
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

#[derive(thiserror::Error, Debug)]
#[error("{text}")]
pub struct CompileError {
    text: String,
    span: Span,
}

impl CompileError {
    /// What the compiler fails with for code that makes instances
    /// of a function declared before it, outside of a REPL line.
    fn uncompiled_instances() -> Self {
        Self {
            text: "encountered instances of a function compiled earlier outside of a REPL line, this is probably an internal error".to_string(),
            span: Span::synthetic(),
        }
    }

    /// What the compiler fails with for a function whose values
    /// don't fit in the registers a frame can have.
    fn too_many_registers(span: Span) -> Self {
        Self {
            text: format!(
                "this function needs more than {WINDOW} registers, which is more than a frame can have",
            ),
            span,
        }
    }

    fn unresolved_binding(span: Span) -> Self {
        Self {
            text: "encountered unresolved binding during compilation, this is probably an internal error".to_string(),
            span,
        }
    }

    /// What the compiler fails with for a tuple index written in
    /// the code that doesn't fit its instruction, which no tuple type the
    /// analyser accepts has.
    fn index_too_large(span: Span) -> Self {
        Self {
            text: "this tuple index is larger than an instruction can say".to_string(),
            span,
        }
    }

    fn unresolved_cast(span: Span) -> Self {
        Self {
            text:
                "encountered unresolved cast during compilation, this is probably an internal error"
                    .to_string(),
            span,
        }
    }

    fn unnamed_intrinsic(span: Span) -> Self {
        Self {
            text: "encountered an intrinsic used other than by name during compilation, this is probably an internal error".to_string(),
            span,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }
}
