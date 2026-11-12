use crate::chunk::{JumpTarget, LabelId, OpCode, OptimizerIr};
use crate::value::{CompiledFunction, Function};
use crate::{Object, Value};
use ahash::AHashMap;
use ndc_analysis::{Analysis, Binding, Candidate, Instance, ResolvedVar};
use ndc_core::NodeId;
use ndc_core::StaticType;
use ndc_lexer::{NumericLiteral, Span};
use ndc_parser::{
    AssignmentTarget, AssignmentTargetLocation, BindingPattern, BindingPatternLocation, Expression,
    ExpressionLocation, ForBody, ForIteration, LogicalOperator,
};
use std::cell::{Cell, RefCell};
use std::rc::Rc;

pub struct Compiler {
    ir: OptimizerIr,
    source_locals: usize,
    temp_count: usize,
    temp_uses: Vec<TempUse>,
    loop_stack: Vec<LoopContext>,
    allow_return: bool,
    optimize: bool,
    /// What the analyser worked out about the batch being compiled.
    analysis: Rc<Analysis>,
    /// The instance of a generic function whose body is being compiled,
    /// whose picks take the place of the function's own.
    instance: Option<Rc<Instance>>,
    /// Whether the code compiled so far used a pick of `instance` that
    /// differs from the function's own, so it differs from what the
    /// function's own body compiles to.
    used_instance: Cell<bool>,
    /// The functions compiled outside of any instance, by declaration. A
    /// function an instance's body declares is the same function when it
    /// compiles the same, so it keeps its identity.
    prototypes: Rc<RefCell<AHashMap<NodeId, Rc<CompiledFunction>>>>,
}

impl Compiler {
    fn new() -> Self {
        Self {
            ir: Default::default(),
            source_locals: 0,
            temp_count: 0,
            temp_uses: Vec::new(),
            loop_stack: vec![],
            allow_return: false,
            optimize: true,
            analysis: Rc::default(),
            instance: None,
            used_instance: Cell::new(false),
            prototypes: Rc::default(),
        }
    }

    fn new_without_optimization() -> Self {
        Self {
            ir: Default::default(),
            source_locals: 0,
            temp_count: 0,
            temp_uses: Vec::new(),
            loop_stack: vec![],
            allow_return: false,
            optimize: false,
            analysis: Rc::default(),
            instance: None,
            used_instance: Cell::new(false),
            prototypes: Rc::default(),
        }
    }
    /// Compiles a program, or one REPL line, into a function the VM runs at
    /// the top level. Its locals are only what the code's blocks declare; its
    /// top-level declarations are globals.
    pub fn compile(
        expressions: impl Iterator<Item = ExpressionLocation>,
        analysis: Analysis,
    ) -> Result<CompiledFunction, CompileError> {
        let mut compiler = Self::new();
        compiler.compile_batch(expressions, analysis)?;
        Ok(compiler.finish())
    }

    /// Compile expressions without running the peephole optimizer. Useful
    /// for tests that want to inspect the raw compiler output, and for
    /// debugging tools (e.g. a future `--no-optimize` disassembler flag).
    pub fn compile_unoptimized(
        expressions: impl Iterator<Item = ExpressionLocation>,
        analysis: Analysis,
    ) -> Result<CompiledFunction, CompileError> {
        let mut compiler = Self::new_without_optimization();
        compiler.compile_batch(expressions, analysis)?;
        Ok(compiler.finish())
    }

    /// Compiles one batch of top-level expressions, `analysis` being what the
    /// analyser worked out about them.
    fn compile_batch(
        &mut self,
        expressions: impl Iterator<Item = ExpressionLocation>,
        analysis: Analysis,
    ) -> Result<(), CompileError> {
        self.source_locals = analysis.top_level_frame_size;
        self.analysis = Rc::new(analysis);
        for expression in expressions {
            self.compile_expr(expression)?;
        }
        Ok(())
    }

    /// Push the unit value; the result of expressions that have nothing else
    /// to produce. `span` is synthetic because unit has no source of its own.
    fn emit_unit(&mut self) {
        let idx = self.ir.add_constant(Value::unit());
        self.ir.write(OpCode::Constant(idx), Span::synthetic());
    }

    /// Pop the stack down to a loop's entry depth. Emitted before the jump of
    /// a `break`/`continue` that sits inside a partially compiled expression,
    /// e.g. `f(1 + break)`, so the pending operands don't leak.
    fn emit_operand_cleanup(&mut self, target_depth: isize, span: Span) {
        for _ in target_depth..self.ir.stack_depth() {
            self.ir.write(OpCode::Pop, span);
        }
    }

    fn allocate_temp(&mut self) -> TempSlot {
        let temp = TempSlot(self.temp_count);
        self.temp_count += 1;
        temp
    }

    fn write_temp(&mut self, temp: TempSlot, span: Span, opcode: impl FnOnce(usize) -> OpCode) {
        let instruction_idx = self.ir.len();
        self.ir.write(opcode(0), span);
        self.temp_uses.push(TempUse {
            instruction_idx,
            temp,
        });
    }

    fn layout_temporaries(&mut self) {
        for temp_use in &self.temp_uses {
            self.ir.set_local_slot(
                temp_use.instruction_idx,
                self.source_locals + temp_use.temp.0,
            );
        }
    }

    /// Number of local slots: the code's own variables, then the temporaries.
    fn num_locals(&self) -> usize {
        self.source_locals + self.temp_count
    }

    /// Ends the top-level function with `Halt`.
    fn finish(mut self) -> CompiledFunction {
        self.layout_temporaries();
        self.ir.write(OpCode::Halt, Span::synthetic());
        if self.optimize {
            self.ir.peephole();
        }

        let num_locals = self.num_locals();
        CompiledFunction {
            name: None,
            static_type: StaticType::Function {
                parameters: Some(vec![]),
                return_type: Box::new(StaticType::Any),
            },
            body: self.ir.into_chunk(),
            num_locals,
            num_globals: self.analysis.global_count,
        }
    }

    /// Compile one expression. Leaves exactly one result value on the stack,
    /// on top of any local slots the expression declared. Diverging
    /// expressions (`break`, `continue`, `return`) leave nothing because
    /// control never returns to observe a result.
    fn compile_expr(
        &mut self,
        expression_location: ExpressionLocation,
    ) -> Result<(), CompileError> {
        let ExpressionLocation {
            id,
            expression,
            span,
        } = expression_location;
        let entry_depth = self.ir.stack_depth();
        // A `;`-statement pops its own value and is the one expression that
        // leaves nothing on the stack.
        let leaves_value = !matches!(expression, Expression::Statement(_));
        match expression {
            Expression::BoolLiteral(b) => {
                let idx = self.ir.add_constant(Value::Bool(b));
                self.ir.write(OpCode::Constant(idx), span);
            }
            Expression::NoneLiteral => {
                let idx = self.ir.add_constant(Value::None);
                self.ir.write(OpCode::Constant(idx), span);
            }
            Expression::StringLiteral(s) => {
                let idx = self.ir.add_constant(Value::string(s));
                self.ir.write(OpCode::Constant(idx), span);
            }
            Expression::NumericLiteral(literal) => {
                let value = match literal {
                    NumericLiteral::Int64(i) => Value::int(i),
                    NumericLiteral::Float64(f) => Value::float(f),
                    NumericLiteral::NumberInt(i) => {
                        Value::number(crate::value::AdvancedNumber::Int(i))
                    }
                    NumericLiteral::NumberFloat(f) => {
                        Value::number(crate::value::AdvancedNumber::Float(f))
                    }
                    NumericLiteral::Complex(c) => Value::complex(c),
                };
                let idx = self.ir.add_constant(value);
                self.ir.write(OpCode::Constant(idx), span);
            }
            Expression::Identifier { .. } => {
                self.compile_binding(&self.binding(id), span)?;
            }
            Expression::Cast { value, .. } => {
                self.compile_expr(*value)?;
                match self.analysis.cast_checks.get(&id) {
                    None => return Err(CompileError::unresolved_cast(span)),
                    Some(None) => {}
                    Some(Some(target)) => {
                        let target = Rc::new(target.clone());
                        self.ir.write(OpCode::CheckType(target), span);
                    }
                }
            }
            Expression::Statement(stm) => {
                self.compile_expr(*stm)?;
                self.ir.write(OpCode::Pop, Span::synthetic());
            }
            Expression::Logical {
                left,
                right,
                operator,
            } => {
                let left_span = left.span;
                self.compile_expr(*left)?;
                match operator {
                    LogicalOperator::And => {
                        let end_jump = self
                            .ir
                            .write(OpCode::JumpIfFalse(JumpTarget::PLACEHOLDER), left_span);
                        self.ir.write(OpCode::Pop, Span::synthetic());
                        self.compile_expr(*right)?;
                        self.patch_jump(end_jump);
                    }
                    LogicalOperator::Or => {
                        let end_jump = self
                            .ir
                            .write(OpCode::JumpIfTrue(JumpTarget::PLACEHOLDER), left_span);
                        self.ir.write(OpCode::Pop, Span::synthetic());
                        self.compile_expr(*right)?;
                        self.patch_jump(end_jump);
                    }
                }
            }
            Expression::VariableDeclaration { value, l_value, .. } => {
                self.compile_expr(*value)?;
                self.compile_binding_pattern(l_value, span)?;
                self.emit_unit();
            }
            Expression::Assignment {
                l_value,
                r_value: value,
            } => match l_value.target {
                AssignmentTarget::Index {
                    value: container,
                    index,
                } => {
                    self.compile_binding(&self.setter(l_value.id), span)?;
                    self.compile_expr(*container)?;
                    self.compile_expr(*index)?;
                    self.compile_expr(*value)?;
                    self.expect_type(l_value.id, span);
                    self.ir.write(OpCode::Call(3), span);
                }
                AssignmentTarget::Identifier { span: lv_span, .. } => {
                    self.compile_expr(*value)?;
                    self.expect_type(l_value.id, lv_span);
                    self.emit_set_var(self.assigned_variable(l_value.id), lv_span);
                    self.emit_unit();
                }
                AssignmentTarget::Sequence(seq) => {
                    self.compile_expr(*value)?;
                    self.ir.write(OpCode::Unpack(seq.len()), span);
                    for l_value in seq {
                        self.compile_assignment_target(l_value, span)?;
                    }
                    self.emit_unit();
                }
                AssignmentTarget::Member {
                    receiver,
                    member_span,
                    ..
                } => {
                    self.compile_binding(&self.setter(l_value.id), member_span)?;
                    self.compile_expr(*receiver)?;
                    self.compile_expr(*value)?;
                    self.expect_type(l_value.id, member_span);
                    self.ir.write(OpCode::Call(2), member_span);
                }
            },
            Expression::OpAssignment {
                l_value, r_value, ..
            } => {
                let target_id = l_value.id;
                let target = PreparedAssignmentTarget::prepare(self, l_value)?;
                let Some(binding) = self.augmented_assignment(id) else {
                    return Err(CompileError::unresolved_binding(span));
                };

                let opcode = Self::call_opcode_for(&binding, 2);
                self.compile_binding(&binding, span)?;
                target.emit_read(self)?;
                self.compile_expr(*r_value)?;
                self.ir.write(opcode, span);
                self.expect_type(target_id, span);
                target.emit_store(self)?;

                self.emit_unit();
            }
            Expression::FunctionDeclaration {
                name, body, pure, ..
            } => {
                self.compile_function_decl(id, name, *body, pure, span)?;
            }
            Expression::StructDeclaration { .. } => {
                let analysis = Rc::clone(&self.analysis);
                let declared = analysis.declared_struct(id);
                // The constructor and accessors share this one `Rc`: struct
                // values and accessor checks compare their info by identity.
                let info = Rc::new(declared.info.clone());

                let idx = self
                    .ir
                    .add_constant(Value::function(Function::struct_constructor(Rc::clone(
                        &info,
                    ))));

                self.ir.write(OpCode::Constant(idx), span);

                let constructor = self
                    .analysis
                    .variable(id)
                    .expect("internal error: a struct declaration declares its constructor");
                self.emit_declare(constructor, span);

                for (field_offset, field) in declared.fields.iter().enumerate() {
                    // GET
                    let get_idx = self
                        .ir
                        .add_constant(Value::function(Function::struct_getter(
                            Rc::clone(&info),
                            field_offset,
                        )));
                    self.ir.write(OpCode::Constant(get_idx), span);
                    self.emit_declare(field.getter, span);

                    // SET
                    let set_idx = self
                        .ir
                        .add_constant(Value::function(Function::struct_setter(
                            Rc::clone(&info),
                            field_offset,
                        )));
                    self.ir.write(OpCode::Constant(set_idx), span);
                    self.emit_declare(field.setter, span);
                }
                self.emit_unit();
            }
            Expression::Grouping(statements) => {
                self.compile_expr(*statements)?;
            }
            Expression::Block { statements } => {
                self.compile_block(statements, span)?;
            }
            Expression::If {
                condition,
                on_true,
                on_false,
            } => {
                self.compile_if(*condition, *on_true, on_false.map(|e| *e), span)?;
            }
            Expression::While {
                expression: condition,
                loop_body,
            } => {
                self.compile_while(*condition, *loop_body, span)?;
            }
            Expression::For { iterations, body } => {
                self.compile_for(&iterations, *body, span)?;
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
            } => {
                let function_span = function.span;
                let opcode = match &function.expression {
                    Expression::Identifier { .. } => {
                        Self::call_opcode_for(&self.binding(function.id), arguments.len())
                    }
                    _ => OpCode::Call(arguments.len()),
                };
                self.compile_expr(*function)?;

                for argument in arguments {
                    self.compile_expr(argument)?;
                }

                self.ir.write(opcode, function_span);
            }
            Expression::MemberAccess {
                receiver,
                member_span,
                ..
            } => {
                let getter = self
                    .getter(id)
                    .expect("internal error: every member access is analysed before it's compiled");
                self.compile_binding(&getter, member_span)?;
                self.compile_expr(*receiver)?;
                self.ir.write(OpCode::Call(1), member_span);
            }
            Expression::Tuple { values } => {
                let size = values.len();
                for expression in values {
                    self.compile_expr(expression)?;
                }
                self.ir.write(OpCode::MakeTuple(size), span);
            }
            Expression::List { values } => {
                let size = values.len();
                for expression in values {
                    self.compile_expr(expression)?;
                }
                self.ir.write(OpCode::MakeList(size), span);
            }
            Expression::Map { values, default } => {
                let pairs = values.len();
                let has_default = default.is_some();
                for (key, value) in values {
                    self.compile_expr(key)?;
                    if let Some(v) = value {
                        self.compile_expr(v)?
                    } else {
                        self.emit_unit();
                    }
                }
                if let Some(default) = default {
                    self.compile_expr(*default)?;
                }
                self.ir.write(OpCode::MakeMap { pairs, has_default }, span);
            }
            Expression::Return { value } => {
                assert!(
                    self.allow_return,
                    "internal error: the analyser rejects a return outside of a function"
                );
                self.compile_expr(*value)?;
                self.ir.write(OpCode::Return, span);
            }
            Expression::Break => {
                let context = self
                    .current_loop_context()
                    .expect("internal error: the analyser rejects a break outside of a loop");
                self.emit_operand_cleanup(context.stack_depth, span);
                let idx = self.ir.write(OpCode::Jump(JumpTarget::PLACEHOLDER), span); // will be backpatched

                self.current_loop_context_mut()
                    .expect("loop context checked above")
                    .break_instructions
                    .push(idx);
            }
            Expression::Continue => {
                let context = self
                    .current_loop_context()
                    .expect("internal error: the analyser rejects a continue outside of a loop");
                let start = context.start;
                self.emit_operand_cleanup(context.stack_depth, span);
                self.write_jump_back(start, span);
            }
            range @ (Expression::RangeInclusive { .. } | Expression::RangeExclusive { .. }) => {
                let (inclusive, start, end) = match range {
                    Expression::RangeInclusive { start, end } => (true, start, end),
                    Expression::RangeExclusive { start, end } => (false, start, end),
                    _ => unreachable!(),
                };
                let start = start.expect("unbounded range start not yet supported");
                self.compile_expr(*start)?;
                let bounded = end.is_some();
                if let Some(end) = end {
                    self.compile_expr(*end)?;
                }
                self.ir
                    .write(OpCode::MakeRange { inclusive, bounded }, span);
            }
        }

        if leaves_value {
            self.expect_type(id, span);
        }
        // Every expression except a statement leaves exactly one value.
        self.ir
            .set_stack_depth(entry_depth + isize::from(leaves_value));
        Ok(())
    }

    /// Checks the value on top of the stack, which comes from `Any`, where
    /// the analyser says code at `at` expects a type.
    fn expect_type(&mut self, at: NodeId, span: Span) {
        if let Some(expected) = self
            .instance_pick(|instance| instance.type_checks.get(&at))
            .or_else(|| self.analysis.type_checks.get(&at))
        {
            let expected = Rc::new(expected.clone());
            self.ir.write(OpCode::ExpectType(expected), span);
        }
    }

    fn compile_assignment_target(
        &mut self,
        l_value: AssignmentTargetLocation,
        span: Span,
    ) -> Result<(), CompileError> {
        match l_value.target {
            AssignmentTarget::Identifier { span: lv_span, .. } => {
                self.expect_type(l_value.id, lv_span);
                self.emit_set_var(self.assigned_variable(l_value.id), lv_span);
            }
            AssignmentTarget::Index { value, index } => {
                // Value to store is on top of stack. We need to:
                // 1. Save it to a temp slot
                // 2. Compile container and index
                // 3. Get the value back
                // 4. Call []= function
                // 5. Pop the return value

                self.expect_type(l_value.id, span);
                let tmp_value = self.allocate_temp();
                self.write_temp(tmp_value, span, OpCode::SetLocal);

                self.compile_binding(&self.setter(l_value.id), span)?;
                self.compile_expr(*value)?;
                self.compile_expr(*index)?;
                self.write_temp(tmp_value, span, OpCode::GetLocal);
                self.ir.write(OpCode::Call(3), span);
                self.ir.write(OpCode::Pop, Span::synthetic());
            }
            AssignmentTarget::Sequence(seq) => {
                self.ir.write(OpCode::Unpack(seq.len()), span);
                for lv in seq {
                    self.compile_assignment_target(lv, span)?;
                }
            }
            AssignmentTarget::Member { .. } => unreachable!(
                "member assignment is lowered by Expression::Assignment; the parser rejects members inside destructuring patterns"
            ),
        }

        Ok(())
    }

    fn compile_binding_pattern(
        &mut self,
        pattern: BindingPatternLocation,
        span: Span,
    ) -> Result<(), CompileError> {
        match pattern.pattern {
            BindingPattern::Identifier { .. } => {
                let variable = self.analysis.variable(pattern.id).expect(
                    "internal error: every binding pattern is analysed before it's compiled",
                );
                self.expect_type(pattern.id, span);
                self.emit_declare(variable, span);
            }
            BindingPattern::Sequence(seq) => {
                self.ir.write(OpCode::Unpack(seq.len()), span);
                for lv in seq {
                    self.compile_binding_pattern(lv, span)?;
                }
            }
        }
        Ok(())
    }

    /// The variable the identifier assignment target at `target` writes to.
    fn assigned_variable(&self, target: NodeId) -> ResolvedVar {
        self.analysis
            .variable(target)
            .expect("internal error: every assignment target is analysed before it's compiled")
    }

    /// The function an augmented assignment reads the target at `target` with.
    fn target_getter(&self, target: NodeId) -> Binding {
        self.getter(target)
            .expect("internal error: an augmented assignment's target has a getter")
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
        let found = self.instance.as_deref().and_then(pick);
        if found.is_some() {
            self.used_instance.set(true);
        }
        found
    }

    /// The function that reads through the target at `id`, if it reads, in
    /// the instance being compiled.
    fn getter(&self, id: NodeId) -> Option<Binding> {
        self.instance_pick(|instance| instance.getters.get(&id))
            .or_else(|| self.analysis.getter(id))
            .cloned()
    }

    /// The function that writes through the target at `id`, in the instance
    /// being compiled.
    fn setter(&self, id: NodeId) -> Binding {
        self.instance_pick(|instance| instance.setters.get(&id))
            .unwrap_or_else(|| self.analysis.setter(id))
            .clone()
    }

    /// The function the augmented assignment `id` calls, in the instance
    /// being compiled.
    fn augmented_assignment(&self, id: NodeId) -> Option<Binding> {
        self.instance_pick(|instance| instance.augmented_assignments.get(&id))
            .or_else(|| self.analysis.augmented_assignments.get(&id))
            .cloned()
    }

    /// Emit code that puts the resolved callee on top of the stack.
    ///
    /// * `Resolved(Scalar(var))` and `Resolved(Vec(scalar))` both emit a
    ///   direct `GetVar`; the call-site picks `Call` vs `CallVec` based on
    ///   which one the analyser chose (see [`Self::call_opcode_for`]).
    /// * `Dynamic(candidates)` pushes the candidate list as an `OverloadSet`
    ///   constant; the VM dispatcher narrows at runtime.
    fn compile_binding(&mut self, resolved: &Binding, span: Span) -> Result<(), CompileError> {
        match resolved {
            Binding::None => return Err(CompileError::unresolved_binding(span)),
            Binding::Resolved(candidate) => self.emit_get_var(candidate.var(), span),
            Binding::Dynamic(candidates) => {
                // Split candidates by kind: scalars walked first (hot path),
                // vec candidates only consulted as fallback. Keeps the
                // scalar-only call site cost identical to master.
                let (scalars, vec_candidates) =
                    candidates
                        .iter()
                        .fold((Vec::new(), Vec::new()), |mut acc, c| {
                            match *c {
                                Candidate::Scalar(v) => acc.0.push(v),
                                Candidate::Vec(v) => acc.1.push(v),
                            }
                            acc
                        });
                let idx = self
                    .ir
                    .add_constant(Value::Object(Rc::new(Object::OverloadSet {
                        scalars,
                        vec_candidates,
                    })));
                self.ir.write(OpCode::Constant(idx), span);
            }
        }

        Ok(())
    }

    /// Pick the call opcode for a given binding: `CallVec(args)` when the
    /// analyser pinned a single vec candidate at compile time, else
    /// `Call(args)`. Dynamic bindings always use `Call` and let the VM's
    /// `find_overload` route to scalar vs vec dispatch.
    fn call_opcode_for(binding: &Binding, args: usize) -> OpCode {
        match binding {
            Binding::Resolved(Candidate::Vec(_)) => OpCode::CallVec(args),
            _ => OpCode::Call(args),
        }
    }

    fn emit_get_var(&mut self, var: ResolvedVar, span: Span) {
        match var {
            ResolvedVar::Local { slot } => self.ir.write(OpCode::GetLocal(slot), span),
            ResolvedVar::Cell { slot } => self.ir.write(OpCode::GetCell(slot), span),
            ResolvedVar::Upvalue { slot } => self.ir.write(OpCode::GetUpvalue(slot), span),
            ResolvedVar::Global { slot } => self.ir.write(OpCode::GetGlobal(slot), span),
        };
    }

    fn emit_set_var(&mut self, var: ResolvedVar, span: Span) {
        match var {
            ResolvedVar::Local { slot } => {
                self.ir.write(OpCode::SetLocal(slot), span);
            }
            ResolvedVar::Cell { slot } => {
                self.ir.write(OpCode::SetCell(slot), span);
            }
            ResolvedVar::Upvalue { slot } => {
                self.ir.write(OpCode::SetUpvalue(slot), span);
            }
            ResolvedVar::Global { slot } => {
                self.ir.write(OpCode::SetGlobal(slot), span);
            }
        }
    }

    /// Stores the value on top of the stack in a variable being declared. The
    /// declaration replaces whatever the slot held, so a captured local declared
    /// again, as in each loop iteration, gets a new variable instead of writing
    /// through the cell an earlier closure kept.
    fn emit_declare(&mut self, var: ResolvedVar, span: Span) {
        match var {
            ResolvedVar::Local { slot } | ResolvedVar::Cell { slot } => {
                debug_assert!(
                    slot < self.source_locals,
                    "internal error: slot {slot} is outside the frame of {} variables",
                    self.source_locals
                );
                self.ir.write(OpCode::SetLocal(slot), span);
            }
            ResolvedVar::Global { slot } => {
                self.ir.write(OpCode::SetGlobal(slot), span);
            }
            ResolvedVar::Upvalue { .. } => {
                unreachable!("the analyser never assigns a declaration to an upvalue")
            }
        }
    }

    /// Backpatches a forward jump at `op_idx` to land just after the most recently
    /// written instruction.
    fn patch_jump(&mut self, label: LabelId) {
        self.ir
            .set_jump_offset(label, JumpTarget::Label(self.ir.next_label()));
    }

    /// Emits a `Jump` that goes back to `target` (a previously recorded chunk offset).
    fn write_jump_back(&mut self, target: LabelId, span: Span) -> LabelId {
        self.ir.write(OpCode::Jump(JumpTarget::Label(target)), span)
    }

    fn compile_block(
        &mut self,
        statements: Vec<ExpressionLocation>,
        _span: Span,
    ) -> Result<(), CompileError> {
        if statements.is_empty() {
            self.emit_unit();
        } else {
            let last = statements.len() - 1;
            for (i, stmt) in statements.into_iter().enumerate() {
                // A `;`-terminated statement pops its own value, so it can't
                // be the block result; substitute unit.
                let is_last_expr =
                    i == last && !matches!(stmt.expression, Expression::Statement(_));
                self.compile_expr(stmt)?;
                if i == last && !is_last_expr {
                    self.emit_unit();
                }
            }
        }

        Ok(())
    }

    fn compile_if(
        &mut self,
        condition: ExpressionLocation,
        on_true: ExpressionLocation,
        on_false: Option<ExpressionLocation>,
        _span: Span,
    ) -> Result<(), CompileError> {
        let condition_span = condition.span;
        self.compile_expr(condition)?;
        let conditional_jump_idx = self
            .ir
            .write(OpCode::JumpIfFalse(JumpTarget::PLACEHOLDER), condition_span);
        self.ir.write(OpCode::Pop, Span::synthetic());
        self.compile_expr(on_true)?;
        if let Some(on_false) = on_false {
            let jump_to_end = self
                .ir
                .write(OpCode::Jump(JumpTarget::PLACEHOLDER), Span::synthetic());
            self.patch_jump(conditional_jump_idx);
            self.ir.write(OpCode::Pop, Span::synthetic());
            self.compile_expr(on_false)?;
            self.patch_jump(jump_to_end);
        } else {
            // No else branch — push unit so the if-expression always produces a value.
            let jump_to_end = self
                .ir
                .write(OpCode::Jump(JumpTarget::PLACEHOLDER), Span::synthetic());
            self.patch_jump(conditional_jump_idx);
            self.ir.write(OpCode::Pop, Span::synthetic());
            self.emit_unit();
            self.patch_jump(jump_to_end);
        }

        Ok(())
    }

    fn compile_while(
        &mut self,
        condition: ExpressionLocation,
        loop_body: ExpressionLocation,
        _span: Span,
    ) -> Result<(), CompileError> {
        let condition_span = condition.span;
        let loop_start = self.new_loop_context();
        self.compile_expr(condition)?;
        let conditional_jump_idx = self
            .ir
            .write(OpCode::JumpIfFalse(JumpTarget::PLACEHOLDER), condition_span);
        self.ir.write(OpCode::Pop, Span::synthetic());
        self.compile_expr(loop_body)?;
        self.ir.write(OpCode::Pop, Span::synthetic());
        self.write_jump_back(loop_start, Span::synthetic());
        self.patch_jump(conditional_jump_idx);
        self.ir.write(OpCode::Pop, Span::synthetic());
        let break_instructions =
            std::mem::take(&mut self.current_loop_context_mut().unwrap().break_instructions);
        for instruction in break_instructions {
            self.patch_jump(instruction)
        }
        self.end_loop_context();
        self.emit_unit();
        Ok(())
    }

    fn compile_function_decl(
        &mut self,
        id: NodeId,
        name: Option<String>,
        body: ExpressionLocation,
        pure: bool,
        span: Span,
    ) -> Result<(), CompileError> {
        let name_variable = self.analysis.variable(id);
        let analysis = Rc::clone(&self.analysis);
        let instances = analysis.instances.get(&id).map_or(&[][..], Vec::as_slice);
        // The function's own body comes first, so the functions it declares
        // are there for its instances to reuse.
        let (compiled, instance_bodies) = if instances.is_empty() {
            (self.prototype(id, name, body)?, Vec::new())
        } else {
            let compiled = self.prototype(id, name.clone(), body.clone())?;
            let instance_bodies = instances
                .iter()
                .map(|instance| {
                    self.compile_function_body(id, name.clone(), body.clone(), Some(instance))
                        .map(|(compiled, _)| compiled)
                })
                .collect::<Result<Vec<_>, _>>()?;
            (compiled, instance_bodies)
        };
        let idx = self
            .ir
            .add_constant(Value::function(Function::Compiled(compiled)));

        // A captured function may capture itself, before its value exists, so
        // its cell must be in place before the closure is made.
        if let Some(ResolvedVar::Cell { slot }) = name_variable {
            self.ir.write(OpCode::NewCell(slot), span);
        }

        let captures = self.analysis.captures(id);
        if !captures.is_empty() {
            let values = captures.into();
            self.ir.write(
                OpCode::Closure {
                    constant_idx: idx,
                    values,
                },
                span,
            );
        } else {
            self.ir.write(OpCode::Constant(idx), span);
        }

        // For `pure fn`, wrap the function in a memoization cache.  The cache
        // is allocated fresh each time the declaration is evaluated, so each
        // closure instance has its own independent cache.
        if pure {
            self.ir.write(OpCode::Memoize, span);
        }

        match name_variable {
            Some(var @ (ResolvedVar::Local { .. } | ResolvedVar::Global { .. })) => {
                self.emit_declare(var, span);
            }
            Some(ResolvedVar::Cell { slot }) => {
                self.ir.write(OpCode::SetCell(slot), span);
            }
            Some(ResolvedVar::Upvalue { .. }) => {
                unreachable!("the analyser never assigns a declaration to an upvalue")
            }
            None => {}
        }

        // Each instance of a generic function is its body compiled with the
        // instance's picks. A generic function is declared at the top level,
        // so it captures nothing and is a global, and its instances are
        // stored in globals of their own. A `pure fn`'s instances share its
        // cache, as they compute what it does.
        for (instance, instance_body) in instances.iter().zip(instance_bodies) {
            let idx = self
                .ir
                .add_constant(Value::function(Function::Compiled(Rc::new(instance_body))));
            self.ir.write(OpCode::Constant(idx), span);
            if pure && let Some(function) = name_variable {
                self.emit_get_var(function, span);
                self.ir.write(OpCode::MemoizeLike, span);
            }
            self.emit_declare(instance.variable, span);
        }

        if name_variable.is_some() {
            self.emit_unit();
        }

        Ok(())
    }

    /// The compiled function declared at `id`. Inside an instance's body, a
    /// function that compiles as it does outside of it is the same one.
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
        } else if let Some(prototype) = self.prototypes.borrow().get(&id) {
            return Ok(Rc::clone(prototype));
        }
        Ok(Rc::new(compiled))
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
        instance: Option<&Instance>,
    ) -> Result<(CompiledFunction, bool), CompileError> {
        let static_type = self.analysis.function_type(id).clone();
        let mut fn_compiler = Self {
            ir: Default::default(),
            source_locals: self.analysis.frame_size(id),
            temp_count: 0,
            temp_uses: Vec::new(),
            loop_stack: vec![],
            allow_return: true,
            optimize: self.optimize,
            analysis: Rc::clone(&self.analysis),
            instance: instance
                .map(|instance| Rc::new(instance.clone()))
                .or_else(|| self.instance.clone()),
            used_instance: Cell::new(false),
            prototypes: Rc::clone(&self.prototypes),
        };
        fn_compiler.compile_expr(body)?;
        fn_compiler.ir.write(OpCode::Return, Span::synthetic());
        fn_compiler.layout_temporaries();

        if fn_compiler.optimize {
            fn_compiler.ir.peephole();
        }

        let num_locals = fn_compiler.num_locals();
        let used_instance = fn_compiler.used_instance.get();
        Ok((
            CompiledFunction {
                name,
                static_type,
                body: fn_compiler.ir.into_chunk(),
                num_locals,
                num_globals: 0,
            },
            used_instance,
        ))
    }

    fn compile_for(
        &mut self,
        iterations: &[ForIteration],
        body: ForBody,
        span: Span,
    ) -> Result<(), CompileError> {
        match body {
            ForBody::Block(block) => {
                self.compile_for_iterations(iterations, span, &mut |this| {
                    // The body is always a block, which always pushes exactly one value.
                    // Discard it — each iteration's result is unobservable.
                    this.compile_expr(block.clone())?;
                    this.ir.write(OpCode::Pop, span);
                    Ok(())
                })?;
                self.emit_unit();
                Ok(())
            }
            ForBody::List { expr } => {
                let tmp_list = self.allocate_temp();
                self.ir.write(OpCode::MakeList(0), span);
                self.write_temp(tmp_list, span, OpCode::SetLocal);
                self.compile_for_iterations(iterations, span, &mut |this| {
                    this.compile_expr(expr.clone())?;
                    this.write_temp(tmp_list, span, OpCode::ListPush);
                    Ok(())
                })?;
                self.write_temp(tmp_list, span, OpCode::GetLocal);
                Ok(())
            }
            ForBody::Map {
                key,
                value,
                default,
            } => {
                let tmp_map = self.allocate_temp();
                let has_default = default.is_some();
                if let Some(default) = default {
                    self.compile_expr(*default)?;
                }
                self.ir.write(
                    OpCode::MakeMap {
                        pairs: 0,
                        has_default,
                    },
                    span,
                );
                self.write_temp(tmp_map, span, OpCode::SetLocal);
                self.compile_for_iterations(iterations, span, &mut |this| {
                    this.compile_expr(key.clone())?;
                    if let Some(value) = value.clone() {
                        this.compile_expr(value)?;
                    } else {
                        this.emit_unit();
                    }
                    this.write_temp(tmp_map, span, OpCode::MapInsert);
                    Ok(())
                })?;
                self.write_temp(tmp_map, span, OpCode::GetLocal);
                Ok(())
            }
        }
    }

    /// Shared loop scaffolding for `compile_for_block`, `compile_for_list`, and
    /// `compile_for_map`. Handles iteration and guard clauses; calls `compile_leaf`
    /// for the innermost body once all iterations are peeled off.
    fn compile_for_iterations(
        &mut self,
        iterations: &[ForIteration],
        span: Span,
        compile_leaf: &mut dyn FnMut(&mut Self) -> Result<(), CompileError>,
    ) -> Result<(), CompileError> {
        let Some((first, rest)) = iterations.split_first() else {
            return compile_leaf(self);
        };

        match first {
            ForIteration::Iteration { l_value, sequence } => {
                self.compile_expr(sequence.clone())?;
                self.ir.write(OpCode::GetIterator, sequence.span);

                let loop_start = self.new_loop_context();
                let iter_next = self
                    .ir
                    .write(OpCode::IterNext(JumpTarget::PLACEHOLDER), span);
                self.compile_binding_pattern(l_value.clone(), span)?;

                self.compile_for_iterations(rest, span, compile_leaf)?;

                self.write_jump_back(loop_start, span);

                // Both IterNext-done and break jump to the iterator Pop
                self.patch_jump(iter_next);
                let break_instructions = std::mem::take(
                    &mut self.current_loop_context_mut().unwrap().break_instructions,
                );
                for instruction in break_instructions {
                    self.patch_jump(instruction);
                }
                self.end_loop_context();

                // Pop the iterator
                self.ir.write(OpCode::Pop, Span::synthetic());
            }
            ForIteration::Guard(condition) => {
                self.compile_expr(condition.clone())?;
                let skip_jump = self
                    .ir
                    .write(OpCode::JumpIfFalse(JumpTarget::PLACEHOLDER), span);
                self.ir.write(OpCode::Pop, Span::synthetic());
                self.compile_for_iterations(rest, span, compile_leaf)?;
                let end_jump = self.ir.write(OpCode::Jump(JumpTarget::PLACEHOLDER), span);
                self.patch_jump(skip_jump);
                self.ir.write(OpCode::Pop, Span::synthetic());
                self.patch_jump(end_jump);
            }
        }

        Ok(())
    }

    fn new_loop_context(&mut self) -> LabelId {
        let start = self.ir.next_label();
        self.loop_stack.push(LoopContext {
            start,
            break_instructions: Vec::new(),
            stack_depth: self.ir.stack_depth(),
        });
        start
    }

    fn current_loop_context(&self) -> Option<&LoopContext> {
        self.loop_stack.last()
    }

    fn current_loop_context_mut(&mut self) -> Option<&mut LoopContext> {
        self.loop_stack.last_mut()
    }

    fn end_loop_context(&mut self) {
        self.loop_stack
            .pop()
            .expect("expected there to be a loop context to pop");
    }
}

#[derive(Clone)]
struct LoopContext {
    start: LabelId,
    break_instructions: Vec<LabelId>,
    /// The simulated stack depth at loop entry; `break`/`continue` pop back
    /// down to it before jumping.
    stack_depth: isize,
}

/// Logical compiler-local slot, assigned a concrete frame slot after lowering.
#[derive(Clone, Copy)]
struct TempSlot(usize);

#[derive(Clone, Copy)]
struct TempUse {
    instruction_idx: usize,
    temp: TempSlot,
}

/// An assignment location whose side-effecting components have already been
/// evaluated and cached. This lets augmented assignment use one lowering for
/// every target without evaluating an index expression more than once.
enum PreparedAssignmentTarget {
    Variable {
        variable: ResolvedVar,
        span: Span,
    },
    Index {
        cached_container: TempSlot,
        cached_index: TempSlot,
        container_span: Span,
        index_span: Span,
        getter: Binding,
        setter: Binding,
    },
    Member {
        cached_receiver: TempSlot,
        receiver_span: Span,
        member_span: Span,
        getter: Binding,
        setter: Binding,
    },
}

impl PreparedAssignmentTarget {
    fn prepare(
        compiler: &mut Compiler,
        l_value: AssignmentTargetLocation,
    ) -> Result<Self, CompileError> {
        match l_value.target {
            AssignmentTarget::Identifier { span, .. } => Ok(Self::Variable {
                variable: compiler.assigned_variable(l_value.id),
                span,
            }),
            AssignmentTarget::Index { value, index } => {
                let container_span = value.span;
                let index_span = index.span;
                let cached_container = compiler.allocate_temp();
                let cached_index = compiler.allocate_temp();

                compiler.compile_expr(*value)?;
                compiler.write_temp(cached_container, container_span, OpCode::SetLocal);
                compiler.compile_expr(*index)?;
                compiler.write_temp(cached_index, index_span, OpCode::SetLocal);

                Ok(Self::Index {
                    cached_container,
                    cached_index,
                    container_span,
                    index_span,
                    getter: compiler.target_getter(l_value.id),
                    setter: compiler.setter(l_value.id),
                })
            }
            AssignmentTarget::Member {
                receiver,
                member_span,
                ..
            } => {
                let receiver_span = receiver.span;
                let cached_receiver = compiler.allocate_temp();

                compiler.compile_expr(*receiver)?;
                compiler.write_temp(cached_receiver, receiver_span, OpCode::SetLocal);

                Ok(Self::Member {
                    cached_receiver,
                    receiver_span,
                    member_span,
                    getter: compiler.target_getter(l_value.id),
                    setter: compiler.setter(l_value.id),
                })
            }
            AssignmentTarget::Sequence(_) => unreachable!(
                "internal error: name resolution rejects a sequence as an augmented assignment target"
            ),
        }
    }

    fn emit_read(&self, compiler: &mut Compiler) -> Result<(), CompileError> {
        match self {
            Self::Variable { variable, span } => compiler.emit_get_var(*variable, *span),
            Self::Index {
                cached_container,
                cached_index,
                container_span,
                index_span,
                getter,
                ..
            } => {
                compiler.compile_binding(getter, *index_span)?;
                compiler.write_temp(*cached_container, *container_span, OpCode::GetLocal);
                compiler.write_temp(*cached_index, *index_span, OpCode::GetLocal);
                compiler
                    .ir
                    .write(OpCode::Call(2), container_span.merge(*index_span));
            }
            Self::Member {
                cached_receiver,
                receiver_span,
                member_span,
                getter,
                ..
            } => {
                compiler.compile_binding(getter, *member_span)?;
                compiler.write_temp(*cached_receiver, *receiver_span, OpCode::GetLocal);
                compiler.ir.write(OpCode::Call(1), *member_span);
            }
        }
        Ok(())
    }

    fn emit_store(&self, compiler: &mut Compiler) -> Result<(), CompileError> {
        match self {
            Self::Variable { variable, span } => compiler.emit_set_var(*variable, *span),
            Self::Index {
                cached_container,
                cached_index,
                container_span,
                index_span,
                setter,
                ..
            } => {
                let cached_value = compiler.allocate_temp();
                let target_span = container_span.merge(*index_span);

                compiler.write_temp(cached_value, target_span, OpCode::SetLocal);
                compiler.compile_binding(setter, target_span)?;
                compiler.write_temp(*cached_container, *container_span, OpCode::GetLocal);
                compiler.write_temp(*cached_index, *index_span, OpCode::GetLocal);
                compiler.write_temp(cached_value, target_span, OpCode::GetLocal);
                compiler.ir.write(OpCode::Call(3), target_span);
                compiler.ir.write(OpCode::Pop, target_span);
            }
            Self::Member {
                cached_receiver,
                receiver_span,
                member_span,
                setter,
                ..
            } => {
                let cached_value = compiler.allocate_temp();

                compiler.write_temp(cached_value, *member_span, OpCode::SetLocal);
                compiler.compile_binding(setter, *member_span)?;
                compiler.write_temp(*cached_receiver, *receiver_span, OpCode::GetLocal);
                compiler.write_temp(cached_value, *member_span, OpCode::GetLocal);
                compiler.ir.write(OpCode::Call(2), *member_span);
                compiler.ir.write(OpCode::Pop, *member_span);
            }
        }
        Ok(())
    }
}

#[derive(thiserror::Error, Debug)]
#[error("{text}")]
pub struct CompileError {
    text: String,
    span: Span,
}

impl CompileError {
    fn unresolved_binding(span: Span) -> Self {
        Self {
            text: "encountered unresolved binding during compilation, this is probably an internal error".to_string(),
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

    pub fn span(&self) -> Span {
        self.span
    }
}
