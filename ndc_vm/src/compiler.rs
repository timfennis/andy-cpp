use crate::chunk::{JumpTarget, LabelId, OpCode, OptimizerIr};
use crate::value::{CompiledFunction, Function};
use crate::{Object, Value};
use ndc_core::{StaticType, TypeSignature};
use ndc_lexer::Span;
use ndc_parser::{
    Binding, Candidate, CaptureSource, Expression, ExpressionLocation, ForBody, ForIteration,
    FunctionParameter, LogicalOperator, Lvalue, ResolvedVar,
};
use std::rc::Rc;

#[derive(Clone)]
pub struct Compiler {
    ir: OptimizerIr,
    num_locals: usize,
    loop_stack: Vec<LoopContext>,
    allow_return: bool,
    optimize: bool,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            ir: OptimizerIr::default(),
            num_locals: 0,
            loop_stack: Vec::new(),
            allow_return: false,
            optimize: true,
        }
    }
}

impl Compiler {
    pub fn compile(
        expressions: impl Iterator<Item = ExpressionLocation>,
    ) -> Result<CompiledFunction, CompileError> {
        let mut compiler = Self::default();
        for expr_loc in expressions {
            compiler.compile_expr(expr_loc)?;
        }
        Ok(compiler.finish()?.0)
    }

    /// Compile expressions without running the peephole optimizer. Useful
    /// for tests that want to inspect the raw compiler output, and for
    /// debugging tools (e.g. a future `--no-optimize` disassembler flag).
    pub fn compile_unoptimized(
        expressions: impl Iterator<Item = ExpressionLocation>,
    ) -> Result<CompiledFunction, CompileError> {
        let mut compiler = Self {
            optimize: false,
            ..Default::default()
        };
        for expr_loc in expressions {
            compiler.compile_expr(expr_loc)?;
        }
        Ok(compiler.finish()?.0)
    }

    /// Compile expressions and return both the finished function and a
    /// checkpoint that can be passed to `resume` to append more code later.
    /// The checkpoint is the compiler state *before* the `Halt` instruction,
    /// so `resume` can extend the bytecode without re-running old instructions.
    ///
    /// Optimization is disabled on this path: the REPL's resume-from-
    /// halt machinery relies on `halt_ip` matching the position of `Halt` in
    /// the emitted chunk, and shifting instructions invalidates that.
    pub fn compile_resumable(
        expressions: impl Iterator<Item = ExpressionLocation>,
    ) -> Result<(CompiledFunction, Self), CompileError> {
        let mut compiler = Self {
            optimize: false,
            ..Default::default()
        };
        for expr_loc in expressions {
            compiler.compile_expr(expr_loc)?;
        }
        compiler.finish()
    }

    /// Resume from a checkpoint produced by `compile_resumable` or a previous
    /// `resume` call.  Compiles `new_expressions` starting where the checkpoint
    /// left off, returning the extended function and a new checkpoint.
    ///
    /// The returned `CompiledFunction` contains all instructions (old + new),
    /// so the VM can be pointed at `checkpoint.halt_ip()` to execute only the
    /// new part while the stack already holds the old locals.
    pub fn resume(
        self,
        new_expressions: impl Iterator<Item = ExpressionLocation>,
    ) -> Result<(CompiledFunction, Self), CompileError> {
        let mut compiler = self; // checkpoint has no trailing Halt
        for expr_loc in new_expressions {
            compiler.compile_expr(expr_loc)?;
        }
        compiler.finish()
    }

    /// The instruction index where the trailing `Halt` was written.
    /// When resuming, this is the `ip` to start from in the new function.
    pub fn halt_ip(&self) -> usize {
        self.ir.len()
    }

    /// Number of top-level local slots used so far.
    pub fn num_locals(&self) -> usize {
        self.num_locals
    }

    /// Internal: clone a checkpoint (pre-Halt), write Halt, return both.
    fn finish(mut self) -> Result<(CompiledFunction, Self), CompileError> {
        let checkpoint = self.clone();
        self.ir.write(OpCode::Halt, Span::synthetic());
        if self.optimize {
            self.ir.peephole();
        }

        let function = CompiledFunction {
            name: None,
            static_type: StaticType::Function {
                parameters: Some(vec![]),
                return_type: Box::new(StaticType::Any),
            },
            body: self.ir.into_chunk(),
            num_locals: self.num_locals,
        };
        Ok((function, checkpoint))
    }

    fn compile_expr(
        &mut self,
        expression_location: ExpressionLocation,
    ) -> Result<(), CompileError> {
        let ExpressionLocation {
            expression, span, ..
        } = expression_location;
        match expression {
            Expression::BoolLiteral(b) => {
                let idx = self.ir.add_constant(Value::Bool(b));
                self.ir.write(OpCode::Constant(idx), span);
            }
            Expression::StringLiteral(s) => {
                let idx = self.ir.add_constant(Value::string(s));
                self.ir.write(OpCode::Constant(idx), span);
            }
            Expression::Int64Literal(i) => {
                let idx = self.ir.add_constant(Value::int(i));
                self.ir.write(OpCode::Constant(idx), span);
            }
            Expression::Float64Literal(f) => {
                let idx = self.ir.add_constant(Value::float(f));
                self.ir.write(OpCode::Constant(idx), span);
            }
            Expression::BigIntLiteral(i) => {
                let idx = self.ir.add_constant(Value::bigint(i));
                self.ir.write(OpCode::Constant(idx), span);
            }
            Expression::ComplexLiteral(c) => {
                let idx = self.ir.add_constant(Value::complex(c));
                self.ir.write(OpCode::Constant(idx), span);
            }
            Expression::Identifier { name, resolved } => {
                if name == "None" {
                    let idx = self.ir.add_constant(Value::None);
                    self.ir.write(OpCode::Constant(idx), span);
                } else {
                    self.compile_binding(resolved, span)?;
                }
            }
            Expression::Statement(stm) => {
                let needs_pop = produces_value(&stm.expression);
                self.compile_expr(*stm)?;
                if needs_pop {
                    self.ir.write(OpCode::Pop, Span::synthetic());
                }
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
                self.compile_declare_lvalue(l_value, span)?;
            }
            Expression::Assignment {
                l_value,
                r_value: value,
            } => match l_value {
                Lvalue::Index {
                    value: container,
                    index,
                    resolved_set,
                    ..
                } => {
                    let set_fn = resolved_set.expect("[]= must be resolved");
                    self.compile_binding(set_fn, span)?;
                    self.compile_expr(*container)?;
                    self.compile_expr(*index)?;
                    self.compile_expr(*value)?;
                    self.ir.write(OpCode::Call(3), span);
                }
                l_value @ Lvalue::Identifier { .. } => {
                    self.compile_expr(*value)?;
                    self.compile_lvalue(l_value, span)?;
                    let idx = self.ir.add_constant(Value::unit());
                    self.ir.write(OpCode::Constant(idx), Span::synthetic());
                }
                Lvalue::Sequence(seq) => {
                    self.compile_expr(*value)?;
                    self.ir.write(OpCode::Unpack(seq.len()), span);
                    for l_value in seq {
                        self.compile_lvalue(l_value, span)?;
                    }
                    let idx = self.ir.add_constant(Value::unit());
                    self.ir.write(OpCode::Constant(idx), Span::synthetic());
                }
            },
            Expression::OpAssignment {
                l_value,
                r_value,
                resolved_assign_operation,
                resolved_operation,
                ..
            } => {
                match l_value {
                    Lvalue::Identifier {
                        resolved,
                        span: lv_span,
                        ..
                    } => {
                        let var = resolved.expect("lvalue must be resolved");
                        match Self::op_assign_strategy(&resolved_assign_operation) {
                            OpAssignStrategy::InPlaceScalar => {
                                // `|=`, `&=`, `++=` over a List/Map/String:
                                // the in-place op mutates the value's Rc and
                                // returns unit (or the lhs). The slot already
                                // points at the shared Rc, so just discard.
                                self.compile_binding(resolved_assign_operation, span)?;
                                self.emit_get_var(var, lv_span);
                                self.compile_expr(*r_value)?;
                                self.ir.write(OpCode::Call(2), span);
                                self.ir.write(OpCode::Pop, span);
                            }
                            OpAssignStrategy::DynamicMerge => {
                                // `op=` exists but either dispatches to
                                // multiple candidates at runtime, or resolved
                                // to a single vec candidate that produces a
                                // fresh tuple. Either way the result must be
                                // stored back via SetVar — `op=` returns lhs
                                // when it mutates in place, or a fresh value
                                // when vec'd.
                                let (opcode, callee_binding): (OpCode, Binding) =
                                    match resolved_assign_operation {
                                        Binding::Resolved(Candidate::Vec(_)) => {
                                            (OpCode::CallVec(2), resolved_assign_operation)
                                        }
                                        Binding::Dynamic(assign_candidates) => {
                                            // Merge with `op` candidates so the runtime
                                            // dispatcher can fall back to `a op b` shape
                                            // for arg types `op=` doesn't accept.
                                            let mut merged = assign_candidates;
                                            match resolved_operation {
                                                Binding::Dynamic(c) => merged.extend(c),
                                                Binding::Resolved(c) => merged.push(c),
                                                Binding::None => {}
                                            }
                                            (OpCode::Call(2), Binding::Dynamic(merged))
                                        }
                                        _ => unreachable!(
                                            "DynamicMerge fires only for Resolved(Vec) or Dynamic op="
                                        ),
                                    };
                                self.compile_binding(callee_binding, span)?;
                                self.emit_get_var(var, lv_span);
                                self.compile_expr(*r_value)?;
                                self.ir.write(opcode, span);
                                self.emit_set_var(var, lv_span);
                            }
                            OpAssignStrategy::FallbackToOp => {
                                // No `op=` overload: lower to `lhs = lhs op rhs`.
                                // Vec-resolved `op` (e.g. `a += (3, 4)` on
                                // `Tuple<Int, Int>`) goes through `CallVec`
                                // for the speed-up; everything else is `Call`.
                                let opcode = Self::call_opcode_for(&resolved_operation, 2);
                                self.compile_binding(resolved_operation, span)?;
                                self.emit_get_var(var, lv_span);
                                self.compile_expr(*r_value)?;
                                self.ir.write(opcode, span);
                                self.emit_set_var(var, lv_span);
                            }
                        }
                    }
                    Lvalue::Index {
                        value,
                        index,
                        resolved_get,
                        resolved_set,
                    } => {
                        let container_span = value.span;
                        let index_span = index.span;

                        let tmp_container = self.num_locals;
                        let tmp_index = self.num_locals + 1;
                        self.num_locals += 2;

                        self.compile_expr(*value)?;
                        self.ir
                            .write(OpCode::SetLocal(tmp_container), container_span);
                        self.compile_expr(*index)?;
                        self.ir.write(OpCode::SetLocal(tmp_index), index_span);

                        self.compile_binding(
                            resolved_set.expect("[]= must be resolved"),
                            container_span.merge(index_span),
                        )?;
                        self.ir
                            .write(OpCode::GetLocal(tmp_container), container_span);
                        self.ir.write(OpCode::GetLocal(tmp_index), index_span);

                        let op_opcode = Self::call_opcode_for(&resolved_operation, 2);
                        self.compile_binding(resolved_operation, span)?;
                        self.compile_binding(
                            resolved_get.expect("[] must be resolved"),
                            index_span,
                        )?;
                        self.ir
                            .write(OpCode::GetLocal(tmp_container), container_span);
                        self.ir.write(OpCode::GetLocal(tmp_index), index_span);
                        self.ir.write(OpCode::Call(2), span); // [](container, index) → current_value
                        self.compile_expr(*r_value)?;
                        self.ir.write(op_opcode, span); // op(current_value, r_value) → new_value
                        self.ir.write(OpCode::Call(3), span); // []=(container, index, new_value)
                        self.ir.write(OpCode::Pop, span); // discard []= result; common code below pushes unit
                    }
                    Lvalue::Sequence(_) => {
                        return Err(CompileError::lvalue_required_to_be_single_identifier(span));
                    }
                }
                let idx = self.ir.add_constant(Value::unit());
                self.ir.write(OpCode::Constant(idx), span);
            }
            Expression::FunctionDeclaration {
                name,
                resolved_name,
                body,
                parameters,
                return_type,
                captures,
                pure,
                ..
            } => {
                let type_signature = FunctionParameter::from_params(&parameters);
                self.compile_function_decl(
                    name,
                    resolved_name,
                    *body,
                    &type_signature,
                    return_type,
                    captures,
                    pure,
                    span,
                )?;
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
            } => {
                let function_span = function.span;
                let opcode = match &function.expression {
                    Expression::Identifier { resolved, .. } => {
                        Self::call_opcode_for(resolved, arguments.len())
                    }
                    _ => OpCode::Call(arguments.len()),
                };
                self.compile_expr(*function)?;

                for argument in arguments {
                    self.compile_expr(argument)?;
                }

                self.ir.write(opcode, function_span);
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
                        let idx = self.ir.add_constant(Value::unit());
                        self.ir.write(OpCode::Constant(idx), Span::synthetic());
                    }
                }
                if let Some(default) = default {
                    self.compile_expr(*default)?;
                }
                self.ir.write(OpCode::MakeMap { pairs, has_default }, span);
            }
            Expression::Return { value } => {
                if !self.allow_return {
                    return Err(CompileError::return_outside_function(span));
                }
                self.compile_expr(*value)?;
                self.ir.write(OpCode::Return, span);
            }
            Expression::Break => {
                let idx = self.ir.write(OpCode::Jump(JumpTarget::PLACEHOLDER), span); // will be backpatched

                self.current_loop_context_mut()
                    .ok_or(CompileError::unexpected_break(span))?
                    .break_instructions
                    .push(idx);
            }
            Expression::Continue => {
                self.write_jump_back(
                    self.current_loop_context()
                        .ok_or(CompileError::unexpected_continue(span))?
                        .start,
                    span,
                );
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

        Ok(())
    }

    fn compile_lvalue(&mut self, l_value: Lvalue, span: Span) -> Result<(), CompileError> {
        match l_value {
            Lvalue::Identifier {
                resolved,
                span: lv_span,
                ..
            } => {
                self.emit_set_var(resolved.expect("identifiers must be resolved"), lv_span);
            }
            Lvalue::Index {
                value,
                index,
                resolved_set,
                ..
            } => {
                // Value to store is on top of stack. We need to:
                // 1. Save it to a temp slot
                // 2. Compile container and index
                // 3. Get the value back
                // 4. Call []= function
                // 5. Pop the return value

                let tmp_value = self.num_locals;
                self.num_locals += 1;
                self.ir.write(OpCode::SetLocal(tmp_value), span);

                self.compile_binding(resolved_set.expect("[]= must be resolved"), span)?;
                self.compile_expr(*value)?;
                self.compile_expr(*index)?;
                self.ir.write(OpCode::GetLocal(tmp_value), span);
                self.ir.write(OpCode::Call(3), span);
                self.ir.write(OpCode::Pop, Span::synthetic());
            }
            Lvalue::Sequence(seq) => {
                self.ir.write(OpCode::Unpack(seq.len()), span);
                for lv in seq {
                    self.compile_lvalue(lv, span)?;
                }
            }
        }

        Ok(())
    }

    fn compile_declare_lvalue(&mut self, l_value: Lvalue, span: Span) -> Result<(), CompileError> {
        match l_value {
            Lvalue::Identifier { resolved, .. } => {
                let slot = match resolved.expect("declaration lvalue must be resolved") {
                    ResolvedVar::Local { slot } => slot,
                    _ => unreachable!("declaration lvalue must be a local"),
                };
                self.ir.write(OpCode::SetLocal(slot), span);
                self.num_locals = self.num_locals.max(slot + 1);
            }
            Lvalue::Index { .. } => unreachable!("cannot declare into index"),
            Lvalue::Sequence(seq) => {
                self.ir.write(OpCode::Unpack(seq.len()), span);
                for lv in seq {
                    self.compile_declare_lvalue(lv, span)?;
                }
            }
        }
        Ok(())
    }

    /// Emit code that puts the resolved callee on top of the stack.
    ///
    /// * `Resolved(Scalar(var))` and `Resolved(Vec(scalar))` both emit a
    ///   direct `GetVar`; the call-site picks `Call` vs `CallVec` based on
    ///   which one the analyser chose (see [`Self::call_opcode_for`]).
    /// * `Dynamic(candidates)` pushes the candidate list as an `OverloadSet`
    ///   constant; the VM dispatcher narrows at runtime.
    fn compile_binding(&mut self, resolved: Binding, span: Span) -> Result<(), CompileError> {
        match resolved {
            Binding::None => return Err(CompileError::unresolved_binding(span)),
            Binding::Resolved(candidate) => self.emit_get_var(candidate.var(), span),
            Binding::Dynamic(candidates) => {
                // Split candidates by kind: scalars walked first (hot path),
                // vec candidates only consulted as fallback. Keeps the
                // scalar-only call site cost identical to master.
                let (scalars, vec_candidates) =
                    candidates
                        .into_iter()
                        .fold((Vec::new(), Vec::new()), |mut acc, c| {
                            match c {
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
            ResolvedVar::Upvalue { slot } => self.ir.write(OpCode::GetUpvalue(slot), span),
            ResolvedVar::Global { slot } => self.ir.write(OpCode::GetGlobal(slot), span),
        };
    }

    fn emit_set_var(&mut self, var: ResolvedVar, span: Span) {
        match var {
            ResolvedVar::Local { slot } => self.ir.write(OpCode::SetLocal(slot), span),
            ResolvedVar::Upvalue { slot } => self.ir.write(OpCode::SetUpvalue(slot), span),
            ResolvedVar::Global { .. } => unreachable!("globals are native, never assigned"),
        };
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
            let idx = self.ir.add_constant(Value::unit());
            // Synthetic unit from empty block has no meaningful source
            self.ir.write(OpCode::Constant(idx), Span::synthetic());
        } else {
            let last = statements.len() - 1;
            for (i, stmt) in statements.into_iter().enumerate() {
                let is_last_expr = i == last && produces_value(&stmt.expression);
                self.compile_expr(stmt)?;
                if i == last && !is_last_expr {
                    let idx = self.ir.add_constant(Value::unit());
                    // Synthetic unit when last statement doesn't produce value
                    self.ir.write(OpCode::Constant(idx), Span::synthetic());
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
            let idx = self.ir.add_constant(Value::unit());
            self.ir.write(OpCode::Constant(idx), Span::synthetic());
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
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_function_decl(
        &mut self,
        name: Option<String>,
        resolved_name: Option<ResolvedVar>,
        body: ExpressionLocation,
        type_signature: &TypeSignature,
        return_type: Option<StaticType>,
        captures: Vec<CaptureSource>,
        pure: bool,
        span: Span,
    ) -> Result<(), CompileError> {
        let num_params = match &type_signature {
            TypeSignature::Exact(params) => params.len(),
            TypeSignature::Variadic => 0,
        };
        let return_type = return_type.unwrap_or_default();
        let static_type = StaticType::Function {
            parameters: match &type_signature {
                TypeSignature::Variadic => None,
                TypeSignature::Exact(types) => {
                    Some(types.iter().map(|x| x.type_name.clone()).collect())
                }
            },
            return_type: Box::new(return_type.clone()),
        };
        let mut fn_compiler = Self {
            num_locals: num_params,
            allow_return: true,
            optimize: self.optimize,
            ..Default::default()
        };
        fn_compiler.compile_expr(body)?;
        fn_compiler.ir.write(OpCode::Return, Span::synthetic());

        if fn_compiler.optimize {
            fn_compiler.ir.peephole();
        }

        let compiled = CompiledFunction {
            name,
            static_type,
            body: fn_compiler.ir.into_chunk(),
            num_locals: fn_compiler.num_locals,
        };
        let idx = self
            .ir
            .add_constant(Value::function(Function::Compiled(Rc::new(compiled))));

        if !captures.is_empty() {
            self.ir.write(
                OpCode::Closure {
                    constant_idx: idx,
                    values: captures.into(),
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

        match resolved_name {
            Some(ResolvedVar::Local { slot }) => {
                self.ir.write(OpCode::SetLocal(slot), span);
                self.num_locals = self.num_locals.max(slot + 1);
            }
            Some(ResolvedVar::Upvalue { .. } | ResolvedVar::Global { .. }) => {
                unreachable!("the analyser never assigns a declaration to a non-local binding")
            }
            None => {}
        }

        Ok(())
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
                    // Discard it — the loop itself produces no value.
                    this.compile_expr(block.clone())?;
                    this.ir.write(OpCode::Pop, span);
                    Ok(())
                })?;
                Ok(())
            }
            ForBody::List {
                expr,
                accumulator_slot,
            } => {
                let tmp_list = accumulator_slot
                    .ok_or_else(|| CompileError::unresolved_accumulator_slot(span))?;
                self.num_locals = self.num_locals.max(tmp_list + 1);
                self.ir.write(OpCode::MakeList(0), span);
                self.ir.write(OpCode::SetLocal(tmp_list), span);
                self.compile_for_iterations(iterations, span, &mut |this| {
                    this.compile_expr(expr.clone())?;
                    this.ir.write(OpCode::ListPush(tmp_list), span);
                    Ok(())
                })?;
                self.ir.write(OpCode::GetLocal(tmp_list), span);
                Ok(())
            }
            ForBody::Map {
                key,
                value,
                default,
                accumulator_slot,
            } => {
                let tmp_map = accumulator_slot
                    .ok_or_else(|| CompileError::unresolved_accumulator_slot(span))?;
                self.num_locals = self.num_locals.max(tmp_map + 1);
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
                self.ir.write(OpCode::SetLocal(tmp_map), span);
                self.compile_for_iterations(iterations, span, &mut |this| {
                    this.compile_expr(key.clone())?;
                    if let Some(value) = value.clone() {
                        this.compile_expr(value)?;
                    } else {
                        let idx = this.ir.add_constant(Value::unit());
                        this.ir.write(OpCode::Constant(idx), Span::synthetic());
                    }
                    this.ir.write(OpCode::MapInsert(tmp_map), span);
                    Ok(())
                })?;
                self.ir.write(OpCode::GetLocal(tmp_map), span);
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
                self.compile_declare_lvalue(l_value.clone(), span)?;

                self.compile_for_iterations(rest, span, compile_leaf)?;

                // Close upvalues for the loop variable so each iteration's closures
                // get their own frozen copy rather than sharing a mutable slot.
                if let Some(slot) = min_lvalue_slot(l_value) {
                    self.ir.write(OpCode::CloseUpvalue(slot), span);
                }

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
}

/// Which lowering shape an `op=` site takes.
enum OpAssignStrategy {
    /// A scalar `op=` overload exists and was resolved exactly. The op
    /// mutates the value's Rc in place; the result is discarded.
    InPlaceScalar,
    /// `op=` resolved to multiple candidates. Merge with `op` candidates and
    /// dispatch at runtime; store the result back.
    DynamicMerge,
    /// No `op=` overload — lower to `lhs = lhs op rhs`.
    FallbackToOp,
}

impl Compiler {
    fn op_assign_strategy(op_assign: &Binding) -> OpAssignStrategy {
        match op_assign {
            Binding::Resolved(Candidate::Scalar(_)) => OpAssignStrategy::InPlaceScalar,
            // A vec-resolved op= would produce a fresh tuple result that must
            // be stored back; the merge path handles that correctly via
            // SetVar after the call. (In practice the stdlib has no such
            // overload, but this keeps the contract uniform.)
            Binding::Resolved(Candidate::Vec(_)) | Binding::Dynamic(_) => {
                OpAssignStrategy::DynamicMerge
            }
            Binding::None => OpAssignStrategy::FallbackToOp,
        }
    }
}

/// Returns the minimum local slot referenced by an lvalue, used to determine
/// which upvalues to close at the end of a loop iteration.
fn min_lvalue_slot(lv: &Lvalue) -> Option<usize> {
    match lv {
        Lvalue::Identifier {
            resolved: Some(ResolvedVar::Local { slot }),
            ..
        } => Some(*slot),
        Lvalue::Sequence(seq) => seq.iter().filter_map(min_lvalue_slot).min(),
        _ => None,
    }
}

fn produces_value(expr: &Expression) -> bool {
    match expr {
        Expression::Statement(_)
        | Expression::VariableDeclaration { .. }
        | Expression::FunctionDeclaration {
            resolved_name: Some(_),
            ..
        }
        | Expression::While { .. }
        | Expression::Break
        | Expression::Continue
        | Expression::Return { .. } => false,
        Expression::For { body, .. } => {
            matches!(**body, ForBody::List { .. } | ForBody::Map { .. })
        }
        _ => true,
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

    fn unexpected_break(span: Span) -> Self {
        Self {
            text: "unexpected break statement outside of loop".to_string(),
            span,
        }
    }
    fn unexpected_continue(span: Span) -> Self {
        Self {
            text: "unexpected continue statement outside of loop".to_string(),
            span,
        }
    }

    fn return_outside_function(span: Span) -> Self {
        Self {
            text: "unexpected return statement outside of function body".to_string(),
            span,
        }
    }

    fn lvalue_required_to_be_single_identifier(span: Span) -> Self {
        Self {
            text: "This lvalue is required to be a single identifier".to_string(),
            span,
        }
    }

    fn unresolved_accumulator_slot(span: Span) -> Self {
        Self {
            text: "accumulator slot was not assigned by the analyser; this is an internal error"
                .to_string(),
            span,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }
}
