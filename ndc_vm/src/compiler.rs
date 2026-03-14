use crate::chunk::{Chunk, OpCode};
use crate::value::{CompiledFunction, Function};
use crate::{Object, Value};
use ndc_lexer::Span;
use ndc_parser::{
    Binding, CaptureSource, Expression, ExpressionLocation, LogicalOperator, Lvalue, ResolvedVar,
    StaticType, TypeSignature,
};
use std::rc::Rc;

#[derive(Default)]
pub struct Compiler {
    chunk: Chunk,
    max_local: usize,
    loop_stack: Vec<LoopContext>,
    allow_return: bool,
}

impl Compiler {
    pub fn compile(
        expressions: impl Iterator<Item = ExpressionLocation>,
    ) -> Result<CompiledFunction, CompileError> {
        let mut compiler = Self::default();

        for expr_loc in expressions {
            compiler.compile_expr(expr_loc)?;
        }

        compiler.chunk.write(OpCode::Halt, Span::new(0, 0));

        Ok(CompiledFunction {
            name: None,
            type_signature: TypeSignature::default(),
            body: compiler.chunk,
            return_type: StaticType::Any,
            num_locals: compiler.max_local,
        })
    }

    fn compile_expr(
        &mut self,
        expression_location: ExpressionLocation,
    ) -> Result<(), CompileError> {
        let ExpressionLocation { expression, span } = expression_location;
        match expression {
            Expression::BoolLiteral(b) => {
                let idx = self.chunk.add_constant(Value::Bool(b));
                self.chunk.write(OpCode::Constant(idx), span);
            }
            Expression::StringLiteral(s) => {
                let idx = self.chunk.add_constant(Object::String(s).into());
                self.chunk.write(OpCode::Constant(idx), span);
            }
            Expression::Int64Literal(i) => {
                let idx = self.chunk.add_constant(Value::Int(i));
                self.chunk.write(OpCode::Constant(idx), span);
            }
            Expression::Float64Literal(f) => {
                let idx = self.chunk.add_constant(Value::Float(f));
                self.chunk.write(OpCode::Constant(idx), span);
            }
            Expression::BigIntLiteral(i) => {
                let idx = self.chunk.add_constant(Object::BigInt(i).into());
                self.chunk.write(OpCode::Constant(idx), span);
            }
            Expression::ComplexLiteral(c) => {
                let idx = self.chunk.add_constant(Object::Complex(c).into());
                self.chunk.write(OpCode::Constant(idx), span);
            }
            Expression::Identifier { resolved, .. } => self.compile_binding(resolved, span)?,
            Expression::Statement(stm) => {
                let needs_pop = produces_value(&stm.expression);
                self.compile_expr(*stm)?;
                if needs_pop {
                    self.chunk.write(OpCode::Pop, span);
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
                        let end_jump = self.chunk.write(OpCode::JumpIfFalse(0), left_span);
                        self.chunk.write(OpCode::Pop, span);
                        self.compile_expr(*right)?;
                        self.chunk.patch_jump(end_jump);
                    }
                    LogicalOperator::Or => {
                        let end_jump = self.chunk.write(OpCode::JumpIfTrue(0), left_span);
                        self.chunk.write(OpCode::Pop, span);
                        self.compile_expr(*right)?;
                        self.chunk.patch_jump(end_jump);
                    }
                }
            }
            Expression::VariableDeclaration { value, l_value } => {
                self.compile_expr(*value)?;
                let slot = extract_lvalue_slot(&l_value);
                self.chunk.write(OpCode::SetLocal(slot), span);
                self.max_local = self.max_local.max(slot + 1);
            }
            Expression::Assignment {
                l_value,
                r_value: value,
            } => {
                match l_value {
                    Lvalue::Index {
                        value: container,
                        index,
                        resolved_set,
                    } => {
                        let set_fn = resolved_set.expect("[]= must be resolved");
                        self.compile_binding(set_fn, span)?;
                        self.compile_expr(*container)?;
                        self.compile_expr(*index)?;
                        self.compile_expr(*value)?;
                        self.chunk.write(OpCode::Call(3), span);
                    }
                    _ => {
                        self.compile_expr(*value)?;
                        self.compile_lvalue(l_value)?;
                        let idx = self.chunk.add_constant(Value::unit());
                        self.chunk.write(OpCode::Constant(idx), span);
                    }
                }
            }
            Expression::OpAssignment {
                l_value,
                r_value,
                resolved_assign_operation: _todo_in_place,
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
                        self.compile_binding(resolved_operation, span)?;
                        self.emit_get_var(var, lv_span);
                        self.compile_expr(*r_value)?;
                        self.chunk.write(OpCode::Call(2), span);
                        self.emit_set_var(var, lv_span);
                    }
                    Lvalue::Index { .. } => todo!("index op-assignment"),
                    Lvalue::Sequence(_) => todo!("sequence op-assignment"),
                }
                let idx = self.chunk.add_constant(Value::unit());
                self.chunk.write(OpCode::Constant(idx), span);
            }
            Expression::FunctionDeclaration {
                name,
                resolved_name,
                body,
                type_signature,
                return_type,
                captures,
                ..
            } => {
                self.compile_function_decl(
                    name,
                    resolved_name,
                    *body,
                    type_signature,
                    return_type,
                    captures,
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
            Expression::For { .. } => todo!("for loop"),
            Expression::Call {
                function,
                arguments,
            } => {
                let function_span = function.span;
                self.compile_expr(*function)?;

                let argument_count = arguments.len();
                for argument in arguments {
                    self.compile_expr(argument)?;
                }

                self.chunk
                    .write(OpCode::Call(argument_count), function_span);
            }
            Expression::Tuple { values } => {
                let size = values.len();
                for expression in values {
                    self.compile_expr(expression)?;
                }
                self.chunk.write(OpCode::MakeTuple(size), span);
            }
            Expression::List { values } => {
                let size = values.len();
                for expression in values {
                    self.compile_expr(expression)?;
                }
                self.chunk.write(OpCode::MakeList(size), span);
            }
            Expression::Map { .. } => todo!("map literal"),
            Expression::Return { value } => {
                if !self.allow_return {
                    return Err(CompileError::return_outside_function(span));
                }
                self.compile_expr(*value)?;
                self.chunk.write(OpCode::Return, span);
            }
            Expression::Break => {
                let idx = self.chunk.write(OpCode::Jump(0), span); // will be backpatched
                self.current_loop_context_mut()
                    .ok_or(CompileError::unexpected_break(span))?
                    .break_instructions
                    .push(idx);
            }
            Expression::Continue => {
                self.chunk.write_jump_back(
                    self.current_loop_context()
                        .ok_or(CompileError::unexpected_continue(span))?
                        .start,
                    span,
                );
            }
            Expression::RangeInclusive { .. } => todo!("inclusive range"),
            Expression::RangeExclusive { .. } => todo!("exclusive range"),
        }

        Ok(())
    }

    fn compile_lvalue(&mut self, l_value: Lvalue) -> Result<(), CompileError> {
        match l_value {
            Lvalue::Identifier {
                resolved,
                span: lv_span,
                ..
            } => {
                self.emit_set_var(resolved.expect("identifiers must be resolved"), lv_span);
            }
            Lvalue::Index { .. } => todo!("?"),
            Lvalue::Sequence(_) => todo!("?"),
        }

        Ok(())
    }

    fn compile_binding(&mut self, resolved: Binding, span: Span) -> Result<(), CompileError> {
        match resolved {
            Binding::None => return Err(CompileError::unresolved_binding(span)),
            Binding::Resolved(var) => self.emit_get_var(var, span),
            Binding::Dynamic(candidates) => {
                let idx = self
                    .chunk
                    .add_constant(Object::OverloadSet(candidates).into());
                self.chunk.write(OpCode::Constant(idx), span);
            }
        }

        Ok(())
    }

    fn emit_get_var(&mut self, var: ResolvedVar, span: Span) {
        match var {
            ResolvedVar::Local { slot } => self.chunk.write(OpCode::GetLocal(slot), span),
            ResolvedVar::Upvalue { slot } => self.chunk.write(OpCode::GetUpvalue(slot), span),
            ResolvedVar::Global { slot } => self.chunk.write(OpCode::GetGlobal(slot), span),
        };
    }

    fn emit_set_var(&mut self, var: ResolvedVar, span: Span) {
        match var {
            ResolvedVar::Local { slot } => self.chunk.write(OpCode::SetLocal(slot), span),
            ResolvedVar::Upvalue { slot } => self.chunk.write(OpCode::SetUpvalue(slot), span),
            ResolvedVar::Global { .. } => unreachable!("globals are native, never assigned"),
        };
    }
    fn compile_block(
        &mut self,
        statements: Vec<ExpressionLocation>,
        span: Span,
    ) -> Result<(), CompileError> {
        if statements.is_empty() {
            let idx = self.chunk.add_constant(Value::unit());
            self.chunk.write(OpCode::Constant(idx), span);
        } else {
            let last = statements.len() - 1;
            for (i, stmt) in statements.into_iter().enumerate() {
                let is_last_expr = i == last && produces_value(&stmt.expression);
                self.compile_expr(stmt)?;
                if i == last && !is_last_expr {
                    let idx = self.chunk.add_constant(Value::unit());
                    self.chunk.write(OpCode::Constant(idx), span);
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
        span: Span,
    ) -> Result<(), CompileError> {
        let condition_span = condition.span;
        self.compile_expr(condition)?;
        let conditional_jump_idx = self.chunk.write(OpCode::JumpIfFalse(0), condition_span);
        self.chunk.write(OpCode::Pop, span);
        self.compile_expr(on_true)?;
        if let Some(on_false) = on_false {
            // In the true branch, jump over the false branch
            let jump_to_end = self.chunk.write(OpCode::Jump(0), span);
            // The earlier JumpIfFalse lands here (start of false branch)
            self.chunk.patch_jump(conditional_jump_idx);
            self.chunk.write(OpCode::Pop, span);
            self.compile_expr(on_false)?;
            self.chunk.patch_jump(jump_to_end);
        } else {
            self.chunk.patch_jump(conditional_jump_idx);
            self.chunk.write(OpCode::Pop, span);
        }

        Ok(())
    }

    fn compile_while(
        &mut self,
        condition: ExpressionLocation,
        loop_body: ExpressionLocation,
        span: Span,
    ) -> Result<(), CompileError> {
        let condition_span = condition.span;
        let loop_start = self.new_loop_context();
        self.compile_expr(condition)?;
        let conditional_jump_idx = self.chunk.write(OpCode::JumpIfFalse(0), condition_span);
        self.chunk.write(OpCode::Pop, span);
        self.compile_expr(loop_body)?;
        self.chunk.write_jump_back(loop_start, span);
        self.chunk.patch_jump(conditional_jump_idx);
        self.chunk.write(OpCode::Pop, span);
        let break_instructions =
            std::mem::take(&mut self.current_loop_context_mut().unwrap().break_instructions);
        for instruction in break_instructions {
            self.chunk.patch_jump(instruction)
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
        type_signature: TypeSignature,
        return_type: Option<StaticType>,
        captures: Vec<CaptureSource>,
        span: Span,
    ) -> Result<(), CompileError> {
        let num_params = match &type_signature {
            TypeSignature::Exact(params) => params.len(),
            TypeSignature::Variadic => 0,
        };
        let mut fn_compiler = Self {
            max_local: num_params,
            allow_return: true,
            ..Default::default()
        };
        fn_compiler.compile_expr(body)?;
        fn_compiler.chunk.write(OpCode::Return, span);

        let compiled = CompiledFunction {
            name,
            type_signature,
            body: fn_compiler.chunk,
            return_type: return_type.unwrap_or_default(),
            num_locals: fn_compiler.max_local,
        };
        let idx = self
            .chunk
            .add_constant(Value::function(Function::Compiled(Rc::new(compiled))));

        if !captures.is_empty() {
            self.chunk.write(
                OpCode::Closure {
                    constant_idx: idx,
                    values: captures.into(),
                },
                span,
            );
        } else {
            self.chunk.write(OpCode::Constant(idx), span);
        }

        match resolved_name {
            Some(ResolvedVar::Local { slot }) => {
                self.chunk.write(OpCode::SetLocal(slot), span);
                self.max_local = self.max_local.max(slot + 1);
            }
            Some(ResolvedVar::Upvalue { .. } | ResolvedVar::Global { .. }) => {
                unreachable!("the analyser never assigns a declaration to a non-local binding")
            }
            None => {}
        }

        Ok(())
    }

    fn new_loop_context(&mut self) -> usize {
        let start = self.chunk.len();
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

struct LoopContext {
    start: usize,
    break_instructions: Vec<usize>,
}

fn produces_value(expr: &Expression) -> bool {
    !matches!(
        expr,
        Expression::Statement(_)
            | Expression::VariableDeclaration { .. }
            | Expression::FunctionDeclaration {
                resolved_name: Some(_),
                ..
            }
    )
}

fn extract_lvalue_slot(lvalue: &Lvalue) -> usize {
    match lvalue {
        Lvalue::Identifier {
            resolved: Some(ResolvedVar::Local { slot }),
            ..
        } => *slot,
        _ => panic!("expected resolved local identifier"),
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

    pub fn span(&self) -> Span {
        self.span
    }
}
