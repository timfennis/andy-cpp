use crate::chunk::{Chunk, OpCode};
use crate::value::{CompiledFunction, Function};
use crate::{Object, Value};
use ndc_lexer::Span;
use ndc_parser::{
    Binding, Expression, ExpressionLocation, LogicalOperator, Lvalue, ResolvedVar, StaticType,
    TypeSignature,
};
use std::rc::Rc;

#[derive(Default)]
pub struct Compiler {
    chunk: Chunk,
    max_local: usize,
}

impl Compiler {
    pub fn compile(
        expressions: impl Iterator<Item = ExpressionLocation>,
    ) -> Result<CompiledFunction, CompileError> {
        let mut compiler = Compiler::default();

        for expr_loc in expressions {
            compiler.compile_expr(expr_loc);
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

    fn compile_expr(&mut self, expression_location: ExpressionLocation) {
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
            Expression::Identifier { resolved, .. } => match resolved {
                Binding::None => todo!("return a nice error"),
                Binding::Resolved(ResolvedVar::Local { slot }) => {
                    self.chunk.write(OpCode::GetLocal(slot), span);
                }
                Binding::Resolved(ResolvedVar::Upvalue { slot, depth }) => {
                    self.chunk.write(OpCode::GetUpvalue { slot, depth }, span);
                }
                Binding::Resolved(ResolvedVar::Global { slot }) => {
                    self.chunk.write(OpCode::GetGlobal(slot), span);
                }
                Binding::Dynamic(candidates) => {
                    let idx = self
                        .chunk
                        .add_constant(Object::OverloadSet(candidates).into());
                    self.chunk.write(OpCode::Constant(idx), span);
                }
            },
            Expression::Statement(stm) => {
                let needs_pop = produces_value(&stm.expression);
                self.compile_expr(*stm);
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
                self.compile_expr(*left);
                match operator {
                    LogicalOperator::And => {
                        let end_jump = self.chunk.write(OpCode::JumpIfFalse(0), left_span);
                        self.chunk.write(OpCode::Pop, span);
                        self.compile_expr(*right);
                        self.chunk.patch_jump(end_jump);
                    }
                    LogicalOperator::Or => {
                        let end_jump = self.chunk.write(OpCode::JumpIfTrue(0), left_span);
                        self.chunk.write(OpCode::Pop, span);
                        self.compile_expr(*right);
                        self.chunk.patch_jump(end_jump);
                    }
                }
            }
            Expression::VariableDeclaration { value, l_value } => {
                self.compile_expr(*value);
                let slot = extract_lvalue_slot(&l_value);
                self.chunk.write(OpCode::SetLocal(slot), span);
                self.max_local = self.max_local.max(slot + 1);
            }
            Expression::Assignment {
                l_value,
                r_value: value,
            } => {
                self.compile_expr(*value);
                match l_value {
                    Lvalue::Identifier {
                        resolved,
                        span: lv_span,
                        ..
                    } => match resolved.expect("identifiers must be resolved") {
                        ResolvedVar::Local { slot } => {
                            self.chunk.write(OpCode::SetLocal(slot), lv_span);
                        }
                        ResolvedVar::Upvalue { .. } => todo!("?"),
                        ResolvedVar::Global { .. } => {
                            unreachable!("globals are native, never assigned")
                        }
                    },
                    Lvalue::Index { .. } => todo!("?"),
                    Lvalue::Sequence(_) => todo!("?"),
                }
                let idx = self.chunk.add_constant(Value::unit());
                self.chunk.write(OpCode::Constant(idx), span);
            }
            Expression::OpAssignment { .. } => todo!("op-assignment"),
            Expression::FunctionDeclaration {
                name,
                resolved_name,
                body,
                type_signature,
                return_type,
                ..
            } => {
                let num_params = match &type_signature {
                    TypeSignature::Exact(params) => params.len(),
                    TypeSignature::Variadic => 0,
                };
                let mut fn_compiler = Compiler {
                    max_local: num_params,
                    ..Default::default()
                };
                fn_compiler.compile_expr(*body);
                let is_closure = fn_compiler
                    .chunk
                    .iter()
                    .any(|(_, op, _)| matches!(op, OpCode::GetUpvalue { .. }));
                fn_compiler.chunk.write(OpCode::Return, span);
                let captures = fn_compiler.chunk.capture_upvalues();

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

                if is_closure {
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
                    Some(ResolvedVar::Upvalue { .. }) => {
                        todo!("?")
                    }
                    Some(ResolvedVar::Global { .. }) => {
                        unreachable!("globals are native, never compiled")
                    }
                    None => {}
                }
            }
            Expression::Grouping(statements) => {
                self.compile_expr(*statements);
            }
            Expression::Block { statements } => {
                if statements.is_empty() {
                    let idx = self.chunk.add_constant(Value::unit());
                    self.chunk.write(OpCode::Constant(idx), span);
                } else {
                    let last = statements.len() - 1;
                    for (i, stmt) in statements.into_iter().enumerate() {
                        let is_block_result =
                            i == last && produces_value(&stmt.expression);
                        self.compile_expr(stmt);
                        if i == last && !is_block_result {
                            let idx = self.chunk.add_constant(Value::unit());
                            self.chunk.write(OpCode::Constant(idx), span);
                        }
                    }
                }
            }
            Expression::If {
                condition,
                on_true,
                on_false,
            } => {
                let condition_span = condition.span;
                self.compile_expr(*condition);
                let conditional_jump_idx = self.chunk.write(OpCode::JumpIfFalse(0), condition_span);
                self.chunk.write(OpCode::Pop, span);
                self.compile_expr(*on_true);
                // If there is an else branch we need to compile it and backpatch the jump instruction to find it
                if let Some(on_false) = on_false {
                    // In the true branch still we add a jump instruction at the end
                    let jump_to_end_op = self.chunk.write(OpCode::Jump(0), span);
                    // Change the earlier jump to jump over the jump (YO DAWG)
                    self.chunk.patch_jump(conditional_jump_idx);
                    self.chunk.write(OpCode::Pop, span);
                    self.compile_expr(*on_false);
                    self.chunk.patch_jump(jump_to_end_op);
                } else {
                    self.chunk.patch_jump(conditional_jump_idx);
                    // If we're jumping over true we still need to pop the condition from the stack
                    self.chunk.write(OpCode::Pop, span);
                }
            }
            Expression::While {
                expression: condition,
                loop_body,
            } => {
                let condition_span = condition.span;
                let loop_start = self.chunk.len();
                self.compile_expr(*condition);
                let conditional_jump_idx = self.chunk.write(OpCode::JumpIfFalse(0), condition_span);
                self.chunk.write(OpCode::Pop, span);
                self.compile_expr(*loop_body);
                self.chunk.write(
                    OpCode::Jump(
                        -isize::try_from(self.chunk.len() - loop_start + 1)
                            .expect("loop too large to jump back"),
                    ),
                    span,
                );
                self.chunk.patch_jump(conditional_jump_idx);
                self.chunk.write(OpCode::Pop, span);
            }
            Expression::For { .. } => todo!("for loop"),
            Expression::Call {
                function,
                arguments,
            } => {
                let function_span = function.span;
                self.compile_expr(*function);

                let argument_count = arguments.len();
                for argument in arguments {
                    self.compile_expr(argument);
                }

                self.chunk
                    .write(OpCode::Call(argument_count), function_span);
            }
            Expression::Tuple { values } => {
                let size = values.len();
                for expression in values {
                    self.compile_expr(expression);
                }
                self.chunk.write(OpCode::MakeTuple(size), span);
            }
            Expression::List { values } => {
                let size = values.len();
                for expression in values {
                    self.compile_expr(expression);
                }
                self.chunk.write(OpCode::MakeList(size), span);
            }
            Expression::Map { .. } => todo!("map literal"),
            Expression::Return { value } => {
                self.compile_expr(*value);
                self.chunk.write(OpCode::Return, span);
            }
            Expression::Break => todo!("break"),
            Expression::Continue => todo!("continue"),
            Expression::RangeInclusive { .. } => todo!("inclusive range"),
            Expression::RangeExclusive { .. } => todo!("exclusive range"),
        }
    }
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
pub enum CompileError {}
