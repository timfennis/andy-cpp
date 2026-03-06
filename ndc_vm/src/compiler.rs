use crate::chunk::{Chunk, OpCode};
use crate::value::{CompiledFunction, Function};
use crate::{Object, Value};
use ndc_lexer::Span;
use ndc_parser::{
    Binding, Expression, ExpressionLocation, LogicalOperator, Lvalue, ResolvedVar, StaticType,
    TypeSignature,
};

pub struct Compiler;

impl Compiler {
    pub fn compile(
        expressions: impl Iterator<Item = ExpressionLocation>,
    ) -> Result<CompiledFunction, CompileError> {
        let mut chunk = Chunk::default();

        for expr_loc in expressions {
            compile_expr(expr_loc, &mut chunk);
        }

        chunk.write(OpCode::Halt, Span::new(0, 0));

        Ok(CompiledFunction {
            name: None,
            type_signature: TypeSignature::default(),
            body: chunk,
            return_type: StaticType::Any,
        })
    }
}

fn compile_expr(
    ExpressionLocation { expression, span }: ExpressionLocation,
    chunk: &mut Chunk,
) -> usize {
    let start_len = chunk.len();
    match expression {
        Expression::BoolLiteral(b) => {
            let idx = chunk.add_constant(Value::Bool(b));
            chunk.write(OpCode::Constant(idx), span);
        }
        Expression::StringLiteral(s) => {
            let idx = chunk.add_constant(Object::String(s).into());
            chunk.write(OpCode::Constant(idx), span);
        }
        Expression::Int64Literal(i) => {
            let idx = chunk.add_constant(Value::Int(i));
            chunk.write(OpCode::Constant(idx), span);
        }
        Expression::Float64Literal(f) => {
            let idx = chunk.add_constant(Value::Float(f));
            chunk.write(OpCode::Constant(idx), span);
        }
        Expression::BigIntLiteral(i) => {
            let idx = chunk.add_constant(Object::BigInt(i).into());
            chunk.write(OpCode::Constant(idx), span);
        }
        Expression::ComplexLiteral(c) => {
            let idx = chunk.add_constant(Object::Complex(c).into());
            chunk.write(OpCode::Constant(idx), span);
        }
        Expression::Identifier { resolved, .. } => match resolved {
            Binding::None => todo!("return a nice error"),
            Binding::Resolved(ResolvedVar::Local { slot }) => {
                chunk.write(OpCode::GetLocal(slot), span);
            }
            Binding::Resolved(ResolvedVar::Upvalue { slot, depth }) => {
                chunk.write(OpCode::GetUpvalue { slot, depth }, span);
            }
            Binding::Resolved(ResolvedVar::Global { slot }) => {
                chunk.write(OpCode::GetGlobal(slot), span);
            }
            Binding::Dynamic(candidates) => {
                let idx = chunk.add_constant(Object::OverloadSet(candidates).into());
                chunk.write(OpCode::Constant(idx), span);
            }
        },
        Expression::Statement(stm) => {
            compile_expr(*stm, chunk);
            chunk.write(OpCode::Pop, span);
        }
        Expression::Logical {
            left,
            right,
            operator,
        } => {
            let left_span = left.span;
            compile_expr(*left, chunk);
            match operator {
                LogicalOperator::And => {
                    let end_jump = chunk.write(OpCode::JumpIfFalse(0), left_span);
                    chunk.write(OpCode::Pop, span);
                    compile_expr(*right, chunk);
                    chunk.patch_jump(end_jump);
                }
                LogicalOperator::Or => {
                    let end_jump = chunk.write(OpCode::JumpIfTrue(0), left_span);
                    chunk.write(OpCode::Pop, span);
                    compile_expr(*right, chunk);
                    chunk.patch_jump(end_jump);
                }
            }
        }
        Expression::VariableDeclaration { value, .. } => {
            compile_expr(*value, chunk);
        }
        Expression::Assignment {
            l_value,
            r_value: value,
        } => {
            compile_expr(*value, chunk);
            match l_value {
                Lvalue::Identifier {
                    resolved,
                    span: lv_span,
                    ..
                } => match resolved.expect("identifiers must be resolved") {
                    ResolvedVar::Local { slot } => {
                        chunk.write(OpCode::SetLocal(slot), lv_span);
                    }
                    ResolvedVar::Upvalue { .. } => todo!("?"),
                    ResolvedVar::Global { .. } => {
                        unreachable!("globals are native, never assigned")
                    }
                },
                Lvalue::Index { .. } => todo!("?"),
                Lvalue::Sequence(_) => todo!("?"),
            }
            let idx = chunk.add_constant(Value::unit());
            chunk.write(OpCode::Constant(idx), span);
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
            let mut fn_chunk = Chunk::default();
            compile_expr(*body, &mut fn_chunk);
            let is_closure = fn_chunk
                .iter()
                .any(|(_, op, _)| matches!(op, OpCode::GetUpvalue { .. }));
            // Ensure there is a return, this could become dead code if there is one already.
            fn_chunk.write(OpCode::Return, span);

            let compiled = CompiledFunction {
                name,
                type_signature,
                body: fn_chunk,
                return_type: return_type.unwrap_or_default(),
            };
            let idx = chunk.add_constant(
                Object::Function(Function::Compiled(std::rc::Rc::new(compiled))).into(),
            );

            // Put the compiled function inside the chunk's constant storage
            if is_closure {
                chunk.write(OpCode::Closure(idx), span);
            } else {
                chunk.write(OpCode::Constant(idx), span);
            }

            match resolved_name {
                Some(ResolvedVar::Local { .. }) => {
                    // value already on the stack at the right slot — no SetLocal needed
                }
                Some(ResolvedVar::Upvalue { .. }) => todo!("?"),
                Some(ResolvedVar::Global { .. }) => {
                    unreachable!("globals are native, never compiled")
                }
                None => {}
            }
        }
        Expression::Grouping(statements) => {
            compile_expr(*statements, chunk);
        }
        Expression::Block { statements } => {
            if statements.is_empty() {
                let idx = chunk.add_constant(Value::unit());
                chunk.write(OpCode::Constant(idx), span);
            } else {
                let last = statements.len() - 1;
                for (i, stmt) in statements.into_iter().enumerate() {
                    if i == last && !matches!(stmt.expression, Expression::Statement(_)) {
                        compile_expr(stmt, chunk);
                    } else if i == last {
                        compile_expr(stmt, chunk); // emits inner + Pop
                        let idx = chunk.add_constant(Value::unit());
                        chunk.write(OpCode::Constant(idx), span);
                    } else {
                        compile_expr(stmt, chunk); // non-last, always a Statement → inner + Pop
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
            compile_expr(*condition, chunk);
            let conditional_jump_idx = chunk.write(OpCode::JumpIfFalse(0), condition_span);
            chunk.write(OpCode::Pop, span);
            compile_expr(*on_true, chunk);
            // If there is an else branch we need to compile it and backpatch the jump instruction to find it
            if let Some(on_false) = on_false {
                // In the true branch still we add a jump instruction at the end
                let jump_to_end_op = chunk.write(OpCode::Jump(0), span);
                // Change the earlier jump to jump over the jump (YO DAWG)
                chunk.patch_jump(conditional_jump_idx);
                chunk.write(OpCode::Pop, span);
                compile_expr(*on_false, chunk);
                chunk.patch_jump(jump_to_end_op);
            } else {
                chunk.patch_jump(conditional_jump_idx);
                // If we're jumping over true we still need to pop the condition from the stack
                chunk.write(OpCode::Pop, span);
            }
        }
        Expression::While {
            expression: condition,
            loop_body,
        } => {
            let condition_span = condition.span;
            let loop_start = chunk.len();
            compile_expr(*condition, chunk);
            let conditional_jump_idx = chunk.write(OpCode::JumpIfFalse(0), condition_span);
            chunk.write(OpCode::Pop, span);
            compile_expr(*loop_body, chunk);
            chunk.write(
                OpCode::Jump(
                    -isize::try_from(chunk.len() - loop_start + 1)
                        .expect("loop too large to jump back"),
                ),
                span,
            );
            chunk.patch_jump(conditional_jump_idx);
            chunk.write(OpCode::Pop, span);
        }
        Expression::For { .. } => todo!("for loop"),
        Expression::Call {
            function,
            arguments,
        } => {
            let function_span = function.span;
            compile_expr(*function, chunk);

            let argument_count = arguments.len();
            for argument in arguments {
                compile_expr(argument, chunk);
            }

            chunk.write(OpCode::Call(argument_count), function_span);
        }
        Expression::Index { .. } => todo!("index expression"),
        Expression::Tuple { values } => {
            let size = values.len();
            for expression in values {
                compile_expr(expression, chunk);
            }
            chunk.write(OpCode::MakeTuple(size), span);
        }
        Expression::List { values } => {
            let size = values.len();
            for expression in values {
                compile_expr(expression, chunk);
            }
            chunk.write(OpCode::MakeList(size), span);
        }
        Expression::Map { .. } => todo!("map literal"),
        Expression::Return { value } => {
            compile_expr(*value, chunk);
            chunk.write(OpCode::Return, span);
        }
        Expression::Break => todo!("break"),
        Expression::Continue => todo!("continue"),
        Expression::RangeInclusive { .. } => todo!("inclusive range"),
        Expression::RangeExclusive { .. } => todo!("exclusive range"),
    }

    chunk.len() - start_len
}

#[derive(thiserror::Error, Debug)]
pub enum CompileError {}
