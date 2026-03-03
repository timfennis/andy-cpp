use crate::chunk::{Chunk, OpCode};
use crate::{Object, Value};
use ndc_lexer::Span;
use ndc_parser::{Expression, ExpressionLocation, Lvalue, ResolvedVar};

pub struct Compiler;

impl Compiler {
    pub fn compile(expressions: impl Iterator<Item = ExpressionLocation>) -> Chunk {
        let mut chunk = Chunk::default();

        for expr_loc in expressions {
            compile_expr(expr_loc, &mut chunk);
        }

        // TODO: 0,0 span is kinda strange
        chunk.write(OpCode::Halt, Span::new(0, 0));
        chunk
    }
}
fn compile_expr(ExpressionLocation { expression, span }: ExpressionLocation, chunk: &mut Chunk) {
    eprintln!("[COMPILING]: {expression:?}");
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
            ndc_parser::Binding::Resolved(ResolvedVar::Local { slot }) => {
                chunk.write(OpCode::GetLocal(slot), span);
            }
            _ => {}
        },
        Expression::Statement(stm) => {
            compile_expr(*stm, chunk);
        }
        Expression::Logical { .. } => {}
        // TODO: is this supposed to be different in the VM?
        Expression::Assignment {
            l_value,
            r_value: value,
        }
        | Expression::VariableDeclaration { l_value, value } => {
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
                    ResolvedVar::Upvalue { .. } => {}
                    ResolvedVar::Global { .. } => {}
                },
                Lvalue::Index { .. } => {}
                Lvalue::Sequence(_) => {}
            }
        }
        Expression::OpAssignment { .. } => {}
        Expression::FunctionDeclaration { .. } => {}
        Expression::Grouping(statements) => {
            compile_expr(*statements, chunk);
        }
        Expression::Block { statements } => {
            for statement in statements {
                compile_expr(statement, chunk);
            }
        }
        Expression::If { .. } => {}
        Expression::While { .. } => {}
        Expression::For { .. } => {}
        Expression::Call { .. } => {}
        Expression::Index { .. } => {}
        Expression::Tuple { .. } => {}
        Expression::List { .. } => {}
        Expression::Map { .. } => {}
        Expression::Return { .. } => {}
        Expression::Break => {}
        Expression::Continue => {}
        Expression::RangeInclusive { .. } => {}
        Expression::RangeExclusive { .. } => {}
    }
}
