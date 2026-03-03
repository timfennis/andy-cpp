use crate::chunk::{Chunk, OpCode};
use crate::{Object, Value};
use ndc_core::int::Int;
use ndc_core::num::Number;
use ndc_parser::{Expression, ExpressionLocation};

pub struct Compiler;

impl Compiler {
    pub fn compile(expressions: impl Iterator<Item = ExpressionLocation>) -> Chunk {
        let mut chunk = Chunk::default();

        for expr_loc in expressions {
            compile_expr(expr_loc, &mut chunk);
        }

        chunk
    }
}
fn compile_expr(ExpressionLocation { expression, span }: ExpressionLocation, chunk: &mut Chunk) {
    println!("COMPILING: {expression:?}");
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
        Expression::Identifier { .. } => {}
        Expression::Statement(stm) => {
            compile_expr(*stm, chunk);
        }
        Expression::Logical { .. } => {}
        Expression::Grouping(_) => {}
        Expression::VariableDeclaration { .. } => {}
        Expression::Assignment { .. } => {}
        Expression::OpAssignment { .. } => {}
        Expression::FunctionDeclaration { .. } => {}
        Expression::Block { .. } => {}
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
