use crate::chunk::{Chunk, OpCode};
use ndc_interpreter::value::Value;
use ndc_parser::{Expression, ExpressionLocation};

pub struct Compiler;

impl Compiler {
    pub fn compile(expressions: impl Iterator<Item = ExpressionLocation>) -> Chunk {
        let mut chunk = Chunk::default();

        for ExpressionLocation { expression, span } in expressions {
            match expression {
                Expression::BoolLiteral(b) => {
                    let idx = chunk.add_constant(Value::Bool(b));
                    chunk.write(OpCode::Constant(idx), span);
                }
                Expression::StringLiteral(s) => {
                    let idx = chunk.add_constant(Value::string(s));
                    chunk.write(OpCode::Constant(idx), span);
                }
                Expression::Int64Literal(i) => {
                    let idx = chunk.add_constant(Value::from(i));
                    chunk.write(OpCode::Constant(idx), span);
                }
                Expression::Float64Literal(f) => {
                    let idx = chunk.add_constant(Value::from(f));
                    chunk.write(OpCode::Constant(idx), span);
                }
                Expression::BigIntLiteral(i) => {
                    let idx = chunk.add_constant(Value::from(i));
                    chunk.write(OpCode::Constant(idx), span);
                }
                Expression::ComplexLiteral(c) => {
                    let idx = chunk.add_constant(Value::from(c));
                    chunk.write(OpCode::Constant(idx), span);
                }
                Expression::Identifier { .. } => {}
                Expression::Statement(_) => {}
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

        chunk
    }
}
