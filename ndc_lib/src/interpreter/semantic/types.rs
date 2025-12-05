use crate::ast::{Expression, ExpressionLocation};
use crate::interpreter::num::NumberType;
use crate::interpreter::value::ValueType;

fn give_type(ExpressionLocation { expression, .. }: &ExpressionLocation) -> ValueType {
    match expression {
        Expression::BoolLiteral(_) => ValueType::Bool,
        Expression::StringLiteral(_) => ValueType::String,
        Expression::Int64Literal(_) => ValueType::Number(NumberType::Int),
        Expression::Float64Literal(_) => ValueType::Number(NumberType::Float),
        Expression::BigIntLiteral(_) => ValueType::Number(NumberType::Int),
        Expression::ComplexLiteral(_) => ValueType::Number(NumberType::Complex),
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
