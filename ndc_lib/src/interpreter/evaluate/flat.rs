use crate::interpreter::InterpreterError;
use crate::interpreter::environment::Environment;
use crate::interpreter::evaluate::{EvaluationError, LiftEvaluationResult, PoolWalker};
use crate::interpreter::function::FunctionCarrier;
use crate::interpreter::int::Int;
use crate::interpreter::num::Number;
use crate::interpreter::value::Value;
use ndc_parser::{Expression, ExpressionPool};
use std::cell::RefCell;
use std::rc::Rc;

fn evaluate_flat(
    pool: ExpressionPool,
    environment: &Rc<RefCell<Environment>>,
) -> Result<Value, FunctionCarrier> {
    let pool = Rc::new(pool);

    let mut value = Value::unit();
    let mut state: Vec<Value> = Vec::with_capacity(pool.len());
    for (idx, expr) in pool.iter().enumerate() {
        match &expr.expression {
            Expression::BoolLiteral(v) => state[idx] = Value::Bool(*v),
            Expression::StringLiteral(s) => state[idx] = Value::string(s),
            Expression::Int64Literal(i) => state[idx] = Value::from(*i),
            Expression::Float64Literal(f) => state[idx] = Value::from(*f),
            Expression::BigIntLiteral(i) => {
                state[idx] = Value::Number(Number::Int(Int::BigInt(i.clone())))
            } // TODO: mem take?
            Expression::ComplexLiteral(c) => state[idx] = Value::Number(Number::Complex(c.clone())),
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
            Expression::Call {
                function,
                arguments,
            } => {
                let mut arguments: Vec<_> = arguments
                    .into_iter()
                    .map(|arg| state.remove(arg.as_usize()))
                    .collect();

                let function = &state[function.as_usize()];

                if let Value::Function(function) = function {
                    state[idx] = function
                        .call(&mut arguments, environment)
                        .add_span(expr.span)?
                } else {
                    return Err(FunctionCarrier::EvaluationError(EvaluationError::new(
                        format!("Unable to invoke {} as a function.", function.static_type()),
                        expr.span,
                    )));
                }
            }
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

    Ok(Value::unit())
}
