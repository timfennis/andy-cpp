use std::cmp::Ordering;
use std::collections::VecDeque;
use std::error::Error;
use std::fmt;
use std::ops::{Neg, Rem};
use std::rc::Rc;

use crate::ast::{
    BinaryOperator, Expression, ExpressionLocation, LogicalOperator, Lvalue, UnaryOperator,
};
use crate::interpreter::environment::{Environment, EnvironmentRef};
use crate::interpreter::function::{Closure, FunctionCarrier};
use crate::interpreter::int::Int;
use crate::interpreter::num::Number;
use crate::interpreter::value::{Sequence, Value, ValueType};
use crate::lexer::Location;

pub type EvaluationResult = Result<Value, FunctionCarrier>;

#[allow(clippy::too_many_lines)]
pub(crate) fn evaluate_expression(
    expression_location: &ExpressionLocation,
    environment: &mut EnvironmentRef,
) -> EvaluationResult {
    let (start, end) = (expression_location.start, expression_location.end);
    let literal: Value = match &expression_location.expression {
        Expression::Unary {
            expression: expression_location,
            operator,
        } => {
            let value = evaluate_expression(expression_location, environment)?;
            match (value, operator) {
                (Value::Number(n), UnaryOperator::Neg) => Value::Number(n.neg()),
                (Value::Bool(b), UnaryOperator::Bang) => Value::Bool(!b),
                (_, UnaryOperator::Bang) => {
                    return Err(EvaluationError::type_error(
                        "the '!' operator cannot be applied to this type",
                        start,
                        end,
                    )
                    .into());
                }
                (_, UnaryOperator::Neg) => {
                    return Err(EvaluationError::type_error(
                        "this type cannot be negated",
                        start,
                        end,
                    )
                    .into());
                }
            }
        }
        Expression::Binary {
            left,
            operator: operator_token,
            right,
        } => {
            let left = evaluate_expression(left, environment)?;
            let right = evaluate_expression(right, environment)?;
            apply_operator(left, *operator_token, right)?
        }
        Expression::Grouping(expr) => evaluate_expression(expr, environment)?,
        Expression::VariableDeclaration {
            l_value: Lvalue::Variable { identifier },
            value,
        } => {
            let value = evaluate_expression(value, environment)?;

            if environment.borrow().contains(identifier) {
                let new_env = Environment::new_scope(environment);
                *environment = new_env;
            }
            environment.borrow_mut().declare(identifier, value.clone());

            value
        }
        Expression::VariableAssignment {
            l_value: Lvalue::Variable { identifier },
            value,
        } => {
            if !environment.borrow().contains(identifier) {
                return Err(EvaluationError::syntax_error(
                    &format!("undefined variable {identifier}"),
                    start,
                    end,
                )
                .into());
            }
            let value = evaluate_expression(value, environment)?;
            environment
                .borrow_mut()
                .assign(identifier.clone(), value.clone());
            value
        }
        Expression::Block { statements } => {
            let mut local_scope = Environment::new_scope(environment);

            let mut value = Value::Unit;
            for stm in statements {
                value = evaluate_expression(stm, &mut local_scope)?;
            }

            drop(local_scope);
            value
        }
        Expression::If {
            expression,
            on_true,
            on_false,
        } => {
            let result = evaluate_expression(expression, environment)?;

            match (result, on_false) {
                (Value::Bool(true), _) => evaluate_expression(on_true, environment)?,
                (Value::Bool(false), Some(block)) => evaluate_expression(block, environment)?,
                (Value::Bool(false), None) => Value::Unit,
                (value, _) => {
                    return Err(EvaluationError::type_error(
                        &format!(
                            "mismatched types: expected bool, found {}",
                            ValueType::from(&value)
                        ),
                        start,
                        end,
                    )
                    .into())
                }
            }
        }
        Expression::Statement(expression) => {
            evaluate_expression(expression, environment)?;
            Value::Unit
        }
        Expression::Logical {
            operator,
            left,
            right,
        } => {
            let left = evaluate_expression(left, environment)?;
            match (operator, left) {
                (LogicalOperator::And, Value::Bool(true)) => {
                    evaluate_expression(right, environment)?
                }
                (LogicalOperator::And, Value::Bool(false)) => Value::Bool(false),
                (LogicalOperator::Or, Value::Bool(false)) => {
                    evaluate_expression(right, environment)?
                }
                (LogicalOperator::Or, Value::Bool(true)) => Value::Bool(true),
                (LogicalOperator::And | LogicalOperator::Or, value) => {
                    return Err(EvaluationError::type_error(
                        &format!(
                            "Cannot apply logical operator to non bool value {}",
                            ValueType::from(&value)
                        ),
                        start,
                        end,
                    )
                    .into())
                }
            }
        }
        Expression::While {
            expression,
            loop_body,
        } => {
            let mut local_scope = Environment::new_scope(environment);
            loop {
                let lit = evaluate_expression(expression, &mut local_scope)?;
                if lit == Value::Bool(true) {
                    evaluate_expression(loop_body, &mut local_scope)?;
                } else if lit == Value::Bool(false) {
                    break;
                } else {
                    return Err(EvaluationError::type_error(
                        "Expression in a while structure must return a bool",
                        start,
                        end,
                    )
                    .into());
                }
            }
            drop(local_scope);
            Value::Unit
        }
        Expression::BoolLiteral(b) => Value::Bool(*b),
        Expression::StringLiteral(s) => Value::Sequence(Sequence::String(s.clone())),
        Expression::Int64Literal(n) => Value::Number(Number::Int(Int::Int64(*n))),
        Expression::BigIntLiteral(n) => Value::Number(Number::Int(Int::BigInt(n.clone()))),
        Expression::Float64Literal(n) => Value::Number(Number::Float(*n)),
        Expression::ComplexLiteral(n) => Value::Number(Number::Complex(*n)),
        Expression::UnitLiteral => Value::Unit,
        Expression::Call {
            function,
            arguments,
        } => {
            // The Expression in `function` must either be an identifier in which case it will be looked up in the
            // environment, or it must be some expression that evaluates to a function.
            let function = if let Expression::Identifier(name) = &function.expression {
                if let Some(refcell) = environment.borrow().get(name) {
                    let value = &*refcell.borrow();
                    if let Value::Function(function) = value {
                        function.clone()
                    } else {
                        return Err(EvaluationError::syntax_error(
                            &format!("{name} is not a function"),
                            expression_location.start,
                            expression_location.end,
                        )
                        .into());
                    }
                } else {
                    return Err(EvaluationError::syntax_error(
                        &format!("undefined function {name}"),
                        expression_location.start,
                        expression_location.end,
                    )
                    .into());
                }
            } else {
                let value = evaluate_expression(function, environment)?;
                if let Value::Function(function) = value {
                    function.clone()
                } else {
                    return Err(EvaluationError::syntax_error(
                        &format!("{} is not callable", ValueType::from(&value)),
                        // FIXME: this is the location of the expression and not the parentheses that make this a function call
                        expression_location.start,
                        expression_location.end,
                    )
                    .into());
                }
            };

            let mut evaluated_args = Vec::new();

            for argument in arguments {
                evaluated_args.push(evaluate_expression(argument, environment)?);
            }

            return function.call(&evaluated_args, environment);
        }
        Expression::FunctionDeclaration {
            arguments,
            body,
            name,
        } => {
            let name = name.try_into_identifier()?;

            let user_function = Closure {
                parameters: arguments.try_into_parameters()?,
                body: body.clone(),
                environment: environment.clone(),
            };

            environment
                .borrow_mut()
                .declare(&name, Value::Function(Rc::new(user_function)));
            Value::Unit
        }
        Expression::Tuple { .. } => todo!("tuples are not yet implemented in this position"),
        Expression::Identifier(identifier) => {
            if let Some(value) = environment.borrow().get(identifier) {
                // TODO: is cloning the value a good idea here??
                value.borrow().clone()
            } else {
                return Err(EvaluationError::syntax_error(
                    &format!("undefined variable {identifier}"),
                    expression_location.start,
                    expression_location.end,
                )
                .into());
            }
        }
        Expression::List { values } => {
            let mut values_out = VecDeque::new();
            let mut last_type = None;
            for expression in values {
                let v = evaluate_expression(expression, environment)?;
                if last_type.is_none() {
                    last_type = Some(ValueType::from(&v));
                } else if last_type != Some(ValueType::from(&v)) {
                    let add_type = ValueType::from(&v);
                    let last_type = last_type.unwrap();
                    return Err(EvaluationError::type_error(
                        &format!("cannot add {add_type} to a list of {last_type}",),
                        expression.start,
                        expression.end,
                    )
                    .into());
                }
                values_out.push_back(v);
            }
            Value::Sequence(Sequence::List(Rc::new(values_out)))
        }
        Expression::For {
            l_value,
            sequence,
            loop_body,
        } => {
            let Value::Sequence(sequence) = evaluate_expression(sequence, environment)? else {
                // TODO: fix this error
                panic!("can only iterate over sequences")
            };

            let Lvalue::Variable {
                identifier: var_name,
            } = l_value;

            match sequence {
                Sequence::String(str) => {
                    for n in 0..str.len() {
                        let mut scope = Environment::new_scope(environment);
                        let substr = &str[n..=n];

                        // TODO: allocating a new string here is probably not optimal
                        scope.borrow_mut().declare(
                            var_name,
                            Value::Sequence(Sequence::String(Rc::new(String::from(substr)))),
                        );

                        evaluate_expression(loop_body, &mut scope)?;
                    }
                }
                Sequence::List(xs) => {
                    for x in xs.iter() {
                        let mut scope = Environment::new_scope(environment);

                        // TODO: this clone here is probably not what we want
                        scope.borrow_mut().declare(var_name, x.clone());

                        evaluate_expression(loop_body, &mut scope)?;
                    }
                }
            }

            Value::Unit
        }
        Expression::Return { value } => {
            return Err(FunctionCarrier::Return(evaluate_expression(
                value,
                environment,
            )?));
        }
    };

    Ok(literal)
}

fn apply_operator(
    left: Value,
    operator: BinaryOperator,
    right: Value,
) -> Result<Value, EvaluationError> {
    let literal: Value = match (left, operator, right) {
        // Integer
        (Value::Number(a), op, Value::Number(b)) => match op {
            BinaryOperator::Equality => (a == b).into(),
            BinaryOperator::Inequality => (a != b).into(),
            BinaryOperator::Greater => (a > b).into(),
            BinaryOperator::GreaterEquals => (a >= b).into(),
            BinaryOperator::Less => (a < b).into(),
            BinaryOperator::LessEquals => (a <= b).into(),
            // Math operations
            BinaryOperator::Plus => Value::Number(a + b),
            BinaryOperator::Minus => Value::Number(a - b),
            BinaryOperator::Multiply => Value::Number(a * b),
            BinaryOperator::Divide => Value::Number(a / b),
            BinaryOperator::CModulo => Value::Number(a.rem(b)),
            BinaryOperator::EuclideanModulo => Value::Number(a.checked_rem_euclid(b)?),
            BinaryOperator::Exponent => Value::Number(a.checked_pow(b)?),
        },
        // Boolean
        (Value::Bool(a), BinaryOperator::Equality, Value::Bool(b)) => (a == b).into(),
        (Value::Bool(a), BinaryOperator::Inequality, Value::Bool(b)) => (a != b).into(),

        // Some mixed memes
        (Value::Sequence(Sequence::String(a)), BinaryOperator::Multiply, Value::Number(b)) => {
            Value::Sequence(Sequence::String(Rc::new(a.repeat(
                usize::try_from(b).map_err(|_err| {
                    EvaluationError::type_error(
                        "can't multiply a string with this value",
                        // TODO: somehow figure out the line number, or defer creation of this error to another location
                        Location { line: 0, column: 0 },
                        Location { line: 0, column: 0 },
                    )
                })?,
            ))))
        }
        (Value::Number(a), BinaryOperator::Multiply, Value::Sequence(Sequence::String(b))) => {
            Value::Sequence(Sequence::String(Rc::new(b.repeat(
                usize::try_from(a).map_err(|()| {
                    EvaluationError::type_error(
                        "can't multiply a string with this value",
                        // TODO: somehow figure out the line number, or defer creation of this error to another location
                        Location { line: 0, column: 0 },
                        Location { line: 0, column: 0 },
                    )
                })?,
            ))))
        }

        // String apply operators to string
        (Value::Sequence(Sequence::String(a)), op, Value::Sequence(Sequence::String(b))) => {
            let comp = a.cmp(&b);
            match op {
                BinaryOperator::Equality => (comp == Ordering::Equal).into(),
                BinaryOperator::Inequality => (comp != Ordering::Equal).into(),
                BinaryOperator::Greater => (comp == Ordering::Greater).into(),
                BinaryOperator::GreaterEquals => (comp != Ordering::Less).into(),
                BinaryOperator::Less => (comp == Ordering::Less).into(),
                BinaryOperator::LessEquals => (comp != Ordering::Greater).into(),
                BinaryOperator::Plus => {
                    Value::Sequence(Sequence::String(Rc::new(format!("{a}{b}"))))
                }
                _ => {
                    return Err(EvaluationError::type_error(
                        &format!("cannot apply operator {operator:?} to string and string"),
                        Location { line: 0, column: 0 },
                        Location { line: 0, column: 0 },
                    ));
                }
            }
        }

        (a, _op, b) => {
            return Err(EvaluationError::type_error(
                &format!(
                    "cannot apply operator {operator:?} to {} and {}",
                    ValueType::from(&a),
                    ValueType::from(&b)
                ),
                Location { line: 0, column: 0 },
                Location { line: 0, column: 0 },
            ));
        }
    };

    Ok(literal)
}

pub struct EvaluationError {
    text: String,
    start: Location,
    #[allow(unused)]
    end: Location,
}

impl EvaluationError {
    #[must_use]
    pub fn type_error(message: &str, start: Location, end: Location) -> Self {
        Self {
            text: format!("Type error: {message}"),
            start,
            end,
        }
    }
    #[must_use]
    pub fn syntax_error(message: &str, start: Location, end: Location) -> Self {
        Self {
            text: format!("Syntax error: {message}"),
            start,
            end,
        }
    }

    #[must_use]
    pub fn io_error(err: &std::io::Error, start: Location, end: Location) -> Self {
        Self {
            text: format!("IO error: {err}"),
            start,
            end,
        }
    }
}
impl From<std::io::Error> for EvaluationError {
    fn from(value: std::io::Error) -> Self {
        Self {
            text: format!("io error: {value:?}"),
            // TODO: fix start/end
            start: Location { line: 0, column: 0 },
            end: Location { line: 0, column: 0 },
        }
    }
}

impl fmt::Display for EvaluationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} on {}", self.text, self.start)
    }
}

impl fmt::Debug for EvaluationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl Error for EvaluationError {}
