mod environment;
mod evaluate;
mod function;
mod int;
mod num;
pub mod stdlib;
mod value;

pub use crate::interpreter::num::Number;
pub use crate::interpreter::value::{Sequence, Value, ValueType};
pub use evaluate::EvaluationError;
pub use function::Function;

use crate::ast::{Expression, ExpressionLocation, Lvalue};
use crate::interpreter::environment::Environment;
use crate::lexer::{Lexer, TokenLocation};

use crate::ast::{LogicalOperator, UnaryOperator};
use crate::interpreter::int::Int;
use std::fmt::{Display, Formatter};
use std::ops::Neg;

pub struct Interpreter<'a, W> {
    destination: &'a mut W,
    environment: Environment,
}

impl<'a, W: std::io::Write> Interpreter<'a, W> {
    pub fn new(dest: &'a mut W) -> Self {
        Self {
            destination: dest,
            environment: Environment::new_with_stdlib(),
        }
    }

    /// Parse and execute the string
    /// # Errors
    ///
    /// Will return an Interpreter error if Lexing, Parsing or Evaluation of the code failed. See [Error].
    pub fn run_str(&mut self, input: &str, debug: bool) -> Result<String, Error> {
        let scanner = Lexer::new(input);
        let tokens = scanner.collect::<Result<Vec<TokenLocation>, _>>()?;

        if debug {
            for token in &tokens {
                eprintln!("{token:?}");
            }
        }

        let mut parser = crate::ast::Parser::from_tokens(tokens);
        let statements = parser.parse()?;

        let final_value = self.interpret(statements.into_iter())?;

        Ok(format!("{final_value}"))
    }
    fn interpret(
        &mut self,
        expressions: impl Iterator<Item = ExpressionLocation>,
    ) -> Result<Value, EvaluationError> {
        let mut value = Value::Unit;
        for expr in expressions {
            value = self.evaluate_expression(&expr)?;
        }
        Ok(value)
    }

    #[allow(clippy::too_many_lines)]
    fn evaluate_expression(
        &mut self,
        expression_location: &ExpressionLocation,
    ) -> Result<Value, EvaluationError> {
        let (start, end) = (expression_location.start, expression_location.end);
        let literal: Value = match &expression_location.expression {
            Expression::Unary {
                expression: expression_location,
                operator,
            } => {
                let value = self.evaluate_expression(expression_location)?;
                match (value, operator) {
                    (Value::Number(n), UnaryOperator::Neg) => Value::Number(n.neg()),
                    (Value::Bool(b), UnaryOperator::Bang) => Value::Bool(!b),
                    (_, UnaryOperator::Bang) => {
                        return Err(EvaluationError::TypeError {
                            message: "the '!' operator cannot be applied to this type".to_string(),
                        });
                    }
                    (_, UnaryOperator::Neg) => {
                        return Err(EvaluationError::TypeError {
                            message: "this type cannot be negated".to_string(),
                        });
                    }
                }
            }
            Expression::Binary {
                left,
                operator: operator_token,
                right,
            } => {
                let left = self.evaluate_expression(left)?;
                let right = self.evaluate_expression(right)?;
                evaluate::apply_operator(left, *operator_token, right)?
            }
            Expression::Grouping(expr) => self.evaluate_expression(expr)?,
            Expression::Variable { ref identifier } => self
                .environment
                .get(identifier)
                .ok_or(EvaluationError::UndefinedVariable {
                    identifier: identifier.clone(),
                    start,
                    end,
                })?
                .clone(),
            Expression::VariableDeclaration {
                l_value: Lvalue::Variable { identifier },
                value,
            } => {
                let value = self.evaluate_expression(value)?;
                self.environment.declare(identifier, value.clone());
                value
            }
            Expression::VariableAssignment {
                l_value: Lvalue::Variable { identifier },
                value,
            } => {
                if !self.environment.contains(identifier) {
                    return Err(EvaluationError::UndefinedVariable {
                        identifier: identifier.clone(),
                        start,
                        end,
                    });
                }
                let value = self.evaluate_expression(value)?;
                self.environment.assign(identifier.clone(), value.clone());
                value
            }
            Expression::BlockExpression { statements } => {
                self.environment.new_scope();

                let mut value = Value::Unit;
                for stm in statements {
                    value = self.evaluate_expression(stm)?;
                }

                self.environment.destroy_scope();
                value
            }
            Expression::IfExpression {
                expression,
                on_true,
                on_false,
            } => {
                let result = self.evaluate_expression(expression)?;

                match (result, on_false) {
                    (Value::Bool(true), _) => self.evaluate_expression(on_true)?,
                    (Value::Bool(false), Some(block)) => self.evaluate_expression(block)?,
                    (Value::Bool(false), None) => Value::Unit,
                    (value, _) => {
                        return Err(EvaluationError::TypeError {
                            message: format!(
                                "mismatched types: expected bool, found {}",
                                ValueType::from(value)
                            ),
                        })
                    }
                }
            }
            Expression::Statement(expression) => {
                self.evaluate_expression(expression)?;
                Value::Unit
            }
            Expression::Print(expression) => {
                let value = self.evaluate_expression(expression)?;
                // TODO maybe don't use debug printing
                writeln!(self.destination, "{value}")?;
                Value::Unit
            }
            Expression::Logical {
                operator,
                left,
                right,
            } => {
                let left = self.evaluate_expression(left)?;
                match (operator, left) {
                    (LogicalOperator::And, Value::Bool(true)) => self.evaluate_expression(right)?,
                    (LogicalOperator::And, Value::Bool(false)) => Value::Bool(false),
                    (LogicalOperator::Or, Value::Bool(false)) => self.evaluate_expression(right)?,
                    (LogicalOperator::Or, Value::Bool(true)) => Value::Bool(true),
                    (LogicalOperator::And | LogicalOperator::Or, value) => {
                        return Err(EvaluationError::TypeError {
                            message: format!(
                                "Cannot apply logical operator to non bool value {}",
                                ValueType::from(value)
                            ),
                        })
                    }
                }
            }
            Expression::WhileExpression {
                expression,
                loop_body,
            } => {
                self.environment.new_scope();
                loop {
                    let lit = self.evaluate_expression(expression)?;
                    if let Value::Bool(true) = lit {
                        self.evaluate_expression(loop_body)?;
                    } else if let Value::Bool(false) = lit {
                        break;
                    } else {
                        return Err(EvaluationError::TypeError {
                            message: "Expression in a while structure must return a bool"
                                .to_string(),
                        });
                    }
                }
                self.environment.destroy_scope();
                Value::Unit
            }
            Expression::BoolLiteral(b) => Value::Bool(*b),
            Expression::StringLiteral(s) => Value::Sequence(Sequence::String(s.clone())),
            Expression::Int64Literal(n) => Value::Number(Number::Int(Int::Int64(*n))),
            Expression::BigIntLiteral(n) => Value::Number(Number::Int(Int::BigInt(n.clone()))),
            Expression::Float64Literal(n) => Value::Number(Number::Float(*n)),
            Expression::ComplexLiteral(n) => Value::Number(Number::Complex(*n)),
            Expression::Call {
                function,
                arguments,
            } => {
                // TODO: Maybe check if the function exists before we evaluate the arguments.
                //       the reason we're doing it in this order is to not have to fight the borrowchecker
                let mut evaluated_args = Vec::new();

                for argument in arguments {
                    evaluated_args.push(self.evaluate_expression(argument)?);
                }

                if let Some(Value::Function(function)) = self.environment.get(function) {
                    function.call(&evaluated_args)
                } else {
                    return Err(EvaluationError::UndefinedFunction {
                        identifier: function.clone(),
                    });
                }
            }
        };

        Ok(literal)
    }
}

#[derive(Debug)]
pub enum Error {
    Lexer { cause: crate::lexer::Error },
    Parser { cause: crate::ast::Error },
    Evaluation { cause: EvaluationError },
}

impl From<crate::lexer::Error> for Error {
    fn from(value: crate::lexer::Error) -> Self {
        Error::Lexer { cause: value }
    }
}

impl From<crate::ast::Error> for Error {
    fn from(value: crate::ast::Error) -> Self {
        Error::Parser { cause: value }
    }
}

impl From<EvaluationError> for Error {
    fn from(value: EvaluationError) -> Self {
        Error::Evaluation { cause: value }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Lexer { cause } => write!(f, "Lexer error: {cause}"),
            Error::Parser { cause } => write!(f, "Parser error: {cause}"),
            Error::Evaluation { cause } => write!(f, "Evaluation error: {cause}"),
        }
    }
}

impl std::error::Error for Error {}
