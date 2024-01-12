mod environment;
mod evaluate;

use crate::ast::{Expression, ExpressionLocation, Literal, LogicalOperator, Lvalue, UnaryOperator};
use crate::interpreter::environment::Environment;
use crate::lexer::{Lexer, TokenLocation};
pub use evaluate::EvaluationError;
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
            environment: Environment::default(),
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

        Ok(format!("{final_value:?}"))
    }
    fn interpret(
        &mut self,
        expressions: impl Iterator<Item = ExpressionLocation>,
    ) -> Result<Literal, EvaluationError> {
        let mut value = Literal::Unit;
        for expr in expressions {
            value = self.evaluate_expression(&expr)?;
        }
        Ok(value)
    }

    #[allow(clippy::too_many_lines)]
    fn evaluate_expression(
        &mut self,
        expression_location: &ExpressionLocation,
    ) -> Result<Literal, EvaluationError> {
        let (start, end) = (expression_location.start, expression_location.end);
        let literal = match &expression_location.expression {
            Expression::Literal(l) => l.clone(),
            Expression::Unary {
                expression: expression_location,
                operator,
            } => {
                let value = self.evaluate_expression(expression_location)?;
                match (value, operator) {
                    (Literal::Integer(n), UnaryOperator::Neg) => Literal::Integer(n.neg()),
                    (Literal::True, UnaryOperator::Bang) => Literal::False,
                    (Literal::False, UnaryOperator::Bang) => Literal::True,
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

                let mut value = Literal::Unit;
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
                    (Literal::True, _) => self.evaluate_expression(on_true)?,
                    (Literal::False, Some(block)) => self.evaluate_expression(block)?,
                    (Literal::False, None) => Literal::Unit,
                    (literal, _) => {
                        return Err(EvaluationError::TypeError {
                            message: format!(
                                "mismatched types: expected bool, found {}",
                                literal.type_name()
                            ),
                        })
                    }
                }
            }
            Expression::Statement(expression) => {
                self.evaluate_expression(expression)?;
                Literal::Unit
            }
            Expression::Print(expression) => {
                let value = self.evaluate_expression(expression)?;
                writeln!(self.destination, "{value}")?;
                Literal::Unit
            }
            Expression::Logical {
                operator,
                left,
                right,
            } => {
                let left = self.evaluate_expression(left)?;
                match (operator, left) {
                    (LogicalOperator::And, Literal::True) => self.evaluate_expression(right)?,
                    (LogicalOperator::And, Literal::False) => Literal::False,
                    (LogicalOperator::Or, Literal::False) => self.evaluate_expression(right)?,
                    (LogicalOperator::Or, Literal::True) => Literal::True,
                    (LogicalOperator::And | LogicalOperator::Or, literal) => {
                        return Err(EvaluationError::TypeError {
                            message: format!(
                                "Cannot apply logical operator to non bool value {}",
                                literal.type_name()
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
                    if lit == Literal::True {
                        self.evaluate_expression(loop_body)?;
                    } else if lit == Literal::False {
                        break;
                    } else {
                        return Err(EvaluationError::TypeError {
                            message: "Expression in a while structure must return a bool"
                                .to_string(),
                        });
                    }
                }
                self.environment.destroy_scope();
                Literal::Unit
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
