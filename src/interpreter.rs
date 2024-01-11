mod environment;
mod evaluate;

use crate::ast::{Expression, Literal, Operator};
use crate::interpreter::environment::Environment;
use crate::lexer::{Lexer, Token};
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
        let tokens = scanner.collect::<Result<Vec<Token>, _>>()?;

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
        expressions: impl Iterator<Item = Expression>,
    ) -> Result<Literal, EvaluationError> {
        // TODO: The interpreter defaults to returning unit if there are no statements, this makes no sense.
        let mut value = Literal::Unit;

        for expr in expressions {
            value = self.evaluate_expression(expr)?;
        }

        Ok(value)
    }

    fn evaluate_expression(&mut self, expr: Expression) -> Result<Literal, EvaluationError> {
        let literal = match expr {
            Expression::Literal(l) => l.clone(),
            Expression::Unary {
                expression,
                operator_token,
            } => {
                let value = self.evaluate_expression(*expression)?;
                match (value, operator_token.operator) {
                    (Literal::Integer(n), Operator::Minus) => Literal::Integer(n.neg()),
                    (Literal::True, Operator::Bang) => Literal::False,
                    (Literal::False, Operator::Bang) => Literal::True,
                    (_, Operator::Bang) => {
                        return Err(EvaluationError::TypeError {
                            message: "the '!' operator cannot be applied to this type".to_string(),
                        });
                    }
                    (_, Operator::Minus) => {
                        return Err(EvaluationError::TypeError {
                            message: "this type cannot be negated".to_string(),
                        });
                    }
                    _ => panic!("invalid unary operator encountered"),
                }
            }
            Expression::Binary {
                left,
                operator_token,
                right,
            } => {
                let left = self.evaluate_expression(*left)?;
                let right = self.evaluate_expression(*right)?;
                evaluate::apply_operator(left, operator_token, right)?
            }
            Expression::Grouping(expr) => self.evaluate_expression(*expr)?,
            Expression::Variable { token } => self
                .environment
                .get(&token.name)
                .ok_or(EvaluationError::UndefinedVariable { token })?
                // TODO: big FIXME, figure out if we can somehow return a reference instead of having to clone here
                //       does returning a reference make sense though since we're interested in the result at this point?
                .clone(),
            Expression::VariableDeclaration { token, value } => {
                let value = self.evaluate_expression(*value)?;
                self.environment.declare(&token.name, value.clone());
                value
            }
            Expression::VariableAssignment { token, value } => {
                let value = self.evaluate_expression(*value)?;
                if !self.environment.assign(&token.name, value.clone()) {
                    return Err(EvaluationError::UndefinedVariable { token });
                }
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
                let result = self.evaluate_expression(*expression)?;

                match (result, on_false) {
                    (Literal::True, _) => self.evaluate_expression(*on_true)?,
                    (Literal::False, Some(block)) => self.evaluate_expression(*block)?,
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
                self.evaluate_expression(*expression)?;
                Literal::Unit
            }
            Expression::Print(expression) => {
                let value = self.evaluate_expression(*expression)?;
                writeln!(self.destination, "{value}")?;
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
