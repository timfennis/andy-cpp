use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::ast::ExpressionLocation;
use crate::interpreter::environment::{Environment, EnvironmentRef, InterpreterOutput};
use crate::interpreter::evaluate::{evaluate_expression, EvaluationError};
use crate::interpreter::function::FunctionCarrier;
use crate::interpreter::value::Value;
use crate::lexer::{Lexer, Location, TokenLocation};

pub mod environment;
pub mod evaluate;
pub mod function;
pub mod int;
pub mod num;
pub mod stdlib;
mod value;

pub struct Interpreter {
    environment: EnvironmentRef,
}

impl Interpreter {
    #[must_use]
    pub fn new(dest: Box<dyn InterpreterOutput>) -> Self {
        Self {
            environment: Rc::new(RefCell::new(Environment::new_with_stdlib(dest))),
        }
    }

    #[must_use]
    pub fn environment(self) -> EnvironmentRef {
        self.environment
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
        if debug {
            for s in &statements {
                dbg!(s);
            }
        }

        let final_value = self.interpret(statements.into_iter())?;

        Ok(format!("{final_value}"))
    }
    fn interpret(
        &mut self,
        expressions: impl Iterator<Item = ExpressionLocation>,
    ) -> Result<Value, EvaluationError> {
        let mut value = Value::Unit;
        for expr in expressions {
            match evaluate_expression(&expr, &mut self.environment) {
                Ok(val) => value = val,
                Err(FunctionCarrier::Return(_)) => {
                    return Err(EvaluationError::syntax_error(
                        "unexpected return statement outside of function body",
                        expr.start,
                        expr.end,
                    ));
                }
                Err(FunctionCarrier::EvaluationError(e)) => return Err(e),
                Err(FunctionCarrier::ArgumentError(msg)) => {
                    // TODO: coercing ArgumentError into a EvaluationError in this way is pretty cringe
                    return Err(EvaluationError::type_error(
                        &msg,
                        Location { line: 0, column: 0 },
                        Location { line: 0, column: 0 },
                    ));
                }
                _ => todo!("unimplemented"),
            }
        }
        Ok(value)
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
        Self::Lexer { cause: value }
    }
}

impl From<crate::ast::Error> for Error {
    fn from(value: crate::ast::Error) -> Self {
        Self::Parser { cause: value }
    }
}

impl From<EvaluationError> for Error {
    fn from(value: EvaluationError) -> Self {
        Self::Evaluation { cause: value }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lexer { cause } => write!(f, "Lexer error: {cause}"),
            Self::Parser { cause } => write!(f, "Parser error: {cause}"),
            Self::Evaluation { cause } => write!(f, "Evaluation error: {cause}"),
        }
    }
}

impl std::error::Error for Error {}
