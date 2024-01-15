use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::ast::ExpressionLocation;
use crate::interpreter::environment::{Environment, EnvironmentRef, InterpreterOutput};
use crate::interpreter::evaluate::{evaluate_expression, EvaluationError};
use crate::interpreter::value::Value;
use crate::lexer::{Lexer, TokenLocation};

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

        let final_value = self.interpret(statements.into_iter())?;

        Ok(format!("{final_value}"))
    }
    fn interpret(
        &mut self,
        expressions: impl Iterator<Item = ExpressionLocation>,
    ) -> Result<Value, EvaluationError> {
        let mut value = Value::Unit;
        for expr in expressions {
            value = evaluate_expression(&expr, &mut self.environment)?;
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
