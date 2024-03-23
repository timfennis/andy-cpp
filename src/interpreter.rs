use std::cell::RefCell;
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

    // TODO: use thiserror to convert FunctionCarrier into InterpreterError
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
                Err(FunctionCarrier::EvaluationError(e)) => {
                    return Err(e);
                }
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

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Lexer error: {cause}")]
    Lexer {
        #[from]
        cause: crate::lexer::Error,
    },
    #[error("Parser error: {cause}")]
    Parser {
        #[from]
        cause: crate::ast::Error,
    },
    #[error("Evaluation error: {cause}")]
    Evaluation {
        #[from]
        cause: EvaluationError,
    },
}
