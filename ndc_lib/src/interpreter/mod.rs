use std::cell::RefCell;
use std::rc::Rc;

use miette::Diagnostic;

use crate::ast::ExpressionLocation;
use crate::interpreter::environment::{Environment, EnvironmentRef, InterpreterOutput};
use crate::interpreter::evaluate::{EvaluationError, evaluate_expression};
use crate::interpreter::function::FunctionCarrier;
use crate::interpreter::value::Value;
use crate::lexer::{Lexer, TokenLocation};

pub mod environment;
pub mod evaluate;
pub mod function;
pub(crate) mod heap;
pub mod int;
pub mod iterator;
pub mod num;
pub mod sequence;
pub mod value;

pub struct Interpreter {
    environment: EnvironmentRef,
}

#[allow(clippy::dbg_macro, clippy::print_stdout, clippy::print_stderr)]
impl Interpreter {
    #[must_use]
    pub fn new<T>(dest: T) -> Self
    where
        T: InterpreterOutput + 'static,
    {
        Self {
            environment: Rc::new(RefCell::new(Environment::new_with_stdlib(Box::new(dest)))),
        }
    }

    #[must_use]
    pub fn environment(self) -> EnvironmentRef {
        self.environment
    }

    pub fn run_str(&mut self, input: &str, debug: bool) -> Result<String, InterpreterError> {
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
                println!("{s:#?}");
            }
        }

        let final_value = self.interpret(statements.into_iter())?;

        if debug {
            dbg!(&final_value, final_value.value_type());
        }

        Ok(format!("{final_value}"))
    }

    fn interpret(
        &mut self,
        expressions: impl Iterator<Item = ExpressionLocation>,
    ) -> Result<Value, InterpreterError> {
        let mut value = Value::unit();
        for expr in expressions {
            match evaluate_expression(&expr, &mut self.environment) {
                Ok(val) => value = val,
                Err(FunctionCarrier::Return(_)) => {
                    Err(EvaluationError::syntax_error(
                        "unexpected return statement outside of function body".to_string(),
                        expr.span,
                    ))?;
                }
                Err(FunctionCarrier::Break(_)) => {
                    Err(EvaluationError::syntax_error(
                        "unexpected break statement outside of loop body".to_string(),
                        expr.span,
                    ))?;
                }
                Err(FunctionCarrier::EvaluationError(e)) => return Err(InterpreterError::from(e)),
                _ => {
                    panic!(
                        "internal error: unhandled function carrier variant returned from evaluate_expression"
                    );
                }
            }
        }
        Ok(value)
    }
}

#[derive(thiserror::Error, Diagnostic, Debug)]
pub enum InterpreterError {
    #[error("Error while lexing source")]
    #[diagnostic(transparent)]
    Lexer {
        #[from]
        cause: crate::lexer::Error,
    },
    #[error("Error while parsing source")]
    #[diagnostic(transparent)]
    Parser {
        #[from]
        cause: crate::ast::Error,
    },
    #[error("Error while executing code")]
    #[diagnostic(transparent)]
    Evaluation(#[from] EvaluationError),
}
