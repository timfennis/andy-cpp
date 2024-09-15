use std::cell::RefCell;
use std::rc::Rc;

use miette::Diagnostic;

use crate::ast::ExpressionLocation;
use crate::interpreter::environment::{Environment, EnvironmentRef, InterpreterOutput};
use crate::interpreter::evaluate::{evaluate_expression, EvaluationError};
use crate::interpreter::function::FunctionCarrier;
use crate::interpreter::value::Value;
use crate::lexer::{Lexer, TokenLocation};

pub mod environment;
pub mod evaluate;
pub mod function;
pub mod int;
pub mod iterator;
pub mod num;
pub mod sequence;
pub mod value;

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
                dbg!(s);
            }
        }

        let final_value = self.interpret(statements.into_iter())?;

        if debug {
            dbg!(&final_value, final_value.value_type());
        }

        Ok(format!("{final_value}"))
    }

    /// This function is dumb, but I want to have it for now
    #[allow(unused)]
    fn print_functions(&self) {
        self.environment.borrow().print_functions();
    }

    fn interpret(
        &mut self,
        expressions: impl Iterator<Item = ExpressionLocation>,
    ) -> Result<Value, InterpreterError> {
        let mut value = Value::Unit;
        for expr in expressions {
            match evaluate_expression(&expr, &mut self.environment) {
                Ok(val) => value = val,
                Err(FunctionCarrier::Return(_)) => {
                    Err(EvaluationError::syntax_error(
                        "unexpected return statement outside of function body",
                        expr.span,
                    ))?;
                }
                Err(FunctionCarrier::Break(_)) => {
                    Err(EvaluationError::syntax_error(
                        "unexpected break statement outside of loop body",
                        expr.span,
                    ))?;
                }
                Err(e) => Err(e)?,
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

/// This trait converts a `FunctionError` into an `InterpreterError` but the callee needs to ensure the variant is already `FunctionCarrier::EvaluationError`
// TODO: maybe we can change this into a `TryFrom` that can fail en replace all the calls to make the interpreter not panic if there is a bug
impl From<FunctionCarrier> for InterpreterError {
    fn from(value: FunctionCarrier) -> Self {
        match value {
            FunctionCarrier::Return(_) => panic!("attempted to convert return to Error"),
            FunctionCarrier::Break(_) => panic!("attempted to convert break to Error"),
            FunctionCarrier::EvaluationError(e) => e.into(),
            FunctionCarrier::FunctionNotFound => panic!("attempted to convert FunctionNotFound to Error without line number info"),
            FunctionCarrier::IntoEvaluationError(_) => panic!("attempted to convert incomplete EvaluationError into Error without line number info"),
        }
    }
}
