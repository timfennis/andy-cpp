use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::ExpressionLocation;
use crate::interpreter::environment::{Environment, InterpreterOutput};
use crate::interpreter::evaluate::{EvaluationError, evaluate_expression};
use crate::interpreter::function::FunctionCarrier;
use crate::interpreter::semantic::analyser::{Analyser, ScopeTree};
use crate::interpreter::value::Value;
use crate::lexer::{Lexer, TokenLocation};
pub mod environment;
pub mod evaluate;
pub mod function;
pub(crate) mod heap;
pub mod int;
pub mod iterator;
pub mod num;
pub mod semantic;
pub mod sequence;
pub mod value;

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    analyser: Analyser,
}

#[allow(clippy::dbg_macro, clippy::print_stdout, clippy::print_stderr)]
impl Interpreter {
    #[must_use]
    pub fn new<T>(dest: T) -> Self
    where
        T: InterpreterOutput + 'static,
    {
        let environment = Environment::new_with_stdlib(Box::new(dest));
        let global_identifiers = environment.get_global_identifiers();

        Self {
            environment: Rc::new(RefCell::new(environment)),
            analyser: Analyser::from_scope_tree(ScopeTree::from_global_scope(global_identifiers)),
        }
    }

    #[must_use]
    pub fn environment(self) -> Rc<RefCell<Environment>> {
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

        let mut expressions = parser.parse()?;

        if debug {
            for s in &expressions {
                println!("{s:#?}");
            }
        }

        for e in &mut expressions {
            self.analyser.analyse(e)?;
        }

        //dbg!(&expressions);
        //dbg!(&self.analyser);

        let final_value = self.interpret(expressions.into_iter())?;

        if debug {
            dbg!(&final_value, final_value.static_type());
        }

        Ok(format!("{final_value}"))
    }

    fn interpret(
        &mut self,
        expressions: impl Iterator<Item = ExpressionLocation>,
    ) -> Result<Value, InterpreterError> {
        let mut value = Value::unit();
        for expr in expressions {
            match evaluate_expression(&expr, &self.environment) {
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
                Err(FunctionCarrier::Continue) => {
                    Err(EvaluationError::syntax_error(
                        "unexpected continue statement outside of loop body".to_string(),
                        expr.span,
                    ))?;
                }
                Err(FunctionCarrier::EvaluationError(e)) => return Err(InterpreterError::from(e)),
                r => {
                    panic!(
                        "internal error: unhandled function carrier variant returned from evaluate_expression"
                    );
                }
            }
        }
        Ok(value)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum InterpreterError {
    #[error("Error while lexing source")]
    Lexer {
        #[from]
        cause: crate::lexer::Error,
    },
    #[error("Error while parsing source")]
    Parser {
        #[from]
        cause: crate::ast::Error,
    },
    #[error("Error during static analysis")]
    Resolver {
        #[from]
        cause: semantic::analyser::AnalysisError,
    },
    #[error("Error while executing code")]
    Evaluation(#[from] EvaluationError),
}
