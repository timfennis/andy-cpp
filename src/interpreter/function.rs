use std::fmt;
use std::rc::Rc;

use crate::ast::ExpressionLocation;
use crate::interpreter::environment::{Environment, EnvironmentRef};
use crate::interpreter::evaluate::{evaluate_expression, EvaluationError};
use crate::interpreter::value::{Value, ValueType};

pub type FunctionResult = Result<Value, FunctionError>;

pub trait Function: fmt::Debug {
    /// # Errors
    /// For now it'll return evaluation errors if the there are any during the execution of the function body
    fn call(&self, args: &[Value], env: &EnvironmentRef) -> FunctionResult;
}

pub struct Closure {
    pub parameters: Vec<String>,
    pub body: Rc<ExpressionLocation>,
    pub environment: EnvironmentRef,
}

impl Function for Closure {
    fn call(&self, args: &[Value], _env: &EnvironmentRef) -> FunctionResult {
        let mut local_scope = Environment::new_scope(&self.environment);

        let mut env = local_scope.borrow_mut();
        for (name, value) in self.parameters.iter().zip(args.iter()) {
            //TODO: is this clone a good plan?
            env.declare(name, value.clone());
        }
        // This drop is very important
        drop(env);

        let return_value = evaluate_expression(&self.body, &mut local_scope)?;

        // This explicit drops are probably not needed but who cares
        drop(local_scope);
        Ok(return_value)
    }
}

impl fmt::Debug for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.parameters)
    }
}

impl From<EvaluationError> for FunctionError {
    fn from(value: EvaluationError) -> Self {
        FunctionError::EvaluationError(Box::new(value))
    }
}

pub enum FunctionError {
    Return(Value),
    EvaluationError(Box<EvaluationError>),
    ArgumentError(String),
    IOError(std::io::Error),
}

impl FunctionError {
    #[must_use]
    pub fn argument_type_error(expected: &ValueType, actual: &ValueType) -> Self {
        Self::ArgumentError(format!("argument error: expected {expected} got {actual}"))
    }

    #[must_use]
    pub fn argument_count_error(expected: usize, actual: usize) -> Self {
        Self::ArgumentError(format!(
            "argument error: expected {expected} arguments got {actual}"
        ))
    }
}
impl fmt::Display for FunctionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionError::Return(_) => write!(f, "return is not an error"),
            FunctionError::EvaluationError(e) => write!(f, "{}", *e),
            FunctionError::ArgumentError(text) => write!(f, "{text}"),
            FunctionError::IOError(err) => write!(f, "{err}"),
        }
    }
}
