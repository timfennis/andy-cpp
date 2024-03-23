use std::rc::Rc;

use crate::ast::ExpressionLocation;
use crate::interpreter::environment::{Environment, EnvironmentRef};
use crate::interpreter::evaluate::{evaluate_expression, EvaluationError, EvaluationResult};
use crate::interpreter::num::{Number, NumberType};
use crate::interpreter::value::{Value, ValueType};
use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug)] // TODO: create a sane implementation
pub struct OverloadedFunction {
    implementations: HashMap<Vec<ValueType>, Function>,
}

impl OverloadedFunction {
    pub fn call(&self, args: &[Value], env: &EnvironmentRef) -> EvaluationResult {
        let types = args.iter().map(Value::value_type).collect::<Vec<_>>();

        if self.implementations.len() == 1 {
            let imp = self.implementations.iter().next().unwrap().1;
            imp.call(args, env)
        } else {
            todo!()
        }
    }
}

// TODO: does it make sense to have this since we need to have merge logic somehwere
impl From<Function> for OverloadedFunction {
    fn from(value: Function) -> Self {
        Self {
            // TODO: actually bind the implementation correctly
            implementations: HashMap::from([(vec![], value)]),
        }
    }
}
#[derive(Clone)]
pub enum Function {
    Closure {
        parameter_names: Vec<String>,
        body: Rc<ExpressionLocation>,
        environment: EnvironmentRef,
    },
    SingleNumberFunction {
        body: fn(number: Number) -> Number,
    },
    GenericFunction {
        function: fn(&[Value], &EnvironmentRef) -> EvaluationResult,
    },
}

impl Function {
    pub fn call(&self, args: &[Value], env: &EnvironmentRef) -> EvaluationResult {
        match self {
            Function::Closure {
                body,
                environment,
                parameter_names: parameters,
            } => {
                let mut local_scope = Environment::new_scope(environment);

                let mut env = local_scope.borrow_mut();
                for (name, value) in parameters.iter().zip(args.iter()) {
                    //TODO: is this clone a good plan?
                    env.declare(name, value.clone());
                }
                // This drop is very important
                drop(env);

                let return_value = match evaluate_expression(body, &mut local_scope) {
                    Err(FunctionCarrier::Return(v)) | Ok(v) => v,
                    e => return e,
                };

                // This explicit drops are probably not needed but who cares
                drop(local_scope);
                Ok(return_value)
            }
            Function::SingleNumberFunction { body } => match args {
                [Value::Number(num)] => Ok(Value::Number((body)(num.clone()))),
                [v] => Err(FunctionCarrier::argument_type_error(
                    &ValueType::Number(NumberType::Float),
                    &v.value_type(),
                )),
                _ => Err(FunctionCarrier::argument_count_error(1, 0)),
            },
            Function::GenericFunction { function } => function(args, env),
        }
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "function")
    }
}

// Named after the Carrier trait
#[derive(thiserror::Error, Debug)]
pub enum FunctionCarrier {
    #[error("not an error")]
    Return(Value),
    #[error("evaluation error {0}")]
    EvaluationError(#[from] EvaluationError),
    #[error("argument error {0}")]
    ArgumentError(String),
    #[error("IO Error: {0}")]
    IOError(#[from] std::io::Error),
}

impl FunctionCarrier {
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
