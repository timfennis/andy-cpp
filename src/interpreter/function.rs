use std::fmt::Debug;
use std::rc::Rc;

use crate::ast::ExpressionLocation;
use crate::interpreter::environment::Environment;
use crate::interpreter::evaluate::evaluate_expression;
use crate::interpreter::{EnvironmentRef, EvaluationError, Value};

pub trait Function: Debug {
    /// # Errors
    /// For now it'll return evaluation errors if the there are any during the execution of the function body
    fn call(&self, args: &[Value], env: &EnvironmentRef) -> Result<Value, EvaluationError>;
}

#[derive(Debug)]
pub struct UserFunction {
    pub parameters: Vec<String>,
    pub expression: Rc<ExpressionLocation>,
}

impl Function for UserFunction {
    fn call(&self, args: &[Value], env: &EnvironmentRef) -> Result<Value, EvaluationError> {
        let local_scope = Environment::new_scope_ref(env);

        let mut env = local_scope.borrow_mut();
        for (name, value) in self.parameters.iter().zip(args.iter()) {
            //TODO: is this clone a good plan?
            env.declare(name, value.clone());
        }
        drop(env);

        let return_value = evaluate_expression(&self.expression, &local_scope)?;

        // These explicit drops are probably not needed but who cares
        drop(local_scope);
        Ok(return_value)
    }
}
