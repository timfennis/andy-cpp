use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use crate::ast::ExpressionLocation;
use crate::interpreter::environment::Environment;
use crate::interpreter::evaluate::evaluate_expression;
use crate::interpreter::{EvaluationError, Value};

pub trait Function: Debug {
    fn call(
        &self,
        args: &[Value],
        env: &Rc<RefCell<Environment>>,
    ) -> Result<Value, EvaluationError>;
}

#[derive(Debug)]
pub struct UserFunction {
    pub parameters: Vec<String>,
    pub expression: Rc<ExpressionLocation>,
}

impl Function for UserFunction {
    fn call(
        &self,
        args: &[Value],
        env: &Rc<RefCell<Environment>>,
    ) -> Result<Value, EvaluationError> {
        {
            let mut env = env.borrow_mut();
            env.new_scope();
            for (name, value) in self.parameters.iter().zip(args.iter()) {
                //TODO: is this clone a good plan?
                env.declare(name, value.clone());
            }
        }

        let return_value = evaluate_expression(&self.expression, env)?;
        env.borrow_mut().destroy_scope();
        Ok(return_value)
    }
}
