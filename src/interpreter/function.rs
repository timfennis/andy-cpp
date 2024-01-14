use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use crate::ast::ExpressionLocation;
use crate::interpreter::environment::Environment;
use crate::interpreter::evaluate::evaluate_expression;
use crate::interpreter::Value;

pub trait Function: Debug {
    fn call(&self, args: &[Value], env: &Rc<RefCell<Environment>>) -> Value;
}

#[derive(Debug)]
pub struct UserFunction {
    expression: ExpressionLocation,
}

impl Function for UserFunction {
    fn call(&self, _args: &[Value], env: &Rc<RefCell<Environment>>) -> Value {
        evaluate_expression(&self.expression, env)
            .expect("TODO: we need to change the signature of Function::call")
    }
}
