use crate::interpreter::Value;
use std::fmt::Debug;

pub trait Function: Debug {
    fn call(&self, args: &[Value]) -> Value;
}
