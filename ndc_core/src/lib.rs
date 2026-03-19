pub mod compare;
pub mod hash_map;
pub mod int;
pub mod num;
pub mod static_type;

pub use static_type::{Parameter, StaticType, TypeSignature};
use std::slice::Iter;

pub struct FunctionRegistry<T> {
    functions: Vec<T>,
}

impl<T> Default for FunctionRegistry<T> {
    fn default() -> Self {
        Self { functions: vec![] }
    }
}

impl<T> FunctionRegistry<T> {
    pub fn declare_global_fn(&mut self, f: T) {
        self.functions.push(f)
    }

    pub fn iter(&'_ self) -> Iter<'_, T> {
        self.functions.iter()
    }

    pub fn take(&mut self) -> Vec<T> {
        std::mem::take(&mut self.functions)
    }
}
