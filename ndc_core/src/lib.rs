mod node_id;
mod signature;
pub mod static_type;
pub mod r#struct;

pub use node_id::NodeId;
pub use static_type::{Parameter, StaticType, TypeSignature, TypeVar};
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
}
