mod node_id;
mod signature;
pub mod static_type;
pub mod r#struct;

pub use node_id::NodeId;
pub use static_type::{StaticType, TypeVar};
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

/// The name of the native that indexes a tuple or an `Any`, which the
/// analyser picks itself rather than as an overload of `[]`: no signature
/// says what a tuple's element at an index is, and as an overload it would
/// take a map indexed with a key of the wrong type. No identifier is spelled
/// like it.
pub const ANY_INDEX: &str = "[any]";
