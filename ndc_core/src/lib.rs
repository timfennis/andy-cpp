mod intrinsic;
mod node_id;
mod signature;
pub mod static_type;
pub mod r#struct;

pub use intrinsic::{Intrinsic, IntrinsicFunction};
pub use node_id::NodeId;
pub use static_type::{StaticType, TypeVar};
use std::slice::Iter;

/// The built-in functions a program can call: natives implemented in Rust,
/// and intrinsics, which the compiler writes as bytecode. In global slot
/// order: the natives, then the intrinsics.
pub struct FunctionRegistry<T> {
    functions: Vec<T>,
    intrinsics: Vec<IntrinsicFunction>,
}

impl<T> Default for FunctionRegistry<T> {
    fn default() -> Self {
        Self {
            functions: vec![],
            intrinsics: vec![],
        }
    }
}

impl<T> FunctionRegistry<T> {
    pub fn declare_global_fn(&mut self, f: T) {
        self.functions.push(f)
    }

    pub fn declare_intrinsic(&mut self, f: IntrinsicFunction) {
        self.intrinsics.push(f)
    }

    /// The natives, in registration order.
    pub fn iter(&'_ self) -> Iter<'_, T> {
        self.functions.iter()
    }

    /// The intrinsics, in registration order.
    pub fn intrinsics(&'_ self) -> Iter<'_, IntrinsicFunction> {
        self.intrinsics.iter()
    }
}

/// The name of the native that indexes a tuple or an `Any`, which the
/// analyser picks itself rather than as an overload of `[]`: no signature
/// says what a tuple's element at an index is, and as an overload it would
/// take a map indexed with a key of the wrong type. No identifier is spelled
/// like it.
pub const ANY_INDEX: &str = "[any]";
