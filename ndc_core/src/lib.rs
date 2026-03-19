pub mod compare;
pub mod hash_map;
pub mod int;
pub mod num;
pub mod static_type;

pub use static_type::{Parameter, StaticType, TypeSignature};

pub struct FunctionRegistry<T> {
    functions: Vec<T>,
}

impl<T> FunctionRegistry<T> {
    fn declare_global_fn(&mut self, f: T) {
        self.functions.push(f)
    }
}
