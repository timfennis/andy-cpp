pub mod chunk;
pub mod compiler;
pub mod disassemble;
pub mod error;
pub mod iterator;
#[cfg(feature = "trace")]
#[allow(clippy::print_stderr)]
pub mod tracer;
pub mod value;
mod vm;

pub use vm::*;

pub use compiler::CompileError;
pub use error::VmError;
pub use iterator::{
    CombinationsIter, EnumerateIter, RepeatIter, SharedIterator, TakeIter, VmIterator,
};
pub use value::*;

#[cfg(test)]
mod test {

    #[test]
    fn test_that_value_size_does_not_change() {
        assert_eq!(size_of::<crate::Value>(), 16)
    }

    #[test]
    fn test_that_opcode_size_does_not_change() {
        // NOTE: this is allowed to change, but we'd like to know about it.
        assert_eq!(size_of::<crate::chunk::OpCode>(), 32)
    }

    /// A struct's constructor, getter, and setter must each display
    /// distinctly, so `ndc disassemble` output tells them apart.
    #[test]
    fn test_struct_function_display_is_unambiguous() {
        use crate::value::Function;
        use ndc_core::StaticType;
        use ndc_core::r#struct::StructRegistry;
        use std::rc::Rc;

        let mut registry = StructRegistry::default();
        let id = registry.register("Point", vec![("x".to_string(), StaticType::Int)]);
        let info = Rc::clone(&registry[id]);

        let constructor = Function::struct_constructor(Rc::clone(&info));
        let getter = Function::struct_getter(Rc::clone(&info), 0);
        let setter = Function::struct_setter(info, 0);

        assert_eq!(constructor.to_string(), "<fn Point>");
        assert_eq!(getter.to_string(), "<fn Point.x>");
        assert_eq!(setter.to_string(), "<fn Point.x=>");
    }
}
