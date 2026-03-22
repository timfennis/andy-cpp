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
pub use iterator::{CombinationsIter, RepeatIter, SharedIterator, TakeIter, VmIterator};
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

    /// B2: Return at top level (frame_pointer == 0) previously caused usize underflow.
    #[test]
    fn test_return_at_top_level_returns_error() {
        use crate::chunk::{Chunk, OpCode};
        use crate::value::CompiledFunction;
        use ndc_core::StaticType;
        use ndc_lexer::Span;

        let dummy_span = Span::synthetic();
        let mut chunk = Chunk::default();
        chunk.write(OpCode::Return, dummy_span);
        let function = CompiledFunction {
            name: None,
            static_type: StaticType::Any,
            body: chunk,
            num_locals: 1, // pre-fills one stack slot so Return has a value to pop
        };
        let mut vm = crate::Vm::new(function, vec![]);
        assert!(
            vm.run().is_err(),
            "Return at top level should return a VmError"
        );
    }
}
