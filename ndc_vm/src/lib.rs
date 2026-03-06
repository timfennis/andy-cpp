pub mod chunk;
pub mod compiler;
pub mod disassemble;
pub mod value;
pub mod vm;

pub use compiler::CompileError;
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
}
