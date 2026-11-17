use crate::StaticType;

/// A built-in function the compiler writes as bytecode at each call, instead
/// of calling a Rust implementation: `cast::<Int>(x)` checks `x` in place.
/// Used as a value, as in `xs.map(cast::<Int>)`, it's a small function the
/// compiler makes around the same bytecode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Intrinsic {
    /// `cast::<T>(x)`: `x`, checked to be a `T`.
    Cast,
    /// `cast?::<T>(x)`: `Some(x)` if `x` is a `T`, otherwise `None`.
    TryCast,
}

impl Intrinsic {
    /// How many arguments a call of it takes.
    #[must_use]
    pub fn arity(self) -> usize {
        match self {
            Self::Cast | Self::TryCast => 1,
        }
    }
}

/// An intrinsic as a program sees it: what it's called, its signature and
/// its documentation, as a native has.
#[derive(Debug, Clone)]
pub struct IntrinsicFunction {
    pub name: String,
    pub documentation: Option<String>,
    pub static_type: StaticType,
    pub intrinsic: Intrinsic,
}
