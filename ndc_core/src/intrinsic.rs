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
    /// `t[1]` for a tuple `t` and an index written in the code: the element
    /// there, counting a negative index from the end. No identifier names it:
    /// the analyser picks it for such a read of `[]`, see [`TUPLE_INDEX`].
    TupleIndex,
}

/// The name of [`Intrinsic::TupleIndex`], which no identifier is spelled
/// like.
pub const TUPLE_INDEX: &str = "[tuple]";

impl Intrinsic {
    /// How many arguments a call of it takes.
    #[must_use]
    pub fn arity(self) -> usize {
        match self {
            Self::Cast | Self::TryCast => 1,
            Self::TupleIndex => 2,
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

impl IntrinsicFunction {
    /// The intrinsics of the language: `cast` and `cast?`, which every
    /// program can call, and the tuple index, which the analyser picks.
    #[must_use]
    pub fn all() -> Vec<Self> {
        let function = |name: &str, signature: &str, intrinsic, documentation: &str| Self {
            name: name.to_string(),
            documentation: Some(documentation.to_string()),
            static_type: StaticType::signature(signature)
                .expect("internal error: an intrinsic's signature parses"),
            intrinsic,
        };
        vec![
            function(
                "cast",
                "<T>(Any) -> T",
                Intrinsic::Cast,
                "Returns the value as a `T`: `cast::<Int>(value)`. It checks that the value is a `T` while the program runs, and never converts it. Parts of the value's type that aren't known yet are `T`'s, so `cast::<Float>(1)` is `1.0`.",
            ),
            function(
                "cast?",
                "<T>(Any) -> Option<T>",
                Intrinsic::TryCast,
                "Returns `Some(value)` if the value is a `T` and `None` if it isn't: `cast?::<Int>(value)`. Unlike `cast`, it only tests the value, which keeps its type.",
            ),
            function(
                TUPLE_INDEX,
                "<T, E>(T, Int) -> E",
                Intrinsic::TupleIndex,
                "Returns the element of a tuple at an index written in the code: `pair[1]`. A negative index counts from the end.",
            ),
        ]
    }
}
