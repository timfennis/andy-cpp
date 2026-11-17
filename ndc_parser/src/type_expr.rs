use ndc_core::NodeId;
use ndc_lexer::Span;
use std::fmt;

/// A type annotation exactly as written in the source: `Int`, `Option<Int>`,
/// `[Int]`, `%{String: Int}`, `(Int, String)`. Purely syntactic — names are not validated and carry no
/// meaning here; every node keeps the span of the source text it was parsed from.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TypeExpr {
    /// A (possibly generic) named type: `Int`, `Option<Int>`.
    Name {
        /// Identity of this name, independent of its source location. In
        /// `Option<P>`, `Option` and `P` each have their own.
        id: NodeId,
        name: String,
        args: Vec<Self>,
        span: Span,
    },
    /// A list type: `[Int]`.
    List { element: Box<Self>, span: Span },
    /// A map type: `%{String: Int}`, or the set type `%{Int}`, which has no
    /// value type.
    Map {
        key: Box<Self>,
        value: Option<Box<Self>>,
        span: Span,
    },
    /// A tuple type: `(Int, String)` or the 1-tuple `(Int,)`. The unit type
    /// `()` is the empty tuple. `(Int)` is just `Int`, not a tuple.
    Tuple { elements: Vec<Self>, span: Span },
    /// A function type: `fn(Int, Int) -> Int`. Without `-> …` the function
    /// returns `()`.
    Function {
        parameters: Vec<Self>,
        return_type: Option<Box<Self>>,
        span: Span,
    },
}

impl TypeExpr {
    #[must_use]
    pub fn span(&self) -> Span {
        match self {
            Self::Name { span, .. }
            | Self::List { span, .. }
            | Self::Map { span, .. }
            | Self::Tuple { span, .. }
            | Self::Function { span, .. } => *span,
        }
    }
}

impl fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn join(f: &mut fmt::Formatter<'_>, items: &[TypeExpr]) -> fmt::Result {
            for (idx, item) in items.iter().enumerate() {
                if idx > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{item}")?;
            }
            Ok(())
        }

        match self {
            Self::Name { name, args, .. } => {
                write!(f, "{name}")?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    join(f, args)?;
                    write!(f, ">")?;
                }
                Ok(())
            }
            Self::List { element, .. } => write!(f, "[{element}]"),
            Self::Map { key, value, .. } => match value {
                Some(value) => write!(f, "%{{{key}: {value}}}"),
                None => write!(f, "%{{{key}}}"),
            },
            Self::Tuple { elements, .. } => {
                write!(f, "(")?;
                join(f, elements)?;
                if elements.len() == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")")
            }
            Self::Function {
                parameters,
                return_type,
                ..
            } => {
                write!(f, "fn(")?;
                join(f, parameters)?;
                write!(f, ")")?;
                match return_type {
                    Some(return_type) => write!(f, " -> {return_type}"),
                    None => Ok(()),
                }
            }
        }
    }
}
