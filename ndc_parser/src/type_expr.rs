use ndc_lexer::Span;
use std::fmt;

/// A type annotation exactly as written in the source: `Int`, `Map<String, Int>`,
/// `(Int, String)`. Purely syntactic — names are not validated and carry no
/// meaning here; every node keeps the span of the source text it was parsed from.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TypeExpr {
    /// A (possibly generic) named type: `Int`, `List<Int>`, `Map<String, Int>`.
    Name {
        name: String,
        args: Vec<Self>,
        span: Span,
    },
    /// A tuple type: `(Int, String)`. The unit type `()` is the empty tuple.
    Tuple { elements: Vec<Self>, span: Span },
}

impl TypeExpr {
    #[must_use]
    pub fn span(&self) -> Span {
        match self {
            Self::Name { span, .. } | Self::Tuple { span, .. } => *span,
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
            Self::Tuple { elements, .. } => {
                write!(f, "(")?;
                join(f, elements)?;
                write!(f, ")")
            }
        }
    }
}
