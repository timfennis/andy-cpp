use std::fmt;

/// Enumeration representing literals in the Andy C++ programming language.
#[derive(Eq, PartialEq, Clone)]
pub enum Literal {
    /// Represents an integer literal.
    Integer(i64),

    /// Represents a string literal.
    String(String),

    /// Represents a unit `()` literal that represents the absence of any value.
    Unit,

    /// Represents the boolean literal `true`.
    True,

    /// Represents the boolean literal `false`.
    False,

    /// Represents the null literal.
    Null,
}

impl Literal {
    pub(crate) fn type_name(&self) -> &'static str {
        match self {
            Literal::Integer(_) => "int",
            Literal::String(_) => "string",
            Literal::True | Literal::False => "bool",
            Literal::Null => "null",
            Literal::Unit => "unit",
        }
    }
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Literal::Integer(n) => write!(f, "{n}"),
            Literal::String(val) => write!(f, "\"{val}\""),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Null => write!(f, "null"),
            Literal::Unit => write!(f, "()"),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Literal::Integer(n) => write!(f, "{n}"),
            Literal::String(val) => write!(f, "{val}"),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Null => write!(f, "null"),
            Literal::Unit => Ok(()),
        }
    }
}

impl From<bool> for Literal {
    fn from(value: bool) -> Self {
        if value {
            Literal::True
        } else {
            Literal::False
        }
    }
}

impl From<i64> for Literal {
    fn from(value: i64) -> Self {
        Literal::Integer(value)
    }
}

impl From<String> for Literal {
    fn from(value: String) -> Self {
        Literal::String(value)
    }
}
