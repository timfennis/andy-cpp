use std::fmt;

#[derive(Eq, PartialEq, Clone)]
pub enum Literal {
    Integer(i64),
    String(String),
    True,
    False,
    Null,
}

impl Literal {
    pub fn type_name(&self) -> &'static str {
        match self {
            Literal::Integer(_) => "int",
            Literal::String(_) => "string",
            Literal::True | Literal::False => "bool",
            Literal::Null => "null"
        }
    }
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "Literal({})", self)
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Literal::Integer(n) => write!(f, "{n}"),
            Literal::String(val) => write!(f, "\"{val}\""),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Null => write!(f, "nil"),
        }
    }
}

impl From<bool> for Literal {
    fn from(value: bool) -> Self {
        match value {
            true => Literal::True,
            false => Literal::False,
        }
    }
}

impl From<i64> for Literal {
    fn from(value: i64) -> Self {
        Literal::Integer(value)
    }
}
