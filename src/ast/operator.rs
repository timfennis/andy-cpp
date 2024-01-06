use std::fmt;

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Operator {
    // Assignment
    CreateVar,
    EqualsSign,

    // Comparison
    Equality,
    Inequality,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
    // Math
    Plus,
    Minus,
    Multiply,
    Divide,
    CModulo,
    EuclideanModulo,
    Exponent,
    // Unary
    Bang,
}

impl TryFrom<char> for Operator {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '-' => Ok(Operator::Minus),
            '+' => Ok(Operator::Plus),
            '*' => Ok(Operator::Multiply),
            '^' => Ok(Operator::Exponent),
            '%' => Ok(Operator::CModulo),
            '!' => Ok(Operator::Bang),
            '=' => Ok(Operator::EqualsSign),
            '>' => Ok(Operator::Greater),
            '<' => Ok(Operator::Less),
            '/' => Ok(Operator::Divide),
            _ => Err(()),
        }
    }
}

impl TryFrom<(char, Option<char>)> for Operator {
    type Error = ();

    fn try_from((cur, next): (char, Option<char>)) -> Result<Self, Self::Error> {
        let Some(next) = next else {
            return Err(());
        };
        match (cur, next) {
            ('%', '%') => Ok(Operator::EuclideanModulo),
            (':', '=') => Ok(Operator::CreateVar),
            ('=', '=') => Ok(Operator::Equality),
            ('!', '=') => Ok(Operator::Inequality),
            ('>', '=') => Ok(Operator::GreaterEquals),
            ('<', '=') => Ok(Operator::LessEquals),
            _ => Err(()),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                Operator::Equality => "==",
                Operator::Inequality => "!=",
                Operator::Plus => "+",
                Operator::Minus => "-",
                Operator::Multiply => "*",
                Operator::Divide => "/",
                Operator::Greater => ">",
                Operator::GreaterEquals => ">=",
                Operator::Less => "<",
                Operator::LessEquals => "<=",
                Operator::CModulo => "%",
                Operator::EuclideanModulo => "%%",
                Operator::CreateVar => ":=",
                Operator::EqualsSign => "=",
                Operator::Bang => "!",
                Operator::Exponent => "^",
            }
        )
    }
}
impl fmt::Debug for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "Operator({self})")
    }
}
