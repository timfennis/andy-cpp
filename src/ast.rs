use crate::lexer;
use std::fmt;

enum ParserError {
    InvalidOperator(lexer::TokenType),
}
enum Operator {
    Equals,
    Plus,
    Minus,
    Multiply,
    Divide,
}

/// Example of what converting tokens to AST operators could look like
impl TryFrom<lexer::TokenType> for Operator {
    type Error = ParserError;

    fn try_from(value: lexer::TokenType) -> Result<Self, Self::Error> {
        match value {
            lexer::TokenType::Minus => Ok(Operator::Minus),
            token => Err(ParserError::InvalidOperator(token)),
        }
    }
}

impl fmt::Debug for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                Operator::Equals => "-",
                Operator::Plus => "+",
                Operator::Minus => "-",
                Operator::Multiply => "*",
                Operator::Divide => "/",
            }
        )
    }
}

enum Literal {
    Number(i64),
    String(String),
    True,
    False,
    Nil,
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Literal::Number(n) => write!(f, "{n}"),
            Literal::String(val) => write!(f, "\"{val}\""),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

enum UnaryOperator {
    Neg,
    Not,
}

impl fmt::Debug for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            UnaryOperator::Neg => write!(f, "-"),
            UnaryOperator::Not => write!(f, "!"),
        }
    }
}

enum Expression {
    Literal(Literal),
    Unary {
        operator: UnaryOperator,
        expression: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
    Grouping(Box<Expression>),
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Expression::Literal(lit) => write!(f, "{:?}", lit),
            Expression::Unary {
                operator,
                expression,
            } => write!(f, "({:?} {:?})", operator, expression),
            Expression::Binary {
                left,
                operator,
                right,
            } => write!(f, "({:?} {:?} {:?})", operator, left, right),
            Expression::Grouping(expr) => write!(f, "(group {:?})", expr),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn printing() {
        let ast = Expression::Binary {
            left: Box::new(Expression::Unary {
                operator: UnaryOperator::Neg,
                expression: Box::new(Expression::Literal(Literal::Number(123))),
            }),
            operator: Operator::Multiply,
            right: Box::new(Expression::Grouping(Box::new(Expression::Literal(
                Literal::Number(69),
            )))),
        };

        assert_eq!(format!("{:?}", ast), "(* (- 123) (group 69))");
    }
}
