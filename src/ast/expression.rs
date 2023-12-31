use crate::ast::literal::Literal;
use crate::ast::Statement;
use crate::lexer::{IdentifierToken, OperatorToken};
use std::fmt;

#[derive(Eq, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Unary {
        operator_token: OperatorToken,
        expression: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator_token: OperatorToken,
        right: Box<Expression>,
    },
    Grouping(Box<Expression>),
    Variable {
        token: IdentifierToken,
    },
    VariableDeclaration {
        token: IdentifierToken,
        value: Box<Expression>,
    },
    VariableAssignment {
        token: IdentifierToken,
        value: Box<Expression>,
    },
    BlockExpression {
        statements: Vec<Statement>,
    },
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Expression::Literal(lit) => write!(f, "{lit}"),
            Expression::Unary {
                operator_token,
                expression,
            } => write!(f, "({} {:?})", operator_token.operator, expression),
            Expression::Binary {
                left,
                operator_token,
                right,
            } => write!(f, "({} {:?} {:?})", operator_token.operator, left, right),
            Expression::Grouping(expr) => write!(f, "(group {expr:?})"),
            Expression::Variable { token } => write!(f, "{}", token.name),
            _ => write!(f, "TODO"),
        }
    }
}
