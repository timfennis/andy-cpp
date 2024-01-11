use crate::ast::literal::Literal;
use crate::ast::parser::Error as ParseError;
use crate::lexer::{Location, Token, TokenLocation};

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    Bang,
    Neg,
}

impl TryFrom<TokenLocation> for UnaryOperator {
    type Error = ParseError;

    fn try_from(value: TokenLocation) -> Result<Self, Self::Error> {
        Ok(match value.token {
            Token::Minus => UnaryOperator::Neg,
            Token::Bang => UnaryOperator::Bang,
            _ => {
                return Err(ParseError::ExpectedToken {
                    expected_tokens: vec![Token::Minus, Token::Bang],
                    actual_token: value,
                })
            }
        })
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Operator {
    Equality,
    Inequality,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
    Plus,
    Minus,
    Multiply,
    Divide,
    CModulo,
    EuclideanModulo,
    Exponent,
}

impl TryFrom<TokenLocation> for Operator {
    type Error = ParseError;

    fn try_from(value: TokenLocation) -> Result<Self, Self::Error> {
        Ok(match value.token {
            Token::Equality => Operator::Equality,
            Token::Inequality => Operator::Inequality,
            Token::Greater => Operator::Greater,
            Token::GreaterEquals => Operator::GreaterEquals,
            Token::Less => Operator::Less,
            Token::LessEquals => Operator::LessEquals,
            Token::Plus => Operator::Plus,
            Token::Minus => Operator::Minus,
            Token::Multiply => Operator::Multiply,
            Token::Divide => Operator::Divide,
            Token::CModulo => Operator::CModulo,
            Token::EuclideanModulo => Operator::EuclideanModulo,
            Token::Exponent => Operator::Exponent,
            _ => {
                return Err(ParseError::ExpectedToken {
                    actual_token: value,
                    expected_tokens: vec![
                        Token::Equality,
                        Token::Inequality,
                        Token::Greater,
                        Token::GreaterEquals,
                        Token::Less,
                        Token::LessEquals,
                        Token::Plus,
                        Token::Minus,
                        Token::Multiply,
                        Token::Divide,
                        Token::CModulo,
                        Token::EuclideanModulo,
                        Token::Exponent,
                    ],
                })
            }
        })
    }
}

#[derive(Debug, Eq, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub struct ExpressionLocation {
    pub expression: Expression,
    pub start: Location,
    pub end: Location,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Lvalue {
    Variable { identifier: String },
}
#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    Statement(Box<ExpressionLocation>),
    Print(Box<ExpressionLocation>),
    Literal(Literal),
    Unary {
        operator: UnaryOperator,
        expression: Box<ExpressionLocation>,
    },
    Binary {
        left: Box<ExpressionLocation>,
        operator: Operator,
        right: Box<ExpressionLocation>,
    },
    Grouping(Box<ExpressionLocation>),
    Variable {
        identifier: String,
    },
    VariableDeclaration {
        l_value: Lvalue,
        value: Box<ExpressionLocation>,
    },
    VariableAssignment {
        l_value: Lvalue,
        value: Box<ExpressionLocation>,
    },
    BlockExpression {
        statements: Vec<ExpressionLocation>,
    },
    IfExpression {
        expression: Box<ExpressionLocation>,
        on_true: Box<ExpressionLocation>,
        on_false: Option<Box<ExpressionLocation>>,
    },
}

impl Expression {
    #[must_use]
    pub fn to_location(self, start: Location, end: Location) -> ExpressionLocation {
        ExpressionLocation {
            start,
            end,
            expression: self,
        }
    }
}

impl ExpressionLocation {
    #[must_use]
    pub fn to_statement(self) -> Self {
        Self {
            start: self.start,
            end: self.end,
            expression: Expression::Statement(Box::new(self)),
        }
    }
}
