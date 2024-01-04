mod expression;
mod literal;
mod operator;
mod parser;

use crate::lexer::TokenType;
pub use expression::Expression;
pub use literal::Literal;
pub use operator::Operator;
pub use parser::{Parser, ParserError};

pub enum Statement {
    Print(Expression),
    Expression(Expression),
    VariableDeclaration {
        identifier: String,
        expr: Expression,
    },
}
