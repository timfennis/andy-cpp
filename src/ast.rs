mod expression;
mod literal;
mod operator;
mod parser;

use crate::lexer::IdentifierToken;
pub use expression::Expression;
pub use literal::Literal;
pub use operator::Operator;
pub use parser::Error;
pub(crate) use parser::Parser;

pub enum Statement {
    Print(Expression),
    Expression(Expression),
    VariableDeclaration {
        identifier: IdentifierToken,
        expr: Expression,
    },
    VariableAssignment {
        identifier: IdentifierToken,
        expr: Expression,
    },
}
