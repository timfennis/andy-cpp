mod expression;
mod literal;
mod operator;
mod parser;

pub use expression::Expression;
pub use literal::Literal;
pub use operator::Operator;
pub use parser::Error;
pub(crate) use parser::Parser;

#[derive(Eq, PartialEq)]
pub enum Statement {
    Print(Expression),
    Expression(Expression),
}
