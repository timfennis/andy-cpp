use crate::ast::operator::{LogicalOperator, Operator, UnaryOperator};
use crate::lexer::Location;
use num::{BigInt, Complex};
use std::rc::Rc;

#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
pub enum Expression {
    // Literals
    BoolLiteral(bool),
    StringLiteral(Rc<String>),
    Int64Literal(i64),
    Float64Literal(f64),
    BigIntLiteral(BigInt),
    ComplexLiteral(Complex<f64>),

    //
    Statement(Box<ExpressionLocation>),
    Print(Box<ExpressionLocation>),
    Unary {
        operator: UnaryOperator,
        expression: Box<ExpressionLocation>,
    },
    Binary {
        left: Box<ExpressionLocation>,
        operator: Operator,
        right: Box<ExpressionLocation>,
    },
    Logical {
        left: Box<ExpressionLocation>,
        operator: LogicalOperator,
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
    WhileExpression {
        expression: Box<ExpressionLocation>,
        loop_body: Box<ExpressionLocation>,
    },
    Call {
        function: String, // Name of the function
        arguments: Vec<ExpressionLocation>,
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
