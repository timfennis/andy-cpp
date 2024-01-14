use crate::ast::operator::{LogicalOperator, Operator, UnaryOperator};
use crate::interpreter::EvaluationError;
use crate::lexer::Location;
use num::complex::Complex64;
use num::BigInt;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
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
    ComplexLiteral(Complex64),
    Identifier(String),
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
    VariableDeclaration {
        l_value: Lvalue,
        value: Box<ExpressionLocation>,
    },
    VariableAssignment {
        l_value: Lvalue,
        value: Box<ExpressionLocation>,
    },
    FunctionDeclaration {
        name: Box<ExpressionLocation>,
        // TODO this probably not good enough
        arguments: Box<ExpressionLocation>,
        body: Rc<ExpressionLocation>,
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
        function_identifier: Box<ExpressionLocation>, // Name of the function
        arguments: Box<ExpressionLocation>,
    },
    Tuple {
        values: Vec<ExpressionLocation>,
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

    /// # Errors
    /// If this expression cannot be converted into an identifier an `EvaluationError::InvalidExpression` will be returned
    pub fn try_into_identifier(&self) -> Result<String, EvaluationError> {
        match &self.expression {
            Expression::Identifier(i) => Ok(i.clone()),
            _ => Err(EvaluationError::InvalidExpression {
                expected_type: String::from("identifier"),
                start: self.start,
                end: self.end,
            }),
        }
    }

    /// # Errors
    /// If this expression cannot be converted into an identifier an `EvaluationError::InvalidExpression` will be returned
    pub fn try_into_parameters(&self) -> Result<Vec<String>, EvaluationError> {
        match &self.expression {
            Expression::Tuple {
                values: tuple_values,
            } => tuple_values
                .iter()
                .map(ExpressionLocation::try_into_identifier)
                .collect::<Result<Vec<String>, EvaluationError>>(),
            _ => Err(EvaluationError::InvalidExpression {
                expected_type: String::from("parameter list"),
                start: self.start,
                end: self.end,
            }),
        }
    }
}
