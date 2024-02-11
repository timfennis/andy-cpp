use crate::ast::operator::{BinaryOperator, LogicalOperator, UnaryOperator};
use crate::interpreter::evaluate::EvaluationError;
use crate::lexer::Location;
use num::complex::Complex64;
use num::BigInt;
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;

#[derive(PartialEq)]
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
    UnitLiteral,
    BoolLiteral(bool),
    StringLiteral(Rc<String>),
    Int64Literal(i64),
    Float64Literal(f64),
    BigIntLiteral(BigInt),
    ComplexLiteral(Complex64),
    Identifier(String),
    //
    Statement(Box<ExpressionLocation>),
    Unary {
        operator: UnaryOperator,
        expression: Box<ExpressionLocation>,
    },
    Binary {
        left: Box<ExpressionLocation>,
        operator: BinaryOperator,
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
        arguments: Box<ExpressionLocation>,
        body: Rc<ExpressionLocation>, //TODO: we probably made body an rc because Noulith does this, but why?
    },
    Block {
        statements: Vec<ExpressionLocation>,
    },
    If {
        expression: Box<ExpressionLocation>,
        on_true: Box<ExpressionLocation>,
        on_false: Option<Box<ExpressionLocation>>,
    },
    While {
        expression: Box<ExpressionLocation>,
        loop_body: Box<ExpressionLocation>,
    },
    For {
        l_value: Lvalue,
        sequence: Box<ExpressionLocation>,
        loop_body: Box<ExpressionLocation>,
    },
    Call {
        /// The function to call, could be an identifier, or any expression that produces a function as its value
        function: Box<ExpressionLocation>,
        arguments: Vec<ExpressionLocation>,
    },
    Tuple {
        values: Vec<ExpressionLocation>,
    },
    List {
        values: Vec<ExpressionLocation>,
    },
    Return {
        value: Box<ExpressionLocation>,
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
            _ => Err(EvaluationError::syntax_error(
                "expected identifier",
                self.start,
                self.end,
            )),
        }
    }

    /// # Errors
    /// If this expression cannot be converted into a tuple (or possibly another type that can be a valid parameter list) an `EvaluationError::InvalidExpression` will be returned
    pub fn try_into_parameters(&self) -> Result<Vec<String>, EvaluationError> {
        match &self.expression {
            Expression::Tuple {
                values: tuple_values,
            } => tuple_values
                .iter()
                .map(Self::try_into_identifier)
                .collect::<Result<Vec<String>, EvaluationError>>(),
            _ => Err(EvaluationError::syntax_error(
                "expected a parameter list",
                self.start,
                self.end,
            )),
        }
    }
}

impl fmt::Debug for ExpressionLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[{:?} on {}]", self.expression, self.start)
    }
}
