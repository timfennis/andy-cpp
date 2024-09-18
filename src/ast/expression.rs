use crate::ast::operator::{BinaryOperator, LogicalOperator, UnaryOperator};
use crate::ast::parser::Error as ParseError;
use crate::interpreter::evaluate::EvaluationError;
use crate::lexer::Span;
use either::Either;
use num::complex::Complex64;
use num::BigInt;
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;

#[derive(Eq, PartialEq)]
pub struct ExpressionLocation {
    pub expression: Expression,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    // Literals
    UnitLiteral,
    BoolLiteral(bool),
    StringLiteral(String),
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
    Assignment {
        l_value: Lvalue,
        r_value: Box<ExpressionLocation>,
    },
    OpAssignment {
        l_value: Lvalue,
        value: Box<ExpressionLocation>,
        operation: Either<BinaryOperator, String>,
    },
    FunctionDeclaration {
        name: Box<ExpressionLocation>,
        arguments: Box<ExpressionLocation>,
        body: Rc<ExpressionLocation>, //TODO what happens if we remove the Rc?
    },
    Block {
        statements: Vec<ExpressionLocation>,
    },
    If {
        condition: Box<ExpressionLocation>,
        on_true: Box<ExpressionLocation>,
        on_false: Option<Box<ExpressionLocation>>,
    },
    While {
        expression: Box<ExpressionLocation>,
        loop_body: Box<ExpressionLocation>,
    },
    For {
        iterations: Vec<ForIteration>,
        body: Box<ForBody>,
    },
    Call {
        /// The function to call, could be an identifier, or any expression that produces a function as its value
        function: Box<ExpressionLocation>,
        arguments: Vec<ExpressionLocation>,
    },
    Index {
        value: Box<ExpressionLocation>,
        index: Box<ExpressionLocation>,
    },
    Tuple {
        values: Vec<ExpressionLocation>,
    },
    List {
        values: Vec<ExpressionLocation>,
    },
    Map {
        values: Vec<(ExpressionLocation, Option<ExpressionLocation>)>,
        default: Option<Box<ExpressionLocation>>,
    },
    Return {
        value: Box<ExpressionLocation>,
    },
    Break,
    RangeInclusive {
        start: Option<Box<ExpressionLocation>>,
        end: Option<Box<ExpressionLocation>>,
    },
    RangeExclusive {
        start: Option<Box<ExpressionLocation>>,
        end: Option<Box<ExpressionLocation>>,
    },
}

#[derive(Debug, Eq, PartialEq)]
pub enum ForIteration {
    Iteration {
        l_value: Lvalue,
        sequence: ExpressionLocation,
    },
    Guard(ExpressionLocation),
}

#[derive(Debug, Eq, PartialEq)]
pub enum ForBody {
    Block(ExpressionLocation),
    List(ExpressionLocation),
    Map {
        key: ExpressionLocation,
        value: Option<ExpressionLocation>,
        default: Option<Box<ExpressionLocation>>,
    },
}

#[derive(Debug, Eq, PartialEq)]
pub enum Lvalue {
    // Example: `foo := ...`
    Variable {
        identifier: String,
    },
    // Example: `foo()[1] = ...`
    Index {
        value: Box<ExpressionLocation>,
        index: Box<ExpressionLocation>,
    },
    // Example: `a, b := ...`
    Sequence(Vec<Lvalue>),
}

impl Eq for Expression {}

impl Expression {
    #[must_use]
    pub fn to_location(self, span: Span) -> ExpressionLocation {
        ExpressionLocation {
            expression: self,
            span,
        }
    }
}

impl ExpressionLocation {
    #[must_use]
    pub fn to_statement(self) -> Self {
        Self {
            span: self.span,
            expression: Expression::Statement(Box::new(self)),
        }
    }

    /// # Errors
    /// If this expression cannot be converted into an identifier an `EvaluationError::InvalidExpression` will be returned
    pub fn try_into_identifier(&self) -> Result<String, EvaluationError> {
        match &self.expression {
            Expression::Identifier(i) => Ok(i.clone()),
            _ => Err(EvaluationError::syntax_error(
                "expected identifier".to_string(),
                self.span,
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
                "expected a parameter list".to_string(),
                self.span,
            )),
        }
    }

    /// If this `ExpressionLocation` is a tuple expression with length one, it returns the
    /// `ExpressionLocation` inside the tuple.
    #[must_use]
    pub fn simplify(self) -> ExpressionLocation {
        match self {
            ExpressionLocation {
                expression: Expression::Tuple { mut values },
                ..
            // } if values.len() == 1 => values.remove(0).simplify(),
            } if values.len() == 1 => values.remove(0),
            tuple @ ExpressionLocation { .. } => tuple,
        }
    }
}

impl Lvalue {
    /// Returns the type name for this Lvalue, this can be used to serialize expression in error messages
    /// ```
    /// use ndc_lib::ast::Lvalue;
    /// let l = Lvalue::Variable { identifier: "foo".to_string() };
    /// assert_eq!(l.expression_type_name(), "variable");
    /// ```
    #[must_use]
    pub fn expression_type_name(&self) -> &str {
        match self {
            Lvalue::Variable { .. } => "variable",
            Lvalue::Index { .. } => "index expression",
            Lvalue::Sequence(_) => "destructure pattern", // ??
        }
    }

    #[must_use]
    pub fn can_build_from_expression(expression: &Expression) -> bool {
        match expression {
            Expression::Identifier(_) | Expression::Index { .. } => true,
            Expression::List { values } | Expression::Tuple { values } => values
                .iter()
                .all(|el| Self::can_build_from_expression(&el.expression)),
            Expression::Grouping(inner) => Self::can_build_from_expression(&inner.expression),
            _ => false,
        }
    }
}

impl TryFrom<ExpressionLocation> for Lvalue {
    type Error = ParseError;

    fn try_from(value: ExpressionLocation) -> Result<Self, Self::Error> {
        match value.expression {
            Expression::Identifier(identifier) => Ok(Lvalue::Variable { identifier }),
            Expression::Index { value, index } => Ok(Lvalue::Index { value, index }),
            Expression::List { values } | Expression::Tuple { values } => Ok(Lvalue::Sequence(
                values
                    .into_iter()
                    .map(Lvalue::try_from)
                    .collect::<Result<Vec<Self>, Self::Error>>()?,
            )),
            Expression::Grouping(value) => Ok(Lvalue::Sequence(vec![Self::try_from(*value)?])),
            _expr => Err(ParseError::text("invalid l-value".to_string(), value.span)),
        }
    }
}

impl fmt::Debug for ExpressionLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{{:?} at {:?}}}", self.expression, self.span)
    }
}
