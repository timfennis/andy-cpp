use crate::ast::operator::LogicalOperator;
use crate::ast::parser::Error as ParseError;
use crate::interpreter::evaluate::EvaluationError;
use crate::interpreter::function::StaticType;
use crate::lexer::Span;
use num::BigInt;
use num::complex::Complex64;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Binding {
    None,
    Resolved(ResolvedVar),
    Dynamic(Vec<ResolvedVar>), // figure it out at runtime
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ResolvedVar {
    Captured { depth: usize, slot: usize },
    Global { slot: usize },
}

#[derive(Eq, PartialEq, Clone)]
pub struct ExpressionLocation {
    pub expression: Expression,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    // Literals
    BoolLiteral(bool),
    StringLiteral(String),
    Int64Literal(i64),
    Float64Literal(f64),
    BigIntLiteral(BigInt),
    ComplexLiteral(Complex64),
    Identifier {
        name: String,
        resolved: Binding,
    },
    Statement(Box<ExpressionLocation>),
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
        r_value: Box<ExpressionLocation>,
        operation: String,
        resolved_assign_operation: Option<ResolvedVar>,
        resolved_operation: Option<ResolvedVar>,
    },
    FunctionDeclaration {
        name: Option<String>,
        resolved_name: Option<ResolvedVar>,
        parameters: Box<ExpressionLocation>,
        body: Box<ExpressionLocation>,
        return_type: Option<StaticType>,
        pure: bool,
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
    Continue,
    RangeInclusive {
        start: Option<Box<ExpressionLocation>>,
        end: Option<Box<ExpressionLocation>>,
    },
    RangeExclusive {
        start: Option<Box<ExpressionLocation>>,
        end: Option<Box<ExpressionLocation>>,
    },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ForIteration {
    Iteration {
        l_value: Lvalue,
        sequence: ExpressionLocation,
    },
    Guard(ExpressionLocation),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ForBody {
    Block(ExpressionLocation),
    List(ExpressionLocation),
    Map {
        key: ExpressionLocation,
        value: Option<ExpressionLocation>,
        default: Option<Box<ExpressionLocation>>,
    },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Lvalue {
    // Example: `let foo = ...`
    Identifier {
        identifier: String,
        resolved: Option<ResolvedVar>,
    },
    // Example: `foo()[1] = ...`
    Index {
        value: Box<ExpressionLocation>,
        index: Box<ExpressionLocation>,
    },
    // Example: `let a, b = ...`
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
    pub fn try_into_identifier(&self) -> Result<&str, EvaluationError> {
        match &self.expression {
            Expression::Identifier { name, resolved: _ } => Ok(name),
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
                .map(|it| it.try_into_identifier().map(ToString::to_string))
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
    pub fn simplify(self) -> Self {
        match self {
            Self {
                expression: Expression::Tuple { mut values },
                ..
            // } if values.len() == 1 => values.remove(0).simplify(),
            } if values.len() == 1 => values.remove(0),
            tuple @ Self { .. } => tuple,
        }
    }
}

impl Lvalue {
    #[must_use]
    pub fn expression_type_name(&self) -> &str {
        match self {
            Self::Identifier { .. } => "variable",
            Self::Index { .. } => "index expression",
            Self::Sequence(_) => "destructure pattern", // ??
        }
    }

    #[must_use]
    pub fn can_build_from_expression(expression: &Expression) -> bool {
        match expression {
            Expression::Identifier { .. } | Expression::Index { .. } => true,
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
            Expression::Identifier {
                name: identifier, ..
            } => Ok(Self::Identifier {
                identifier,
                resolved: None,
            }),
            Expression::Index { value, index } => Ok(Self::Index { value, index }),
            Expression::List { values } | Expression::Tuple { values } => Ok(Self::Sequence(
                values
                    .into_iter()
                    .map(Self::try_from)
                    .collect::<Result<Vec<Self>, Self::Error>>()?,
            )),
            Expression::Grouping(value) => Ok(Self::Sequence(vec![Self::try_from(*value)?])),
            _expr => Err(ParseError::text("invalid l-value".to_string(), value.span)),
        }
    }
}

#[allow(clippy::missing_fields_in_debug, clippy::too_many_lines)]
impl std::fmt::Debug for ExpressionLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write!(f, "{{{:?} at {:?}}}", self.expression, self.span)
        match &self.expression {
            Expression::BoolLiteral(b) => {
                f.debug_struct("BooleanLiteral").field("value", &b).finish()
            }
            Expression::StringLiteral(s) => {
                f.debug_struct("StringLiteral").field("value", &s).finish()
            }
            Expression::Int64Literal(i) => f.debug_struct("IntLiteral").field("value", &i).finish(),
            Expression::Float64Literal(v) => {
                f.debug_struct("FloatLiteral").field("value", &v).finish()
            }
            Expression::BigIntLiteral(big_int) => f
                .debug_struct("BigIntLiteral")
                .field("value", &big_int)
                .finish(),
            Expression::ComplexLiteral(complex) => f
                .debug_struct("CoplexLiteral")
                .field("value", &complex)
                .finish(),
            Expression::Identifier {
                name: ident,
                resolved,
            } => f
                .debug_struct("Ident")
                .field("value", &ident)
                .field("resolved", resolved)
                .finish(),
            Expression::Statement(expression_location) => f
                .debug_struct("Statement")
                .field("expression", &expression_location)
                .finish(),
            Expression::Logical {
                left,
                operator,
                right,
            } => f
                .debug_struct("Logical")
                .field("left", left)
                .field("operator", operator)
                .field("right", right)
                .finish(),
            Expression::Grouping(expression_location) => f
                .debug_struct("Grouping")
                .field("expression", expression_location)
                .finish(),
            Expression::VariableDeclaration { l_value, value } => f
                .debug_struct("VariableDeclaration")
                .field("l_value", l_value)
                .field("value", value)
                .finish(),
            Expression::Assignment { l_value, r_value } => f
                .debug_struct("Assignment")
                .field("l_value", l_value)
                .field("r_value", r_value)
                .finish(),
            Expression::OpAssignment {
                l_value,
                r_value: value,
                operation,
                resolved_operation,
                resolved_assign_operation,
            } => f
                .debug_struct("OpAssignment")
                .field("l_value", l_value)
                .field("value", value)
                .field("operation", operation)
                .field("resolved_operation", resolved_operation)
                .field("resolved_assign_operation", resolved_assign_operation)
                .finish(),
            Expression::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body,
                pure,
                resolved_name,
            } => f
                .debug_struct("FunctionDeclaration")
                .field("name", name)
                .field("resolved_name", resolved_name)
                .field("parameters", parameters)
                .field("return_type", return_type)
                .field("body", body)
                .field("pure", pure)
                .finish(),
            Expression::Block { statements } => f
                .debug_struct("Block")
                .field("statements", statements)
                .finish(),
            Expression::If {
                condition,
                on_true,
                on_false,
            } => f
                .debug_struct("If")
                .field("condition", condition)
                .field("on_true", on_true)
                .field("on_false", on_false)
                .finish(),
            Expression::While {
                expression,
                loop_body,
            } => f
                .debug_struct("While")
                .field("expression", expression)
                .field("loop_body", loop_body)
                .finish(),
            Expression::For { iterations, body } => f
                .debug_struct("For")
                .field("iterations", iterations)
                .field("body", body)
                .finish(),
            Expression::Call {
                function,
                arguments,
            } => f
                .debug_struct("Call")
                .field("function", function)
                .field("arguments", arguments)
                .finish(),
            Expression::Index { value, index } => f
                .debug_struct("Index")
                .field("value", value)
                .field("index", index)
                .finish(),
            Expression::Tuple { values } => {
                f.debug_struct("Tuple").field("values", values).finish()
            }
            Expression::List { values } => f.debug_struct("List").field("values", values).finish(),
            Expression::Map { values, default } => f
                .debug_struct("Map")
                .field("values", values)
                .field("default", default)
                .finish(),
            Expression::Return { value } => f.debug_struct("Return").field("value", value).finish(),
            Expression::Break => f.debug_struct("Break").finish(),
            Expression::Continue => f.debug_struct("Continue").finish(),
            Expression::RangeInclusive { start, end } => f
                .debug_struct("RangeInclusive")
                .field("start", start)
                .field("end", end)
                .field("start", start)
                .field("end", end)
                .finish(),
            Expression::RangeExclusive { start, end } => f
                .debug_struct("RangeExclusive")
                .field("start", start)
                .field("end", end)
                .field("start", start)
                .field("end", end)
                .finish(),
        }
    }
}
