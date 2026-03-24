use crate::operator::LogicalOperator;
use crate::parser::Error as ParseError;
use ndc_core::{StaticType, TypeSignature};
use ndc_lexer::Span;
use num::BigInt;
use num::complex::Complex64;
use std::sync::atomic::{AtomicU32, Ordering};

/// Unique identity for an AST node. Used as a key in side tables (e.g. the
/// analyser's expression type map) so that tooling data doesn't bloat the AST.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

static NEXT_NODE_ID: AtomicU32 = AtomicU32::new(0);

impl NodeId {
    pub fn next() -> Self {
        Self(NEXT_NODE_ID.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Binding {
    None,
    Resolved(ResolvedVar),
    Dynamic(Vec<ResolvedVar>), // figure it out at runtime
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ResolvedVar {
    Local { slot: usize },
    Upvalue { slot: usize },
    Global { slot: usize },
}

impl ResolvedVar {
    pub fn slot(self) -> usize {
        match self {
            Self::Local { slot } | Self::Upvalue { slot, .. } | Self::Global { slot } => slot,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CaptureSource {
    Local(usize),
    Upvalue(usize),
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct ExpressionLocation {
    pub id: NodeId,
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
        annotated_type: Option<StaticType>,
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
        resolved_assign_operation: Binding,
        resolved_operation: Binding,
    },
    FunctionDeclaration {
        name: Option<String>,
        resolved_name: Option<ResolvedVar>,
        type_signature: TypeSignature,
        parameters_span: Span,
        body: Box<ExpressionLocation>,
        return_type: Option<StaticType>,
        captures: Vec<CaptureSource>,
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
#[allow(clippy::large_enum_variant)]
pub enum ForBody {
    Block(ExpressionLocation),
    List {
        expr: ExpressionLocation,
        accumulator_slot: Option<usize>,
    },
    Map {
        key: ExpressionLocation,
        value: Option<ExpressionLocation>,
        default: Option<Box<ExpressionLocation>>,
        accumulator_slot: Option<usize>,
    },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Lvalue {
    // Example: `let foo = ...`
    Identifier {
        identifier: String,
        resolved: Option<ResolvedVar>,
        span: Span,
        inferred_type: Option<StaticType>,
    },
    // Example: `foo()[1] = ...`
    Index {
        value: Box<ExpressionLocation>,
        index: Box<ExpressionLocation>,
        resolved_set: Option<Binding>,
        resolved_get: Option<Binding>,
    },
    // Example: `let a, b = ...`
    Sequence(Vec<Self>),
}

impl Eq for Expression {}

impl Expression {
    #[must_use]
    pub fn to_location(self, span: Span) -> ExpressionLocation {
        ExpressionLocation {
            id: NodeId::next(),
            expression: self,
            span,
        }
    }
}

impl ExpressionLocation {
    #[must_use]
    pub fn to_statement(self) -> Self {
        Self {
            id: NodeId::next(),
            span: self.span,
            expression: Expression::Statement(Box::new(self)),
        }
    }

    pub fn as_identifier(&self) -> &str {
        match &self.expression {
            Expression::Identifier { name, resolved: _ } => name,
            _ => panic!("the parser should have guaranteed us the right type of expression"),
        }
    }

    pub fn as_parameters(&self) -> Vec<&str> {
        match &self.expression {
            Expression::Tuple {
                values: tuple_values,
            } => tuple_values.iter().map(|it| it.as_identifier()).collect(),
            _ => panic!("the parser should have guaranteed us the right type of expression"),
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
            Expression::Identifier { .. } => true,
            Expression::Call {
                function,
                arguments,
            } if is_index_call(function, arguments) => true,
            Expression::List { values } | Expression::Tuple { values } => values
                .iter()
                .all(|el| Self::can_build_from_expression(&el.expression)),
            Expression::Grouping(inner) => Self::can_build_from_expression(&inner.expression),
            _ => false,
        }
    }

    pub fn new_identifier(identifier: String, span: Span) -> Self {
        Self::Identifier {
            identifier,
            resolved: None,
            span,
            inferred_type: None,
        }
    }
}

impl TryFrom<ExpressionLocation> for Lvalue {
    type Error = ParseError;

    fn try_from(value: ExpressionLocation) -> Result<Self, Self::Error> {
        match value.expression {
            Expression::Identifier { name, .. } => Ok(Self::new_identifier(name, value.span)),
            Expression::Call {
                function,
                mut arguments,
            } if is_index_call(&function, &arguments) => {
                let index = arguments.remove(1);
                let container = arguments.remove(0);
                Ok(Self::Index {
                    value: Box::new(container),
                    index: Box::new(index),
                    resolved_set: None,
                    resolved_get: None,
                })
            }
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

fn is_index_call(function: &ExpressionLocation, arguments: &[ExpressionLocation]) -> bool {
    matches!(
        &function.expression,
        Expression::Identifier { name, .. } if name == "[]"
    ) && arguments.len() == 2
}
