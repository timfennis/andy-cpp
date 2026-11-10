use crate::operator::LogicalOperator;
use crate::parser::Error as ParseError;
use crate::type_expr::TypeExpr;
use ndc_core::NodeId;
use ndc_lexer::{NumericLiteral, Span};

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
    /// `None`, the empty option.
    NoneLiteral,
    StringLiteral(String),
    NumericLiteral(NumericLiteral),
    Identifier {
        name: String,
        /// The identifier token's range. When parsing `(foo)`, the parser reuses
        /// the identifier expression and widens `ExpressionLocation.span` to
        /// include the parentheses. This field still covers only `foo`, so a
        /// binding pattern can use it as the go-to-definition destination.
        identifier_span: Span,
    },
    Statement(Box<ExpressionLocation>),
    Logical {
        left: Box<ExpressionLocation>,
        operator: LogicalOperator,
        right: Box<ExpressionLocation>,
    },
    Grouping(Box<ExpressionLocation>),
    /// `value as Type` — asserts the value has the target type; never converts.
    Cast {
        value: Box<ExpressionLocation>,
        annotation: TypeExpr,
    },
    VariableDeclaration {
        l_value: BindingPatternLocation,
        annotated_type: Option<TypeExpr>,
        value: Box<ExpressionLocation>,
    },
    Assignment {
        l_value: AssignmentTargetLocation,
        r_value: Box<ExpressionLocation>,
    },
    OpAssignment {
        l_value: AssignmentTargetLocation,
        r_value: Box<ExpressionLocation>,
        operation: String,
    },
    FunctionDeclaration {
        name: Option<String>,
        parameters: Vec<FunctionParameter>,
        parameters_span: Span,
        body: Box<ExpressionLocation>,
        return_annotation: Option<TypeExpr>,
        pure: bool,
    },
    StructDeclaration {
        name: String,
        fields: Vec<StructField>,
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
        function: Box<ExpressionLocation>,
        arguments: Vec<ExpressionLocation>,
    },
    OperatorCall {
        function: Box<ExpressionLocation>,
        arguments: Vec<ExpressionLocation>,
    },
    // Example: reading from `foo.bar`
    MemberAccess {
        receiver: Box<ExpressionLocation>,
        member: String,
        member_span: Span,
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
        l_value: BindingPatternLocation,
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
    },
    Map {
        key: ExpressionLocation,
        value: Option<ExpressionLocation>,
        default: Option<Box<ExpressionLocation>>,
    },
}
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructField {
    pub identifier: String,
    pub annotation: TypeExpr,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FunctionParameter {
    pub lvalue: BindingPatternLocation,
    pub annotation: Option<TypeExpr>,
    pub span: Span,
}

impl FunctionParameter {
    pub fn name(&self) -> &str {
        match &self.lvalue.pattern {
            BindingPattern::Identifier { identifier, .. } => identifier,
            BindingPattern::Sequence(_) => {
                unreachable!("a function parameter can never be a sequence")
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AssignmentTarget {
    // Example: `foo = ...`
    Identifier {
        identifier: String,
        span: Span,
    },
    // Example: `foo()[1] = ...`
    Index {
        value: Box<ExpressionLocation>,
        index: Box<ExpressionLocation>,
    },
    // Example: `a, b = ...`
    Sequence(Vec<AssignmentTargetLocation>),
    // Example: `a.b = ...`
    Member {
        receiver: Box<ExpressionLocation>,
        member: String,
        member_span: Span,
    },
}

/// A target that writes through an existing value instead of introducing a new
/// binding, and so cannot appear in a `let`.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum NonBindingTarget {
    Index,
    Member,
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
            Expression::Identifier { name, .. } => name,
            _ => panic!("the parser should have guaranteed us the right type of expression"),
        }
    }

    pub fn to_identifier(self) -> String {
        match self.expression {
            Expression::Identifier { name, .. } => name,
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

impl AssignmentTarget {
    #[must_use]
    pub fn can_build_from_expression(expression: &Expression) -> bool {
        match expression {
            Expression::Identifier { .. } | Expression::MemberAccess { .. } => true,
            Expression::Call {
                function,
                arguments,
            } if is_index_call(function, arguments) => true,
            Expression::List { values } | Expression::Tuple { values } => values
                .iter()
                .all(|el| Self::can_build_destructure_from_expression(&el.expression)),
            // Parentheses around a target are transparent: `(s.x) = 5` writes
            // the same location as `s.x = 5`.
            Expression::Grouping(inner) => Self::can_build_from_expression(&inner.expression),
            _ => false,
        }
    }

    fn can_build_destructure_from_expression(expression: &Expression) -> bool {
        match expression {
            Expression::MemberAccess { .. } => false,
            Expression::List { values } | Expression::Tuple { values } => values
                .iter()
                .all(|el| Self::can_build_destructure_from_expression(&el.expression)),
            Expression::Grouping(inner) => {
                Self::can_build_destructure_from_expression(&inner.expression)
            }
            expression => Self::can_build_from_expression(expression),
        }
    }
}

/// A declaration pattern, with identity independent of its source location.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BindingPatternLocation {
    pub id: NodeId,
    pub pattern: BindingPattern,
    pub span: Span,
}

/// Syntax that introduces names. Index and member writes are not declarations.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BindingPattern {
    Identifier {
        identifier: String,
        /// The identifier token, which may be narrower than the pattern's span.
        span: Span,
    },
    Sequence(Vec<BindingPatternLocation>),
}

/// A destination for a write, with the complete target's source range.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AssignmentTargetLocation {
    pub id: NodeId,
    pub target: AssignmentTarget,
    pub span: Span,
}

impl TryFrom<AssignmentTargetLocation> for BindingPatternLocation {
    type Error = NonBindingTarget;

    fn try_from(value: AssignmentTargetLocation) -> Result<Self, Self::Error> {
        let pattern = match value.target {
            AssignmentTarget::Identifier { identifier, span } => {
                BindingPattern::Identifier { identifier, span }
            }
            AssignmentTarget::Sequence(items) => BindingPattern::Sequence(
                items
                    .into_iter()
                    .map(Self::try_from)
                    .collect::<Result<_, _>>()?,
            ),
            AssignmentTarget::Index { .. } => return Err(NonBindingTarget::Index),
            AssignmentTarget::Member { .. } => return Err(NonBindingTarget::Member),
        };
        Ok(Self {
            id: value.id,
            pattern,
            span: value.span,
        })
    }
}

impl TryFrom<ExpressionLocation> for AssignmentTargetLocation {
    type Error = ParseError;

    /// Convert a parsed expression into an assignment target, reusing its NodeId
    /// and source span. For `(object.field)`, remove the Grouping wrapper and use
    /// its ID and span for the resulting member target. Discard the inner member
    /// node's ID; keep the receiver expression and its ID.
    fn try_from(value: ExpressionLocation) -> Result<Self, Self::Error> {
        let target = match value.expression {
            Expression::Identifier {
                name,
                identifier_span,
                ..
            } => AssignmentTarget::Identifier {
                identifier: name,
                span: identifier_span,
            },
            Expression::Call {
                function,
                mut arguments,
            } if is_index_call(&function, &arguments) => {
                let index = arguments.remove(1);
                let container = arguments.remove(0);
                AssignmentTarget::Index {
                    value: Box::new(container),
                    index: Box::new(index),
                }
            }
            Expression::MemberAccess {
                receiver,
                member,
                member_span,
                ..
            } => AssignmentTarget::Member {
                receiver,
                member,
                member_span,
            },
            Expression::List { values } | Expression::Tuple { values } => {
                AssignmentTarget::Sequence(
                    values
                        .into_iter()
                        .map(Self::try_from)
                        .collect::<Result<_, _>>()?,
                )
            }
            Expression::Grouping(inner) => Self::try_from(*inner)?.target,
            _ => return Err(ParseError::text("invalid l-value".to_string(), value.span)),
        };
        Ok(Self {
            id: value.id,
            target,
            span: value.span,
        })
    }
}

fn is_index_call(function: &ExpressionLocation, arguments: &[ExpressionLocation]) -> bool {
    matches!(
        &function.expression,
        Expression::Identifier { name, .. } if name == "[]"
    ) && arguments.len() == 2
}
