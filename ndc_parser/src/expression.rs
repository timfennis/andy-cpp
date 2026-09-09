use crate::operator::LogicalOperator;
use crate::parser::Error as ParseError;
use crate::type_expr::TypeExpr;
use ndc_core::r#struct::StructId;
use ndc_core::{StaticType, TypeSignature};
use ndc_lexer::{NumericLiteral, Span};
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
    Resolved(Candidate),
    Dynamic(Vec<Candidate>), // figure it out at runtime
}

/// The operation selected by the analyser for an augmented assignment.
///
/// Every selected operation returns the updated left value internally. The
/// compiler writes that value back through the assignment target, while the
/// augmented-assignment expression itself evaluates to unit.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AugmentedAssignmentPlan {
    Unresolved,
    Resolved(Binding),
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

/// A function overload candidate the analyser picked. The two variants encode
/// what kind of call this candidate is — a direct scalar call, or an element-wise
/// tuple broadcast over the scalar overload that `var()` returns.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Candidate {
    Scalar(ResolvedVar),
    /// Element-wise tuple broadcast. The inner `ResolvedVar` points to the
    /// scalar overload that fires for each element pair.
    Vec(ResolvedVar),
}

impl Candidate {
    pub fn var(self) -> ResolvedVar {
        match self {
            Self::Scalar(v) | Self::Vec(v) => v,
        }
    }

    pub fn is_vec(self) -> bool {
        matches!(self, Self::Vec(_))
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
    NumericLiteral(NumericLiteral),
    Identifier {
        name: String,
        resolved: Binding,
        /// Original token range, retained when grouping widens the expression span.
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
        resolved_type: Option<StaticType>,
        /// Cleared by the analyser when the value's static type proves the cast.
        requires_check: bool,
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
        plan: AugmentedAssignmentPlan,
    },
    FunctionDeclaration {
        name: Option<String>,
        resolved_name: Option<ResolvedVar>,
        parameters: Vec<FunctionParameter>,
        parameters_span: Span,
        body: Box<ExpressionLocation>,
        return_annotation: Option<TypeExpr>,
        resolved_return_type: Option<StaticType>,
        captures: Vec<CaptureSource>,
        pure: bool,
    },
    StructDeclaration {
        name: String,
        fields: Vec<StructField>,
        resolved: Option<StructId>,
        resolved_name: Option<ResolvedVar>,
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
        resolved_getter: Binding,
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
    pub resolved_getter: Option<ResolvedVar>,
    pub resolved_setter: Option<ResolvedVar>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FunctionParameter {
    pub lvalue: BindingPatternLocation,
    pub annotation: Option<TypeExpr>,
    pub resolved_type: Option<StaticType>,
    pub span: Span,
}

impl FunctionParameter {
    pub fn from_params(params: &[Self]) -> TypeSignature {
        TypeSignature::from_annotated_bindings(
            params
                .iter()
                .map(|p| {
                    let BindingPattern::Identifier { identifier, .. } = &p.lvalue.pattern else {
                        unreachable!(
                            "parameter list may only contain identifiers {:?} found.",
                            p.lvalue
                        );
                    };
                    (identifier.clone(), p.resolved_type.clone())
                })
                .collect(),
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AssignmentTarget {
    // Example: `foo = ...`
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
    // Example: `a, b = ...`
    Sequence(Vec<AssignmentTargetLocation>),
    // Example: `a.b = ...`
    Member {
        receiver: Box<ExpressionLocation>,
        member: String,
        member_span: Span,
        resolved_getter: Option<Binding>,
        resolved_setter: Option<Binding>,
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
        resolved: Option<ResolvedVar>,
        /// The identifier token, which may be narrower than the pattern's span.
        span: Span,
        inferred_type: Option<StaticType>,
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
            AssignmentTarget::Identifier {
                identifier,
                resolved,
                span,
                inferred_type,
            } => BindingPattern::Identifier {
                identifier,
                resolved,
                span,
                inferred_type,
            },
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

    /// Consuming conversion keeps the expression's identity. Grouping is folded
    /// into the target, retaining its outer identity and full source range.
    fn try_from(value: ExpressionLocation) -> Result<Self, Self::Error> {
        let target = match value.expression {
            Expression::Identifier {
                name,
                identifier_span,
                ..
            } => AssignmentTarget::Identifier {
                identifier: name,
                resolved: None,
                span: identifier_span,
                inferred_type: None,
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
                    resolved_set: None,
                    resolved_get: None,
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
                resolved_getter: None,
                resolved_setter: None,
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
