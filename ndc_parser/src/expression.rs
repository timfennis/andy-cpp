use crate::operator::LogicalOperator;
use crate::parser::Error as ParseError;
use crate::static_type::StaticType;
use ndc_lexer::Span;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExpressionRef(u32);

impl ExpressionRef {
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
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
    Statement(ExpressionRef),
    Logical {
        left: ExpressionRef,
        operator: LogicalOperator,
        right: ExpressionRef,
    },
    Grouping(ExpressionRef),
    VariableDeclaration {
        l_value: Lvalue,
        value: ExpressionRef,
    },
    Assignment {
        l_value: Lvalue,
        r_value: ExpressionRef,
    },
    OpAssignment {
        l_value: Lvalue,
        r_value: ExpressionRef,
        operation: String,
        resolved_assign_operation: Binding,
        resolved_operation: Binding,
    },
    FunctionDeclaration {
        name: Option<String>,
        resolved_name: Option<ResolvedVar>,
        // TODO: Instead of an ExpressionLocation with a Tuple the parser should just give us something we can actually work with
        parameters: ExpressionRef,
        body: ExpressionRef,
        return_type: Option<StaticType>,
        pure: bool,
    },
    Block {
        statements: Vec<ExpressionRef>,
    },
    If {
        condition: ExpressionRef,
        on_true: ExpressionRef,
        on_false: Option<ExpressionRef>,
    },
    While {
        expression: ExpressionRef,
        loop_body: ExpressionRef,
    },
    For {
        iterations: Vec<ForIteration>,
        body: Box<ForBody>,
    },
    Call {
        /// The function to call, could be an identifier, or any expression that produces a function as its value
        function: ExpressionRef,
        arguments: Vec<ExpressionRef>,
    },
    Index {
        value: ExpressionRef,
        index: ExpressionRef,
    },
    Tuple {
        values: Vec<ExpressionRef>,
    },
    List {
        values: Vec<ExpressionRef>,
    },
    Map {
        values: Vec<(ExpressionRef, Option<ExpressionRef>)>,
        default: Option<ExpressionRef>,
    },
    Return {
        value: ExpressionRef,
    },
    Break,
    Continue,
    RangeInclusive {
        start: Option<ExpressionRef>,
        end: Option<ExpressionRef>,
    },
    RangeExclusive {
        start: Option<ExpressionRef>,
        end: Option<ExpressionRef>,
    },
}

impl Eq for Expression {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ForIteration {
    Iteration {
        l_value: Lvalue,
        sequence: ExpressionRef,
    },
    Guard(ExpressionRef),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ForBody {
    Block(ExpressionRef),
    List(ExpressionRef),
    Map {
        key: ExpressionRef,
        value: Option<ExpressionRef>,
        default: Option<Box<ExpressionRef>>,
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
        value: ExpressionRef,
        index: ExpressionRef,
    },
    // Example: `let a, b = ...`
    Sequence(Vec<Self>),
}

impl Expression {
    #[must_use]
    pub fn to_location(self, span: Span) -> ExpressionLocation {
        ExpressionLocation {
            expression: self,
            span,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ExpressionPool {
    region: Vec<ExpressionLocation>,
    root_refs: Vec<ExpressionRef>,
}

impl ExpressionPool {}

impl IntoIterator for ExpressionPool {
    type Item = ExpressionLocation;
    type IntoIter = std::vec::IntoIter<ExpressionLocation>;

    fn into_iter(self) -> Self::IntoIter {
        self.region.into_iter()
    }
}

impl ExpressionPool {
    pub fn len(&self) -> usize {
        self.region.len()
    }

    pub fn is_empty(&self) -> bool {
        self.region.is_empty()
    }

    pub fn add_root_ref(&mut self, root_ref: ExpressionRef) {
        self.root_refs.push(root_ref)
    }

    pub fn get(&self, er: ExpressionRef) -> &ExpressionLocation {
        &self.region[er.0 as usize]
    }
    pub fn add(&mut self, expr: ExpressionLocation) -> ExpressionRef {
        self.region.push(expr);

        ExpressionRef((self.region.len() - 1) as u32)
    }

    pub fn set_span(&mut self, er: ExpressionRef, span: Span) {
        self.region[er.0 as usize].span = span;
    }

    pub fn get_mut(&mut self, er: ExpressionRef) -> &mut ExpressionLocation {
        &mut self.region[er.0 as usize]
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &ExpressionLocation> {
        self.region.iter()
    }

    pub fn root_expressions(&self) -> impl DoubleEndedIterator<Item = &ExpressionRef> {
        self.root_refs.iter()
    }
    pub fn merged_span(&self, left_id: ExpressionRef, right_id: ExpressionRef) -> Span {
        self.region[left_id.0 as usize]
            .span
            .merge(self.region[right_id.0 as usize].span)
    }

    pub fn simplify(&mut self, target: ExpressionRef) {
        // NOTE: for now we put a copy of the expression in the target slot but in the future we might want to move it
        match self.get(target) {
            ExpressionLocation {
                expression: Expression::Tuple { values },
                ..
            } if values.len() == 1 => {
                self.region[target.0 as usize] = self.region[values[0].0 as usize].clone()
            }
            _ => {}
        }
    }
}

impl ExpressionLocation {
    pub fn as_identifier(&self) -> &str {
        match &self.expression {
            Expression::Identifier { name, resolved: _ } => name,
            _ => panic!("the parser should have guaranteed us the right type of expression"),
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
    pub fn can_build_from_expression(expression: &Expression, pool: &ExpressionPool) -> bool {
        match expression {
            Expression::Identifier { .. } | Expression::Index { .. } => true,
            Expression::List { values } | Expression::Tuple { values } => values
                .iter()
                .all(|el| Self::can_build_from_expression(&pool.get(*el).expression, pool)),
            Expression::Grouping(inner) => {
                Self::can_build_from_expression(&pool.get(*inner).expression, pool)
            }
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

impl Lvalue {
    pub fn from_expression_location(
        value: ExpressionLocation,
        pool: &ExpressionPool,
    ) -> Result<Self, ParseError> {
        match value.expression {
            Expression::Identifier { name, .. } => Ok(Self::new_identifier(name, value.span)),
            Expression::Index { value, index } => Ok(Self::Index { value, index }),
            Expression::List { values } | Expression::Tuple { values } => Ok(Self::Sequence(
                values
                    .into_iter()
                    .map(|er| Self::from_expression_location(pool.get(er).clone(), pool))
                    .collect::<Result<Vec<Self>, ParseError>>()?,
            )),
            Expression::Grouping(inner) => {
                Ok(Self::Sequence(vec![Self::from_expression_location(
                    pool.get(inner).clone(),
                    pool,
                )?]))
            }
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
