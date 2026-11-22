//! Every name written in an analysed document: where it is, what the
//! analysis keys it by, and whether it writes. Go-to-definition, references,
//! document highlight and rename all start from here.

use ndc_core::NodeId;
use ndc_core::collections::HashMap;
use ndc_lexer::{Lexer, Span, Token, TokenLocation};
use ndc_parser::{
    AssignmentTarget, AssignmentTargetLocation, BindingPattern, BindingPatternLocation, Expression,
    ExpressionLocation, ForIteration, TypeExpr,
};

use crate::resolved::Definition;

/// What a name is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Refers {
    /// A name the resolver and the analyser resolve, by its node: an
    /// identifier, a member access or member target, an assignment target, a
    /// binding pattern, a name in a type annotation, or the function an
    /// augmented assignment calls (`f` in `x f= y`, by the assignment).
    Node(NodeId),
    /// The name a declaration gives a function, a struct, a field or a type
    /// parameter, which only declares.
    Declaration(Definition),
}

/// One name written in the document.
#[derive(Debug, Clone, Copy)]
pub struct Name {
    pub refers: Refers,
    pub span: Span,
    /// Whether it declares or assigns rather than reads.
    pub write: bool,
    pub place: Place,
}

/// Where a name is written.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Place {
    /// In code: a value, a function or a declaration.
    Code,
    /// In a type annotation, as `Int` in `let x: Int`.
    Type,
    /// After a dot, reading or writing a field: `x` in `p.x`.
    Member,
}

/// What a declaration declares.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Declares {
    Function,
    /// A struct: its type and its constructor.
    Struct,
    Field,
    TypeParameter,
    Parameter,
    Variable,
}

/// The names of one analysed text.
#[derive(Debug, Default)]
pub struct Names {
    /// In the order they're written. Names never overlap.
    names: Vec<Name>,
    /// The name each declaration declares.
    declarations: HashMap<Definition, Span>,
    /// What each declaration declares.
    kinds: HashMap<Definition, Declares>,
}

impl Names {
    /// The names in `ast`, parsed from `text`.
    pub fn collect(ast: &[ExpressionLocation], text: &str) -> Self {
        let source_id = ast.first().map(|e| e.span.source_id());
        let tokens =
            source_id.map_or_default(|id| Lexer::new(text, id).filter_map(Result::ok).collect());
        let mut collector = Collector {
            tokens,
            names: Self::default(),
        };
        for expr in ast {
            collector.expression(expr);
        }
        let mut names = collector.names;
        names.names.sort_by_key(|name| name.span.offset());
        names
    }

    /// The name the cursor at `offset` is on: inside it, or just after it.
    pub fn at(&self, offset: usize) -> Option<&Name> {
        // The names are in order and don't overlap: the one starting at or
        // before `offset` contains it, or ends where it is.
        let next = self
            .names
            .partition_point(|name| name.span.offset() <= offset);
        let name = self.names.get(next.checked_sub(1)?)?;
        (offset <= name.span.end()).then_some(name)
    }

    /// Where the name `definition` declares is.
    pub fn declaration(&self, definition: Definition) -> Option<Span> {
        self.declarations.get(&definition).copied()
    }

    /// What `definition` declares.
    pub fn kind(&self, definition: Definition) -> Option<Declares> {
        self.kinds.get(&definition).copied()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Name> {
        self.names.iter()
    }
}

struct Collector {
    /// The text's tokens, for the names the AST keeps no span of.
    tokens: Vec<TokenLocation>,
    names: Names,
}

impl Collector {
    fn name(&mut self, refers: Refers, span: Span, write: bool) {
        self.placed(refers, span, write, Place::Code);
    }

    fn placed(&mut self, refers: Refers, span: Span, write: bool, place: Place) {
        self.names.names.push(Name {
            refers,
            span,
            write,
            place,
        });
        if let Refers::Declaration(definition) = refers {
            self.names.declarations.insert(definition, span);
        }
    }

    /// The name a declaration gives what it declares.
    fn declares(&mut self, definition: Definition, span: Span, kind: Declares) {
        self.name(Refers::Declaration(definition), span, true);
        self.names.kinds.insert(definition, kind);
    }

    fn expression(&mut self, expr: &ExpressionLocation) {
        match &expr.expression {
            Expression::Identifier {
                identifier_span,
                type_arguments,
                ..
            } => {
                self.name(Refers::Node(expr.id), *identifier_span, false);
                for argument in type_arguments {
                    self.type_names(argument);
                }
            }
            Expression::MemberAccess { member_span, .. } => {
                self.placed(Refers::Node(expr.id), *member_span, false, Place::Member);
            }
            Expression::VariableDeclaration {
                l_value,
                annotated_type,
                ..
            } => {
                self.pattern(l_value, Declares::Variable);
                if let Some(annotation) = annotated_type {
                    self.type_names(annotation);
                }
            }
            Expression::FunctionDeclaration {
                name,
                type_parameters,
                parameters,
                return_annotation,
                ..
            } => {
                if name.is_some()
                    && let Some(span) = self.name_after(&Token::Fn, expr.span)
                {
                    self.declares(Definition::Node(expr.id), span, Declares::Function);
                }
                for parameter in type_parameters {
                    let definition = Definition::Node(parameter.id);
                    self.declares(definition, parameter.span, Declares::TypeParameter);
                }
                for parameter in parameters {
                    self.pattern(&parameter.lvalue, Declares::Parameter);
                    if let Some(annotation) = &parameter.annotation {
                        self.type_names(annotation);
                    }
                }
                if let Some(annotation) = return_annotation {
                    self.type_names(annotation);
                }
            }
            Expression::StructDeclaration { fields, .. } => {
                if let Some(span) = self.name_after(&Token::Struct, expr.span) {
                    self.declares(Definition::Node(expr.id), span, Declares::Struct);
                }
                for (index, field) in fields.iter().enumerate() {
                    let definition = Definition::Field {
                        declaration: expr.id,
                        index,
                    };
                    let span = Span::new(
                        field.span.source_id(),
                        field.span.offset(),
                        field.identifier.len(),
                    );
                    self.declares(definition, span, Declares::Field);
                    self.type_names(&field.annotation);
                }
            }
            Expression::For { iterations, .. } => {
                for iteration in iterations {
                    if let ForIteration::Iteration { l_value, .. } = iteration {
                        self.pattern(l_value, Declares::Variable);
                    }
                }
            }
            Expression::Assignment { l_value, .. } => self.target(l_value),
            Expression::OpAssignment { l_value, .. } => {
                self.target(l_value);
                if let Some(span) = self.augmented_name(expr.span) {
                    self.name(Refers::Node(expr.id), span, false);
                }
            }
            _ => {}
        }
        for child in expr.children() {
            self.expression(child);
        }
    }

    /// The names a binding pattern declares, each a `kind`: a parameter or
    /// a variable. A pattern is a name the analysis resolves (to itself), and
    /// a declaration too.
    fn pattern(&mut self, pattern: &BindingPatternLocation, kind: Declares) {
        match &pattern.pattern {
            BindingPattern::Identifier { span, .. } => {
                self.name(Refers::Node(pattern.id), *span, true);
                let definition = Definition::Node(pattern.id);
                self.names.declarations.insert(definition, *span);
                self.names.kinds.insert(definition, kind);
            }
            BindingPattern::Sequence(patterns) => {
                for pattern in patterns {
                    self.pattern(pattern, kind);
                }
            }
        }
    }

    fn target(&mut self, target: &AssignmentTargetLocation) {
        match &target.target {
            AssignmentTarget::Identifier { span, .. } => {
                self.name(Refers::Node(target.id), *span, true);
            }
            AssignmentTarget::Member { member_span, .. } => {
                self.placed(Refers::Node(target.id), *member_span, true, Place::Member);
            }
            AssignmentTarget::Sequence(targets) => {
                for target in targets {
                    self.target(target);
                }
            }
            // Its expressions are among the assignment's children.
            AssignmentTarget::Index { .. } => {}
        }
    }

    fn type_names(&mut self, annotation: &TypeExpr) {
        match annotation {
            TypeExpr::Name {
                id,
                name,
                args,
                span,
            } => {
                let name_span = Span::new(span.source_id(), span.offset(), name.len());
                self.placed(Refers::Node(*id), name_span, false, Place::Type);
                for arg in args {
                    self.type_names(arg);
                }
            }
            TypeExpr::List { element, .. } => self.type_names(element),
            TypeExpr::Map { key, value, .. } => {
                self.type_names(key);
                if let Some(value) = value {
                    self.type_names(value);
                }
            }
            TypeExpr::Tuple { elements, .. } => {
                for element in elements {
                    self.type_names(element);
                }
            }
            TypeExpr::Function {
                parameters,
                return_type,
                ..
            } => {
                for parameter in parameters {
                    self.type_names(parameter);
                }
                if let Some(return_type) = return_type {
                    self.type_names(return_type);
                }
            }
        }
    }

    /// The function an augmented assignment in `within` calls, if it's named:
    /// `f` in `x f= y`, which the AST keeps only as text.
    fn augmented_name(&self, within: Span) -> Option<Span> {
        let start = self
            .tokens
            .partition_point(|token| token.span.offset() < within.offset());
        self.tokens[start..]
            .iter()
            .take_while(|token| token.span.offset() < within.end())
            .find_map(|token| match &token.token {
                // The token covers the `=` too.
                Token::OpAssign(inner) => match &inner.token {
                    Token::Identifier(name) => Some(Span::new(
                        inner.span.source_id(),
                        inner.span.offset(),
                        name.len(),
                    )),
                    _ => None,
                },
                _ => None,
            })
    }

    /// The identifier right after the first `keyword` in `within`: the name
    /// in `fn name(…)` or `struct Name { … }`, which the AST keeps no span of.
    fn name_after(&self, keyword: &Token, within: Span) -> Option<Span> {
        let start = self
            .tokens
            .partition_point(|token| token.span.offset() < within.offset());
        let mut tokens = self.tokens[start..]
            .iter()
            .take_while(|token| token.span.offset() < within.end());
        tokens.find(|token| token.token == *keyword)?;
        match tokens.next()? {
            TokenLocation {
                token: Token::Identifier(_),
                span,
            } => Some(*span),
            _ => None,
        }
    }
}
