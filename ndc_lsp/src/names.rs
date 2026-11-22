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
    /// binding pattern or a name in a type annotation.
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
}

/// The names of one analysed text.
#[derive(Debug, Default)]
pub struct Names {
    /// In the order they're written. Names never overlap.
    names: Vec<Name>,
    /// The name each declaration declares.
    declarations: HashMap<Definition, Span>,
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
        let inside = |name: &&Name| name.span.offset() <= offset && offset < name.span.end();
        let after = |name: &&Name| name.span.end() == offset;
        self.names
            .iter()
            .find(inside)
            .or_else(|| self.names.iter().find(after))
    }

    /// Where the name `definition` declares is.
    pub fn declaration(&self, definition: Definition) -> Option<Span> {
        self.declarations.get(&definition).copied()
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
        self.names.names.push(Name {
            refers,
            span,
            write,
        });
        if let Refers::Declaration(definition) = refers {
            self.names.declarations.insert(definition, span);
        }
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
                self.name(Refers::Node(expr.id), *member_span, false);
            }
            Expression::VariableDeclaration {
                l_value,
                annotated_type,
                ..
            } => {
                self.pattern(l_value);
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
                    self.name(Refers::Declaration(Definition::Node(expr.id)), span, true);
                }
                for parameter in type_parameters {
                    let definition = Definition::Node(parameter.id);
                    self.name(Refers::Declaration(definition), parameter.span, true);
                }
                for parameter in parameters {
                    self.pattern(&parameter.lvalue);
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
                    self.name(Refers::Declaration(Definition::Node(expr.id)), span, true);
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
                    self.name(Refers::Declaration(definition), span, true);
                    self.type_names(&field.annotation);
                }
            }
            Expression::For { iterations, .. } => {
                for iteration in iterations {
                    if let ForIteration::Iteration { l_value, .. } = iteration {
                        self.pattern(l_value);
                    }
                }
            }
            Expression::Assignment { l_value, .. } | Expression::OpAssignment { l_value, .. } => {
                self.target(l_value);
            }
            _ => {}
        }
        for child in expr.children() {
            self.expression(child);
        }
    }

    fn pattern(&mut self, pattern: &BindingPatternLocation) {
        match &pattern.pattern {
            BindingPattern::Identifier { span, .. } => {
                self.name(Refers::Node(pattern.id), *span, true);
                self.names
                    .declarations
                    .insert(Definition::Node(pattern.id), *span);
            }
            BindingPattern::Sequence(patterns) => {
                for pattern in patterns {
                    self.pattern(pattern);
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
                self.name(Refers::Node(target.id), *member_span, true);
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
                self.name(Refers::Node(*id), name_span, false);
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
