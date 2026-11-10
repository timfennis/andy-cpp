use crate::error::AnalysisError;
use crate::overloads::CallKind;
use crate::types::{TypeBinding, TypeTable};
use ahash::AHashMap;
use ndc_analysis::{Analysis, Binding, Candidate};
use ndc_core::StaticType;
use ndc_core::r#struct::{StructInfo, StructRegistry};
use ndc_layout::{Layout, Picks};
use ndc_lexer::NumericLiteral;
use ndc_parser::{Expression, ExpressionLocation, ForBody, ForIteration, TypeExpr};
use ndc_resolver::{NameResolver, SymbolId};
use std::fmt::Debug;
use std::rc::Rc;

mod assignments;
mod calls;
mod declarations;
#[cfg(test)]
mod tests;

/// What analysing one batch produced: the analysis, and the errors found on
/// the way.
#[derive(Debug, Default)]
pub struct AnalysisResult {
    /// What the analyser worked out, keyed by AST node.
    pub analysis: Analysis,
    /// The errors analysis found. It carries on past each with a fallback
    /// type, usually `Any`, so one pass reports every error it can.
    pub errors: Vec<AnalysisError>,
}

#[derive(Debug)]
pub struct Analyser {
    /// Resolves the names in each statement before it is analysed.
    resolver: NameResolver,
    /// Every struct declared so far, which hands out their ids.
    struct_registry: StructRegistry,
    /// The type of every value declared so far.
    types: TypeTable,
    /// What each struct's type symbol stands for, so `p: Point` lowers to
    /// `Point`'s struct type. Every other type name is built in.
    struct_types: AHashMap<SymbolId, Rc<StructInfo>>,
    /// Stack of explicit `return` types for each enclosing function scope.
    /// Pushed on function entry, popped on exit. The value accumulates the
    /// lub of all `return <expr>` types seen so far.
    return_type_stack: Vec<Option<StaticType>>,
    /// How many loops enclose the expression being analysed in the current
    /// function, so `break` and `continue` outside of one are reported.
    loop_depth: usize,
    /// What the current batch's analysis has found so far.
    result: AnalysisResult,
    /// The symbols the current batch's uses pick, which the layout turns into
    /// locations at the end of the batch.
    picks: Picks,
    /// How many resolver scopes earlier batches created. The current batch's
    /// scopes come after them.
    laid_out_scopes: usize,
}

impl Analyser {
    /// An analyser for programs that can call `natives`, given as name and
    /// type in registry order: the first is the native in global slot 0.
    pub fn new(natives: Vec<(String, StaticType)>) -> Self {
        let resolver = NameResolver::new(
            natives.iter().map(|(name, _)| name.as_str()),
            StaticType::BUILTIN_TYPE_NAMES.iter().copied(),
        );
        let mut types = TypeTable::default();
        for ((_, typ), symbol) in natives.into_iter().zip(resolver.native_symbols()) {
            types.declare(symbol, TypeBinding::Inferred(typ));
        }
        Self {
            resolver,
            types,
            struct_registry: StructRegistry::default(),
            struct_types: AHashMap::new(),
            return_type_stack: Vec::new(),
            loop_depth: 0,
            result: AnalysisResult::default(),
            picks: Picks::default(),
            laid_out_scopes: 0,
        }
    }

    /// Take the accumulated analysis result (including any errors),
    /// resetting it for the next analysis.
    pub fn take_result(&mut self) -> AnalysisResult {
        let mut result = std::mem::take(&mut self.result);
        Layout::new(&self.resolver).lay_out(
            std::mem::take(&mut self.picks),
            self.laid_out_scopes,
            &mut result.analysis,
        );
        self.laid_out_scopes = self.resolver.scope_count();
        result
    }

    /// Records an analysis error. The analyser continues with a fallback type
    /// (usually `Any`) so that subsequent code is still checked.
    fn emit(&mut self, err: AnalysisError) {
        self.result.errors.push(err);
    }

    /// Resolves a syntactic type annotation to a `StaticType`: `Int` becomes
    /// `StaticType::Int`, `Map<String, Int>` becomes `StaticType::Map { .. }`,
    /// and a struct name becomes that struct's `StaticType::Struct`. The
    /// resolver has already matched each name to its type symbol. A wrong
    /// generic-argument count is reported as an analysis error at the
    /// annotation's span, and the annotation falls back to `Any` so the rest
    /// of the program is still checked.
    fn lower_type_expr(&mut self, expr: &TypeExpr) -> StaticType {
        match expr {
            TypeExpr::Tuple { elements, .. } => {
                StaticType::Tuple(elements.iter().map(|e| self.lower_type_expr(e)).collect())
            }
            TypeExpr::Name {
                name,
                args,
                span,
                id,
            } => {
                let has_args = !args.is_empty();
                let args = args.iter().map(|a| self.lower_type_expr(a)).collect();
                let symbol = self.resolver.type_name_symbol(*id);
                match self.struct_types.get(&symbol) {
                    Some(_) if has_args => {
                        self.emit(AnalysisError::type_does_not_take_generic_args(name, *span));
                        StaticType::Any
                    }
                    Some(info) => info.static_type(),
                    // A built-in type: `from_name_and_args` checks its generic arity.
                    None => match StaticType::from_name_and_args(name, args) {
                        Ok(typ) => typ,
                        Err(err) => {
                            self.emit(AnalysisError::invalid_type_annotation(&err, *span));
                            StaticType::Any
                        }
                    },
                }
            }
        }
    }

    fn lower_annotation(&mut self, annotation: Option<&TypeExpr>) -> Option<StaticType> {
        annotation.map(|expr| self.lower_type_expr(expr))
    }

    /// Analyses a top-level statement. Names resolve first; a statement with
    /// unresolved names isn't analysed further, which would only repeat its errors.
    /// The names it still declared are bound as `Any`, so in
    /// `fn f() { missing } let g = f`, only `missing` is reported.
    pub fn analyse(&mut self, expr_loc: &ExpressionLocation) -> StaticType {
        let bound_before = self.resolver.current_bindings().len();
        self.resolver.resolve_statement(expr_loc);
        let name_errors = self.resolver.take_errors();
        if !name_errors.is_empty() {
            self.result
                .errors
                .extend(name_errors.into_iter().map(AnalysisError::from));
            let declared = self.resolver.current_bindings()[bound_before..].to_vec();
            for symbol in declared {
                self.types
                    .declare(symbol, TypeBinding::Inferred(StaticType::Any));
            }
            return StaticType::Any;
        }
        self.analyse_expr(expr_loc)
    }

    fn analyse_expr(&mut self, expr_loc: &ExpressionLocation) -> StaticType {
        let typ = self.analyse_inner(expr_loc);
        self.result
            .analysis
            .expr_types
            .insert(expr_loc.id, typ.clone());
        typ
    }

    /// Like [`Self::analyse_expr`], but with an expected type from an annotation
    /// or an annotated target: `let x: List<Int> = []` types the literal as
    /// `List<Int>` instead of `List<Any>`. A container literal with no
    /// elements has nothing to infer from, so it adopts the expected element
    /// types; non-empty literals recurse so nested empties (`[(1, [])]`)
    /// adopt too. Everything else falls back to plain analysis, and the
    /// caller still subtype-checks the result against the expectation.
    ///
    /// This is a temporary measure until the type system supports type
    /// parameters and proper unification, at which point empty literals can
    /// be typed with fresh type variables instead of adopting the annotation.
    fn analyse_with_expected(
        &mut self,
        expr_loc: &ExpressionLocation,
        expected: &StaticType,
    ) -> StaticType {
        let typ = match (&expr_loc.expression, expected) {
            (
                Expression::List { values },
                StaticType::List(element) | StaticType::Sequence(element),
            ) => {
                let mut element_type: Option<StaticType> = None;
                for value in values {
                    let found = self.analyse_with_expected(value, element);
                    Self::fold_lub(&mut element_type, found);
                }
                StaticType::List(Box::new(
                    element_type.unwrap_or_else(|| element.as_ref().clone()),
                ))
            }
            (Expression::Map { values, default }, StaticType::Map { key, value }) => {
                let is_empty = values.is_empty();
                let mut key_type: Option<StaticType> = None;
                let mut value_type: Option<StaticType> = None;
                for (entry_key, entry_value) in values {
                    let found = self.analyse_with_expected(entry_key, key);
                    Self::fold_lub(&mut key_type, found);
                    if let Some(entry_value) = entry_value {
                        let found = self.analyse_with_expected(entry_value, value);
                        Self::fold_lub(&mut value_type, found);
                    }
                }
                if let Some(default) = default {
                    let found = self.analyse_with_expected(default, value);
                    Self::fold_lub(&mut value_type, found);
                }
                StaticType::Map {
                    key: Box::new(key_type.unwrap_or_else(|| key.as_ref().clone())),
                    value: Box::new(value_type.unwrap_or_else(|| {
                        if is_empty {
                            value.as_ref().clone()
                        } else {
                            // A set literal like `%{1, 2}` has unit values.
                            StaticType::unit()
                        }
                    })),
                }
            }
            (Expression::Tuple { values }, StaticType::Tuple(elements))
                if values.len() == elements.len() =>
            {
                let types = values
                    .iter()
                    .zip(elements)
                    .map(|(value, element)| self.analyse_with_expected(value, element))
                    .collect();
                StaticType::Tuple(types)
            }
            _ => return self.analyse_expr(expr_loc),
        };
        self.result
            .analysis
            .expr_types
            .insert(expr_loc.id, typ.clone());
        typ
    }

    fn analyse_inner(
        &mut self,
        ExpressionLocation {
            expression,
            span,
            id,
        }: &ExpressionLocation,
    ) -> StaticType {
        match expression {
            Expression::BoolLiteral(_) => StaticType::Bool,
            Expression::NoneLiteral => StaticType::Option(Box::new(StaticType::Any)),
            Expression::StringLiteral(_) => StaticType::String,
            Expression::NumericLiteral(NumericLiteral::Int64(_)) => StaticType::Int,
            Expression::NumericLiteral(NumericLiteral::Float64(_)) => StaticType::Float,
            Expression::NumericLiteral(
                NumericLiteral::NumberInt(_)
                | NumericLiteral::NumberFloat(_)
                | NumericLiteral::Complex(_),
            ) => StaticType::Number,
            Expression::Break => {
                if self.loop_depth == 0 {
                    self.emit(AnalysisError::break_outside_loop(*span));
                }
                StaticType::Never
            }
            Expression::Continue => {
                if self.loop_depth == 0 {
                    self.emit(AnalysisError::continue_outside_loop(*span));
                }
                StaticType::Never
            }
            Expression::Identifier { .. } => {
                let symbol = self.resolver.innermost_reference(*id);
                let binding = Binding::Resolved(Candidate::Scalar(symbol));
                self.picks.bindings.insert(*id, binding);
                self.types.get_type(symbol).clone()
            }
            Expression::Statement(inner) => {
                let typ = self.analyse_expr(inner);
                // Diverging statements (return/break/continue) propagate Never
                // so that blocks can see that control doesn't fall through.
                if typ == StaticType::Never {
                    StaticType::Never
                } else {
                    StaticType::unit()
                }
            }
            Expression::Logical { left, right, .. } => {
                self.analyse_expr(left);
                self.analyse_expr(right);
                StaticType::Bool
            }
            Expression::Grouping(expr) => self.analyse_expr(expr),
            Expression::Cast { value, annotation } => {
                let target = self.lower_type_expr(annotation);
                let found = self.analyse_with_expected(value, &target);
                if !found.overlaps(&target) {
                    self.emit(AnalysisError::impossible_cast(&found, &target, *span));
                }
                let check = (!found.is_subtype(&target)).then(|| target.clone());
                self.result.analysis.cast_checks.insert(*id, check);
                target
            }
            Expression::VariableDeclaration {
                l_value,
                annotated_type,
                value,
            } => {
                let annotated_type = self.lower_annotation(annotated_type.as_ref());
                let value_span = value.span;
                let found_type = match &annotated_type {
                    Some(expected) => self.analyse_with_expected(value, expected),
                    None => self.analyse_expr(value),
                };

                self.declare_binding_pattern(
                    l_value,
                    annotated_type,
                    found_type.clone(),
                    value_span,
                );
                StaticType::unit()
            }
            Expression::Assignment { l_value, r_value } => {
                let old_type = self.analyse_assignment_target(l_value, *span);
                let new_type = self.analyse_with_expected(r_value, &old_type);
                self.validate_assignment_write(l_value, &old_type, &new_type, *span);

                StaticType::unit()
            }
            Expression::OpAssignment {
                l_value,
                r_value,
                operation,
            } => self.analyse_op_assignment(*id, l_value, r_value, operation, *span),
            Expression::FunctionDeclaration {
                name,
                parameters,
                body,
                return_annotation,
                ..
            } => self.analyse_function_declaration(
                *id,
                name.as_deref(),
                parameters,
                body,
                return_annotation.as_ref(),
                *span,
            ),
            Expression::Block { statements } => {
                let mut last = None;
                for s in statements {
                    last = Some(self.analyse_expr(s));
                }

                last.unwrap_or_else(StaticType::unit)
            }
            Expression::If {
                condition,
                on_true,
                on_false,
            } => {
                self.analyse_expr(condition);
                let true_type = self.analyse_expr(on_true);
                let false_type = if let Some(on_false) = on_false {
                    self.analyse_expr(on_false)
                } else {
                    StaticType::unit()
                };

                true_type.lub(&false_type)
            }
            Expression::While {
                expression,
                loop_body,
            } => {
                // The condition is part of the loop too, as it runs on every
                // iteration.
                self.loop_depth += 1;
                self.analyse_expr(expression);
                self.analyse_expr(loop_body);
                self.loop_depth -= 1;
                StaticType::unit()
            }
            Expression::For { iterations, body } => {
                // A map comprehension's default is evaluated once, before the
                // loop starts.
                if let ForBody::Map {
                    default: Some(default),
                    ..
                } = body.as_ref()
                {
                    self.analyse_expr(default);
                }
                self.analyse_for_iterations(iterations, body)
            }
            Expression::Call {
                function,
                arguments,
            } => self.analyse_call(function, arguments, CallKind::Regular, *span),
            Expression::OperatorCall {
                function,
                arguments,
            } => self.analyse_call(function, arguments, CallKind::Operator, *span),
            Expression::MemberAccess {
                receiver,
                member,
                member_span,
            } => self.analyse_member_access(*id, receiver, member, *member_span),
            Expression::Tuple { values } => {
                let mut types = Vec::with_capacity(values.len());
                for v in values {
                    types.push(self.analyse_expr(v));
                }

                StaticType::Tuple(types)
            }
            Expression::List { values } => {
                let element_type = self.analyse_multiple_expression_with_same_type(values);

                StaticType::List(Box::new(element_type.unwrap_or(StaticType::Any)))
            }
            Expression::Map { values, default } => {
                let mut key_type: Option<StaticType> = None;
                let mut value_type: Option<StaticType> = None;
                for (key, value) in values {
                    Self::fold_lub(&mut key_type, self.analyse_expr(key));
                    if let Some(value) = value {
                        Self::fold_lub(&mut value_type, self.analyse_expr(value));
                    }
                }

                // Reads can produce the default value, so its type is part of
                // the map's value type.
                if let Some(default) = default {
                    let default_type = self.analyse_expr(default);
                    Self::fold_lub(&mut value_type, default_type);
                }

                StaticType::Map {
                    key: Box::new(key_type.unwrap_or(StaticType::Any)),
                    value: Box::new(value_type.unwrap_or_else(StaticType::unit)),
                }
            }
            Expression::Return { value } => {
                let typ = self.analyse_expr(value);
                match self.return_type_stack.last_mut() {
                    Some(slot) => Self::fold_lub(slot, typ),
                    None => self.emit(AnalysisError::return_outside_function(*span)),
                }
                StaticType::Never
            }
            Expression::RangeInclusive { start, end }
            | Expression::RangeExclusive { start, end } => {
                if let Some(start) = start {
                    self.analyse_expr(start);
                }
                if let Some(end) = end {
                    self.analyse_expr(end);
                }

                StaticType::Iterator(Box::new(StaticType::Int))
            }
            Expression::StructDeclaration { name, fields } => {
                self.analyse_struct_declaration(*id, name, fields)
            }
        }
    }

    /// Analyses the iterations of a `for` from the first, and then its body.
    /// Each iteration's sequence is evaluated outside of its own loop, and what
    /// follows it inside. A map comprehension's default is analysed before.
    fn analyse_for_iterations(
        &mut self,
        iterations: &[ForIteration],
        body: &ForBody,
    ) -> StaticType {
        let Some((iteration, tail)) = iterations.split_first() else {
            unreachable!("because this function is never called with an empty slice");
        };

        match iteration {
            ForIteration::Iteration { l_value, sequence } => {
                let sequence_span = sequence.span;
                let sequence_type = self.analyse_expr(sequence);

                let found_type = sequence_type
                    .sequence_element_type()
                    .unwrap_or(StaticType::Any);

                self.loop_depth += 1;
                // A `for` binder has no type annotation.
                self.declare_binding_pattern(l_value, None, found_type, sequence_span);
            }
            ForIteration::Guard(expr) => {
                self.analyse_expr(expr);
            }
        }

        let typ = if !tail.is_empty() {
            self.analyse_for_iterations(tail, body)
        } else {
            match body {
                ForBody::Block(block) => {
                    self.analyse_expr(block);
                    StaticType::unit()
                }
                ForBody::List { expr } => StaticType::List(Box::new(self.analyse_expr(expr))),
                ForBody::Map { key, value, .. } => {
                    let key_type = self.analyse_expr(key);
                    let value_type = if let Some(value) = value {
                        self.analyse_expr(value)
                    } else {
                        StaticType::unit()
                    };

                    StaticType::Map {
                        key: Box::new(key_type),
                        value: Box::new(value_type),
                    }
                }
            }
        };
        if matches!(iteration, ForIteration::Iteration { .. }) {
            self.loop_depth -= 1;
        }
        typ
    }

    fn analyse_multiple_expression_with_same_type(
        &mut self,
        expressions: &Vec<ExpressionLocation>,
    ) -> Option<StaticType> {
        let mut element_type: Option<StaticType> = None;
        for expression in expressions {
            Self::fold_lub(&mut element_type, self.analyse_expr(expression));
        }
        element_type
    }

    /// Fold a new type into an accumulator via least-upper-bound.
    fn fold_lub(acc: &mut Option<StaticType>, new_type: StaticType) {
        match acc {
            Some(prev) => *prev = prev.lub(&new_type),
            None => *acc = Some(new_type),
        }
    }
}
