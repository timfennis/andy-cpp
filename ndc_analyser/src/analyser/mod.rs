use crate::error::AnalysisError;
use crate::overloads::CallKind;
use crate::types::{TypeTable, TypeVars};
use ahash::{AHashMap, AHashSet};
use ndc_analysis::Analysis;
use ndc_core::r#struct::{StructInfo, StructRegistry};
use ndc_core::{NodeId, StaticType, TypeVar};
use ndc_layout::{Layout, Picks};
use ndc_lexer::{NumericLiteral, Span};
use ndc_parser::{Expression, ExpressionLocation, ForBody, ForIteration, TypeExpr};
use ndc_resolver::{NameResolver, SymbolId};
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::rc::Rc;

mod assignments;
mod calls;
mod declarations;
mod function_values;
pub(crate) mod generics;
mod solver;

use declarations::FunctionDeclaration;
pub use generics::tied_natives;
use solver::{Constraint, EarlyPick, Origin, Waiting};
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
    /// What the type variables in those types stand for, once known.
    vars: TypeVars,
    /// The calls and other uses that wait for types that aren't known yet,
    /// by the order they started waiting in.
    waiting: BTreeMap<u64, Waiting>,
    /// The constraints of each generic function declared in the program,
    /// such as `+(A, Int) -> B` for `fn inc(x) => x + 1`, which a call of it
    /// copies with the types it's called with.
    schemes: AHashMap<SymbolId, Rc<[Waiting]>>,
    /// The constraints a call just copied from a generic function, with the
    /// function, until the call is known to commit to it.
    pending_copies: Vec<(SymbolId, Constraint, Option<Origin>)>,
    /// The calls the constraint being solved was copied through, when it's
    /// a copy of a generic function's constraint.
    copied_through: Option<Origin>,
    /// The number the next waiting constraint gets.
    next_serial: u64,
    /// The waiting constraints that each unsettled variable would let make
    /// progress once settled.
    watchers: AHashMap<TypeVar, Vec<u64>>,
    /// [`TypeVars::settled_count`] when the waiting constraints were last
    /// woken, so the ones watching a variable settled since are woken next.
    watched_settlings: usize,
    /// The calls that committed to an overload before their arguments were
    /// known, which debug builds check at the end of the batch.
    early_picks: Vec<EarlyPick>,
    /// The nodes of `xs[i] op= v` whose read of `xs[i]` failed, so the
    /// write, which fails the same way, isn't reported again.
    failed_reads: AHashSet<NodeId>,
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
            types.declare(symbol, typ);
        }
        Self {
            resolver,
            types,
            vars: TypeVars::default(),
            waiting: BTreeMap::new(),
            schemes: AHashMap::new(),
            pending_copies: Vec::new(),
            copied_through: None,
            next_serial: 0,
            watchers: AHashMap::new(),
            watched_settlings: 0,
            failed_reads: AHashSet::new(),
            early_picks: Vec::new(),
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
        // Nothing after this batch can settle its type variables any more,
        // so the ones left unsettled get their fallbacks, what waited on them
        // is solved, and no variable leaves the analyser.
        self.settle_waiting(0);
        debug_assert!(self.waiting.is_empty(), "unsolved: {:?}", self.waiting);
        self.watchers.clear();
        if cfg!(debug_assertions) {
            self.check_early_picks();
        }
        self.failed_reads.clear();
        self.types.resolve_all(&self.vars);
        let analysis = &mut self.result.analysis;
        for table in [
            &mut analysis.expr_types,
            &mut analysis.variable_types,
            &mut analysis.inferred_return_types,
            &mut analysis.function_types,
        ] {
            for typ in table.values_mut() {
                *typ = self.vars.resolve(typ);
                debug_assert!(!typ.has_vars(), "unsettled type variable in {typ}");
            }
        }

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
    /// (usually `Any`) so that subsequent code is still checked. An error in
    /// a copy of a generic function's constraint is the call's: it's
    /// reported there, with notes pointing into the function.
    fn emit(&mut self, err: AnalysisError) {
        let err = match &self.copied_through {
            Some(frames) => {
                err.through_calls(frames.iter().map(|frame| (frame.name.as_str(), frame.call)))
            }
            None => err,
        };
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
            TypeExpr::Function {
                parameters,
                return_type,
                ..
            } => StaticType::Function {
                parameters: Some(parameters.iter().map(|p| self.lower_type_expr(p)).collect()),
                return_type: Box::new(
                    return_type
                        .as_ref()
                        .map_or_else(StaticType::unit, |r| self.lower_type_expr(r)),
                ),
            },
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
                self.types.declare(symbol, StaticType::Any);
            }
            return StaticType::Any;
        }
        self.analyse_expr(expr_loc)
    }

    fn analyse_expr(&mut self, expr_loc: &ExpressionLocation) -> StaticType {
        let typ = self.analyse_inner(expr_loc);
        self.solve_waiting();
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
            // What `None`, `[]` and `%{}` hold is only known from later uses.
            Expression::NoneLiteral => {
                StaticType::Option(Box::new(self.vars.fresh(StaticType::Any)))
            }
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
            Expression::Identifier { name, .. } => self.analyse_value_reference(*id, name, *span),
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
                let found = self.analyse_expr(value);
                // What isn't known about the value yet is what the cast says
                // it is: `(fn(x) => x) as Fn(Int) -> Int` takes an Int, and
                // needs no check as the program runs. Known parts are checked.
                if self.vars.resolve(&found).has_vars() {
                    self.vars.try_unify(&found, &target);
                }
                let found = self.vars.resolve(&found);
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
                let found_type = self.analyse_expr(value);

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
                let new_type = self.analyse_expr(r_value);
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
            } => self.analyse_function_declaration(FunctionDeclaration {
                id: *id,
                name: name.as_deref(),
                parameters,
                body,
                return_annotation: return_annotation.as_ref(),
                span: *span,
            }),
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

                self.join(&true_type, &false_type, *span)
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
            }
            | Expression::MethodCall {
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
                let element_type = match self.analyse_multiple_expression_with_same_type(values) {
                    Some(element_type) => element_type,
                    None => self.vars.fresh(StaticType::Any),
                };
                StaticType::List(Box::new(element_type))
            }
            Expression::Map { values, default } => {
                let mut key_type: Option<StaticType> = None;
                let mut value_type: Option<StaticType> = None;
                for (key, value) in values {
                    let found = self.analyse_expr(key);
                    self.fold_join(&mut key_type, found, key.span);
                    if let Some(value) = value {
                        let found = self.analyse_expr(value);
                        self.fold_join(&mut value_type, found, value.span);
                    }
                }

                // Reads can produce the default value, so its type is part of
                // the map's value type.
                if let Some(default) = default {
                    let default_type = self.analyse_expr(default);
                    let default_type = self.constrain_default_value(default_type, default.span);
                    self.fold_join(&mut value_type, default_type, default.span);
                }

                let key_type = match key_type {
                    Some(key_type) => key_type,
                    None => self.vars.fresh(StaticType::Any),
                };
                // A literal with keys but no values, like `%{1, 2}`, is a set.
                let value_type = match value_type {
                    Some(value_type) => value_type,
                    None if values.is_empty() => self.vars.fresh(StaticType::unit()),
                    None => StaticType::unit(),
                };
                StaticType::Map {
                    key: Box::new(key_type),
                    value: Box::new(value_type),
                }
            }
            Expression::Return { value } => {
                let typ = self.analyse_expr(value);
                match self.return_type_stack.pop() {
                    Some(mut returned) => {
                        self.fold_join(&mut returned, typ, *span);
                        self.return_type_stack.push(returned);
                    }
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

                let found_type = self.constrain_iteration(sequence_type, sequence_span);

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
            let found = self.analyse_expr(expression);
            self.fold_join(&mut element_type, found, expression.span);
        }
        element_type
    }

    /// The type of the values a map default of type `default` produces. A
    /// function default, as in `%{:fn() => []}`, is called for each missing
    /// key, so the values are what it returns.
    pub(super) fn default_value_type(default: StaticType) -> StaticType {
        match default {
            StaticType::Function { return_type, .. } => *return_type,
            other => other,
        }
    }

    /// Joins a new type into an accumulator, as with [`Self::join`].
    fn fold_join(&mut self, acc: &mut Option<StaticType>, new_type: StaticType, span: Span) {
        *acc = Some(match acc.take() {
            Some(prev) => self.join(&prev, &new_type, span),
            None => new_type,
        });
    }
}
