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
mod instances;
mod solver;

use assignments::DeferredWrite;
use declarations::FunctionDeclaration;
pub use generics::tied_natives;
use instances::{GenericUse, Pending, Scheme};
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
    schemes: AHashMap<SymbolId, Scheme>,
    /// What a call just copied from a generic function, until the call is
    /// known to commit to it.
    pending: Pending,
    /// The uses of generic functions in the batch whose instances aren't
    /// known yet.
    uses: Vec<GenericUse>,
    /// The instance of each generic function for each set of types of its
    /// key that a call gave it.
    instances: AHashMap<(SymbolId, Vec<StaticType>), SymbolId>,
    /// The instance whose constraints are being solved, which gets the
    /// picks they make.
    recording: Option<SymbolId>,
    /// The generic functions the batch declares, which the batch compiles
    /// instances of.
    declared_in_batch: AHashSet<SymbolId>,
    /// The generic function whose body is being analysed.
    declaring: Option<SymbolId>,
    /// The calls its body makes of itself.
    recursive_calls: Vec<NodeId>,
    /// The nodes of overloaded function values that waited for the type
    /// expected of them, as `abs` in `[len, abs]` does.
    /// The functions the batch declares, in the order their declarations
    /// start, so a generic function knows the ones its body declares.
    declared_functions: Vec<NodeId>,
    waiting_function_values: AHashSet<NodeId>,
    /// The types an anonymous function's unannotated parameters get from
    /// the call it's passed to, by the function's node: in
    /// `players.map(fn(p) => p.score)`, `p` is a `Player`.
    expected_parameters: AHashMap<NodeId, Vec<Option<StaticType>>>,
    /// The calls the constraint being solved was copied through, when it's
    /// a copy of a generic function's constraint.
    copied_through: Option<Origin>,
    /// The instance and the calls each literal made while solving a copy of
    /// a generic function's constraints was made in, keyed by its element
    /// type, so an error in its values is reported as one made there.
    literal_origins: AHashMap<TypeVar, (Option<SymbolId>, Option<Origin>)>,
    /// The writes of values that share no type, which are checked again
    /// once those are settled.
    deferred_writes: Vec<DeferredWrite>,
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
    /// What each enclosing function returns with `return`, innermost last.
    /// Pushed on function entry, popped on exit.
    return_type_stack: Vec<Returns>,
    /// The loops that enclose the expression being analysed in the current
    /// function, innermost last, each with whether a `break` leaves it: a
    /// `while true` that none leaves never ends. `break` and `continue`
    /// outside of one are reported.
    loops: Vec<bool>,
    /// What the current batch's analysis has found so far.
    result: AnalysisResult,
    /// The symbols the current batch's uses pick, which the layout turns into
    /// locations at the end of the batch.
    picks: Picks,
    /// How many resolver scopes earlier batches created. The current batch's
    /// scopes come after them.
    laid_out_scopes: usize,
}

/// What a function returns.
#[derive(Debug)]
pub(super) struct Returns {
    /// Its annotated return type.
    annotated: Option<StaticType>,
    /// The type its values have in common, without an annotation.
    joined: Option<StaticType>,
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
            pending: Pending::default(),
            uses: Vec::new(),
            instances: AHashMap::new(),
            recording: None,
            declared_in_batch: AHashSet::new(),
            declaring: None,
            recursive_calls: Vec::new(),
            waiting_function_values: AHashSet::new(),
            declared_functions: Vec::new(),
            expected_parameters: AHashMap::new(),
            copied_through: None,
            literal_origins: AHashMap::new(),
            deferred_writes: Vec::new(),
            next_serial: 0,
            watchers: AHashMap::new(),
            watched_settlings: 0,
            failed_reads: AHashSet::new(),
            early_picks: Vec::new(),
            struct_registry: StructRegistry::default(),
            struct_types: AHashMap::new(),
            return_type_stack: Vec::new(),
            loops: Vec::new(),
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
        self.vars.forget_literals();
        self.literal_origins.clear();
        debug_assert!(
            self.deferred_writes.is_empty(),
            "a write waits for literals that are all settled"
        );
        self.instantiate_uses();
        self.declared_in_batch.clear();
        debug_assert!(self.waiting.is_empty(), "unsolved: {:?}", self.waiting);
        self.watchers.clear();
        if cfg!(debug_assertions) {
            self.check_early_picks();
        }
        self.failed_reads.clear();
        self.waiting_function_values.clear();
        self.declared_functions.clear();
        // A function an instance's body declares takes what the instance's
        // types make it take.
        for picks in self.picks.instances.values_mut() {
            for typ in picks.function_types.values_mut() {
                *typ = self.vars.finish(typ).replace(&|part| {
                    matches!(part, StaticType::Var(_) | StaticType::Generic(_))
                        .then_some(StaticType::Any)
                });
            }
        }
        self.types.resolve_all(&self.vars);
        let analysis = &mut self.result.analysis;
        for table in [
            &mut analysis.expr_types,
            &mut analysis.variable_types,
            &mut analysis.inferred_return_types,
            &mut analysis.function_types,
        ] {
            for typ in table.values_mut() {
                *typ = self.vars.finish(typ);
                debug_assert!(!typ.has_vars(), "unsettled type variable in {typ}");
            }
        }
        for typ in analysis.cast_checks.values_mut().flatten() {
            *typ = self.vars.finish(typ);
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
    /// (usually [`StaticType::Error`], which nothing reports again) so that
    /// the rest of the program is still checked. An error in
    /// a copy of a generic function's constraint is the call's: it's
    /// reported there, with notes pointing into the function.
    fn emit(&mut self, err: AnalysisError) {
        // An instance's constraints were checked where they were copied to
        // its calls.
        if self.recording.is_some() {
            return;
        }
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
    /// annotation's span, and the annotation is a failed check so the rest
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
                        StaticType::Error
                    }
                    Some(info) => info.static_type(),
                    // A built-in type: `from_name_and_args` checks its generic arity.
                    None => match StaticType::from_name_and_args(name, args) {
                        Ok(typ) => typ,
                        Err(err) => {
                            self.emit(AnalysisError::invalid_type_annotation(&err, *span));
                            StaticType::Error
                        }
                    },
                }
            }
        }
    }

    /// Lowers an annotation of something other than a parameter, which an
    /// `Iterable<T>` can't be: `let xs: Iterable<Int>` doesn't say what
    /// kind of sequence `xs` is.
    fn lower_annotation(&mut self, annotation: Option<&TypeExpr>) -> Option<StaticType> {
        annotation.map(|expr| self.lower_value_type(expr))
    }

    /// [`Self::lower_type_expr`] for the type of a value, which an
    /// `Iterable<T>` is never part of: only a built-in function takes any
    /// kind of sequence of a type.
    fn lower_value_type(&mut self, expr: &TypeExpr) -> StaticType {
        let typ = self.lower_type_expr(expr);
        self.without_iterables(typ, expr.span())
    }

    /// `typ`, reporting each `Iterable<T>` in it, which becomes the type of
    /// a failed check.
    fn without_iterables(&mut self, typ: StaticType, span: Span) -> StaticType {
        if !typ.contains(&|part| matches!(part, StaticType::Iterable(_))) {
            return typ;
        }
        self.emit(AnalysisError::iterable_in_an_annotation(span));
        typ.replace(&|part| matches!(part, StaticType::Iterable(_)).then_some(StaticType::Error))
    }

    /// Analyses a top-level statement. Names resolve first; a statement with
    /// unresolved names isn't analysed further, which would only repeat its errors.
    /// The names it still declared are bound as failed checks, so in
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
                self.types.declare(symbol, StaticType::Error);
            }
            return StaticType::Error;
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
            Expression::NumericLiteral(NumericLiteral::Int64(_)) => self.vars.fresh_number(*span),
            Expression::NumericLiteral(NumericLiteral::Float64(_)) => StaticType::Float,
            Expression::NumericLiteral(
                NumericLiteral::NumberInt(_)
                | NumericLiteral::NumberFloat(_)
                | NumericLiteral::Complex(_),
            ) => StaticType::Number,
            Expression::Break => {
                match self.loops.last_mut() {
                    Some(broken) => *broken = true,
                    None => self.emit(AnalysisError::break_outside_loop(*span)),
                }
                StaticType::Never
            }
            Expression::Continue => {
                if self.loops.is_empty() {
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
            Expression::Cast {
                value,
                annotation,
                optional,
            } => {
                let target = self.lower_value_type(annotation);
                let found = self.analyse_expr(value);
                // What isn't known about the value yet is what the cast says
                // it is: `(fn(x) => x) as Fn(Int) -> Int` takes an Int, and
                // needs no check as the program runs, and `1 as Float` is a
                // Float. Known parts are checked, and so is a value nothing
                // is known about, like what a call that waits returns.
                // `as?` only tests the value, which stays what it is: in `fn
                // f(xs) { (xs as? List<Int>).is_some() }`, `xs` can still be a
                // list of Strings. A number written there is what it's cast
                // to, as with `as`: `1 as? Float` is `Some(1.0)`.
                let found = self.vars.resolve(&found);
                let unknown = matches!(found, StaticType::Var(var) if !self.vars.is_literal(var));
                let literal = matches!(found, StaticType::Var(var) if self.vars.is_literal(var));
                if found.has_vars() && !unknown && (!*optional || literal) {
                    self.vars.try_unify(&found, &target);
                }
                let found = self.vars.resolve(&found);
                if !found.overlaps(&target) {
                    let found = self.vars.shown(&found);
                    self.emit(AnalysisError::impossible_cast(&found, &target, *span));
                }
                let check = (!found.fits(&target)).then(|| target.clone());
                self.result.analysis.cast_checks.insert(*id, check);
                // `as?` is the same cast, with `None` where `as` would fail.
                // One that can never succeed is an error like `as`: it would
                // always be `None`.
                if *optional {
                    StaticType::Option(Box::new(target))
                } else {
                    target
                }
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
                pure,
                ..
            } => self.analyse_function_declaration(FunctionDeclaration {
                id: *id,
                name: name.as_deref(),
                parameters,
                body,
                return_annotation: return_annotation.as_ref(),
                pure: *pure,
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
                // An `if` without an `else` is `()`, whatever its branch is:
                // `if c { 1 }` has no value when `c` is false.
                let Some(on_false) = on_false else {
                    return StaticType::unit();
                };
                let false_type = self.analyse_expr(on_false);
                self.join(&true_type, &false_type, *span)
            }
            Expression::While {
                expression,
                loop_body,
            } => {
                // The condition is part of the loop too, as it runs on every
                // iteration.
                self.loops.push(false);
                self.analyse_expr(expression);
                self.analyse_expr(loop_body);
                let broken = self.loops.pop().expect("the loop was pushed");
                // A `while true` that no `break` leaves only ends by
                // returning, so it has no value, like a `return`: a function
                // whose body ends in one returns what its `return`s do.
                if !broken && matches!(expression.expression, Expression::BoolLiteral(true)) {
                    StaticType::Never
                } else {
                    StaticType::unit()
                }
            }
            Expression::For { iterations, body } => {
                // A map comprehension's default is evaluated once, before the
                // loop starts.
                let default = match body.as_ref() {
                    ForBody::Map {
                        default: Some(default),
                        ..
                    } => {
                        let typ = self.analyse_expr(default);
                        Some(self.constrain_default_value(typ, default.span))
                    }
                    _ => None,
                };
                match (self.analyse_for_iterations(iterations, body), default) {
                    // Reads can produce the default value, so it's one of the
                    // map's values: `%{:"none", x: x for x in xs}` holds
                    // Strings as well as what `xs` does.
                    (StaticType::Map { key, value }, Some(default)) => StaticType::Map {
                        key,
                        value: Box::new(self.join(&value, &default, *span)),
                    },
                    (typ, _) => typ,
                }
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
            // Each element of a tuple literal takes the type its uses settle,
            // as a list literal's elements do: `let t: (Any, Int) = ("a", 1)`.
            // An element whose type isn't known yet already does.
            Expression::Tuple { values } => {
                let mut types = Vec::with_capacity(values.len());
                for value in values {
                    let typ = self.analyse_expr(value);
                    types.push(match self.vars.resolve(&typ) {
                        StaticType::Var(_) => typ,
                        _ => self.holding(vec![(Some(value.id), value.span, typ)], value.span),
                    });
                }

                StaticType::Tuple(types)
            }
            Expression::List { values } => {
                let element_type = if values.is_empty() {
                    self.vars.fresh(StaticType::Any)
                } else {
                    let values = values
                        .iter()
                        .map(|value| (Some(value.id), value.span, self.analyse_expr(value)))
                        .collect();
                    self.holding(values, *span)
                };
                StaticType::List(Box::new(element_type))
            }
            Expression::Map { values, default } => {
                let mut keys = Vec::new();
                let mut held = Vec::new();
                for (key, value) in values {
                    keys.push((Some(key.id), key.span, self.analyse_expr(key)));
                    if let Some(value) = value {
                        held.push((Some(value.id), value.span, self.analyse_expr(value)));
                    }
                }
                // A key without a value holds `()`, which in `%{"a", "b": 1}`
                // is one of the map's values too.
                if !held.is_empty() {
                    for (key, _) in values.iter().filter(|(_, value)| value.is_none()) {
                        held.push((None, key.span, StaticType::unit()));
                    }
                }

                // Reads can produce the default value, so its type is part of
                // the map's value type. A default that's a function is
                // called for each missing key, so what it makes isn't the
                // default's own value.
                if let Some(default) = default {
                    let default_type = self.analyse_expr(default);
                    let default_type = self.constrain_default_value(default_type, default.span);
                    held.push((None, default.span, default_type));
                }

                let key_type = if keys.is_empty() {
                    self.vars.fresh(StaticType::Any)
                } else {
                    self.holding(keys, *span)
                };
                // A literal with keys but no values, like `%{1, 2}`, is a set.
                let value_type = if !held.is_empty() {
                    self.holding(held, *span)
                } else if values.is_empty() {
                    self.vars.fresh(StaticType::unit())
                } else {
                    StaticType::unit()
                };
                StaticType::Map {
                    key: Box::new(key_type),
                    value: Box::new(value_type),
                }
            }
            Expression::Return { value } => {
                let typ = self.analyse_expr(value);
                match self.return_type_stack.pop() {
                    Some(mut returns) => {
                        self.add_return(&mut returns, value, typ);
                        self.return_type_stack.push(returns);
                    }
                    None => self.emit(AnalysisError::return_outside_function(*span)),
                }
                StaticType::Never
            }
            Expression::RangeInclusive { start, end }
            | Expression::RangeExclusive { start, end } => {
                // A range counts in Ints, so its bounds are Ints: in `0..n`,
                // `n` is one, whatever else it's used for.
                for bound in [start, end].into_iter().flatten() {
                    let typ = self.analyse_expr(bound);
                    self.check_write(&StaticType::Int, &typ, bound.span);
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

                self.loops.push(false);
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
            self.loops.pop();
        }
        typ
    }

    /// The type of the elements of the literal at `span` holding `values`,
    /// each with where it is and its type, which takes the type its uses
    /// settle: in `let xs: List<Any> = [1, 2]`, `Any`.
    fn holding(
        &mut self,
        values: Vec<(Option<NodeId>, Span, StaticType)>,
        span: Span,
    ) -> StaticType {
        let element = self.fresh_holding(
            values
                .iter()
                .map(|(_, span, typ)| (typ.clone(), *span))
                .collect(),
            span,
        );
        // An overloaded function value waiting for its type, as `abs` in
        // `[len, abs]`, is one of the literal's values.
        for node in values.into_iter().filter_map(|(node, _, _)| node) {
            if self.waiting_function_values.contains(&node) {
                for waiting in self.waiting.values_mut() {
                    if let Constraint::FunctionValue(value) = &mut waiting.constraint
                        && value.id() == node
                    {
                        value.mark_in_literal();
                    }
                }
            }
        }
        element
    }

    /// [`TypeVars::fresh_holding`], remembering where the literal is made
    /// for [`Self::report_unshared`].
    pub(super) fn fresh_holding(
        &mut self,
        held: Vec<(StaticType, Span)>,
        span: Span,
    ) -> StaticType {
        let element = self.vars.fresh_holding(held, span);
        let StaticType::Var(var) = element else {
            unreachable!("a literal's type is a variable");
        };
        if self.recording.is_some() || self.copied_through.is_some() {
            self.literal_origins
                .insert(var, (self.recording, self.copied_through.clone()));
        } else {
            self.literal_origins.remove(&var);
        }
        element
    }

    /// Reports the literals settled as errors since this last ran, as their
    /// values share no type, as an error made where each literal was.
    pub(super) fn report_unshared(&mut self) {
        for unshared in self.vars.take_unshared() {
            let origin = self
                .literal_origins
                .get(&unshared.literal)
                .cloned()
                .unwrap_or_default();
            let recording = std::mem::replace(&mut self.recording, origin.0);
            let copied_through = std::mem::replace(&mut self.copied_through, origin.1);
            self.emit(AnalysisError::values_share_no_type(&unshared));
            self.recording = recording;
            self.copied_through = copied_through;
        }
        self.recheck_deferred_writes();
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

    /// Adds the value `value` of type `typ` that a function returns to
    /// `returns`. A function with an annotated return type returns that
    /// type, and each value it returns has to fit it. Without one, it
    /// returns the type its values join as, as with [`Self::join`].
    fn add_return(&mut self, returns: &mut Returns, value: &ExpressionLocation, typ: StaticType) {
        if let Some(annotated) = &returns.annotated {
            self.check_write(annotated, &typ, value.span);
            return;
        }
        returns.joined = Some(match returns.joined.take() {
            Some(joined) => self.join(&joined, &typ, value.span),
            None => typ,
        });
    }
}
