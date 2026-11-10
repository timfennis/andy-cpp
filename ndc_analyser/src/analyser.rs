use crate::overloads::{CallKind, Overloads, ResolvedCall};
use crate::types::{TypeBinding, TypeTable};
use ahash::AHashMap;
use itertools::{Itertools, izip};
use ndc_analysis::{Analysis, Binding, Candidate, DeclaredStruct, FieldAccessors, ResolvedVar};
use ndc_core::NodeId;
use ndc_core::static_type::StaticTypeConstructionError;
use ndc_core::r#struct::{StructInfo, StructRegistry};
use ndc_core::{StaticType, TypeSignature};
use ndc_layout::Layout;
use ndc_lexer::{NumericLiteral, Span};
use ndc_parser::{
    AssignmentTarget, AssignmentTargetLocation, BindingPattern, BindingPatternLocation, Expression,
    ExpressionLocation, ForBody, ForIteration, TypeExpr,
};
use ndc_resolver::{Candidates, NameResolutionError, NameResolver, SymbolId};
use std::fmt::Debug;
use std::rc::Rc;

/// What analysing one batch produced: the analysis, and the errors found on
/// the way.
#[derive(Debug, Default)]
pub struct AnalysisResult {
    /// What the analyser worked out, keyed by AST node.
    pub analysis: Analysis,
    /// Errors accumulated during analysis. Non-empty when the analyser
    /// encountered problems but was able to continue with fallback types.
    pub errors: Vec<AnalysisError>,
}

/// A snapshot of the analyser's persistent state: the resolver, the types of
/// the values declared so far, the struct types and the number of registered
/// structs. Restoring it discards every binding and struct declared since.
#[derive(Debug, Clone)]
pub struct Checkpoint {
    resolver: NameResolver,
    types: TypeTable,
    struct_types: AHashMap<SymbolId, Rc<StructInfo>>,
    struct_count: usize,
}

#[derive(Debug)]
pub struct Analyser {
    /// Resolves names before analysis. It runs alongside the scope tree below
    /// until the type pass reads names from it instead.
    resolver: NameResolver,
    /// Every struct declared so far, which hands out their ids.
    struct_registry: StructRegistry,
    /// The type of every value declared so far.
    types: TypeTable,
    /// What each struct's type symbol stands for, so `p: Point` lowers to
    /// `Point`'s struct type. Every other type name is built in.
    struct_types: AHashMap<SymbolId, Rc<StructInfo>>,
    /// The function declarations being analysed, innermost last. Empty at the
    /// top level.
    functions: Vec<NodeId>,
    /// Stack of explicit `return` types for each enclosing function scope.
    /// Pushed on function entry, popped on exit. The value accumulates the
    /// lub of all `return <expr>` types seen so far.
    return_type_stack: Vec<Option<StaticType>>,
    /// Side table populated during analysis.
    result: AnalysisResult,
    /// Non-fatal errors accumulated during the current analysis pass.
    errors: Vec<AnalysisError>,
}

impl Analyser {
    /// An analyser for programs that can call `natives`, given as name and
    /// type in registry order: the first is the native in global slot 0.
    pub fn new(natives: Vec<(String, StaticType)>) -> Self {
        let resolver = NameResolver::new(natives.iter().map(|(name, _)| name.as_str()));
        let mut types = TypeTable::default();
        for ((_, typ), symbol) in natives.into_iter().zip(resolver.native_symbols()) {
            types.declare(symbol, TypeBinding::Inferred(typ));
        }
        Self {
            resolver,
            types,
            struct_registry: StructRegistry::default(),
            struct_types: AHashMap::new(),
            functions: Vec::new(),
            return_type_stack: Vec::new(),
            result: AnalysisResult::default(),
            errors: Vec::new(),
        }
    }

    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint {
            resolver: self.resolver.clone(),
            types: self.types.clone(),
            struct_types: self.struct_types.clone(),
            struct_count: self.struct_registry.len(),
        }
    }

    /// Goes back to `checkpoint`, for a batch that never ran.
    pub fn restore(&mut self, checkpoint: Checkpoint) {
        self.resolver = checkpoint.resolver;
        self.types = checkpoint.types;
        self.struct_types = checkpoint.struct_types;
        self.struct_registry.truncate(checkpoint.struct_count);
    }

    /// Goes back to `checkpoint` for a batch that failed while running. What it
    /// declared is forgotten, since the VM drops those values, but what it did
    /// to values declared before stays: running may already have boxed them for
    /// a closure or assigned them a value of their widened type.
    pub fn restore_after_failed_run(&mut self, checkpoint: Checkpoint) {
        self.resolver.rewind_keeping_captures(checkpoint.resolver);
        let later_types = std::mem::replace(&mut self.types, checkpoint.types);
        self.types.keep_types_from(later_types);
        self.struct_types = checkpoint.struct_types;
        self.struct_registry.truncate(checkpoint.struct_count);
    }

    /// Take the accumulated analysis result (including any errors),
    /// resetting it for the next analysis.
    pub fn take_result(&mut self) -> AnalysisResult {
        let mut result = std::mem::take(&mut self.result);
        result.errors = std::mem::take(&mut self.errors);
        result.analysis.top_level_frame_size = Layout::new(&self.resolver).frame_size(None);
        self.resolver.end_batch();
        result
    }

    /// Records the type of a value the resolver declared, and returns where
    /// it lives: a local, or a cell if closures capture it. A declaration is
    /// always in the frame being analysed.
    fn declare(&mut self, symbol: SymbolId, binding: TypeBinding) -> ResolvedVar {
        self.types.declare(symbol, binding);
        self.locate(symbol)
    }

    /// How the code being analysed reaches `symbol`: see [`Layout::location`].
    fn locate(&self, symbol: SymbolId) -> ResolvedVar {
        Layout::new(&self.resolver).location(symbol, self.functions.last().copied())
    }

    /// Why no candidate accepts a call of `name` with arguments of these types:
    /// a variable holding a function ended the search, a cast would make an
    /// overload fit, or nothing fits.
    fn no_match(
        &self,
        candidates: &Candidates,
        name: &str,
        types: &[StaticType],
        span: Span,
    ) -> AnalysisError {
        let overloads = self.overloads();
        if overloads.variable_ending_search(candidates).is_some() {
            AnalysisError::variable_rejects_arguments(name, types, span)
        } else if overloads.a_cast_could_resolve(candidates, types) {
            AnalysisError::function_not_found_suggest_cast(name, types, span)
        } else {
            AnalysisError::function_not_found(name, types, span)
        }
    }

    /// Overload resolution over the types declared so far.
    fn overloads(&self) -> Overloads<'_> {
        Overloads::new(&self.types, self.resolver.symbols())
    }

    /// Reports a read `x.name`, the call `name(x)`, that a value of unknown
    /// type named `name` makes ambiguous, as [`Overloads::any_value_hiding_function`]
    /// describes. `node` is the read's, which holds its getter candidates.
    fn check_member_read(&mut self, node: NodeId, receiver: &StaticType, member: &str, span: Span) {
        let ambiguous = self
            .resolver
            .getter_candidates(node)
            .is_some_and(|getters| {
                self.overloads()
                    .any_value_hiding_function(getters, std::slice::from_ref(receiver))
                    .is_some()
            });
        if ambiguous {
            self.emit(AnalysisError::ambiguous_call(member, span));
        }
    }

    /// Whether a function declared earlier in the same scope as `symbol` has
    /// its name and the same parameter types, which makes `symbol` an illegal
    /// redefinition. `fn f(x: Int)` and `fn f(x: String)` are two overloads,
    /// but `fn f(x)` and `fn f(y)` both take `Any`. `None` is variadic.
    fn redefines_function(&self, symbol: SymbolId, parameters: Option<&[StaticType]>) -> bool {
        let name = self.resolver.symbol_name(symbol);
        let scope = self.resolver.scope(self.resolver.scope_of(symbol));
        scope.bindings()[..self.resolver.binding_position(symbol)]
            .iter()
            .filter(|&&earlier| self.resolver.symbol_name(earlier) == name)
            .any(|&earlier| match self.types.get_type(earlier) {
                StaticType::Function {
                    parameters: earlier,
                    ..
                } => earlier.as_deref() == parameters,
                _ => false,
            })
    }

    /// Record a non-fatal analysis error. The analyser continues with a
    /// fallback type (usually `Any`) so that subsequent code is still checked.
    fn emit(&mut self, err: AnalysisError) {
        self.errors.push(err);
    }

    /// Record an error from outside the analyser (e.g. a hard error caught by the caller).
    pub fn emit_external(&mut self, err: AnalysisError) {
        self.errors.push(err);
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

    /// Returns `true` if any errors have been recorded during the current
    /// analysis pass.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Analyses a top-level statement. Names resolve first; a statement with
    /// unresolved names isn't analysed further, which would only repeat its errors.
    /// The names it still declared are bound as `Any`, so in
    /// `fn f() { missing } let g = f`, only `missing` is reported.
    pub fn analyse(
        &mut self,
        expr_loc: &mut ExpressionLocation,
    ) -> Result<StaticType, AnalysisError> {
        let bound_before = self.resolver.current_bindings().len();
        self.resolver.resolve_statement(expr_loc);
        let name_errors = self.resolver.take_errors();
        if !name_errors.is_empty() {
            self.errors
                .extend(name_errors.into_iter().map(AnalysisError::from));
            let declared = self.resolver.current_bindings()[bound_before..].to_vec();
            for symbol in declared {
                self.declare(symbol, TypeBinding::Inferred(StaticType::Any));
            }
            return Ok(StaticType::Any);
        }
        self.analyse_expr(expr_loc)
    }

    fn analyse_expr(
        &mut self,
        expr_loc: &mut ExpressionLocation,
    ) -> Result<StaticType, AnalysisError> {
        let typ = self.analyse_inner(expr_loc)?;
        self.result
            .analysis
            .expr_types
            .insert(expr_loc.id, typ.clone());
        Ok(typ)
    }

    /// Like [`analyse`], but on error emits the error and returns `Any`
    /// so that analysis can continue.
    fn analyse_or_any(&mut self, expr_loc: &mut ExpressionLocation) -> StaticType {
        match self.analyse_expr(expr_loc) {
            Ok(t) => t,
            Err(e) => {
                self.emit(e);
                StaticType::Any
            }
        }
    }

    /// Like [`analyse_or_any`], but with an expected type from an annotation
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
        expr_loc: &mut ExpressionLocation,
        expected: &StaticType,
    ) -> StaticType {
        let typ = match (&mut expr_loc.expression, expected) {
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
                    .iter_mut()
                    .zip(elements)
                    .map(|(value, element)| self.analyse_with_expected(value, element))
                    .collect();
                StaticType::Tuple(types)
            }
            _ => return self.analyse_or_any(expr_loc),
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
        }: &mut ExpressionLocation,
    ) -> Result<StaticType, AnalysisError> {
        match expression {
            Expression::BoolLiteral(_) => Ok(StaticType::Bool),
            Expression::NoneLiteral => Ok(StaticType::Option(Box::new(StaticType::Any))),
            Expression::StringLiteral(_) => Ok(StaticType::String),
            Expression::NumericLiteral(NumericLiteral::Int64(_)) => Ok(StaticType::Int),
            Expression::NumericLiteral(NumericLiteral::Float64(_)) => Ok(StaticType::Float),
            Expression::NumericLiteral(
                NumericLiteral::NumberInt(_)
                | NumericLiteral::NumberFloat(_)
                | NumericLiteral::Complex(_),
            ) => Ok(StaticType::Number),
            Expression::Continue | Expression::Break => Ok(StaticType::Never),
            Expression::Identifier { .. } => {
                let symbol = self.resolver.innermost_reference(*id);
                let binding = Binding::Resolved(Candidate::Scalar(self.locate(symbol)));
                self.result.analysis.bindings.insert(*id, binding);
                Ok(self.types.get_type(symbol).clone())
            }
            Expression::Statement(inner) => {
                let typ = self.analyse_or_any(inner);
                // Diverging statements (return/break/continue) propagate Never
                // so that blocks can see that control doesn't fall through.
                if typ == StaticType::Never {
                    Ok(StaticType::Never)
                } else {
                    Ok(StaticType::unit())
                }
            }
            Expression::Logical { left, right, .. } => {
                self.analyse_or_any(left);
                self.analyse_or_any(right);
                Ok(StaticType::Bool)
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
                Ok(target)
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
                    None => self.analyse_or_any(value),
                };

                self.resolve_binding_pattern(
                    l_value,
                    annotated_type,
                    found_type.clone(),
                    value_span,
                );
                Ok(StaticType::unit())
            }
            Expression::Assignment { l_value, r_value } => {
                let old_type = self.resolve_assignment_target_or_any(l_value, *span);
                let new_type = self.analyse_with_expected(r_value, &old_type);
                self.validate_assignment_write(l_value, &old_type, &new_type, *span);

                Ok(StaticType::unit())
            }
            Expression::OpAssignment {
                l_value,
                r_value,
                operation,
            } => {
                let left_type = self.resolve_single_assignment_target(l_value, *span)?;
                // `p.x += 1` reads `p.x`, the call `x(p)`, first.
                if let AssignmentTarget::Member {
                    receiver,
                    member,
                    member_span,
                    ..
                } = &l_value.target
                {
                    let receiver_type = self
                        .result
                        .analysis
                        .expr_types
                        .get(&receiver.id)
                        .cloned()
                        .unwrap_or(StaticType::Any);
                    self.check_member_read(l_value.id, &receiver_type, member, *member_span);
                }
                let right_type = self.analyse_or_any(r_value);
                let arg_types = vec![left_type.clone(), right_type.clone()];

                let ResolvedCall {
                    binding: assign_binding,
                    return_type: assign_return,
                } = self.overloads().resolve_call(
                    &self.resolver.augmented_candidates(*id).in_place,
                    &arg_types,
                    CallKind::Operator,
                    |symbol| self.locate(symbol),
                );
                let ResolvedCall {
                    binding: op_binding,
                    return_type: op_return,
                } = self.overloads().resolve_call(
                    &self.resolver.augmented_candidates(*id).operator,
                    &arg_types,
                    CallKind::Operator,
                    |symbol| self.locate(symbol),
                );
                let has_op_binding = !matches!(op_binding, Binding::None);
                let assign_is_eligible = Self::augmented_rhs_is_compatible(&left_type, &right_type);

                let writeback_type = match assign_binding {
                    Binding::Resolved(candidate) if assign_is_eligible => {
                        self.result
                            .analysis
                            .augmented_assignments
                            .insert(*id, Binding::Resolved(candidate));
                        None
                    }
                    Binding::Dynamic(mut assign_candidates) if assign_is_eligible => {
                        match op_binding.clone() {
                            Binding::Resolved(candidate) => {
                                if !assign_candidates.contains(&candidate) {
                                    assign_candidates.push(candidate);
                                }
                            }
                            Binding::Dynamic(op_candidates) => {
                                for candidate in op_candidates {
                                    if !assign_candidates.contains(&candidate) {
                                        assign_candidates.push(candidate);
                                    }
                                }
                            }
                            Binding::None => {}
                        }

                        let result_type = if has_op_binding {
                            assign_return.lub(&op_return)
                        } else {
                            assign_return
                        };
                        self.result
                            .analysis
                            .augmented_assignments
                            .insert(*id, Binding::Dynamic(assign_candidates));
                        Some(result_type)
                    }
                    Binding::Resolved(_) | Binding::Dynamic(_) => {
                        // A specialized mutation exists, but using it would
                        // change the concrete left type. Reject it here rather
                        // than falling through to an ordinary operation whose
                        // erased return type could widen the same target.
                        self.emit(AnalysisError::mismatched_types(
                            &right_type,
                            &left_type,
                            *span,
                        ));
                        None
                    }
                    Binding::None if has_op_binding => {
                        self.result
                            .analysis
                            .augmented_assignments
                            .insert(*id, op_binding);
                        Some(op_return)
                    }
                    Binding::None => {
                        self.emit(AnalysisError::function_not_found(
                            operation, &arg_types, *span,
                        ));
                        None
                    }
                };

                if let Some(result_type) = writeback_type {
                    self.validate_assignment_write(l_value, &left_type, &result_type, *span);
                }

                Ok(StaticType::unit())
            }
            Expression::FunctionDeclaration {
                name,
                parameters,
                body,
                return_annotation,
                ..
            } => {
                let type_signature = TypeSignature::from_annotated_bindings(
                    parameters
                        .iter()
                        .map(|param| {
                            let annotation = self.lower_annotation(param.annotation.as_ref());
                            (param.name().to_string(), annotation)
                        })
                        .collect(),
                );
                let annotated_return = self.lower_annotation(return_annotation.as_ref());
                self.result.analysis.function_types.insert(
                    *id,
                    StaticType::Function {
                        parameters: type_signature.types(),
                        return_type: Box::new(annotated_return.clone().unwrap_or_default()),
                    },
                );

                // Pre-register the function before analyzing its body so recursive calls can
                // resolve the name. The return type is unknown at this point so we use Any.
                // A redefinition is reported but still bound, since the resolver declared it.
                let declared = if let Some(name) = name {
                    let symbol = self.resolver.declared_symbol(*id);
                    let parameters = type_signature.types();
                    if self.redefines_function(symbol, parameters.as_deref()) {
                        self.emit(AnalysisError::function_redefinition(
                            name,
                            parameters.as_deref(),
                            *span,
                        ));
                    }
                    let placeholder = StaticType::Function {
                        parameters: type_signature.types(),
                        return_type: Box::new(annotated_return.clone().unwrap_or_default()),
                    };
                    let slot = self.declare(symbol, TypeBinding::Inferred(placeholder));
                    Some((symbol, slot))
                } else {
                    None
                };

                self.functions.push(*id);
                self.return_type_stack.push(None);
                let parameter_nodes = parameters.iter().map(|p| p.lvalue.id).collect_vec();
                let param_types =
                    self.resolve_parameters_declarative(&type_signature, &parameter_nodes, *span);

                for (param, typ) in parameters.iter().zip(&param_types) {
                    self.result
                        .analysis
                        .variable_types
                        .insert(param.lvalue.id, typ.clone());
                }

                let implicit_return = self.analyse_or_any(body);
                let explicit_return = self.return_type_stack.pop().unwrap();
                self.functions.pop();
                let captures = Layout::new(&self.resolver).captures(*id);
                self.result.analysis.captures.insert(*id, captures);
                self.result
                    .analysis
                    .frame_sizes
                    .insert(*id, Layout::new(&self.resolver).frame_size(Some(*id)));

                // Combine explicit `return` types with the block's implicit return type.
                let inferred_return = match explicit_return {
                    Some(ret) => ret.lub(&implicit_return),
                    None => implicit_return,
                };

                // If there is an annotated return type, validate it;
                // otherwise record the inferred type in the analysis.
                if let Some(annotated) = &annotated_return {
                    if !inferred_return.is_subtype(annotated) {
                        self.emit(AnalysisError::mismatched_types(
                            &inferred_return,
                            annotated,
                            *span,
                        ));
                    }
                } else {
                    self.result
                        .analysis
                        .inferred_return_types
                        .insert(*id, inferred_return.clone());
                }

                let effective_return = annotated_return.unwrap_or(inferred_return);

                let function_type = StaticType::Function {
                    parameters: Some(param_types.clone()),
                    return_type: Box::new(effective_return),
                };

                if let Some((symbol, slot)) = declared {
                    let _ = self
                        .types
                        .update_binding_type(symbol, function_type.clone());
                    self.result.analysis.variables.insert(*id, slot);
                }

                Ok(function_type)
            }
            Expression::Block { statements } => {
                let mut last = None;
                for s in statements {
                    last = Some(self.analyse_or_any(s));
                }

                Ok(last.unwrap_or_else(StaticType::unit))
            }
            Expression::If {
                condition,
                on_true,
                on_false,
            } => {
                self.analyse_or_any(condition);
                let true_type = self.analyse_or_any(on_true);
                let false_type = if let Some(on_false) = on_false {
                    self.analyse_or_any(on_false)
                } else {
                    StaticType::unit()
                };

                Ok(true_type.lub(&false_type))
            }
            Expression::While {
                expression,
                loop_body,
            } => {
                self.analyse_or_any(expression);
                self.analyse_or_any(loop_body);
                Ok(StaticType::unit())
            }
            Expression::For { iterations, body } => {
                let return_type = self.resolve_for_iterations(iterations, body, *span);
                Ok(return_type)
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
            } => {
                let receiver_type = self.analyse_or_any(receiver);
                // `xs.len` is the call `len(xs)`, checked like any other call.
                self.check_member_read(*id, &receiver_type, member, *member_span);
                let getters = self
                    .resolver
                    .getter_candidates(*id)
                    .expect("internal error: member access without getter candidates");
                let ResolvedCall {
                    binding,
                    return_type,
                } = self.overloads().resolve_call(
                    getters,
                    std::slice::from_ref(&receiver_type),
                    CallKind::Regular,
                    |symbol| self.locate(symbol),
                );

                if matches!(binding, Binding::None) {
                    let error = self.no_match(getters, member, &[receiver_type], *member_span);
                    self.emit(error);
                }

                self.result.analysis.getters.insert(*id, binding);
                Ok(return_type)
            }
            Expression::Tuple { values } => {
                let mut types = Vec::with_capacity(values.len());
                for v in values {
                    types.push(self.analyse_or_any(v));
                }

                Ok(StaticType::Tuple(types))
            }
            Expression::List { values } => {
                let element_type = self.analyse_multiple_expression_with_same_type(values);

                Ok(StaticType::List(Box::new(
                    element_type.unwrap_or(StaticType::Any),
                )))
            }
            Expression::Map { values, default } => {
                let mut key_type: Option<StaticType> = None;
                let mut value_type: Option<StaticType> = None;
                for (key, value) in values {
                    Self::fold_lub(&mut key_type, self.analyse_or_any(key));
                    if let Some(value) = value {
                        Self::fold_lub(&mut value_type, self.analyse_or_any(value));
                    }
                }

                // Reads can produce the default value, so its type is part of
                // the map's value type.
                if let Some(default) = default {
                    let default_type = self.analyse_or_any(default);
                    Self::fold_lub(&mut value_type, default_type);
                }

                Ok(StaticType::Map {
                    key: Box::new(key_type.unwrap_or(StaticType::Any)),
                    value: Box::new(value_type.unwrap_or_else(StaticType::unit)),
                })
            }
            Expression::Return { value } => {
                let typ = self.analyse_or_any(value);
                if let Some(slot) = self.return_type_stack.last_mut() {
                    Self::fold_lub(slot, typ);
                }
                Ok(StaticType::Never)
            }
            Expression::RangeInclusive { start, end }
            | Expression::RangeExclusive { start, end } => {
                if let Some(start) = start {
                    self.analyse_or_any(start);
                }
                if let Some(end) = end {
                    self.analyse_or_any(end);
                }

                Ok(StaticType::Iterator(Box::new(StaticType::Int)))
            }
            Expression::StructDeclaration { name, fields } => {
                let field_types: Vec<StaticType> = fields
                    .iter()
                    .map(|f| self.lower_type_expr(&f.annotation))
                    .collect();

                let struct_id = self.struct_registry.register(
                    &*name,
                    fields
                        .iter()
                        .zip(&field_types)
                        .map(|(f, t)| (f.identifier.clone(), t.clone()))
                        .collect(),
                );

                // Bind the constructor and per-field accessors with the same
                // types the runtime functions report (StructInfo is the source
                // of truth), so static bindings and runtime dispatch agree.
                let info = Rc::clone(&self.struct_registry[struct_id]);

                let symbols = self.resolver.struct_symbols(*id).clone();
                self.struct_types
                    .insert(symbols.type_symbol, Rc::clone(&info));
                let constructor = self.declare(
                    symbols.constructor,
                    TypeBinding::Annotated(info.constructor_type()),
                );
                self.result.analysis.variables.insert(*id, constructor);

                let fields = symbols
                    .accessors
                    .iter()
                    .enumerate()
                    .map(|(index, &(getter, setter))| FieldAccessors {
                        getter: self
                            .declare(getter, TypeBinding::Annotated(info.getter_type(index))),
                        setter: self
                            .declare(setter, TypeBinding::Annotated(info.setter_type(index))),
                    })
                    .collect();
                self.result.analysis.structs.insert(
                    *id,
                    DeclaredStruct {
                        info: StructInfo::clone(&info),
                        fields,
                    },
                );

                Ok(StaticType::unit())
            }
        }
    }

    /// Resolves a call (regular or operator-form) and returns its result type.
    /// Only operator-form calls are eligible for vec dispatch.
    fn analyse_call(
        &mut self,
        function: &mut ExpressionLocation,
        arguments: &mut [ExpressionLocation],
        kind: CallKind,
        span: Span,
    ) -> Result<StaticType, AnalysisError> {
        // Higher-order call shapes like `get_function()()` have a non-identifier
        // function position; in that case we just analyse the callee as a value
        // and trust the runtime to dispatch. The callee comes before the
        // arguments, in source order like the resolver, so declarations in it
        // get their slots first.
        let Expression::Identifier { name, .. } = &mut function.expression else {
            let callee_type = self.analyse_or_any(function);
            for arg in arguments {
                self.analyse_or_any(arg);
            }
            return Ok(match callee_type {
                StaticType::Function { return_type, .. } => *return_type,
                StaticType::Any => StaticType::Any,
                other => {
                    self.emit(AnalysisError::not_callable(&other, span));
                    StaticType::Any
                }
            });
        };

        // A named callee declares nothing, so its overload can be picked after
        // the arguments' types are known.
        let mut type_sig = Vec::with_capacity(arguments.len());
        for arg in arguments.iter_mut() {
            type_sig.push(self.analyse_or_any(arg));
        }

        // A value whose type isn't known may hold a function itself, so one
        // standing nearer than a function that accepts the call makes it
        // ambiguous. `xs.len()` is the call `len(xs)`, so it's checked too.
        if self
            .overloads()
            .any_value_hiding_function(self.resolver.reference_candidates(function.id), &type_sig)
            .is_some()
        {
            self.emit(AnalysisError::ambiguous_call(name, function.span));
        }

        let ResolvedCall {
            binding,
            return_type,
        } = self.overloads().resolve_call(
            self.resolver.reference_candidates(function.id),
            &type_sig,
            kind,
            |symbol| self.locate(symbol),
        );

        if matches!(binding, Binding::None) {
            let candidates = self.resolver.reference_candidates(function.id);
            let error = self.no_match(candidates, name, &type_sig, span);
            self.emit(error);
            self.result.analysis.bindings.insert(function.id, binding);
            return Ok(StaticType::Any);
        }

        self.result.analysis.bindings.insert(function.id, binding);
        Ok(return_type)
    }

    fn resolve_for_iterations(
        &mut self,
        iterations: &mut [ForIteration],
        body: &mut ForBody,
        _span: Span,
    ) -> StaticType {
        let Some((iteration, tail)) = iterations.split_first_mut() else {
            unreachable!("because this function is never called with an empty slice");
        };

        match iteration {
            ForIteration::Iteration { l_value, sequence } => {
                let sequence_span = sequence.span;
                let sequence_type = self.analyse_or_any(sequence);

                let found_type = sequence_type
                    .sequence_element_type()
                    .unwrap_or(StaticType::Any);

                // TOOD: get this from the AST when the parser adds it
                let expected_type = None;

                self.resolve_binding_pattern(l_value, expected_type, found_type, sequence_span);
            }
            ForIteration::Guard(expr) => {
                self.analyse_or_any(expr);
            }
        }

        if !tail.is_empty() {
            self.resolve_for_iterations(tail, body, _span)
        } else {
            match body {
                ForBody::Block(block) => {
                    self.analyse_or_any(block);
                    StaticType::unit()
                }
                ForBody::List { expr } => StaticType::List(Box::new(self.analyse_or_any(expr))),
                ForBody::Map {
                    key,
                    value,
                    default,
                } => {
                    let key_type = self.analyse_or_any(key);
                    let value_type = if let Some(value) = value {
                        self.analyse_or_any(value)
                    } else {
                        StaticType::unit()
                    };

                    if let Some(default) = default {
                        self.analyse_or_any(default);
                    }

                    StaticType::Map {
                        key: Box::new(key_type),
                        value: Box::new(value_type),
                    }
                }
            }
        }
    }

    fn resolve_single_assignment_target(
        &mut self,
        lvalue: &mut AssignmentTargetLocation,
        span: Span,
    ) -> Result<StaticType, AnalysisError> {
        if matches!(lvalue.target, AssignmentTarget::Sequence(_)) {
            return Err(AnalysisError::lvalue_required_to_be_single_identifier(span));
        }
        self.resolve_assignment_target(lvalue, span)
    }

    fn resolve_assignment_target(
        &mut self,
        lvalue: &mut AssignmentTargetLocation,
        span: Span,
    ) -> Result<StaticType, AnalysisError> {
        match &mut lvalue.target {
            AssignmentTarget::Identifier { .. } => {
                let symbol = self.resolver.assigned_symbol(lvalue.id);
                let variable = self.locate(symbol);
                self.result.analysis.variables.insert(lvalue.id, variable);
                Ok(self.types.get_type(symbol).clone())
            }
            AssignmentTarget::Index { index, value } => {
                // The container before the index, in source order like the resolver.
                let type_of_index_target = self.analyse_or_any(value);
                let index_type = self.analyse_or_any(index);

                let get_args = [type_of_index_target.clone(), index_type.clone()];
                let set_args = [
                    type_of_index_target.clone(),
                    index_type.clone(),
                    StaticType::Any,
                ];

                // Indexing isn't operator-form for vec purposes: there's no
                // natural broadcast story for `(list_a, list_b)[i]`. Only an
                // augmented assignment reads the old value through `[]`.
                if let Some(getters) = self.resolver.getter_candidates(lvalue.id) {
                    let getter = self
                        .overloads()
                        .resolve_call(getters, &get_args, CallKind::Regular, |symbol| {
                            self.locate(symbol)
                        })
                        .binding;
                    self.result.analysis.getters.insert(lvalue.id, getter);
                }
                let setter = self
                    .overloads()
                    .resolve_call(
                        self.resolver.setter_candidates(lvalue.id),
                        &set_args,
                        CallKind::Regular,
                        |symbol| self.locate(symbol),
                    )
                    .binding;
                self.result.analysis.setters.insert(lvalue.id, setter);

                if Self::index_type_is_slice(&index_type)
                    && let StaticType::List(element) = &type_of_index_target
                {
                    return Ok(StaticType::List(element.clone()));
                }

                if let Some(t) = type_of_index_target.index_element_type() {
                    Ok(t)
                } else {
                    self.emit(AnalysisError::unable_to_index_into(
                        &type_of_index_target,
                        span,
                    ));
                    Ok(StaticType::Any)
                }
            }
            // What `(a, b) = …` can store is a tuple of what each target can.
            AssignmentTarget::Sequence(seq) => Ok(StaticType::Tuple(
                seq.iter_mut()
                    .map(|sub_lvalue| self.resolve_assignment_target_or_any(sub_lvalue, span))
                    .collect(),
            )),
            AssignmentTarget::Member {
                receiver,
                member,
                member_span,
            } => {
                let receiver_type = self.analyse_or_any(receiver);
                let getter_args = [receiver_type.clone()];
                let setter_args = [receiver_type, StaticType::Any];
                let getters = self
                    .resolver
                    .getter_candidates(lvalue.id)
                    .expect("internal error: member target without getter candidates");
                let getter = self.overloads().resolve_call(
                    getters,
                    &getter_args,
                    CallKind::Regular,
                    |symbol| self.locate(symbol),
                );
                let setter_name = format!("{member}=");
                let setter = self.overloads().resolve_call(
                    self.resolver.setter_candidates(lvalue.id),
                    &setter_args,
                    CallKind::Regular,
                    |symbol| self.locate(symbol),
                );
                let getter_missing = matches!(getter.binding, Binding::None);
                let setter_missing = matches!(setter.binding, Binding::None);

                self.result
                    .analysis
                    .getters
                    .insert(lvalue.id, getter.binding);
                self.result
                    .analysis
                    .setters
                    .insert(lvalue.id, setter.binding);

                if getter_missing {
                    return Err(AnalysisError::function_not_found(
                        member,
                        &getter_args,
                        *member_span,
                    ));
                }
                if setter_missing {
                    return Err(AnalysisError::function_not_found(
                        &setter_name,
                        &setter_args,
                        *member_span,
                    ));
                }

                Ok(getter.return_type)
            }
        }
    }

    fn resolve_assignment_target_or_any(
        &mut self,
        lvalue: &mut AssignmentTargetLocation,
        span: Span,
    ) -> StaticType {
        match self.resolve_assignment_target(lvalue, span) {
            Ok(t) => t,
            Err(e) => {
                self.emit(e);
                StaticType::Any
            }
        }
    }

    /// An index expression typed as an integer sequence (e.g. `0..2`) selects
    /// a slice of the container rather than a single element.
    fn index_type_is_slice(index_type: &StaticType) -> bool {
        index_type.is_subtype(&StaticType::Iterator(Box::new(StaticType::Int)))
    }

    /// Specialized `op=` implementations mutate the left operand in place and
    /// keep its concrete type, so the right operand must provably fit it: the
    /// operator copies values across without inspecting them, and nothing
    /// downstream re-checks. A merely overlapping right operand is not enough —
    /// `List<Int> ++= List<Number>` would leave the target holding a `Float`.
    ///
    /// A tuple left-hand side represents vector dispatch, so a scalar right
    /// operand must be compatible with every concrete tuple element.
    fn augmented_rhs_is_compatible(left_type: &StaticType, right_type: &StaticType) -> bool {
        match (left_type, right_type) {
            (StaticType::Tuple(left), StaticType::Tuple(right)) => {
                left.len() == right.len()
                    && right
                        .iter()
                        .zip(left)
                        .all(|(right, left)| right.is_subtype(left))
            }
            (StaticType::Tuple(left), right) => left.iter().all(|left| right.is_subtype(left)),
            (left, right) => right.is_subtype(left),
        }
    }

    /// Validate a value that will be stored through an lvalue, widening an
    /// inferred binding when the location has a stable variable to update.
    fn validate_assignment_write(
        &mut self,
        lvalue: &AssignmentTargetLocation,
        stored_type: &StaticType,
        value_type: &StaticType,
        span: Span,
    ) {
        match &lvalue.target {
            AssignmentTarget::Identifier { .. }
                if self.result.analysis.variables.contains_key(&lvalue.id) =>
            {
                let target = self.resolver.assigned_symbol(lvalue.id);
                let widened = stored_type.lub(value_type);
                if widened != *stored_type
                    && let Err(annotated_type) = self.types.update_binding_type(target, widened)
                    && !value_type.is_subtype(&annotated_type)
                {
                    self.emit(AnalysisError::mismatched_types(
                        value_type,
                        &annotated_type,
                        span,
                    ));
                }
            }
            AssignmentTarget::Member { .. } => {
                if !value_type.is_subtype(stored_type) {
                    self.emit(AnalysisError::mismatched_types(
                        value_type,
                        stored_type,
                        span,
                    ));
                }
            }
            AssignmentTarget::Index { value, index, .. } => {
                if value_type.is_subtype(stored_type) {
                    return;
                }

                let is_slice = self
                    .result
                    .analysis
                    .expr_types
                    .get(&index.id)
                    .is_some_and(Self::index_type_is_slice);
                let (stored_element_type, value_element_type) = if is_slice {
                    let Some(stored_element_type) = stored_type.index_element_type() else {
                        self.emit(AnalysisError::mismatched_types(
                            value_type,
                            stored_type,
                            span,
                        ));
                        return;
                    };
                    let Some(value_element_type) = value_type.sequence_element_type() else {
                        self.emit(AnalysisError::mismatched_types(
                            value_type,
                            stored_type,
                            span,
                        ));
                        return;
                    };
                    (stored_element_type, value_element_type)
                } else {
                    (stored_type.clone(), value_type.clone())
                };

                if matches!(
                    self.result.analysis.binding(value.id),
                    Binding::Resolved(Candidate::Scalar(_))
                ) {
                    let target = self.resolver.innermost_reference(value.id);
                    let container_type = self.types.get_type(target).clone();
                    // An earlier target of the same destructuring may already
                    // have widened the container, as in `l[0], l[1] = …`, so
                    // check against its current type rather than `stored_type`.
                    if container_type
                        .index_element_type()
                        .is_some_and(|element| value_element_type.is_subtype(&element))
                    {
                        return;
                    }
                    let widened_container = container_type.lub(
                        &container_type
                            .with_element_type(stored_element_type.lub(&value_element_type)),
                    );
                    if widened_container != container_type
                        && self
                            .types
                            .update_binding_type(target, widened_container)
                            .is_ok()
                    {
                        return;
                    }
                }

                self.emit(AnalysisError::mismatched_types(
                    value_type,
                    stored_type,
                    span,
                ));
            }
            // Each target gets the element unpacked for it, as in a `let`.
            AssignmentTarget::Sequence(targets) => {
                let StaticType::Tuple(stored_types) = stored_type else {
                    unreachable!("internal error: a sequence target stores a tuple")
                };
                if let StaticType::Tuple(value_types) = value_type
                    && value_types.len() != targets.len()
                {
                    self.emit(AnalysisError::tuple_arity_mismatch(
                        targets.len(),
                        value_types.len(),
                        span,
                    ));
                    return;
                }
                let Some(value_types) = value_type.unpack() else {
                    self.emit(AnalysisError::unable_to_unpack_type(value_type, span));
                    return;
                };
                for (target, stored_type, value_type) in izip!(targets, stored_types, value_types) {
                    self.validate_assignment_write(target, stored_type, value_type, span);
                }
            }
            AssignmentTarget::Identifier { .. } => {}
        }
    }

    /// Resolve expressions as arguments to a function and return the function arity
    fn resolve_parameters_declarative(
        &mut self,
        type_signature: &TypeSignature,
        parameter_nodes: &[NodeId],
        span: Span,
    ) -> Vec<StaticType> {
        let TypeSignature::Exact(parameters) = type_signature else {
            return vec![];
        };

        for param in parameters.iter().duplicates_by(|param| &param.name) {
            self.emit(AnalysisError::parameter_redefined(&param.name, span));
        }

        for (param, &node) in parameters
            .iter()
            .zip(parameter_nodes)
            .unique_by(|(param, _)| &param.name)
        {
            let has_annotation = param.type_name != StaticType::Any;
            let binding = if has_annotation {
                TypeBinding::Annotated(param.type_name.clone())
            } else {
                TypeBinding::Inferred(StaticType::Any)
            };

            let symbol = self.resolver.declared_symbol(node);
            self.declare(symbol, binding);
        }

        parameters
            .iter()
            .map(|param| param.type_name.clone())
            .collect()
    }
    fn resolve_binding_pattern(
        &mut self,
        lvalue: &mut BindingPatternLocation,
        expected_type: Option<StaticType>,
        found_type: StaticType,
        span: Span,
    ) {
        match &mut lvalue.pattern {
            BindingPattern::Identifier { span, .. } => {
                // If there is a type annotation and the given type is not a subtype of the annotated type we emit an error
                if let Some(expected_type) = &expected_type
                    && !found_type.is_subtype(expected_type)
                {
                    self.emit(AnalysisError::mismatched_types(
                        &found_type,
                        expected_type,
                        *span,
                    ));
                }

                let type_binding = match expected_type {
                    Some(annotated) => TypeBinding::Annotated(annotated),
                    None => TypeBinding::Inferred(found_type),
                };

                let symbol = self.resolver.declared_symbol(lvalue.id);
                let variable = self.declare(symbol, type_binding.clone());
                self.result.analysis.variables.insert(lvalue.id, variable);
                self.result
                    .analysis
                    .variable_types
                    .insert(lvalue.id, type_binding.typ().clone());
            }
            BindingPattern::Sequence(seq) => {
                // If the type is a fixed-length Tuple whose arity doesn't match
                // the number of bindings, fall back to Any for each element. This
                // can happen when a variable is declared with one type (e.g. ())
                // and later reassigned to a tuple of a different arity — the
                // analyser doesn't track reassignment types.
                let is_annotated = expected_type.is_some();
                let resolved_type = expected_type.unwrap_or(found_type.clone());

                let sub_types: Box<dyn Iterator<Item = &StaticType>> =
                    if let StaticType::Tuple(elems) = &resolved_type {
                        if elems.len() != seq.len() {
                            self.emit(AnalysisError::tuple_arity_mismatch(
                                seq.len(),
                                elems.len(),
                                span,
                            ));
                            self.bind_patterns_as_any(seq, span);
                            return;
                        } else {
                            Box::new(elems.iter())
                        }
                    } else if let Some(iter) = resolved_type.unpack() {
                        iter
                    } else {
                        self.emit(AnalysisError::unable_to_unpack_type(&resolved_type, span));
                        self.bind_patterns_as_any(seq, span);
                        return;
                    };

                let found_types = found_type
                    .unpack()
                    .unwrap_or_else(|| Box::new(std::iter::repeat(&StaticType::Any)));

                let desired_length = seq.len();
                let mut actual_len = 0;

                for (sub_lvalue, sub_type, found_type) in
                    izip!(seq.iter_mut(), sub_types, found_types)
                {
                    let sub_expected = if is_annotated {
                        Some(sub_type.clone())
                    } else {
                        None
                    };
                    self.resolve_binding_pattern(
                        sub_lvalue,
                        sub_expected,
                        found_type.clone(),
                        span,
                    );

                    actual_len += 1;
                }

                if desired_length != actual_len {
                    self.emit(AnalysisError::unable_to_unpack_type(&found_type, span));
                    self.bind_patterns_as_any(&mut seq[actual_len..], span);
                }
            }
        }
    }

    /// Binds every name in `patterns` as `Any`, so a pattern that failed to
    /// type-check still declares its names. In `let (a, b, c) = (1, 2)`, `c`
    /// is bound and a later `print(c)` reports nothing further.
    fn bind_patterns_as_any(&mut self, patterns: &mut [BindingPatternLocation], span: Span) {
        for pattern in patterns {
            self.resolve_binding_pattern(pattern, None, StaticType::Any, span);
        }
    }

    fn analyse_multiple_expression_with_same_type(
        &mut self,
        expressions: &mut Vec<ExpressionLocation>,
    ) -> Option<StaticType> {
        let mut element_type: Option<StaticType> = None;
        for expression in expressions {
            Self::fold_lub(&mut element_type, self.analyse_or_any(expression));
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

#[derive(thiserror::Error, Debug)]
#[error("{text}")]
pub struct AnalysisError {
    text: String,
    span: Span,
    help_text: Option<String>,
}

impl From<NameResolutionError> for AnalysisError {
    fn from(error: NameResolutionError) -> Self {
        Self {
            text: error.to_string(),
            span: error.span(),
            help_text: None,
        }
    }
}

impl AnalysisError {
    pub fn span(&self) -> Span {
        self.span
    }

    /// Advice on how to fix the error, rendered as a separate note under the
    /// snippet rather than crammed into the message.
    pub fn help_text(&self) -> Option<&str> {
        self.help_text.as_deref()
    }

    fn invalid_type_annotation(err: &StaticTypeConstructionError, span: Span) -> Self {
        Self {
            text: err.to_string(),
            span,
            help_text: Some(err.help_text().to_string()),
        }
    }

    fn type_does_not_take_generic_args(name: &str, span: Span) -> Self {
        Self {
            text: format!("type `{name}` does not take generic arguments"),
            span,
            help_text: None,
        }
    }

    fn tuple_arity_mismatch(ident_len: usize, annotation_len: usize, span: Span) -> Self {
        Self {
            text: format!(
                "mismatched tuple arity: found a len={ident_len} identifier and a len={annotation_len} annotation."
            ),
            span,
            help_text: None,
        }
    }

    fn mismatched_types(found: &StaticType, expected: &StaticType, span: Span) -> Self {
        Self {
            text: format!("mismatched types: found {found} but expected {expected}"),
            span,
            help_text: None,
        }
    }

    fn impossible_cast(found: &StaticType, target: &StaticType, span: Span) -> Self {
        Self {
            text: format!("invalid cast: {found} can never be {target}"),
            span,
            help_text: None,
        }
    }

    fn function_redefinition(name: &str, parameters: Option<&[StaticType]>, span: Span) -> Self {
        let parameters_desc = match parameters {
            Some(types) => format!("({})", types.iter().join(", ")),
            None => "variadic parameters".to_string(),
        };
        Self {
            text: format!(
                "Illegal redefinition of function '{name}' with parameters {parameters_desc} in the same scope"
            ),
            span,
            help_text: Some(
                "Overloads in one scope need different parameter types, so annotate a parameter's type to tell them apart".to_string(),
            ),
        }
    }

    fn ambiguous_call(name: &str, span: Span) -> Self {
        Self {
            text: format!(
                "Ambiguous call to '{name}': it could mean the function '{name}' or a value named '{name}' whose type isn't known"
            ),
            span,
            help_text: Some(
                "Rename the value if it holds a function, or annotate its type if it doesn't"
                    .to_string(),
            ),
        }
    }

    fn parameter_redefined(param: &str, span: Span) -> Self {
        Self {
            text: format!("Illegal redefinition of parameter {param}"),
            span,
            help_text: None,
        }
    }
    fn unable_to_index_into(typ: &StaticType, span: Span) -> Self {
        Self {
            text: format!("Unable to index into {typ}"),
            span,
            help_text: None,
        }
    }
    fn unable_to_unpack_type(typ: &StaticType, span: Span) -> Self {
        Self {
            text: format!("Invalid unpacking of {typ}"),
            span,
            help_text: None,
        }
    }
    fn lvalue_required_to_be_single_identifier(span: Span) -> Self {
        Self {
            text: "This lvalue is required to be a single identifier".to_string(),
            span,
            help_text: None,
        }
    }

    fn variable_rejects_arguments(name: &str, types: &[StaticType], span: Span) -> Self {
        Self {
            text: format!(
                "The variable '{name}' holds a function that doesn't accept the arguments '{}'",
                types.iter().join(", ")
            ),
            span,
            help_text: Some(format!(
                "A variable hides the functions named '{name}' further out: it doesn't join their overloads"
            )),
        }
    }

    fn function_not_found(ident: &str, types: &[StaticType], span: Span) -> Self {
        Self {
            text: format!(
                "No function called '{ident}' found that matches the arguments '{}'",
                types.iter().join(", ")
            ),
            span,
            help_text: None,
        }
    }

    /// Same as [`Self::function_not_found`], for a call an overload would
    /// accept if the arguments were known more precisely.
    fn function_not_found_suggest_cast(ident: &str, types: &[StaticType], span: Span) -> Self {
        Self {
            help_text: Some(
                concat!(
                    "An overload would accept a narrower argument type. ",
                    "Cast to say what the value holds, as in `value as List<Int>`.",
                )
                .to_string(),
            ),
            ..Self::function_not_found(ident, types, span)
        }
    }

    fn not_callable(typ: &StaticType, span: Span) -> Self {
        Self {
            text: format!("Unable to invoke {typ} as a function."),
            span,
            help_text: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ndc_lexer::{Lexer, SourceId};
    use ndc_parser::Parser;

    fn analyse_with_globals(
        source: &str,
        globals: Vec<(String, StaticType)>,
    ) -> (StaticType, AnalysisResult) {
        let tokens = Lexer::new(source, SourceId::SYNTHETIC)
            .collect::<Result<Vec<_>, _>>()
            .expect("lex failed");
        let mut expressions = Parser::from_tokens(tokens).parse().expect("parse failed");
        let mut analyser = Analyser::new(globals);
        let mut last_type = StaticType::unit();
        for expression in &mut expressions {
            last_type = analyser.analyse(expression).expect("analysis failed");
        }
        (last_type, analyser.take_result())
    }

    fn analyse_last_type_with_globals(
        source: &str,
        globals: Vec<(String, StaticType)>,
    ) -> StaticType {
        let (last_type, result) = analyse_with_globals(source, globals);
        assert!(
            result.errors.is_empty(),
            "analysis errors: {:?}",
            result.errors
        );
        last_type
    }

    fn analyse_last_type(source: &str) -> StaticType {
        analyse_last_type_with_globals(source, vec![])
    }

    /// `[]` and `[]=`, which index reads and writes go through. Name resolution
    /// rejects index access without them, as it would any other unknown function.
    fn index_natives() -> Vec<(String, StaticType)> {
        let any_function = StaticType::Function {
            parameters: None,
            return_type: Box::new(StaticType::Any),
        };
        vec![
            ("[]".to_string(), any_function.clone()),
            ("[]=".to_string(), any_function),
        ]
    }

    fn assert_analysis_error(source: &str, globals: Vec<(String, StaticType)>, expected: &str) {
        let (_, result) = analyse_with_globals(source, globals);
        assert!(
            result
                .errors
                .iter()
                .any(|error| error.to_string().contains(expected)),
            "expected an error containing {expected:?}, got {:?}",
            result.errors,
        );
    }

    #[test]
    fn destructuring_assignment_checks_each_target() {
        assert_analysis_error(
            "let a: Int = 1; let b = 2; (a, b) = (\"s\", 2);",
            vec![],
            "mismatched types",
        );
        assert_analysis_error("let a = 1; let b = 2; (a, b) = (1, 2, 3);", vec![], "tuple");
    }

    #[test]
    fn index_write_into_a_string_must_be_a_string() {
        for source in [
            "let s: String = \"abc\"; s[0] = 42;",
            "let s = \"abc\"; s[0] = 42;",
        ] {
            assert_analysis_error(source, index_natives(), "mismatched types");
        }
    }

    #[test]
    fn destructuring_assignment_widens_each_target() {
        let typ = analyse_last_type("let a = 1; let b = 2; (a, b) = (\"s\", 2); a");
        assert_eq!(typ, StaticType::Int.lub(&StaticType::String));
    }

    #[test]
    fn inferred_list_index_assignment_widens_element_type() {
        assert_eq!(
            analyse_last_type_with_globals(
                "let values = [1]; values[0] = \"two\"; values",
                index_natives()
            ),
            StaticType::List(Box::new(StaticType::Any)),
        );
    }

    #[test]
    fn inferred_list_slice_assignment_widens_element_type() {
        assert_eq!(
            analyse_last_type_with_globals(
                "let values = [1]; values[0..1] = [\"two\"]; values",
                index_natives()
            ),
            StaticType::List(Box::new(StaticType::Any)),
        );
    }

    #[test]
    fn inferred_map_index_assignment_widens_value_and_preserves_key_type() {
        assert_eq!(
            analyse_last_type_with_globals(
                "let values = %{\"one\": 1}; values[\"two\"] = \"two\"; values",
                index_natives()
            ),
            StaticType::Map {
                key: Box::new(StaticType::String),
                value: Box::new(StaticType::Any),
            },
        );
    }

    #[test]
    fn inferred_index_augmented_assignment_widens_element_type() {
        let add = StaticType::Function {
            parameters: Some(vec![StaticType::Int, StaticType::Float]),
            return_type: Box::new(StaticType::Number),
        };
        assert_eq!(
            analyse_last_type_with_globals(
                "let values = [1]; values[0] += 0.5; values",
                [index_natives(), vec![("+".to_string(), add)]].concat(),
            ),
            StaticType::List(Box::new(StaticType::Any)),
        );
    }

    #[test]
    fn inferred_identifier_assignments_widen_subsequent_reads() {
        assert_eq!(analyse_last_type("let x = 3; x = 0.5; x"), StaticType::Any,);

        let add = StaticType::Function {
            parameters: Some(vec![StaticType::Int, StaticType::Float]),
            return_type: Box::new(StaticType::Float),
        };
        assert_eq!(
            analyse_last_type_with_globals("let x = 3; x += 0.5; x", vec![("+".to_string(), add)],),
            StaticType::Any,
        );
    }

    #[test]
    fn annotated_identifier_augmented_assignment_rejects_widening() {
        let add = StaticType::Function {
            parameters: Some(vec![StaticType::Int, StaticType::Float]),
            return_type: Box::new(StaticType::Float),
        };
        assert_analysis_error(
            "let x: Int = 3; x += 0.5;",
            vec![("+".to_string(), add)],
            "mismatched types: found Float but expected Int",
        );
    }

    #[test]
    fn compatible_specialized_assignment_preserves_left_type() {
        let list_any = StaticType::List(Box::new(StaticType::Any));
        let append = StaticType::Function {
            parameters: Some(vec![list_any.clone(), list_any.clone()]),
            return_type: Box::new(list_any),
        };

        assert_eq!(
            analyse_last_type_with_globals(
                "let values = [1]; values ++= [2]; values",
                vec![("++=".to_string(), append)],
            ),
            StaticType::List(Box::new(StaticType::Int)),
        );
    }

    #[test]
    fn incompatible_specialized_assignment_is_rejected() {
        let list_any = StaticType::List(Box::new(StaticType::Any));
        let concat = StaticType::Function {
            parameters: Some(vec![list_any.clone(), list_any.clone()]),
            return_type: Box::new(list_any),
        };

        assert_analysis_error(
            "let values = [1]; values ++= [\"two\"];",
            vec![
                ("++=".to_string(), concat.clone()),
                ("++".to_string(), concat.clone()),
            ],
            "mismatched types: found List<String> but expected List<Int>",
        );
        assert_analysis_error(
            "let values: List<Int> = [1]; values ++= [\"two\"];",
            vec![
                ("++=".to_string(), concat.clone()),
                ("++".to_string(), concat),
            ],
            "mismatched types: found List<String> but expected List<Int>",
        );
    }

    #[test]
    fn annotated_declaration_types_empty_container_literals() {
        assert_eq!(
            analyse_last_type("let x: Map<Int, Int> = %{}; x"),
            StaticType::Map {
                key: Box::new(StaticType::Int),
                value: Box::new(StaticType::Int),
            },
        );
        assert_eq!(
            analyse_last_type("let x: List<Int> = []; x"),
            StaticType::List(Box::new(StaticType::Int)),
        );
        assert_eq!(
            analyse_last_type("let x: List<List<Int>> = [[]]; x"),
            StaticType::List(Box::new(StaticType::List(Box::new(StaticType::Int)))),
        );
    }

    #[test]
    fn annotated_declaration_still_rejects_mismatched_literals() {
        assert_analysis_error(
            "let x: Map<Int, Int> = %{\"a\": 1};",
            vec![],
            "mismatched types: found Map<String, Int> but expected Map<Int, Int>",
        );
    }

    #[test]
    fn constructor_arity_mismatch_is_rejected() {
        assert_analysis_error(
            "struct Point { x: Int, y: Int }\nPoint(1)",
            vec![],
            "No function called 'Point' found that matches the arguments 'Int'",
        );
    }

    #[test]
    fn constructor_argument_type_mismatch_is_rejected() {
        assert_analysis_error(
            "struct Point { x: Int, y: Int }\nPoint(\"x\", 2)",
            vec![],
            "No function called 'Point' found that matches the arguments 'String, Int'",
        );
    }

    #[test]
    fn any_typed_callee_still_dispatches_dynamically() {
        let (_, result) =
            analyse_with_globals("f(\"x\")", vec![("f".to_string(), StaticType::Any)]);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn map_default_value_contributes_to_value_type() {
        assert_eq!(
            analyse_last_type("let m = %{:0}; m"),
            StaticType::Map {
                key: Box::new(StaticType::Any),
                value: Box::new(StaticType::Int),
            },
        );
    }

    #[test]
    fn duplicate_struct_field_is_rejected() {
        assert_analysis_error(
            "struct Dup { a: Int, a: String, a: Bool }",
            vec![],
            "Illegal redefinition of field 'a' in struct 'Dup'",
        );
    }

    #[test]
    fn struct_with_duplicate_fields_does_not_claim_its_name() {
        let (_, result) = analyse_with_globals(
            "struct Dup { a: Int, a: String }\nstruct Dup { a: Int }",
            vec![],
        );
        assert_eq!(
            result.errors.iter().map(ToString::to_string).collect_vec(),
            vec!["Illegal redefinition of field 'a' in struct 'Dup'"],
        );
    }

    /// `source` reports exactly one error, containing `expected`, and its last
    /// expression is `Any`. For code after a failed declaration: the names it
    /// declared are bound as `Any`, so using them adds no second error.
    fn assert_one_error_then_any(source: &str, expected: &str) {
        let (last_type, result) = analyse_with_globals(source, vec![]);
        assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);
        assert!(
            result.errors[0].to_string().contains(expected),
            "expected an error containing {expected:?}, got {:?}",
            result.errors,
        );
        assert_eq!(last_type, StaticType::Any);
    }

    #[test]
    fn short_tuple_still_binds_every_pattern_name() {
        assert_one_error_then_any(
            "let (a, b, c): (Int, Int, Int) = (1, 2); c",
            "Invalid unpacking",
        );
    }

    #[test]
    fn tuple_arity_mismatch_still_binds_every_pattern_name() {
        assert_one_error_then_any(
            "let (a, b, c): (Int, Int) = (1, 2); c",
            "mismatched tuple arity",
        );
    }

    #[test]
    fn unpacking_a_scalar_still_binds_every_pattern_name() {
        assert_one_error_then_any("let (a, (b, c)) = (1, 2); c", "Invalid unpacking");
    }

    #[test]
    fn function_with_an_unknown_name_still_binds_its_name() {
        assert_one_error_then_any(
            "fn f() { missing; } let g = f; g",
            "Identifier missing has not previously been declared",
        );
    }

    #[test]
    fn function_with_an_unknown_name_can_still_be_called() {
        assert_one_error_then_any(
            "fn f() { missing; } f()",
            "Identifier missing has not previously been declared",
        );
    }

    #[test]
    fn function_with_an_unknown_name_can_still_be_captured() {
        assert_one_error_then_any(
            "fn f() { missing; } fn g() { f } g()",
            "Identifier missing has not previously been declared",
        );
    }

    /// `source` reports exactly one error, the redefinition of `f`.
    fn assert_only_redefinition_error(source: &str) {
        let (_, result) = analyse_with_globals(source, vec![]);
        assert_eq!(
            result.errors.iter().map(ToString::to_string).collect_vec(),
            vec!["Illegal redefinition of function 'f' with parameters (Any) in the same scope"],
        );
    }

    #[test]
    fn redefined_function_can_still_be_used_as_a_value() {
        assert_only_redefinition_error("fn f(x) { x } fn f(y) { y } let g = f;");
    }

    #[test]
    fn redefined_function_can_still_be_called() {
        assert_only_redefinition_error("fn f(x) { x } fn f(y) { y } f(1)");
    }

    // An annotation names the struct its scope sees: the inner `T` inside
    // `inner`, the outer `T` after it. Mixing them up would make `t.b` or
    // `u.a` an unknown field.
    #[test]
    fn annotations_name_the_struct_in_scope() {
        assert_eq!(
            analyse_last_type(
                "struct T { a: Int }
                 fn inner() { struct T { b: String } let t: T = T(\"x\"); t.b }
                 let u: T = T(1);
                 u.a;
                 inner()"
            ),
            StaticType::String,
        );
    }

    // Each native is typed by its own registry entry: mixing them up would
    // give `second()` the type of `first()`.
    #[test]
    fn natives_are_typed_by_their_registry_entry() {
        let native = |name: &str, return_type| {
            (
                name.to_string(),
                StaticType::Function {
                    parameters: Some(vec![]),
                    return_type: Box::new(return_type),
                },
            )
        };
        assert_eq!(
            analyse_last_type_with_globals(
                "second()",
                vec![
                    native("first", StaticType::Int),
                    native("second", StaticType::String)
                ],
            ),
            StaticType::String,
        );
    }

    #[test]
    fn same_arity_overloads_with_different_parameter_types_are_allowed() {
        assert_eq!(
            analyse_last_type(
                "fn f(x: Int) { 1 } fn f(x: String) { \"one\" } fn f(x: List<Int>) { [1] } f(\"a\")"
            ),
            StaticType::String,
        );
    }

    #[test]
    fn same_parameter_types_under_other_names_are_a_redefinition() {
        let (_, result) = analyse_with_globals("fn f(a: Int) { a } fn f(b: Int) { b }", vec![]);
        assert_eq!(
            result.errors.iter().map(ToString::to_string).collect_vec(),
            vec!["Illegal redefinition of function 'f' with parameters (Int) in the same scope"],
        );
    }

    /// `len`, a native taking a `String`, for the tests below.
    fn len_native() -> Vec<(String, StaticType)> {
        vec![(
            "len".to_string(),
            StaticType::Function {
                parameters: Some(vec![StaticType::String]),
                return_type: Box::new(StaticType::Int),
            },
        )]
    }

    fn errors(source: &str, globals: Vec<(String, StaticType)>) -> Vec<String> {
        let (_, result) = analyse_with_globals(source, globals);
        result.errors.iter().map(ToString::to_string).collect()
    }

    #[test]
    fn unknown_value_nearer_than_an_accepting_function_is_ambiguous() {
        assert_eq!(
            errors("fn foo(x) { 1 } fn test(foo) { foo(2) }", vec![]),
            vec![
                "Ambiguous call to 'foo': it could mean the function 'foo' or a value named 'foo' whose type isn't known"
            ],
        );
    }

    #[test]
    fn natives_compete_with_unknown_values_too() {
        assert_eq!(
            errors("fn test(len) { len(\"abc\") }", len_native()),
            vec![
                "Ambiguous call to 'len': it could mean the function 'len' or a value named 'len' whose type isn't known"
            ],
        );
    }

    #[test]
    fn unknown_value_is_called_when_no_function_accepts_the_call() {
        assert_eq!(
            errors("fn foo(a, b) { 1 } fn test(foo) { foo(2) }", vec![]),
            Vec::<String>::new()
        );
    }

    #[test]
    fn method_calls_are_checked_like_plain_calls() {
        assert_eq!(
            errors("fn test(len) { \"abc\".len() }", len_native()),
            vec![
                "Ambiguous call to 'len': it could mean the function 'len' or a value named 'len' whose type isn't known"
            ],
        );
    }

    #[test]
    fn annotating_a_value_that_cant_be_called_leaves_the_name_to_the_function() {
        assert_eq!(
            errors("fn test(len: Int) { len(\"abc\") }", len_native()),
            Vec::<String>::new()
        );
    }

    #[test]
    fn a_nearer_function_shadows_an_unknown_value() {
        assert_eq!(
            errors("fn test(foo) { fn foo(x) { 1 } foo(2) }", vec![]),
            Vec::<String>::new()
        );
    }

    /// `+`, which augmented assignment needs, for the tests below.
    fn plus_native() -> Vec<(String, StaticType)> {
        vec![(
            "+".to_string(),
            StaticType::Function {
                parameters: Some(vec![StaticType::Int, StaticType::Int]),
                return_type: Box::new(StaticType::Int),
            },
        )]
    }

    #[test]
    fn property_reads_are_checked_like_calls() {
        assert_eq!(
            errors("fn test(len) { \"abc\".len }", len_native()),
            vec![
                "Ambiguous call to 'len': it could mean the function 'len' or a value named 'len' whose type isn't known"
            ],
        );
    }

    #[test]
    fn a_field_read_is_ambiguous_with_an_unknown_value_named_like_the_field() {
        assert_eq!(
            errors("struct P { x: Int } fn f(p, x) { p.x }", vec![]),
            vec![
                "Ambiguous call to 'x': it could mean the function 'x' or a value named 'x' whose type isn't known"
            ],
        );
    }

    #[test]
    fn augmented_assignment_to_a_field_reads_it_first() {
        assert_eq!(
            errors("struct P { x: Int } fn f(p, x) { p.x += 1 }", plus_native()),
            vec![
                "Ambiguous call to 'x': it could mean the function 'x' or a value named 'x' whose type isn't known"
            ],
        );
    }

    #[test]
    fn plain_assignment_to_a_field_reads_nothing() {
        assert_eq!(
            errors("struct P { x: Int } fn f(p, x) { p.x = 1 }", vec![]),
            Vec::<String>::new()
        );
    }
}
