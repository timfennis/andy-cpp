use super::Analyser;
use crate::error::AnalysisError;
use crate::overloads::{CallKind, ResolvedCall};
use itertools::izip;
use ndc_analysis::Binding;
use ndc_core::{NodeId, StaticType};
use ndc_lexer::Span;
use ndc_parser::{AssignmentTarget, AssignmentTargetLocation, ExpressionLocation};

impl Analyser {
    /// Analyses `l_value op= r_value`: picks the in-place overload (`+=`) if one
    /// fits and keeps the target's type, or else the plain operator (`+`) whose
    /// result is written back.
    pub(super) fn analyse_op_assignment(
        &mut self,
        id: NodeId,
        l_value: &AssignmentTargetLocation,
        r_value: &ExpressionLocation,
        operation: &str,
        span: Span,
    ) -> StaticType {
        let left_type = self.analyse_assignment_target(l_value, span);
        let right_type = self.analyse_expr(r_value);
        // An in-place operation such as `++=` keeps its target's type, so
        // what it stores has to fit that type. Where the target's type isn't
        // fully known, the right operand settles it: in
        // `let xs = []; xs ++= [1]`, the elements of `xs` are `Int`.
        let left_type = if left_type.has_vars() {
            match &left_type {
                // A tuple target with a scalar operand applies the operand to
                // each element: `t ++= [1]` appends to every list in `t`.
                StaticType::Tuple(targets) if !matches!(right_type, StaticType::Tuple(_)) => {
                    for target in targets {
                        self.vars.unify(target, &right_type);
                    }
                }
                _ => {
                    self.vars.unify(&left_type, &right_type);
                }
            }
            self.vars.resolve(&left_type)
        } else {
            left_type
        };
        let arg_types = vec![left_type.clone(), right_type.clone()];

        let ResolvedCall {
            binding: assign_binding,
            return_type: assign_return,
        } = self.overloads().resolve_call(
            &self.resolver.augmented_candidates(id).in_place,
            &arg_types,
            CallKind::Operator,
        );
        let ResolvedCall {
            binding: op_binding,
            return_type: op_return,
        } = self.overloads().resolve_call(
            &self.resolver.augmented_candidates(id).operator,
            &arg_types,
            CallKind::Operator,
        );
        let has_op_binding = !matches!(op_binding, Binding::None);
        let assign_is_eligible = Self::augmented_rhs_is_compatible(&left_type, &right_type);

        let writeback_type = match assign_binding {
            Binding::Resolved(candidate) if assign_is_eligible => {
                self.picks
                    .augmented_assignments
                    .insert(id, Binding::Resolved(candidate));
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
                self.picks
                    .augmented_assignments
                    .insert(id, Binding::Dynamic(assign_candidates));
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
                    span,
                ));
                None
            }
            Binding::None if has_op_binding => {
                self.picks.augmented_assignments.insert(id, op_binding);
                Some(op_return)
            }
            Binding::None => {
                self.emit(AnalysisError::function_not_found(
                    operation, &arg_types, span,
                ));
                None
            }
        };

        if let Some(result_type) = writeback_type {
            self.validate_assignment_write(l_value, &left_type, &result_type, span);
        }

        StaticType::unit()
    }

    /// Records where an assignment stores its value, and returns the type of
    /// what is stored there.
    pub(super) fn analyse_assignment_target(
        &mut self,
        lvalue: &AssignmentTargetLocation,
        span: Span,
    ) -> StaticType {
        match &lvalue.target {
            AssignmentTarget::Identifier { .. } => {
                let symbol = self.resolver.assigned_symbol(lvalue.id);
                self.picks.variables.insert(lvalue.id, symbol);
                self.vars.resolve(self.types.get_type(symbol))
            }
            AssignmentTarget::Index { index, value } => {
                // The container before the index, in source order like the resolver.
                let container = self.analyse_expr(value);
                let index_type = self.analyse_expr(index);
                // `xs[i] = v` is the call `[]=(xs, i, v)`. The value isn't
                // analysed yet, so what the container holds stands in for
                // it, and a slice like `xs[0..2]` stores a list of that.
                let element = container.index_element_type();
                let stored = match &element {
                    Some(element)
                        if Self::index_type_is_slice(&index_type)
                            && matches!(container, StaticType::List(_)) =>
                    {
                        StaticType::List(Box::new(element.clone()))
                    }
                    Some(element) => element.clone(),
                    None => StaticType::Any,
                };

                // Indexing isn't operator-form for vec purposes: there's no
                // natural broadcast story for `(list_a, list_b)[i]`. Only an
                // augmented assignment reads the old value through `[]`.
                let errors_before = self.result.errors.len();
                if let Some(getters) = self.resolver.getter_candidates(lvalue.id) {
                    let get_args = [container.clone(), index_type.clone()];
                    let getter =
                        self.overloads()
                            .resolve_call(getters, &get_args, CallKind::Regular);
                    if matches!(getter.binding, Binding::None) {
                        self.emit(AnalysisError::function_not_found("[]", &get_args, span));
                    } else {
                        self.commit_call(
                            &getter.binding,
                            &mut get_args.clone(),
                            &[],
                            getter.return_type,
                            "[]",
                            span,
                        );
                    }
                    self.picks.getters.insert(lvalue.id, getter.binding);
                }

                // Committing the setter checks the index against the
                // container, and settles what neither is known to hold yet,
                // as the key type in `let m = %{}; m["a"] = 1`.
                let set_args = [container.clone(), index_type, stored.clone()];
                let setter = self.overloads().resolve_call(
                    self.resolver.setter_candidates(lvalue.id),
                    &set_args,
                    CallKind::Regular,
                );
                // When reading the old value failed, storing the new one
                // fails the same way, which is reported once.
                let read_failed = self.result.errors.len() > errors_before;
                match setter.binding {
                    _ if read_failed => {}
                    Binding::None if element.is_none() && !container.is_unknown() => {
                        self.emit(AnalysisError::unable_to_index_into(&container, span));
                    }
                    Binding::None => {
                        self.emit(AnalysisError::function_not_found("[]=", &set_args, span));
                    }
                    _ => {
                        self.commit_call(
                            &setter.binding,
                            &mut set_args.clone(),
                            &[],
                            setter.return_type,
                            "[]=",
                            span,
                        );
                    }
                }
                self.picks.setters.insert(lvalue.id, setter.binding);
                self.vars.resolve(&stored)
            }
            // What `(a, b) = …` can store is a tuple of what each target can.
            AssignmentTarget::Sequence(seq) => StaticType::Tuple(
                seq.iter()
                    .map(|sub_lvalue| self.analyse_assignment_target(sub_lvalue, span))
                    .collect(),
            ),
            AssignmentTarget::Member {
                receiver,
                member,
                member_span,
            } => {
                let receiver_type = self.analyse_expr(receiver);
                let getter_args = [receiver_type.clone()];
                let setter_args = [receiver_type, StaticType::Any];
                let getters = self
                    .resolver
                    .getter_candidates(lvalue.id)
                    .expect("internal error: member target without getter candidates");
                let getter =
                    self.overloads()
                        .resolve_call(getters, &getter_args, CallKind::Regular);
                let setter_name = format!("{member}=");
                let setter = self.overloads().resolve_call(
                    self.resolver.setter_candidates(lvalue.id),
                    &setter_args,
                    CallKind::Regular,
                );
                let getter_missing = matches!(getter.binding, Binding::None);
                let setter_missing = matches!(setter.binding, Binding::None);

                self.picks.getters.insert(lvalue.id, getter.binding);
                self.picks.setters.insert(lvalue.id, setter.binding);

                if getter_missing {
                    self.emit(AnalysisError::function_not_found(
                        member,
                        &getter_args,
                        *member_span,
                    ));
                } else if setter_missing {
                    self.emit(AnalysisError::function_not_found(
                        &setter_name,
                        &setter_args,
                        *member_span,
                    ));
                }

                getter.return_type
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

    /// Whether a value of `value_type` can be stored where values of
    /// `stored_type` are kept, settling type variables on the way: in
    /// `let xs = []; xs[0] = 1`, the write settles the element type of `xs`
    /// as `Int`. The stored type never widens to fit the value. An `Any`
    /// value fits anywhere, as `Any` is the dynamic type.
    pub(super) fn accepts_write(
        &mut self,
        stored_type: &StaticType,
        value_type: &StaticType,
    ) -> bool {
        if self.vars.unify(stored_type, value_type) {
            return true;
        }
        let stored_type = self.vars.resolve(stored_type);
        let value_type = self.vars.resolve(value_type);
        value_type == StaticType::Any || value_type.is_subtype(&stored_type)
    }

    /// Reports that a value of `value_type` can't be stored where values of
    /// `stored_type` are kept.
    pub(super) fn emit_rejected_write(
        &mut self,
        stored_type: &StaticType,
        value_type: &StaticType,
        span: Span,
    ) {
        let error = AnalysisError::mismatched_types(
            &self.vars.resolve(value_type),
            &self.vars.resolve(stored_type),
            span,
        );
        self.emit(error);
    }

    /// Checks a value that will be stored through an lvalue against the type
    /// stored there.
    pub(super) fn validate_assignment_write(
        &mut self,
        lvalue: &AssignmentTargetLocation,
        stored_type: &StaticType,
        value_type: &StaticType,
        span: Span,
    ) {
        match &lvalue.target {
            AssignmentTarget::Identifier { .. }
                if self.picks.variables.contains_key(&lvalue.id) =>
            {
                if !self.accepts_write(stored_type, value_type) {
                    self.emit_rejected_write(stored_type, value_type, span);
                }
            }
            AssignmentTarget::Member { .. } => {
                if !self.accepts_write(stored_type, value_type) {
                    self.emit_rejected_write(stored_type, value_type, span);
                }
            }
            AssignmentTarget::Index { index, .. } => {
                if self.accepts_write(stored_type, value_type) {
                    return;
                }
                // `xs[0..2] = (1, 2)` stores the tuple's elements.
                let is_slice = self
                    .result
                    .analysis
                    .expr_types
                    .get(&index.id)
                    .is_some_and(Self::index_type_is_slice);
                if is_slice
                    && let Some(stored_element) = stored_type.index_element_type()
                    && let Some(value_element) = value_type.sequence_element_type()
                    && self.accepts_write(&stored_element, &value_element)
                {
                    return;
                }
                self.emit_rejected_write(stored_type, value_type, span);
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
}
