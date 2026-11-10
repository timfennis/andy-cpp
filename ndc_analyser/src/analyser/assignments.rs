use super::Analyser;
use crate::error::AnalysisError;
use crate::overloads::{CallKind, ResolvedCall};
use itertools::izip;
use ndc_analysis::{Binding, Candidate};
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
        let right_type = self.analyse_expr(r_value);
        let arg_types = vec![left_type.clone(), right_type.clone()];

        let ResolvedCall {
            binding: assign_binding,
            return_type: assign_return,
        } = self.overloads().resolve_call(
            &self.resolver.augmented_candidates(id).in_place,
            &arg_types,
            CallKind::Operator,
            |symbol| self.locate(symbol),
        );
        let ResolvedCall {
            binding: op_binding,
            return_type: op_return,
        } = self.overloads().resolve_call(
            &self.resolver.augmented_candidates(id).operator,
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
                self.result
                    .analysis
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
                self.result
                    .analysis
                    .augmented_assignments
                    .insert(id, op_binding);
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
                let variable = self.locate(symbol);
                self.result.analysis.variables.insert(lvalue.id, variable);
                self.types.get_type(symbol).clone()
            }
            AssignmentTarget::Index { index, value } => {
                // The container before the index, in source order like the resolver.
                let type_of_index_target = self.analyse_expr(value);
                let index_type = self.analyse_expr(index);

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
                    return StaticType::List(element.clone());
                }

                if let Some(t) = type_of_index_target.index_element_type() {
                    t
                } else {
                    self.emit(AnalysisError::unable_to_index_into(
                        &type_of_index_target,
                        span,
                    ));
                    StaticType::Any
                }
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

    /// Validate a value that will be stored through an lvalue, widening an
    /// inferred binding when the location has a stable variable to update.
    pub(super) fn validate_assignment_write(
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
}
