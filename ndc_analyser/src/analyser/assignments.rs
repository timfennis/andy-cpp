use super::Analyser;
use super::solver::{CallNodes, CallSite};
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
        // `a op= b` is `a = op(a, b)`. Where the operands aren't fully known
        // yet, the operator waits for them like any call, and the in-place
        // overload is picked along with it.
        let (left_type, right_type) = (
            self.vars.resolve(&left_type),
            self.vars.resolve(&right_type),
        );
        if (left_type.has_vars() || right_type.has_vars())
            && !self
                .overloads()
                .reachable_symbols(&self.resolver.augmented_candidates(id).operator)
                .is_empty()
        {
            let result = self.constrain_call(
                CallSite::OpAssignment(id),
                vec![left_type.clone(), right_type],
                CallNodes {
                    arguments: vec![None, Some(r_value.id)],
                    result: None,
                },
                CallKind::Operator,
                operation,
                span,
            );
            self.validate_assignment_write(l_value, &left_type, &result, span);
            return StaticType::unit();
        }
        let written = self.op_assignment_now(id, &left_type, &right_type, operation, span);
        self.validate_assignment_write(l_value, &left_type, &written, span);
        StaticType::unit()
    }

    /// Picks what `a op= b` with operands of these types does and returns
    /// the type of the value written back to `a`. An in-place overload
    /// such as `++=` is used where it keeps `a`'s type, and otherwise `op`
    /// makes a new value.
    pub(super) fn op_assignment_now(
        &mut self,
        id: NodeId,
        left_type: &StaticType,
        right_type: &StaticType,
        operation: &str,
        span: Span,
    ) -> StaticType {
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
        let assign_is_eligible = Self::augmented_rhs_is_compatible(left_type, right_type);

        let writeback_type = match assign_binding {
            Binding::Resolved(candidate) if assign_is_eligible => {
                self.record_augmented_pick(id, Binding::Resolved(candidate));
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
                self.record_augmented_pick(id, Binding::Dynamic(assign_candidates));
                Some(result_type)
            }
            Binding::Resolved(_) | Binding::Dynamic(_) => {
                // A specialized mutation exists, but using it would
                // change the concrete left type. Reject it here rather
                // than falling through to an ordinary operation whose
                // erased return type could widen the same target.
                self.emit(AnalysisError::mismatched_types(right_type, left_type, span));
                Some(StaticType::Any)
            }
            Binding::None if has_op_binding => {
                self.record_augmented_pick(id, op_binding);
                Some(op_return)
            }
            Binding::None => {
                self.emit(AnalysisError::function_not_found(
                    operation, &arg_types, span,
                ));
                Some(StaticType::Any)
            }
        };

        writeback_type.unwrap_or_else(|| left_type.clone())
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
                // `xs[i] op= v` reads the old value with `[]`, so the value
                // written has the type that reading gives. A plain write
                // stores a value whose type the write settles.
                let stored = match self.resolver.getter_candidates(lvalue.id) {
                    Some(_) => self.constrain_call(
                        CallSite::Getter(lvalue.id),
                        vec![container.clone(), index_type.clone()],
                        CallNodes {
                            arguments: vec![Some(value.id), Some(index.id)],
                            result: None,
                        },
                        CallKind::Regular,
                        "[]",
                        span,
                    ),
                    None => self.vars.fresh(StaticType::Any),
                };
                // `xs[i] = v` is the call `[]=(xs, i, v)`, which waits for
                // the value's type when nothing else picks the overload.
                self.constrain_call(
                    CallSite::Setter(lvalue.id),
                    vec![container, index_type, stored.clone()],
                    CallNodes {
                        arguments: vec![Some(value.id), Some(index.id), Some(lvalue.id)],
                        result: None,
                    },
                    CallKind::Regular,
                    "[]=",
                    span,
                );
                stored
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
                // `p.x = v` stores through `x=(p, v)`, and what it stores
                // has the type `x(p)` reads.
                let receiver_type = self.analyse_expr(receiver);
                let stored = self.constrain_call(
                    CallSite::Getter(lvalue.id),
                    vec![receiver_type.clone()],
                    CallNodes {
                        arguments: vec![Some(receiver.id)],
                        result: None,
                    },
                    CallKind::Regular,
                    member,
                    *member_span,
                );
                self.constrain_call(
                    CallSite::FieldSetter(lvalue.id),
                    vec![receiver_type, stored.clone()],
                    CallNodes {
                        arguments: vec![Some(receiver.id), Some(lvalue.id)],
                        result: None,
                    },
                    CallKind::Regular,
                    &format!("{member}="),
                    *member_span,
                );
                stored
            }
        }
    }

    /// An index expression typed as an integer sequence (e.g. `0..2`) selects
    /// a slice of the container rather than a single element.
    pub(super) fn index_type_is_slice(index_type: &StaticType) -> bool {
        index_type.fits(&StaticType::Iterator(Box::new(StaticType::Int)))
    }

    /// Specialized `op=` implementations mutate the left operand in place and
    /// keep its concrete type, so the right operand must provably fit it: the
    /// operator copies values across without inspecting them, and nothing
    /// downstream re-checks. A merely overlapping right operand is not enough —
    /// `List<Int> ++= List<Number>` would leave the target holding a `Float`.
    ///
    /// A tuple left-hand side represents vector dispatch, so a scalar right
    /// operand must be compatible with every concrete tuple element.
    pub(super) fn augmented_rhs_is_compatible(
        left_type: &StaticType,
        right_type: &StaticType,
    ) -> bool {
        match (left_type, right_type) {
            (StaticType::Tuple(left), StaticType::Tuple(right)) => {
                left.len() == right.len()
                    && right.iter().zip(left).all(|(right, left)| right.fits(left))
            }
            (StaticType::Tuple(left), right) => left.iter().all(|left| right.fits(left)),
            (left, right) => right.fits(left),
        }
    }

    /// Whether a value of `value_type` can be stored where values of
    /// `stored_type` are kept, settling type variables on the way: in
    /// `let xs = []; xs[0] = 1`, the write settles the element type of `xs`
    /// as `Int`. The stored type never widens to fit the value. An `Any`
    /// value fits anywhere, as `Any` is the dynamic type, and is checked at
    /// `node` as the program runs.
    pub(super) fn accepts_write(
        &mut self,
        stored_type: &StaticType,
        value_type: &StaticType,
        node: Option<NodeId>,
    ) -> bool {
        // A write passes the value where a value of the stored type is
        // expected, as a call passes an argument: `let xs: Sequence<Int> =
        // []` settles the list's elements as Ints.
        let snapshot = self.vars.snapshot();
        if !self.vars.bind_argument(stored_type, value_type) {
            self.vars.rollback(snapshot);
            return false;
        }
        if let Some(node) = node
            && self.vars.resolve(value_type) == StaticType::Any
        {
            self.expect(node, stored_type.clone());
        }
        true
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
            &self.vars.shown(value_type),
            &self.vars.shown(stored_type),
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
                if !self.accepts_write(stored_type, value_type, Some(lvalue.id)) {
                    self.emit_rejected_write(stored_type, value_type, span);
                }
            }
            AssignmentTarget::Member { .. } => {
                if !self.accepts_write(stored_type, value_type, Some(lvalue.id)) {
                    self.emit_rejected_write(stored_type, value_type, span);
                }
            }
            AssignmentTarget::Index { index, .. } => {
                if self.accepts_write(stored_type, value_type, Some(lvalue.id)) {
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
                    && let Some(stored_element) =
                        self.vars.resolve(stored_type).index_element_type()
                    && let Some(value_element) = value_type.sequence_element_type()
                    && self.accepts_write(&stored_element, &value_element, None)
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
