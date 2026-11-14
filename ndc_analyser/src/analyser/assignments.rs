use super::Analyser;
use super::solver::{CallNodes, CallSite};
use crate::error::AnalysisError;
use crate::overloads::{CallKind, ResolvedCall};
use itertools::izip;
use ndc_analysis::{Binding, Candidate};
use ndc_core::{NodeId, StaticType};
use ndc_lexer::Span;
use ndc_parser::{AssignmentTarget, AssignmentTargetLocation, ExpressionLocation};
use ndc_resolver::SymbolId;

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
        // `a op= b` is `a = op(a, b)`: the operator is a call like any other,
        // and the in-place overload is picked along with it where it does the
        // same (see `record_pick`).
        let (left_type, right_type) = (
            self.vars.resolve(&left_type),
            self.vars.resolve(&right_type),
        );
        if !self
            .overloads()
            .reachable_symbols(&self.resolver.augmented_candidates(id).operator)
            .is_empty()
        {
            let result = self.constrain_call(
                CallSite::OpAssignment(id),
                vec![left_type.clone(), right_type],
                // What the operator returns is checked where it's written
                // back, when the program picks the operator as it runs.
                CallNodes {
                    arguments: vec![None, Some(r_value.id)],
                    result: Some(l_value.id),
                },
                CallKind::Operator,
                operation,
                span,
            );
            self.validate_assignment_write(l_value, &left_type, &result, span);
            return StaticType::unit();
        }
        let written = self.in_place_only(id, &left_type, &right_type, operation, span);
        self.validate_assignment_write(l_value, &left_type, &written, span);
        StaticType::unit()
    }

    /// Picks the in-place overload `a op= b` calls when there's no operator
    /// `op` to fall back on, and returns the type of the value written back
    /// to `a`, which an in-place overload doesn't change.
    fn in_place_only(
        &mut self,
        id: NodeId,
        left_type: &StaticType,
        right_type: &StaticType,
        operation: &str,
        span: Span,
    ) -> StaticType {
        let arguments = [left_type.clone(), right_type.clone()];
        // An operand whose check failed was reported there.
        if arguments.iter().any(StaticType::has_errors) {
            return StaticType::Error;
        }
        let ResolvedCall { binding, .. } = self.overloads().resolve_call(
            &self.resolver.augmented_candidates(id).in_place,
            &arguments,
            CallKind::Operator,
        );
        if matches!(binding, Binding::None) {
            self.emit(AnalysisError::function_not_found(
                operation, &arguments, span,
            ));
            return StaticType::Error;
        }
        self.record_augmented_pick(id, binding);
        left_type.clone()
    }

    /// What `a op= b` does when the program picks as it runs: the in-place
    /// overloads `in_place` first, so it mutates in place where it can, and
    /// then the operators `operators` picks that aren't among them.
    pub(super) fn in_place_then_operators(
        mut in_place: Vec<Candidate<SymbolId>>,
        operators: Binding<SymbolId>,
    ) -> Binding<SymbolId> {
        let operators = match operators {
            Binding::Resolved(operator) => vec![operator],
            Binding::Dynamic(operators) => operators,
            Binding::None => Vec::new(),
        };
        for operator in operators {
            if !in_place.contains(&operator) {
                in_place.push(operator);
            }
        }
        Binding::Dynamic(in_place)
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
        // expected, as a call passes an argument: `let xs: Iterable<Int> =
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

    /// Checks that a value of `value_type` can be stored where values of
    /// `stored_type` are kept, as [`Self::accepts_write`], and reports it
    /// where it can't. A constraint that waited settles the type it gave
    /// out this way, which code analysed meanwhile may have settled already.
    pub(super) fn check_write(
        &mut self,
        stored_type: &StaticType,
        value_type: &StaticType,
        span: Span,
        node: Option<NodeId>,
    ) {
        if !self.accepts_write(stored_type, value_type, node) {
            self.emit_rejected_write(stored_type, value_type, span);
        }
    }

    /// Reports that a value of `value_type` can't be stored where values of
    /// `stored_type` are kept. Values that share no type, as the branches
    /// of `if c { 1 } else { "a" }`, are reported as such once nothing
    /// else can settle them.
    pub(super) fn emit_rejected_write(
        &mut self,
        stored_type: &StaticType,
        value_type: &StaticType,
        span: Span,
    ) {
        if let StaticType::Var(var) = self.vars.resolve(value_type)
            && self.vars.is_literal(var)
            && self.vars.holds_unshared_values(var)
        {
            return;
        }
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
                self.check_write(stored_type, value_type, span, Some(lvalue.id));
            }
            AssignmentTarget::Member { .. } | AssignmentTarget::Index { .. } => {
                self.check_write(stored_type, value_type, span, Some(lvalue.id));
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
