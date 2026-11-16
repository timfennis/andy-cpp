use super::Analyser;
use super::solver::{CallSite, Origin};
use crate::error::AnalysisError;
use crate::overloads::{CallKind, ResolvedCall};
use itertools::izip;
use ndc_analysis::Binding;
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
        let stored_type = self.analyse_assignment_target(l_value, span);
        let left_type = stored_type.clone();
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
                CallKind::Operator,
                operation,
                span,
            );
            self.validate_assignment_write(l_value, &stored_type, &result, span);
            return StaticType::unit();
        }
        let written = self.in_place_only(id, &left_type, &right_type, operation, span);
        self.validate_assignment_write(l_value, &stored_type, &written, span);
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
        let candidates = self.resolver.augmented_candidates(id).in_place.clone();
        let ResolvedCall { binding, .. } =
            self.resolve_overloads(&candidates, &arguments, CallKind::Operator);
        if matches!(binding, Binding::None) {
            self.emit(AnalysisError::function_not_found(
                operation, &arguments, span,
            ));
            return StaticType::Error;
        }
        self.record_augmented_pick(id, binding);
        left_type.clone()
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
                self.types.get_type(symbol).clone()
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
                        CallKind::Regular,
                        "[]",
                        span,
                    ),
                    None => self.vars.fresh(),
                };
                // `xs[i] = v` is the call `[]=(xs, i, v)`, which waits for
                // the value's type when nothing else picks the overload.
                self.constrain_call(
                    CallSite::Setter(lvalue.id),
                    vec![container, index_type, stored.clone()],
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
                    CallKind::Regular,
                    member,
                    *member_span,
                );
                self.constrain_call(
                    CallSite::FieldSetter(lvalue.id),
                    vec![receiver_type, stored.clone()],
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

    /// Whether a value of `value_type` can be stored where values of
    /// `stored_type` are kept, settling type variables on the way: in
    /// `let xs = []; xs[0] = 1`, the write settles the element type of `xs`
    /// as `Int`. The stored type never widens to fit the value. An `Any`
    /// value only fits where an `Any` is kept: it takes a cast to be stored
    /// as anything else.
    pub(super) fn accepts_write(
        &mut self,
        stored_type: &StaticType,
        value_type: &StaticType,
    ) -> bool {
        // A write passes the value where a value of the stored type is
        // expected, as a call passes an argument: `let xs: Iterable<Int> =
        // []` settles the list's elements as Ints.
        let snapshot = self.vars.snapshot();
        if !self.vars.bind_argument(stored_type, value_type) {
            self.vars.rollback(snapshot);
            return false;
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
    ) {
        if !self.accepts_write(stored_type, value_type) {
            self.emit_rejected_write(stored_type, value_type, span, false);
        }
    }

    /// [`Self::check_write`] for an assignment to a variable, whose error
    /// says how a variable's type is decided.
    fn check_variable_write(
        &mut self,
        stored_type: &StaticType,
        value_type: &StaticType,
        span: Span,
    ) {
        if !self.accepts_write(stored_type, value_type) {
            self.emit_rejected_write(stored_type, value_type, span, true);
        }
    }

    /// Reports that a value of `value_type` can't be stored where values of
    /// `stored_type` are kept. Values that share no type, as the branches
    /// of `if c { 1 } else { "a" }`, are reported as such once they're
    /// settled, unless a later use makes them `Any`s, as `let v: Any = …`
    /// does, so their write is checked again then.
    /// `into_variable` says it's an assignment to a variable, which the help
    /// then explains.
    pub(super) fn emit_rejected_write(
        &mut self,
        stored_type: &StaticType,
        value_type: &StaticType,
        span: Span,
        into_variable: bool,
    ) {
        if let StaticType::Var(var) = self.vars.resolve(value_type)
            && self.vars.is_literal(var)
            && self.vars.holds_unshared_values(var)
        {
            self.deferred_writes.push(DeferredWrite {
                stored_type: stored_type.clone(),
                value_type: value_type.clone(),
                span,
                recording: self.recording,
                copied_through: self.copied_through.clone(),
            });
            return;
        }
        let mut error = AnalysisError::mismatched_types(
            &self.vars.shown(value_type),
            &self.vars.shown(stored_type),
            span,
        );
        let value_is_a_number = matches!(
            self.vars.shown(value_type),
            StaticType::Int | StaticType::Float | StaticType::Number
        );
        let stored = self.vars.resolve(stored_type);
        if self.vars.has_number_literal(stored_type) && value_is_a_number {
            error = error.with_help(
                "A number written without a `.` is an Int, and a variable holds what it's \
                 given first: write it as a Float, as in `let total = 0.0`, or annotate the \
                 variable, as in `let total: Float = 0`",
            );
        } else if into_variable && stored == StaticType::unit() {
            error = error.with_help(
                "`()` holds nothing, and a variable holds what it's given first. A value that \
                 only comes later is an `Option`: start with `None` and write `Some(…)`, as in \
                 `let best = None;` and `best = Some(x)`",
            );
        } else if into_variable {
            error = error.with_help(
                "A variable holds one type, the type of what it's given first. Use a new name \
                 for a value of another type, or annotate the variable with `Any` to hold values \
                 of different types, as in `let x: Any = 1`",
            );
        }
        self.emit(error);
    }

    /// Checks again each write [`Self::emit_rejected_write`] put off whose
    /// value is settled now: as `Any`, the write is reported unless `Any` is
    /// what's stored there, and as values that share no type, it was
    /// reported as such.
    pub(super) fn recheck_deferred_writes(&mut self) {
        if self.deferred_writes.is_empty() {
            return;
        }
        let writes = std::mem::take(&mut self.deferred_writes);
        for write in writes {
            let value = self.vars.resolve(&write.value_type);
            if matches!(value, StaticType::Var(var) if self.vars.is_literal(var)) {
                self.deferred_writes.push(write);
                continue;
            }
            if value.has_errors() {
                continue;
            }
            let recording = std::mem::replace(&mut self.recording, write.recording);
            let copied_through = std::mem::replace(&mut self.copied_through, write.copied_through);
            self.check_write(&write.stored_type, &write.value_type, write.span);
            self.recording = recording;
            self.copied_through = copied_through;
        }
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
                self.check_variable_write(stored_type, value_type, span);
            }
            AssignmentTarget::Member { .. } | AssignmentTarget::Index { .. } => {
                self.check_write(stored_type, value_type, span);
            }
            // Each target gets the element unpacked for it, as in a `let`.
            AssignmentTarget::Sequence(targets) => {
                let StaticType::Tuple(stored_types) = stored_type else {
                    unreachable!("internal error: a sequence target stores a tuple")
                };
                let value_types = self.unpack(value_type, targets.len(), span);
                for (target, stored_type, value_type) in izip!(targets, stored_types, value_types) {
                    self.validate_assignment_write(target, stored_type, &value_type, span);
                }
            }
            AssignmentTarget::Identifier { .. } => {}
        }
    }
}

/// A write of values that share no type, put off until they're settled.
#[derive(Debug)]
pub(super) struct DeferredWrite {
    stored_type: StaticType,
    value_type: StaticType,
    span: Span,
    recording: Option<SymbolId>,
    copied_through: Option<Origin>,
}
