use super::Analyser;
use crate::error::AnalysisError;
use crate::overloads::{CallKind, Overloads, ResolvedCall};
use ndc_analysis::Binding;
use ndc_core::{NodeId, StaticType};
use ndc_lexer::Span;
use ndc_parser::{Expression, ExpressionLocation};
use ndc_resolver::Candidates;

impl Analyser {
    /// Why no candidate accepts a call of `name` with arguments of these types:
    /// a variable ended the search, a cast would make an overload fit, or
    /// nothing fits.
    fn no_match(
        &self,
        candidates: &Candidates,
        name: &str,
        types: &[StaticType],
        span: Span,
    ) -> AnalysisError {
        let overloads = self.overloads();
        if let Some(variable) = overloads.variable_ending_search(candidates) {
            let hides_functions = overloads.hides_functions(candidates);
            AnalysisError::variable_rejects_arguments(
                name,
                self.types.get_type(variable),
                types,
                hides_functions,
                span,
            )
        } else if overloads.a_cast_could_resolve(candidates, types) {
            AnalysisError::function_not_found_suggest_cast(name, types, span)
        } else {
            AnalysisError::function_not_found(name, types, span)
        }
    }

    /// Overload resolution over the types declared so far.
    pub(super) fn overloads(&self) -> Overloads<'_> {
        Overloads::new(&self.types, self.resolver.symbols())
    }

    /// Analyses `receiver.member`, the call `member(receiver)`.
    pub(super) fn analyse_member_access(
        &mut self,
        id: NodeId,
        receiver: &ExpressionLocation,
        member: &str,
        member_span: Span,
    ) -> StaticType {
        // `xs.len` is the call `len(xs)`, among functions only.
        let receiver_type = self.analyse_expr(receiver);
        let getters = self
            .resolver
            .getter_candidates(id)
            .expect("internal error: member access without getter candidates");
        let ResolvedCall {
            binding,
            return_type,
        } = self.overloads().resolve_call(
            getters,
            std::slice::from_ref(&receiver_type),
            CallKind::Regular,
        );

        if matches!(binding, Binding::None) {
            let error = self.no_match(getters, member, &[receiver_type], member_span);
            self.emit(error);
        }

        self.picks.getters.insert(id, binding);
        return_type
    }

    /// Resolves a call (regular or operator-form) and returns its result type.
    /// Only operator-form calls are eligible for vec dispatch.
    pub(super) fn analyse_call(
        &mut self,
        function: &ExpressionLocation,
        arguments: &[ExpressionLocation],
        kind: CallKind,
        span: Span,
    ) -> StaticType {
        // Higher-order call shapes like `get_function()()` have a non-identifier
        // function position; in that case we just analyse the callee as a value
        // and trust the runtime to dispatch. The callee comes before the
        // arguments, in source order like the resolver, so declarations in it
        // get their slots first.
        let Expression::Identifier { name, .. } = &function.expression else {
            let callee_type = self.analyse_expr(function);
            for arg in arguments {
                self.analyse_expr(arg);
            }
            return match callee_type {
                StaticType::Function { return_type, .. } => *return_type,
                StaticType::Any => StaticType::Any,
                other => {
                    self.emit(AnalysisError::not_callable(&other, span));
                    StaticType::Any
                }
            };
        };

        // A named callee declares nothing, so its overload can be picked after
        // the arguments' types are known.
        let mut type_sig = Vec::with_capacity(arguments.len());
        for arg in arguments.iter() {
            type_sig.push(self.analyse_expr(arg));
        }

        let ResolvedCall {
            binding,
            return_type,
        } = self.overloads().resolve_call(
            self.resolver.reference_candidates(function.id),
            &type_sig,
            kind,
        );

        if matches!(binding, Binding::None) {
            let candidates = self.resolver.reference_candidates(function.id);
            let error = self.no_match(candidates, name, &type_sig, span);
            self.emit(error);
            self.picks.bindings.insert(function.id, binding);
            return StaticType::Any;
        }

        self.picks.bindings.insert(function.id, binding);
        return_type
    }
}
