use ahash::AHashMap;
use ndc_core::StaticType;
use ndc_parser::{Binding, Candidate, ResolvedVar};
use ndc_resolver::{Candidates, SymbolId};

/// Whether a call site can fall back to element-wise tuple broadcast when no
/// scalar overload matches the argument types directly. Set by the parser:
/// operator desugars (`a + b`, `-x`, `op=`) produce `Operator`; everything
/// else (`f(x)`, dot calls, indexing) produces `Regular`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallKind {
    Regular,
    Operator,
}

/// What [`ScopeTree::resolve_call`] hands back: the binding the analyser will
/// store on the call node, plus the inferred return type of the call.
///
/// Folding the return-type computation into the same walk that produces the
/// binding lets the analyser avoid doing per-position resolution twice on
/// every Dynamic operator-form call.
pub(crate) struct ResolvedCall {
    pub binding: Binding,
    pub return_type: StaticType,
}

/// Per-position vec resolution result for an operator-form call. See
/// [`ScopeTree::resolve_vec`] for the case breakdown.
pub(crate) enum VecResolution {
    Static {
        axis_len: usize,
        /// `positions[i]` is the priority-ordered scalar overloads compatible
        /// with the call's element types at position `i`. Always non-empty —
        /// an empty position cancels the whole vec resolution upstream.
        positions: Vec<Vec<SymbolId>>,
    },
    AnyFallback(Vec<SymbolId>),
}

/// Output of a single scalar overload walk across the scope chain.
struct ScalarWalk {
    /// First exact-subtype match found; short-circuits the walk.
    exact: Option<SymbolId>,
    /// First-scope-wins loose-compatibility candidates.
    loose: Option<Vec<SymbolId>>,
    /// Every same-name callable across the whole chain, for runtime-narrowing
    /// fallback when the callee's static type is `Any`.
    all_by_name: Vec<SymbolId>,
}

/// Returns `true` iff `more` is a strict subtype of `less` pointwise — i.e.
/// every position satisfies `more[i] <: less[i]` and at least one position is
/// not bidirectionally equivalent. Used by [`ScopeTree::most_specific_match`] to order
/// matching overloads by specificity: an overload with `more` parameters
/// accepts strictly fewer call signatures than one with `less` parameters.
fn is_strictly_more_specific(more: &[StaticType], less: &[StaticType]) -> bool {
    if more.len() != less.len() {
        return false;
    }
    let mut any_strict = false;
    for (m, l) in more.iter().zip(less) {
        if !m.is_subtype(l) {
            return false;
        }
        if !l.is_subtype(m) {
            any_strict = true;
        }
    }
    any_strict
}

/// If every per-position candidate list contains exactly one entry and they
/// all point to the same scalar overload, return it. This is the only case
/// where `Binding::Resolved(Candidate::Vec)` is safe to emit: a single scalar
/// fires for every element pair.
fn unique_scalar(positions: &[Vec<SymbolId>]) -> Option<SymbolId> {
    let first = positions.first()?.first().copied()?;
    for pos in positions {
        if pos.len() != 1 || pos[0] != first {
            return None;
        }
    }
    Some(first)
}

/// Push `item` unless `out` already holds it, preserving first-seen order. A
/// symbol can appear once as `Candidate::Scalar` and once as `Candidate::Vec`:
/// they are distinct dispatch shapes that must each reach the runtime.
fn push_unique<T: PartialEq>(out: &mut Vec<T>, item: T) {
    if !out.contains(&item) {
        out.push(item);
    }
}

#[derive(Debug, Clone)]
pub(crate) enum TypeBinding {
    Inferred(StaticType),
    Annotated(StaticType),
}

impl TypeBinding {
    pub fn typ(&self) -> &StaticType {
        match self {
            Self::Inferred(t) | Self::Annotated(t) => t,
        }
    }

    pub fn is_annotated(&self) -> bool {
        matches!(self, Self::Annotated(_))
    }
}

/// The type of every value the name resolver declared, natives included,
/// keyed by its symbol. An assignment can widen an inferred type, but an
/// annotated one is fixed.
#[derive(Debug, Clone)]
pub struct ScopeTree {
    /// The natives in registry order: the first is the native in global slot 0.
    natives: Vec<(String, StaticType)>,
    types: AHashMap<SymbolId, TypeBinding>,
}

impl ScopeTree {
    /// A table seeded with the natives' types, in registry order. They're keyed
    /// by symbol once the resolver has declared them, see
    /// [`Self::attach_native_symbols`].
    pub fn from_global_scope(natives: Vec<(String, StaticType)>) -> Self {
        Self {
            natives,
            types: AHashMap::new(),
        }
    }

    /// The names of the natives, in registry order.
    pub(crate) fn native_names(&self) -> impl Iterator<Item = &str> {
        self.natives.iter().map(|(name, _)| name.as_str())
    }

    /// Gives each native's type to its resolver symbol, in registry order.
    pub(crate) fn attach_native_symbols(&mut self, symbols: impl IntoIterator<Item = SymbolId>) {
        for ((_, typ), symbol) in self.natives.iter().zip(symbols) {
            self.types
                .insert(symbol, TypeBinding::Inferred(typ.clone()));
        }
    }

    /// Records the type of a value the resolver declared.
    pub(crate) fn declare(&mut self, symbol: SymbolId, binding: TypeBinding) {
        self.types.insert(symbol, binding);
    }

    pub(crate) fn get_type(&self, symbol: SymbolId) -> &StaticType {
        self.types
            .get(&symbol)
            .expect("internal error: a value is analysed after its declaration")
            .typ()
    }

    /// Try to update a value's type. Returns `Err` with the annotated type if
    /// the value has an explicit type annotation and cannot be widened.
    pub(crate) fn update_binding_type(
        &mut self,
        symbol: SymbolId,
        new_type: StaticType,
    ) -> Result<(), StaticType> {
        let binding = self
            .types
            .get_mut(&symbol)
            .expect("internal error: a value is analysed after its declaration");
        if binding.is_annotated() {
            return Err(binding.typ().clone());
        }
        *binding = TypeBinding::Inferred(new_type);
        Ok(())
    }

    /// Whether some overload among `candidates` would accept these arguments
    /// if their types were known more precisely: every parameter overlaps the
    /// argument in that position, and at least one argument does not already
    /// fit. A failed call that satisfies this can be fixed with a cast, so the
    /// diagnostic says so; one that does not is a genuine mismatch and gets no
    /// misleading advice.
    pub(crate) fn a_cast_could_resolve(&self, candidates: &Candidates, sig: &[StaticType]) -> bool {
        candidates.iter().flatten().any(|&symbol| {
            let StaticType::Function {
                parameters: Some(parameters),
                ..
            } = self.get_type(symbol)
            else {
                return false;
            };
            parameters.len() == sig.len()
                && parameters
                    .iter()
                    .zip(sig)
                    .all(|(param, arg)| param.overlaps(arg))
                && parameters
                    .iter()
                    .zip(sig)
                    .any(|(param, arg)| !arg.is_subtype(param))
        })
    }

    /// Resolve a function call to a binding and an inferred return type.
    ///
    /// `candidates` are the resolver's same-named symbols for the callee,
    /// grouped by scope, nearest first. `locate` says how the call site
    /// reaches the symbols it picks.
    ///
    /// Single entry point for both regular calls (`f(x)`) and operator-form
    /// calls (`a + b`, `-x`, `op=`). The two differ only in whether
    /// element-wise tuple broadcast is allowed when no scalar overload matches.
    ///
    /// Resolution order, first match wins:
    ///   1. **Scalar exact match** — `is_fn_and_matches` on the args. Returns
    ///      `Binding::Resolved(Scalar)`, return type = the overload's declared
    ///      return.
    ///   2. **Operator-form vec, single scalar across all positions** — the
    ///      per-position lookup pins exactly one scalar for every element
    ///      position and no looser scalar competes. Returns
    ///      `Binding::Resolved(Vec)`, return type = `Tuple<scalar_return; n>`.
    ///   3. **Loose scalar or vec candidates** — returns
    ///      `Binding::Dynamic(candidates)`, return type = LUB of contributions
    ///      (pure-scalar LUBs declared returns; pure-vec LUBs per position and
    ///      wraps in `Tuple<…>`; mixed scalar+vec collapses to `Any`).
    ///   4. **Same-name callables, no signature match** — last-resort
    ///      fallback so `Any`-typed callees still get a candidate list.
    ///      Returns `Binding::Dynamic`, return type = `Any`.
    ///   5. Nothing found — `Binding::None`, return type = `Any`.
    pub(crate) fn resolve_call(
        &self,
        candidates: &Candidates,
        sig: &[StaticType],
        kind: CallKind,
        locate: impl Fn(SymbolId) -> ResolvedVar,
    ) -> ResolvedCall {
        let walk = self.scalar_walk(candidates, sig);

        // 1. Scalar exact match.
        if let Some(exact) = walk.exact {
            return ResolvedCall {
                binding: Binding::Resolved(Candidate::Scalar(locate(exact))),
                return_type: self.scalar_return_type(exact),
            };
        }

        // 2. Per-position vec resolution, but only for operator-form calls.
        let vec_resolution = match kind {
            CallKind::Operator => self.resolve_vec(candidates, sig),
            CallKind::Regular => None,
        };

        // 2a. If the user clearly *meant* vec dispatch (at least one arg is
        // statically a tuple) but a per-position lookup found no overload at
        // some position, the call can't succeed at runtime either. Surfacing
        // this as `Binding::None` lets the caller emit a precise compile-time
        // error instead of letting the program limp on to a runtime miss.
        if let Some(VecResolution::Static { positions, .. }) = &vec_resolution
            && sig
                .iter()
                .any(|t| matches!(t, StaticType::Tuple(elems) if !elems.is_empty()))
            && positions.iter().any(|p| p.is_empty())
        {
            return ResolvedCall {
                binding: Binding::None,
                return_type: StaticType::Any,
            };
        }

        let scalar_loose = walk.loose.unwrap_or_default();

        // Resolved(Vec): vec is the unique candidate (no loose scalars compete)
        // and every position pins the same scalar overload.
        if scalar_loose.is_empty()
            && let Some(VecResolution::Static {
                axis_len,
                positions,
            }) = &vec_resolution
            && let Some(scalar) = unique_scalar(positions)
        {
            let scalar_return = self.scalar_return_type(scalar);
            return ResolvedCall {
                binding: Binding::Resolved(Candidate::Vec(locate(scalar))),
                return_type: StaticType::Tuple(vec![scalar_return; *axis_len]),
            };
        }

        // 3. Dynamic: mix scalar loose candidates with vec candidates.
        if !scalar_loose.is_empty() || vec_resolution.is_some() {
            let return_type = self.dynamic_return_type(&scalar_loose, vec_resolution.as_ref());
            let vec_symbols = match vec_resolution {
                Some(VecResolution::Static { positions, .. }) => {
                    positions.into_iter().flatten().collect()
                }
                Some(VecResolution::AnyFallback(symbols)) => symbols,
                None => Vec::new(),
            };
            let mut combined = Vec::new();
            for symbol in scalar_loose {
                let candidate = Candidate::Scalar(locate(symbol));
                push_unique(&mut combined, candidate);
            }
            for symbol in vec_symbols {
                let candidate = Candidate::Vec(locate(symbol));
                push_unique(&mut combined, candidate);
            }
            return ResolvedCall {
                binding: Binding::Dynamic(combined),
                return_type,
            };
        }

        // 4. Last-resort same-name callables (for Any-typed callees, upvalues, …),
        //    but only when at least one candidate could still accept this call
        //    at runtime. When every candidate provably rejects the signature
        //    (fully-annotated functions of the wrong arity or with conflicting
        //    parameter types) the call can never succeed, so we fall through
        //    and let the caller report it at compile time.
        if walk
            .all_by_name
            .iter()
            .any(|&symbol| self.could_accept_call(symbol, sig))
        {
            let candidates = walk
                .all_by_name
                .into_iter()
                .map(|symbol| Candidate::Scalar(locate(symbol)))
                .collect();
            return ResolvedCall {
                binding: Binding::Dynamic(candidates),
                return_type: StaticType::Any,
            };
        }

        // 5. Nothing.
        ResolvedCall {
            binding: Binding::None,
            return_type: StaticType::Any,
        }
    }

    /// Per-scope walk for scalar matches against `sig`. Exact matches
    /// short-circuit; `loose` holds the first scope's compatibility candidates;
    /// `all_by_name` accumulates every callable candidate across the chain for
    /// the runtime-narrowing fallback.
    fn scalar_walk(&self, candidates: &Candidates, sig: &[StaticType]) -> ScalarWalk {
        let mut loose: Option<Vec<SymbolId>> = None;
        let mut all_by_name = Vec::new();

        for group in candidates.iter() {
            if let Some(exact) = self.most_specific_match(group, sig) {
                return ScalarWalk {
                    exact: Some(exact),
                    loose: None,
                    all_by_name: Vec::new(),
                };
            }

            if loose.is_none() {
                let compatible = group
                    .iter()
                    .copied()
                    .filter(|&symbol| self.is_compatible(symbol, sig))
                    .collect::<Vec<_>>();
                if !compatible.is_empty() {
                    loose = Some(compatible);
                }
            }

            // Oldest first within a scope, as the runtime tries them in order.
            all_by_name.extend(
                group
                    .iter()
                    .rev()
                    .copied()
                    .filter(|&symbol| self.get_type(symbol).could_be_callable()),
            );
        }

        ScalarWalk {
            exact: None,
            loose,
            all_by_name,
        }
    }

    /// The best-matching overload in one scope's `group` (newest first) for a
    /// call of the given signature, or `None` if none of them accepts the call.
    ///
    /// Resolution proceeds in two stages:
    ///   1. Filter to overloads whose declared signature accepts `sig`
    ///      (the usual subtype check via [`StaticType::is_fn_and_matches`]).
    ///   2. Among the survivors, pick the one whose parameter signature is
    ///      most specific — i.e. not strictly dominated by any other match's
    ///      parameter signature in the subtype partial order. A signature `b`
    ///      strictly dominates `a` when `b <: a` and `a` is not `<: b`.
    ///
    /// Variadic overloads (`parameters: None`) match anything but are treated
    /// as the least specific shape: any concrete overload that also matches
    /// will dominate them.
    ///
    /// When multiple undominated overloads remain (genuinely incomparable
    /// shapes, e.g. `fn(Int, Any)` vs `fn(Any, Int)` called with `(Int, Int)`),
    /// the latest-registered one wins — preserving shadow semantics.
    fn most_specific_match(&self, group: &[SymbolId], sig: &[StaticType]) -> Option<SymbolId> {
        let matches = group
            .iter()
            .copied()
            .filter(|&symbol| self.get_type(symbol).is_fn_and_matches(sig))
            .collect::<Vec<_>>();

        let params_of = |symbol: SymbolId| -> Option<&[StaticType]> {
            match self.get_type(symbol) {
                StaticType::Function {
                    parameters: Some(p),
                    ..
                } => Some(p.as_slice()),
                // Variadic / non-function bindings have no concrete shape.
                _ => None,
            }
        };

        // `group` is newest first, so ties resolve to the most-recently registered overload.
        matches.iter().copied().find(|&i| {
            let Some(pi) = params_of(i) else {
                // Variadic only wins if every other match is also variadic.
                return matches.iter().all(|&j| params_of(j).is_none());
            };
            matches.iter().all(|&j| {
                if i == j {
                    return true;
                }
                let Some(pj) = params_of(j) else {
                    // Variadic never dominates a concrete overload.
                    return true;
                };
                !is_strictly_more_specific(pj, pi)
            })
        })
    }

    /// Whether `symbol` is a function whose parameters don't provably conflict
    /// with `sig`, so it might accept the call at runtime.
    fn is_compatible(&self, symbol: SymbolId, sig: &[StaticType]) -> bool {
        let StaticType::Function { parameters, .. } = self.get_type(symbol) else {
            return false;
        };
        let Some(parameters) = parameters else {
            unreachable!(
                "a variadic function always matches exactly before compatibility is checked"
            );
        };
        parameters.len() == sig.len()
            && parameters
                .iter()
                .zip(sig)
                .all(|(param, arg)| !param.is_incompatible_with(arg))
    }

    /// Try to resolve a vec candidate set for `sig`.
    ///
    /// * `Static` — at least one arg is statically a non-empty tuple and
    ///   every tuple-shaped arg shares that length. Positions *may* contain
    ///   empty candidate lists; the caller surfaces that as a call error.
    /// * `AnyFallback` — no static tuple shape but at least one arg is `Any`,
    ///   so vec might still apply at runtime; the runtime narrows per pair.
    /// * `None` — no possible vec interpretation.
    fn resolve_vec(&self, candidates: &Candidates, sig: &[StaticType]) -> Option<VecResolution> {
        if let Some((axis_len, positions)) = self.resolve_vec_static(candidates, sig) {
            return Some(VecResolution::Static {
                axis_len,
                positions,
            });
        }
        if !sig.iter().any(|t| matches!(t, StaticType::Any)) {
            return None;
        }
        let permissive: Vec<StaticType> = vec![StaticType::Any; sig.len()];
        let symbols = self.candidates_for_sig(candidates, &permissive);
        if symbols.is_empty() {
            None
        } else {
            Some(VecResolution::AnyFallback(symbols))
        }
    }

    fn resolve_vec_static(
        &self,
        candidates: &Candidates,
        sig: &[StaticType],
    ) -> Option<(usize, Vec<Vec<SymbolId>>)> {
        let mut axis: Option<usize> = None;
        for arg in sig {
            if let StaticType::Tuple(elems) = arg {
                if elems.is_empty() {
                    return None;
                }
                match axis {
                    None => axis = Some(elems.len()),
                    Some(n) if n == elems.len() => {}
                    _ => return None,
                }
            }
        }
        let axis_len = axis?;

        let mut positions = Vec::with_capacity(axis_len);
        for i in 0..axis_len {
            let pos_sig: Vec<StaticType> = sig
                .iter()
                .map(|arg| match arg {
                    StaticType::Tuple(elems) => elems[i].clone(),
                    other => other.clone(),
                })
                .collect();
            positions.push(self.candidates_for_sig(candidates, &pos_sig));
        }
        Some((axis_len, positions))
    }

    /// Priority-ordered scalar candidates for one signature, used by per-position
    /// vec resolution. Exact match (if any) first, then loose-compat candidates
    /// with the exact entry deduplicated.
    fn candidates_for_sig(&self, candidates: &Candidates, sig: &[StaticType]) -> Vec<SymbolId> {
        let walk = self.scalar_walk(candidates, sig);
        let mut out = Vec::new();
        out.extend(walk.exact);
        for symbol in walk.loose.into_iter().flatten() {
            push_unique(&mut out, symbol);
        }
        out
    }

    /// Whether a candidate could still accept a call with this signature at
    /// runtime: its type is `Any`, a variadic function, or a function whose
    /// parameters don't provably conflict with `sig`.
    fn could_accept_call(&self, symbol: SymbolId, sig: &[StaticType]) -> bool {
        match self.get_type(symbol) {
            StaticType::Function {
                parameters: Some(parameters),
                ..
            } => {
                parameters.len() == sig.len()
                    && parameters
                        .iter()
                        .zip(sig)
                        .all(|(param, arg)| !param.is_incompatible_with(arg))
            }
            StaticType::Function {
                parameters: None, ..
            }
            | StaticType::Any => true,
            _ => false,
        }
    }

    /// Return type of a scalar candidate. Falls back to `Any` if the binding
    /// turned out not to carry a function type (e.g. `Any`-typed callee).
    fn scalar_return_type(&self, symbol: SymbolId) -> StaticType {
        match self.get_type(symbol) {
            StaticType::Function { return_type, .. } => return_type.as_ref().clone(),
            _ => StaticType::Any,
        }
    }

    /// LUB the return types of a list of scalar candidates. Used when every
    /// candidate in a Dynamic binding came from the compat-filtered walk and
    /// is therefore guaranteed to be a function.
    fn lub_scalar_returns(&self, symbols: &[SymbolId]) -> StaticType {
        symbols
            .iter()
            .map(|&symbol| self.scalar_return_type(symbol))
            .reduce(|a, b| a.lub(&b))
            .unwrap_or(StaticType::Any)
    }

    /// Return type for a `Binding::Dynamic`. See [`ResolvedCall`] for the
    /// case breakdown.
    fn dynamic_return_type(
        &self,
        scalar_loose: &[SymbolId],
        vec_resolution: Option<&VecResolution>,
    ) -> StaticType {
        let has_scalar = !scalar_loose.is_empty();

        // Pure scalar: LUB the declared returns. This is the precision-recovery
        // case PR #140 had to widen to `Any` for soundness; routing it through
        // the LUB now is safe because we know none of the candidates can vec.
        if has_scalar && vec_resolution.is_none() {
            return self.lub_scalar_returns(scalar_loose);
        }

        // Pure static vec: per-position LUB → Tuple<…> of element returns.
        if !has_scalar
            && let Some(VecResolution::Static {
                axis_len,
                positions,
            }) = vec_resolution
        {
            let elements: Vec<StaticType> = positions
                .iter()
                .map(|pos| self.lub_scalar_returns(pos))
                .collect();
            debug_assert_eq!(elements.len(), *axis_len);
            return StaticType::Tuple(elements);
        }

        // Mixed scalar+vec, Any-fallback vec, or empty (defensive): the
        // result has no useful upper bound — widen to `Any`. `LUB(scalar,
        // Tuple<…>)` collapses to `Any` in our lattice anyway, so there's
        // nothing more precise to compute for the mixed case.
        StaticType::Any
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ndc_resolver::{BindingKind, SymbolTable};
    use std::cell::RefCell;

    thread_local! {
        static SYMBOLS: RefCell<SymbolTable> = RefCell::default();
    }

    /// A fresh symbol, distinct from every other one in the test.
    fn symbol() -> SymbolId {
        SYMBOLS.with_borrow_mut(|symbols| symbols.declare(String::new(), BindingKind::Plain))
    }

    fn fn_type(params: Vec<StaticType>, ret: StaticType) -> StaticType {
        StaticType::Function {
            parameters: Some(params),
            return_type: Box::new(ret),
        }
    }

    fn variadic_fn(ret: StaticType) -> StaticType {
        StaticType::Function {
            parameters: None,
            return_type: Box::new(ret),
        }
    }

    /// A table of these natives, each given a fresh symbol, returned in registry order.
    fn with_natives(natives: Vec<(&str, StaticType)>) -> (ScopeTree, Vec<SymbolId>) {
        let mut tree = ScopeTree::from_global_scope(
            natives
                .into_iter()
                .map(|(name, typ)| (name.to_string(), typ))
                .collect(),
        );
        let symbols = tree.native_names().map(|_| symbol()).collect::<Vec<_>>();
        tree.attach_native_symbols(symbols.clone());
        (tree, symbols)
    }

    /// Candidates of these groups, each given oldest first as declared. The
    /// resolver lists each group newest first.
    fn candidates(groups: &[&[SymbolId]]) -> Candidates {
        Candidates::from_groups(
            groups
                .iter()
                .map(|group| group.iter().rev().copied().collect())
                .collect(),
        )
    }

    /// Asserts the call picks `expected` as its single overload. Each symbol in
    /// `symbols` is located at its position, so the pick shows in the binding.
    fn assert_resolves_to(
        tree: &ScopeTree,
        symbols: &[SymbolId],
        candidates: &Candidates,
        sig: &[StaticType],
        expected: SymbolId,
    ) {
        let locate = |symbol: SymbolId| ResolvedVar::Local {
            slot: symbols
                .iter()
                .position(|&known| known == symbol)
                .expect("the test knows every symbol"),
        };
        let resolved = tree.resolve_call(candidates, sig, CallKind::Regular, locate);
        match resolved.binding {
            Binding::Resolved(Candidate::Scalar(var)) => assert_eq!(var, locate(expected)),
            other => panic!("expected Resolved Scalar, got {other:?}"),
        }
    }

    #[test]
    fn natives_get_their_types_by_symbol() {
        let (tree, natives) = with_natives(vec![
            ("len", fn_type(vec![StaticType::String], StaticType::Int)),
            ("print", variadic_fn(StaticType::unit())),
        ]);
        assert_eq!(
            tree.get_type(natives[0]),
            &fn_type(vec![StaticType::String], StaticType::Int)
        );
        assert_eq!(tree.get_type(natives[1]), &variadic_fn(StaticType::unit()));
    }

    #[test]
    fn inferred_types_widen_but_annotated_types_are_fixed() {
        let mut tree = ScopeTree::from_global_scope(vec![]);
        let inferred = symbol();
        let annotated = symbol();
        tree.declare(inferred, TypeBinding::Inferred(StaticType::Int));
        tree.declare(annotated, TypeBinding::Annotated(StaticType::Int));

        assert_eq!(
            tree.update_binding_type(inferred, StaticType::Number),
            Ok(())
        );
        assert_eq!(tree.get_type(inferred), &StaticType::Number);
        assert_eq!(
            tree.update_binding_type(annotated, StaticType::Number),
            Err(StaticType::Int)
        );
        assert_eq!(tree.get_type(annotated), &StaticType::Int);
    }

    // Two overloads of `locate` registered in the same scope: a more-specific
    // predicate version (takes a function) and a less-specific element version
    // (takes any value). When called with a function value, the predicate
    // version must win even though the element version was registered later.
    #[test]
    fn more_specific_overload_wins_in_same_scope() {
        let seq_any = StaticType::Sequence(Box::new(StaticType::Any));
        let fn_any = variadic_fn(StaticType::Any);
        let (tree, natives) = with_natives(vec![
            (
                "locate",
                fn_type(vec![seq_any.clone(), fn_any.clone()], StaticType::Any),
            ),
            (
                "locate",
                fn_type(vec![seq_any.clone(), StaticType::Any], StaticType::Any),
            ),
        ]);

        let call_sig = [
            StaticType::List(Box::new(StaticType::Int)),
            fn_type(vec![StaticType::Any], StaticType::Bool),
        ];
        assert_resolves_to(
            &tree,
            &natives,
            &candidates(&[&natives]),
            &call_sig,
            natives[0],
        );
    }

    // Int and Number are siblings. An Int call must select the Int overload
    // without treating the Number overload as a fallback candidate.
    #[test]
    fn more_specific_numeric_overload_wins() {
        let (tree, natives) = with_natives(vec![
            ("foo", fn_type(vec![StaticType::Number], StaticType::String)),
            ("foo", fn_type(vec![StaticType::Int], StaticType::String)),
        ]);
        assert_resolves_to(
            &tree,
            &natives,
            &candidates(&[&natives]),
            &[StaticType::Int],
            natives[1],
        );
    }

    // `fn foo(Any)` declared in an inner scope matches every call shape, so
    // it completely shadows any `foo` overload in the parent scope — even a
    // more-specific one. The walk must stop at the first scope with a match.
    #[test]
    fn inner_any_overload_shadows_parent() {
        let (mut tree, natives) = with_natives(vec![(
            "foo",
            fn_type(vec![StaticType::Int], StaticType::Int),
        )]);
        let inner = symbol();
        tree.declare(
            inner,
            TypeBinding::Inferred(fn_type(vec![StaticType::Any], StaticType::Any)),
        );

        assert_resolves_to(
            &tree,
            &[natives[0], inner],
            &candidates(&[&[inner], &natives]),
            &[StaticType::Int],
            inner,
        );
    }

    // `fn foo(Int)` in an inner scope only matches `Int` calls; calls with a
    // different argument type must fall through to the parent scope's overload.
    #[test]
    fn inner_int_overload_does_not_shadow_other_types() {
        let (mut tree, natives) = with_natives(vec![(
            "foo",
            fn_type(vec![StaticType::Any], StaticType::Int),
        )]);
        let inner = symbol();
        tree.declare(
            inner,
            TypeBinding::Inferred(fn_type(vec![StaticType::Int], StaticType::Int)),
        );

        assert_resolves_to(
            &tree,
            &[natives[0], inner],
            &candidates(&[&[inner], &natives]),
            &[StaticType::String],
            natives[0],
        );
    }

    // A concrete overload should beat a variadic overload registered in the
    // same scope when both match the call.
    #[test]
    fn concrete_overload_beats_variadic() {
        let (tree, natives) = with_natives(vec![
            ("foo", variadic_fn(StaticType::Any)),
            ("foo", fn_type(vec![StaticType::Int], StaticType::Int)),
        ]);
        assert_resolves_to(
            &tree,
            &natives,
            &candidates(&[&natives]),
            &[StaticType::Int],
            natives[1],
        );
    }
}
