use crate::types::{TypeTable, TypeVars};
use ndc_analysis::{Binding, Candidate};
use ndc_core::StaticType;
use ndc_resolver::{BindingKind, Candidates, SymbolId, SymbolTable};
use std::borrow::Cow;

/// Whether a call site can fall back to element-wise tuple broadcast when no
/// scalar overload matches the argument types directly. Set by the parser:
/// operator desugars (`a + b`, `-x`, `op=`) produce `Operator`; everything
/// else (`f(x)`, dot calls, indexing) produces `Regular`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallKind {
    Regular,
    Operator,
}

/// What [`Overloads::resolve_call`] hands back: the binding the analyser will
/// store on the call node, plus the inferred return type of the call.
///
/// Folding the return-type computation into the same walk that produces the
/// binding lets the analyser avoid doing per-position resolution twice on
/// every Dynamic operator-form call.
pub(crate) struct ResolvedCall {
    pub binding: Binding<SymbolId>,
    pub return_type: StaticType,
}

/// Per-position vec resolution result for an operator-form call. See
/// [`Overloads::resolve_vec`] for the case breakdown.
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

/// Whether a value of type `arg` might be accepted where `param` is expected
/// when the program runs. For a function passed for a function parameter,
/// only what both types know has to agree: `fn(x) => x * 2`, typed
/// `Fn(Any) -> Any`, can be passed for an `Fn(Int) -> Int`, but
/// `fn(x: String) => 1` can't, and neither can a function of two arguments.
fn could_pass(param: &StaticType, arg: &StaticType) -> bool {
    match (param, arg) {
        (
            StaticType::Function {
                parameters: expected,
                return_type: expected_return,
            },
            StaticType::Function {
                parameters: takes,
                return_type: returns,
            },
        ) => {
            let parameters = match (expected, takes) {
                // What the parameter passes has to fit what the function takes.
                (Some(expected), Some(takes)) => {
                    expected.len() == takes.len()
                        && expected
                            .iter()
                            .zip(takes)
                            .all(|(expected, takes)| could_pass(takes, expected))
                }
                _ => true,
            };
            parameters && could_pass(expected_return, returns)
        }
        _ => !param.is_incompatible_with(arg),
    }
}

/// Returns `true` iff `more` is a strict subtype of `less` pointwise — i.e.
/// every position satisfies `more[i] <: less[i]` and at least one position is
/// not bidirectionally equivalent. Used by [`Overloads::most_specific_match`] to order
/// matching overloads by specificity: an overload with `more` parameters
/// accepts strictly fewer call signatures than one with `less` parameters.
pub(crate) fn is_strictly_more_specific(more: &[StaticType], less: &[StaticType]) -> bool {
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

/// Overload resolution over the type table: which of the candidates a call
/// could mean accepts its arguments.
#[derive(Clone, Copy)]
pub(crate) struct Overloads<'a> {
    types: &'a TypeTable,
    /// What the type variables in those types stand for so far. A function
    /// that returns a captured `let xs = []` returns what `xs` holds.
    vars: &'a TypeVars,
    /// Tells a `fn` from a plain binding like a `let`, which doesn't join overloads.
    symbols: &'a SymbolTable,
}

impl<'a> Overloads<'a> {
    pub(crate) fn new(types: &'a TypeTable, vars: &'a TypeVars, symbols: &'a SymbolTable) -> Self {
        Self {
            types,
            vars,
            symbols,
        }
    }

    /// The candidate groups a call can reach, nearest scope first. A plain
    /// binding, like a `let` or a parameter, is the last candidate: the search
    /// stops there instead of reaching the functions further out, since a
    /// variable doesn't join their overloads. Its type doesn't matter, so
    /// `let len = 5; len(xs)` calls the variable and fails.
    fn reachable(self, candidates: &Candidates) -> impl Iterator<Item = &[SymbolId]> {
        let mut stopped = false;
        candidates.iter().map_while(move |group| {
            if stopped {
                return None;
            }
            Some(
                match group.iter().position(|&symbol| self.ends_search(symbol)) {
                    Some(last) => {
                        stopped = true;
                        &group[..=last]
                    }
                    None => group.as_slice(),
                },
            )
        })
    }

    /// The type of `symbol` as far as it's known: a parameter `g` whose type
    /// a call such as `g(1)` settled is a function from then on.
    fn type_of(self, symbol: SymbolId) -> Cow<'a, StaticType> {
        let typ = self.types.get_type(symbol);
        if typ.has_vars() {
            Cow::Owned(self.vars.resolve(typ))
        } else {
            Cow::Borrowed(typ)
        }
    }

    /// Every candidate a call can reach, nearest scope first.
    pub(crate) fn reachable_symbols(self, candidates: &Candidates) -> Vec<SymbolId> {
        self.reachable(candidates).flatten().copied().collect()
    }

    /// The variable that ends the search, if one does: in
    /// `fn foo(a) { … } let foo = fn(a, b) { … };`, a call of `foo` ends at the `let`.
    pub(crate) fn variable_ending_search(self, candidates: &Candidates) -> Option<SymbolId> {
        self.reachable(candidates)
            .last()
            .and_then(|group| group.last().copied())
            .filter(|&symbol| self.ends_search(symbol))
    }

    /// Whether the search for a call of these candidates stops before some
    /// functions with the name: in `fn foo(a) { … } let foo = fn(a, b) { … };`,
    /// the `let` hides `fn foo`.
    pub(crate) fn hides_functions(self, candidates: &Candidates) -> bool {
        let reachable: usize = self.reachable(candidates).map(<[SymbolId]>::len).sum();
        candidates
            .iter()
            .flatten()
            .skip(reachable)
            .any(|&symbol| self.symbols.kind(symbol) == BindingKind::Function)
    }

    /// Whether `symbol` is a plain binding, which no search goes past.
    fn ends_search(self, symbol: SymbolId) -> bool {
        self.symbols.kind(symbol) == BindingKind::Plain
    }

    /// Whether some overload among `candidates` would accept these arguments
    /// if their types were known more precisely: every parameter overlaps the
    /// argument in that position, and at least one argument does not already
    /// fit. A failed call that satisfies this can be fixed with a cast, so the
    /// diagnostic says so; one that does not is a genuine mismatch and gets no
    /// misleading advice.
    pub(crate) fn a_cast_could_resolve(self, candidates: &Candidates, sig: &[StaticType]) -> bool {
        self.reachable(candidates).flatten().any(|&symbol| {
            let typ = self.type_of(symbol);
            let StaticType::Function {
                parameters: Some(parameters),
                ..
            } = typ.as_ref()
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
    /// grouped by scope, nearest first. The binding names the symbols it picks;
    /// the layout works out later where they live.
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
        self,
        candidates: &Candidates,
        sig: &[StaticType],
        kind: CallKind,
    ) -> ResolvedCall {
        let walk = self.scalar_walk(candidates, sig);

        // 1. Scalar exact match.
        if let Some(exact) = walk.exact {
            return ResolvedCall {
                binding: Binding::Resolved(Candidate::Scalar(exact)),
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
                binding: Binding::Resolved(Candidate::Vec(scalar)),
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
                let candidate = Candidate::Scalar(symbol);
                push_unique(&mut combined, candidate);
            }
            for symbol in vec_symbols {
                let candidate = Candidate::Vec(symbol);
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
                .map(Candidate::Scalar)
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
    fn scalar_walk(self, candidates: &Candidates, sig: &[StaticType]) -> ScalarWalk {
        let mut loose: Option<Vec<SymbolId>> = None;
        let mut all_by_name = Vec::new();

        for group in self.reachable(candidates) {
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
                    .filter(|&symbol| self.type_of(symbol).could_be_callable()),
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
    fn most_specific_match(self, group: &[SymbolId], sig: &[StaticType]) -> Option<SymbolId> {
        let matches = group
            .iter()
            .copied()
            .filter(|&symbol| self.type_of(symbol).is_fn_and_matches(sig))
            .collect::<Vec<_>>();

        let params_of = |symbol: SymbolId| -> Option<Vec<StaticType>> {
            match self.type_of(symbol).into_owned() {
                StaticType::Function {
                    parameters: Some(p),
                    ..
                } => Some(p),
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
                !is_strictly_more_specific(&pj, &pi)
            })
        })
    }

    /// Whether `symbol` is a function whose parameters don't provably conflict
    /// with `sig`, so it might accept the call at runtime.
    fn is_compatible(self, symbol: SymbolId, sig: &[StaticType]) -> bool {
        let typ = self.type_of(symbol);
        let StaticType::Function { parameters, .. } = typ.as_ref() else {
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
                .all(|(param, arg)| could_pass(param, arg))
    }

    /// Try to resolve a vec candidate set for `sig`.
    ///
    /// * `Static` — at least one arg is statically a non-empty tuple and
    ///   every tuple-shaped arg shares that length. Positions *may* contain
    ///   empty candidate lists; the caller surfaces that as a call error.
    /// * `AnyFallback` — no static tuple shape but at least one arg is `Any`,
    ///   so vec might still apply at runtime; the runtime narrows per pair.
    /// * `None` — no possible vec interpretation.
    fn resolve_vec(self, candidates: &Candidates, sig: &[StaticType]) -> Option<VecResolution> {
        if let Some((axis_len, positions)) = self.resolve_vec_static(candidates, sig) {
            return Some(VecResolution::Static {
                axis_len,
                positions,
            });
        }
        if !sig.iter().any(StaticType::is_unknown) {
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
        self,
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
    fn candidates_for_sig(self, candidates: &Candidates, sig: &[StaticType]) -> Vec<SymbolId> {
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
    fn could_accept_call(self, symbol: SymbolId, sig: &[StaticType]) -> bool {
        match self.type_of(symbol).as_ref() {
            StaticType::Function {
                parameters: Some(parameters),
                ..
            } => {
                parameters.len() == sig.len()
                    && parameters
                        .iter()
                        .zip(sig)
                        .all(|(param, arg)| could_pass(param, arg))
            }
            StaticType::Function {
                parameters: None, ..
            } => true,
            // A value whose type isn't known may hold a function.
            unknown => unknown.is_unknown(),
        }
    }

    /// Return type of a scalar candidate. Falls back to `Any` if the binding
    /// turned out not to carry a function type (e.g. `Any`-typed callee).
    ///
    /// A type parameter such as `T` in `first<T>(List<T>) -> T` is `Any`
    /// here: only a call that commits to the overload knows what it stands
    /// for, and one picked at runtime never does.
    fn scalar_return_type(self, symbol: SymbolId) -> StaticType {
        match self.type_of(symbol).as_ref() {
            StaticType::Function { return_type, .. } => self
                .vars
                .resolve(return_type)
                .replace(&|part| matches!(part, StaticType::Generic(_)).then_some(StaticType::Any)),
            _ => StaticType::Any,
        }
    }

    /// LUB the return types of a list of scalar candidates. Used when every
    /// candidate in a Dynamic binding came from the compat-filtered walk and
    /// is therefore guaranteed to be a function.
    fn lub_scalar_returns(self, symbols: &[SymbolId]) -> StaticType {
        symbols
            .iter()
            .map(|&symbol| self.scalar_return_type(symbol))
            .reduce(|a, b| a.lub(&b))
            .unwrap_or(StaticType::Any)
    }

    /// Return type for a `Binding::Dynamic`. See [`ResolvedCall`] for the
    /// case breakdown.
    fn dynamic_return_type(
        self,
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

    /// A fresh symbol of this kind, distinct from every other one in the test.
    fn symbol(kind: BindingKind) -> SymbolId {
        SYMBOLS.with_borrow_mut(|symbols| symbols.declare(String::new(), kind))
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

    /// A table holding these functions' types, each under a fresh `fn`
    /// symbol, returned in order.
    fn table(types: Vec<StaticType>) -> (TypeTable, Vec<SymbolId>) {
        let mut table = TypeTable::default();
        let symbols = types
            .into_iter()
            .map(|typ| {
                let symbol = symbol(BindingKind::Function);
                table.declare(symbol, typ);
                symbol
            })
            .collect();
        (table, symbols)
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

    fn resolve(
        types: &TypeTable,
        candidates: &Candidates,
        sig: &[StaticType],
        kind: CallKind,
    ) -> ResolvedCall {
        SYMBOLS.with_borrow(|table| {
            Overloads::new(types, &TypeVars::default(), table).resolve_call(candidates, sig, kind)
        })
    }

    /// Asserts the call picks `expected` as its single overload.
    fn assert_resolves_to(
        types: &TypeTable,
        candidates: &Candidates,
        sig: &[StaticType],
        expected: SymbolId,
    ) {
        let resolved = resolve(types, candidates, sig, CallKind::Regular);
        assert_eq!(
            resolved.binding,
            Binding::Resolved(Candidate::Scalar(expected))
        );
    }

    // Two overloads of `locate` registered in the same scope: a more-specific
    // predicate version (takes a function) and a less-specific element version
    // (takes any value). When called with a function value, the predicate
    // version must win even though the element version was registered later.
    #[test]
    fn more_specific_overload_wins_in_same_scope() {
        let seq_any = StaticType::Sequence(Box::new(StaticType::Any));
        let fn_any = variadic_fn(StaticType::Any);
        let (types, locates) = table(vec![
            fn_type(vec![seq_any.clone(), fn_any], StaticType::Any),
            fn_type(vec![seq_any, StaticType::Any], StaticType::Any),
        ]);

        let call_sig = [
            StaticType::List(Box::new(StaticType::Int)),
            fn_type(vec![StaticType::Any], StaticType::Bool),
        ];
        assert_resolves_to(&types, &candidates(&[&locates]), &call_sig, locates[0]);
    }

    // `fn foo(Any)` declared in an inner scope matches every call shape, so
    // it completely shadows any `foo` overload in the parent scope — even a
    // more-specific one. The walk must stop at the first scope with a match.
    #[test]
    fn inner_any_overload_shadows_parent() {
        let (types, foos) = table(vec![
            fn_type(vec![StaticType::Int], StaticType::Int),
            fn_type(vec![StaticType::Any], StaticType::Any),
        ]);
        let (outer, inner) = (foos[0], foos[1]);

        assert_resolves_to(
            &types,
            &candidates(&[&[inner], &[outer]]),
            &[StaticType::Int],
            inner,
        );
    }

    // `fn foo(Int)` in an inner scope only matches `Int` calls; calls with a
    // different argument type must fall through to the parent scope's overload.
    #[test]
    fn inner_int_overload_does_not_shadow_other_types() {
        let (types, foos) = table(vec![
            fn_type(vec![StaticType::Any], StaticType::Int),
            fn_type(vec![StaticType::Int], StaticType::Int),
        ]);
        let (outer, inner) = (foos[0], foos[1]);

        assert_resolves_to(
            &types,
            &candidates(&[&[inner], &[outer]]),
            &[StaticType::String],
            outer,
        );
    }

    // The variadic overload is the newer one, which would win a tie.
    #[test]
    fn a_concrete_overload_beats_a_variadic_one() {
        let (types, foos) = table(vec![
            fn_type(vec![StaticType::Int], StaticType::Int),
            variadic_fn(StaticType::Any),
        ]);
        assert_resolves_to(&types, &candidates(&[&foos]), &[StaticType::Int], foos[0]);
    }

    // Neither `(Int, Any)` nor `(Any, Int)` is more specific than the other.
    #[test]
    fn the_newest_of_incomparable_overloads_wins() {
        let (types, foos) = table(vec![
            fn_type(vec![StaticType::Int, StaticType::Any], StaticType::Any),
            fn_type(vec![StaticType::Any, StaticType::Int], StaticType::Any),
        ]);
        assert_resolves_to(
            &types,
            &candidates(&[&foos]),
            &[StaticType::Int, StaticType::Int],
            foos[1],
        );
    }

    // An `Any` argument might be an `Int` or a `String`, so both overloads stay
    // candidates, and the call returns what either of them could.
    #[test]
    fn overloads_that_might_accept_the_call_are_picked_at_runtime() {
        let list_of = |typ| StaticType::List(Box::new(typ));
        let (types, foos) = table(vec![
            fn_type(vec![StaticType::Int], list_of(StaticType::Int)),
            fn_type(vec![StaticType::String], list_of(StaticType::String)),
        ]);
        let resolved = resolve(
            &types,
            &candidates(&[&foos]),
            &[StaticType::Any],
            CallKind::Regular,
        );
        let Binding::Dynamic(picked) = resolved.binding else {
            panic!("expected a dynamic binding, got {:?}", resolved.binding);
        };
        assert_eq!(picked.len(), 2);
        assert!(
            foos.iter()
                .all(|&foo| picked.contains(&Candidate::Scalar(foo)))
        );
        assert_eq!(resolved.return_type, list_of(StaticType::Any));
    }

    #[test]
    fn a_call_every_overload_rejects_binds_nothing() {
        let (types, foos) = table(vec![
            fn_type(vec![StaticType::Int], StaticType::Int),
            fn_type(vec![StaticType::Int, StaticType::Int], StaticType::Int),
        ]);
        let resolved = resolve(
            &types,
            &candidates(&[&foos]),
            &[StaticType::String],
            CallKind::Regular,
        );
        assert_eq!(resolved.binding, Binding::None);
    }

    // `(1, 2) + (3, 4)` adds each pair of elements with the scalar `+`.
    #[test]
    fn an_operator_on_tuples_applies_one_scalar_overload_to_each_element() {
        let (types, plus) = table(vec![fn_type(
            vec![StaticType::Int, StaticType::Int],
            StaticType::Float,
        )]);
        let pair = StaticType::Tuple(vec![StaticType::Int, StaticType::Int]);
        let resolved = resolve(
            &types,
            &candidates(&[&plus]),
            &[pair.clone(), pair],
            CallKind::Operator,
        );
        assert_eq!(resolved.binding, Binding::Resolved(Candidate::Vec(plus[0])));
        assert_eq!(
            resolved.return_type,
            StaticType::Tuple(vec![StaticType::Float, StaticType::Float])
        );
    }

    // Simulates: fn foo(x: Int) { … } let foo = fn(x: String) { … }; foo(1)
    // The `let` ends the search, so the outer `fn` that would accept `Int`
    // isn't reached and nothing matches.
    #[test]
    fn a_variable_holding_a_function_ends_the_search() {
        let (mut types, fns) = table(vec![fn_type(vec![StaticType::Int], StaticType::Int)]);
        let variable = symbol(BindingKind::Plain);
        types.declare(variable, fn_type(vec![StaticType::String], StaticType::Int));
        let candidates = candidates(&[&[fns[0], variable]]);

        SYMBOLS.with_borrow(|table| {
            let vars = TypeVars::default();
            let overloads = Overloads::new(&types, &vars, table);
            let resolved =
                overloads.resolve_call(&candidates, &[StaticType::Int], CallKind::Regular);
            assert_eq!(resolved.binding, Binding::None);
            assert_eq!(
                overloads.variable_ending_search(&candidates),
                Some(variable)
            );
        });
    }

    #[test]
    fn a_variable_ending_the_search_hides_only_the_functions_further_out() {
        let (mut types, fns) = table(vec![fn_type(vec![StaticType::Int], StaticType::Int)]);
        let variable = symbol(BindingKind::Plain);
        types.declare(variable, fn_type(vec![StaticType::String], StaticType::Int));

        SYMBOLS.with_borrow(|table| {
            let vars = TypeVars::default();
            let overloads = Overloads::new(&types, &vars, table);
            assert!(overloads.hides_functions(&candidates(&[&[variable], &[fns[0]]])));
            assert!(!overloads.hides_functions(&candidates(&[&[variable]])));
        });
    }

    // Any plain binding ends the search, whatever its type: one that can't
    // be called leaves the call without a match instead of reaching the
    // function further out.
    #[test]
    fn a_variable_ends_the_search_whatever_its_type() {
        let (mut types, fns) = table(vec![fn_type(vec![StaticType::Int], StaticType::Int)]);
        let variable = symbol(BindingKind::Plain);
        types.declare(variable, StaticType::Int);
        let candidates = candidates(&[&[variable], &[fns[0]]]);

        let resolved = resolve(&types, &candidates, &[StaticType::Int], CallKind::Regular);
        assert_eq!(resolved.binding, Binding::None);
        SYMBOLS.with_borrow(|table| {
            let vars = TypeVars::default();
            let overloads = Overloads::new(&types, &vars, table);
            assert_eq!(
                overloads.variable_ending_search(&candidates),
                Some(variable)
            );
        });
    }
}
