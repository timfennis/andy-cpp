use crate::name_resolution::Candidates;
use crate::symbols::SymbolId;
use ndc_core::StaticType;
use ndc_core::r#struct::StructInfo;
use ndc_parser::{Binding, Candidate, CaptureSource, ResolvedVar};
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

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

/// What a type name refers to in the type namespace. Type names live in the
/// same lexical scopes as value bindings but form a separate namespace: they
/// have no runtime slots and are only consulted when lowering annotations.
#[derive(Debug, Clone)]
pub(crate) enum TypeDef {
    /// A built-in type name (`Int`, `List`, …); lowering delegates to
    /// [`StaticType::from_name_and_args`], which also checks generic arity.
    Builtin,
    /// A user-declared struct.
    Struct(Rc<StructInfo>),
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

#[derive(Debug, Clone)]
pub(crate) struct ScopeBinding {
    pub name: String,
    pub binding: TypeBinding,
    /// The name resolver's symbol for this binding. Natives get theirs when the
    /// analyser is created, since this tree is built before the resolver.
    pub symbol: Option<SymbolId>,
}

/// A binding a function captures from an enclosing function.
#[derive(Debug, Clone)]
struct Upvalue {
    /// The name resolver's symbol for the captured binding.
    symbol: SymbolId,
    source: CaptureSource,
}

#[derive(Debug, Clone)]
pub(crate) struct Scope {
    parent_idx: Option<usize>,
    creates_environment: bool, // Only true for function scopes and for-loop iterations
    base_offset: usize,
    function_scope_idx: usize,
    identifiers: Vec<ScopeBinding>,
    upvalues: Vec<Upvalue>,
    /// Type names declared in this scope (separate namespace from `identifiers`).
    types: Vec<(String, TypeDef)>,
}

impl Scope {
    pub(crate) fn offset(&self) -> usize {
        self.base_offset + self.identifiers.len()
    }

    pub(crate) fn new_function_scope(parent_idx: Option<usize>, function_scope_idx: usize) -> Self {
        Self {
            parent_idx,
            creates_environment: true,
            base_offset: 0,
            function_scope_idx,
            identifiers: Vec::default(),
            upvalues: Vec::default(),
            types: Vec::default(),
        }
    }

    pub(crate) fn new_block_scope(
        parent_idx: Option<usize>,
        base_offset: usize,
        function_scope_idx: usize,
    ) -> Self {
        Self {
            parent_idx,
            creates_environment: false,
            base_offset,
            function_scope_idx,
            identifiers: Vec::default(),
            upvalues: Vec::default(),
            types: Vec::default(),
        }
    }

    /// Identical to `new_block_scope` today — kept as a separate constructor so that
    /// iteration-specific behaviour (e.g. break/continue scoping) can be added later.
    pub(crate) fn new_iteration_scope(
        parent_idx: Option<usize>,
        base_offset: usize,
        function_scope_idx: usize,
    ) -> Self {
        Self::new_block_scope(parent_idx, base_offset, function_scope_idx)
    }

    fn find_slot_by_symbol(&self, symbol: SymbolId) -> Option<usize> {
        self.identifiers
            .iter()
            .rposition(|b| b.symbol == Some(symbol))
            .map(|idx| idx + self.base_offset)
    }

    /// Check if this scope already contains a function with the given name and arity.
    fn has_function_with_arity(&self, name: &str, arity: Option<usize>) -> bool {
        self.identifiers.iter().any(|b| {
            if b.name != name {
                return false;
            }
            match b.binding.typ() {
                StaticType::Function {
                    parameters: Some(params),
                    ..
                } => match arity {
                    Some(a) => params.len() == a,
                    None => false,
                },
                StaticType::Function {
                    parameters: None, ..
                } => arity.is_none(),
                _ => false,
            }
        })
    }

    fn allocate(&mut self, name: String, type_binding: TypeBinding, symbol: SymbolId) -> usize {
        self.identifiers.push(ScopeBinding {
            name,
            binding: type_binding,
            symbol: Some(symbol),
        });
        self.base_offset + self.identifiers.len() - 1
    }

    fn add_upvalue(&mut self, symbol: SymbolId, source: CaptureSource) -> usize {
        if let Some(idx) = self
            .upvalues
            .iter()
            .position(|u| u.symbol == symbol && u.source == source)
        {
            return idx;
        }

        self.upvalues.push(Upvalue { symbol, source });
        self.upvalues.len() - 1
    }

    fn find_upvalue_by_symbol(&self, symbol: SymbolId) -> Option<usize> {
        self.upvalues.iter().position(|u| u.symbol == symbol)
    }
}

#[derive(Clone)]
pub struct ScopeTree {
    current_scope_idx: usize,
    global_scope: Scope,
    scopes: Vec<Scope>,
}

impl ScopeTree {
    /// Build a `ScopeTree` seeded with pre-registered global bindings (native functions etc.).
    ///
    /// Two root scopes exist by design: `global_scope` holds native/built-in bindings that are
    /// always accessible, while `scopes[0]` is the user's top-level function scope where
    /// user-defined declarations land. This separation keeps native bindings out of the
    /// mutable scope chain so they can be searched as a fallback without interfering with
    /// user-level shadowing.
    pub fn from_global_scope(global_scope_map: Vec<(String, StaticType)>) -> Self {
        let mut global_scope = Scope::new_function_scope(None, 0);
        global_scope.identifiers = global_scope_map
            .into_iter()
            .map(|(name, typ)| ScopeBinding {
                name,
                binding: TypeBinding::Inferred(typ),
                symbol: None,
            })
            .collect();
        global_scope.types = StaticType::BUILTIN_TYPE_NAMES
            .iter()
            .map(|name| ((*name).to_string(), TypeDef::Builtin))
            .collect();

        Self {
            current_scope_idx: 0,
            global_scope,
            scopes: vec![Scope::new_function_scope(None, 0)],
        }
    }

    /// The names of the native bindings, in registration order.
    pub(crate) fn native_names(&self) -> impl Iterator<Item = &str> {
        self.global_scope
            .identifiers
            .iter()
            .map(|binding| binding.name.as_str())
    }

    pub(crate) fn get_type(&self, res: ResolvedVar) -> &StaticType {
        match res {
            ResolvedVar::Local { slot } => self.find_type_by_slot(self.current_scope_idx, slot),
            ResolvedVar::Upvalue { slot } => {
                let mut scope_idx = self.scopes[self.current_scope_idx].function_scope_idx;
                let mut check_slot = slot;

                loop {
                    match &self.scopes[scope_idx].upvalues[check_slot].source {
                        CaptureSource::Local(slot) => {
                            let parent = self.scopes[scope_idx]
                                .parent_idx
                                .expect("expected parent scope");
                            return self.find_type_by_slot(parent, *slot);
                        }
                        CaptureSource::Upvalue(slot) => {
                            scope_idx = self.get_parent_function_scope_idx(scope_idx);
                            check_slot = *slot;
                        }
                    }
                }
            }
            ResolvedVar::Global { slot } => self.global_scope.identifiers[slot].binding.typ(),
        }
    }

    fn get_parent_function_scope_idx(&self, scope_idx: usize) -> usize {
        self.scopes[self.scopes[scope_idx]
            .parent_idx
            .expect("expected parent scope to exist")]
        .function_scope_idx
    }

    pub(crate) fn find_type_by_slot(&self, start_scope: usize, slot: usize) -> &StaticType {
        let mut scope_idx = start_scope;
        loop {
            let scope = &self.scopes[scope_idx];
            if slot >= scope.base_offset && slot < scope.base_offset + scope.identifiers.len() {
                return scope.identifiers[slot - scope.base_offset].binding.typ();
            }
            scope_idx = scope
                .parent_idx
                .expect("slot not found in any scope within function");
        }
    }

    pub(crate) fn new_block_scope(&mut self) -> &Scope {
        let old_scope_idx = self.current_scope_idx;
        self.current_scope_idx = self.scopes.len();
        let new_scope = Scope::new_block_scope(
            Some(old_scope_idx),
            self.scopes[old_scope_idx].offset(),
            self.scopes[old_scope_idx].function_scope_idx,
        );
        self.scopes.push(new_scope);
        &self.scopes[self.current_scope_idx]
    }

    pub(crate) fn new_function_scope(&mut self) -> &Scope {
        let old_scope_idx = self.current_scope_idx;
        self.current_scope_idx = self.scopes.len();
        let new_scope = Scope::new_function_scope(Some(old_scope_idx), self.scopes.len());
        self.scopes.push(new_scope);
        &self.scopes[self.current_scope_idx]
    }

    pub(crate) fn new_iteration_scope(&mut self) -> &Scope {
        let old_scope_idx = self.current_scope_idx;
        self.current_scope_idx = self.scopes.len();
        let new_scope = Scope::new_iteration_scope(
            Some(old_scope_idx),
            self.scopes[old_scope_idx].offset(),
            self.scopes[old_scope_idx].function_scope_idx,
        );
        self.scopes.push(new_scope);
        &self.scopes[self.current_scope_idx]
    }

    pub(crate) fn destroy_scope(&mut self) {
        let next = self.scopes[self.current_scope_idx]
            .parent_idx
            .expect("tried to destroy scope while there were none");
        self.current_scope_idx = next;
    }

    pub(crate) fn current_scope_captures(&self) -> Vec<CaptureSource> {
        self.scopes[self.current_scope_idx]
            .upvalues
            .iter()
            .map(|upvalue| upvalue.source.clone())
            .collect()
    }

    /// Where the binding for the resolver's `symbol` lives, seen from the
    /// current scope. A binding in an enclosing function becomes an upvalue of
    /// every function in between, so `x` in `let x = 1; fn f() { x }` is
    /// `f`'s upvalue 0.
    pub(crate) fn get_binding(&mut self, symbol: SymbolId) -> ResolvedVar {
        let mut scope_ptr = self.current_scope_idx;
        let mut env_scopes: Vec<usize> = Vec::default();

        loop {
            if let Some(slot) = self.scopes[scope_ptr].find_slot_by_symbol(symbol) {
                return self.resolve_found_local(scope_ptr, slot, &env_scopes);
            }
            if let Some(slot) = self.scopes[scope_ptr].find_upvalue_by_symbol(symbol) {
                return self.resolve_found_upvalue(scope_ptr, slot, &env_scopes);
            }

            if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                if self.scopes[scope_ptr].creates_environment {
                    env_scopes.push(scope_ptr);
                }
                scope_ptr = parent_idx;
            } else {
                let slot = self
                    .global_scope
                    .find_slot_by_symbol(symbol)
                    .expect("internal error: the resolver's symbol has no binding in scope");
                return ResolvedVar::Global { slot };
            }
        }
    }

    /// The type of the binding for `symbol`. Unlike [`Self::get_binding`], it
    /// creates no upvalues, so overload resolution can inspect every candidate
    /// and capture only the ones it picks.
    fn symbol_type(&self, symbol: SymbolId) -> &StaticType {
        let mut scope_ptr = Some(self.current_scope_idx);
        while let Some(idx) = scope_ptr {
            let scope = &self.scopes[idx];
            if let Some(slot) = scope.find_slot_by_symbol(symbol) {
                return scope.identifiers[slot - scope.base_offset].binding.typ();
            }
            scope_ptr = scope.parent_idx;
        }

        let slot = self
            .global_scope
            .find_slot_by_symbol(symbol)
            .expect("internal error: the resolver's symbol has no binding in scope");
        self.global_scope.identifiers[slot].binding.typ()
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
            } = self.symbol_type(symbol)
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
    /// grouped by scope, nearest first. Only the symbols that end up in the
    /// binding are captured as upvalues.
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
        &mut self,
        candidates: &Candidates,
        sig: &[StaticType],
        kind: CallKind,
    ) -> ResolvedCall {
        let walk = self.scalar_walk(candidates, sig);

        // 1. Scalar exact match.
        if let Some(exact) = walk.exact {
            return ResolvedCall {
                binding: Binding::Resolved(Candidate::Scalar(self.get_binding(exact))),
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
                binding: Binding::Resolved(Candidate::Vec(self.get_binding(scalar))),
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
                let candidate = Candidate::Scalar(self.get_binding(symbol));
                push_unique(&mut combined, candidate);
            }
            for symbol in vec_symbols {
                let candidate = Candidate::Vec(self.get_binding(symbol));
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
                .map(|symbol| Candidate::Scalar(self.get_binding(symbol)))
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
                    .filter(|&symbol| self.symbol_type(symbol).could_be_callable()),
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
            .filter(|&symbol| self.symbol_type(symbol).is_fn_and_matches(sig))
            .collect::<Vec<_>>();

        let params_of = |symbol: SymbolId| -> Option<&[StaticType]> {
            match self.symbol_type(symbol) {
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
        let StaticType::Function { parameters, .. } = self.symbol_type(symbol) else {
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
        match self.symbol_type(symbol) {
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
        match self.symbol_type(symbol) {
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

    /// Declare a type name in the current scope. Shadowing a same-named type
    /// in an outer scope is allowed; the caller is responsible for rejecting
    /// redeclarations within one scope and shadowing of built-in type names.
    pub(crate) fn declare_type(&mut self, name: String, def: TypeDef) {
        self.scopes[self.current_scope_idx].types.push((name, def));
    }

    /// Resolve a type name through the scope chain (innermost first), falling
    /// back to the global scope where the built-in type names live.
    pub(crate) fn resolve_type(&self, name: &str) -> Option<&TypeDef> {
        let mut scope_idx = self.current_scope_idx;
        loop {
            let scope = &self.scopes[scope_idx];
            if let Some((_, def)) = scope.types.iter().rev().find(|(n, _)| n == name) {
                return Some(def);
            }
            match scope.parent_idx {
                Some(parent_idx) => scope_idx = parent_idx,
                None => break,
            }
        }
        self.global_scope
            .types
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, def)| def)
    }

    /// Whether the current scope itself already declares a type with this
    /// name. Used to detect illegal same-scope redefinitions.
    pub(crate) fn has_type_in_current_scope(&self, name: &str) -> bool {
        self.scopes[self.current_scope_idx]
            .types
            .iter()
            .any(|(n, _)| n == name)
    }

    pub(crate) fn create_local_binding(
        &mut self,
        ident: String,
        binding: TypeBinding,
        symbol: SymbolId,
    ) -> ResolvedVar {
        ResolvedVar::Local {
            slot: self.scopes[self.current_scope_idx].allocate(ident, binding, symbol),
        }
    }

    /// Gives each native binding its resolver symbol, in registry order.
    pub(crate) fn attach_native_symbols(&mut self, symbols: impl IntoIterator<Item = SymbolId>) {
        for (binding, symbol) in self.global_scope.identifiers.iter_mut().zip(symbols) {
            binding.symbol = Some(symbol);
        }
    }

    /// Check whether the current scope already has a `fn` declaration with
    /// the given name and arity. Used to detect illegal same-scope redefinitions.
    pub(crate) fn has_function_in_current_scope(&self, name: &str, arity: Option<usize>) -> bool {
        self.scopes[self.current_scope_idx].has_function_with_arity(name, arity)
    }

    /// Try to update a binding's type. Returns `Err` with the annotated type
    /// if the binding has an explicit type annotation and cannot be widened.
    pub(crate) fn update_binding_type(
        &mut self,
        var: ResolvedVar,
        new_type: StaticType,
    ) -> Result<(), StaticType> {
        let binding = self.get_binding_mut(var);
        if binding.is_annotated() {
            return Err(binding.typ().clone());
        }
        *binding = TypeBinding::Inferred(new_type);
        Ok(())
    }

    fn get_binding_mut(&mut self, var: ResolvedVar) -> &mut TypeBinding {
        match var {
            ResolvedVar::Local { slot } => {
                let scope_idx = self.find_scope_owning_slot(self.current_scope_idx, slot);
                let base = self.scopes[scope_idx].base_offset;
                &mut self.scopes[scope_idx].identifiers[slot - base].binding
            }
            ResolvedVar::Upvalue { slot } => {
                let mut scope_idx = self.scopes[self.current_scope_idx].function_scope_idx;
                let mut check_slot = slot;

                loop {
                    match self.scopes[scope_idx].upvalues[check_slot].source {
                        CaptureSource::Local(local_slot) => {
                            let parent = self.scopes[scope_idx]
                                .parent_idx
                                .expect("expected parent scope");
                            let owning = self.find_scope_owning_slot(parent, local_slot);
                            let base = self.scopes[owning].base_offset;
                            return &mut self.scopes[owning].identifiers[local_slot - base].binding;
                        }
                        CaptureSource::Upvalue(uv_slot) => {
                            scope_idx = self.get_parent_function_scope_idx(scope_idx);
                            check_slot = uv_slot;
                        }
                    }
                }
            }
            ResolvedVar::Global { .. } => {
                unreachable!("get_binding_mut called with a global binding")
            }
        }
    }

    pub(crate) fn find_scope_owning_slot(&self, start_scope: usize, slot: usize) -> usize {
        let mut scope_idx = start_scope;
        loop {
            let scope = &self.scopes[scope_idx];
            if slot >= scope.base_offset && slot < scope.base_offset + scope.identifiers.len() {
                return scope_idx;
            }
            scope_idx = scope
                .parent_idx
                .expect("slot not found in any scope within function");
        }
    }

    /// Given a local slot of `scope_idx` found during a scope walk, return the appropriate
    /// `ResolvedVar`. If `env_scopes` is empty the slot is in the current function scope and
    /// can be referenced directly as a `Local`. Otherwise, it must be hoisted through
    /// intervening function scopes as an upvalue chain.
    fn resolve_found_local(
        &mut self,
        scope_idx: usize,
        slot: usize,
        env_scopes: &[usize],
    ) -> ResolvedVar {
        if env_scopes.is_empty() {
            return ResolvedVar::Local { slot };
        }

        let scope = &self.scopes[scope_idx];
        let symbol = scope.identifiers[slot - scope.base_offset]
            .symbol
            .expect("internal error: local binding without a resolver symbol");
        let slot = self.hoist_upvalue(symbol, slot, env_scopes);
        ResolvedVar::Upvalue { slot }
    }

    /// Given an upvalue slot of `scope_idx` found during a scope walk, return the appropriate
    /// `ResolvedVar`. If `env_scopes` is empty the upvalue belongs to the current function
    /// scope. Otherwise it must be hoisted further through intervening function scopes.
    fn resolve_found_upvalue(
        &mut self,
        scope_idx: usize,
        slot: usize,
        env_scopes: &[usize],
    ) -> ResolvedVar {
        if env_scopes.is_empty() {
            return ResolvedVar::Upvalue { slot };
        }

        let symbol = self.scopes[scope_idx].upvalues[slot].symbol;
        let slot = self.hoist_from_upvalue(symbol, slot, env_scopes);
        ResolvedVar::Upvalue { slot }
    }

    // In a situation where the analyser is recursing through the scope tree and finds an identifier
    // in the current local scope, if we were searching from a nested scope we now have to 'hoist'
    // this local value as an upvalue in all the nested scopes. This function is responsible for adding
    // this value as an upvalue to all the nested scopes.
    //
    // `env_scopes`: is a list of scopes that the analyser has already searched that will need to get this upvalue.
    fn hoist_upvalue(
        &mut self,
        symbol: SymbolId,
        local_slot: usize,
        env_scopes: &[usize],
    ) -> usize {
        let mut capture_idx = local_slot;
        let mut is_local = true;

        for &scope_idx in env_scopes.iter().rev() {
            // The very first scope we encounter when we iterate this list in reverse is the scope directly inside
            // the scope that captures the identifier as a local scope. In this case we want to capture the variable on the stack instead.
            let source = if is_local {
                CaptureSource::Local(capture_idx)
            } else {
                CaptureSource::Upvalue(capture_idx)
            };
            capture_idx = self.scopes[scope_idx].add_upvalue(symbol, source);
            is_local = false; // only the first iteration is local
        }

        capture_idx
    }

    fn hoist_from_upvalue(
        &mut self,
        symbol: SymbolId,
        upvalue_slot: usize,
        env_scopes: &[usize],
    ) -> usize {
        let mut capture_idx = upvalue_slot;
        for &scope_idx in env_scopes.iter().rev() {
            capture_idx =
                self.scopes[scope_idx].add_upvalue(symbol, CaptureSource::Upvalue(capture_idx));
        }

        capture_idx
    }
}

impl Debug for ScopeTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f)?;
        for (id, scope) in self.scopes.iter().enumerate() {
            writeln!(f, "{id}: {scope:?}")?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbols::{BindingKind, SymbolTable};
    use ndc_parser::ResolvedVar;
    use std::cell::RefCell;

    thread_local! {
        static SYMBOLS: RefCell<SymbolTable> = RefCell::default();
    }

    /// A fresh symbol, distinct from every other one in the test.
    fn symbol() -> SymbolId {
        SYMBOLS.with_borrow_mut(|symbols| symbols.declare(String::new(), BindingKind::Plain))
    }

    fn empty_scope_tree() -> ScopeTree {
        ScopeTree::from_global_scope(vec![])
    }

    #[test]
    fn single_local_in_function_scope() {
        let x_symbol = symbol();
        let mut tree = empty_scope_tree();
        let var =
            tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int), x_symbol);
        assert_eq!(var, ResolvedVar::Local { slot: 0 });
        assert_eq!(tree.get_binding(x_symbol), ResolvedVar::Local { slot: 0 });
    }

    #[test]
    fn multiple_locals_get_ascending_slots() {
        let mut tree = empty_scope_tree();
        let x =
            tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int), symbol());
        let y =
            tree.create_local_binding("y".into(), TypeBinding::Inferred(StaticType::Int), symbol());
        let z =
            tree.create_local_binding("z".into(), TypeBinding::Inferred(StaticType::Int), symbol());
        assert_eq!(x, ResolvedVar::Local { slot: 0 });
        assert_eq!(y, ResolvedVar::Local { slot: 1 });
        assert_eq!(z, ResolvedVar::Local { slot: 2 });
    }

    #[test]
    fn block_scope_continues_flat_numbering() {
        let x_symbol = symbol();
        let mut tree = empty_scope_tree();
        let x =
            tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int), x_symbol);
        assert_eq!(x, ResolvedVar::Local { slot: 0 });

        tree.new_block_scope();
        let y =
            tree.create_local_binding("y".into(), TypeBinding::Inferred(StaticType::Int), symbol());
        assert_eq!(y, ResolvedVar::Local { slot: 1 });

        assert_eq!(tree.get_binding(x_symbol), ResolvedVar::Local { slot: 0 });
    }

    #[test]
    fn nested_block_scopes_continue_numbering() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("a".into(), TypeBinding::Inferred(StaticType::Int), symbol());

        tree.new_block_scope();
        let b =
            tree.create_local_binding("b".into(), TypeBinding::Inferred(StaticType::Int), symbol());
        assert_eq!(b, ResolvedVar::Local { slot: 1 });

        tree.new_block_scope();
        let c =
            tree.create_local_binding("c".into(), TypeBinding::Inferred(StaticType::Int), symbol());
        assert_eq!(c, ResolvedVar::Local { slot: 2 });
    }

    #[test]
    fn block_scope_does_not_create_upvalue() {
        let x_symbol = symbol();
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int), x_symbol);

        tree.new_block_scope();
        assert_eq!(tree.get_binding(x_symbol), ResolvedVar::Local { slot: 0 });
    }

    #[test]
    fn function_scope_resets_slots_and_captures_as_upvalue() {
        let x_symbol = symbol();
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int), x_symbol);

        tree.new_function_scope();
        let y =
            tree.create_local_binding("y".into(), TypeBinding::Inferred(StaticType::Int), symbol());
        assert_eq!(y, ResolvedVar::Local { slot: 0 });

        assert_eq!(tree.get_binding(x_symbol), ResolvedVar::Upvalue { slot: 0 });
    }

    #[test]
    fn iteration_scope_continues_numbering_and_is_transparent() {
        let x_symbol = symbol();
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int), x_symbol);

        tree.new_iteration_scope();
        let i =
            tree.create_local_binding("i".into(), TypeBinding::Inferred(StaticType::Int), symbol());
        assert_eq!(i, ResolvedVar::Local { slot: 1 });

        assert_eq!(tree.get_binding(x_symbol), ResolvedVar::Local { slot: 0 });
    }

    #[test]
    fn global_lookup() {
        let print_symbol = symbol();
        let mut tree = ScopeTree::from_global_scope(vec![(
            "print".into(),
            StaticType::Function {
                parameters: None,
                return_type: Box::new(StaticType::Any),
            },
        )]);
        tree.attach_native_symbols([print_symbol]);
        assert_eq!(
            tree.get_binding(print_symbol),
            ResolvedVar::Global { slot: 0 }
        );
    }

    #[test]
    fn slot_reuse_after_scope_destroy() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("a".into(), TypeBinding::Inferred(StaticType::Int), symbol());

        tree.new_block_scope();
        tree.create_local_binding("b".into(), TypeBinding::Inferred(StaticType::Int), symbol());
        tree.destroy_scope();

        let c =
            tree.create_local_binding("c".into(), TypeBinding::Inferred(StaticType::Int), symbol());
        assert_eq!(c, ResolvedVar::Local { slot: 1 });
    }

    #[test]
    fn get_type_returns_correct_type() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int), symbol());
        tree.create_local_binding(
            "y".into(),
            TypeBinding::Inferred(StaticType::String),
            symbol(),
        );

        assert_eq!(
            tree.get_type(ResolvedVar::Local { slot: 0 }),
            &StaticType::Int
        );
        assert_eq!(
            tree.get_type(ResolvedVar::Local { slot: 1 }),
            &StaticType::String
        );
    }

    // Simulates: let x = 1; fn outer() { fn inner() { x } }
    // inner needs x, which is 2 function scopes away. hoist_upvalue should
    // create a capture chain: outer captures x as Local(0) from the top-level,
    // then inner captures it as Upvalue(0) from outer.
    #[test]
    fn upvalue_hoisting_across_two_function_scopes() {
        let x_symbol = symbol();
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int), x_symbol);

        tree.new_function_scope(); // outer
        tree.new_function_scope(); // inner

        let resolved = tree.get_binding(x_symbol);
        assert_eq!(resolved, ResolvedVar::Upvalue { slot: 0 });

        // Verify the capture chain was built correctly.
        // inner's upvalue 0 should point to outer's upvalue via Upvalue(0).
        // outer's upvalue 0 should capture the top-level local via Local(0).
        let inner_scope_idx = tree.current_scope_idx;
        let inner_capture = &tree.scopes[inner_scope_idx].upvalues[0];
        assert_eq!(inner_capture.source, CaptureSource::Upvalue(0));

        let outer_scope_idx = tree.scopes[inner_scope_idx]
            .parent_idx
            .expect("inner must have parent");
        let outer_capture = &tree.scopes[outer_scope_idx].upvalues[0];
        assert_eq!(outer_capture.source, CaptureSource::Local(0));
    }

    // Simulates: let a = 1; let b = 2; fn f() { a; b }
    // Both a and b are captured. They should get distinct upvalue indices.
    #[test]
    fn multiple_upvalues_get_distinct_indices() {
        let a_symbol = symbol();
        let b_symbol = symbol();
        let mut tree = empty_scope_tree();
        tree.create_local_binding("a".into(), TypeBinding::Inferred(StaticType::Int), a_symbol);
        tree.create_local_binding(
            "b".into(),
            TypeBinding::Inferred(StaticType::String),
            b_symbol,
        );

        tree.new_function_scope();

        let a = tree.get_binding(a_symbol);
        let b = tree.get_binding(b_symbol);
        assert_eq!(a, ResolvedVar::Upvalue { slot: 0 });
        assert_eq!(b, ResolvedVar::Upvalue { slot: 1 });
    }

    // Resolving the same upvalue twice should return the same index,
    // not create a duplicate entry.
    #[test]
    fn duplicate_upvalue_resolution_reuses_index() {
        let x_symbol = symbol();
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int), x_symbol);

        tree.new_function_scope();

        let first = tree.get_binding(x_symbol);
        let second = tree.get_binding(x_symbol);
        assert_eq!(first, second);
        assert_eq!(first, ResolvedVar::Upvalue { slot: 0 });

        let fn_scope = &tree.scopes[tree.current_scope_idx];
        assert_eq!(fn_scope.upvalues.len(), 1);
    }

    // Simulates: let x = 1; fn outer() { fn inner() { x } }
    // After resolving x in inner (which hoists through outer), we should
    // be able to look up x's type via get_type on the upvalue.
    #[test]
    fn get_type_follows_upvalue_chain() {
        let x_symbol = symbol();
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int), x_symbol);

        tree.new_function_scope(); // outer
        tree.new_function_scope(); // inner

        let resolved = tree.get_binding(x_symbol);
        assert_eq!(tree.get_type(resolved), &StaticType::Int);
    }

    // Simulates: fn outer() { let x = 1; fn middle() { fn inner() { x } } }
    // When inner's upvalue for x is resolved, a sibling function of inner
    // should be able to find x already registered as an upvalue on middle
    // (via find_upvalue_by_symbol), rather than re-walking all the way to outer.
    #[test]
    fn sibling_closure_finds_existing_upvalue() {
        let x_symbol = symbol();
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int), x_symbol);

        tree.new_function_scope(); // middle

        // First closure resolves x, which registers it on middle's upvalue list
        tree.new_function_scope(); // inner1
        let r1 = tree.get_binding(x_symbol);
        assert_eq!(r1, ResolvedVar::Upvalue { slot: 0 });
        tree.destroy_scope(); // back to middle

        // Second closure resolves x — middle already has it as an upvalue,
        // so it should be found via find_upvalue_by_symbol without re-hoisting
        tree.new_function_scope(); // inner2
        let r2 = tree.get_binding(x_symbol);
        assert_eq!(r2, ResolvedVar::Upvalue { slot: 0 });
        tree.destroy_scope(); // back to middle

        // middle should still have exactly one upvalue entry
        let middle_idx = tree.current_scope_idx;
        assert_eq!(tree.scopes[middle_idx].upvalues.len(), 1);
    }

    // Simulates: let x = 1; { let x = 2; } with both bindings still in scope.
    // Each symbol finds its own binding, not just the newest `x`.
    #[test]
    fn lookup_tells_same_named_bindings_apart() {
        let outer = symbol();
        let inner = symbol();
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int), outer);
        tree.new_block_scope();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::String), inner);

        assert_eq!(tree.get_binding(outer), ResolvedVar::Local { slot: 0 });
        assert_eq!(tree.get_binding(inner), ResolvedVar::Local { slot: 1 });
    }

    // Simulates: let x = 1; fn f() { let x = 2; fn g() { x } }
    // with `g` reading both `x`s: each gets its own upvalue in `g`.
    #[test]
    fn same_named_captures_get_their_own_upvalues() {
        let top = symbol();
        let in_f = symbol();
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int), top);
        tree.new_function_scope(); // f
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::String), in_f);
        tree.new_function_scope(); // g

        assert_eq!(tree.get_binding(top), ResolvedVar::Upvalue { slot: 0 });
        assert_eq!(tree.get_binding(in_f), ResolvedVar::Upvalue { slot: 1 });
        assert_eq!(
            tree.get_type(ResolvedVar::Upvalue { slot: 0 }),
            &StaticType::Int
        );
        assert_eq!(
            tree.get_type(ResolvedVar::Upvalue { slot: 1 }),
            &StaticType::String
        );
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

    /// Gives each native in `tree` a fresh symbol, returned in registry order.
    fn native_symbols(tree: &mut ScopeTree) -> Vec<SymbolId> {
        let symbols = tree.native_names().map(|_| symbol()).collect::<Vec<_>>();
        tree.attach_native_symbols(symbols.clone());
        symbols
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

    fn assert_resolves_to(
        tree: &mut ScopeTree,
        candidates: &Candidates,
        sig: &[StaticType],
        expected: ResolvedVar,
    ) {
        let resolved = tree.resolve_call(candidates, sig, CallKind::Regular);
        match resolved.binding {
            Binding::Resolved(Candidate::Scalar(var)) => assert_eq!(var, expected),
            other => panic!("expected Resolved Scalar, got {other:?}"),
        }
    }

    // Two overloads of `locate` registered in the same scope: a more-specific
    // predicate version (takes a function) and a less-specific element version
    // (takes any value). When called with a function value, the predicate
    // version must win even though the element version was registered later.
    #[test]
    fn more_specific_overload_wins_in_same_scope() {
        let seq_any = StaticType::Sequence(Box::new(StaticType::Any));
        let fn_any = variadic_fn(StaticType::Any);
        let mut tree = ScopeTree::from_global_scope(vec![
            (
                "locate".into(),
                fn_type(vec![seq_any.clone(), fn_any.clone()], StaticType::Any),
            ),
            (
                "locate".into(),
                fn_type(vec![seq_any.clone(), StaticType::Any], StaticType::Any),
            ),
        ]);
        let natives = native_symbols(&mut tree);

        let call_sig = [
            StaticType::List(Box::new(StaticType::Int)),
            fn_type(vec![StaticType::Any], StaticType::Bool),
        ];
        // Slot 0 is the predicate version; it must win over slot 1 (element).
        assert_resolves_to(
            &mut tree,
            &candidates(&[&natives]),
            &call_sig,
            ResolvedVar::Global { slot: 0 },
        );
    }

    // Int and Number are siblings. An Int call must select the Int overload
    // without treating the Number overload as a fallback candidate.
    #[test]
    fn more_specific_numeric_overload_wins() {
        let mut tree = ScopeTree::from_global_scope(vec![
            (
                "foo".into(),
                fn_type(vec![StaticType::Number], StaticType::String),
            ),
            (
                "foo".into(),
                fn_type(vec![StaticType::Int], StaticType::String),
            ),
        ]);
        let natives = native_symbols(&mut tree);
        assert_resolves_to(
            &mut tree,
            &candidates(&[&natives]),
            &[StaticType::Int],
            ResolvedVar::Global { slot: 1 },
        );
    }

    // `fn foo(Any)` declared in an inner scope matches every call shape, so
    // it completely shadows any `foo` overload in the parent scope — even a
    // more-specific one. The walk must stop at the first scope with a match.
    #[test]
    fn inner_any_overload_shadows_parent() {
        let mut tree = ScopeTree::from_global_scope(vec![(
            "foo".into(),
            fn_type(vec![StaticType::Int], StaticType::Int),
        )]);
        let natives = native_symbols(&mut tree);

        // Push an inner scope holding `foo(Any) -> Any`.
        tree.new_block_scope();
        let inner = symbol();
        let inner_var = tree.create_local_binding(
            "foo".into(),
            TypeBinding::Inferred(fn_type(vec![StaticType::Any], StaticType::Any)),
            inner,
        );

        // Called with `Int`: inner `foo(Any)` matches and shadows the global.
        assert_resolves_to(
            &mut tree,
            &candidates(&[&[inner], &natives]),
            &[StaticType::Int],
            inner_var,
        );
    }

    // `fn foo(Int)` in an inner scope only matches `Int` calls; calls with a
    // different argument type must fall through to the parent scope's overload.
    #[test]
    fn inner_int_overload_does_not_shadow_other_types() {
        let mut tree = ScopeTree::from_global_scope(vec![(
            "foo".into(),
            fn_type(vec![StaticType::Any], StaticType::Int),
        )]);
        let natives = native_symbols(&mut tree);

        tree.new_block_scope();
        let inner = symbol();
        tree.create_local_binding(
            "foo".into(),
            TypeBinding::Inferred(fn_type(vec![StaticType::Int], StaticType::Int)),
            inner,
        );

        // Called with `String`: inner `foo(Int)` doesn't match, falls through
        // to the global `foo(Any)`.
        assert_resolves_to(
            &mut tree,
            &candidates(&[&[inner], &natives]),
            &[StaticType::String],
            ResolvedVar::Global { slot: 0 },
        );
    }

    // A concrete overload should beat a variadic overload registered in the
    // same scope when both match the call.
    #[test]
    fn concrete_overload_beats_variadic() {
        let mut tree = ScopeTree::from_global_scope(vec![
            ("foo".into(), variadic_fn(StaticType::Any)),
            (
                "foo".into(),
                fn_type(vec![StaticType::Int], StaticType::Int),
            ),
        ]);
        let natives = native_symbols(&mut tree);
        assert_resolves_to(
            &mut tree,
            &candidates(&[&natives]),
            &[StaticType::Int],
            ResolvedVar::Global { slot: 1 },
        );
    }

    // Simulates: fn foo(x: Int) {} { fn foo(x: String) {} fn f() { foo(1) } }
    // The call inspects `foo(String)` on its way to `foo(Int)` but only
    // captures the overload it picks.
    #[test]
    fn call_captures_only_the_overload_it_picks() {
        let mut tree = empty_scope_tree();
        let foo_int = symbol();
        tree.create_local_binding(
            "foo".into(),
            TypeBinding::Inferred(fn_type(vec![StaticType::Int], StaticType::unit())),
            foo_int,
        );
        tree.new_block_scope();
        let foo_string = symbol();
        tree.create_local_binding(
            "foo".into(),
            TypeBinding::Inferred(fn_type(vec![StaticType::String], StaticType::unit())),
            foo_string,
        );
        tree.new_function_scope(); // f

        assert_resolves_to(
            &mut tree,
            &candidates(&[&[foo_string], &[foo_int]]),
            &[StaticType::Int],
            ResolvedVar::Upvalue { slot: 0 },
        );
        assert_eq!(tree.scopes[tree.current_scope_idx].upvalues.len(), 1);
    }
}
