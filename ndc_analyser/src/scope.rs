use ndc_core::StaticType;
use ndc_parser::{Binding, Candidate, CaptureSource, ResolvedVar};
use std::fmt::{Debug, Formatter};

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
        positions: Vec<Vec<ResolvedVar>>,
    },
    AnyFallback(Vec<ResolvedVar>),
}

/// Output of a single scalar overload walk across the scope chain.
struct ScalarWalk {
    /// First exact-subtype match found; short-circuits the walk.
    exact: Option<ResolvedVar>,
    /// First-scope-wins loose-compatibility candidates.
    loose: Option<Vec<ResolvedVar>>,
    /// Every same-name callable across the whole chain, for runtime-narrowing
    /// fallback when the callee's static type is `Any`.
    all_by_name: Vec<ResolvedVar>,
}

/// If every per-position candidate list contains exactly one entry and they
/// all point to the same scalar overload, return it. This is the only case
/// where `Binding::Resolved(Candidate::Vec)` is safe to emit: a single scalar
/// fires for every element pair.
fn unique_scalar(positions: &[Vec<ResolvedVar>]) -> Option<ResolvedVar> {
    let first = positions.first()?.first().copied()?;
    for pos in positions {
        if pos.len() != 1 || pos[0] != first {
            return None;
        }
    }
    Some(first)
}

/// Extend `out` with the result of mapping `vars` through `wrap`, skipping any
/// `Candidate` whose underlying `ResolvedVar` is already present in `out`.
/// Preserves first-seen order — relevant because the runtime probes
/// candidates in list order and stops at the first match.
fn extend_dedup(
    out: &mut Vec<Candidate>,
    vars: impl IntoIterator<Item = ResolvedVar>,
    wrap: fn(ResolvedVar) -> Candidate,
) {
    for v in vars {
        if !out.iter().any(|c| c.var() == v) {
            out.push(wrap(v));
        }
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

#[derive(Debug, Clone)]
pub(crate) struct ScopeBinding {
    pub name: String,
    pub binding: TypeBinding,
}

#[derive(Debug, Clone)]
pub(crate) struct Scope {
    parent_idx: Option<usize>,
    creates_environment: bool, // Only true for function scopes and for-loop iterations
    base_offset: usize,
    function_scope_idx: usize,
    identifiers: Vec<ScopeBinding>,
    upvalues: Vec<(String, CaptureSource)>,
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

    pub(crate) fn find_slot_by_name(&self, find_ident: &str) -> Option<usize> {
        self.identifiers
            .iter()
            .rposition(|b| b.name == find_ident)
            .map(|idx| idx + self.base_offset)
    }

    /// Returns slots for all bindings with the given name whose type could be
    /// callable at runtime (`Function` or `Any`). Bindings with concrete
    /// non-function types (e.g. `Int`, `String`) are excluded so they never
    /// pollute the dynamic overload set used for function resolution.
    fn find_all_callable_slots_by_name(&self, find_ident: &str) -> Vec<usize> {
        self.identifiers
            .iter()
            .enumerate()
            .filter_map(|(slot, b)| {
                if b.name == find_ident && b.binding.typ().could_be_callable() {
                    Some(slot + self.base_offset)
                } else {
                    None
                }
            })
            .collect()
    }

    fn find_function_candidates(&self, find_ident: &str, find_types: &[StaticType]) -> Vec<usize> {
        self.identifiers.iter()
            .enumerate()
            .rev()
            .filter_map(|(slot, b)| {
                if b.name != find_ident {
                    return None;
                }

                // If the thing is not a function we're not interested
                let StaticType::Function { parameters, .. } = b.binding.typ() else {
                    return None;
                };

                let Some(param_types) = parameters else {
                    unreachable!("find_function_candidates should never be called when there are variadic matches");
                };

                let is_good = param_types.len() == find_types.len()
                    && param_types.iter().zip(find_types.iter()).all(|(typ_1, typ_2)| !typ_1.is_incompatible_with(typ_2));

                is_good.then_some(slot)
            })
            .map(|idx| idx + self.base_offset)
            .collect()
    }
    fn find_function(&self, find_ident: &str, find_types: &[StaticType]) -> Option<usize> {
        self.identifiers
            .iter()
            .rposition(|b| b.name == find_ident && b.binding.typ().is_fn_and_matches(find_types))
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

    fn allocate(&mut self, name: String, type_binding: TypeBinding) -> usize {
        self.identifiers.push(ScopeBinding {
            name,
            binding: type_binding,
        });
        self.base_offset + self.identifiers.len() - 1
    }

    fn add_upvalue(&mut self, name: &str, source: CaptureSource) -> usize {
        // Deduplicate by name AND source so that multiple overloads of the same
        // function name can each have their own upvalue entry.
        if let Some(idx) = self
            .upvalues
            .iter()
            .position(|(n, s)| n == name && *s == source)
        {
            return idx;
        }

        self.upvalues.push((name.to_string(), source));
        self.upvalues.len() - 1
    }

    fn find_upvalue(&self, name: &str) -> Option<usize> {
        self.upvalues.iter().position(|(n, _)| n == name)
    }

    fn find_upvalues_by_name(&self, name: &str) -> Vec<usize> {
        self.upvalues
            .iter()
            .enumerate()
            .filter_map(|(idx, (n, _))| if n == name { Some(idx) } else { None })
            .collect()
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
            })
            .collect();

        Self {
            current_scope_idx: 0,
            global_scope,
            scopes: vec![Scope::new_function_scope(None, 0)],
        }
    }

    pub(crate) fn get_type(&self, res: ResolvedVar) -> &StaticType {
        match res {
            ResolvedVar::Local { slot } => self.find_type_by_slot(self.current_scope_idx, slot),
            ResolvedVar::Upvalue { slot } => {
                let mut scope_idx = self.scopes[self.current_scope_idx].function_scope_idx;
                let mut check_slot = slot;

                loop {
                    let (_, source) = &self.scopes[scope_idx].upvalues[check_slot];

                    match source {
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
            .map(|(_, source)| source.clone())
            .collect()
    }

    // When the Analyser encounters an identifier as the rhs of an expression during resolution it
    // will use this method to lookup if that identifier has already been seen.
    pub(crate) fn get_binding_any(&mut self, ident: &str) -> Option<ResolvedVar> {
        let mut scope_ptr = self.current_scope_idx;
        let mut env_scopes: Vec<usize> = Vec::default();

        loop {
            if let Some(slot) = self.scopes[scope_ptr].find_slot_by_name(ident) {
                return Some(self.resolve_found_local(ident, slot, &env_scopes));
            }
            if let Some(slot) = self.scopes[scope_ptr].find_upvalue(ident) {
                return Some(self.resolve_found_upvalue(ident, slot, &env_scopes));
            }

            if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                if self.scopes[scope_ptr].creates_environment {
                    env_scopes.push(scope_ptr);
                }
                scope_ptr = parent_idx;
            } else {
                return Some(ResolvedVar::Global {
                    slot: self.global_scope.find_slot_by_name(ident)?,
                });
            }
        }
    }

    /// Resolve a function call to a binding and an inferred return type.
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
        ident: &str,
        sig: &[StaticType],
        kind: CallKind,
    ) -> ResolvedCall {
        let walk = self.scalar_walk(ident, sig);

        // 1. Scalar exact match.
        if let Some(exact) = walk.exact {
            let return_type = self.scalar_return_type(exact);
            return ResolvedCall {
                binding: Binding::Resolved(Candidate::Scalar(exact)),
                return_type,
            };
        }

        // 2. Per-position vec resolution, but only for operator-form calls.
        let vec_resolution = match kind {
            CallKind::Operator => self.resolve_vec(ident, sig),
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
        let has_vec_candidates = vec_resolution.is_some();
        if !scalar_loose.is_empty() || has_vec_candidates {
            let return_type = self.dynamic_return_type(&scalar_loose, vec_resolution.as_ref());
            let mut combined: Vec<Candidate> = scalar_loose
                .iter()
                .copied()
                .map(Candidate::Scalar)
                .collect();
            match vec_resolution {
                Some(VecResolution::Static { positions, .. }) => {
                    extend_dedup(&mut combined, positions.into_iter().flatten(), |v| {
                        Candidate::Vec(v)
                    });
                }
                Some(VecResolution::AnyFallback(vars)) => {
                    extend_dedup(&mut combined, vars, Candidate::Vec);
                }
                None => {}
            }
            return ResolvedCall {
                binding: Binding::Dynamic(combined),
                return_type,
            };
        }

        // 4. Last-resort same-name callables (for Any-typed callees, upvalues, …).
        if !walk.all_by_name.is_empty() {
            return ResolvedCall {
                binding: Binding::Dynamic(
                    walk.all_by_name
                        .into_iter()
                        .map(Candidate::Scalar)
                        .collect(),
                ),
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
    /// `all_by_name` accumulates every same-name callable across the chain for
    /// the runtime-narrowing fallback.
    fn scalar_walk(&mut self, ident: &str, sig: &[StaticType]) -> ScalarWalk {
        let mut scope_ptr = self.current_scope_idx;
        let mut env_scopes: Vec<usize> = Vec::default();
        let mut loose: Option<Vec<ResolvedVar>> = None;
        let mut all_by_name: Vec<ResolvedVar> = Vec::new();

        loop {
            if let Some(slot) = self.scopes[scope_ptr].find_function(ident, sig) {
                return ScalarWalk {
                    exact: Some(self.resolve_found_local(ident, slot, &env_scopes)),
                    loose: None,
                    all_by_name: Vec::new(),
                };
            }

            for uv_slot in self.scopes[scope_ptr].find_upvalues_by_name(ident) {
                all_by_name.push(self.resolve_found_upvalue(ident, uv_slot, &env_scopes));
            }

            if loose.is_none() {
                let candidates = self.scopes[scope_ptr].find_function_candidates(ident, sig);
                if !candidates.is_empty() {
                    loose = Some(
                        candidates
                            .into_iter()
                            .map(|slot| self.resolve_found_local(ident, slot, &env_scopes))
                            .collect(),
                    );
                }
            }

            let slots = self.scopes[scope_ptr].find_all_callable_slots_by_name(ident);
            all_by_name.extend(
                slots
                    .into_iter()
                    .map(|slot| self.resolve_found_local(ident, slot, &env_scopes)),
            );

            if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                if self.scopes[scope_ptr].creates_environment {
                    env_scopes.push(scope_ptr);
                }
                scope_ptr = parent_idx;
            } else {
                if let Some(slot) = self.global_scope.find_function(ident, sig) {
                    return ScalarWalk {
                        exact: Some(ResolvedVar::Global { slot }),
                        loose: None,
                        all_by_name: Vec::new(),
                    };
                }
                if loose.is_none() {
                    let candidates = self.global_scope.find_function_candidates(ident, sig);
                    if !candidates.is_empty() {
                        loose = Some(
                            candidates
                                .into_iter()
                                .map(|slot| ResolvedVar::Global { slot })
                                .collect(),
                        );
                    }
                }
                all_by_name.extend(
                    self.global_scope
                        .find_all_callable_slots_by_name(ident)
                        .into_iter()
                        .map(|slot| ResolvedVar::Global { slot }),
                );
                break;
            }
        }

        ScalarWalk {
            exact: None,
            loose,
            all_by_name,
        }
    }

    /// Try to resolve a vec candidate set for `(name, sig)`.
    ///
    /// * `Static` — at least one arg is statically a non-empty tuple and
    ///   every tuple-shaped arg shares that length. Positions *may* contain
    ///   empty candidate lists; the caller surfaces that as a call error.
    /// * `AnyFallback` — no static tuple shape but at least one arg is `Any`,
    ///   so vec might still apply at runtime; the runtime narrows per pair.
    /// * `None` — no possible vec interpretation.
    fn resolve_vec(&mut self, ident: &str, sig: &[StaticType]) -> Option<VecResolution> {
        if let Some((axis_len, positions)) = self.resolve_vec_static(ident, sig) {
            return Some(VecResolution::Static {
                axis_len,
                positions,
            });
        }
        if !sig.iter().any(|t| matches!(t, StaticType::Any)) {
            return None;
        }
        let permissive: Vec<StaticType> = vec![StaticType::Any; sig.len()];
        let vars = self.candidates_for_sig(ident, &permissive);
        if vars.is_empty() {
            None
        } else {
            Some(VecResolution::AnyFallback(vars))
        }
    }

    fn resolve_vec_static(
        &mut self,
        ident: &str,
        sig: &[StaticType],
    ) -> Option<(usize, Vec<Vec<ResolvedVar>>)> {
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
            positions.push(self.candidates_for_sig(ident, &pos_sig));
        }
        Some((axis_len, positions))
    }

    /// Priority-ordered scalar candidates for one signature, used by per-position
    /// vec resolution. Exact match (if any) first, then loose-compat candidates
    /// with the exact entry deduplicated.
    fn candidates_for_sig(&mut self, ident: &str, sig: &[StaticType]) -> Vec<ResolvedVar> {
        let walk = self.scalar_walk(ident, sig);
        let mut out: Vec<ResolvedVar> = Vec::new();
        if let Some(e) = walk.exact {
            out.push(e);
        }
        if let Some(loose) = walk.loose {
            for v in loose {
                if !out.contains(&v) {
                    out.push(v);
                }
            }
        }
        out
    }

    /// Return type of a scalar candidate. Falls back to `Any` if the binding
    /// turned out not to carry a function type (e.g. `Any`-typed callee).
    fn scalar_return_type(&self, var: ResolvedVar) -> StaticType {
        match self.get_type(var) {
            StaticType::Function { return_type, .. } => return_type.as_ref().clone(),
            _ => StaticType::Any,
        }
    }

    /// LUB the return types of a list of scalar candidates. Used when every
    /// candidate in a Dynamic binding came from the compat-filtered walk and
    /// is therefore guaranteed to be a function.
    fn lub_scalar_returns(&self, vars: &[ResolvedVar]) -> StaticType {
        vars.iter()
            .map(|v| self.scalar_return_type(*v))
            .reduce(|a, b| a.lub(&b))
            .unwrap_or(StaticType::Any)
    }

    /// Return type for a `Binding::Dynamic`. See [`ResolvedCall`] for the
    /// case breakdown.
    fn dynamic_return_type(
        &self,
        scalar_loose: &[ResolvedVar],
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

    pub(crate) fn create_local_binding(
        &mut self,
        ident: String,
        binding: TypeBinding,
    ) -> ResolvedVar {
        ResolvedVar::Local {
            slot: self.scopes[self.current_scope_idx].allocate(ident, binding),
        }
    }

    /// Check whether the current scope already has a `fn` declaration with
    /// the given name and arity. Used to detect illegal same-scope redefinitions.
    pub(crate) fn has_function_in_current_scope(&self, name: &str, arity: Option<usize>) -> bool {
        self.scopes[self.current_scope_idx].has_function_with_arity(name, arity)
    }

    /// Reserve a slot in the current scope without creating a named binding.
    /// Used to allocate the list/map accumulator before analysing the body of a
    /// for-comprehension, so that any nested comprehensions receive strictly
    /// higher slot numbers and cannot collide with this accumulator.
    ///
    /// Uses `"\x00"` as a sentinel name that can never collide with user identifiers
    /// since the lexer never produces null bytes.
    pub(crate) fn reserve_anonymous_slot(&mut self) -> usize {
        self.scopes[self.current_scope_idx]
            .allocate("\x00".to_string(), TypeBinding::Inferred(StaticType::Any))
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
                    let (_, source) = self.scopes[scope_idx].upvalues[check_slot].clone();

                    match source {
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

    /// Given a local slot found during a scope walk, return the appropriate `ResolvedVar`.
    /// If `env_scopes` is empty the slot is in the current function scope and can be
    /// referenced directly as a `Local`. Otherwise, it must be hoisted through intervening
    /// function scopes as an upvalue chain.
    fn resolve_found_local(
        &mut self,
        ident: &str,
        slot: usize,
        env_scopes: &[usize],
    ) -> ResolvedVar {
        if env_scopes.is_empty() {
            ResolvedVar::Local { slot }
        } else {
            let slot = self.hoist_upvalue(ident, slot, env_scopes);
            ResolvedVar::Upvalue { slot }
        }
    }

    /// Given an upvalue slot found during a scope walk, return the appropriate `ResolvedVar`.
    /// If `env_scopes` is empty the upvalue belongs to the current function scope. Otherwise
    /// it must be hoisted further through intervening function scopes.
    fn resolve_found_upvalue(
        &mut self,
        ident: &str,
        slot: usize,
        env_scopes: &[usize],
    ) -> ResolvedVar {
        if env_scopes.is_empty() {
            ResolvedVar::Upvalue { slot }
        } else {
            let slot = self.hoist_from_upvalue(ident, slot, env_scopes);
            ResolvedVar::Upvalue { slot }
        }
    }

    // In a situation where the analyser is recursing through the scope tree and finds an identifier
    // in the current local scope, if we were searching from a nested scope we now have to 'hoist'
    // this local value as an upvalue in all the nested scopes. This function is responsible for adding
    // this value as an upvalue to all the nested scopes.
    //
    // `env_scopes`: is a list of scopes that the analyser has already searched that will need to get this upvalue.
    pub(crate) fn hoist_upvalue(
        &mut self,
        name: &str,
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
            capture_idx = self.scopes[scope_idx].add_upvalue(name, source);
            is_local = false; // only the first iteration is local
        }

        capture_idx
    }

    pub(crate) fn hoist_from_upvalue(
        &mut self,
        name: &str,
        upvalue_slot: usize,
        env_scopes: &[usize],
    ) -> usize {
        let mut capture_idx = upvalue_slot;
        for &scope_idx in env_scopes.iter().rev() {
            capture_idx =
                self.scopes[scope_idx].add_upvalue(name, CaptureSource::Upvalue(capture_idx));
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
    use ndc_parser::ResolvedVar;

    fn empty_scope_tree() -> ScopeTree {
        ScopeTree::from_global_scope(vec![])
    }

    #[test]
    fn single_local_in_function_scope() {
        let mut tree = empty_scope_tree();
        let var = tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int));
        assert_eq!(var, ResolvedVar::Local { slot: 0 });
        assert_eq!(
            tree.get_binding_any("x"),
            Some(ResolvedVar::Local { slot: 0 })
        );
    }

    #[test]
    fn multiple_locals_get_ascending_slots() {
        let mut tree = empty_scope_tree();
        let x = tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int));
        let y = tree.create_local_binding("y".into(), TypeBinding::Inferred(StaticType::Int));
        let z = tree.create_local_binding("z".into(), TypeBinding::Inferred(StaticType::Int));
        assert_eq!(x, ResolvedVar::Local { slot: 0 });
        assert_eq!(y, ResolvedVar::Local { slot: 1 });
        assert_eq!(z, ResolvedVar::Local { slot: 2 });
    }

    #[test]
    fn block_scope_continues_flat_numbering() {
        let mut tree = empty_scope_tree();
        let x = tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int));
        assert_eq!(x, ResolvedVar::Local { slot: 0 });

        tree.new_block_scope();
        let y = tree.create_local_binding("y".into(), TypeBinding::Inferred(StaticType::Int));
        assert_eq!(y, ResolvedVar::Local { slot: 1 });

        assert_eq!(
            tree.get_binding_any("x"),
            Some(ResolvedVar::Local { slot: 0 })
        );
    }

    #[test]
    fn nested_block_scopes_continue_numbering() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("a".into(), TypeBinding::Inferred(StaticType::Int));

        tree.new_block_scope();
        let b = tree.create_local_binding("b".into(), TypeBinding::Inferred(StaticType::Int));
        assert_eq!(b, ResolvedVar::Local { slot: 1 });

        tree.new_block_scope();
        let c = tree.create_local_binding("c".into(), TypeBinding::Inferred(StaticType::Int));
        assert_eq!(c, ResolvedVar::Local { slot: 2 });
    }

    #[test]
    fn block_scope_does_not_create_upvalue() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int));

        tree.new_block_scope();
        assert_eq!(
            tree.get_binding_any("x"),
            Some(ResolvedVar::Local { slot: 0 })
        );
    }

    #[test]
    fn function_scope_resets_slots_and_captures_as_upvalue() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int));

        tree.new_function_scope();
        let y = tree.create_local_binding("y".into(), TypeBinding::Inferred(StaticType::Int));
        assert_eq!(y, ResolvedVar::Local { slot: 0 });

        assert_eq!(
            tree.get_binding_any("x"),
            Some(ResolvedVar::Upvalue { slot: 0 })
        );
    }

    #[test]
    fn iteration_scope_continues_numbering_and_is_transparent() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int));

        tree.new_iteration_scope();
        let i = tree.create_local_binding("i".into(), TypeBinding::Inferred(StaticType::Int));
        assert_eq!(i, ResolvedVar::Local { slot: 1 });

        assert_eq!(
            tree.get_binding_any("x"),
            Some(ResolvedVar::Local { slot: 0 })
        );
    }

    #[test]
    fn global_lookup() {
        let tree = ScopeTree::from_global_scope(vec![(
            "print".into(),
            StaticType::Function {
                parameters: None,
                return_type: Box::new(StaticType::Any),
            },
        )]);
        let mut tree = tree;
        assert_eq!(
            tree.get_binding_any("print"),
            Some(ResolvedVar::Global { slot: 0 })
        );
    }

    #[test]
    fn slot_reuse_after_scope_destroy() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("a".into(), TypeBinding::Inferred(StaticType::Int));

        tree.new_block_scope();
        tree.create_local_binding("b".into(), TypeBinding::Inferred(StaticType::Int));
        tree.destroy_scope();

        let c = tree.create_local_binding("c".into(), TypeBinding::Inferred(StaticType::Int));
        assert_eq!(c, ResolvedVar::Local { slot: 1 });
    }

    #[test]
    fn get_type_returns_correct_type() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int));
        tree.create_local_binding("y".into(), TypeBinding::Inferred(StaticType::String));

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
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int));

        tree.new_function_scope(); // outer
        tree.new_function_scope(); // inner

        let resolved = tree.get_binding_any("x");
        assert_eq!(resolved, Some(ResolvedVar::Upvalue { slot: 0 }));

        // Verify the capture chain was built correctly.
        // inner's upvalue 0 should point to outer's upvalue via Upvalue(0).
        // outer's upvalue 0 should capture the top-level local via Local(0).
        let inner_scope_idx = tree.current_scope_idx;
        let inner_capture = &tree.scopes[inner_scope_idx].upvalues[0];
        assert_eq!(inner_capture.1, CaptureSource::Upvalue(0));

        let outer_scope_idx = tree.scopes[inner_scope_idx]
            .parent_idx
            .expect("inner must have parent");
        let outer_capture = &tree.scopes[outer_scope_idx].upvalues[0];
        assert_eq!(outer_capture.1, CaptureSource::Local(0));
    }

    // Simulates: let a = 1; let b = 2; fn f() { a; b }
    // Both a and b are captured. They should get distinct upvalue indices.
    #[test]
    fn multiple_upvalues_get_distinct_indices() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("a".into(), TypeBinding::Inferred(StaticType::Int));
        tree.create_local_binding("b".into(), TypeBinding::Inferred(StaticType::String));

        tree.new_function_scope();

        let a = tree.get_binding_any("a");
        let b = tree.get_binding_any("b");
        assert_eq!(a, Some(ResolvedVar::Upvalue { slot: 0 }));
        assert_eq!(b, Some(ResolvedVar::Upvalue { slot: 1 }));
    }

    // Resolving the same upvalue twice should return the same index,
    // not create a duplicate entry.
    #[test]
    fn duplicate_upvalue_resolution_reuses_index() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int));

        tree.new_function_scope();

        let first = tree.get_binding_any("x");
        let second = tree.get_binding_any("x");
        assert_eq!(first, second);
        assert_eq!(first, Some(ResolvedVar::Upvalue { slot: 0 }));

        let fn_scope = &tree.scopes[tree.current_scope_idx];
        assert_eq!(fn_scope.upvalues.len(), 1);
    }

    // Simulates: let x = 1; fn outer() { fn inner() { x } }
    // After resolving x in inner (which hoists through outer), we should
    // be able to look up x's type via get_type on the upvalue.
    #[test]
    fn get_type_follows_upvalue_chain() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int));

        tree.new_function_scope(); // outer
        tree.new_function_scope(); // inner

        let resolved = tree.get_binding_any("x").unwrap();
        assert_eq!(tree.get_type(resolved), &StaticType::Int);
    }

    // Simulates: fn outer() { let x = 1; fn middle() { fn inner() { x } } }
    // When inner's upvalue for x is resolved, a sibling function of inner
    // should be able to find x already registered as an upvalue on middle
    // (via find_upvalue), rather than re-walking all the way to outer.
    #[test]
    fn sibling_closure_finds_existing_upvalue() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), TypeBinding::Inferred(StaticType::Int));

        tree.new_function_scope(); // middle

        // First closure resolves x, which registers it on middle's upvalue list
        tree.new_function_scope(); // inner1
        let r1 = tree.get_binding_any("x");
        assert_eq!(r1, Some(ResolvedVar::Upvalue { slot: 0 }));
        tree.destroy_scope(); // back to middle

        // Second closure resolves x — middle already has it as an upvalue,
        // so it should be found via find_upvalue without re-hoisting
        tree.new_function_scope(); // inner2
        let r2 = tree.get_binding_any("x");
        assert_eq!(r2, Some(ResolvedVar::Upvalue { slot: 0 }));
        tree.destroy_scope(); // back to middle

        // middle should still have exactly one upvalue entry
        let middle_idx = tree.current_scope_idx;
        assert_eq!(tree.scopes[middle_idx].upvalues.len(), 1);
    }
}
