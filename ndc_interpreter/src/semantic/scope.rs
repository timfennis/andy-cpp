use ndc_parser::{Binding, CaptureSource, ResolvedVar, StaticType};
use std::fmt::{Debug, Formatter};

#[derive(Debug, Clone)]
pub(crate) struct Scope {
    parent_idx: Option<usize>,
    creates_environment: bool, // Only true for function scopes and for-loop iterations
    base_offset: usize,
    function_scope_idx: usize,
    identifiers: Vec<(String, StaticType)>,
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

    pub(crate) fn new_iteration_scope(
        parent_idx: Option<usize>,
        base_offset: usize,
        function_scope_idx: usize,
    ) -> Self {
        Self {
            parent_idx,
            creates_environment: true,
            base_offset,
            function_scope_idx,
            identifiers: Vec::default(),
            upvalues: Vec::default(),
        }
    }

    pub(crate) fn find_slot_by_name(&self, find_ident: &str) -> Option<usize> {
        self.identifiers
            .iter()
            .rposition(|(ident, _)| ident == find_ident)
            .map(|idx| idx + self.base_offset)
    }

    fn find_all_slots_by_name(&self, find_ident: &str) -> Vec<usize> {
        self.identifiers
            .iter()
            .enumerate()
            .filter_map(|(slot, (ident, _))| {
                if ident == find_ident {
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
            .filter_map(|(slot, (ident, typ))| {
                if ident != find_ident {
                    return None;
                }

                // If the thing is not a function we're not interested
                let StaticType::Function { parameters, .. } = typ else {
                    return None;
                };

                let Some(param_types) = parameters else {
                    // If this branch happens then the function we're matching against is variadic meaning it's always a match
                    debug_assert!(false, "we should never be calling find_function_candidates if there were variadic matches");
                    // TODO: Change to unreachable?
                    return Some(slot);
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
            .rposition(|(ident, typ)| ident == find_ident && typ.is_fn_and_matches(find_types))
            .map(|idx| idx + self.base_offset)
    }

    fn allocate(&mut self, name: String, typ: StaticType) -> usize {
        self.identifiers.push((name, typ));
        // Slot is just the length of the list minus one
        self.base_offset + self.identifiers.len() - 1
    }

    fn add_upvalue(&mut self, name: &str, source: CaptureSource) -> usize {
        if let Some(idx) = self.upvalues.iter().position(|(n, _)| n == name) {
            return idx;
        }

        self.upvalues.push((name.to_string(), source));
        self.upvalues.len() - 1
    }

    fn find_upvalue(&self, name: &str) -> Option<usize> {
        self.upvalues.iter().position(|(n, _)| n == name)
    }
}

#[derive(Clone)]
pub(crate) struct ScopeTree {
    current_scope_idx: usize,
    global_scope: Scope,
    scopes: Vec<Scope>,
}

impl ScopeTree {
    pub(crate) fn from_global_scope(global_scope_map: Vec<(String, StaticType)>) -> Self {
        let mut global_scope = Scope::new_function_scope(None, 0);
        global_scope.identifiers = global_scope_map;

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
                            return self.find_type_by_slot(
                                self.get_parent_function_scope_idx(scope_idx),
                                *slot,
                            );
                        }
                        CaptureSource::Upvalue(slot) => {
                            scope_idx = self.get_parent_function_scope_idx(scope_idx);
                            check_slot = *slot;
                        }
                    }
                }
            }
            ResolvedVar::Global { slot } => &self.global_scope.identifiers[slot].1,
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
                return &scope.identifiers[slot - scope.base_offset].1;
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
            self.scopes[old_scope_idx].offset(), // todo: @claude is this correct
            self.scopes.len(),
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
                if env_scopes.is_empty() {
                    return Some(ResolvedVar::Local { slot });
                }

                let slot = self.hoist_upvalue(ident, slot, &env_scopes);
                return Some(ResolvedVar::Upvalue { slot });
            }

            if let Some(slot) = self.scopes[scope_ptr].find_upvalue(ident) {
                if env_scopes.is_empty() {
                    return Some(ResolvedVar::Upvalue { slot });
                }

                let slot = self.hoist_from_upvalue(ident, slot, &env_scopes);
                return Some(ResolvedVar::Upvalue { slot });
            }

            // We couldn't find this ident in the local scope, or as an upvalue in the current scope
            // in this case we just recurse.
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

    pub(crate) fn resolve_function_dynamic(
        &mut self,
        ident: &str,
        sig: &[StaticType],
    ) -> Vec<ResolvedVar> {
        let mut scope_ptr = self.current_scope_idx;
        let mut env_scopes: Vec<usize> = Vec::default();

        loop {
            let candidates = self.scopes[scope_ptr].find_function_candidates(ident, sig);
            if !candidates.is_empty() {
                return candidates
                    .into_iter()
                    .map(|slot| {
                        if env_scopes.is_empty() {
                            return ResolvedVar::Local { slot };
                        }

                        let slot = self.hoist_upvalue(ident, slot, &env_scopes);
                        ResolvedVar::Upvalue { slot }
                    })
                    .collect();
            } else if let Some(slot) = self.scopes[scope_ptr].find_upvalue(ident) {
                if env_scopes.is_empty() {
                    return vec![ResolvedVar::Upvalue { slot }];
                }

                let slot = self.hoist_from_upvalue(ident, slot, &env_scopes);

                return vec![ResolvedVar::Upvalue { slot }];
            } else if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                if self.scopes[scope_ptr].creates_environment {
                    env_scopes.push(scope_ptr);
                }

                scope_ptr = parent_idx;
            } else {
                return self
                    .global_scope
                    .find_function_candidates(ident, sig)
                    .into_iter()
                    .map(|slot| ResolvedVar::Global { slot })
                    .collect();
            }
        }
    }

    pub(crate) fn resolve_function_binding(&mut self, ident: &str, sig: &[StaticType]) -> Binding {
        self.resolve_function(ident, sig)
            .map(Binding::Resolved)
            .or_else(|| {
                let loose_bindings = self.resolve_function_dynamic(ident, sig);

                if loose_bindings.is_empty() {
                    return None;
                }

                Some(Binding::Dynamic(loose_bindings))
            })
            // If we can't find any function in scope that could match, fall back to all same-named
            // bindings so runtime dynamic dispatch (including vectorization) can pick the right one.
            .or_else(|| {
                let all_bindings = self.get_all_bindings_by_name(ident);
                if all_bindings.is_empty() {
                    return None;
                }
                Some(Binding::Dynamic(all_bindings))
            })
            .unwrap_or(Binding::None)
    }

    pub(crate) fn get_all_bindings_by_name(&mut self, ident: &str) -> Vec<ResolvedVar> {
        let mut results = Vec::new();
        let mut scope_ptr = self.current_scope_idx;
        let mut env_scopes: Vec<usize> = Vec::default();

        loop {
            let slots = self.scopes[scope_ptr].find_all_slots_by_name(ident);

            results.extend(slots.into_iter().map(|slot| {
                if env_scopes.is_empty() {
                    ResolvedVar::Local { slot }
                } else {
                    let slot = self.hoist_upvalue(ident, slot, &env_scopes);
                    ResolvedVar::Upvalue { slot }
                }
            }));

            if let Some(slot) = self.scopes[scope_ptr].find_upvalue(ident) {
                if env_scopes.is_empty() {
                    results.push(ResolvedVar::Upvalue { slot });
                } else {
                    let slot = self.hoist_from_upvalue(ident, slot, &env_scopes);
                    results.push(ResolvedVar::Upvalue { slot });
                }
            }

            if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                if self.scopes[scope_ptr].creates_environment {
                    env_scopes.push(scope_ptr);
                }

                scope_ptr = parent_idx;
            } else {
                let global_slots = self.global_scope.find_all_slots_by_name(ident);
                results.extend(
                    global_slots
                        .into_iter()
                        .map(|slot| ResolvedVar::Global { slot }),
                );
                break;
            }
        }

        results
    }

    pub(crate) fn resolve_function(
        &mut self,
        ident: &str,
        arg_types: &[StaticType],
    ) -> Option<ResolvedVar> {
        let mut scope_ptr = self.current_scope_idx;
        let mut env_scopes: Vec<usize> = Vec::default();

        loop {
            if let Some(slot) = self.scopes[scope_ptr].find_function(ident, arg_types) {
                if env_scopes.is_empty() {
                    return Some(ResolvedVar::Local { slot });
                }

                let slot = self.hoist_upvalue(ident, slot, &env_scopes);

                return Some(ResolvedVar::Upvalue { slot });
            } else if let Some(slot) = self.scopes[scope_ptr].find_upvalue(ident) {
                if env_scopes.is_empty() {
                    return Some(ResolvedVar::Upvalue { slot });
                }

                let slot = self.hoist_from_upvalue(ident, slot, &env_scopes);
                return Some(ResolvedVar::Upvalue { slot });
            } else if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                if self.scopes[scope_ptr].creates_environment {
                    env_scopes.push(scope_ptr);
                }
                scope_ptr = parent_idx;
            } else {
                return Some(ResolvedVar::Global {
                    slot: self.global_scope.find_function(ident, arg_types)?,
                });
            }
        }
    }

    pub(crate) fn create_local_binding(&mut self, ident: String, typ: StaticType) -> ResolvedVar {
        ResolvedVar::Local {
            slot: self.scopes[self.current_scope_idx].allocate(ident, typ),
        }
    }

    pub(crate) fn update_binding_type(&mut self, var: ResolvedVar, new_type: StaticType) {
        match var {
            ResolvedVar::Local { slot } => {
                let scope_idx = self.find_scope_owning_slot(self.current_scope_idx, slot);
                let base = self.scopes[scope_idx].base_offset;
                self.scopes[scope_idx].identifiers[slot - base].1 = new_type;
            }
            ResolvedVar::Upvalue { slot } => {
                let mut scope_idx = self.scopes[self.current_scope_idx].function_scope_idx;
                let mut check_slot = slot;

                loop {
                    let (_, source) = self.scopes[scope_idx].upvalues[check_slot].clone();
                    let parent_fn = self.get_parent_function_scope_idx(scope_idx);

                    match source {
                        CaptureSource::Local(local_slot) => {
                            let owning = self.find_scope_owning_slot(parent_fn, local_slot);
                            let base = self.scopes[owning].base_offset;
                            self.scopes[owning].identifiers[local_slot - base].1 = new_type;
                            return;
                        }
                        CaptureSource::Upvalue(uv_slot) => {
                            scope_idx = parent_fn;
                            check_slot = uv_slot;
                        }
                    }
                }
            }
            ResolvedVar::Global { .. } => {
                panic!("update_binding_type called with a global binding")
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
        let var = tree.create_local_binding("x".into(), StaticType::Int);
        assert_eq!(var, ResolvedVar::Local { slot: 0 });
        assert_eq!(
            tree.get_binding_any("x"),
            Some(ResolvedVar::Local { slot: 0 })
        );
    }

    #[test]
    fn multiple_locals_get_ascending_slots() {
        let mut tree = empty_scope_tree();
        let x = tree.create_local_binding("x".into(), StaticType::Int);
        let y = tree.create_local_binding("y".into(), StaticType::Int);
        let z = tree.create_local_binding("z".into(), StaticType::Int);
        assert_eq!(x, ResolvedVar::Local { slot: 0 });
        assert_eq!(y, ResolvedVar::Local { slot: 1 });
        assert_eq!(z, ResolvedVar::Local { slot: 2 });
    }

    #[test]
    fn block_scope_continues_flat_numbering() {
        let mut tree = empty_scope_tree();
        let x = tree.create_local_binding("x".into(), StaticType::Int);
        assert_eq!(x, ResolvedVar::Local { slot: 0 });

        tree.new_block_scope();
        let y = tree.create_local_binding("y".into(), StaticType::Int);
        assert_eq!(y, ResolvedVar::Local { slot: 1 });

        assert_eq!(
            tree.get_binding_any("x"),
            Some(ResolvedVar::Local { slot: 0 })
        );
    }

    #[test]
    fn nested_block_scopes_continue_numbering() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("a".into(), StaticType::Int);

        tree.new_block_scope();
        let b = tree.create_local_binding("b".into(), StaticType::Int);
        assert_eq!(b, ResolvedVar::Local { slot: 1 });

        tree.new_block_scope();
        let c = tree.create_local_binding("c".into(), StaticType::Int);
        assert_eq!(c, ResolvedVar::Local { slot: 2 });
    }

    #[test]
    fn block_scope_does_not_create_upvalue() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), StaticType::Int);

        tree.new_block_scope();
        assert_eq!(
            tree.get_binding_any("x"),
            Some(ResolvedVar::Local { slot: 0 })
        );
    }

    #[test]
    fn function_scope_resets_slots_and_captures_as_upvalue() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), StaticType::Int);

        tree.new_function_scope();
        let y = tree.create_local_binding("y".into(), StaticType::Int);
        assert_eq!(y, ResolvedVar::Local { slot: 0 });

        assert_eq!(
            tree.get_binding_any("x"),
            Some(ResolvedVar::Upvalue { slot: 0 })
        );
    }

    #[test]
    fn iteration_scope_continues_numbering_and_captures_as_upvalue() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), StaticType::Int);

        tree.new_iteration_scope();
        let i = tree.create_local_binding("i".into(), StaticType::Int);
        assert_eq!(i, ResolvedVar::Local { slot: 1 });

        assert_eq!(
            tree.get_binding_any("x"),
            Some(ResolvedVar::Upvalue { slot: 0 })
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
        tree.create_local_binding("a".into(), StaticType::Int);

        tree.new_block_scope();
        tree.create_local_binding("b".into(), StaticType::Int);
        tree.destroy_scope();

        let c = tree.create_local_binding("c".into(), StaticType::Int);
        assert_eq!(c, ResolvedVar::Local { slot: 1 });
    }

    #[test]
    fn get_type_returns_correct_type() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), StaticType::Int);
        tree.create_local_binding("y".into(), StaticType::String);

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
        tree.create_local_binding("x".into(), StaticType::Int);

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
        tree.create_local_binding("a".into(), StaticType::Int);
        tree.create_local_binding("b".into(), StaticType::String);

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
        tree.create_local_binding("x".into(), StaticType::Int);

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
        tree.create_local_binding("x".into(), StaticType::Int);

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
        tree.create_local_binding("x".into(), StaticType::Int);

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
