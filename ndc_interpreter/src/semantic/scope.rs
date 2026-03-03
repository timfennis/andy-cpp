use ndc_parser::{Binding, ResolvedVar, StaticType};
use std::fmt::{Debug, Formatter};

#[derive(Debug, Clone)]
pub(crate) struct Scope {
    parent_idx: Option<usize>,
    creates_environment: bool, // Only true for function scopes and for-loop iterations
    base_offset: usize,
    function_scope_idx: usize,
    identifiers: Vec<(String, StaticType)>,
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
            ResolvedVar::Upvalue { slot, depth } => {
                let mut scope_idx = self.current_scope_idx;
                let mut depth = depth;
                while depth > 0 {
                    scope_idx = self.scopes[scope_idx]
                        .parent_idx
                        .expect("parent_idx was None while traversing the scope tree");
                    if self.scopes[scope_idx].creates_environment {
                        depth -= 1;
                    }
                }
                self.find_type_by_slot(scope_idx, slot)
            }
            ResolvedVar::Global { slot } => &self.global_scope.identifiers[slot].1,
        }
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

    pub(crate) fn get_binding_any(&mut self, ident: &str) -> Option<ResolvedVar> {
        let mut depth = 0;
        let mut scope_ptr = self.current_scope_idx;

        loop {
            if let Some(slot) = self.scopes[scope_ptr].find_slot_by_name(ident) {
                return Some(if depth == 0 {
                    ResolvedVar::Local { slot }
                } else {
                    ResolvedVar::Upvalue { slot, depth }
                });
            } else if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                if self.scopes[scope_ptr].creates_environment {
                    depth += 1;
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
        let mut depth = 0;
        let mut scope_ptr = self.current_scope_idx;

        loop {
            let candidates = self.scopes[scope_ptr].find_function_candidates(ident, sig);
            if !candidates.is_empty() {
                return candidates
                    .into_iter()
                    .map(|slot| {
                        if depth == 0 {
                            ResolvedVar::Local { slot }
                        } else {
                            ResolvedVar::Upvalue { slot, depth }
                        }
                    })
                    .collect();
            } else if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                if self.scopes[scope_ptr].creates_environment {
                    depth += 1;
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

    pub(crate) fn get_all_bindings_by_name(&self, ident: &str) -> Vec<ResolvedVar> {
        let mut results = Vec::new();
        let mut depth = 0;
        let mut scope_ptr = self.current_scope_idx;

        loop {
            let slots = self.scopes[scope_ptr].find_all_slots_by_name(ident);
            results.extend(slots.into_iter().map(|slot| {
                if depth == 0 {
                    ResolvedVar::Local { slot }
                } else {
                    ResolvedVar::Upvalue { slot, depth }
                }
            }));

            if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                if self.scopes[scope_ptr].creates_environment {
                    depth += 1;
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
        let mut depth = 0;
        let mut scope_ptr = self.current_scope_idx;

        loop {
            if let Some(slot) = self.scopes[scope_ptr].find_function(ident, arg_types) {
                return Some(if depth == 0 {
                    ResolvedVar::Local { slot }
                } else {
                    ResolvedVar::Upvalue { slot, depth }
                });
            } else if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                if self.scopes[scope_ptr].creates_environment {
                    depth += 1;
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
        let scope_idx = match var {
            ResolvedVar::Local { slot } => {
                self.find_scope_owning_slot(self.current_scope_idx, slot)
            }
            ResolvedVar::Upvalue { depth, .. } => {
                let mut scope_idx = self.current_scope_idx;
                let mut depth = depth;
                while depth > 0 {
                    scope_idx = self.scopes[scope_idx]
                        .parent_idx
                        .expect("parent_idx was None while traversing the scope tree");
                    if self.scopes[scope_idx].creates_environment {
                        depth -= 1;
                    }
                }
                self.find_scope_owning_slot(scope_idx, var.slot())
            }
            ResolvedVar::Global { .. } => {
                panic!("update_binding_type called with a global binding")
            }
        };
        let slot = var.slot();
        let base = self.scopes[scope_idx].base_offset;
        self.scopes[scope_idx].identifiers[slot - base].1 = new_type;
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
    fn block_scope_does_not_increment_depth() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), StaticType::Int);

        tree.new_block_scope();
        assert_eq!(
            tree.get_binding_any("x"),
            Some(ResolvedVar::Local { slot: 0 })
        );
    }

    #[test]
    fn function_scope_resets_slots_and_increments_depth() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), StaticType::Int);

        tree.new_function_scope();
        let y = tree.create_local_binding("y".into(), StaticType::Int);
        assert_eq!(y, ResolvedVar::Local { slot: 0 });

        assert_eq!(
            tree.get_binding_any("x"),
            Some(ResolvedVar::Upvalue { depth: 1, slot: 0 })
        );
    }

    #[test]
    fn iteration_scope_continues_numbering_but_increments_depth() {
        let mut tree = empty_scope_tree();
        tree.create_local_binding("x".into(), StaticType::Int);

        tree.new_iteration_scope();
        let i = tree.create_local_binding("i".into(), StaticType::Int);
        assert_eq!(i, ResolvedVar::Local { slot: 1 });

        assert_eq!(
            tree.get_binding_any("x"),
            Some(ResolvedVar::Upvalue { depth: 1, slot: 0 })
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
        // get_binding_any requires &mut self
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
}
