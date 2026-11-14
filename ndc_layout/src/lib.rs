//! Storage layout: where each value the name resolver declared lives when the
//! program runs. Natives and the program's top-level declarations are globals,
//! which every function reaches directly. A function call gets a frame of local
//! slots, numbered from 0, and a closure reaches the locals it captures through
//! upvalues. Everything here is read off the resolver's scope tree.
//!
//! The analyser picks symbols; [`Layout::lay_out`] then fills in where each of
//! them lives, once per batch.

use ahash::AHashMap;
use ndc_analysis::{
    Analysis, Binding, CaptureSource, DeclaredStruct, FieldAccessors, Instance, ResolvedVar,
};
use ndc_core::r#struct::StructInfo;
use ndc_core::{NodeId, StaticType};
use ndc_resolver::{NameResolver, ScopeId, ScopeKind, SymbolId};

#[cfg(test)]
mod tests;

/// What the analyser picked in one batch, by symbol. [`Layout::lay_out`] turns
/// each into the location the matching [`Analysis`] table holds.
#[derive(Debug, Default)]
pub struct Picks {
    /// For [`Analysis::bindings`].
    pub bindings: AHashMap<NodeId, Binding<SymbolId>>,
    /// For [`Analysis::getters`].
    pub getters: AHashMap<NodeId, Binding<SymbolId>>,
    /// For [`Analysis::setters`].
    pub setters: AHashMap<NodeId, Binding<SymbolId>>,
    /// For [`Analysis::augmented_assignments`].
    pub augmented_assignments: AHashMap<NodeId, Binding<SymbolId>>,
    /// For [`Analysis::variables`].
    pub variables: AHashMap<NodeId, SymbolId>,
    /// Each struct declaration's struct, for [`Analysis::structs`], which also
    /// says where its field accessors live.
    pub structs: AHashMap<NodeId, StructInfo>,
    /// For [`Analysis::instances`], by the symbol each instance is stored as.
    pub instances: AHashMap<SymbolId, InstancePicks>,
}

/// What one instance of a generic function picks in its body, where that
/// differs from the function's own picks.
#[derive(Debug)]
pub struct InstancePicks {
    /// The generic function's declaration.
    pub declaration: NodeId,
    pub bindings: AHashMap<NodeId, Binding<SymbolId>>,
    pub getters: AHashMap<NodeId, Binding<SymbolId>>,
    pub setters: AHashMap<NodeId, Binding<SymbolId>>,
    pub augmented_assignments: AHashMap<NodeId, Binding<SymbolId>>,
    /// As [`Analysis::type_checks`], for the instance's body.
    pub type_checks: AHashMap<NodeId, StaticType>,
    /// As [`Analysis::function_types`], for the functions the instance's
    /// body declares, with the instance's types.
    pub function_types: AHashMap<NodeId, StaticType>,
}

impl InstancePicks {
    pub fn new(declaration: NodeId) -> Self {
        Self {
            declaration,
            bindings: AHashMap::new(),
            getters: AHashMap::new(),
            setters: AHashMap::new(),
            augmented_assignments: AHashMap::new(),
            type_checks: AHashMap::new(),
            function_types: AHashMap::new(),
        }
    }
}

/// Where the resolver's symbols live at runtime.
#[derive(Clone, Copy)]
pub struct Layout<'a> {
    resolver: &'a NameResolver,
}

impl<'a> Layout<'a> {
    pub fn new(resolver: &'a NameResolver) -> Self {
        Self { resolver }
    }

    /// The global slot of `symbol`, if it's a global: the natives come first,
    /// then the program's top-level declarations in order, counting earlier
    /// REPL lines. With natives `[len]`, `let a = 1; let b = 2;` puts `a` in
    /// slot 1 and `b` in slot 2.
    pub fn global_slot(self, symbol: SymbolId) -> Option<usize> {
        if let Some(slot) = self.resolver.native_slot(symbol) {
            return Some(slot);
        }
        let scope = self.resolver.scope_of(symbol);
        (self.resolver.scope(scope).kind() == ScopeKind::Program)
            .then(|| self.resolver.native_count() + self.resolver.binding_position(symbol))
    }

    /// How many global slots the program needs: the natives and every top-level
    /// declaration, counting earlier REPL lines.
    pub fn global_count(self) -> usize {
        let declarations: usize = self
            .resolver
            .scopes()
            .filter(|(_, scope)| scope.kind() == ScopeKind::Program)
            .map(|(_, scope)| scope.bindings().len())
            .sum();
        self.resolver.native_count() + declarations
    }

    /// The slot `symbol` occupies among the locals of the frame that declares
    /// it. A frame starts at 0, with a function's parameters first. A block
    /// numbers on from its parent, and its slots are free again once it ends:
    /// in `fn f() { let a = 1; { let b = 2; } let c = 3; }`, `a` is 0, `b` is 1
    /// and `c` is 1.
    pub fn local_slot(self, symbol: SymbolId) -> usize {
        self.first_slot(self.resolver.scope_of(symbol)) + self.resolver.binding_position(symbol)
    }

    /// How code in `function` reaches `symbol` (`None` for the program's top
    /// level): a global by its global slot, a value of the same frame by its
    /// local slot, and a local further out through the function's upvalues. A
    /// local that closures capture is a cell, which they share. In
    /// `fn f() { let x = 1; fn g() { x } }`, `x` is cell 0 in `f` and upvalue 0
    /// in `g`.
    pub fn location(self, symbol: SymbolId, function: Option<NodeId>) -> ResolvedVar {
        if let Some(slot) = self.global_slot(symbol) {
            return ResolvedVar::Global { slot };
        }
        let slot = self.local_slot(symbol);
        let local = if self.resolver.is_captured(symbol) {
            ResolvedVar::Cell { slot }
        } else {
            ResolvedVar::Local { slot }
        };
        let Some(function) = function else {
            return local;
        };
        if self.resolver.function_scope(function) == self.frame_of(self.resolver.scope_of(symbol)) {
            return local;
        }
        let slot = self
            .captured_locals(function)
            .position(|captured| captured == symbol)
            .expect("internal error: a function captures every outer local it uses");
        ResolvedVar::Upvalue { slot }
    }

    /// Fills the location tables of `analysis` for a batch whose scopes are
    /// those after the first `first_scope`: where each of the analyser's
    /// `picks` lives, seen from the function its use sits in, and the frame
    /// size and captures of each function the batch declares.
    pub fn lay_out(self, picks: Picks, first_scope: usize, analysis: &mut Analysis) {
        let locate_use =
            |node: NodeId, symbol: SymbolId| self.location(symbol, self.resolver.use_site(node));
        let locate_binding = |(node, binding): (NodeId, Binding<SymbolId>)| {
            (node, binding.map(|symbol| locate_use(node, symbol)))
        };
        for (symbol, picks) in picks.instances {
            let instance = Instance {
                variable: self.location(symbol, None),
                bindings: picks.bindings.into_iter().map(locate_binding).collect(),
                getters: picks.getters.into_iter().map(locate_binding).collect(),
                setters: picks.setters.into_iter().map(locate_binding).collect(),
                augmented_assignments: picks
                    .augmented_assignments
                    .into_iter()
                    .map(locate_binding)
                    .collect(),
                type_checks: picks.type_checks,
                function_types: picks.function_types,
            };
            analysis
                .instances
                .entry(picks.declaration)
                .or_default()
                .push(instance);
        }
        analysis
            .bindings
            .extend(picks.bindings.into_iter().map(locate_binding));
        analysis
            .getters
            .extend(picks.getters.into_iter().map(locate_binding));
        analysis
            .setters
            .extend(picks.setters.into_iter().map(locate_binding));
        analysis
            .augmented_assignments
            .extend(picks.augmented_assignments.into_iter().map(locate_binding));
        // A declaration is in the frame it is made in, so only an assignment
        // target can be a use from a function further in.
        analysis.variables.extend(
            picks
                .variables
                .into_iter()
                .map(|(node, symbol)| (node, locate_use(node, symbol))),
        );
        analysis
            .structs
            .extend(picks.structs.into_iter().map(|(node, info)| {
                let fields = self
                    .resolver
                    .struct_symbols(node)
                    .accessors
                    .iter()
                    .map(|&(getter, setter)| FieldAccessors {
                        getter: self.location(getter, None),
                        setter: self.location(setter, None),
                    })
                    .collect();
                (node, DeclaredStruct { info, fields })
            }));

        // One sweep over the batch's scopes: each frame needs one past the
        // highest slot any of its scopes uses. In
        // `fn f(a) { let b = 1; { let c = 2; } }` that's 3. The program scope's
        // own declarations are globals, so at the top level only blocks and
        // loops count.
        let mut frame_sizes: AHashMap<ScopeId, usize> = AHashMap::new();
        for (id, scope) in self.resolver.scopes().skip(first_scope) {
            match scope.kind() {
                ScopeKind::Builtins | ScopeKind::Program => continue,
                ScopeKind::Function(function) => {
                    analysis.captures.insert(function, self.captures(function));
                }
                ScopeKind::Block => {}
            }
            let size = self.first_slot(id) + scope.bindings().len();
            let frame = frame_sizes.entry(self.frame_of(id)).or_default();
            *frame = (*frame).max(size);
        }
        analysis.top_level_frame_size = 0;
        for (frame, size) in frame_sizes {
            match self.resolver.scope(frame).kind() {
                ScopeKind::Function(function) => {
                    analysis.frame_sizes.insert(function, size);
                }
                ScopeKind::Program => analysis.top_level_frame_size = size,
                ScopeKind::Block | ScopeKind::Builtins => {
                    unreachable!("internal error: a frame is the program or a function")
                }
            }
        }
        analysis.global_count = self.global_count();
    }

    /// Where each upvalue of the function declared at `function` comes from,
    /// in the resolver's capture order: a local of the enclosing frame, or one
    /// of the enclosing function's own upvalues. In
    /// `fn f() { let x = 1; fn g() { fn h() { x } } }`, `g` gets `[Local(0)]`
    /// and `h` gets `[Upvalue(0)]`. Globals are never captured.
    pub fn captures(self, function: NodeId) -> Vec<CaptureSource> {
        let parameters = self.resolver.function_scope(function);
        let enclosing = self.frame_of(
            self.resolver
                .scope(parameters)
                .parent()
                .expect("internal error: a function scope always has a parent"),
        );

        self.captured_locals(function)
            .map(|symbol| {
                if self.frame_of(self.resolver.scope_of(symbol)) == enclosing {
                    return CaptureSource::Local(self.local_slot(symbol));
                }
                let ScopeKind::Function(outer) = self.resolver.scope(enclosing).kind() else {
                    unreachable!("internal error: only a function can pass a capture through");
                };
                let upvalue = self
                    .captured_locals(outer)
                    .position(|captured| captured == symbol)
                    .expect(
                        "internal error: the enclosing function captures what it passes through",
                    );
                CaptureSource::Upvalue(upvalue)
            })
            .collect()
    }

    /// The outer values `function` captures that are locals, in the resolver's
    /// capture order. Globals are reached directly instead.
    fn captured_locals(self, function: NodeId) -> impl Iterator<Item = SymbolId> + 'a {
        let layout = self;
        self.resolver
            .captures_of(function)
            .iter()
            .copied()
            .filter(move |&symbol| layout.global_slot(symbol).is_none())
    }

    /// The first slot of `scope`'s own values: 0 for a frame, and for a block
    /// the slot after everything its parent had bound when the block began.
    fn first_slot(self, scope: ScopeId) -> usize {
        let info = self.resolver.scope(scope);
        match info.kind() {
            ScopeKind::Block => {
                let parent = info
                    .parent()
                    .expect("internal error: a block always has a parent");
                // What the program scope binds is global, so its blocks start
                // the top-level frame at 0.
                if self.resolver.scope(parent).kind() == ScopeKind::Program {
                    return 0;
                }
                self.first_slot(parent) + info.entered_after()
            }
            ScopeKind::Program | ScopeKind::Function(_) => 0,
            ScopeKind::Builtins => unreachable!("internal error: natives are globals, not locals"),
        }
    }

    /// The scope whose frame `scope` belongs to: itself for the program or a
    /// function, the nearest such ancestor for a block.
    fn frame_of(self, scope: ScopeId) -> ScopeId {
        let mut current = scope;
        while self.resolver.scope(current).kind() == ScopeKind::Block {
            current = self
                .resolver
                .scope(current)
                .parent()
                .expect("internal error: a block always has a parent");
        }
        current
    }
}
