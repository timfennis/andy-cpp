//! Storage layout: where each value the name resolver declared lives when the
//! program runs. A function call gets a frame of local slots, numbered from 0,
//! and a closure reaches the values it captures through upvalues. Everything
//! here is read off the resolver's scope tree.

use ndc_parser::{CaptureSource, NodeId};
use ndc_resolver::{NameResolver, ScopeId, ScopeKind, SymbolId};

#[cfg(test)]
mod tests;

/// Where the resolver's symbols live at runtime.
#[derive(Clone, Copy)]
pub struct Layout<'a> {
    resolver: &'a NameResolver,
}

impl<'a> Layout<'a> {
    pub fn new(resolver: &'a NameResolver) -> Self {
        Self { resolver }
    }

    /// The slot `symbol` occupies among the locals of the frame that declares
    /// it. A frame starts at 0, with a function's parameters first. A block
    /// numbers on from its parent, and its slots are free again once it ends:
    /// in `let a = 1; { let b = 2; } let c = 3;`, `a` is 0, `b` is 1 and `c` is 1.
    pub fn local_slot(self, symbol: SymbolId) -> usize {
        let scope = self.resolver.scope_of(symbol);
        let index = self
            .resolver
            .scope(scope)
            .bindings()
            .iter()
            .position(|&bound| bound == symbol)
            .expect("internal error: a symbol is bound in its own scope");
        self.first_slot(scope) + index
    }

    /// Where each upvalue of the function declared at `function` comes from,
    /// in the resolver's capture order: a local of the enclosing frame, or one
    /// of the enclosing function's own upvalues. In
    /// `let x = 1; fn f() { fn g() { x } }`, `f` gets `[Local(0)]` and `g` gets
    /// `[Upvalue(0)]`.
    pub fn captures(self, function: NodeId) -> Vec<CaptureSource> {
        let parameters = self.resolver.function_scope(function);
        let enclosing = self.frame_of(
            self.resolver
                .scope(parameters)
                .parent()
                .expect("internal error: a function scope always has a parent"),
        );

        self.resolver
            .captures_of(function)
            .iter()
            .map(|&symbol| {
                if self.frame_of(self.resolver.scope_of(symbol)) == enclosing {
                    return CaptureSource::Local(self.local_slot(symbol));
                }
                let ScopeKind::Function(outer) = self.resolver.scope(enclosing).kind() else {
                    unreachable!("internal error: only a function can pass a capture through");
                };
                let upvalue = self
                    .resolver
                    .captures_of(outer)
                    .iter()
                    .position(|&captured| captured == symbol)
                    .expect(
                        "internal error: the enclosing function captures what it passes through",
                    );
                CaptureSource::Upvalue(upvalue)
            })
            .collect()
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
