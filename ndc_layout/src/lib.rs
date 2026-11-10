//! Storage layout: where each value the name resolver declared lives when the
//! program runs. A function call gets a frame of local slots, numbered from 0,
//! and a closure reaches the values it captures through upvalues. Everything
//! here is read off the resolver's scope tree.

use ndc_analysis::{CaptureSource, ResolvedVar};
use ndc_core::NodeId;
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
        self.first_slot(self.resolver.scope_of(symbol)) + self.resolver.binding_position(symbol)
    }

    /// How code in `function` reaches `symbol` (`None` for the program's top
    /// level): a native by its global slot, a value of the same frame by its
    /// local slot, and anything further out through the function's upvalues.
    /// A local that closures capture is a cell, which they share. In
    /// `let x = 1; fn f() { x }`, `x` is cell 0 at the top level and upvalue 0
    /// inside `f`.
    pub fn location(self, symbol: SymbolId, function: Option<NodeId>) -> ResolvedVar {
        if let Some(slot) = self.resolver.native_slot(symbol) {
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
            .resolver
            .captures_of(function)
            .iter()
            .position(|&captured| captured == symbol)
            .expect("internal error: a function captures every outer value it uses");
        ResolvedVar::Upvalue { slot }
    }

    /// How many local slots the frame of `function` (`None` for the program's
    /// top level) needs for its own values: one past the highest slot any of
    /// its scopes uses. In `fn f(a) { let b = 1; { let c = 2; } }` that's 3.
    pub fn frame_size(self, function: Option<NodeId>) -> usize {
        let frame = function.map(|function| self.resolver.function_scope(function));
        self.resolver
            .scopes()
            .filter(|(_, scope)| scope.kind() != ScopeKind::Builtins)
            .filter(|&(id, _)| {
                let own = self.frame_of(id);
                match frame {
                    Some(frame) => own == frame,
                    None => self.resolver.scope(own).kind() == ScopeKind::Program,
                }
            })
            .map(|(id, scope)| self.first_slot(id) + scope.bindings().len())
            .max()
            .unwrap_or(0)
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
