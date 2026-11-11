//! Name resolution: matches every name in a program to the declaration it
//! means, without looking at types. Each declaration gets a [`SymbolId`], and
//! each use records every symbol it could mean, grouped by scope, for the type
//! pass to choose between. In `let x = 1; { let x = 2; x }`, the last `x` could
//! mean either `x`, nearest first.

mod name_resolution;
mod symbols;

pub use name_resolution::{
    AugmentedCandidates, Candidates, NameResolutionError, NameResolver, Scope, ScopeId, ScopeKind,
    StructSymbols,
};
pub use symbols::{BindingKind, SymbolId, SymbolTable};
