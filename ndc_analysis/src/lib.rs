//! What the analyser works out about a program, for the compiler and the
//! editor tooling to read. The parser's output is syntax; everything decided
//! about it after parsing belongs here.

use ahash::AHashMap;
use ndc_core::{NodeId, StaticType};

/// What the analyser worked out about one batch of statements: a whole program,
/// or one line of the REPL.
#[derive(Debug, Default, Clone)]
pub struct Analysis {
    /// Each expression's inferred type.
    pub expr_types: AHashMap<NodeId, StaticType>,
    /// The inferred return type of each function without a return type
    /// annotation, keyed by its declaration.
    pub inferred_return_types: AHashMap<NodeId, StaticType>,
    /// What each identifier expression refers to, keyed by the identifier. For
    /// a call like `f(x)` that's the overload of `f` the call picks.
    pub bindings: AHashMap<NodeId, Binding>,
    /// The variable each declared or assigned name is stored in, keyed by its
    /// binding pattern, its assignment target, or the named function or struct
    /// declaration. In `let x = 1; x = 2;` both the pattern `x` and the target
    /// `x` map to the same local.
    pub variables: AHashMap<NodeId, ResolvedVar>,
    /// Where the values each function captures come from, in upvalue order,
    /// keyed by its declaration.
    pub captures: AHashMap<NodeId, Vec<CaptureSource>>,
    /// How many local slots each function's own variables use, parameters
    /// first, keyed by its declaration. The compiler places its temporaries
    /// after them.
    pub frame_sizes: AHashMap<NodeId, usize>,
    /// How many local slots the program's top-level variables use, counting
    /// earlier REPL lines too.
    pub top_level_frame_size: usize,
}

impl Analysis {
    /// What the identifier expression `identifier` refers to; `Binding::None`
    /// if it refers to nothing.
    pub fn binding(&self, identifier: NodeId) -> &Binding {
        self.bindings.get(&identifier).unwrap_or(&Binding::None)
    }

    /// The variable the name declared or assigned at `node` is stored in; `None`
    /// for an anonymous function, which declares no name.
    pub fn variable(&self, node: NodeId) -> Option<ResolvedVar> {
        self.variables.get(&node).copied()
    }

    /// Where the values the function declared at `function` captures come from.
    pub fn captures(&self, function: NodeId) -> &[CaptureSource] {
        self.captures
            .get(&function)
            .expect("internal error: every function is analysed before it's compiled")
    }

    /// The frame size of the function declared at `function`.
    pub fn frame_size(&self, function: NodeId) -> usize {
        *self
            .frame_sizes
            .get(&function)
            .expect("internal error: every function is analysed before it's compiled")
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Binding {
    None,
    Resolved(Candidate),
    Dynamic(Vec<Candidate>), // figure it out at runtime
}

/// The operation selected by the analyser for an augmented assignment.
///
/// Every selected operation returns the updated left value internally. The
/// compiler writes that value back through the assignment target, while the
/// augmented-assignment expression itself evaluates to unit.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AugmentedAssignmentPlan {
    Unresolved,
    Resolved(Binding),
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ResolvedVar {
    Local {
        slot: usize,
    },
    /// A local that closures capture. Its slot holds the value until the first
    /// capture moves it into a heap cell the closures share, so reads and
    /// writes go through the cell once there is one.
    Cell {
        slot: usize,
    },
    Upvalue {
        slot: usize,
    },
    Global {
        slot: usize,
    },
}

impl ResolvedVar {
    pub fn slot(self) -> usize {
        match self {
            Self::Local { slot }
            | Self::Cell { slot }
            | Self::Upvalue { slot, .. }
            | Self::Global { slot } => slot,
        }
    }
}

/// A function overload candidate the analyser picked. The two variants encode
/// what kind of call this candidate is — a direct scalar call, or an element-wise
/// tuple broadcast over the scalar overload that `var()` returns.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Candidate {
    Scalar(ResolvedVar),
    /// Element-wise tuple broadcast. The inner `ResolvedVar` points to the
    /// scalar overload that fires for each element pair.
    Vec(ResolvedVar),
}

impl Candidate {
    pub fn var(self) -> ResolvedVar {
        match self {
            Self::Scalar(v) | Self::Vec(v) => v,
        }
    }

    pub fn is_vec(self) -> bool {
        matches!(self, Self::Vec(_))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CaptureSource {
    Local(usize),
    Upvalue(usize),
}
