//! What the analyser works out about a program, for the compiler and the
//! editor tooling to read. The parser's output is syntax; everything decided
//! about it after parsing belongs here.

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
