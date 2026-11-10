//! What the analyser works out about a program, for the compiler and the
//! editor tooling to read. The parser's output is syntax; everything decided
//! about it after parsing belongs here.

use ahash::AHashMap;
use ndc_core::r#struct::StructInfo;
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
    /// The type of each name a binding pattern declares, keyed by the pattern:
    /// its annotation, or what the analyser inferred from the value. A
    /// parameter's type is its annotation, `Any` if it has none.
    pub variable_types: AHashMap<NodeId, StaticType>,
    /// The type each function declaration gives the function value, keyed by
    /// the declaration: its annotated parameter and return types, `Any` where
    /// there is no annotation. `fn f(x: Int) { x }` is `Function(Int) -> Any`.
    pub function_types: AHashMap<NodeId, StaticType>,
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
    /// The function that reads through each member access or member or index
    /// assignment target, keyed by it. `p.x` calls the getter `x(p)`; an index
    /// target has one only when it's read, as in `xs[0] += 1`.
    pub getters: AHashMap<NodeId, Binding>,
    /// The function that writes through each member or index assignment
    /// target, keyed by it: `x=` for `p.x = 1`, `[]=` for `xs[0] = 1`.
    pub setters: AHashMap<NodeId, Binding>,
    /// The function each augmented assignment like `x += 1` calls, keyed by the
    /// assignment: a mutating overload such as `+=` when one fits, otherwise
    /// the plain operator `+`. Either returns the updated left value, which the
    /// compiler writes back through the target. Absent when neither fits.
    pub augmented_assignments: AHashMap<NodeId, Binding>,
    /// What each struct declaration declares, keyed by the declaration.
    pub structs: AHashMap<NodeId, DeclaredStruct>,
    /// The type each `as` cast checks its value against at runtime, keyed by
    /// the cast. `None` when the value is already known to have the type, as
    /// in `1 as Int`, so there is nothing to check.
    pub cast_checks: AHashMap<NodeId, Option<StaticType>>,
    /// How many local slots each function's own variables use, parameters
    /// first, keyed by its declaration. The compiler places its temporaries
    /// after them.
    pub frame_sizes: AHashMap<NodeId, usize>,
    /// How many local slots the top-level code needs for the variables of its
    /// blocks and loops. Its own top-level declarations are globals.
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

    /// The function that reads through the member access or assignment target
    /// at `node`, if it reads.
    pub fn getter(&self, node: NodeId) -> Option<&Binding> {
        self.getters.get(&node)
    }

    /// The function that writes through the assignment target at `target`.
    pub fn setter(&self, target: NodeId) -> &Binding {
        self.setters
            .get(&target)
            .expect("internal error: every assignment target is analysed before it's compiled")
    }

    /// What the struct declared at `declaration` declares.
    pub fn declared_struct(&self, declaration: NodeId) -> &DeclaredStruct {
        self.structs
            .get(&declaration)
            .expect("internal error: every struct is analysed before it's compiled")
    }

    /// The type the function declared at `function` gives its value.
    pub fn function_type(&self, function: NodeId) -> &StaticType {
        self.function_types
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

/// What a struct declaration declares besides its constructor. In
/// `struct Point { x: Int, y: Int }` that's the struct itself and the accessor
/// functions `x`, `x=`, `y` and `y=`.
#[derive(Debug, PartialEq, Clone)]
pub struct DeclaredStruct {
    /// The struct's name, id and fields. A value rather than an `Rc`, so the
    /// analysis can move between threads (the language server needs that).
    pub info: StructInfo,
    /// Each field's accessors, in field order.
    pub fields: Vec<FieldAccessors>,
}

/// Where a struct field's getter (`x`) and setter (`x=`) are stored.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct FieldAccessors {
    pub getter: ResolvedVar,
    pub setter: ResolvedVar,
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
