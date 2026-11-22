//! What the analyser works out about a program, for the compiler and the
//! editor tooling to read. The parser's output is syntax; everything decided
//! about it after parsing belongs here.

use ndc_core::collections::{HashMap, HashSet};
use ndc_core::r#struct::StructInfo;
use ndc_core::{Intrinsic, NodeId, StaticType};

/// What the analyser worked out about one batch of statements: a whole program,
/// or one line of the REPL.
#[derive(Debug, Default, Clone)]
pub struct Analysis {
    /// Each expression's inferred type.
    pub expr_types: HashMap<NodeId, StaticType>,
    /// The inferred return type of each function without a return type
    /// annotation, keyed by its declaration.
    pub inferred_return_types: HashMap<NodeId, StaticType>,
    /// The type of each name a binding pattern declares, keyed by the pattern:
    /// its annotation, or what the analyser inferred from the value. A
    /// parameter's type is its annotation, or what the analyser inferred for
    /// it.
    pub variable_types: HashMap<NodeId, StaticType>,
    /// The type each function declaration gives the function value, keyed by
    /// the declaration: its parameters' types, and its annotated return type,
    /// or `Any` without one, except a lambda's, which is what its body
    /// returns. `fn f(x: Int) { x }` is `fn(Int) -> Any`.
    pub function_types: HashMap<NodeId, StaticType>,
    /// What each identifier expression refers to, keyed by the identifier. For
    /// a call like `f(x)` that's the overload of `f` the call picks.
    pub bindings: HashMap<NodeId, Binding>,
    /// The variable each declared or assigned name is stored in, keyed by its
    /// binding pattern, its assignment target, or the named function or struct
    /// declaration. In `let x = 1; x = 2;` both the pattern `x` and the target
    /// `x` map to the same local.
    pub variables: HashMap<NodeId, ResolvedVar>,
    /// Where the values each function captures come from, in upvalue order,
    /// keyed by its declaration.
    pub captures: HashMap<NodeId, Vec<CaptureSource>>,
    /// The function that reads through each member access or member or index
    /// assignment target, keyed by it. `p.x` calls the getter `x(p)`; an index
    /// target has one only when it's read, as in `xs[0] += 1`.
    pub getters: HashMap<NodeId, Binding>,
    /// The function that writes through each member or index assignment
    /// target, keyed by it: `x=` for `p.x = 1`, `[]=` for `xs[0] = 1`.
    pub setters: HashMap<NodeId, Binding>,
    /// The function each augmented assignment like `x += 1` calls, keyed by the
    /// assignment: a mutating overload, such as `++=` for `xs ++= ys`, when
    /// one fits, otherwise the plain operator, `+` for `x += 1`. Either
    /// returns the updated left value, which the compiler writes back
    /// through the target. Absent when neither fits.
    pub augmented_assignments: HashMap<NodeId, Binding>,
    /// What each struct declaration declares, keyed by the declaration.
    pub structs: HashMap<NodeId, DeclaredStruct>,
    /// The type each cast checks its value against at runtime, keyed by the
    /// `cast` or `cast?` name, called or used as a value. `None` when the
    /// value is already known to have the type, as in `cast::<Int>(1)`, so
    /// there is nothing to check. A cast that gives a container a new type
    /// is checked even then, as the check is what copies a container
    /// something else holds.
    pub cast_checks: HashMap<NodeId, Option<StaticType>>,
    /// The sequences of the `for` loops and comprehensions that are sets,
    /// keyed by the sequence's expression: a set iterates as its keys, which
    /// the loop takes from it.
    pub iterated_sets: HashSet<NodeId>,
    /// The positions of the arguments of each call that are sets passed
    /// where a native takes any kind of sequence, an `Iterable<T>`, keyed by
    /// the call's site as [`Self::bindings`] and the other pick tables are:
    /// the native gets the set's keys.
    pub set_arguments: HashMap<NodeId, Vec<usize>>,
    /// How many local slots each function's own variables use, parameters
    /// first, keyed by its declaration. The compiler places its temporaries
    /// after them.
    pub frame_sizes: HashMap<NodeId, usize>,
    /// How many local slots the top-level code needs for the variables of its
    /// blocks and loops. Its own top-level declarations are globals.
    pub top_level_frame_size: usize,
    /// How many global slots the program needs: the natives and every
    /// top-level declaration, counting earlier REPL lines.
    pub global_count: usize,
    /// The instances of generic functions the batch makes, keyed by the
    /// function's declaration. An instance is the function's body for the
    /// types one set of calls gives it, which picks the overloads its calls
    /// use from those types: `fn inc(x) => x + 1` has one for Ints, whose
    /// `+` is `+(Int, Int)`. A function the batch declares whose calls
    /// depend on its types has an entry, empty if nothing calls it, and runs
    /// only as its instances. For an earlier REPL line's function, the entry
    /// holds only the instances this line made.
    pub instances: HashMap<NodeId, Vec<Instance>>,
    /// How many of the global slots are the built-ins': the natives, then
    /// the intrinsics. The program's own globals follow them.
    pub builtin_count: usize,
}

/// One instance of a generic function: where it's stored, and the picks in
/// its body that differ from the function's own.
#[derive(Debug, Clone)]
pub struct Instance {
    /// The global the instance is stored in.
    pub variable: ResolvedVar,
    /// As [`Analysis::bindings`], for the instance's body.
    pub bindings: HashMap<NodeId, Binding>,
    /// As [`Analysis::getters`], for the instance's body.
    pub getters: HashMap<NodeId, Binding>,
    /// As [`Analysis::setters`], for the instance's body.
    pub setters: HashMap<NodeId, Binding>,
    /// As [`Analysis::augmented_assignments`], for the instance's body.
    pub augmented_assignments: HashMap<NodeId, Binding>,
    /// As [`Analysis::function_types`], for the functions the instance's
    /// body declares, with the instance's types.
    pub function_types: HashMap<NodeId, StaticType>,
    /// As [`Analysis::cast_checks`], for the casts in the instance's body,
    /// which its types may already prove.
    pub cast_checks: HashMap<NodeId, Option<StaticType>>,
    /// As [`Analysis::iterated_sets`], for the loops in the instance's body
    /// whose sequences its types make sets.
    pub iterated_sets: HashSet<NodeId>,
    /// As [`Analysis::set_arguments`], for the calls in the instance's body.
    pub set_arguments: HashMap<NodeId, Vec<usize>>,
}

impl Analysis {
    /// Adds what the analyser worked out about a later batch, such as the
    /// next REPL line. Node ids are unique across batches, so its tables join
    /// these. Its instances, top-level frame size, global count and built-in
    /// count replace
    /// these: they're what compiling the later batch takes, which for
    /// [`Self::instances`] means the instances it made, of its own functions
    /// and of earlier ones.
    pub fn extend(&mut self, later: Self) {
        let Self {
            expr_types,
            inferred_return_types,
            variable_types,
            function_types,
            bindings,
            variables,
            captures,
            getters,
            setters,
            augmented_assignments,
            structs,
            cast_checks,
            iterated_sets,
            set_arguments,
            frame_sizes,
            top_level_frame_size,
            global_count,
            instances,
            builtin_count,
        } = later;
        self.expr_types.extend(expr_types);
        self.inferred_return_types.extend(inferred_return_types);
        self.variable_types.extend(variable_types);
        self.function_types.extend(function_types);
        self.bindings.extend(bindings);
        self.variables.extend(variables);
        self.captures.extend(captures);
        self.getters.extend(getters);
        self.setters.extend(setters);
        self.augmented_assignments.extend(augmented_assignments);
        self.structs.extend(structs);
        self.cast_checks.extend(cast_checks);
        self.iterated_sets.extend(iterated_sets);
        self.set_arguments.extend(set_arguments);
        self.frame_sizes.extend(frame_sizes);
        self.top_level_frame_size = top_level_frame_size;
        self.global_count = global_count;
        self.instances = instances;
        self.builtin_count = builtin_count;
    }

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

/// What a use of a name refers to. The analyser picks symbols, and the layout
/// turns each into the [`Callee`] the compiler reads.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Binding<V = Callee> {
    None,
    Resolved(Candidate<V>),
    /// An operator applied to each element of tuple arguments whose positions
    /// take different overloads: the one at index `i` applies to the elements
    /// at index `i`, as `(1, 2.5) + (3, 4)` adds two Ints, then two Floats.
    PerPosition(Vec<V>),
}

impl<V> Binding<V> {
    /// The same binding with each candidate's target mapped by `f`.
    pub fn map<W>(self, f: impl FnMut(V) -> W) -> Binding<W> {
        match self {
            Self::None => Binding::None,
            Self::Resolved(candidate) => Binding::Resolved(candidate.map(f)),
            Self::PerPosition(overloads) => {
                Binding::PerPosition(overloads.into_iter().map(f).collect())
            }
        }
    }
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

/// What a binding refers to: a value stored in a variable, or an intrinsic,
/// which the compiler writes as bytecode where it's used and which isn't
/// stored anywhere.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Callee {
    Var(ResolvedVar),
    Intrinsic(Intrinsic),
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

/// A function overload candidate the analyser picked. The two variants encode
/// what kind of call this candidate is — a direct scalar call, or an element-wise
/// tuple broadcast over the scalar overload that `var()` returns.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Candidate<V = Callee> {
    Scalar(V),
    /// Element-wise tuple broadcast. The inner value points to the scalar
    /// overload that fires for each element pair.
    Vec(V),
}

impl<V> Candidate<V> {
    pub fn var(self) -> V {
        match self {
            Self::Scalar(v) | Self::Vec(v) => v,
        }
    }

    /// The same kind of candidate with its target mapped by `f`.
    pub fn map<W>(self, f: impl FnOnce(V) -> W) -> Candidate<W> {
        match self {
            Self::Scalar(v) => Candidate::Scalar(f(v)),
            Self::Vec(v) => Candidate::Vec(f(v)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CaptureSource {
    Local(usize),
    Upvalue(usize),
}
