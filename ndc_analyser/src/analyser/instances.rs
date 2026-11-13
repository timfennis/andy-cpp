//! Instances of generic functions. A generic function's body is compiled
//! once for each set of types its calls give the type parameters its
//! constraints depend on, its *key*, and each copy picks its calls' overloads
//! from those types. `fn inc(x) => x + 1` has key `A`: `inc(1)` calls the
//! instance for `Int`, whose `+` is `+(Int, Int)`, and `inc(2.5)` the one
//! for `Float`. `fn wrap(x) => [x]` has an empty key, so its own body serves
//! every call.

use super::Analyser;
use super::solver::{CallSite, Constraint, Origin, Waiting};
use crate::error::AnalysisError;
use crate::types::LiteralBounds;
use ahash::AHashMap;
use ndc_analysis::{Binding, Candidate};
use ndc_core::{NodeId, StaticType};
use ndc_layout::InstancePicks;
use ndc_lexer::Span;
use ndc_resolver::SymbolId;
use std::rc::Rc;

/// A generic function's type beyond its signature: what its body still
/// waits on, and which types an instance is for.
#[derive(Debug, Clone)]
pub(super) struct Scheme {
    /// The constraints its body left waiting on its type parameters.
    pub(super) constraints: Rc<[Waiting]>,
    /// The type parameters the constraints and `uses` depend on, which an
    /// instance is for.
    pub(super) key: Rc<[Box<str>]>,
    /// The generic functions its body calls with types that depend on its
    /// type parameters.
    pub(super) uses: Rc<[GenericUse]>,
    /// The function's declaration.
    pub(super) declaration: NodeId,
    /// The calls its body makes of itself, which have its own types.
    pub(super) recursive_calls: Rc<[NodeId]>,
    /// Its type parameters that are the types of literals in its body,
    /// with their bounds, which each call settles as a literal there would
    /// be: the `1` in `fn fact(n) => if n < 2 { 1 } else { n * fact(n - 1)
    /// }` is a Float for `fact(5.0)`.
    pub(super) literals: Rc<[(Box<str>, LiteralBounds)]>,
    /// The integer literals in its body whose type is one of its type
    /// parameters, with that type, which an instance records its own types
    /// for.
    pub(super) literal_nodes: Rc<[(NodeId, StaticType)]>,
}

/// A call of a generic function, or its name used as a value, which needs
/// the instance for the types it gives the function's key.
#[derive(Debug, Clone)]
pub(super) struct GenericUse {
    /// Where the use's pick is recorded.
    pub(super) site: CallSite,
    pub(super) function: SymbolId,
    /// The types it gives the function's key, in the key's order.
    pub(super) key: Vec<StaticType>,
    /// The instance whose body the use is in, `None` for code that's compiled
    /// once, like the program's top level.
    pub(super) instance: Option<SymbolId>,
    /// Where the use is.
    pub(super) span: Span,
}

/// What a call that's about to commit copied from the generic function it
/// commits to, and the checks its arguments need, until it's known whether
/// the call commits.
#[derive(Debug, Default)]
pub(super) struct Pending {
    /// Its constraints, with the function they're copied from and the calls
    /// they were copied through before.
    pub(super) copies: Vec<(SymbolId, Constraint, Option<Origin>)>,
    /// The function and the types the call gives its key.
    pub(super) uses: Vec<(SymbolId, Vec<StaticType>)>,
    /// Where an argument of type `Any` is checked, and against what.
    pub(super) checks: Vec<(NodeId, StaticType)>,
    /// The arguments not known yet that were passed for an `Iterable<T>`,
    /// with the `T` their elements settle once they're known.
    pub(super) iterations: Vec<(StaticType, StaticType)>,
}

impl Pending {
    /// How much is pending, to [`Self::truncate`] to later.
    pub(super) fn len(&self) -> [usize; 4] {
        [
            self.copies.len(),
            self.uses.len(),
            self.checks.len(),
            self.iterations.len(),
        ]
    }

    pub(super) fn truncate(&mut self, [copies, uses, checks, iterations]: [usize; 4]) {
        self.copies.truncate(copies);
        self.uses.truncate(uses);
        self.checks.truncate(checks);
        self.iterations.truncate(iterations);
    }

    pub(super) fn clear(&mut self) {
        self.truncate([0; 4]);
    }
}

impl Analyser {
    /// Records that the call at `site` committed to the generic functions
    /// [`Pending::uses`] holds, unless it's in a copy of a constraint, whose
    /// pick belongs to the function it was copied from. A tuple operator's
    /// call commits to one overload per element, which needs no instance.
    pub(super) fn record_uses(&mut self, site: CallSite, span: Span) {
        let uses = std::mem::take(&mut self.pending.uses);
        if self.copied_through.is_some() {
            return;
        }
        if let [(function, key)] = &uses[..] {
            self.uses.push(GenericUse {
                site,
                function: *function,
                key: key.clone(),
                instance: self.recording,
                span,
            });
        }
    }

    /// Finds the instance each use of a generic function in the batch needs,
    /// and points the use at it. A new instance's constraints are solved for
    /// its types, which records its picks and the instances its own uses
    /// need, until there are no new ones. Every error was reported where the
    /// constraints were copied to the calls, so solving them again reports
    /// nothing.
    pub(super) fn instantiate_uses(&mut self) {
        let mut new = Vec::new();
        loop {
            let uses = std::mem::take(&mut self.uses);
            for generic_use in uses {
                self.point_at_instance(&generic_use, &mut new);
            }
            let Some((instance, function, key)) = new.pop() else {
                break;
            };
            self.solve_instance(instance, function, &key);
        }
    }

    /// Points `generic_use` at the instance it needs, adding it to `new` if
    /// it's new. A use that left the pick to the program as it runs keeps
    /// it.
    fn point_at_instance(
        &mut self,
        generic_use: &GenericUse,
        new: &mut Vec<(SymbolId, SymbolId, Vec<StaticType>)>,
    ) {
        let key = generic_use
            .key
            .iter()
            .map(|typ| self.vars.shown(typ))
            .collect::<Vec<_>>();
        debug_assert!(
            !key.iter()
                .any(|typ| typ.contains(&|part| matches!(part, StaticType::Generic(_)))),
            "internal error: a use outside of generic bodies has a type parameter in {key:?}"
        );
        // A function whose instance calls it with types built from its own,
        // as `fn f(x) => f([x])` does, would need instances without end.
        // The call is left to the program as it runs.
        if self.grows_an_instance(generic_use, &key) {
            let name = self.resolver.symbol_name(generic_use.function).to_string();
            self.emit(AnalysisError::recursion_changes_types(
                &name,
                generic_use.span,
            ));
            return;
        }
        let instance = self.instance_for(generic_use.function, key, new);
        if instance == generic_use.function {
            return;
        }
        // An instance's body picks what the function's own body does,
        // except where its types say otherwise.
        let function = Binding::Resolved(Candidate::Scalar(generic_use.function));
        let picked = generic_use
            .instance
            .and_then(|of| self.pick_tables(Some(of)).at(generic_use.site).cloned())
            .or_else(|| self.pick_tables(None).at(generic_use.site).cloned());
        if picked != Some(function) {
            return;
        }
        self.pick_tables(generic_use.instance).insert(
            generic_use.site,
            Binding::Resolved(Candidate::Scalar(instance)),
        );
    }

    /// Whether `generic_use`, in the body of an instance of the function it
    /// uses, needs an instance whose key holds that instance's key inside a
    /// bigger type: `f<List<Int>>` from `f<Int>`.
    fn grows_an_instance(&self, generic_use: &GenericUse, key: &[StaticType]) -> bool {
        let Some(outer) = generic_use.instance else {
            return false;
        };
        self.instances
            .iter()
            .any(|((function, outer_key), &instance)| {
                instance == outer
                    && *function == generic_use.function
                    && key
                        .iter()
                        .zip(outer_key)
                        .any(|(new, old)| new != old && new.contains(&|part| part == old))
            })
    }

    /// The tables a pick in the body of `instance` goes in, or the shared
    /// ones for `None`.
    pub(super) fn pick_tables(&mut self, instance: Option<SymbolId>) -> PickTables<'_> {
        let picks = &mut self.picks;
        match instance.and_then(|instance| picks.instances.get_mut(&instance)) {
            Some(picks) => PickTables {
                bindings: &mut picks.bindings,
                getters: &mut picks.getters,
                setters: &mut picks.setters,
                augmented_assignments: &mut picks.augmented_assignments,
            },
            None => PickTables {
                bindings: &mut picks.bindings,
                getters: &mut picks.getters,
                setters: &mut picks.setters,
                augmented_assignments: &mut picks.augmented_assignments,
            },
        }
    }

    /// The instance of `function` for these types of its key: the function
    /// itself when they're all `Any`, as its own body picks its calls as the
    /// program runs, or when it was declared by an earlier batch, which
    /// compiled it already. Its own body's integer literals are Ints, so an
    /// integer literal's type parameter may be one.
    fn instance_for(
        &mut self,
        function: SymbolId,
        key: Vec<StaticType>,
        new: &mut Vec<(SymbolId, SymbolId, Vec<StaticType>)>,
    ) -> SymbolId {
        let scheme = &self.schemes[&function];
        let is_own = scheme.key.iter().zip(&key).all(|(name, typ)| {
            *typ == StaticType::Any
                || (*typ == StaticType::Int
                    && scheme
                        .literals
                        .iter()
                        .any(|(literal, bounds)| literal == name && bounds.is_number()))
        });
        if is_own || !self.declared_in_batch.contains(&function) {
            return function;
        }
        if let Some(&instance) = self.instances.get(&(function, key.clone())) {
            return instance;
        }
        let name = format!(
            "{}<{}>",
            self.resolver.symbol_name(function),
            key.iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        );
        let instance = self.resolver.declare_instance(name);
        self.types
            .declare(instance, self.types.get_type(function).clone());
        let declaration = self.schemes[&function].declaration;
        self.picks
            .instances
            .insert(instance, InstancePicks::new(declaration));
        self.instances.insert((function, key.clone()), instance);
        new.push((instance, function, key));
        instance
    }

    /// Solves the constraints of `function` for the instance whose key has
    /// these types, recording the picks they make in the instance's picks.
    fn solve_instance(&mut self, instance: SymbolId, function: SymbolId, key: &[StaticType]) {
        let scheme = self.schemes[&function].clone();
        let mut made = scheme
            .key
            .iter()
            .cloned()
            .zip(key.iter().cloned())
            .collect::<Vec<_>>();
        let outer = self.recording.replace(instance);
        let first = self.vars.count();
        self.vars.instantiate_literals(&scheme.literals, &mut made);
        for waiting in scheme.constraints.iter() {
            let mut copy = Waiting {
                constraint: waiting
                    .constraint
                    .map_types(&mut |typ| self.vars.instantiate_with(typ, &mut made)),
                origin: waiting.origin.clone(),
            };
            if !self.try_waiting(&mut copy) {
                self.wait_as(copy);
            }
        }
        for &call in scheme.recursive_calls.iter() {
            self.pick_tables(Some(instance)).insert(
                CallSite::Call(call),
                Binding::Resolved(Candidate::Scalar(instance)),
            );
        }
        for generic_use in scheme.uses.iter() {
            let key = generic_use
                .key
                .iter()
                .map(|typ| self.vars.instantiate_with(typ, &mut made))
                .collect();
            self.uses.push(GenericUse {
                site: generic_use.site,
                function: generic_use.function,
                key,
                instance: Some(instance),
                span: generic_use.span,
            });
        }
        self.settle_waiting(first);
        let literal_types = scheme
            .literal_nodes
            .iter()
            .map(|(node, typ)| {
                let typ = self.vars.instantiate_with(typ, &mut made);
                (*node, self.vars.resolve(&typ))
            })
            .collect::<Vec<_>>();
        if let Some(picks) = self.picks.instances.get_mut(&instance) {
            picks.literal_types.extend(literal_types);
        }
        self.recording = outer;
    }
}

/// The tables of one body's picks, by the kind of site.
pub(super) struct PickTables<'a> {
    bindings: &'a mut AHashMap<NodeId, Binding<SymbolId>>,
    getters: &'a mut AHashMap<NodeId, Binding<SymbolId>>,
    setters: &'a mut AHashMap<NodeId, Binding<SymbolId>>,
    augmented_assignments: &'a mut AHashMap<NodeId, Binding<SymbolId>>,
}

impl<'a> PickTables<'a> {
    fn table(self, site: CallSite) -> (&'a mut AHashMap<NodeId, Binding<SymbolId>>, NodeId) {
        match site {
            CallSite::Call(id) => (self.bindings, id),
            CallSite::Getter(id) => (self.getters, id),
            CallSite::Setter(id) | CallSite::FieldSetter(id) => (self.setters, id),
            CallSite::OpAssignment(id) => (self.augmented_assignments, id),
        }
    }

    /// The pick at `site`.
    pub(super) fn at(self, site: CallSite) -> Option<&'a Binding<SymbolId>> {
        let (table, id) = self.table(site);
        table.get(&id)
    }

    /// Records `binding` as the pick at `site`.
    pub(super) fn insert(self, site: CallSite, binding: Binding<SymbolId>) {
        let (table, id) = self.table(site);
        table.insert(id, binding);
    }
}
