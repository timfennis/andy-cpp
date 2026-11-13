//! Generic functions. A `fn` declared in the program scope takes any
//! arguments its body can work with: its unannotated parameters are type
//! variables, and the calls its body makes on them wait as its constraints.
//! `fn inc(x) => x + 1` is `inc<A>(A) -> B where +(A, Int) -> B`, and a call
//! `inc(2.5)` copies the constraint as `+(Float, Int) -> ?`, which settles
//! the call's type as `Float`.

use super::Analyser;
use super::instances::Scheme;
use super::solver::{CallConstraint, Constraint, Waiting};
use crate::error::AnalysisError;
use crate::overloads::is_strictly_more_specific;
use crate::types::{LiteralBounds, TypeVars};
use ahash::{AHashMap, AHashSet};
use ndc_analysis::{Binding, Candidate};
use ndc_core::{NodeId, StaticType, TypeVar};
use ndc_lexer::Span;
use ndc_resolver::{ScopeKind, SymbolId};

impl Analyser {
    /// Whether the function declared as `symbol` is generic, which a `fn`
    /// declared in the program scope is. One declared in a function or a
    /// block has one type, like a lambda.
    pub(super) fn is_generic_function(&self, symbol: SymbolId) -> bool {
        self.resolver.scope(self.resolver.scope_of(symbol)).kind() == ScopeKind::Program
    }

    /// Ends the body of the generic function `symbol`, whose type is
    /// `signature`, and returns its type with its type parameters: each
    /// variable its body made that nothing outside it refers to becomes
    /// one, and the constraints still waiting on them become the function's.
    /// A variable those constraints wait on but that the parameters don't
    /// decide, like the element type of `[]` in `fn f(x) { [].push(x) }`,
    /// can't be decided by a call either, so it's settled here first, as at
    /// the end of the program.
    pub(super) fn generalize(
        &mut self,
        symbol: SymbolId,
        signature: &StaticType,
        declaration: NodeId,
        first_use: usize,
        first_literal: usize,
    ) -> StaticType {
        self.solve_waiting();
        // A literal in the body that its type parameters don't reach has
        // one type, which a call can't change. The others are type
        // parameters, which each call settles.
        let tied = self.literals_tied_to_parameters(signature);
        self.default_literals(|vars, var| vars.is_generalizable(var) && !tied.contains(&var));
        self.settle_ambiguous(signature, &tied);
        self.merge_copies();

        let scheme = self
            .generic_constraints()
            .into_iter()
            .map(|serial| {
                self.waiting
                    .remove(&serial)
                    .expect("the constraint is waiting")
            })
            .collect::<Vec<_>>();

        // The generic functions the body calls with types that depend on
        // its type parameters are the instance's to pick.
        let (uses, outside): (Vec<_>, Vec<_>) = self
            .uses
            .split_off(first_use)
            .into_iter()
            .partition(|generic_use| {
                generic_use.key.iter().any(|typ| {
                    self.vars
                        .unsettled(typ)
                        .into_iter()
                        .any(|var| self.vars.is_generalizable(var))
                })
            });
        self.uses.extend(outside);

        // Name the type parameters in the order they appear.
        let mut parameters = self.vars.unsettled(signature);
        for waiting in &scheme {
            for typ in waiting
                .constraint
                .inputs()
                .iter()
                .chain(waiting.constraint.outputs())
            {
                for var in self.vars.unsettled(typ) {
                    if !parameters.contains(&var) {
                        parameters.push(var);
                    }
                }
            }
        }
        let mut named = 0;
        let mut literals: Vec<(Box<str>, LiteralBounds)> = Vec::new();
        let mut seen = parameters.iter().copied().collect::<AHashSet<_>>();
        // The integer literals one literal holds have one type, `[x, 1, 2]`
        // holds Ints or Floats, so they're one type parameter, named for
        // the first of them.
        let mut first_number = AHashMap::<TypeVar, TypeVar>::new();
        let mut names = AHashMap::<TypeVar, Box<str>>::new();
        let mut next = 0;
        while let Some(&var) = parameters.get(next) {
            next += 1;
            // What a literal holds is a type parameter too: the `1` in
            // `fn g(x) => [x, 1]` is a Float when `x` is.
            let mut first = None;
            for held in self.vars.held_vars(var) {
                if seen.insert(held) {
                    parameters.push(held);
                }
                if self.vars.is_literal(held) && self.vars.is_number(held) {
                    match first {
                        None => first = Some(held),
                        Some(first) if first != held => {
                            first_number.entry(held).or_insert(first);
                        }
                        Some(_) => {}
                    }
                }
            }
            if !self.vars.is_generalizable(var) {
                continue;
            }
            if let Some(name) = first_number.get(&var).and_then(|first| names.get(first)) {
                let name = name.clone();
                if let Some(bounds) = self.vars.generalize(var, &name)
                    && let Some((_, first_bounds)) =
                        literals.iter_mut().find(|(literal, _)| *literal == name)
                {
                    first_bounds.extend(bounds);
                }
                continue;
            }
            let name: Box<str> = type_parameter_name(named).into();
            if let Some(bounds) = self.vars.generalize(var, &name) {
                literals.push((name.clone(), bounds));
            }
            names.insert(var, name);
            named += 1;
        }
        let literals = literals
            .into_iter()
            .map(|(name, bounds)| (name, self.vars.resolve_bounds(&bounds)))
            .collect::<Vec<_>>();
        // A literal that's tied to a type parameter but that nothing outside
        // the body sees, like the list in `fn f(x) { let xs = [x]; 0 }`,
        // takes its default.
        self.default_literals(TypeVars::is_generalizable);
        let literal_nodes = self
            .number_literals
            .split_off(first_literal)
            .into_iter()
            .map(|(node, typ)| (node, self.vars.resolve(&typ)))
            .filter(|(_, typ)| matches!(typ, StaticType::Generic(_)))
            .collect::<Vec<_>>();

        // A type only the body makes, as of a list nothing else uses, gets
        // its fallback.
        let uses = uses
            .into_iter()
            .map(|mut generic_use| {
                for typ in &generic_use.key {
                    let unnamed = self.vars.unsettled(typ);
                    self.vars.settle_with_fallbacks(&unnamed);
                }
                generic_use.key = generic_use
                    .key
                    .iter()
                    .map(|typ| self.vars.resolve(typ))
                    .collect();
                generic_use
            })
            .collect::<Vec<_>>();
        self.watched_settlings = self.vars.settled_count();

        let scheme = scheme
            .into_iter()
            .map(|waiting| Waiting {
                constraint: waiting
                    .constraint
                    .map_types(&mut |typ| self.vars.resolve(typ)),
                origin: waiting.origin,
            })
            .collect::<Vec<_>>();
        for waiting in &scheme {
            if waiting.origin.is_none()
                && let Constraint::Call(call) = &waiting.constraint
            {
                self.pick_for_any_arguments(call);
            }
        }
        // An instance is for the types of the type parameters the
        // constraints and uses depend on.
        let mut key: Vec<Box<str>> = Vec::new();
        // The literals typed by a type parameter compile to what it is.
        for typ in scheme
            .iter()
            .flat_map(|waiting| waiting.constraint.inputs())
            .chain(uses.iter().flat_map(|generic_use| &generic_use.key))
            .chain(literal_nodes.iter().map(|(_, typ)| typ))
        {
            collect_type_parameters(typ, &mut key);
        }
        self.schemes.insert(
            symbol,
            Scheme {
                constraints: scheme.into(),
                key: key.into(),
                uses: uses.into(),
                declaration,
                recursive_calls: std::mem::take(&mut self.recursive_calls).into(),
                literals: literals.into(),
                literal_nodes: literal_nodes.into(),
            },
        );
        self.vars.resolve(signature)
    }

    /// The literals in the body of the generic function whose type is
    /// `signature` that its parameters' types reach, through a waiting
    /// constraint or what a literal holds: the `1` in `fn fact(n) => if n <
    /// 2 { 1 } else { n * fact(n - 1) }` is what `fact` returns, like `n *
    /// fact(n - 1)`, and the list in `fn k(x) => [x, "b"]` holds `x`.
    fn literals_tied_to_parameters(&self, signature: &StaticType) -> AHashSet<TypeVar> {
        let open = |var: &TypeVar| self.vars.is_generalizable(*var);
        let literals = self
            .vars
            .unsettled_literals(TypeVars::is_generalizable)
            .into_iter()
            .collect::<AHashSet<_>>();
        // The variables that are the same or decide each other: those of one
        // constraint, and a literal with what it holds.
        let mut groups = self
            .generic_constraints()
            .into_iter()
            .map(|serial| {
                let constraint = &self.waiting[&serial].constraint;
                constraint
                    .inputs()
                    .iter()
                    .chain(constraint.outputs())
                    .flat_map(|typ| self.vars.unsettled(typ))
                    .filter(open)
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        for &literal in &literals {
            let mut group = self.vars.held_vars(literal);
            group.retain(open);
            group.push(literal);
            groups.push(group);
        }
        let parameters = match signature {
            StaticType::Function {
                parameters: Some(parameters),
                ..
            } => parameters.as_slice(),
            _ => &[],
        };
        let mut reached = parameters
            .iter()
            .flat_map(|typ| self.vars.unsettled(typ))
            .filter(open)
            .collect::<AHashSet<_>>();
        loop {
            let mut changed = false;
            for group in &groups {
                if group.iter().any(|var| reached.contains(var)) {
                    for &var in group {
                        changed |= reached.insert(var);
                    }
                }
            }
            if !changed {
                break;
            }
        }
        reached.retain(|var| literals.contains(var));
        reached
    }

    /// The waiting constraints that wait on or settle a variable of the
    /// generic function whose body just ended, oldest first.
    fn generic_constraints(&self) -> Vec<u64> {
        self.waiting
            .iter()
            .filter(|(_, waiting)| {
                waiting
                    .constraint
                    .inputs()
                    .iter()
                    .chain(waiting.constraint.outputs())
                    .flat_map(|typ| self.vars.unsettled(typ))
                    .any(|var| self.vars.is_generalizable(var))
            })
            .map(|(&serial, _)| serial)
            .collect()
    }

    /// The generic function's variables that `constraint` waits on, leaving
    /// out what it settles itself.
    fn generic_inputs(&self, constraint: &Constraint) -> Vec<TypeVar> {
        let outputs = self.generic_outputs(constraint);
        constraint
            .inputs()
            .iter()
            .flat_map(|typ| self.vars.unsettled(typ))
            .filter(|&var| self.vars.is_generalizable(var) && !outputs.contains(&var))
            .collect()
    }

    /// The generic function's variables that `constraint` settles once it's
    /// solved. A function passed to a call gets the types the call passes
    /// it, as `fn(a, x) => a + x` in `xs.fold(0, fn(a, x) => a + x)`, so
    /// they count as settled by the call too.
    fn generic_outputs(&self, constraint: &Constraint) -> Vec<TypeVar> {
        let passed_functions = match constraint {
            Constraint::Call(call) => call
                .arguments()
                .iter()
                .map(|argument| self.vars.resolve(argument))
                .filter(|argument| matches!(argument, StaticType::Function { .. }))
                .collect(),
            _ => Vec::new(),
        };
        constraint
            .outputs()
            .iter()
            .chain(&passed_functions)
            .flat_map(|typ| self.vars.unsettled(typ))
            .filter(|&var| self.vars.is_generalizable(var))
            .collect()
    }

    /// The generic function's variables that its constraints wait on but
    /// that its parameters don't decide, by constraint, oldest first. What
    /// a constraint settles is decided when what it waits on is. An
    /// overloaded function's name used as a value has to be decided in the
    /// body, which is compiled once, so what it waits on counts too.
    fn ambiguous_vars(
        &self,
        parameters: &[StaticType],
        tied: &AHashSet<TypeVar>,
    ) -> Vec<(u64, Vec<TypeVar>)> {
        let serials = self.generic_constraints();
        let mut decided = parameters
            .iter()
            .flat_map(|typ| self.vars.unsettled(typ))
            .filter(|&var| self.vars.is_generalizable(var))
            .chain(tied.iter().copied())
            .collect::<AHashSet<_>>();
        loop {
            let mut changed = false;
            for serial in &serials {
                let constraint = &self.waiting[serial].constraint;
                if matches!(constraint, Constraint::FunctionValue(_))
                    || !self
                        .generic_inputs(constraint)
                        .iter()
                        .all(|var| decided.contains(var))
                {
                    continue;
                }
                for var in self.generic_outputs(constraint) {
                    changed |= decided.insert(var);
                }
            }
            if !changed {
                break;
            }
        }
        serials
            .into_iter()
            .filter_map(|serial| {
                let constraint = &self.waiting[&serial].constraint;
                let ambiguous = match constraint {
                    Constraint::FunctionValue(_) => self.generic_inputs(constraint),
                    _ => self
                        .generic_inputs(constraint)
                        .into_iter()
                        .filter(|var| !decided.contains(var))
                        .collect(),
                };
                (!ambiguous.is_empty()).then_some((serial, ambiguous))
            })
            .collect()
    }

    /// Settles the variables of the generic function with type `signature`
    /// that its parameters don't decide, like the end of the program
    /// settles its own: a call that a default rule decides is decided by it,
    /// and what's left gets its fallbacks, oldest constraint first.
    fn settle_ambiguous(&mut self, signature: &StaticType, tied: &AHashSet<TypeVar>) {
        let parameters = match signature {
            StaticType::Function {
                parameters: Some(parameters),
                ..
            } => parameters.clone(),
            _ => Vec::new(),
        };
        loop {
            let ambiguous = self.ambiguous_vars(&parameters, tied);
            let Some((_, vars)) = ambiguous.first().cloned() else {
                return;
            };
            let decided_by_default = ambiguous.iter().any(|(serial, vars)| {
                let only_ambiguous = self.waiting.get(serial).is_some_and(|waiting| {
                    self.generic_inputs(&waiting.constraint)
                        .iter()
                        .all(|var| vars.contains(var))
                });
                (only_ambiguous
                    && (self.default_to_narrowest(*serial) || self.default_to_operand(*serial)))
                    || self.default_to_branch(*serial, vars)
            });
            if !decided_by_default {
                self.vars.settle_with_fallbacks(&vars);
            }
            self.solve_waiting();
        }
    }

    /// Settles a branch of the waiting `if` `serial` whose type is one of
    /// `ambiguous` as the other branch's type, and returns whether it did.
    /// In `fn fib(n) => if n < 2 { n } else { fib(n - 1) + fib(n - 2) }`,
    /// what `+` returns is what `fib` returns, which is what the `if`
    /// returns: nothing but the other branch can say what it is.
    pub(super) fn default_to_branch(&mut self, serial: u64, ambiguous: &[TypeVar]) -> bool {
        let Some(Waiting {
            constraint: Constraint::Join {
                types: [a, b, _], ..
            },
            ..
        }) = self.waiting.get(&serial)
        else {
            return false;
        };
        let (a, b) = (self.vars.resolve(a), self.vars.resolve(b));
        for (branch, other) in [(&a, &b), (&b, &a)] {
            if let StaticType::Var(var) = branch
                && ambiguous.contains(var)
                && self.vars.try_unify(branch, other)
            {
                return true;
            }
        }
        false
    }

    /// Merges each copy of a generic function's constraint into an earlier
    /// constraint that's the same call with the same arguments, which
    /// returns the same type. In `fn g(x) => h(x) + h(x)`, both calls of
    /// `h` copy `h`'s constraints, and `g` keeps one of each.
    fn merge_copies(&mut self) {
        let serials = self.generic_constraints();
        let mut kept: Vec<u64> = Vec::new();
        for serial in serials {
            let duplicate = {
                let waiting = &self.waiting[&serial];
                let Constraint::Call(call) = &waiting.constraint else {
                    continue;
                };
                if waiting.origin.is_none() {
                    kept.push(serial);
                    continue;
                }
                kept.iter().copied().find(|earlier| {
                    let Constraint::Call(earlier) = &self.waiting[earlier].constraint else {
                        return false;
                    };
                    earlier.is_same_call(call, &self.vars)
                })
            };
            match duplicate {
                Some(earlier) => {
                    let (Constraint::Call(earlier), Constraint::Call(call)) = (
                        &self.waiting[&earlier].constraint,
                        &self.waiting[&serial].constraint,
                    ) else {
                        unreachable!("both are calls");
                    };
                    // Copies of one literal become one, as they're settled
                    // the same way.
                    let earlier = StaticType::Tuple(
                        earlier
                            .arguments()
                            .iter()
                            .chain([earlier.result()])
                            .cloned()
                            .collect(),
                    );
                    let later = StaticType::Tuple(
                        call.arguments()
                            .iter()
                            .chain([call.result()])
                            .cloned()
                            .collect(),
                    );
                    if self.vars.try_unify(&earlier, &later) {
                        self.waiting.remove(&serial);
                    } else {
                        kept.push(serial);
                    }
                }
                None => kept.push(serial),
            }
        }
        self.solve_waiting();
    }

    /// Records what the body of a generic function calls at `call`, which
    /// is compiled once for every type it's called with: the overloads that
    /// can take the arguments, picked as the program runs.
    fn pick_for_any_arguments(&mut self, call: &CallConstraint) {
        let arguments = call
            .arguments()
            .iter()
            .map(|argument| {
                argument.replace(&|part| {
                    matches!(part, StaticType::Generic(_)).then_some(StaticType::Any)
                })
            })
            .collect::<Vec<_>>();
        let binding = match self
            .resolve_overloads(call.candidates(), &arguments, call.kind())
            .binding
        {
            // An overload may take the values the body gets as the program
            // runs although none takes their types with `Any` in them, as
            // `sum` takes no `List<Any>`. The program picks among them as it
            // runs, which looks at a container's elements.
            Binding::None => {
                let candidates = self
                    .overloads()
                    .reachable_symbols(call.candidates())
                    .into_iter()
                    .filter(
                        |&symbol| match self.vars.resolve(&self.types.callable_type(symbol)) {
                            StaticType::Function {
                                parameters: Some(parameters),
                                ..
                            } => parameters.len() == arguments.len(),
                            StaticType::Function {
                                parameters: None, ..
                            } => true,
                            _ => false,
                        },
                    )
                    .map(Candidate::Scalar)
                    .collect::<Vec<_>>();
                if candidates.is_empty() {
                    Binding::None
                } else {
                    Binding::Dynamic(candidates)
                }
            }
            binding => binding,
        };
        self.record_pick(call.site(), binding, &arguments);
    }

    /// Reports the function `symbol`, about to be declared with parameters
    /// of these types, if an earlier function with its name in its scope
    /// takes the same parameters, or if a call could fit both and neither
    /// is more specific, so nothing says which one it means. A parameter
    /// without an annotation takes any argument the body can work with, so
    /// `fn f(x: Int, y)` and `fn f(x, y: Int)` both fit `f(1, 2)`. But
    /// `fn show(x)` next to `fn show(x: Int)` is a fallback that a call
    /// with an Int doesn't pick, like `fn show(x: Any)`.
    pub(super) fn check_overlap(
        &mut self,
        symbol: SymbolId,
        name: &str,
        parameters: Option<&[StaticType]>,
        span: Span,
    ) {
        let scope = self.resolver.scope(self.resolver.scope_of(symbol));
        let earlier = scope.bindings()[..self.resolver.binding_position(symbol)]
            .iter()
            .filter(|&&earlier| self.resolver.symbol_name(earlier) == name)
            .filter_map(
                |&earlier| match self.vars.resolve(&self.types.callable_type(earlier)) {
                    StaticType::Function { parameters, .. } => Some(parameters),
                    _ => None,
                },
            )
            .collect::<Vec<_>>();
        let shown = |parameters: Option<&[StaticType]>| {
            parameters.map(|parameters| {
                parameters
                    .iter()
                    .map(|parameter| {
                        parameter.replace(&|part| {
                            matches!(part, StaticType::Var(_) | StaticType::Generic(_))
                                .then_some(StaticType::Any)
                        })
                    })
                    .collect::<Vec<_>>()
            })
        };
        for earlier in earlier {
            if shown(earlier.as_deref()) == shown(parameters) {
                self.emit(AnalysisError::function_redefinition(
                    name,
                    shown(parameters).as_deref(),
                    span,
                ));
                return;
            }
            if let (Some(earlier), Some(parameters)) = (&earlier, parameters)
                && is_a_tie(earlier, parameters)
            {
                self.emit(AnalysisError::tied_overloads(
                    name,
                    &shown_with_blanks(parameters),
                    &shown_with_blanks(earlier),
                    span,
                ));
                return;
            }
        }
    }
}

/// Adds the type parameters in `typ` that `parameters` doesn't have yet.
fn collect_type_parameters(typ: &StaticType, parameters: &mut Vec<Box<str>>) {
    let found = std::cell::RefCell::new(Vec::new());
    typ.contains(&|part| {
        if let StaticType::Generic(name) = part {
            found.borrow_mut().push(name.clone());
        }
        false
    });
    for name in found.into_inner() {
        if !parameters.contains(&name) {
            parameters.push(name);
        }
    }
}

/// The name of a generic function's `index`th type parameter: `A`, `B`, …,
/// `Z`, `A1`, ….
fn type_parameter_name(index: usize) -> String {
    let letter = char::from(b'A' + u8::try_from(index % 26).expect("less than 26"));
    match index / 26 {
        0 => letter.to_string(),
        round => format!("{letter}{round}"),
    }
}

/// Parameter types as an error shows them, with `_` for a parameter whose
/// type the body decides.
fn shown_with_blanks(parameters: &[StaticType]) -> Vec<String> {
    parameters
        .iter()
        .map(|parameter| match parameter {
            StaticType::Var(_) | StaticType::Generic(_) => "_".to_string(),
            other => other.to_string(),
        })
        .collect()
}

/// Whether some call could fit overloads taking `a` and `b`, but neither
/// is more specific than the other, so nothing says which one it means.
fn is_a_tie(a: &[StaticType], b: &[StaticType]) -> bool {
    a.len() == b.len()
        && a.iter().zip(b).all(|(a, b)| may_take_the_same(a, b))
        && !is_strictly_more_specific(a, b)
        && !is_strictly_more_specific(b, a)
}

/// Whether some argument could be passed both where `a` and where `b` is
/// expected and not lose to either: a type parameter takes anything, and
/// `Any` only another `Any` or a type parameter, as it loses to every
/// other type.
fn may_take_the_same(a: &StaticType, b: &StaticType) -> bool {
    let wildcard = |typ: &StaticType| matches!(typ, StaticType::Var(_) | StaticType::Generic(_));
    match (a, b) {
        _ if wildcard(a) || wildcard(b) => true,
        (StaticType::Option(x), StaticType::Option(y))
        | (StaticType::Iterable(x), StaticType::Iterable(y))
        | (StaticType::List(x), StaticType::List(y))
        | (StaticType::Iterator(x), StaticType::Iterator(y))
        | (StaticType::MinHeap(x), StaticType::MinHeap(y))
        | (StaticType::MaxHeap(x), StaticType::MaxHeap(y))
        | (StaticType::Deque(x), StaticType::Deque(y)) => may_take_the_same(x, y),
        (StaticType::Tuple(xs), StaticType::Tuple(ys)) => {
            xs.len() == ys.len() && xs.iter().zip(ys).all(|(x, y)| may_take_the_same(x, y))
        }
        (StaticType::Map { key: k1, value: v1 }, StaticType::Map { key: k2, value: v2 }) => {
            may_take_the_same(k1, k2) && may_take_the_same(v1, v2)
        }
        // `Any` takes only what another `Any` does.
        _ => a == b,
    }
}

/// The pairs of `natives` with one name that a call could fit with
/// neither more specific than the other, which overloads in one scope may
/// not be, each described as `name(A) and name(B)`.
pub fn tied_natives(natives: &[(String, StaticType)]) -> Vec<String> {
    let mut tied = Vec::new();
    for (i, (name, a)) in natives.iter().enumerate() {
        for (other, b) in &natives[i + 1..] {
            if let (
                true,
                StaticType::Function {
                    parameters: Some(a),
                    ..
                },
                StaticType::Function {
                    parameters: Some(b),
                    ..
                },
            ) = (name == other, a, b)
                && is_a_tie(a, b)
            {
                let shown = |parameters: &[StaticType]| {
                    parameters
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                };
                tied.push(format!("{name}({}) and {name}({})", shown(a), shown(b)));
            }
        }
    }
    tied
}
