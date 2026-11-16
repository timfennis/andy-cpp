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
use crate::types::TypeVars;
use ahash::AHashSet;
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
    /// The body's literals are typed by the body alone, as at the end of
    /// the program: the `1` in `fn inc(x) => x + 1` is an Int, whatever
    /// `x` is. A variable the constraints wait on but that the parameters
    /// don't decide, like the element type of `[]` in `fn f(x) {
    /// [].push(x) }`, can't be decided by a call either, so it's settled
    /// here too.
    pub(super) fn generalize(
        &mut self,
        symbol: SymbolId,
        signature: &StaticType,
        declaration: NodeId,
        pure: bool,
        [first_use, first_function]: [usize; 2],
    ) -> StaticType {
        self.solve_waiting();
        self.default_literals(TypeVars::is_generalizable);
        let connected = self.reached_from_signature(signature);
        self.settle_ambiguous(signature, &connected);
        self.merge_copies();
        self.keep_outer_constraints_outside();

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
        let mut contents = Vec::new();
        for var in parameters {
            if self.vars.is_generalizable(var) {
                let name = type_parameter_name(named);
                if self.vars.is_contents(var) {
                    contents.push(name.as_str().into());
                }
                self.vars.generalize(var, &name);
                named += 1;
            }
        }

        // A type only the body makes, as of a list nothing else uses, gets
        // its fallback. One from outside the function is the outside's to
        // settle, as the keys of `pairs` in `fn f(r) => r.pairwise(fn(a, b)
        // => pairs[a, b])`, which the code around `f` may still decide.
        let uses = uses
            .into_iter()
            .map(|mut generic_use| {
                for typ in &generic_use.key {
                    let unnamed = self
                        .vars
                        .unsettled(typ)
                        .into_iter()
                        .filter(|&var| self.vars.is_generalizable(var))
                        .collect::<Vec<_>>();
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
            .map(|waiting| {
                let mut constraint = waiting
                    .constraint
                    .map_types(&mut |typ| self.vars.resolve(typ));
                if let (
                    Constraint::Join {
                        types: [_, _, result],
                        ..
                    },
                    Constraint::Join { expects_any, .. },
                ) = (&waiting.constraint, &mut constraint)
                {
                    *expects_any |= self.vars.any_is_expected_of(result);
                }
                Waiting {
                    constraint,
                    origin: waiting.origin,
                }
            })
            .collect::<Vec<_>>();
        // An instance is for the types of the type parameters that what it
        // compiles differently depends on: the constraints and uses it
        // picks for.
        // A `pure fn` gives every call with the same arguments what the first
        // one returned, so each set of types it's called with gets an
        // instance, and a cache, of its own: the parameters' types, as `1`
        // and `1.0` are equal arguments, and the return type's, as
        // `pure fn mk() => []` would otherwise give one list to calls that
        // want a `List<Int>` and calls that want a `List<String>`. The same
        // goes for each `pure fn` its body declares, which also takes only
        // data, as each instance is checked to give it.
        let own = pure.then(|| self.vars.resolve(signature));
        let declared_pure = self.declared_functions[first_function..]
            .iter()
            .filter(|node| self.pure_functions.contains_key(node))
            .map(|&node| self.vars.resolve(self.result.analysis.function_type(node)))
            .collect::<Vec<_>>();
        let mut parameters = Vec::new();
        for typ in own.into_iter().chain(declared_pure) {
            if let StaticType::Function {
                parameters: Some(types),
                return_type,
            } = typ
            {
                parameters.extend(types);
                parameters.push(*return_type);
            }
        }
        let mut key: Vec<Box<str>> = Vec::new();
        for typ in scheme
            .iter()
            .flat_map(|waiting| waiting.constraint.inputs())
            .chain(uses.iter().flat_map(|generic_use| &generic_use.key))
            .chain(&parameters)
        {
            for name in typ.type_parameters() {
                if !key.contains(&name) {
                    key.push(name);
                }
            }
        }
        // A function with a key runs only as its instances, which the
        // compiler compiles in place of its own body.
        if !key.is_empty() {
            self.result
                .analysis
                .instances
                .entry(declaration)
                .or_default();
        }
        self.schemes.insert(
            symbol,
            Scheme {
                constraints: scheme.into(),
                contents: contents.into(),
                key: key.into(),
                uses: uses.into(),
                declaration,
                recursive_calls: std::mem::take(&mut self.recursive_calls).into(),
                functions: self.declared_functions[first_function..]
                    .iter()
                    .map(|&node| (node, self.result.analysis.function_type(node).clone()))
                    .collect(),
            },
        );
        self.vars.resolve(signature)
    }

    /// The generic function's variables that its constraints connect to its
    /// parameters or what it returns, which each call settles. Unlike the
    /// `[]` of `fn f(x) { [].push(x) }`, which no call decides, the inner
    /// `%{}` of `fn f(data, k) { let m = %{:fn() => %{}}; m[k].insert(data)
    /// }` is what `data` is once `m[k]` is picked, and the `[]` of `fn f(xs)
    /// => if xs.len() == 0 { [] } else { f(xs[1..]) }` is what the call's
    /// result is used as.
    fn reached_from_signature(&self, signature: &StaticType) -> AHashSet<TypeVar> {
        let open = |var: &TypeVar| self.vars.is_generalizable(*var);
        let groups = self
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
        let mut reached = self
            .vars
            .unsettled(signature)
            .into_iter()
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
        reached
    }

    /// Leaves the constraints that wait only on what's outside the generic
    /// function whose body just ended, and what they settle, to the code
    /// around it: in `let x = 1; fn f() { x + 1 }`, the `+` waits for the
    /// `1` to be decided, and what it gives is the same for every call of
    /// `f`, so it's not a type parameter of `f`. Its pick is then made for
    /// `f`'s own body, like any other call's outside a generic function.
    fn keep_outer_constraints_outside(&mut self) {
        loop {
            let outer = self
                .waiting
                .values()
                .map(|waiting| &waiting.constraint)
                .filter(|constraint| !self.waits_on_the_function(constraint))
                .flat_map(|constraint| self.generic_outputs(constraint))
                .collect::<Vec<_>>();
            if outer.is_empty() {
                return;
            }
            for var in outer {
                self.vars.lower_to_current(&StaticType::Var(var));
            }
        }
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
    /// they count as settled by the call too, and so does an overloaded
    /// function's name passed to one, as `signum` in `xs.map(signum)`,
    /// whose type is what the call expects of it.
    fn generic_outputs(&self, constraint: &Constraint) -> Vec<TypeVar> {
        constraint
            .outputs()
            .iter()
            .chain(&self.passed_functions(constraint))
            .flat_map(|typ| self.vars.unsettled(typ))
            .chain(self.returned_by(constraint))
            .filter(|&var| self.vars.is_generalizable(var))
            .collect()
    }

    /// Whether `constraint` waits on a variable of the generic function
    /// whose body just ended: a call on what's passed to it, a function
    /// passed to it too, as one may use the function's parameters, and the
    /// branches of an `if` on theirs. What it settles itself may be one of
    /// those as well, as `n - 1` in `fn fib(n)` gives what `fib` takes.
    fn waits_on_the_function(&self, constraint: &Constraint) -> bool {
        let waits_on: &[StaticType] = match constraint {
            Constraint::Call(call) => call.arguments(),
            Constraint::Join { types, .. } => &types[..2],
            _ => return !self.generic_inputs(constraint).is_empty(),
        };
        waits_on
            .iter()
            .flat_map(|typ| self.vars.unsettled(typ))
            .any(|var| self.vars.is_generalizable(var))
    }

    /// The functions passed to `constraint`, if it's a call, whose types
    /// it settles: see [`Self::generic_outputs`].
    fn passed_functions(&self, constraint: &Constraint) -> Vec<StaticType> {
        match constraint {
            Constraint::Call(call) => {
                let values = self
                    .waiting
                    .values()
                    .filter_map(|waiting| match &waiting.constraint {
                        Constraint::FunctionValue(value) => {
                            Some(self.vars.resolve(value.expected()))
                        }
                        _ => None,
                    })
                    .collect::<Vec<_>>();
                call.arguments()
                    .iter()
                    .map(|argument| self.vars.resolve(argument))
                    .filter(|argument| {
                        matches!(argument, StaticType::Function { .. }) || values.contains(argument)
                    })
                    .collect()
            }
            // Picking the overload settles what the value returns.
            Constraint::FunctionValue(value) => match self.vars.resolve(value.expected()) {
                StaticType::Function { return_type, .. } => vec![*return_type],
                _ => Vec::new(),
            },
            _ => Vec::new(),
        }
    }

    /// The generic function's variables that its constraints wait on but
    /// that its parameters don't decide, by constraint, oldest first. What
    /// a constraint settles is decided when what it waits on is, and a
    /// branch of an `if` decides the other one once it's partly known: in
    /// `fn f(xs) => if c { [] } else { xs }`, what `xs` is says what the
    /// `[]` holds. An overloaded function's name used as a value is picked
    /// like a call: for each instance, from the types its parameters
    /// decide.
    fn ambiguous_vars(&self, parameters: &[StaticType]) -> Vec<(u64, Vec<TypeVar>)> {
        let serials = self.generic_constraints();
        let mut decided = parameters
            .iter()
            .flat_map(|typ| self.vars.unsettled(typ))
            .filter(|&var| self.vars.is_generalizable(var))
            .collect::<AHashSet<_>>();
        loop {
            let mut changed = false;
            for serial in &serials {
                let constraint = &self.waiting[serial].constraint;
                if let Constraint::Join { types, .. } = constraint {
                    let vars = |typ: &StaticType| {
                        self.vars
                            .unsettled(typ)
                            .into_iter()
                            .filter(|&var| self.vars.is_generalizable(var))
                            .collect::<Vec<_>>()
                    };
                    // The join waits while a branch is wholly unknown, so it
                    // decides nothing about such a branch: the result of
                    // `f(x, n - 1) ++ []` is decided by the `++`.
                    let waits_on = |typ: &StaticType| match self.vars.resolve(typ) {
                        StaticType::Var(var) => !self.vars.is_literal(var),
                        _ => false,
                    };
                    let [a, b, result] = types.each_ref().map(vars);
                    for (branch, other, other_type) in [(&a, &b, &types[1]), (&b, &a, &types[0])] {
                        if !branch.is_empty()
                            && branch.iter().all(|var| decided.contains(var))
                            && !waits_on(other_type)
                        {
                            for &var in other.iter().chain(&result) {
                                changed |= decided.insert(var);
                            }
                        }
                    }
                }
                if !self
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
                let ambiguous = self
                    .generic_inputs(constraint)
                    .into_iter()
                    .filter(|var| !decided.contains(var))
                    .collect::<Vec<_>>();
                (!ambiguous.is_empty()).then_some((serial, ambiguous))
            })
            .collect()
    }

    /// Settles the variables of the generic function with type `signature`
    /// that its parameters don't decide, like the end of the program
    /// settles its own: a call that a default rule decides is decided by it,
    /// and what's left gets its fallbacks, oldest constraint first, except
    /// what the constraints `connected` to the parameters, which each call
    /// settles: in `fn f(data, k) { let m = %{:fn() => %{}}; m[k].insert(data)
    /// }`, the inner map's keys are what `data` is once `m[k]` is picked.
    fn settle_ambiguous(&mut self, signature: &StaticType, connected: &AHashSet<TypeVar>) {
        let parameters = match signature {
            StaticType::Function {
                parameters: Some(parameters),
                ..
            } => parameters.clone(),
            _ => Vec::new(),
        };
        loop {
            let ambiguous = self.ambiguous_vars(&parameters);
            if ambiguous.is_empty() {
                return;
            }
            let decided_by_default = ambiguous
                .iter()
                .any(|(serial, vars)| self.default_to_branch(*serial, vars));
            if !decided_by_default {
                let Some(vars) = ambiguous.iter().find_map(|(_, vars)| {
                    let unconnected = vars
                        .iter()
                        .copied()
                        .filter(|var| !connected.contains(var))
                        .collect::<Vec<_>>();
                    (!unconnected.is_empty()).then_some(unconnected)
                }) else {
                    return;
                };
                let contents = self.vars.unsettled_contents(0);
                let (contents, others): (Vec<_>, Vec<_>) =
                    vars.into_iter().partition(|var| contents.contains(var));
                self.vars.settle_contents(&contents);
                self.vars.settle_with_fallbacks(&others);
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
        // A call's arguments and result, as one type, so the two calls'
        // results become one.
        let call_types = |call: &CallConstraint| {
            StaticType::Tuple(
                call.arguments()
                    .iter()
                    .chain([call.result()])
                    .cloned()
                    .collect(),
            )
        };
        for serial in serials {
            let waiting = &self.waiting[&serial];
            let Constraint::Call(call) = &waiting.constraint else {
                continue;
            };
            // Only a copy can be the same as an earlier call.
            let earlier = waiting.origin.as_ref().and_then(|_| {
                kept.iter()
                    .find_map(|earlier| match &self.waiting[earlier].constraint {
                        Constraint::Call(earlier) if earlier.is_same_call(call, &self.vars) => {
                            Some(earlier)
                        }
                        _ => None,
                    })
            });
            if let Some(earlier) = earlier
                && self.vars.try_unify(&call_types(earlier), &call_types(call))
            {
                self.waiting.remove(&serial);
            } else {
                kept.push(serial);
            }
        }
        self.solve_waiting();
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
                |&earlier| match self.vars.resolve(self.types.get_type(earlier)) {
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

/// The name of a generic function's `index`th type parameter: `A`, `B`, …,
/// `Z`, `A1`, ….
fn type_parameter_name(index: usize) -> String {
    let letter = char::from(b'A' + u8::try_from(index % 26).expect("less than 26"));
    match index / 26 {
        0 => letter.to_string(),
        round => format!("{letter}{round}"),
    }
}

/// `typ` with its type parameters named in the order they're written, as
/// an error shows it: `Fn(B) -> A` is `Fn(A) -> B`.
pub(super) fn named_in_order(typ: &StaticType) -> StaticType {
    let parts = match typ {
        StaticType::Function {
            parameters,
            return_type,
        } => parameters
            .iter()
            .flatten()
            .chain([return_type.as_ref()])
            .collect(),
        other => vec![other],
    };
    let mut order = Vec::new();
    for name in parts.into_iter().flat_map(StaticType::type_parameters) {
        if !order.contains(&name) {
            order.push(name);
        }
    }
    typ.replace(&|part| match part {
        StaticType::Generic(name) => order
            .iter()
            .position(|other| other == name)
            .map(|index| StaticType::Generic(type_parameter_name(index).into())),
        _ => None,
    })
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
