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
        [first_use, first_check]: [usize; 2],
    ) -> StaticType {
        self.solve_waiting();
        self.default_literals(TypeVars::is_generalizable);
        self.settle_ambiguous(signature);
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
        for var in parameters {
            if self.vars.is_generalizable(var) {
                self.vars.generalize(var, &type_parameter_name(named));
                named += 1;
            }
        }

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
        // A value of type `Any` that the body passes where a type parameter
        // is expected, as `a` in `fn f(x) { let a: Any = 5; [x].push(a) }`,
        // is checked against what each call makes it.
        let checks = self.checks[first_check..]
            .iter()
            .filter(|(instance, ..)| instance.is_none())
            .map(|(_, node, typ, expected)| {
                (*node, self.vars.resolve(typ), self.vars.resolve(expected))
            })
            .filter(|(_, typ, expected)| {
                *typ == StaticType::Any
                    && expected.contains(&|part| matches!(part, StaticType::Generic(_)))
            })
            .collect::<Vec<_>>();
        // An instance is for the types of the type parameters that what it
        // compiles differently depends on: the constraints and uses it
        // picks for, and the checks.
        let mut key: Vec<Box<str>> = Vec::new();
        for typ in scheme
            .iter()
            .flat_map(|waiting| waiting.constraint.inputs())
            .chain(uses.iter().flat_map(|generic_use| &generic_use.key))
            .chain(checks.iter().map(|(_, _, expected)| expected))
        {
            for name in typ.type_parameters() {
                if !key.contains(&name) {
                    key.push(name);
                }
            }
        }
        self.schemes.insert(
            symbol,
            Scheme {
                constraints: scheme.into(),
                key: key.into(),
                uses: uses.into(),
                declaration,
                recursive_calls: std::mem::take(&mut self.recursive_calls).into(),
                checks: checks.into(),
            },
        );
        self.vars.resolve(signature)
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
    fn settle_ambiguous(&mut self, signature: &StaticType) {
        let parameters = match signature {
            StaticType::Function {
                parameters: Some(parameters),
                ..
            } => parameters.clone(),
            _ => Vec::new(),
        };
        loop {
            let ambiguous = self.ambiguous_vars(&parameters);
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
                        |&symbol| match self.vars.resolve(self.types.get_type(symbol)) {
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
