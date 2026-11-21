//! Making types the same, and passing values where types are expected.

use super::TypeVars;
use super::store::Undo;
use ndc_core::collections::HashSet;
use ndc_core::{StaticType, TypeVar};

/// What [`TypeVars::bind_arguments`] puts off while it passes the arguments.
#[derive(Debug)]
pub(super) struct Deferred {
    /// The variables of the parameters.
    variables: HashSet<TypeVar>,
    /// Each value passed where one of them is expected directly or as what
    /// an `Iterable` holds.
    passed: Vec<(TypeVar, StaticType)>,
}

impl TypeVars {
    /// Whether `typ` is made from the unsettled variable `var`, directly or
    /// through what a literal in it holds: `[x]` is made from `x`'s type.
    pub(crate) fn is_made_from(&self, typ: &StaticType, var: TypeVar) -> bool {
        self.vars_made_from(std::slice::from_ref(typ))
            .contains(&var)
    }

    /// The unsettled variables in `types` and, through each literal among
    /// them, in what it holds, at any depth: `x`'s type is one `[x]` is
    /// made from.
    pub(crate) fn vars_made_from(&self, types: &[StaticType]) -> HashSet<TypeVar> {
        let mut made_from = HashSet::default();
        let mut todo = types
            .iter()
            .flat_map(|typ| self.unsettled(typ))
            .collect::<Vec<_>>();
        while let Some(var) = todo.pop() {
            if made_from.insert(var) && self.is_literal(var) {
                for held in self.held_types(var) {
                    todo.extend(self.unsettled(&held));
                }
            }
        }
        made_from
    }

    /// Whether `typ` holds the unsettled variable `var` inside a type, as
    /// `[?0]` holds `?0`, so that `var` can't stand for it: it would
    /// have to hold itself without end. What a literal at the top of `typ`
    /// holds counts as `typ` too: in `xs.push([xs])`, `xs`'s element type
    /// would be the type of `[xs]`, whose elements hold a list of it. What's
    /// at the top itself doesn't: in `let xs = [x]; x = xs.pop()`, `x`'s type
    /// is the element type of `[x]`, which holds just `x`'s type.
    pub(super) fn would_contain(&self, typ: &StaticType, var: TypeVar) -> bool {
        let mut seen = HashSet::default();
        let mut todo = vec![typ.clone()];
        while let Some(next) = todo.pop() {
            match self.shallow(&next) {
                StaticType::Var(top) => {
                    if seen.insert(top) && self.is_literal(top) {
                        todo.extend(self.held_types(top));
                    }
                }
                inside => {
                    if self.is_made_from(&inside, var) {
                        return true;
                    }
                }
            }
        }
        false
    }

    /// Unifies `a` and `b` if they can be made the same type, and otherwise
    /// leaves every variable as it was.
    pub(crate) fn try_unify(&mut self, a: &StaticType, b: &StaticType) -> bool {
        self.attempt(|vars| vars.unify(a, b))
    }

    /// Makes `a` and `b` the same type by settling the unsettled variables in
    /// either, and returns whether they are the same now. `[?0]` and
    /// `[Int]` are, once `?0` is `Int`. `Never` fits any variable without
    /// settling it, since a value of that type never arrives. On a mismatch,
    /// the variables settled before it stay settled.
    pub(crate) fn unify(&mut self, a: &StaticType, b: &StaticType) -> bool {
        let (a, b) = (self.shallow(a), self.shallow(b));
        match (&a, &b) {
            (StaticType::Var(x), StaticType::Var(y)) if x == y => true,
            (StaticType::Var(_), StaticType::Never) | (StaticType::Never, StaticType::Var(_)) => {
                true
            }
            (StaticType::Var(var), other) | (other, StaticType::Var(var)) => {
                // A variable can't stand for a type that contains it.
                if self.would_contain(other, *var) {
                    return false;
                }
                self.settle(*var, other.clone())
            }
            // A failed check's type is every type, so nothing built from it
            // fails again.
            (StaticType::Error, _) | (_, StaticType::Error) => true,
            (StaticType::Option(x), StaticType::Option(y))
            | (StaticType::Iterable(x), StaticType::Iterable(y))
            | (StaticType::List(x), StaticType::List(y))
            | (StaticType::Iterator(x), StaticType::Iterator(y))
            | (StaticType::Lazy(x), StaticType::Lazy(y))
            | (StaticType::MinHeap(x), StaticType::MinHeap(y))
            | (StaticType::MaxHeap(x), StaticType::MaxHeap(y))
            | (StaticType::Deque(x), StaticType::Deque(y)) => self.unify(x, y),
            (StaticType::Tuple(xs), StaticType::Tuple(ys)) => {
                xs.len() == ys.len() && xs.iter().zip(ys).all(|(x, y)| self.unify(x, y))
            }
            (StaticType::Map { key: k1, value: v1 }, StaticType::Map { key: k2, value: v2 }) => {
                let keys = self.unify(k1, k2);
                let values = self.unify(v1, v2);
                keys && values
            }
            (
                StaticType::Function {
                    parameters: p1,
                    return_type: r1,
                },
                StaticType::Function {
                    parameters: p2,
                    return_type: r2,
                },
            ) => {
                let parameters = match (p1, p2) {
                    (Some(p1), Some(p2)) => {
                        p1.len() == p2.len() && p1.iter().zip(p2).all(|(x, y)| self.unify(x, y))
                    }
                    (None, None) => true,
                    _ => false,
                };
                let returns = self.unify(r1, r2);
                parameters && returns
            }
            _ => a == b,
        }
    }

    /// The parameters of the function type `typ`, with new variables for
    /// its type parameters but those `made` already has, or `None` when
    /// `typ` isn't a function whose parameters are known.
    pub(crate) fn instantiate_parameters(
        &mut self,
        typ: &StaticType,
        made: &mut Vec<(Box<str>, StaticType)>,
    ) -> Option<Vec<StaticType>> {
        let StaticType::Function {
            parameters: Some(_),
            ..
        } = typ
        else {
            return None;
        };
        let StaticType::Function {
            parameters: Some(parameters),
            ..
        } = self.instantiate_with(typ, made)
        else {
            unreachable!("instantiating a function type gives a function type");
        };
        Some(parameters)
    }

    /// Passes each of `arguments` where its parameter is expected, as
    /// [`Self::bind_argument`], and returns whether there's one argument
    /// for each parameter and each fits.
    ///
    /// A type parameter passed values directly, or as what an `Iterable`
    /// holds, is the one of their types that all of them fit, whatever order
    /// the arguments come in: `in(T, Iterable<T>)` given a String and a
    /// `[Any]` has `T` be `Any`, while given an Int and a `[String]`
    /// it fits neither. A container that can be written to, as the `[T]`
    /// of `push([T], T)`, says exactly what `T` is, which those values
    /// then have to fit.
    pub(crate) fn bind_arguments(
        &mut self,
        parameters: &[StaticType],
        arguments: &[StaticType],
    ) -> bool {
        if parameters.len() != arguments.len() {
            return false;
        }
        let variables = parameters
            .iter()
            .flat_map(|parameter| self.unsettled(parameter))
            .collect::<HashSet<_>>();
        let outer = self.deferred.replace(Deferred {
            variables,
            passed: Vec::new(),
        });
        let fits = parameters
            .iter()
            .zip(arguments)
            .all(|(parameter, argument)| self.bind_argument(parameter, argument));
        let passed = self
            .deferred
            .take()
            .expect("bind_arguments set what's deferred")
            .passed;
        let decided_some = !passed.is_empty();
        let mut fits = fits && self.decide_deferred(passed);
        // What an argument passes on to what it's passed, as `sorted` in
        // `map(xs, sorted)` takes what `xs` holds, is passed again now that
        // the type parameters it depends on are decided.
        if fits && decided_some {
            fits = parameters
                .iter()
                .zip(arguments)
                .all(|(parameter, argument)| self.bind_argument(parameter, argument));
        }
        self.deferred = outer;
        fits
    }

    /// Settles each type parameter whose values [`Self::bind_arguments`]
    /// put off as the one of their types all of them fit, and returns
    /// whether there's one. One a container already settled has to take
    /// each of them.
    pub(super) fn decide_deferred(&mut self, passed: Vec<(TypeVar, StaticType)>) -> bool {
        let mut groups: Vec<(StaticType, Vec<StaticType>)> = Vec::new();
        for (var, value) in passed {
            let target = self.shallow(&StaticType::Var(var));
            match groups.iter_mut().find(|(other, _)| *other == target) {
                Some((_, values)) => values.push(value),
                None => groups.push((target, vec![value])),
            }
        }
        for (target, values) in groups {
            let target = self.shallow(&target);
            let target = if matches!(target, StaticType::Var(_)) {
                let Some(widest) = values.iter().find(|&candidate| {
                    values
                        .iter()
                        .all(|value| self.probe(|vars| vars.bind_argument(candidate, value)))
                }) else {
                    return false;
                };
                if !self.unify(&target, widest) {
                    return false;
                }
                widest.clone()
            } else {
                target
            };
            if !values
                .iter()
                .all(|value| self.bind_argument(&target, value))
            {
                return false;
            }
        }
        true
    }

    /// `typ` with each type parameter, such as `T` in `<T>([T], T)`,
    /// replaced by what `made` has for it, or else a new variable of its
    /// own, which `made` gets. A call does this to the signature of the
    /// overload it picked, so its arguments settle what `T` stands for at
    /// that call, and `f::<Int>` puts `Int` in `made` for it first. A
    /// generic function's constraints are instantiated with its signature.
    pub(crate) fn instantiate_with(
        &mut self,
        typ: &StaticType,
        made: &mut Vec<(Box<str>, StaticType)>,
    ) -> StaticType {
        for name in typ.type_parameters() {
            if !made.iter().any(|(generic, _)| *generic == name) {
                let var = self.fresh();
                made.push((name, var));
            }
        }
        typ.replace(&|part| match part {
            StaticType::Generic(name) => made
                .iter()
                .find(|(generic, _)| generic == name)
                .map(|(_, var)| var.clone()),
            _ => None,
        })
    }

    /// Settles the parts of `argument` that aren't known yet by passing it
    /// where a `parameter` is expected, and returns whether it fits there. In
    /// `push(xs, 1)` with `push([?0], ?0)` and `xs: [?1]`, both
    /// variables become `Int`. Passing makes the two types the same, except
    /// that a parameter of type `Any` takes anything. An argument of type
    /// `Any` only goes where an `Any` is taken: it takes a cast to be passed
    /// as anything else. Inside a container, an `Option` or a tuple, `Any`
    /// is just another type: a `[Int]` doesn't fit a `[Any]`. A
    /// function that takes an `Any` takes what any function type passes, and
    /// one that returns anything fits where an `Any` is returned. An
    /// `Iterable<T>` takes any kind of sequence, and settles `T` from its
    /// elements. An argument nothing is known about yet stays what it is,
    /// as the call that takes it waits for it to iterate.
    pub(crate) fn bind_argument(&mut self, parameter: &StaticType, argument: &StaticType) -> bool {
        if *parameter == StaticType::Any
            && let Some(join) = self
                .chain(argument)
                .into_iter()
                .find(|var| self.joins.contains(var))
            && self
                .solution(join)
                .is_none_or(|solution| matches!(solution, StaticType::Var(_)))
        {
            self.expects_any.push(join);
            self.undo.push(Undo::ExpectedAny);
        }
        let (parameter, argument) = (self.shallow(parameter), self.shallow(argument));
        match (&parameter, &argument) {
            // Values that share no type are what's asked for where an `Any`
            // is expected: `let x: Any = if c { 1 } else { "a" }`.
            (StaticType::Any, StaticType::Var(var))
                if self.is_literal(*var) && self.unshared_values(*var).is_some() =>
            {
                self.settle_literal(*var, &StaticType::Any)
            }
            (_, StaticType::Never) | (StaticType::Any, _) => true,
            // A value of type `Any` doesn't make a literal's type `Any`, so it
            // doesn't go where the literal's values are: `n` in `let n = 1;
            // n = a` is an Int, which `a` takes a cast to be. A value whose
            // check failed leaves the literal's type what the literal says.
            (StaticType::Var(var), StaticType::Error) if self.is_literal(*var) => true,
            (StaticType::Var(var), StaticType::Any) if self.is_literal(*var) => false,
            (StaticType::Iterable(_), StaticType::Var(var)) if !self.is_literal(*var) => true,
            // So does a tuple whose elements aren't known yet: `(x, 1)`
            // iterates as Ints once `x` is one.
            (StaticType::Iterable(_), StaticType::Tuple(_)) if self.waits_to_iterate(&argument) => {
                true
            }
            // A literal passed for an `Iterable<T>` is the sequence it holds,
            // as `["a"]` passed to `len` is a `[String]`: only a
            // parameter is ever an `Iterable` itself.
            (StaticType::Iterable(_), StaticType::Var(var)) => {
                let Some(held) = self
                    .literal_defaults(*var)
                    .and_then(|defaults| defaults.into_iter().next())
                else {
                    return true;
                };
                self.settle_literal(*var, &held) && self.bind_argument(&parameter, &held)
            }
            // A value passed for a type parameter decides it with the others
            // passed for it: see `bind_arguments`.
            (StaticType::Var(var), _)
                if self.unsettled(&argument).is_empty()
                    && !self.is_literal(*var)
                    && self
                        .deferred
                        .as_ref()
                        .is_some_and(|deferred| deferred.variables.contains(var)) =>
            {
                let deferred = self.deferred.as_mut().expect("checked above");
                deferred.passed.push((*var, argument.clone()));
                true
            }
            (StaticType::Var(_), _) | (_, StaticType::Var(_)) => self.unify(&parameter, &argument),
            (_, StaticType::Error) | (StaticType::Error, _) => true,
            // An `Any` takes a cast to be a sequence.
            // A lazy sequence iterates, but isn't an `Iterable`: a function
            // that takes one may not be able to make its elements.
            (StaticType::Iterable(_), StaticType::Any | StaticType::Lazy(_)) => false,
            (
                StaticType::Function {
                    parameters: expected,
                    return_type: p,
                },
                StaticType::Function {
                    parameters: takes,
                    return_type: a,
                },
            ) => {
                // `(*)` is a parameter list that isn't checked here, on either
                // side: a function type that says nothing about the
                // parameters takes any function, and a function that takes
                // anything, as `print` does, takes what any function type
                // passes.
                let parameters = match (expected, takes) {
                    (Some(expected), Some(takes)) => {
                        expected.len() == takes.len()
                            && expected.iter().zip(takes).all(|(e, t)| self.passes(e, t))
                    }
                    _ => true,
                };
                // A function returning an `Iterable<U>` may return any kind of
                // sequence of `U`s, as `flat_map`'s callback does.
                // A function's value goes where an `Any` is expected back,
                // as any value does.
                parameters
                    && match self.shallow(p) {
                        StaticType::Iterable(_) | StaticType::Any => self.bind_argument(p, a),
                        _ => self.unify(p, a),
                    }
            }
            (StaticType::Iterable(p), _) => match self.element_type(&argument) {
                Some(element) => self.bind_argument(p, &element),
                None => false,
            },
            _ => self.unify(&parameter, &argument),
        }
    }

    /// Whether what a function type passes as an `expected` parameter is
    /// what a function that `takes` it takes: the same type, any kind of
    /// sequence for an `Iterable<T>`, as `len` takes the lists that
    /// `[[1], [2]].map(len)` passes, or anything for an `Any`, as
    /// `fn show(x: Any)` takes the Strings of `["a"].map(show)`.
    pub(super) fn passes(&mut self, expected: &StaticType, takes: &StaticType) -> bool {
        match self.shallow(takes) {
            StaticType::Iterable(_) | StaticType::Any => self.bind_argument(takes, expected),
            _ => self.unify(expected, takes),
        }
    }

    /// Whether what iterating over a value of type `typ` gives isn't known
    /// yet: it's not known what `typ` is, or it's a tuple whose elements
    /// aren't all known.
    pub(crate) fn waits_to_iterate(&self, typ: &StaticType) -> bool {
        match self.resolve(typ) {
            // A number never iterates, but a literal holding values is what
            // they are once they're settled: the values of `%{"a": ["b"]}`.
            StaticType::Var(var) => {
                !self.is_literal(var) || self.number_literal_span(var).is_none()
            }
            StaticType::Tuple(elements) => elements.iter().any(|e| self.has_unknowns(e)),
            _ => false,
        }
    }

    /// Whether `typ` has a part not known yet that isn't a literal's type.
    pub(super) fn has_unknowns(&self, typ: &StaticType) -> bool {
        self.unsettled(typ)
            .into_iter()
            .any(|var| !self.is_literal(var))
    }

    /// What iterating over a value of type `typ` gives, or `None` if it
    /// doesn't iterate. A tuple iterates as the one type its elements are
    /// once made the same, or as `Any` if they can't be: `(1, 2.5)` iterates
    /// as Floats, and `(1, "a")` as `Any`.
    pub(crate) fn element_type(&mut self, typ: &StaticType) -> Option<StaticType> {
        match self.resolve(typ) {
            // A number doesn't iterate, whatever kind of number it becomes.
            StaticType::Var(var) if self.is_number(var) => None,
            // What its elements are isn't known yet, and they don't have to
            // be one type, so they're not made one: `(a, b)` iterates as
            // `Any` until `a` and `b` are known.
            StaticType::Tuple(elements) if elements.iter().any(|e| self.has_unknowns(e)) => {
                Some(StaticType::Any)
            }
            StaticType::Tuple(elements) if !elements.is_empty() => {
                let unified = self.attempt(|vars| {
                    elements[1..]
                        .iter()
                        .all(|element| vars.unify(&elements[0], element))
                });
                Some(if unified {
                    self.resolve(&elements[0])
                } else {
                    StaticType::Any
                })
            }
            other => other.sequence_element_type(),
        }
    }
}
