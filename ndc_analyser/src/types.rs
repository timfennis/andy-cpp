use ahash::AHashMap;
use ndc_core::{StaticType, TypeVar};
use ndc_resolver::SymbolId;

/// The type of every value the name resolver declared, natives included,
/// keyed by its symbol. A value keeps its type: an assignment must fit it.
#[derive(Debug, Default)]
pub(crate) struct TypeTable {
    types: AHashMap<SymbolId, StaticType>,
}

impl TypeTable {
    /// Records the type of a value the resolver declared. A function's
    /// placeholder type is replaced this way once its body is analysed.
    pub(crate) fn declare(&mut self, symbol: SymbolId, typ: StaticType) {
        self.types.insert(symbol, typ);
    }

    pub(crate) fn get_type(&self, symbol: SymbolId) -> &StaticType {
        self.types
            .get(&symbol)
            .expect("internal error: a value is analysed after its declaration")
    }

    /// Replaces the settled type variables in every value's type with what
    /// they stand for.
    pub(crate) fn resolve_all(&mut self, vars: &TypeVars) {
        for typ in self.types.values_mut() {
            *typ = vars.resolve(typ);
            debug_assert!(!typ.has_vars(), "unsettled type variable in {typ}");
        }
    }
}

/// What each type variable of the analysis stands for once something settled
/// it. In `let xs = []; xs[0] = 1;`, the element type of `xs` is a variable
/// that the assignment settles as `Int`.
#[derive(Debug, Default)]
pub(crate) struct TypeVars {
    /// What each variable stands for, once settled.
    solutions: Vec<Option<StaticType>>,
    /// What each variable becomes if nothing settles it: `Any` for the
    /// elements of `[]`, which is what `[]` was typed as before.
    fallbacks: Vec<StaticType>,
    /// The variables in the order they were settled, so a [`Snapshot`] can
    /// unsettle the ones settled after it, and the solver can tell which
    /// ones were settled since it last looked.
    trail: Vec<TypeVar>,
}

/// The state of the variables at one point, which [`TypeVars::rollback`]
/// returns to: overload resolution tries each candidate this way.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Snapshot {
    vars: usize,
    trail: usize,
}

impl TypeVars {
    /// A new variable, which becomes `fallback` if nothing settles it.
    pub(crate) fn fresh(&mut self, fallback: StaticType) -> StaticType {
        let var =
            TypeVar(u32::try_from(self.solutions.len()).expect("fewer than 2^32 type variables"));
        self.solutions.push(None);
        self.fallbacks.push(fallback);
        StaticType::Var(var)
    }

    /// How many variables there are, so [`Self::settle_from`] can settle
    /// the ones made after this point.
    pub(crate) fn count(&self) -> usize {
        self.solutions.len()
    }

    fn solution(&self, var: TypeVar) -> Option<&StaticType> {
        self.solutions[var.0 as usize].as_ref()
    }

    /// Settles `var` as `typ`.
    fn settle(&mut self, var: TypeVar, typ: StaticType) {
        debug_assert!(self.solution(var).is_none(), "{var:?} is settled twice");
        self.solutions[var.0 as usize] = Some(typ);
        self.trail.push(var);
    }

    /// How many settlings there have been, to pass to
    /// [`Self::settled_since`] later.
    pub(crate) fn settled_count(&self) -> usize {
        self.trail.len()
    }

    /// The variables settled since there were `count` settlings. A rollback
    /// only undoes settlings made after the snapshot it returns to, so
    /// `count` from before a snapshot stays valid.
    pub(crate) fn settled_since(&self, count: usize) -> &[TypeVar] {
        &self.trail[count.min(self.trail.len())..]
    }

    /// The current state, to [`Self::rollback`] to later.
    pub(crate) fn snapshot(&self) -> Snapshot {
        Snapshot {
            vars: self.solutions.len(),
            trail: self.trail.len(),
        }
    }

    /// Unsettles what was settled since `snapshot`, and forgets the variables
    /// made since.
    pub(crate) fn rollback(&mut self, snapshot: Snapshot) {
        for var in self.trail.drain(snapshot.trail..) {
            self.solutions[var.0 as usize] = None;
        }
        self.solutions.truncate(snapshot.vars);
        self.fallbacks.truncate(snapshot.vars);
    }

    /// Unifies `a` and `b` if they can be made the same type, and otherwise
    /// leaves every variable as it was.
    pub(crate) fn try_unify(&mut self, a: &StaticType, b: &StaticType) -> bool {
        let snapshot = self.snapshot();
        let unified = self.unify(a, b);
        if !unified {
            self.rollback(snapshot);
        }
        unified
    }

    /// The unsettled variables in `typ`, each once, outermost first.
    pub(crate) fn unsettled(&self, typ: &StaticType) -> Vec<TypeVar> {
        let unsettled = std::cell::RefCell::new(Vec::new());
        self.resolve(typ).contains(&|part| {
            if let StaticType::Var(var) = part
                && !unsettled.borrow().contains(var)
            {
                unsettled.borrow_mut().push(*var);
            }
            false
        });
        unsettled.into_inner()
    }

    /// `typ` as an error shows it: the parts not known yet could still be
    /// anything, so they're shown as `Any`.
    pub(crate) fn shown(&self, typ: &StaticType) -> StaticType {
        self.resolve(typ)
            .replace(&|part| matches!(part, StaticType::Var(_)).then_some(StaticType::Any))
    }

    /// Settles each of `vars` that is still unsettled with its fallback.
    pub(crate) fn settle_with_fallbacks(&mut self, vars: &[TypeVar]) {
        for &var in vars {
            if self.solution(var).is_none() {
                let fallback = self.fallbacks[var.0 as usize].clone();
                self.settle(var, fallback);
            }
        }
    }

    /// `typ` with every settled variable replaced by what it stands for.
    pub(crate) fn resolve(&self, typ: &StaticType) -> StaticType {
        typ.replace(&|part| match part {
            StaticType::Var(var) => self.solution(*var).map(|solution| self.resolve(solution)),
            _ => None,
        })
    }

    /// `typ`, or what it stands for if it's a settled variable.
    fn shallow(&self, typ: &StaticType) -> StaticType {
        match typ {
            StaticType::Var(var) => self
                .solution(*var)
                .map_or_else(|| typ.clone(), |solution| self.shallow(solution)),
            _ => typ.clone(),
        }
    }

    /// Makes `a` and `b` the same type by settling the unsettled variables in
    /// either, and returns whether they are the same now. `List<?0>` and
    /// `List<Int>` are, once `?0` is `Int`. `Never` fits any variable without
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
                if self
                    .resolve(other)
                    .contains(&|part| *part == StaticType::Var(*var))
                {
                    return false;
                }
                self.settle(*var, other.clone());
                true
            }
            (StaticType::Option(x), StaticType::Option(y))
            | (StaticType::Sequence(x), StaticType::Sequence(y))
            | (StaticType::List(x), StaticType::List(y))
            | (StaticType::Iterator(x), StaticType::Iterator(y))
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

    /// `typ` with each type parameter, such as `T` in `<T>(List<T>, T)`,
    /// replaced by a new variable of its own. A call does this to the
    /// signature of the overload it picked, so its arguments settle what `T`
    /// stands for at that call.
    pub(crate) fn instantiate(&mut self, typ: &StaticType) -> StaticType {
        let names = std::cell::RefCell::new(Vec::<Box<str>>::new());
        typ.contains(&|part| {
            if let StaticType::Generic(name) = part
                && !names.borrow().contains(name)
            {
                names.borrow_mut().push(name.clone());
            }
            false
        });
        let vars = names
            .into_inner()
            .into_iter()
            .map(|name| (name, self.fresh(StaticType::Any)))
            .collect::<Vec<_>>();
        typ.replace(&|part| match part {
            StaticType::Generic(name) => vars
                .iter()
                .find(|(generic, _)| generic == name)
                .map(|(_, var)| var.clone()),
            _ => None,
        })
    }

    /// Settles the parts of `argument` that aren't known yet by passing it
    /// where a `parameter` is expected, and returns whether it fits there. In
    /// `push(xs, 1)` with `push(List<?0>, ?0)` and `xs: List<?1>`, both
    /// variables become `Int`. A parameter of type `Any` settles what's
    /// unknown in the argument as `Any`, because the function may store
    /// anything in it.
    pub(crate) fn bind_argument(&mut self, parameter: &StaticType, argument: &StaticType) -> bool {
        let (parameter, argument) = (self.shallow(parameter), self.shallow(argument));
        match (&parameter, &argument) {
            (_, StaticType::Never) => true,
            (StaticType::Any, _) => {
                self.settle_unknowns(&argument, &StaticType::Any);
                true
            }
            (StaticType::Var(_), _) | (_, StaticType::Var(_)) => self.unify(&parameter, &argument),
            (StaticType::Option(p), StaticType::Option(a))
            | (StaticType::List(p), StaticType::List(a))
            | (StaticType::Iterator(p), StaticType::Iterator(a))
            | (StaticType::MinHeap(p), StaticType::MinHeap(a))
            | (StaticType::MaxHeap(p), StaticType::MaxHeap(a))
            | (StaticType::Deque(p), StaticType::Deque(a))
            | (StaticType::Sequence(p), StaticType::Sequence(a)) => self.bind_argument(p, a),
            // A function passed as an argument gets the parameter types the
            // function type passes it, and returns what the function type
            // returns: `map(xs, fn(x) => x + 1)` passes `xs`'s elements to
            // `x` and settles `U`. A parameter of type `Any` takes anything,
            // so it settles nothing.
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
                let mut fits = true;
                if let (Some(expected), Some(takes)) = (expected, takes) {
                    // A function taking another number of arguments can't be
                    // called the way the function type says.
                    fits = expected.len() == takes.len();
                    for (expected, takes) in expected.iter().zip(takes) {
                        if self.shallow(takes) != StaticType::Any {
                            fits &= self.bind_argument(takes, expected);
                        }
                    }
                }
                fits && self.bind_argument(p, a)
            }
            (StaticType::Tuple(ps), StaticType::Tuple(args)) if ps.len() == args.len() => {
                ps.iter().zip(args).all(|(p, a)| self.bind_argument(p, a))
            }
            (StaticType::Map { key: pk, value: pv }, StaticType::Map { key: ak, value: av }) => {
                let keys = self.bind_argument(pk, ak);
                let values = self.bind_argument(pv, av);
                keys && values
            }
            // Any sequence passes as a `Sequence<T>` of its elements.
            (StaticType::Sequence(p), _) if argument.sequence_element_type().is_some() => {
                let element = argument
                    .sequence_element_type()
                    .expect("checked by the guard");
                self.bind_argument(p, &element)
            }
            _ => {
                let argument = self.resolve(&argument);
                argument.is_unknown() || argument.is_subtype(&self.resolve(&parameter))
            }
        }
    }

    /// Settles every unsettled variable in `typ` as `to`.
    pub(crate) fn settle_unknowns(&mut self, typ: &StaticType, to: &StaticType) {
        for var in self.unsettled(typ) {
            self.settle(var, to.clone());
        }
    }

    /// Settles every variable made since `first` that nothing settled with
    /// its fallback.
    pub(crate) fn settle_from(&mut self, first: usize) {
        for index in first..self.solutions.len() {
            let var = TypeVar(u32::try_from(index).expect("fewer than 2^32 type variables"));
            self.settle_with_fallbacks(&[var]);
        }
    }
}
