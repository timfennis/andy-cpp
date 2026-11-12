use ahash::{AHashMap, AHashSet};
use ndc_core::{StaticType, TypeVar};
use ndc_lexer::Span;
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
    /// How deep in generic functions each variable belongs: 0 for the
    /// program's top level, 1 for the body of a top-level `fn`. A variable
    /// that a function's body made but that something outside it now refers
    /// to, like `n` in `let xs = []; fn f(n) { xs.push(n) }`, belongs outside.
    levels: Vec<u32>,
    /// The level new variables get.
    level: u32,
    /// The levels lowered so far, with what each was before, so a
    /// [`Snapshot`] can raise them again.
    lowered: Vec<(TypeVar, u32)>,
    /// What each variable may stand for beyond what unifying says, which
    /// settling it checks: the type of the literal `1` is an `Int`, a
    /// `Float` or a `Number`.
    bounds: Vec<Vec<Bound>>,
    /// The variables a bound was added to after they were made, in order,
    /// so a [`Snapshot`] can take the bounds off again.
    bounded: Vec<TypeVar>,
    /// The variables that got a bound, in the order they got it, which are
    /// the types of literals.
    literals: Vec<TypeVar>,
}

/// What a variable may stand for, beyond what unifying says.
#[derive(Debug, Clone)]
enum Bound {
    /// It's the type of an integer literal, like `1`: an `Int`, a `Float`
    /// or a `Number`, or `Any`, which takes anything. It's an `Int` unless
    /// something says otherwise. With the literal's type as it was made,
    /// and where the literal is.
    Number(TypeVar, Span),
    /// It's the type of the elements of a literal that holds a value of
    /// this type, like `[1, 2.5]`, so the value has to fit it. With the
    /// literal, as the type of the elements it was made for, and where the
    /// value is.
    Holds(StaticType, TypeVar, Span),
}

/// The state of the variables at one point, which [`TypeVars::rollback`]
/// returns to: overload resolution tries each candidate this way.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Snapshot {
    vars: usize,
    trail: usize,
    lowered: usize,
    bounded: usize,
}

impl TypeVars {
    /// A new variable, which becomes `fallback` if nothing settles it.
    pub(crate) fn fresh(&mut self, fallback: StaticType) -> StaticType {
        let var =
            TypeVar(u32::try_from(self.solutions.len()).expect("fewer than 2^32 type variables"));
        self.solutions.push(None);
        self.fallbacks.push(fallback);
        self.levels.push(self.level);
        self.bounds.push(Vec::new());
        StaticType::Var(var)
    }

    /// The type of an integer literal, like `1`, which its uses settle as an
    /// `Int`, a `Float` or a `Number`: `1` in `[1.5, 1]` is a Float. It's an
    /// `Int` unless something says otherwise.
    pub(crate) fn fresh_number(&mut self, span: Span) -> StaticType {
        let literal = self.next_var();
        self.fresh_bounded(StaticType::Int, vec![Bound::Number(literal, span)])
    }

    /// The variable [`Self::fresh`] makes next.
    fn next_var(&self) -> TypeVar {
        TypeVar(u32::try_from(self.solutions.len()).expect("fewer than 2^32 type variables"))
    }

    /// The type of the elements of a literal holding values of the types
    /// `held`, which its uses settle as any type each value fits: in
    /// `let xs: List<Any> = [1, 2]`, `Any`. Unless something says otherwise
    /// it's the type the values share, or `Any` if they share none.
    pub(crate) fn fresh_holding(&mut self, held: Vec<(StaticType, Span)>) -> StaticType {
        let literal = self.next_var();
        self.fresh_bounded(
            StaticType::Any,
            held.into_iter()
                .map(|(typ, span)| Bound::Holds(typ, literal, span))
                .collect(),
        )
    }

    /// A literal whose type the literal type `var` holds values of and that
    /// isn't settled yet, as `1`'s is in `[1]`.
    pub(crate) fn held_literal(&self, var: TypeVar) -> Option<TypeVar> {
        self.bounds[var.0 as usize]
            .iter()
            .find_map(|bound| match bound {
                Bound::Holds(held, _, _) => match self.resolve(held) {
                    StaticType::Var(held) if held != var && self.is_literal(held) => Some(held),
                    _ => None,
                },
                Bound::Number(..) => None,
            })
    }

    /// The values the literal type `var` holds, with where each is.
    pub(crate) fn held(&self, var: TypeVar) -> Vec<(StaticType, Span)> {
        self.bounds[var.0 as usize]
            .iter()
            .map(|bound| match bound {
                Bound::Holds(typ, _, span) => (typ.clone(), *span),
                Bound::Number(_, span) => (StaticType::Int, *span),
            })
            .collect()
    }

    /// Whether the literal type `var` holds the values of more than one
    /// literal, as it does once `v = [1]` makes it the type of `v`'s
    /// `["s"]` too. Values of one literal that share no type are held as
    /// `Any`, but two literals of different types are an error.
    pub(crate) fn holds_several_literals(&self, var: TypeVar) -> bool {
        let mut literals = self.bounds[var.0 as usize].iter().map(|bound| match bound {
            Bound::Holds(_, literal, _) | Bound::Number(literal, _) => *literal,
        });
        literals
            .next()
            .is_some_and(|first| literals.any(|literal| literal != first))
    }

    fn fresh_bounded(&mut self, fallback: StaticType, bounds: Vec<Bound>) -> StaticType {
        let typ = self.fresh(fallback);
        let StaticType::Var(var) = typ else {
            unreachable!("a fresh type is a variable");
        };
        self.bounds[var.0 as usize] = bounds;
        self.literals.push(var);
        typ
    }

    /// The unsettled types of literals that `belongs` says to settle, the
    /// ones holding values before the numbers, so a value can still say
    /// what a number is: `1` in `[1, 2.5]` is a Float.
    pub(crate) fn unsettled_literals(
        &self,
        belongs: impl Fn(&Self, TypeVar) -> bool,
    ) -> Vec<TypeVar> {
        let mut literals = Vec::new();
        let mut seen = AHashSet::new();
        for &var in &self.literals {
            if (var.0 as usize) < self.solutions.len()
                && self.solution(var).is_none()
                && !self.bounds[var.0 as usize].is_empty()
                && belongs(self, var)
                && seen.insert(var)
            {
                literals.push(var);
            }
        }
        literals.sort_by_key(|&var| self.is_number(var));
        literals
    }

    /// Forgets the types of literals, once they're all settled at the end
    /// of a batch.
    pub(crate) fn forget_literals(&mut self) {
        debug_assert!(
            self.literals
                .iter()
                .all(|&var| var.0 as usize >= self.solutions.len() || !self.is_literal(var)),
            "a literal's type is left unsettled"
        );
        self.literals.clear();
    }

    /// Whether `var` is the type of an integer literal.
    fn is_number(&self, var: TypeVar) -> bool {
        self.bounds[var.0 as usize]
            .iter()
            .any(|bound| matches!(bound, Bound::Number(..)))
    }

    /// What the unsettled literal type `var` could be when nothing says
    /// otherwise, best first. `None` if that isn't known yet: a list of
    /// numbers is a list of whatever the numbers are, which is up to them.
    pub(crate) fn literal_defaults(&mut self, var: TypeVar) -> Option<Vec<StaticType>> {
        let mut defaults = Vec::new();
        let mut waits = false;
        let mut unknown = None;
        // The literal made first says first what the type is: after `let v
        // = ["s"]; v = [1]`, it's what `v` was.
        let mut bounds = self.bounds[var.0 as usize].iter().collect::<Vec<_>>();
        bounds.sort_by_key(|bound| match bound {
            Bound::Holds(_, literal, _) | Bound::Number(literal, _) => literal.0,
        });
        for bound in bounds {
            let held = match bound {
                Bound::Number(..) => {
                    for number in [StaticType::Int, StaticType::Float, StaticType::Number] {
                        if !defaults.contains(&number) {
                            defaults.push(number);
                        }
                    }
                    continue;
                }
                Bound::Holds(held, _, _) => held,
            };
            match self.resolve(held) {
                StaticType::Var(held) if self.is_literal(held) => waits = true,
                // A value whose type isn't known holds what its type is:
                // `fn wrap(x) => [x]` returns a list of what `x` is.
                StaticType::Var(held) => {
                    unknown.get_or_insert(StaticType::Var(held));
                }
                // A value that never arrives says nothing about the type.
                StaticType::Never => {}
                held => {
                    if !defaults.contains(&held) {
                        defaults.push(held);
                    }
                }
            }
        }
        if defaults.is_empty() && waits {
            return None;
        }
        if defaults.is_empty() {
            // Values that share no type are held as `Any`, which settling
            // the literal's type with a default it doesn't take falls back
            // to.
            defaults.push(unknown.unwrap_or(StaticType::Any));
        } else if !defaults.contains(&StaticType::Any) && self.a_literal_holds_any(var) {
            // A literal whose values share no type is held as `Any`, which
            // any literal can be too: after `let v = [1, "a"]`, `v = ["b"]`
            // holds `Any`s like `v` does.
            defaults.push(StaticType::Any);
        }
        Some(defaults)
    }

    /// Whether a literal whose values `var` holds holds values that share no
    /// type, as `[1, "a"]` does, and is held as `Any` on its own.
    fn a_literal_holds_any(&mut self, var: TypeVar) -> bool {
        let mut literals = Vec::new();
        for bound in &self.bounds[var.0 as usize] {
            match bound {
                Bound::Holds(held, literal, _) => match literals
                    .iter_mut()
                    .find(|(found, _): &&mut (TypeVar, Vec<StaticType>)| found == literal)
                {
                    Some((_, held_by_it)) => held_by_it.push(held.clone()),
                    None => literals.push((*literal, vec![held.clone()])),
                },
                // A number literal is a number.
                Bound::Number(..) => {}
            }
        }
        literals.into_iter().any(|(_, held)| {
            let snapshot = self.snapshot();
            let shared = held[1..].iter().all(|other| self.unify(&held[0], other));
            self.rollback(snapshot);
            !shared
        })
    }

    /// Whether the unsettled literal type `var` could still be settled as
    /// one of its defaults, or waits for the literals it holds to say.
    fn some_default_fits(&mut self, var: TypeVar) -> bool {
        let Some(defaults) = self.literal_defaults(var) else {
            return true;
        };
        defaults.iter().any(|default| {
            if self
                .resolve(default)
                .contains(&|part| *part == StaticType::Var(var))
            {
                return false;
            }
            let snapshot = self.snapshot();
            let fits = self.settle(var, default.clone());
            self.rollback(snapshot);
            fits
        })
    }

    /// What the unsettled literal type `var` is shown as: what it would be
    /// if nothing said otherwise, as far as that's known.
    pub(crate) fn preview(&self, var: TypeVar) -> StaticType {
        if self.is_number(var) {
            return StaticType::Int;
        }
        let mut held = Vec::new();
        for bound in &self.bounds[var.0 as usize] {
            // A literal that holds itself, like `[xs]` pushed to `xs`, says
            // nothing more about what it is.
            if let Bound::Holds(typ, _, _) = bound
                && !self
                    .resolve(typ)
                    .contains(&|part| *part == StaticType::Var(var))
            {
                let shown = self.shown(typ);
                if !held.contains(&shown) {
                    held.push(shown);
                }
            }
        }
        match &held[..] {
            [only] => only.clone(),
            _ => StaticType::Any,
        }
    }

    /// Whether `var` is unsettled and has bounds, as a literal's type does.
    pub(crate) fn is_literal(&self, var: TypeVar) -> bool {
        self.solution(var).is_none() && !self.bounds[var.0 as usize].is_empty()
    }

    /// Whether `typ` has a part that's a literal's type not settled yet.
    pub(crate) fn has_unsettled_literals(&self, typ: &StaticType) -> bool {
        self.unsettled(typ)
            .into_iter()
            .any(|var| self.is_literal(var))
    }

    /// Settles `var` as `typ`, and returns whether `typ` is within its
    /// bounds, as [`Self::unify`] does.
    pub(crate) fn settle_literal(&mut self, var: TypeVar, typ: &StaticType) -> bool {
        self.unify(&StaticType::Var(var), typ)
    }

    /// Starts the body of a generic function: the variables made until
    /// [`Self::leave_level`] belong to it.
    pub(crate) fn enter_level(&mut self) {
        self.level += 1;
    }

    /// Ends the body [`Self::enter_level`] started. The variables it made
    /// that nothing outside refers to can then be generalized.
    pub(crate) fn leave_level(&mut self) {
        self.level -= 1;
    }

    /// Whether `var` is unsettled and belongs to a generic function's body
    /// that has ended, so it can become one of the function's type
    /// parameters.
    pub(crate) fn is_generalizable(&self, var: TypeVar) -> bool {
        self.solution(var).is_none() && self.levels[var.0 as usize] > self.level
    }

    /// Settles `var` as the type parameter `name` of the generic function
    /// whose body made it. A literal's type is settled before, so `var` has
    /// no bounds.
    pub(crate) fn generalize(&mut self, var: TypeVar, name: &str) {
        debug_assert!(
            self.bounds[var.0 as usize].is_empty(),
            "the literal type {var:?} is generalized"
        );
        self.settle(var, StaticType::Generic(name.into()));
    }

    /// How many variables there are, so [`Self::settle_from`] can settle
    /// the ones made after this point.
    pub(crate) fn count(&self) -> usize {
        self.solutions.len()
    }

    fn solution(&self, var: TypeVar) -> Option<&StaticType> {
        self.solutions[var.0 as usize].as_ref()
    }

    /// Settles `var` as `typ`, and returns whether `typ` is within the
    /// bounds of `var`. When `typ` is another variable, it gets them. On a
    /// mismatch, `var` and the variables settled meanwhile stay settled.
    fn settle(&mut self, var: TypeVar, typ: StaticType) -> bool {
        debug_assert!(self.solution(var).is_none(), "{var:?} is settled twice");
        // What `var` stands for now belongs where `var` does.
        let level = self.levels[var.0 as usize];
        if level < self.level {
            for inner in self.unsettled(&typ) {
                let inner_level = self.levels[inner.0 as usize];
                if inner_level > level {
                    self.lowered.push((inner, inner_level));
                    self.levels[inner.0 as usize] = level;
                }
            }
        }
        let shallow = self.shallow(&typ);
        self.solutions[var.0 as usize] = Some(typ);
        self.trail.push(var);
        let bounds = self.bounds[var.0 as usize].clone();
        if bounds.is_empty() {
            return true;
        }
        match shallow {
            StaticType::Var(other) => {
                for bound in bounds {
                    // A literal holding a value of the type the other
                    // variable already is says nothing more.
                    if let Bound::Holds(held, _, _) = &bound
                        && self.shallow(held) == StaticType::Var(other)
                    {
                        continue;
                    }
                    self.bounds[other.0 as usize].push(bound);
                    self.bounded.push(other);
                    self.literals.push(other);
                }
                // The values of two literals have to have a type in common:
                // `["s"]` and `[1]` can't be one literal's type.
                !self.holds_several_literals(other) || self.some_default_fits(other)
            }
            typ => bounds.iter().all(|bound| match bound {
                Bound::Number(..) => matches!(
                    typ,
                    StaticType::Int | StaticType::Float | StaticType::Number | StaticType::Any
                ),
                Bound::Holds(held, _, _) => self.bind_argument(&typ, held),
            }),
        }
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
            lowered: self.lowered.len(),
            bounded: self.bounded.len(),
        }
    }

    /// Unsettles what was settled since `snapshot`, and forgets the variables
    /// made since.
    pub(crate) fn rollback(&mut self, snapshot: Snapshot) {
        for var in self.trail.drain(snapshot.trail..) {
            self.solutions[var.0 as usize] = None;
        }
        for (var, level) in self.lowered.drain(snapshot.lowered..).rev() {
            self.levels[var.0 as usize] = level;
        }
        for var in self.bounded.drain(snapshot.bounded..).rev() {
            if (var.0 as usize) < snapshot.vars {
                self.bounds[var.0 as usize].pop();
            }
        }
        self.solutions.truncate(snapshot.vars);
        self.fallbacks.truncate(snapshot.vars);
        self.levels.truncate(snapshot.vars);
        self.bounds.truncate(snapshot.vars);
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
    /// anything, so they're shown as `Any`, except a literal's, which is
    /// shown as what it is unless something says otherwise: `[1]` as a
    /// `List<Int>`.
    pub(crate) fn shown(&self, typ: &StaticType) -> StaticType {
        self.resolve(typ).replace(&|part| match part {
            StaticType::Var(var) if self.is_literal(*var) => Some(self.preview(*var)),
            StaticType::Var(_) => Some(StaticType::Any),
            _ => None,
        })
    }

    /// Settles each of `vars` that is still unsettled with its fallback. A
    /// literal's type gets the first of its defaults within its bounds.
    pub(crate) fn settle_with_fallbacks(&mut self, vars: &[TypeVar]) {
        for &var in vars {
            if self.solution(var).is_some() {
                continue;
            }
            let defaults = match self.literal_defaults(var) {
                Some(defaults) if !self.bounds[var.0 as usize].is_empty() => defaults,
                _ => vec![self.fallbacks[var.0 as usize].clone()],
            };
            let settled = defaults.iter().any(|default| {
                // A variable can't stand for a type that contains it.
                if self
                    .resolve(default)
                    .contains(&|part| *part == StaticType::Var(var))
                {
                    return false;
                }
                let snapshot = self.snapshot();
                let settled = self.settle(var, default.clone());
                if !settled {
                    self.rollback(snapshot);
                }
                settled
            });
            if !settled {
                // `Any` takes anything a literal holds.
                self.settle(var, StaticType::Any);
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
                self.settle(*var, other.clone())
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
        self.instantiate_with(typ, &mut Vec::new())
    }

    /// [`Self::instantiate`], with the variables already made for some type
    /// parameters in `made`, which gets the ones made for the others. A
    /// generic function's constraints are instantiated with its signature.
    pub(crate) fn instantiate_with(
        &mut self,
        typ: &StaticType,
        made: &mut Vec<(Box<str>, StaticType)>,
    ) -> StaticType {
        let names = std::cell::RefCell::new(Vec::<Box<str>>::new());
        typ.contains(&|part| {
            if let StaticType::Generic(name) = part
                && !names.borrow().contains(name)
            {
                names.borrow_mut().push(name.clone());
            }
            false
        });
        for name in names.into_inner() {
            if !made.iter().any(|(generic, _)| *generic == name) {
                let var = self.fresh(StaticType::Any);
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
    /// `push(xs, 1)` with `push(List<?0>, ?0)` and `xs: List<?1>`, both
    /// variables become `Int`. Passing makes the two types the same, except
    /// that a parameter of type `Any` takes anything, and an argument of
    /// type `Any` goes anywhere, as it's checked as the program runs. Inside
    /// a container, a function type, an `Option` or a tuple, `Any` is just
    /// another type: a `List<Int>` doesn't fit a `List<Any>`. A
    /// `Sequence<T>` takes any kind of sequence, and settles `T` from its
    /// elements.
    pub(crate) fn bind_argument(&mut self, parameter: &StaticType, argument: &StaticType) -> bool {
        let (parameter, argument) = (self.shallow(parameter), self.shallow(argument));
        match (&parameter, &argument) {
            (_, StaticType::Never) | (StaticType::Any, _) => true,
            // A value of type `Any` is checked where it goes, so it leaves a
            // literal's type what the literal says: `n` in `let n = 1;
            // n = a` is still an Int.
            (StaticType::Var(var), StaticType::Any) if self.is_literal(*var) => true,
            (StaticType::Var(_), _) | (_, StaticType::Var(_)) => self.unify(&parameter, &argument),
            (_, StaticType::Any) => true,
            // A function type that says nothing about the parameters takes
            // any function that returns what it says.
            (
                StaticType::Function {
                    parameters: None,
                    return_type: p,
                },
                StaticType::Function { return_type: a, .. },
            ) => self.unify(p, a),
            (StaticType::Sequence(p), StaticType::Sequence(a)) => self.bind_argument(p, a),
            (StaticType::Sequence(p), _) if argument.sequence_element_type().is_some() => {
                let element = argument
                    .sequence_element_type()
                    .expect("checked by the guard");
                self.bind_argument(p, &element)
            }
            _ => self.unify(&parameter, &argument),
        }
    }

    /// Settles every unsettled variable in `typ` as `to`.
    pub(crate) fn settle_unknowns(&mut self, typ: &StaticType, to: &StaticType) {
        for var in self.unsettled(typ) {
            if self.solution(var).is_none() {
                self.settle(var, to.clone());
            }
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

#[cfg(test)]
mod tests {
    use super::TypeVars;
    use ndc_core::StaticType;
    use ndc_lexer::Span;

    fn span() -> Span {
        Span::synthetic()
    }

    #[test]
    fn a_number_literal_is_a_number_or_any() {
        let mut vars = TypeVars::default();
        for typ in [
            StaticType::Int,
            StaticType::Float,
            StaticType::Number,
            StaticType::Any,
        ] {
            let literal = vars.fresh_number(span());
            assert!(vars.try_unify(&literal, &typ), "{typ}");
        }
        let literal = vars.fresh_number(span());
        assert!(!vars.try_unify(&literal, &StaticType::String));
    }

    #[test]
    fn a_literal_holds_what_each_value_fits() {
        let mut vars = TypeVars::default();
        let number = vars.fresh_number(span());
        let element =
            vars.fresh_holding(vec![(number.clone(), span()), (StaticType::Float, span())]);
        assert!(!vars.try_unify(&element, &StaticType::String));
        assert!(vars.try_unify(&element, &StaticType::Float));
        // Holding a Float made the number a Float too.
        assert_eq!(vars.resolve(&number), StaticType::Float);
    }

    #[test]
    fn a_rollback_takes_back_the_bounds_another_variable_got() {
        let mut vars = TypeVars::default();
        let literal = vars.fresh_number(span());
        let other = vars.fresh(StaticType::Any);
        let snapshot = vars.snapshot();
        assert!(vars.unify(&literal, &other));
        // `other` is the literal now, so it can't be a String.
        assert!(!vars.try_unify(&other, &StaticType::String));
        vars.rollback(snapshot);
        assert!(vars.try_unify(&other, &StaticType::String));
    }

    #[test]
    fn a_literal_defaults_to_what_its_values_share() {
        let mut vars = TypeVars::default();
        let number = vars.fresh_number(span());
        let StaticType::Var(element) =
            vars.fresh_holding(vec![(number, span()), (StaticType::Float, span())])
        else {
            unreachable!("a literal's type is a variable");
        };
        assert_eq!(
            vars.literal_defaults(element),
            Some(vec![StaticType::Float])
        );
        let StaticType::Var(mixed) = vars.fresh_holding(vec![
            (StaticType::String, span()),
            (StaticType::Int, span()),
        ]) else {
            unreachable!("a literal's type is a variable");
        };
        vars.settle_with_fallbacks(&[mixed]);
        assert_eq!(vars.resolve(&StaticType::Var(mixed)), StaticType::Any);
    }
}
