use ahash::{AHashMap, AHashSet};
use bounds::{Bound, Bounds, Held, Literals, takes_a_number};
use ena::unify::{InPlace, InPlaceUnificationTable, NoError, UnifyKey, UnifyValue};
use ndc_core::{StaticType, TypeVar};
use ndc_lexer::Span;
use ndc_resolver::SymbolId;
use std::cell::RefCell;

mod bounds;

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
    /// they stand for. Unless `open` allows it, as after a REPL line, none
    /// is left unsettled.
    pub(crate) fn resolve_all(&mut self, vars: &TypeVars, open: bool) {
        for typ in self.types.values_mut() {
            *typ = vars.resolve(typ);
            debug_assert!(open || !typ.has_vars(), "unsettled type variable in {typ}");
        }
    }
}

/// What [`TypeVars::bind_arguments`] puts off while it passes the arguments.
#[derive(Debug)]
struct Deferred {
    /// The variables of the parameters.
    variables: AHashSet<TypeVar>,
    /// Each value passed where one of them is expected directly or as what
    /// an `Iterable` holds.
    passed: Vec<(TypeVar, StaticType)>,
}

/// What each type variable of the analysis stands for once something settled
/// it. In `let xs = []; xs[0] = 1;`, the element type of `xs` is a variable
/// that the assignment settles as `Int`.
#[derive(Debug, Default)]
pub(crate) struct TypeVars {
    /// Which variables were unified with which, and what each set of them
    /// stands for once settled. Each set is one of its variables, the one it
    /// was last unified with, whose fallback, level and bounds are the set's.
    /// Finding it shortens the way there, which needs `&mut`.
    table: RefCell<InPlaceUnificationTable<Key>>,
    /// What each variable becomes if nothing settles it: `Unknown`, as
    /// for what `[]` holds, or `Any` for a literal's type whose values
    /// don't say what it is, or a failed check's type: see
    /// [`Self::fall_back_as_errors`].
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
    /// What each variable may stand for beyond what unifying says, which
    /// settling it checks: the type of the literal `1` is an `Int`, a
    /// `Float` or a `Number`.
    bounds: Vec<Bounds>,
    /// What was changed since the variables were made, other than what the
    /// trail says was settled, so a [`Snapshot`] can take it back.
    undo: Vec<Undo>,
    /// The variables that got a bound, in the order they got it, which are
    /// the types of literals.
    literals: Vec<TypeVar>,
    /// Where each literal holding values is, keyed by the type its elements
    /// were made as: the whole of `[1, "a"]`, or of an `if`.
    literal_spans: AHashMap<TypeVar, Span>,
    /// The literals settled as [`StaticType::Error`] because their values
    /// share no type, which the analyser reports.
    unshared: Vec<(TypeVar, Unshared)>,
    /// The types of the `if`s and `match`es whose branches aren't known yet,
    /// as the sets they belong to: see [`Self::mark_join`].
    joins: AHashSet<TypeVar>,
    /// Those an `Any` was expected of.
    expects_any: AHashSet<TypeVar>,
    /// While [`Self::bind_arguments`] runs, the values passed for the type
    /// parameters, which decide them once every argument is in.
    deferred: Option<Deferred>,
    /// The variables made as what an empty `[]`, `%{}` or `None` holds, in
    /// the order they were made: see [`Self::fresh_contents`].
    contents: Vec<TypeVar>,
}

/// Two values of a literal that share no type, as `1` and `"a"` in `[1,
/// "a"]`, with where the literal is.
#[derive(Debug, Clone)]
pub(crate) struct Unshared {
    /// The literal's element type, as it was made.
    pub(crate) literal: TypeVar,
    pub(crate) first: StaticType,
    pub(crate) second: StaticType,
    pub(crate) span: Span,
}

/// A change to the variables, other than settling one, with what it
/// changed, which a [`Snapshot`] takes back.
#[derive(Debug)]
enum Undo {
    /// The variable belonged at this level before.
    Lowered(TypeVar, u32),
    /// The variable had these bounds before.
    Bounds(TypeVar, Bounds),
    /// The variable had this fallback before.
    Fallback(TypeVar, StaticType),
    /// The variable was marked as the type of a join.
    MarkedJoin(TypeVar),
    /// An `Any` was expected of the join whose type the variable is.
    ExpectedAny(TypeVar),
}

/// A type variable as a key of [`TypeVars::table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Key(u32);

impl UnifyKey for Key {
    type Value = Solution;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(index: u32) -> Self {
        Self(index)
    }

    fn tag() -> &'static str {
        "TypeVar"
    }

    /// The variable another is unified with stands for both, as `b` does
    /// once `a` is settled as `b`, so its fallback, level and bounds are the
    /// set's.
    fn order_roots(a: Self, _: &Solution, b: Self, _: &Solution) -> Option<(Self, Self)> {
        Some((b, a))
    }
}

/// What a set of unified variables stands for, once something settled it as
/// a type that isn't a variable.
#[derive(Debug, Clone, Default)]
struct Solution(Option<StaticType>);

impl UnifyValue for Solution {
    type Error = NoError;

    fn unify_values(a: &Self, b: &Self) -> Result<Self, NoError> {
        Ok(Self(a.0.clone().or_else(|| b.0.clone())))
    }
}

/// The state of the variables at one point, which [`TypeVars::rollback`]
/// returns to: see [`TypeVars::attempt_in`] and [`TypeVars::probe_in`],
/// the only ways to take one.
struct Snapshot {
    table: ena::unify::Snapshot<InPlace<Key>>,
    vars: usize,
    trail: usize,
    undo: usize,
}

impl TypeVars {
    /// A new variable, which is [`StaticType::Unknown`] if nothing settles
    /// it: a type parameter or an `Any` takes it, but no other type is. In
    /// `let f = fn(x) => x + 1;` with `f` never called, no `+` takes `x`.
    pub(crate) fn fresh(&mut self) -> StaticType {
        self.fresh_with(StaticType::Unknown, self.level)
    }

    /// A new variable of `level`, which becomes `fallback` if nothing
    /// settles it.
    fn fresh_with(&mut self, fallback: StaticType, level: u32) -> StaticType {
        let var = self.next_var();
        self.table.get_mut().new_key(Solution::default());
        self.fallbacks.push(fallback);
        self.levels.push(level);
        self.bounds.push(Bounds::default());
        StaticType::Var(var)
    }

    /// What an empty `[]`, `%{}` or `None` holds, which its uses settle. If
    /// nothing does, it's [`StaticType::Unknown`], which a type parameter or
    /// an `Any` takes, but no other type is: `print([])` prints `[]`, and
    /// `pick([])` with `pick([Int])` and `pick([Any])` is an error.
    /// It doesn't depend on what the calls the literal is passed to need,
    /// so it's the same whatever order they're solved in.
    pub(crate) fn fresh_contents(&mut self) -> StaticType {
        let typ = self.fresh();
        if let StaticType::Var(var) = typ {
            self.contents.push(var);
        }
        typ
    }

    /// The unsettled variables made since `first` that stand for what an
    /// empty `[]`, `%{}` or `None` holds, as the variable made for it does,
    /// or one it was unified with does in its place.
    pub(crate) fn unsettled_contents(&self, first: usize) -> Vec<TypeVar> {
        let mut unsettled = Vec::new();
        for &var in &self.contents {
            if let StaticType::Var(now) = self.resolve(&StaticType::Var(var))
                && now.0 as usize >= first
                && !unsettled.contains(&now)
            {
                unsettled.push(now);
            }
        }
        unsettled
    }

    /// Whether the unsettled variable `var` stands for what an empty `[]`,
    /// `%{}` or `None` holds.
    pub(crate) fn is_contents(&self, var: TypeVar) -> bool {
        self.contents
            .iter()
            .any(|&made| self.resolve(&StaticType::Var(made)) == StaticType::Var(var))
    }

    /// Settles each of `vars`, what empty literals hold, as
    /// [`StaticType::Unknown`].
    pub(crate) fn settle_contents(&mut self, vars: &[TypeVar]) {
        for &var in vars {
            if self.solution(var).is_none() {
                self.settle(var, StaticType::Unknown);
            }
        }
    }

    /// Marks `typ`, if it's an unsettled variable, as the type of an `if` or
    /// a `match` whose branches aren't known yet. Where an `Any` is expected
    /// of it, its branches may share no type, as those of one whose branches
    /// are known may: in `g(if c { x } else { [] })` with `g(v: Any)`, it's
    /// an `Any` once `x` is an Int. Branches that share a type keep it: see
    /// [`Self::any_is_expected_of`].
    pub(crate) fn mark_join(&mut self, typ: &StaticType) {
        if let StaticType::Var(var) = self.shallow(typ)
            && self.joins.insert(var)
        {
            self.undo.push(Undo::MarkedJoin(var));
        }
    }

    /// Whether an `Any` was expected of the `if` or `match` whose type
    /// `typ` is, when its branches weren't known yet, which then makes it
    /// an `Any` if they share no type.
    pub(crate) fn any_is_expected_of(&self, typ: &StaticType) -> bool {
        matches!(typ, StaticType::Var(var) if self.expects_any.contains(&self.root(*var)))
    }

    /// The type of an integer literal, like `1`, which its uses settle as an
    /// `Int`, a `Float` or a `Number`: `1` in `[1.5, 1]` is a Float. It's an
    /// `Int` unless something says otherwise.
    pub(crate) fn fresh_number(&mut self, span: Span) -> StaticType {
        let literal = self.next_var();
        self.fresh_bounded(vec![Bound::Number(literal, span)])
    }

    /// The variable [`Self::fresh`] makes next.
    fn next_var(&self) -> TypeVar {
        TypeVar(u32::try_from(self.count()).expect("fewer than 2^32 type variables"))
    }

    /// The type of the elements of a literal at `span` holding values of
    /// the types `held`, which its uses settle as any type each value fits:
    /// in `let xs: [Any] = [1, "a"]`, `Any`. Unless something says
    /// otherwise it's the type the values share. Values that share none
    /// are an error, as `Any` has to be asked for.
    pub(crate) fn fresh_holding(
        &mut self,
        held: Vec<(StaticType, Span)>,
        span: Span,
    ) -> StaticType {
        let literal = self.next_var();
        self.literal_spans.insert(literal, span);
        self.fresh_bounded(
            held.into_iter()
                .map(|(typ, span)| Bound::Holds(typ, literal, span))
                .collect(),
        )
    }

    /// A literal whose type the literal type `var` holds values of and that
    /// isn't settled yet, as `1`'s is in `[1]`.
    pub(crate) fn held_literal(&self, var: TypeVar) -> Option<TypeVar> {
        self.held_literals(var).into_iter().next()
    }

    /// The unsettled literal types that each unsettled literal type is
    /// held by: the element type of `[1]` holds the `1`'s.
    pub(crate) fn holders(&self) -> AHashMap<TypeVar, Vec<TypeVar>> {
        let mut holders = AHashMap::<TypeVar, Vec<TypeVar>>::new();
        let mut seen = AHashSet::new();
        for &holder in &self.literals {
            if (holder.0 as usize) >= self.count()
                || !self.is_literal(holder)
                || !seen.insert(holder)
            {
                continue;
            }
            for held in self.held_literals(holder) {
                let held_by = holders.entry(held).or_default();
                if !held_by.contains(&holder) {
                    held_by.push(holder);
                }
            }
        }
        holders
    }

    /// The unsettled literal types whose values the literal type `var`
    /// holds: the `1`'s and the `2`'s in `[1, 2]`.
    pub(crate) fn held_literals(&self, var: TypeVar) -> Vec<TypeVar> {
        let mut held_literals = Vec::new();
        let mut seen = AHashSet::new();
        for bound in self.bounds[var.0 as usize].iter() {
            if let Bound::Holds(held, _, _) = bound
                && let StaticType::Var(held) = self.resolve(held)
                && held != var
                && self.is_literal(held)
                && seen.insert(held)
            {
                held_literals.push(held);
            }
        }
        held_literals
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
        self.bounds[var.0 as usize].summary().literals == Literals::Several
    }

    /// A new literal's type, within `bounds`. If nothing settles it, it's
    /// the first of its [`Self::literal_defaults`] that fits, or, while
    /// what it holds doesn't say yet, `Any`, which takes whatever it holds.
    fn fresh_bounded(&mut self, bounds: Vec<Bound>) -> StaticType {
        let typ = self.fresh_with(StaticType::Any, self.level);
        let StaticType::Var(var) = typ else {
            unreachable!("a fresh type is a variable");
        };
        self.bounds[var.0 as usize] = Bounds::new(bounds);
        self.literals.push(var);
        typ
    }

    /// The unsettled types of literals that `belongs` says to settle, the
    /// ones holding values before the numbers, so a value can still say
    /// what a number is: `1` in `[1, 2.5]` is a Float. Those whose values
    /// share no type come last, so a call waiting for the others can still
    /// ask for them as `Any`s: in `f(if c { 1 } else { "a" }, 0)` with
    /// `f(x: Any, y: Int)`, once the `0` is an Int.
    pub(crate) fn unsettled_literals(
        &mut self,
        belongs: impl Fn(&Self, TypeVar) -> bool,
    ) -> Vec<TypeVar> {
        let mut literals = Vec::new();
        let mut seen = AHashSet::new();
        for &var in &self.literals {
            if (var.0 as usize) < self.count()
                && self.solution(var).is_none()
                && !self.bounds[var.0 as usize].is_empty()
                && belongs(self, var)
                && seen.insert(var)
            {
                literals.push(var);
            }
        }
        // What the literals a literal holds take decides whether its uses
        // fit, so they're settled last: in `n[0]` on `n = [1]`, whether `[]`
        // gives the `1` back depends on the index `0` being an Int.
        let held = literals
            .iter()
            .flat_map(|&var| self.bounds[var.0 as usize].iter())
            .filter_map(|bound| match bound {
                Bound::Holds(held, _, _) => match self.resolve(held) {
                    StaticType::Var(held) => Some(held),
                    _ => None,
                },
                Bound::Number(..) => None,
            })
            .collect::<AHashSet<_>>();
        let mut keyed = literals
            .into_iter()
            .map(|var| {
                let number = self.is_number(var);
                let unshared = !number && self.unshared_values(var).is_some();
                ((unshared, number, held.contains(&var)), var)
            })
            .collect::<Vec<_>>();
        keyed.sort_by_key(|(key, _)| *key);
        keyed.into_iter().map(|(_, var)| var).collect()
    }

    /// Forgets the types of literals, once they're all settled at the end
    /// of a batch.
    pub(crate) fn forget_literals(&mut self) {
        debug_assert!(
            self.literals
                .iter()
                .all(|&var| var.0 as usize >= self.count() || !self.is_literal(var)),
            "a literal's type is left unsettled"
        );
        self.literals.clear();
    }

    /// Whether `var` is the unsettled type of a number literal, or of a
    /// literal that holds only those, as the elements of `[1, 2]` are. A
    /// literal can hold its own type, as `xs` holds `x` in
    /// `let xs = [x]; x = xs.pop();`, which says nothing either way.
    pub(crate) fn is_number_literal(&self, var: TypeVar) -> bool {
        if self.solution(var).is_some() || !self.is_literal(var) {
            return false;
        }
        let mut seen = AHashSet::from_iter([var]);
        let mut unseen = vec![var];
        let mut holds_a_number = false;
        while let Some(var) = unseen.pop() {
            if self.is_number(var) {
                holds_a_number = true;
                continue;
            }
            for bound in self.bounds[var.0 as usize].iter() {
                let Bound::Holds(held, _, _) = bound else {
                    continue;
                };
                match self.resolve(held) {
                    StaticType::Var(held) if self.is_literal(held) => {
                        if seen.insert(held) {
                            unseen.push(held);
                        }
                    }
                    _ => return false,
                }
            }
        }
        holds_a_number
    }

    /// The unsettled literals in `typ` and those they hold, at any depth:
    /// in `[(1, "a")]`, the list's elements, and the `1`.
    pub(crate) fn literals_within(&self, typ: &StaticType) -> Vec<TypeVar> {
        let literals_of = |typ: &StaticType| {
            self.unsettled(typ)
                .into_iter()
                .filter(|&var| self.is_literal(var))
        };
        let mut within = Vec::new();
        let mut seen = AHashSet::new();
        let mut unseen = literals_of(typ).collect::<Vec<_>>();
        while let Some(var) = unseen.pop() {
            if seen.insert(var) {
                within.push(var);
                for held in self.held_types(var) {
                    unseen.extend(literals_of(&held));
                }
            }
        }
        within
    }

    /// Whether a part of `typ` is the type of a number literal, settled or
    /// not, as the type of `total` in `let total = 0` is.
    pub(crate) fn has_number_literal(&self, typ: &StaticType) -> bool {
        typ.contains(&|part| match part {
            StaticType::Var(var) => {
                self.is_number(*var)
                    || self
                        .solution(*var)
                        .is_some_and(|solution| self.has_number_literal(&solution))
            }
            _ => false,
        })
    }

    /// Whether `var` is the type of an integer literal.
    pub(crate) fn is_number(&self, var: TypeVar) -> bool {
        self.bounds[var.0 as usize].summary().numbers
    }

    /// What the unsettled literal type `var` could be when nothing says
    /// otherwise, best first. `None` if that isn't known yet: a list of
    /// numbers is a list of whatever the numbers are, which is up to them.
    pub(crate) fn literal_defaults(&mut self, var: TypeVar) -> Option<Vec<StaticType>> {
        let mut defaults = Vec::new();
        let mut seen = AHashSet::new();
        let mut waits = false;
        let mut unknown = None;
        // The literal made first says first what the type is: after `let v
        // = ["s"]; v = [1]`, it's what `v` was.
        let mut bounds = self.bounds[var.0 as usize].iter().collect::<Vec<_>>();
        bounds.sort_by_key(|bound| bound.literal().0);
        for bound in bounds {
            let held = match bound {
                Bound::Number(..) => {
                    for number in [StaticType::Int, StaticType::Float, StaticType::Number] {
                        if seen.insert(number.clone()) {
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
                    if seen.insert(held.clone()) {
                        defaults.push(held);
                    }
                }
            }
        }
        if defaults.is_empty() && waits {
            return None;
        }
        if defaults.is_empty() {
            // Values that never arrive are held as `Any`.
            defaults.push(unknown.unwrap_or(StaticType::Any));
        } else if !seen.contains(&StaticType::Any) && self.unshared_values(var).is_some() {
            // A literal whose values share no type is an error, which is
            // what's left when nothing asked for `Any`: in `let xs = [1,
            // "a"]`, the elements are neither Ints nor Strings.
            defaults.push(StaticType::Error);
        }
        Some(defaults)
    }

    /// Whether the values of a literal whose values `var` holds share no
    /// type, as those of `[1, "a"]` don't.
    pub(crate) fn holds_unshared_values(&mut self, var: TypeVar) -> bool {
        self.unshared_values(var).is_some()
    }

    /// Two values of a literal whose values `var` holds that share no type,
    /// as `1` and `"a"` in `[1, "a"]`, looking first into the literals it
    /// holds, as an `if` among the branches of another `if`.
    fn unshared_values(&mut self, var: TypeVar) -> Option<Unshared> {
        self.unshared_values_within(var, &mut Vec::new())
    }

    fn unshared_values_within(
        &mut self,
        var: TypeVar,
        seen: &mut Vec<TypeVar>,
    ) -> Option<Unshared> {
        seen.push(var);
        let mut literals: Vec<(TypeVar, Vec<StaticType>)> = Vec::new();
        for bound in self.bounds[var.0 as usize].iter() {
            match bound {
                Bound::Holds(held, literal, _) => {
                    match literals.iter_mut().find(|(found, _)| found == literal) {
                        Some((_, held_by_it)) => held_by_it.push(held.clone()),
                        None => literals.push((*literal, vec![held.clone()])),
                    }
                }
                // A number literal is a number.
                Bound::Number(..) => {}
            }
        }
        for (literal, held) in literals {
            // A literal holding a value of type `Any` holds `Any`s, which
            // every value is.
            if self.plainly_shared(&held)
                || held
                    .iter()
                    .any(|typ| matches!(self.resolve(typ), StaticType::Any | StaticType::Error))
            {
                continue;
            }
            for typ in &held {
                if let StaticType::Var(inner) = self.resolve(typ)
                    && self.is_literal(inner)
                    && !seen.contains(&inner)
                    && let Some(unshared) = self.unshared_values_within(inner, seen)
                {
                    return Some(unshared);
                }
            }
            let found = self.probe(|vars| {
                held[1..].iter().find_map(|other| {
                    (!vars.try_unify(&held[0], other))
                        .then(|| (vars.shown(&held[0]), vars.shown(other)))
                })
            });
            if let Some((first, second)) = found {
                let span = self.literal_spans[&literal];
                return Some(Unshared {
                    literal,
                    first,
                    second,
                    span,
                });
            }
        }
        None
    }

    /// Whether `held` are plainly of one type without trying to unify
    /// them: numbers, with at most one kind of number written out, or
    /// values of one known type. A literal of many numbers, like `[0, 1,
    /// …, 5000]`, is checked this way.
    fn plainly_shared(&self, held: &[StaticType]) -> bool {
        let mut numbers = false;
        let mut known: Option<StaticType> = None;
        for typ in held {
            match self.resolve(typ) {
                StaticType::Var(var) if self.is_number(var) && self.solution(var).is_none() => {
                    numbers = true;
                }
                StaticType::Var(_) => return false,
                typ if typ.has_vars() => return false,
                typ => match &known {
                    None => known = Some(typ),
                    Some(other) if *other == typ => {}
                    Some(_) => return false,
                },
            }
        }
        !numbers
            || matches!(
                known,
                None | Some(StaticType::Int | StaticType::Float | StaticType::Number)
            )
    }

    /// Settles the unsettled literal type `var` as `default`, one of its
    /// [`Self::literal_defaults`], and returns whether it's within its
    /// bounds. One whose values share no type is remembered to be reported.
    pub(crate) fn settle_default(&mut self, var: TypeVar, default: &StaticType) -> bool {
        match default {
            StaticType::Error => self.settle_unshared(var),
            default => self.settle_literal(var, default),
        }
    }

    /// Settles the unsettled literal type `var` as [`StaticType::Error`],
    /// as its values share no type, and remembers it to be reported.
    fn settle_unshared(&mut self, var: TypeVar) -> bool {
        let unshared = self.unshared_values(var);
        let settled = self.settle_literal(var, &StaticType::Error);
        if let Some(unshared) = unshared {
            self.unshared.push((var, unshared));
        }
        settled
    }

    /// The literals settled as errors since this was last called, as their
    /// values share no type, with two of the values. One that a rollback
    /// unsettled again isn't among them.
    pub(crate) fn take_unshared(&mut self) -> Vec<Unshared> {
        if self.unshared.is_empty() {
            return Vec::new();
        }
        let mut seen = AHashSet::new();
        std::mem::take(&mut self.unshared)
            .into_iter()
            .filter(|(var, _)| {
                seen.insert(*var) && self.resolve(&StaticType::Var(*var)) == StaticType::Error
            })
            .map(|(_, unshared)| unshared)
            .collect()
    }

    /// Whether the unsettled literal type `var` could still be settled as
    /// one of its defaults, or waits for the literals it holds to say.
    fn some_default_fits(&mut self, var: TypeVar) -> bool {
        // Values of one type with nothing left to find out about it fit that
        // type, and numbers fit a type of number, which doesn't need a look
        // at each value: unifying the elements of a literal of thousands of
        // tuples asks this once for each.
        let summary = self.bounds[var.0 as usize].summary();
        let plainly = match &summary.held {
            Held::Nothing => true,
            Held::One(held) => !summary.numbers || takes_a_number(held),
            Held::Open => false,
        };
        if plainly {
            return true;
        }
        let Some(defaults) = self.literal_defaults(var) else {
            return true;
        };
        self.probe(|vars| {
            defaults
                .iter()
                .any(|default| vars.try_unify(&StaticType::Var(var), default))
        })
    }

    /// What the unsettled literal type `var` is shown as: what it would be
    /// if nothing said otherwise, as far as that's known.
    pub(crate) fn preview(&self, var: TypeVar) -> StaticType {
        if self.is_number(var) {
            return StaticType::Int;
        }
        let mut held = Vec::new();
        for bound in self.bounds[var.0 as usize].iter() {
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

    /// Whether the resolved type `typ` is a variable nothing is known about
    /// yet: unsettled, and not a literal's type, whose values say what it
    /// could be. The parameter `x` of `fn(x) => x` is one.
    pub(crate) fn is_unknown(&self, typ: &StaticType) -> bool {
        matches!(typ, StaticType::Var(var) if !self.is_literal(*var))
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

    /// Settles the literal type `var` as a new variable of its level, when
    /// the values it holds are all of its own type, and returns whether it
    /// did. In `fn f(x) { [x] ++ [x] }`, the lists' elements are what `x`
    /// is, which their uses made the literals' type: what isn't known about
    /// them is what isn't known about `x`, which can still be any type.
    pub(crate) fn settle_literal_as_its_values(&mut self, var: TypeVar) -> bool {
        let own = StaticType::Var(var);
        let held = self.held(var);
        if held.is_empty() || held.iter().any(|(typ, _)| self.resolve(typ) != own) {
            return false;
        }
        let plain = self.fresh_with(StaticType::Unknown, self.levels[var.0 as usize]);
        self.settle_literal(var, &plain)
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

    /// The unsettled variables in `typ` that [`Self::is_generalizable`]
    /// says can become type parameters.
    pub(crate) fn generalizable_in(&self, typ: &StaticType) -> Vec<TypeVar> {
        self.unsettled(typ)
            .into_iter()
            .filter(|&var| self.is_generalizable(var))
            .collect()
    }

    /// Makes the unsettled variables in `typ` belong where the code being
    /// analysed now is, so they don't become type parameters of a generic
    /// function whose body made them.
    pub(crate) fn lower_to_current(&mut self, typ: &StaticType) {
        self.lower(typ, self.level);
    }

    /// Makes the unsettled variables in `typ` that belong deeper than
    /// `level` belong at `level`, as [`Self::lower_to_current`] does.
    fn lower(&mut self, typ: &StaticType, level: u32) {
        for var in self.unsettled(typ) {
            let old = self.levels[var.0 as usize];
            if old > level {
                self.undo.push(Undo::Lowered(var, old));
                self.levels[var.0 as usize] = level;
            }
        }
    }

    /// Where the integer literal whose type is `var` is, if `var` is the
    /// type of integer literals only, as the `1` in `fn inc(x) => x + 1`
    /// has, and not of a literal holding values.
    pub(crate) fn number_literal_span(&self, var: TypeVar) -> Option<Span> {
        let bounds = &self.bounds[var.0 as usize];
        if bounds.summary().holds {
            return None;
        }
        match bounds.iter().next()? {
            Bound::Number(_, span) => Some(*span),
            Bound::Holds(..) => None,
        }
    }

    /// Settles `var` as the type parameter `name` of the generic function
    /// whose body made it.
    pub(crate) fn generalize(&mut self, var: TypeVar, name: &str) {
        debug_assert!(
            !self.is_literal(var),
            "a literal's type is a type parameter"
        );
        self.settle(var, StaticType::Generic(name.into()));
    }

    /// Whether `typ` is made from the unsettled variable `var`, directly or
    /// through what a literal in it holds: `[x]` is made from `x`'s type.
    pub(crate) fn is_made_from(&self, typ: &StaticType, var: TypeVar) -> bool {
        self.vars_made_from(std::slice::from_ref(typ))
            .contains(&var)
    }

    /// The unsettled variables in `types` and, through each literal among
    /// them, in what it holds, at any depth: `x`'s type is one `[x]` is
    /// made from.
    pub(crate) fn vars_made_from(&self, types: &[StaticType]) -> AHashSet<TypeVar> {
        let mut made_from = AHashSet::new();
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
    fn would_contain(&self, typ: &StaticType, var: TypeVar) -> bool {
        let mut seen = AHashSet::new();
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

    /// The types the literal type `var` holds: `x`'s and `Int` in `[x, 1]`.
    fn held_types(&self, var: TypeVar) -> Vec<StaticType> {
        self.bounds[var.0 as usize]
            .iter()
            .filter_map(|bound| match bound {
                Bound::Holds(held, _, _) => Some(held.clone()),
                Bound::Number(..) => None,
            })
            .collect()
    }

    /// How many variables there are, so [`Self::settle_from`] can settle
    /// the ones made after this point.
    pub(crate) fn count(&self) -> usize {
        self.table.borrow().len()
    }

    fn solution(&self, var: TypeVar) -> Option<StaticType> {
        let root = self.root(var);
        if root == var {
            self.table.borrow_mut().probe_value(Key(var.0)).0
        } else {
            Some(StaticType::Var(root))
        }
    }

    /// The variable that stands for the set `var` belongs to.
    fn root(&self, var: TypeVar) -> TypeVar {
        TypeVar(self.table.borrow_mut().find(Key(var.0)).0)
    }

    /// Settles `var` as `typ`, and returns whether `typ` is within the
    /// bounds of `var`. When `typ` is another variable, it gets them. On a
    /// mismatch, `var` and the variables settled meanwhile stay settled.
    fn settle(&mut self, var: TypeVar, typ: StaticType) -> bool {
        debug_assert!(self.solution(var).is_none(), "{var:?} is settled twice");
        // What `var` stands for now belongs where `var` does.
        let level = self.levels[var.0 as usize];
        if level < self.level {
            self.lower(&typ, level);
        }
        let shallow = self.shallow(&typ);
        match &shallow {
            StaticType::Var(other) => {
                self.table.get_mut().union(Key(var.0), Key(other.0));
                // The set `var` joins is a join, or an `Any` is expected of
                // it, if `var`'s was.
                if self.joins.contains(&var) && self.joins.insert(*other) {
                    self.undo.push(Undo::MarkedJoin(*other));
                }
                if self.expects_any.contains(&var) && self.expects_any.insert(*other) {
                    self.undo.push(Undo::ExpectedAny(*other));
                }
            }
            _ => self
                .table
                .get_mut()
                .union_value(Key(var.0), Solution(Some(typ))),
        }
        self.trail.push(var);
        let bounds = self.bounds[var.0 as usize].clone();
        if bounds.is_empty() {
            return true;
        }
        match shallow {
            StaticType::Var(other) => {
                // A literal holding a value of the type the other variable
                // already is says nothing more.
                let held_by_it = |vars: &Self, bound: &Bound| {
                    matches!(bound, Bound::Holds(held @ StaticType::Var(_), _, _)
                        if vars.shallow(held) == StaticType::Var(other))
                };
                let kept = if bounds.summary().holds_variables
                    && bounds.iter().any(|bound| held_by_it(self, bound))
                {
                    Bounds::new(
                        bounds
                            .iter()
                            .filter(|bound| !held_by_it(self, bound))
                            .cloned()
                            .collect(),
                    )
                } else {
                    bounds
                };
                if !kept.is_empty() {
                    let joined = self.bounds[other.0 as usize].then(&kept);
                    let old = std::mem::replace(&mut self.bounds[other.0 as usize], joined);
                    self.undo.push(Undo::Bounds(other, old));
                    self.literals.push(other);
                }
                // The values of two literals have to have a type in common:
                // `["s"]` and `[1]` can't be one literal's type.
                !self.holds_several_literals(other) || self.some_default_fits(other)
            }
            typ => bounds.iter().all(|bound| match bound {
                Bound::Number(..) => takes_a_number(&typ),
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

    /// Runs `try_it` on `context`, whose variables `vars` gives, and keeps
    /// what it settled only if it returns `Some`: overload resolution tries
    /// each candidate this way.
    pub(crate) fn attempt_in<C, T>(
        context: &mut C,
        vars: fn(&mut C) -> &mut Self,
        try_it: impl FnOnce(&mut C) -> Option<T>,
    ) -> Option<T> {
        let snapshot = vars(context).snapshot();
        let tried = try_it(context);
        match tried {
            Some(_) => vars(context).commit(snapshot),
            None => vars(context).rollback(snapshot),
        }
        tried
    }

    /// Runs `look` on `context`, whose variables `vars` gives, and takes back
    /// what it settled: whether a call would fit is asked this way.
    pub(crate) fn probe_in<C, R>(
        context: &mut C,
        vars: fn(&mut C) -> &mut Self,
        look: impl FnOnce(&mut C) -> R,
    ) -> R {
        let snapshot = vars(context).snapshot();
        let seen = look(context);
        vars(context).rollback(snapshot);
        seen
    }

    /// [`Self::attempt_in`] on the variables alone, keeping what `try_it`
    /// settled if it returns `true`.
    pub(crate) fn attempt(&mut self, try_it: impl FnOnce(&mut Self) -> bool) -> bool {
        Self::attempt_in(self, |vars| vars, |vars| try_it(vars).then_some(())).is_some()
    }

    /// [`Self::probe_in`] on the variables alone.
    pub(crate) fn probe<R>(&mut self, look: impl FnOnce(&mut Self) -> R) -> R {
        Self::probe_in(self, |vars| vars, look)
    }

    /// The current state, to [`Self::rollback`] to later.
    fn snapshot(&mut self) -> Snapshot {
        let vars = self.count();
        Snapshot {
            table: self.table.get_mut().snapshot(),
            vars,
            trail: self.trail.len(),
            undo: self.undo.len(),
        }
    }

    /// Keeps what was settled since `snapshot`, which a snapshot taken before
    /// it can still take back.
    fn commit(&mut self, snapshot: Snapshot) {
        self.table.get_mut().commit(snapshot.table);
    }

    /// Unsettles what was settled since `snapshot`, and forgets the variables
    /// made since.
    fn rollback(&mut self, snapshot: Snapshot) {
        self.table.get_mut().rollback_to(snapshot.table);
        self.trail.truncate(snapshot.trail);
        for undo in self.undo.drain(snapshot.undo..).rev() {
            match undo {
                Undo::Lowered(var, level) => self.levels[var.0 as usize] = level,
                Undo::Bounds(var, bounds) => self.bounds[var.0 as usize] = bounds,
                Undo::Fallback(var, fallback) => self.fallbacks[var.0 as usize] = fallback,
                Undo::MarkedJoin(var) => {
                    self.joins.remove(&var);
                }
                Undo::ExpectedAny(var) => {
                    self.expects_any.remove(&var);
                }
            }
        }
        self.fallbacks.truncate(snapshot.vars);
        self.levels.truncate(snapshot.vars);
        self.bounds.truncate(snapshot.vars);
        self.contents.retain(|var| (var.0 as usize) < snapshot.vars);
    }

    /// Unifies `a` and `b` if they can be made the same type, and otherwise
    /// leaves every variable as it was.
    pub(crate) fn try_unify(&mut self, a: &StaticType, b: &StaticType) -> bool {
        self.attempt(|vars| vars.unify(a, b))
    }

    /// The unsettled variables in `typ`, each once, outermost first.
    pub(crate) fn unsettled(&self, typ: &StaticType) -> Vec<TypeVar> {
        let unsettled = RefCell::new(Vec::new());
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

    /// `typ` as an error shows it: a part nothing has said the type of yet
    /// is `Unknown`, as `None` is an `Option<Unknown>`, except a literal's,
    /// which is shown as what it is unless something says otherwise: `[1]`
    /// as a `[Int]`. `Any` is only what the program says is one.
    pub(crate) fn shown(&self, typ: &StaticType) -> StaticType {
        self.resolve(typ).replace(&|part| match part {
            StaticType::Var(var) if self.is_literal(*var) => Some(self.preview(*var)),
            StaticType::Var(_) => Some(StaticType::Unknown),
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
            if !defaults
                .iter()
                .any(|default| self.attempt(|vars| vars.settle_default(var, default)))
            {
                // `Any` takes anything a literal holds.
                self.settle(var, StaticType::Any);
            }
        }
    }

    /// `typ` with every settled variable replaced by what it stands for.
    pub(crate) fn resolve(&self, typ: &StaticType) -> StaticType {
        typ.replace(&|part| match part {
            StaticType::Var(var) => self.solution(*var).map(|solution| self.resolve(&solution)),
            _ => None,
        })
    }

    /// `typ` as the analysis hands it on, once every variable in it is
    /// settled: [`Self::resolve`]d, and with each failed check's type shown
    /// as `Any`, which is what's known about it.
    pub(crate) fn finish(&self, typ: &StaticType) -> StaticType {
        typ.replace(&|part| match part {
            StaticType::Var(var) => self.solution(*var).map(|solution| self.finish(&solution)),
            StaticType::Error => Some(StaticType::Any),
            _ => None,
        })
    }

    /// [`Self::finish`], for a type that may hold variables a later REPL line
    /// can still settle, which the analysis shows as `Unknown`.
    pub(crate) fn finish_open(&self, typ: &StaticType) -> StaticType {
        self.finish(typ)
            .replace(&|part| matches!(part, StaticType::Var(_)).then_some(StaticType::Unknown))
    }

    /// `typ`, or what it stands for if it's a settled variable.
    fn shallow(&self, typ: &StaticType) -> StaticType {
        match typ {
            StaticType::Var(var) => self
                .solution(*var)
                .map_or_else(|| typ.clone(), |solution| self.shallow(&solution)),
            _ => typ.clone(),
        }
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
            .collect::<AHashSet<_>>();
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
    fn decide_deferred(&mut self, passed: Vec<(TypeVar, StaticType)>) -> bool {
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
            && let StaticType::Var(join) = self.shallow(argument)
            && self.joins.contains(&join)
            && self.expects_any.insert(join)
        {
            self.undo.push(Undo::ExpectedAny(join));
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
            (StaticType::Iterable(_), StaticType::Any) => false,
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
    fn passes(&mut self, expected: &StaticType, takes: &StaticType) -> bool {
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
    fn has_unknowns(&self, typ: &StaticType) -> bool {
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

    /// Makes every unsettled variable in `typ` but a literal's fall back to
    /// the type of a failed check rather than its own fallback: the type
    /// a generic function's value takes, as `T` in `let f: Any = inc`, is
    /// unknown where nothing says it, not `Any`.
    pub(crate) fn fall_back_as_errors(&mut self, typ: &StaticType) {
        for var in self.unsettled(typ) {
            if self.solution(var).is_none() && !self.is_literal(var) {
                let old = std::mem::replace(&mut self.fallbacks[var.0 as usize], StaticType::Error);
                self.undo.push(Undo::Fallback(var, old));
            }
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

    /// Settles every unsettled variable in `typ` as `to`, but for a
    /// literal's, which takes its default as usual. As [`StaticType::Error`]
    /// they aren't checked: in `missing(xs).map(fn(x) => x.size)`, the call
    /// of `map` says nothing about `x`.
    pub(crate) fn settle_unknowns_but_literals(&mut self, typ: &StaticType, to: &StaticType) {
        for var in self.unsettled(typ) {
            if self.solution(var).is_none() && !self.is_literal(var) {
                self.settle(var, to.clone());
            }
        }
    }

    /// Settles every variable made since `first` that nothing settled with
    /// its fallback, but for those in `except`.
    pub(crate) fn settle_from(&mut self, first: usize, except: &AHashSet<TypeVar>) {
        let contents = self
            .unsettled_contents(first)
            .into_iter()
            .filter(|var| !except.contains(var))
            .collect::<Vec<_>>();
        self.settle_contents(&contents);
        for index in first..self.count() {
            let var = TypeVar(u32::try_from(index).expect("fewer than 2^32 type variables"));
            if !except.contains(&var) {
                self.settle_with_fallbacks(&[var]);
            }
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
        let element = vars.fresh_holding(
            vec![(number.clone(), span()), (StaticType::Float, span())],
            span(),
        );
        assert!(!vars.try_unify(&element, &StaticType::String));
        assert!(vars.try_unify(&element, &StaticType::Float));
        // Holding a Float made the number a Float too.
        assert_eq!(vars.resolve(&number), StaticType::Float);
    }

    #[test]
    fn a_rollback_takes_back_the_bounds_another_variable_got() {
        let mut vars = TypeVars::default();
        let literal = vars.fresh_number(span());
        let other = vars.fresh();
        vars.probe(|vars| {
            assert!(vars.unify(&literal, &other));
            // `other` is the literal now, so it can't be a String.
            assert!(!vars.try_unify(&other, &StaticType::String));
        });
        assert!(vars.try_unify(&other, &StaticType::String));
    }

    #[test]
    fn a_literal_defaults_to_what_its_values_share() {
        let mut vars = TypeVars::default();
        let number = vars.fresh_number(span());
        let StaticType::Var(element) =
            vars.fresh_holding(vec![(number, span()), (StaticType::Float, span())], span())
        else {
            unreachable!("a literal's type is a variable");
        };
        assert_eq!(
            vars.literal_defaults(element),
            Some(vec![StaticType::Float])
        );
        // Values that share no type are an error.
        let StaticType::Var(mixed) = vars.fresh_holding(
            vec![(StaticType::String, span()), (StaticType::Int, span())],
            span(),
        ) else {
            unreachable!("a literal's type is a variable");
        };
        vars.settle_with_fallbacks(&[mixed]);
        assert_eq!(vars.resolve(&StaticType::Var(mixed)), StaticType::Error);
        let unshared = vars.take_unshared();
        assert_eq!(unshared.len(), 1);
        assert_eq!(
            (&unshared[0].first, &unshared[0].second),
            (&StaticType::String, &StaticType::Int)
        );
    }
}
