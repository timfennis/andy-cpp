//! Making type variables, settling them, and taking back what was settled.

use super::TypeVars;
use super::bounds::{Bound, Bounds, takes_a_number};
use ndc_core::collections::HashSet;
use ndc_core::{StaticType, TypeVar};

/// A change to the variables, other than settling one, with what it
/// changed, which a [`Snapshot`] takes back.
#[derive(Debug)]
pub(super) enum Undo {
    /// The variable belonged at this level before.
    Lowered(TypeVar, u32),
    /// The variable had these bounds before.
    Bounds(TypeVar, Bounds),
    /// The variable had this fallback before.
    Fallback(TypeVar, StaticType),
    /// The variable was marked as the type of a join.
    MarkedJoin(TypeVar),
    /// An `Any` was expected of the join whose type the variable is.
    ExpectedAny,
}

/// The state of the variables at one point, which [`TypeVars::rollback`]
/// returns to: see [`TypeVars::attempt_in`] and [`TypeVars::probe_in`],
/// the only ways to take one.
#[derive(Debug, Clone, Copy)]
pub(super) struct Snapshot {
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
    pub(super) fn fresh_with(&mut self, fallback: StaticType, level: u32) -> StaticType {
        let var = self.next_var();
        self.solutions.push(None);
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

    /// The variables `typ` is, through what each is settled as.
    pub(super) fn chain(&self, typ: &StaticType) -> Vec<TypeVar> {
        let mut chain = Vec::new();
        let mut typ = typ;
        while let StaticType::Var(var) = typ {
            chain.push(*var);
            match self.solution(*var) {
                Some(solution) => typ = solution,
                None => break,
            }
        }
        chain
    }

    /// Whether an `Any` was expected of the `if` or `match` whose type
    /// `typ` is, when its branches weren't known yet, which then makes it
    /// an `Any` if they share no type.
    pub(crate) fn any_is_expected_of(&self, typ: &StaticType) -> bool {
        self.chain(typ)
            .iter()
            .any(|var| self.expects_any.contains(var))
    }

    /// The variable [`Self::fresh`] makes next.
    pub(super) fn next_var(&self) -> TypeVar {
        TypeVar(u32::try_from(self.solutions.len()).expect("fewer than 2^32 type variables"))
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
    pub(super) fn lower(&mut self, typ: &StaticType, level: u32) {
        for var in self.unsettled(typ) {
            let old = self.levels[var.0 as usize];
            if old > level {
                self.undo.push(Undo::Lowered(var, old));
                self.levels[var.0 as usize] = level;
            }
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

    /// How many variables there are, so [`Self::settle_from`] can settle
    /// the ones made after this point.
    pub(crate) fn count(&self) -> usize {
        self.solutions.len()
    }

    pub(super) fn solution(&self, var: TypeVar) -> Option<&StaticType> {
        self.solutions[var.0 as usize].as_ref()
    }

    /// Settles `var` as `typ`, and returns whether `typ` is within the
    /// bounds of `var`. When `typ` is another variable, it gets them. On a
    /// mismatch, `var` and the variables settled meanwhile stay settled.
    pub(super) fn settle(&mut self, var: TypeVar, typ: StaticType) -> bool {
        debug_assert!(self.solution(var).is_none(), "{var:?} is settled twice");
        // What `var` stands for now belongs where `var` does.
        let level = self.levels[var.0 as usize];
        if level < self.level {
            self.lower(&typ, level);
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
        if tried.is_none() {
            vars(context).rollback(snapshot);
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
    pub(super) fn snapshot(&self) -> Snapshot {
        Snapshot {
            vars: self.solutions.len(),
            trail: self.trail.len(),
            undo: self.undo.len(),
        }
    }

    /// Unsettles what was settled since `snapshot`, and forgets the variables
    /// made since.
    pub(super) fn rollback(&mut self, snapshot: Snapshot) {
        for var in self.trail.drain(snapshot.trail..) {
            self.solutions[var.0 as usize] = None;
        }
        for undo in self.undo.drain(snapshot.undo..).rev() {
            match undo {
                Undo::Lowered(var, level) => self.levels[var.0 as usize] = level,
                Undo::Bounds(var, bounds) => self.bounds[var.0 as usize] = bounds,
                Undo::Fallback(var, fallback) => self.fallbacks[var.0 as usize] = fallback,
                Undo::MarkedJoin(var) => {
                    self.joins.remove(&var);
                }
                Undo::ExpectedAny => {
                    self.expects_any.pop();
                }
            }
        }
        self.solutions.truncate(snapshot.vars);
        self.fallbacks.truncate(snapshot.vars);
        self.levels.truncate(snapshot.vars);
        self.bounds.truncate(snapshot.vars);
        self.contents.retain(|var| (var.0 as usize) < snapshot.vars);
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
            StaticType::Var(var) => self.solution(*var).map(|solution| self.resolve(solution)),
            _ => None,
        })
    }

    /// `typ` as the analysis hands it on, once every variable in it is
    /// settled: [`Self::resolve`]d. A failed check's type stays
    /// [`StaticType::Error`], so the LSP can tell it from `Any`.
    pub(crate) fn finish(&self, typ: &StaticType) -> StaticType {
        self.resolve(typ)
    }

    /// [`Self::finish`], for a type that may hold variables a later REPL line
    /// can still settle, which the analysis shows as `Unknown`.
    pub(crate) fn finish_open(&self, typ: &StaticType) -> StaticType {
        self.finish(typ)
            .replace(&|part| matches!(part, StaticType::Var(_)).then_some(StaticType::Unknown))
    }

    /// `typ`, or what it stands for if it's a settled variable.
    pub(super) fn shallow(&self, typ: &StaticType) -> StaticType {
        match typ {
            StaticType::Var(var) => self
                .solution(*var)
                .map_or_else(|| typ.clone(), |solution| self.shallow(solution)),
            _ => typ.clone(),
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
    pub(crate) fn settle_from(&mut self, first: usize, except: &HashSet<TypeVar>) {
        let contents = self
            .unsettled_contents(first)
            .into_iter()
            .filter(|var| !except.contains(var))
            .collect::<Vec<_>>();
        self.settle_contents(&contents);
        for index in first..self.solutions.len() {
            let var = TypeVar(u32::try_from(index).expect("fewer than 2^32 type variables"));
            if !except.contains(&var) {
                self.settle_with_fallbacks(&[var]);
            }
        }
    }
}
