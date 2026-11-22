//! The types of literals: what the values they hold say they can be.

use super::bounds::{Bound, Bounds, Held, Literals, takes_a_number};
use super::{TypeVars, Unshared};
use ndc_core::collections::{HashMap, HashSet};
use ndc_core::{StaticType, TypeVar};
use ndc_lexer::Span;

/// A point in the list of literals' types, from which
/// [`TypeVars::unsettled_literals`] and [`TypeVars::holders`] look: the
/// literals that got their types or a bound after it. The default is the
/// start of the batch.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct LiteralsFrom(usize);

impl TypeVars {
    /// The point in the list of literals' types that's here now.
    pub(crate) fn literals_from_now(&self) -> LiteralsFrom {
        LiteralsFrom(self.literals.len())
    }

    /// The literals' types listed since `from`.
    fn literals_since(&self, from: LiteralsFrom) -> &[TypeVar] {
        self.literals.get(from.0..).unwrap_or_default()
    }

    /// The type of an integer literal, like `1`, which its uses settle as an
    /// `Int`, a `Float` or a `Number`: `1` in `[1.5, 1]` is a Float. It's an
    /// `Int` unless something says otherwise.
    pub(crate) fn fresh_number(&mut self, span: Span) -> StaticType {
        let literal = self.next_var();
        self.fresh_bounded(vec![Bound::Number(literal, span)])
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

    /// The unsettled literal types listed since `from` that each unsettled
    /// literal type is held by: the element type of `[1]` holds the `1`'s.
    pub(crate) fn holders(&self, from: LiteralsFrom) -> HashMap<TypeVar, Vec<TypeVar>> {
        let mut holders = HashMap::<TypeVar, Vec<TypeVar>>::default();
        let mut seen = HashSet::default();
        for &holder in self.literals_since(from) {
            if (holder.0 as usize) >= self.solutions.len()
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
        let mut seen = HashSet::default();
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
    pub(super) fn fresh_bounded(&mut self, bounds: Vec<Bound>) -> StaticType {
        let typ = self.fresh_with(StaticType::Any, self.level);
        let StaticType::Var(var) = typ else {
            unreachable!("a fresh type is a variable");
        };
        self.bounds[var.0 as usize] = Bounds::new(bounds);
        self.literals.push(var);
        typ
    }

    /// The unsettled types of literals listed since `from` that `belongs`
    /// says to settle, the
    /// ones holding values before the numbers, so a value can still say
    /// what a number is: `1` in `[1, 2.5]` is a Float. Those whose values
    /// share no type come last, so a call waiting for the others can still
    /// ask for them as `Any`s: in `f(if c { 1 } else { "a" }, 0)` with
    /// `f(x: Any, y: Int)`, once the `0` is an Int.
    pub(crate) fn unsettled_literals(
        &mut self,
        from: LiteralsFrom,
        belongs: impl Fn(&Self, TypeVar) -> bool,
    ) -> Vec<TypeVar> {
        let mut literals = Vec::new();
        let mut seen = HashSet::default();
        for &var in self.literals_since(from) {
            if (var.0 as usize) < self.solutions.len()
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
            .collect::<HashSet<_>>();
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

    /// Drops the settled literals from the list of literals' types, and the
    /// ones listed twice, which no walk of it lets through, so walking it
    /// doesn't take longer the more literals the batch has settled. It
    /// moves what's left, so it's only for between top-level statements,
    /// where no [`LiteralsFrom`] is held and no snapshot is open that could
    /// unsettle one again.
    ///
    /// It prunes only once the list has doubled since it last did, so the
    /// literals that stay unsettled, as a top-level `1` can until the end of
    /// the batch, aren't walked again on every statement.
    pub(crate) fn prune_literals(&mut self) {
        debug_assert_eq!(
            self.level, 0,
            "literals are pruned inside a function's body"
        );
        if self.literals.len() < 2 * self.literals_after_pruning.max(32) {
            return;
        }
        let mut seen = HashSet::default();
        let solutions = &self.solutions;
        self.literals.retain(|&var| {
            solutions.get(var.0 as usize).is_some_and(Option::is_none) && seen.insert(var)
        });
        self.literals_after_pruning = self.literals.len();
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
        self.literals_after_pruning = 0;
    }

    /// Whether `var` is the unsettled type of a number literal, or of a
    /// literal that holds only those, as the elements of `[1, 2]` are. A
    /// literal can hold its own type, as `xs` holds `x` in
    /// `let xs = [x]; x = xs.pop();`, which says nothing either way.
    pub(crate) fn is_number_literal(&self, var: TypeVar) -> bool {
        if self.solution(var).is_some() || !self.is_literal(var) {
            return false;
        }
        let mut seen = HashSet::from_iter([var]);
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
        let mut seen = HashSet::default();
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
                        .is_some_and(|solution| self.has_number_literal(solution))
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
        let mut seen = HashSet::default();
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
    pub(super) fn unshared_values(&mut self, var: TypeVar) -> Option<Unshared> {
        self.unshared_values_within(var, &mut Vec::new())
    }

    pub(super) fn unshared_values_within(
        &mut self,
        var: TypeVar,
        seen: &mut Vec<TypeVar>,
    ) -> Option<Unshared> {
        seen.push(var);
        // The values of each literal, in the order the literals come in.
        let mut literals: Vec<(TypeVar, Vec<StaticType>)> = Vec::new();
        let mut index = HashMap::<TypeVar, usize>::default();
        for bound in self.bounds[var.0 as usize].iter() {
            // A number literal is a number.
            if let Bound::Holds(held, literal, _) = bound {
                let at = *index.entry(*literal).or_insert_with(|| {
                    literals.push((*literal, Vec::new()));
                    literals.len() - 1
                });
                literals[at].1.push(held.clone());
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
    pub(super) fn plainly_shared(&self, held: &[StaticType]) -> bool {
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
    pub(super) fn settle_unshared(&mut self, var: TypeVar) -> bool {
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
        let mut seen = HashSet::default();
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
    pub(super) fn some_default_fits(&mut self, var: TypeVar) -> bool {
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

    /// The types the literal type `var` holds: `x`'s and the `1`'s in `[x, 1]`.
    pub(super) fn held_types(&self, var: TypeVar) -> Vec<StaticType> {
        self.bounds[var.0 as usize]
            .iter()
            .filter_map(|bound| match bound {
                Bound::Holds(held, _, _) => Some(held.clone()),
                Bound::Number(..) => None,
            })
            .collect()
    }
}
