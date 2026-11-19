use ndc_core::{StaticType, TypeVar};
use ndc_lexer::Span;
use std::rc::Rc;

/// What a variable may stand for, beyond what unifying says.
#[derive(Debug, Clone)]
pub(super) enum Bound {
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

impl Bound {
    /// The literal, as the type it was made as, that this bound is from.
    pub(super) fn literal(&self) -> TypeVar {
        match self {
            Self::Number(literal, _) | Self::Holds(_, literal, _) => *literal,
        }
    }
}

/// The bounds of a set of variables unified with each other, in the order
/// they were added. Two sets' bounds join without copying either, as
/// unifying the elements of `[(1, "a"), (2, "b"), …]` joins thousands, and
/// a snapshot keeps the old ones by sharing them.
#[derive(Debug, Clone, Default)]
pub(super) struct Bounds(Option<Rc<Node>>);

#[derive(Debug)]
struct Node {
    summary: Summary,
    parts: Parts,
}

#[derive(Debug)]
enum Parts {
    Leaf(Vec<Bound>),
    Joined(Bounds, Bounds),
}

/// What all the bounds in a [`Bounds`] say together, kept up to date as
/// bounds join, so the questions it answers don't look at each bound.
#[derive(Debug, Clone)]
pub(super) struct Summary {
    pub(super) len: usize,
    /// Whether one is a [`Bound::Number`].
    pub(super) numbers: bool,
    /// Whether one is a [`Bound::Holds`].
    pub(super) holds: bool,
    /// What the [`Bound::Holds`] hold.
    pub(super) held: Held,
    /// The literals the bounds are from.
    pub(super) literals: Literals,
    /// Whether one holds a value whose type is a variable as written, the
    /// only kind that can turn out to hold the type it's a bound of.
    pub(super) holds_variables: bool,
}

/// What the [`Bound::Holds`] of some [`Bounds`] hold.
#[derive(Debug, Clone, PartialEq)]
pub(super) enum Held {
    /// Nothing but values that never arrive, which say nothing.
    Nothing,
    /// Values of this one type, which has no variables in it.
    One(StaticType),
    /// Values of different types, or of a type with variables in it, which
    /// can still turn out to be any type.
    Open,
}

/// The literals some [`Bounds`] are from.
#[derive(Debug, Clone, PartialEq)]
pub(super) enum Literals {
    None,
    One(TypeVar),
    Several,
}

impl Summary {
    const EMPTY: Self = Self {
        len: 0,
        numbers: false,
        holds: false,
        held: Held::Nothing,
        literals: Literals::None,
        holds_variables: false,
    };

    fn of(bound: &Bound) -> Self {
        let literals = Literals::One(bound.literal());
        match bound {
            Bound::Number(..) => Self {
                len: 1,
                numbers: true,
                literals,
                ..Self::EMPTY
            },
            Bound::Holds(held, _, _) => Self {
                len: 1,
                holds: true,
                held: match held {
                    StaticType::Never => Held::Nothing,
                    held if held.has_vars() => Held::Open,
                    held => Held::One(held.clone()),
                },
                literals,
                holds_variables: matches!(held, StaticType::Var(_)),
                ..Self::EMPTY
            },
        }
    }

    fn join(&self, other: &Self) -> Self {
        Self {
            len: self.len + other.len,
            numbers: self.numbers || other.numbers,
            holds: self.holds || other.holds,
            held: match (&self.held, &other.held) {
                (Held::Nothing, held) | (held, Held::Nothing) => held.clone(),
                (Held::One(a), Held::One(b)) if a == b => Held::One(a.clone()),
                _ => Held::Open,
            },
            literals: match (&self.literals, &other.literals) {
                (Literals::None, literals) | (literals, Literals::None) => literals.clone(),
                (Literals::One(a), Literals::One(b)) if a == b => Literals::One(*a),
                _ => Literals::Several,
            },
            holds_variables: self.holds_variables || other.holds_variables,
        }
    }
}

impl Bounds {
    pub(super) fn new(bounds: Vec<Bound>) -> Self {
        if bounds.is_empty() {
            return Self::default();
        }
        let summary = bounds
            .iter()
            .map(Summary::of)
            .reduce(|a, b| a.join(&b))
            .expect("there are bounds");
        Self(Some(Rc::new(Node {
            summary,
            parts: Parts::Leaf(bounds),
        })))
    }

    /// These bounds followed by `other`'s.
    pub(super) fn then(&self, other: &Self) -> Self {
        match (&self.0, &other.0) {
            (None, _) => other.clone(),
            (_, None) => self.clone(),
            (Some(a), Some(b)) => Self(Some(Rc::new(Node {
                summary: a.summary.join(&b.summary),
                parts: Parts::Joined(self.clone(), other.clone()),
            }))),
        }
    }

    pub(super) fn summary(&self) -> &Summary {
        self.0
            .as_ref()
            .map_or(&Summary::EMPTY, |node| &node.summary)
    }

    pub(super) fn is_empty(&self) -> bool {
        self.0.is_none()
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = &Bound> {
        let mut todo = vec![self];
        let mut leaf: std::slice::Iter<'_, Bound> = [].iter();
        std::iter::from_fn(move || {
            loop {
                if let Some(bound) = leaf.next() {
                    return Some(bound);
                }
                let node = todo.pop()?.0.as_ref();
                match node.map(|node| &node.parts) {
                    None => {}
                    Some(Parts::Leaf(bounds)) => leaf = bounds.iter(),
                    Some(Parts::Joined(first, second)) => {
                        todo.push(second);
                        todo.push(first);
                    }
                }
            }
        })
    }

    /// The bounds as a list, to go over while changing the variables.
    pub(super) fn to_vec(&self) -> Vec<Bound> {
        self.iter().cloned().collect()
    }
}

impl Drop for Node {
    /// Drops the parts one at a time, as bounds joined one after another
    /// nest as deep as there were joins.
    fn drop(&mut self) {
        let Parts::Joined(first, second) = &mut self.parts else {
            return;
        };
        let mut todo = vec![std::mem::take(first), std::mem::take(second)];
        while let Some(bounds) = todo.pop() {
            if let Some(node) = bounds.0
                && let Ok(mut node) = Rc::try_unwrap(node)
                && let Parts::Joined(first, second) = &mut node.parts
            {
                todo.push(std::mem::take(first));
                todo.push(std::mem::take(second));
            }
        }
    }
}
