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
            debug_assert!(!typ.any_var(&|_| true), "unsettled type variable in {typ}");
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

    /// `typ` with every settled variable replaced by what it stands for.
    pub(crate) fn resolve(&self, typ: &StaticType) -> StaticType {
        typ.replace_vars(&|var| self.solution(var).map(|solution| self.resolve(solution)))
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
                if self.resolve(other).any_var(&|other| other == *var) {
                    return false;
                }
                self.solutions[var.0 as usize] = Some(other.clone());
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

    /// Settles every variable made since `first` that nothing settled with
    /// its fallback.
    pub(crate) fn settle_from(&mut self, first: usize) {
        for (solution, fallback) in self.solutions[first..]
            .iter_mut()
            .zip(&self.fallbacks[first..])
        {
            solution.get_or_insert_with(|| fallback.clone());
        }
    }
}
