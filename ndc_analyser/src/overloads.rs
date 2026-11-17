use crate::types::{TypeTable, TypeVars};
use ndc_analysis::Binding;
use ndc_core::StaticType;
use ndc_resolver::{BindingKind, Candidates, SymbolId, SymbolTable};
use std::borrow::Cow;

/// Whether a call site can fall back to element-wise tuple broadcast when no
/// scalar overload matches the argument types directly. Operator syntax
/// (`a + b`, `-x`, `op=`) is an `Operator`, but for a comparison, and
/// everything else (`f(x)`, dot calls, indexing) is `Regular`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallKind {
    Regular,
    Operator,
    /// A comparison, `<`, `<=`, `>`, `>=`, `<=>` or `>=<`, which compares
    /// whole values and never applies to each element of a tuple: two tuples
    /// no overload compares, as tuples of two kinds of number, are a type
    /// error, not a tuple of what each pair of elements gives.
    Comparison,
}

impl CallKind {
    /// The kind of a call written with the operator `name`.
    #[must_use]
    pub fn of_operator(name: &str) -> Self {
        if matches!(name, "<" | "<=" | ">" | ">=" | "<=>" | ">=<") {
            Self::Comparison
        } else {
            Self::Operator
        }
    }
}

/// What picking a call's overload hands back: the binding the analyser will
/// store on the call node, plus the inferred return type of the call.
pub(crate) struct ResolvedCall {
    pub binding: Binding<SymbolId>,
    pub return_type: StaticType,
    /// The overloads that tie for the call when no one of them is more
    /// specific than the others, which makes it ambiguous.
    pub tied: Vec<SymbolId>,
}

impl ResolvedCall {
    /// No overload takes the call.
    pub(crate) fn none() -> Self {
        Self {
            binding: Binding::None,
            return_type: StaticType::Any,
            tied: Vec::new(),
        }
    }
}

/// Returns `true` iff `more` is strictly more specific than `less`: each of
/// its parameter types is the other's, or an instance of it, and at least
/// one isn't the other's. An instance of a type parameter is any type, and
/// every type is an instance of `Any`: `f(Int)` is more specific than
/// `f(x)` and `f(Any)`, but `f([Int])` and `f([Any])` are
/// unrelated. Used by `Analyser::most_specific` to order matching
/// overloads by specificity: an overload with `more` parameters accepts
/// strictly fewer call signatures than one with `less` parameters.
pub(crate) fn is_strictly_more_specific(more: &[StaticType], less: &[StaticType]) -> bool {
    if more.len() != less.len() {
        return false;
    }
    let mut any_strict = false;
    for (m, l) in more.iter().zip(less) {
        if !m.fits(l) {
            return false;
        }
        if !l.fits(m) {
            any_strict = true;
        }
    }
    any_strict
}

/// Overload resolution over the type table: which of the candidates a call
/// could mean accepts its arguments.
#[derive(Clone, Copy)]
pub(crate) struct Overloads<'a> {
    types: &'a TypeTable,
    /// What the type variables in those types stand for so far. A function
    /// that returns a captured `let xs = []` returns what `xs` holds.
    vars: &'a TypeVars,
    /// Tells a `fn` from a plain binding like a `let`, which doesn't join overloads.
    symbols: &'a SymbolTable,
}

impl<'a> Overloads<'a> {
    pub(crate) fn new(types: &'a TypeTable, vars: &'a TypeVars, symbols: &'a SymbolTable) -> Self {
        Self {
            types,
            vars,
            symbols,
        }
    }

    /// The candidate groups a call can reach, nearest scope first. A plain
    /// binding, like a `let` or a parameter, is the last candidate: the search
    /// stops there instead of reaching the functions further out, since a
    /// variable doesn't join their overloads. Its type doesn't matter, so
    /// `let len = 5; len(xs)` calls the variable and fails.
    fn reachable(self, candidates: &Candidates) -> impl Iterator<Item = &[SymbolId]> {
        let mut stopped = false;
        candidates.iter().map_while(move |group| {
            if stopped {
                return None;
            }
            Some(
                match group.iter().position(|&symbol| self.ends_search(symbol)) {
                    Some(last) => {
                        stopped = true;
                        &group[..=last]
                    }
                    None => group.as_slice(),
                },
            )
        })
    }

    /// The type of `symbol` as far as it's known: a parameter `g` whose type
    /// a call such as `g(1)` settled is a function from then on.
    fn type_of(self, symbol: SymbolId) -> Cow<'a, StaticType> {
        match self.types.get_type(symbol) {
            typ if typ.has_vars() => Cow::Owned(self.vars.resolve(typ)),
            typ => Cow::Borrowed(typ),
        }
    }

    /// The candidates a call can reach, grouped by scope, nearest first.
    pub(crate) fn reachable_groups(self, candidates: &Candidates) -> Vec<Vec<SymbolId>> {
        self.reachable(candidates)
            .map(<[SymbolId]>::to_vec)
            .collect()
    }

    /// Every candidate a call can reach, nearest scope first.
    pub(crate) fn reachable_symbols(self, candidates: &Candidates) -> Vec<SymbolId> {
        self.reachable(candidates).flatten().copied().collect()
    }

    /// The variable that ends the search, if one does: in
    /// `fn foo(a) { … } let foo = fn(a, b) { … };`, a call of `foo` ends at the `let`.
    pub(crate) fn variable_ending_search(self, candidates: &Candidates) -> Option<SymbolId> {
        self.reachable(candidates)
            .last()
            .and_then(|group| group.last().copied())
            .filter(|&symbol| self.ends_search(symbol))
    }

    /// Whether the search for a call of these candidates stops before some
    /// functions with the name: in `fn foo(a) { … } let foo = fn(a, b) { … };`,
    /// the `let` hides `fn foo`.
    pub(crate) fn hides_functions(self, candidates: &Candidates) -> bool {
        let reachable: usize = self.reachable(candidates).map(<[SymbolId]>::len).sum();
        candidates
            .iter()
            .flatten()
            .skip(reachable)
            .any(|&symbol| self.symbols.kind(symbol) == BindingKind::Function)
    }

    /// Whether `symbol` is a plain binding, which no search goes past.
    fn ends_search(self, symbol: SymbolId) -> bool {
        self.symbols.kind(symbol) == BindingKind::Plain
    }

    /// Whether some overload among `candidates` would accept these arguments
    /// if their types were known more precisely: every parameter overlaps the
    /// argument in that position, and at least one argument does not already
    /// fit. A failed call that satisfies this can be fixed with a cast, so the
    /// diagnostic says so; one that does not is a genuine mismatch and gets no
    /// misleading advice.
    pub(crate) fn a_cast_could_resolve(self, candidates: &Candidates, sig: &[StaticType]) -> bool {
        self.reachable(candidates).flatten().any(|&symbol| {
            let typ = self.type_of(symbol);
            let StaticType::Function {
                parameters: Some(parameters),
                ..
            } = typ.as_ref()
            else {
                return false;
            };
            parameters.len() == sig.len()
                && parameters
                    .iter()
                    .zip(sig)
                    .all(|(param, arg)| param.overlaps(arg))
                && parameters
                    .iter()
                    .zip(sig)
                    .any(|(param, arg)| !arg.fits(param))
        })
    }

    /// The length of the tuples an operator's arguments are, when it applies
    /// to each of their elements: one argument at least is a tuple, and all
    /// of them that are have the same length. `(1, 2) + (3, 4)` adds the
    /// elements in pairs, and `(1, 2) * 2` multiplies each by 2.
    pub(crate) fn tuple_axis(sig: &[StaticType]) -> Option<usize> {
        let mut axis = None;
        for arg in sig {
            if let StaticType::Tuple(elements) = arg {
                if elements.is_empty() {
                    return None;
                }
                match axis {
                    None => axis = Some(elements.len()),
                    Some(length) if length == elements.len() => {}
                    _ => return None,
                }
            }
        }
        axis
    }

    /// The arguments an operator applied to each element of tuple
    /// arguments gets at `position`: the tuples' elements there, and the
    /// other arguments as they are.
    pub(crate) fn at_position(sig: &[StaticType], position: usize) -> Vec<StaticType> {
        sig.iter()
            .map(|arg| match arg {
                StaticType::Tuple(elements) => elements[position].clone(),
                other => other.clone(),
            })
            .collect()
    }

    /// The parameter types of `symbol`, if it's a function that isn't
    /// variadic.
    pub(crate) fn parameters_of(self, symbol: SymbolId) -> Option<Vec<StaticType>> {
        match self.type_of(symbol).into_owned() {
            StaticType::Function {
                parameters: Some(parameters),
                ..
            } => Some(parameters),
            _ => None,
        }
    }

    /// Return type of a scalar candidate. Falls back to `Any` if the binding
    /// turned out not to carry a function type (e.g. `Any`-typed callee).
    ///
    /// A type parameter such as `T` in `first<T>([T]) -> T` is `Any`
    /// here: only a call that commits to the overload knows what it stands
    /// for.
    pub(crate) fn scalar_return_type(self, symbol: SymbolId) -> StaticType {
        match self.type_of(symbol).as_ref() {
            StaticType::Function { return_type, .. } => self
                .vars
                .resolve(return_type)
                .replace(&|part| matches!(part, StaticType::Generic(_)).then_some(StaticType::Any)),
            _ => StaticType::Any,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ndc_resolver::{BindingKind, SymbolTable};
    use std::cell::RefCell;

    thread_local! {
        static SYMBOLS: RefCell<SymbolTable> = RefCell::default();
    }

    /// A fresh symbol of this kind, distinct from every other one in the test.
    fn symbol(kind: BindingKind) -> SymbolId {
        SYMBOLS.with_borrow_mut(|symbols| symbols.declare(String::new(), kind))
    }

    fn fn_type(params: Vec<StaticType>, ret: StaticType) -> StaticType {
        StaticType::Function {
            parameters: Some(params),
            return_type: Box::new(ret),
        }
    }

    /// A table holding these functions' types, each under a fresh `fn`
    /// symbol, returned in order.
    fn table(types: Vec<StaticType>) -> (TypeTable, Vec<SymbolId>) {
        let mut table = TypeTable::default();
        let symbols = types
            .into_iter()
            .map(|typ| {
                let symbol = symbol(BindingKind::Function);
                table.declare(symbol, typ);
                symbol
            })
            .collect();
        (table, symbols)
    }

    /// Candidates of these groups, each given oldest first as declared. The
    /// resolver lists each group newest first.
    fn candidates(groups: &[&[SymbolId]]) -> Candidates {
        Candidates::from_groups(
            groups
                .iter()
                .map(|group| group.iter().rev().copied().collect())
                .collect(),
        )
    }

    // Simulates: fn foo(x: Int) { … } let foo = fn(x: String) { … }; foo(1)
    // The `let` ends the search, so the outer `fn` that would accept `Int`
    // isn't reached.
    #[test]
    fn a_variable_holding_a_function_ends_the_search() {
        let (mut types, fns) = table(vec![fn_type(vec![StaticType::Int], StaticType::Int)]);
        let variable = symbol(BindingKind::Plain);
        types.declare(variable, fn_type(vec![StaticType::String], StaticType::Int));
        let candidates = candidates(&[&[fns[0], variable]]);

        SYMBOLS.with_borrow(|table| {
            let vars = TypeVars::default();
            let overloads = Overloads::new(&types, &vars, table);
            assert_eq!(
                overloads.variable_ending_search(&candidates),
                Some(variable)
            );
        });
    }

    #[test]
    fn a_variable_ending_the_search_hides_only_the_functions_further_out() {
        let (mut types, fns) = table(vec![fn_type(vec![StaticType::Int], StaticType::Int)]);
        let variable = symbol(BindingKind::Plain);
        types.declare(variable, fn_type(vec![StaticType::String], StaticType::Int));

        SYMBOLS.with_borrow(|table| {
            let vars = TypeVars::default();
            let overloads = Overloads::new(&types, &vars, table);
            assert!(overloads.hides_functions(&candidates(&[&[variable], &[fns[0]]])));
            assert!(!overloads.hides_functions(&candidates(&[&[variable]])));
        });
    }

    // Any plain binding ends the search, whatever its type.
    #[test]
    fn a_variable_ends_the_search_whatever_its_type() {
        let (mut types, fns) = table(vec![fn_type(vec![StaticType::Int], StaticType::Int)]);
        let variable = symbol(BindingKind::Plain);
        types.declare(variable, StaticType::Int);
        let candidates = candidates(&[&[variable], &[fns[0]]]);

        SYMBOLS.with_borrow(|table| {
            let vars = TypeVars::default();
            let overloads = Overloads::new(&types, &vars, table);
            assert_eq!(
                overloads.variable_ending_search(&candidates),
                Some(variable)
            );
        });
    }
}
