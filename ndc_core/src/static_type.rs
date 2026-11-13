use crate::r#struct::StructId;
use itertools::Itertools;
use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TypeSignature {
    Variadic,
    Exact(Vec<Parameter>),
}

impl Default for TypeSignature {
    fn default() -> Self {
        Self::Exact(vec![])
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Parameter {
    pub name: String,
    pub type_name: StaticType,
}

impl Parameter {
    pub fn new<N: Into<String>>(name: N, param_type: StaticType) -> Self {
        Self {
            name: name.into(),
            type_name: param_type,
        }
    }
}

/// A type the analyser hasn't worked out yet. See [`StaticType::Var`].
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct TypeVar(pub u32);

#[derive(Debug, Clone, Eq, PartialEq, Hash, Default)]
pub enum StaticType {
    #[default]
    Any,
    /// A type the analyser hasn't worked out yet, such as the element type
    /// of `[]` before anything is stored in it. Only the analyser creates
    /// these, and it replaces every one before handing its results on. Until
    /// then, every method here treats one as `Any`.
    Var(TypeVar),
    /// A type parameter of a generic built-in's signature, such as `T` in
    /// `push<T>(List<T>, T)`. Each call stands for its own type in place of
    /// it, which the analyser works out from the arguments. Every method
    /// here treats one as `Any`.
    Generic(Box<str>),
    /// Bottom type for diverging expressions (`return`, `break`, `continue`).
    /// A value of it fits anywhere, and it joins with any type as that type.
    Never,
    Bool,
    Function {
        parameters: Option<Vec<Self>>,
        return_type: Box<Self>,
    },
    Struct {
        id: StructId,
        name: Box<str>,
    },
    Option(Box<Self>),

    // Numbers
    Number,
    Float,
    Int,

    // What a value of a kind of sequence iterates as, which only a parameter is: `List<Int>` passes for an `Iterable<Int>`.
    Iterable(Box<Self>),
    List(Box<Self>),
    String,
    Tuple(Vec<Self>),
    Map {
        key: Box<Self>,
        value: Box<Self>,
    },
    Iterator(Box<Self>),
    MinHeap(Box<Self>),
    MaxHeap(Box<Self>),
    Deque(Box<Self>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StaticTypeConstructionError {
    message: String,
    help_text: String,
}

impl StaticTypeConstructionError {
    fn new<M: Into<String>, H: Into<String>>(message: M, help_text: H) -> Self {
        Self {
            message: message.into(),
            help_text: help_text.into(),
        }
    }

    pub fn help_text(&self) -> &str {
        &self.help_text
    }
}

impl fmt::Display for StaticTypeConstructionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

impl StaticType {
    /// Every type name recognised by [`StaticType::from_name_and_args`].
    /// Must stay in sync with its match arms; the unit test
    /// `builtin_type_names_are_constructible` guards this.
    pub const BUILTIN_TYPE_NAMES: &'static [&'static str] = &[
        "Any", "Never", "Bool", "Number", "Float", "Int", "String", "Option", "Iterable", "List",
        "Iterator", "MinHeap", "MaxHeap", "Deque", "Tuple", "Map",
    ];

    pub fn from_name_and_args(
        name: &str,
        args: Vec<Self>,
    ) -> Result<Self, StaticTypeConstructionError> {
        match name {
            "Any" => Self::require_no_args(name, &args).map(|_| Self::Any),
            "Never" => Self::require_no_args(name, &args).map(|_| Self::Never),
            "Bool" => Self::require_no_args(name, &args).map(|_| Self::Bool),
            "Number" => Self::require_no_args(name, &args).map(|_| Self::Number),
            "Float" => Self::require_no_args(name, &args).map(|_| Self::Float),
            "Int" => Self::require_no_args(name, &args).map(|_| Self::Int),
            "String" => Self::require_no_args(name, &args).map(|_| Self::String),
            "Option" => {
                Self::require_exactly_one_arg(name, args).map(|elem| Self::Option(Box::new(elem)))
            }
            "Iterable" => {
                Self::require_exactly_one_arg(name, args).map(|elem| Self::Iterable(Box::new(elem)))
            }
            "List" => {
                Self::require_exactly_one_arg(name, args).map(|elem| Self::List(Box::new(elem)))
            }
            "Iterator" => {
                Self::require_exactly_one_arg(name, args).map(|elem| Self::Iterator(Box::new(elem)))
            }
            "MinHeap" => {
                Self::require_exactly_one_arg(name, args).map(|elem| Self::MinHeap(Box::new(elem)))
            }
            "MaxHeap" => {
                Self::require_exactly_one_arg(name, args).map(|elem| Self::MaxHeap(Box::new(elem)))
            }
            "Deque" => {
                Self::require_exactly_one_arg(name, args).map(|elem| Self::Deque(Box::new(elem)))
            }
            "Tuple" => Self::require_at_least_one_arg(name, args).map(Self::Tuple),
            "Map" => {
                let [key, value] = Self::require_exactly_n_args::<2>(name, args)?;
                Ok(Self::Map {
                    key: Box::new(key),
                    value: Box::new(value),
                })
            }
            _ => Err(StaticTypeConstructionError::new(
                format!("unknown type `{name}`"),
                "Use a valid type name in this annotation.",
            )),
        }
    }

    fn require_no_args(name: &str, args: &[Self]) -> Result<(), StaticTypeConstructionError> {
        if args.is_empty() {
            Ok(())
        } else {
            Err(StaticTypeConstructionError::new(
                format!("type `{name}` does not take generic arguments"),
                format!("Remove the generic arguments from `{name}`."),
            ))
        }
    }

    fn require_exactly_one_arg(
        name: &str,
        args: Vec<Self>,
    ) -> Result<Self, StaticTypeConstructionError> {
        let [arg] = Self::require_exactly_n_args(name, args)?;
        Ok(arg)
    }

    fn require_exactly_n_args<const N: usize>(
        name: &str,
        args: Vec<Self>,
    ) -> Result<[Self; N], StaticTypeConstructionError> {
        args.try_into().map_err(|_err: Vec<Self>| {
            StaticTypeConstructionError::new(
                format!("type `{name}` expects exactly {N} generic arguments"),
                format!("Use `{name}<...>` with {N} type arguments."),
            )
        })
    }

    fn require_at_least_one_arg(
        name: &str,
        args: Vec<Self>,
    ) -> Result<Vec<Self>, StaticTypeConstructionError> {
        if args.is_empty() {
            Err(StaticTypeConstructionError::new(
                format!("type `{name}` requires generic arguments"),
                format!("Add generic arguments like `{name}<...>`."),
            ))
        } else {
            Ok(args)
        }
    }

    /// The type a value of either type has: the type itself when both are
    /// the same, and otherwise `Any`. `Never` has no values, so it joins as
    /// the other type. `List<Int>` and `List<Float>` join as `Any`, not as
    /// `List<Any>`, which neither list is.
    #[must_use]
    pub fn join(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Never, other) | (other, Self::Never) => other.clone(),
            _ if self == other => self.clone(),
            _ => Self::Any,
        }
    }

    /// Computes the element type of a tuple when viewed as a sequence: the
    /// type its elements join as.
    fn compute_tuple_element_type(elems: &[Self]) -> Self {
        elems
            .iter()
            .cloned()
            .reduce(|acc, elem| acc.join(&elem))
            .unwrap_or(Self::Any)
    }

    /// Checks if a type is a sequence-like type.
    fn is_sequence(ty: &Self) -> bool {
        matches!(
            ty,
            Self::Iterable(_)
                | Self::List(_)
                | Self::String
                | Self::Tuple(_)
                | Self::Map { .. }
                | Self::Iterator(_)
                | Self::MinHeap(_)
                | Self::MaxHeap(_)
                | Self::Deque(_)
        )
    }

    /// Gets the element type when treating a type as a sequence.
    ///
    /// - `List<T>`, `Iterator<T>`, etc. → `T`
    /// - `String` → `String`
    /// - `Tuple<T1, ..., Tn>` → the join of `T1`, ..., `Tn`
    /// - `Map<K, V>` → `Tuple<K, V>`
    pub fn sequence_element_type(&self) -> Option<Self> {
        match self {
            Self::Iterable(t)
            | Self::List(t)
            | Self::Iterator(t)
            | Self::MinHeap(t)
            | Self::MaxHeap(t)
            | Self::Deque(t) => Some(t.as_ref().clone()),
            Self::String => Some(Self::String),
            Self::Tuple(elems) => Some(Self::compute_tuple_element_type(elems)),
            Self::Map { key, value } => Some(Self::Tuple(vec![
                key.as_ref().clone(),
                value.as_ref().clone(),
            ])),
            unknown if unknown.is_unknown() => Some(Self::Any),
            _ => None,
        }
    }

    /// Whether a value of this type can be passed where `parameter` is
    /// expected as it is, with nothing to check as the program runs. The
    /// two types are the same, except that `parameter` can be `Any`, which
    /// takes anything, and that its type parameters and the parts of it not
    /// known yet stand for any type. Inside a container, a function type,
    /// an `Option` or a tuple, `Any` is just another type: a `List<Int>`
    /// doesn't fit a `List<Any>`, as the list could then get a String. A
    /// `Iterable<T>` takes any kind of sequence of what fits `T`.
    pub fn fits(&self, parameter: &Self) -> bool {
        matches!(parameter, Self::Any) || self.matches(parameter, false)
    }

    /// Whether a value of this type might be passed where `parameter` is
    /// expected, once what isn't known about it is: as [`Self::fits`],
    /// except that the parts of this type not known yet, and the whole of
    /// it if it's `Any`, stand for any type too. `Any` checked as the
    /// program runs can turn out to be an Int, but a `List<Any>` is never a
    /// `List<Int>`.
    pub fn may_fit(&self, parameter: &Self) -> bool {
        matches!(self, Self::Any) || self.fits(parameter) || self.matches(parameter, true)
    }

    /// Whether this type is `parameter`, where what [`Self::fits`] and
    /// [`Self::may_fit`] let stand for any type does, and the variables of
    /// this type too if `unknowns_match`.
    fn matches(&self, parameter: &Self, unknowns_match: bool) -> bool {
        let wildcard = |typ: &Self| matches!(typ, Self::Var(_) | Self::Generic(_));
        if wildcard(parameter) || (unknowns_match && wildcard(self)) {
            return true;
        }
        let matches = |a: &Self, p: &Self| a.matches(p, unknowns_match);
        match (self, parameter) {
            (Self::Never, _) => true,
            (Self::Iterable(a), Self::Iterable(p)) => a.fits(p) || matches(a, p),
            (argument, Self::Iterable(p)) if Self::is_sequence(argument) => {
                let element = argument
                    .sequence_element_type()
                    .expect("a sequence has elements");
                element.fits(p) || matches(&element, p)
            }
            (Self::Option(a), Self::Option(p))
            | (Self::List(a), Self::List(p))
            | (Self::Iterator(a), Self::Iterator(p))
            | (Self::MinHeap(a), Self::MinHeap(p))
            | (Self::MaxHeap(a), Self::MaxHeap(p))
            | (Self::Deque(a), Self::Deque(p)) => matches(a, p),
            (Self::Tuple(a), Self::Tuple(p)) => {
                a.len() == p.len() && a.iter().zip(p).all(|(a, p)| matches(a, p))
            }
            (Self::Map { key: ak, value: av }, Self::Map { key: pk, value: pv }) => {
                matches(ak, pk) && matches(av, pv)
            }
            (
                Self::Function {
                    parameters: a,
                    return_type: ar,
                },
                Self::Function {
                    parameters: p,
                    return_type: pr,
                },
            ) => {
                // What a function value takes and returns isn't checked as
                // the program runs, so whether it might fit only depends on
                // what both types know: `fn(x) => x > 1`, typed `Fn(Any) ->
                // Any` where nothing says what `x` is, might be a predicate.
                let part = |a: &Self, p: &Self| {
                    matches(a, p)
                        || (unknowns_match && (matches!(a, Self::Any) || matches!(p, Self::Any)))
                };
                let parameters = match (a, p) {
                    // A function value whose parameters aren't known fits
                    // a function type that says nothing about them either.
                    (_, None) => true,
                    (None, Some(_)) => unknowns_match,
                    (Some(a), Some(p)) => {
                        a.len() == p.len() && a.iter().zip(p).all(|(a, p)| part(a, p))
                    }
                };
                parameters && part(ar, pr)
            }
            _ => self == parameter,
        }
    }

    /// Whether this type stands for every type: `Any`, a type variable or
    /// a type parameter, which are all `Any` to the methods here.
    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Any | Self::Var(_) | Self::Generic(_))
    }

    /// This type with each part that `replace` gives a type for replaced by
    /// that type, looking at the outer parts first: `List<?0>` becomes
    /// `List<Int>` when `replace` maps `?0` to `Int`.
    #[must_use]
    pub fn replace(&self, replace: &impl Fn(&Self) -> Option<Self>) -> Self {
        if let Some(replaced) = replace(self) {
            return replaced;
        }
        let map = |typ: &Self| Box::new(typ.replace(replace));
        match self {
            Self::Function {
                parameters,
                return_type,
            } => Self::Function {
                parameters: parameters
                    .as_ref()
                    .map(|parameters| parameters.iter().map(|p| p.replace(replace)).collect()),
                return_type: map(return_type),
            },
            Self::Option(t) => Self::Option(map(t)),
            Self::Iterable(t) => Self::Iterable(map(t)),
            Self::List(t) => Self::List(map(t)),
            Self::Iterator(t) => Self::Iterator(map(t)),
            Self::MinHeap(t) => Self::MinHeap(map(t)),
            Self::MaxHeap(t) => Self::MaxHeap(map(t)),
            Self::Deque(t) => Self::Deque(map(t)),
            Self::Tuple(elements) => {
                Self::Tuple(elements.iter().map(|e| e.replace(replace)).collect())
            }
            Self::Map { key, value } => Self::Map {
                key: map(key),
                value: map(value),
            },
            Self::Any
            | Self::Var(_)
            | Self::Generic(_)
            | Self::Never
            | Self::Bool
            | Self::Struct { .. }
            | Self::Number
            | Self::Float
            | Self::Int
            | Self::String => self.clone(),
        }
    }

    /// Whether this type or any of its parts satisfies `test`.
    pub fn contains(&self, test: &impl Fn(&Self) -> bool) -> bool {
        if test(self) {
            return true;
        }
        match self {
            Self::Function {
                parameters,
                return_type,
            } => {
                return_type.contains(test)
                    || parameters
                        .iter()
                        .flatten()
                        .any(|parameter| parameter.contains(test))
            }
            Self::Option(t)
            | Self::Iterable(t)
            | Self::List(t)
            | Self::Iterator(t)
            | Self::MinHeap(t)
            | Self::MaxHeap(t)
            | Self::Deque(t) => t.contains(test),
            Self::Tuple(elements) => elements.iter().any(|e| e.contains(test)),
            Self::Map { key, value } => key.contains(test) || value.contains(test),
            Self::Any
            | Self::Var(_)
            | Self::Generic(_)
            | Self::Never
            | Self::Bool
            | Self::Struct { .. }
            | Self::Number
            | Self::Float
            | Self::Int
            | Self::String => false,
        }
    }

    /// The type parameters in this type, each once, outermost first: `T`
    /// and `U` in `Fn(T) -> List<U>`.
    pub fn type_parameters(&self) -> Vec<Box<str>> {
        let names = std::cell::RefCell::new(Vec::<Box<str>>::new());
        self.contains(&|part| {
            if let Self::Generic(name) = part
                && !names.borrow().contains(name)
            {
                names.borrow_mut().push(name.clone());
            }
            false
        });
        names.into_inner()
    }

    /// Whether any type variable is left in this type.
    pub fn has_vars(&self) -> bool {
        self.contains(&|t| matches!(t, Self::Var(_)))
    }

    #[must_use]
    pub fn unit() -> Self {
        Self::Tuple(vec![])
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Self::Number | Self::Float | Self::Int)
    }

    /// Checks whether some runtime value could satisfy both `self` and `other`.
    ///
    /// # Examples
    /// - `Iterable<String>` overlaps `List<Any>`: a list of Strings is
    ///   both
    /// - `Any` overlaps every type
    /// - `List<Int>` does not overlap `List<String>`
    /// - `Int` does not overlap `String`
    ///
    /// This is the question a cast asks: `as` is impossible only when no value
    /// could satisfy both the operand's type and the target, and anything else
    /// is left to the runtime check.
    ///
    /// Element types are treated as inhabited, so `List<Int>` and `List<String>`
    /// count as disjoint even though the empty list inhabits both. Casting an
    /// empty container to an unrelated element type is therefore rejected —
    /// a deliberate trade, because the alternative gives up the compile-time
    /// error on every non-empty `List<Int> as List<String>` to allow a cast
    /// that could only ever be vacuous.
    ///
    /// `Never` overlaps nothing, since no value inhabits it.
    pub fn overlaps(&self, other: &Self) -> bool {
        // Never has no inhabitants, so nothing can satisfy it — not even
        // another Never.
        if matches!(self, Self::Never) || matches!(other, Self::Never) {
            return false;
        }

        // `Any` and what isn't known yet could be anything, and a type has
        // the values it has.
        if self.is_unknown() || other.is_unknown() || self == other {
            return true;
        }

        match (self, other) {
            // Iterable<T> overlaps every sequence-family type whose element
            // type overlaps T.
            (
                Self::Iterable(t),
                Self::List(u)
                | Self::Iterator(u)
                | Self::MinHeap(u)
                | Self::MaxHeap(u)
                | Self::Deque(u)
                | Self::Iterable(u),
            )
            | (
                Self::List(u)
                | Self::Iterator(u)
                | Self::MinHeap(u)
                | Self::MaxHeap(u)
                | Self::Deque(u),
                Self::Iterable(t),
            ) => t.overlaps(u),
            (Self::Iterable(t), Self::String) | (Self::String, Self::Iterable(t)) => {
                Self::String.overlaps(t)
            }
            (Self::Iterable(t), Self::Tuple(elems)) | (Self::Tuple(elems), Self::Iterable(t)) => {
                elems.iter().all(|elem| elem.overlaps(t))
            }
            (Self::Iterable(t), Self::Map { key, value })
            | (Self::Map { key, value }, Self::Iterable(t)) => {
                Self::Tuple(vec![key.as_ref().clone(), value.as_ref().clone()]).overlaps(t)
            }

            // Covariant containers of the same shape overlap when their
            // element types do.
            (Self::Option(s), Self::Option(t))
            | (Self::List(s), Self::List(t))
            | (Self::Iterator(s), Self::Iterator(t))
            | (Self::MinHeap(s), Self::MinHeap(t))
            | (Self::MaxHeap(s), Self::MaxHeap(t))
            | (Self::Deque(s), Self::Deque(t)) => s.overlaps(t),
            (Self::Tuple(s_elems), Self::Tuple(t_elems)) => {
                s_elems.len() == t_elems.len()
                    && s_elems.iter().zip(t_elems).all(|(s, t)| s.overlaps(t))
            }
            (Self::Map { key: k1, value: v1 }, Self::Map { key: k2, value: v2 }) => {
                k1.overlaps(k2) && v1.overlaps(v2)
            }

            // A function value's parameter and return types are not checkable
            // at runtime, so only a provable arity mismatch rules overlap out.
            (Self::Function { parameters: p1, .. }, Self::Function { parameters: p2, .. }) => {
                match (p1, p2) {
                    (Some(p1), Some(p2)) => p1.len() == p2.len(),
                    _ => true,
                }
            }

            _ => false,
        }
    }

    pub fn index_element_type(&self) -> Option<Self> {
        if let Self::Map { value, .. } = self {
            return Some(value.as_ref().clone());
        }

        self.sequence_element_type()
    }

    /// Returns `true` if this type could be a callable function at runtime.
    /// This is true for `Function` types and for `Any` (which might be a
    /// function at runtime). Concrete non-function types return `false`.
    pub fn could_be_callable(&self) -> bool {
        matches!(self, Self::Function { .. }) || self.is_unknown()
    }

    pub fn is_fn_and_matches(&self, types: &[Self]) -> bool {
        // If the thing is not a function we're not interested
        let Self::Function { parameters, .. } = self else {
            return false;
        };

        let Some(parameters) = parameters else {
            // If this branch happens then the function we're matching against is variadic meaning it's always a match
            return true;
        };

        parameters.len() == types.len()
            && types
                .iter()
                .zip(parameters)
                .all(|(argument, parameter)| argument.fits(parameter))
            && Self::type_parameters_agree(parameters, types)
    }

    /// Whether each type parameter of `parameters` stands for one type in
    /// `arguments`: `(Int, String)` doesn't fit `(T, T)`, as `T` would have to
    /// be both. Inside a container a type parameter is the type there, as a
    /// `List<Any>` makes `T` `Any`, and a value passed for `T` itself only has
    /// to fit it: `push(List<Any>, String)` fits `push(List<T>, T)`. What isn't
    /// known yet, and an argument that's `Any` as a whole, which is checked as
    /// the program runs, agree with anything.
    pub fn type_parameters_agree(parameters: &[Self], arguments: &[Self]) -> bool {
        let mut bound = Vec::new();
        for (parameter, argument) in parameters.iter().zip(arguments) {
            argument.bind_type_parameters(parameter, false, &mut bound);
        }
        bound.iter().enumerate().all(|(i, (name, typ, inside))| {
            let exact = bound
                .iter()
                .find(|(other, _, other_inside)| other == name && *other_inside);
            match exact {
                // What a container says it is decides, and a value passed
                // for it fits it.
                Some((_, exact, _)) if *inside => exact.agrees_with(typ),
                Some((_, exact, _)) => {
                    *typ == Self::Any || typ.fits(exact) || typ.agrees_with(exact)
                }
                // Values passed for it are what it is, so they agree.
                None => bound[..i]
                    .iter()
                    .filter(|(earlier, _, _)| earlier == name)
                    .all(|(_, earlier, _)| {
                        *typ == Self::Any || *earlier == Self::Any || earlier.agrees_with(typ)
                    }),
            }
        })
    }

    /// Adds to `bound` the part of this type that each type parameter in
    /// `parameter` is matched against, as [`Self::matches`] matches them,
    /// with whether it's inside a container, where it has to be that type.
    fn bind_type_parameters<'p>(
        &self,
        parameter: &'p Self,
        inside: bool,
        bound: &mut Vec<(&'p str, Self, bool)>,
    ) {
        match (self, parameter) {
            (_, Self::Generic(name)) => bound.push((name, self.clone(), inside)),
            // A sequence passed for an `Iterable<T>` holds values passed for `T`.
            (Self::Iterable(a), Self::Iterable(p)) => a.bind_type_parameters(p, inside, bound),
            (argument, Self::Iterable(p)) if Self::is_sequence(argument) => {
                if let Some(element) = argument.sequence_element_type() {
                    element.bind_type_parameters(p, inside, bound);
                }
            }
            (Self::Option(a), Self::Option(p))
            | (Self::List(a), Self::List(p))
            | (Self::Iterator(a), Self::Iterator(p))
            | (Self::MinHeap(a), Self::MinHeap(p))
            | (Self::MaxHeap(a), Self::MaxHeap(p))
            | (Self::Deque(a), Self::Deque(p)) => a.bind_type_parameters(p, true, bound),
            (Self::Tuple(a), Self::Tuple(p)) if a.len() == p.len() => {
                for (a, p) in a.iter().zip(p) {
                    a.bind_type_parameters(p, true, bound);
                }
            }
            (Self::Map { key: ak, value: av }, Self::Map { key: pk, value: pv }) => {
                ak.bind_type_parameters(pk, true, bound);
                av.bind_type_parameters(pv, true, bound);
            }
            _ => {}
        }
    }

    /// Whether this type and `other` could be one type, where what isn't
    /// known yet in either could be anything.
    fn agrees_with(&self, other: &Self) -> bool {
        let unknown = |typ: &Self| matches!(typ, Self::Var(_) | Self::Generic(_) | Self::Never);
        if unknown(self) || unknown(other) {
            return true;
        }
        match (self, other) {
            (Self::Option(a), Self::Option(b))
            | (Self::List(a), Self::List(b))
            | (Self::Iterator(a), Self::Iterator(b))
            | (Self::Iterable(a), Self::Iterable(b))
            | (Self::MinHeap(a), Self::MinHeap(b))
            | (Self::MaxHeap(a), Self::MaxHeap(b))
            | (Self::Deque(a), Self::Deque(b)) => a.agrees_with(b),
            (Self::Tuple(a), Self::Tuple(b)) => {
                a.len() == b.len() && a.iter().zip(b).all(|(a, b)| a.agrees_with(b))
            }
            (Self::Map { key: ak, value: av }, Self::Map { key: bk, value: bv }) => {
                ak.agrees_with(bk) && av.agrees_with(bv)
            }
            (
                Self::Function {
                    parameters: ap,
                    return_type: ar,
                },
                Self::Function {
                    parameters: bp,
                    return_type: br,
                },
            ) => {
                let parameters = match (ap, bp) {
                    (Some(a), Some(b)) => {
                        a.len() == b.len() && a.iter().zip(b).all(|(a, b)| a.agrees_with(b))
                    }
                    _ => true,
                };
                parameters && ar.agrees_with(br)
            }
            (a, b) => a == b,
        }
    }

    pub fn unpack(&self) -> Option<Box<dyn Iterator<Item = &Self> + '_>> {
        match self {
            // Any just unpacks to an infinite list of Any
            // The same types as `is_unknown`, spelled out for this exhaustive match.
            Self::Any | Self::Var(_) | Self::Generic(_) => {
                Some(Box::new(std::iter::repeat(&Self::Any)))
            }
            Self::List(elem)
            | Self::Iterable(elem)
            | Self::Iterator(elem)
            | Self::MinHeap(elem)
            | Self::MaxHeap(elem)
            | Self::Deque(elem) => Some(Box::new(std::iter::repeat(&**elem))),
            Self::Tuple(types) => Some(Box::new(types.iter())),
            Self::String => Some(Box::new(std::iter::repeat(&Self::String))),
            Self::Bool
            | Self::Function { .. }
            | Self::Option(_)
            | Self::Number
            | Self::Float
            | Self::Int
            | Self::Map { .. }
            | Self::Struct { .. } // for now, we won't have positional unpacking
            | Self::Never => None,
        }
    }
}

impl fmt::Display for StaticType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Any => write!(f, "Any"),
            Self::Var(_) => write!(f, "_"),
            Self::Never => write!(f, "Never"),
            Self::Bool => write!(f, "Bool"),
            Self::Function {
                parameters,
                return_type,
            } => write!(
                f,
                "Fn({}) -> {return_type}",
                parameters
                    .as_deref()
                    .map_or(String::from("*"), |p| p.iter().join(", "))
            ),
            Self::Generic(name) | Self::Struct { name, .. } => write!(f, "{name}"),
            Self::Option(elem) => write!(f, "Option<{elem}>"),
            Self::Number => write!(f, "Number"),
            Self::Float => write!(f, "Float"),
            Self::Int => write!(f, "Int"),
            Self::Iterable(elem) => write!(f, "Iterable<{elem}>"),
            Self::List(elem) => write!(f, "List<{elem}>"),
            Self::String => write!(f, "String"),
            Self::Tuple(tup) if tup.is_empty() => write!(f, "()"),
            Self::Tuple(tup) => write!(f, "Tuple<{}>", tup.iter().join(", ")),
            Self::Map { key, value } => write!(f, "Map<{key}, {value}>"),
            Self::Iterator(elem) => write!(f, "Iterator<{elem}>"),
            Self::MinHeap(elem) => write!(f, "MinHeap<{elem}>"),
            Self::MaxHeap(elem) => write!(f, "MaxHeap<{elem}>"),
            Self::Deque(elem) => write!(f, "Deque<{elem}>"),
        }
    }
}

impl fmt::Display for TypeSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Variadic => write!(f, "*args"),
            Self::Exact(params) => write!(
                f,
                "{}",
                params
                    .iter()
                    .map(|p| format!("{}: {}", p.name, p.type_name))
                    .join(", ")
            ),
        }
    }
}

#[allow(unused_imports)]
mod test {
    use super::*;

    #[test]
    fn a_list_fits_only_a_list_of_its_own_element_type() {
        let list_of_two_tuple_int = StaticType::List(Box::new(StaticType::Tuple(vec![
            StaticType::Int,
            StaticType::Int,
        ])));
        let list_of_any = StaticType::List(Box::new(StaticType::Any));
        assert!(!list_of_two_tuple_int.fits(&list_of_any));
        assert!(list_of_two_tuple_int.fits(&list_of_two_tuple_int));
        assert!(list_of_two_tuple_int.fits(&StaticType::Any));
        // ...but might once an `Any` it's passed for turns out to hold one.
        assert!(StaticType::Any.may_fit(&list_of_two_tuple_int));
    }

    #[test]
    fn test_function_compatibility() {
        let fun = StaticType::Function {
            parameters: Some(vec![
                StaticType::List(Box::new(StaticType::Any)),
                StaticType::Int,
            ]),
            return_type: Box::new(StaticType::Any),
        };

        let list_of_two_tuple_int = StaticType::List(Box::new(StaticType::Tuple(vec![
            StaticType::Int,
            StaticType::Int,
        ])));

        // A list of tuples isn't a list of `Any`, which could get a String.
        assert!(!fun.is_fn_and_matches(&[list_of_two_tuple_int.clone(), StaticType::Int]));
        let generic = StaticType::signature("<T>(List<T>, Int) -> T").expect("valid signature");
        assert!(generic.is_fn_and_matches(&[list_of_two_tuple_int, StaticType::Int]));
    }

    #[test]
    fn test_overlaps() {
        let list_of = |elem: StaticType| StaticType::List(Box::new(elem));
        let seq_of = |elem: StaticType| StaticType::Iterable(Box::new(elem));

        // Mixed-direction relationships overlap: a List<String> inhabits both.
        assert!(seq_of(StaticType::String).overlaps(&list_of(StaticType::Any)));
        assert!(list_of(StaticType::Any).overlaps(&seq_of(StaticType::String)));

        // Any overlaps everything, including containers of concrete elements.
        assert!(StaticType::Any.overlaps(&list_of(StaticType::Int)));
        assert!(list_of(StaticType::Int).overlaps(&StaticType::Any));

        // Element types are treated as inhabited, so concrete containers with
        // disjoint element types are disjoint.
        assert!(!list_of(StaticType::Int).overlaps(&list_of(StaticType::String)));
        assert!(!seq_of(StaticType::Int).overlaps(&list_of(StaticType::String)));

        // Disjoint scalar constructors never overlap.
        assert!(!StaticType::Int.overlaps(&StaticType::String));
        assert!(!StaticType::Bool.overlaps(&StaticType::Number));

        // A string is a sequence of strings.
        assert!(StaticType::String.overlaps(&seq_of(StaticType::Any)));
        assert!(!StaticType::String.overlaps(&seq_of(StaticType::Int)));
    }

    // Every name in BUILTIN_TYPE_NAMES must be accepted by from_name_and_args
    // for some argument count; a name that is only rejected as "unknown type"
    // means the two lists have drifted apart.
    #[test]
    fn builtin_type_names_are_constructible() {
        for name in StaticType::BUILTIN_TYPE_NAMES {
            let constructible = (0..=2).any(|arg_count| {
                StaticType::from_name_and_args(name, vec![StaticType::Any; arg_count]).is_ok()
            });
            assert!(
                constructible,
                "`{name}` is listed in BUILTIN_TYPE_NAMES but from_name_and_args rejects it"
            );
        }
    }

    #[test]
    fn public_numeric_types_are_siblings() {
        assert!(!StaticType::Int.fits(&StaticType::Number));
        assert!(!StaticType::Float.fits(&StaticType::Number));
        assert_eq!(StaticType::Int.join(&StaticType::Float), StaticType::Any);
        assert_eq!(StaticType::Int.join(&StaticType::Number), StaticType::Any);
        assert!(StaticType::from_name_and_args("Rational", vec![]).is_err());
        assert!(StaticType::from_name_and_args("Complex", vec![]).is_err());
    }
}
