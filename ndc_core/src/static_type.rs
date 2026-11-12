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

impl TypeSignature {
    pub fn from_annotated_bindings(bindings: Vec<(String, Option<StaticType>)>) -> Self {
        Self::Exact(
            bindings
                .into_iter()
                .map(|(name, annotation)| {
                    Parameter::new(name, annotation.unwrap_or(StaticType::Any))
                })
                .collect(),
        )
    }

    pub fn types(&self) -> Option<Vec<StaticType>> {
        match self {
            Self::Variadic => None,
            Self::Exact(v) => Some(v.iter().map(|p| p.type_name.clone()).collect()),
        }
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
    /// `Never` is a subtype of every type: `lub(T, Never) = T`.
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

    // Sequences List<Int> -> Sequence<Int>
    Sequence(Box<Self>),
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
        "Any", "Never", "Bool", "Number", "Float", "Int", "String", "Option", "Sequence", "List",
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
            "Sequence" => {
                Self::require_exactly_one_arg(name, args).map(|elem| Self::Sequence(Box::new(elem)))
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

    /// Checks if `self` is a subtype of `other`.
    ///
    /// A type S is a subtype of T (S <: T) if a value of type S can be safely
    /// used wherever a value of type T is expected.
    ///
    /// # Key Rules
    /// - `Any` is the top type (supertype of all types)
    /// - `Int`, `Float`, and `Number` are sibling types
    /// - `Sequence<T>` > sequence types with element type T
    /// - Generic types are covariant in their type parameters
    /// - Function parameters are **contravariant**, returns are **covariant**
    ///
    /// Note: this function probably doesn't handle variadic functions correctly
    pub fn is_subtype(&self, other: &Self) -> bool {
        // Never is the bottom type: subtype of everything
        if matches!(self, Self::Never) {
            return true;
        }

        // Any is the universal supertype
        if other.is_unknown() {
            return true;
        }

        // Only Any satisfies the above; all other types fail here
        if self.is_unknown() {
            return false;
        }

        #[allow(clippy::match_same_arms)]
        #[allow(clippy::unnested_or_patterns)]
        match (self, other) {
            // Reflexivity: every type is a subtype of itself
            _ if self == other => true,

            // Sequence hierarchy: specific sequences are subtypes of Sequence<T>
            // where T is their element type (covariant)
            (Self::List(s), Self::Sequence(t))
            | (Self::Iterator(s), Self::Sequence(t))
            | (Self::MinHeap(s), Self::Sequence(t))
            | (Self::MaxHeap(s), Self::Sequence(t))
            | (Self::Deque(s), Self::Sequence(t)) => s.is_subtype(t),

            // String is Sequence<String>
            (Self::String, Self::Sequence(t)) => Self::String.is_subtype(t.as_ref()),

            // Tuple is Sequence<LUB of all elements>
            (Self::Tuple(elems), Self::Sequence(t)) => {
                Self::compute_tuple_element_type(elems).is_subtype(t)
            }

            // Map<K,V> is Sequence<Tuple<K,V>>
            (Self::Map { key, value }, Self::Sequence(t)) => {
                Self::Tuple(vec![key.as_ref().clone(), value.as_ref().clone()])
                    .is_subtype(t.as_ref())
            }

            // Generic types are covariant: Container<S> <: Container<T> if S <: T
            (Self::Option(s), Self::Option(t))
            | (Self::List(s), Self::List(t))
            | (Self::Iterator(s), Self::Iterator(t))
            | (Self::MinHeap(s), Self::MinHeap(t))
            | (Self::MaxHeap(s), Self::MaxHeap(t))
            | (Self::Deque(s), Self::Deque(t))
            | (Self::Sequence(s), Self::Sequence(t)) => s.is_subtype(t),

            // Tuples are covariant pointwise and must have the same arity
            (Self::Tuple(s_elems), Self::Tuple(t_elems)) => {
                s_elems.len() == t_elems.len()
                    && s_elems.iter().zip(t_elems).all(|(s, t)| s.is_subtype(t))
            }

            // Maps are covariant in both key and value type parameters
            (Self::Map { key: k1, value: v1 }, Self::Map { key: k2, value: v2 }) => {
                k1.is_subtype(k2) && v1.is_subtype(v2)
            }

            // Functions: contravariant in parameters, covariant in return type
            // F1 <: F2 iff params(F2) <: params(F1) AND return(F1) <: return(F2)
            (
                Self::Function {
                    parameters: p1,
                    return_type: r1,
                },
                Self::Function {
                    parameters: p2,
                    return_type: r2,
                },
            ) => {
                // Return type is covariant: must satisfy r1 <: r2
                let return_ok = r1.is_subtype(r2);

                // Parameters are contravariant: must satisfy p2 <: p1 (reversed!)
                let params_ok = match (p1, p2) {
                    (None, None) => true,
                    // A function with specific params is a subtype of one with generic params
                    (Some(_), None) => true,
                    // Cannot substitute generic params with specific ones
                    (None, Some(_)) => false,
                    (Some(ps1), Some(ps2)) => {
                        ps1.len() == ps2.len() &&
                            // Note the reversal: p2 must be subtype of p1!
                            // A type parameter of a signature, like `T` in
                            // `Fn(T) -> U`, fits whatever the function takes:
                            // the call works out what it stands for. An `Any`
                            // doesn't, as a value of that type is only
                            // checked when the program runs.
                            ps1.iter()
                                .zip(ps2)
                                .all(|(p1, p2)| matches!(p2, Self::Generic(_)) || p2.is_subtype(p1))
                    }
                };

                return_ok && params_ok
            }

            _ => false,
        }
    }

    //
    /// Computes the Least Upper Bound (join) of two types.
    ///
    /// The LUB is the most specific type that is a supertype of both inputs.
    ///
    /// # Examples
    /// - `lub(Int, Float) = Any`
    /// - `lub(List<Int>, List<Float>) = List<Any>`
    /// - `lub(List<Int>, Iterator<Int>) = Sequence<Int>`
    /// - `lub(Int, String) = Any`
    pub fn lub(&self, other: &Self) -> Self {
        // Never is the bottom type: lub(T, Never) = T
        if matches!(self, Self::Never) {
            return other.clone();
        }
        if matches!(other, Self::Never) {
            return self.clone();
        }

        // Any is the top type
        if self.is_unknown() || other.is_unknown() {
            return Self::Any;
        }

        // Reflexivity: lub(T, T) = T
        if self == other {
            return self.clone();
        }

        // If one is a subtype of the other, return the supertype
        if self.is_subtype(other) {
            return other.clone();
        }
        if other.is_subtype(self) {
            return self.clone();
        }

        match (self, other) {
            // Covariant generic types: compute LUB pointwise
            (Self::Option(s), Self::Option(t)) => Self::Option(Box::new(s.lub(t))),
            (Self::List(s), Self::List(t)) => Self::List(Box::new(s.lub(t))),
            (Self::Iterator(s), Self::Iterator(t)) => Self::Iterator(Box::new(s.lub(t))),
            (Self::MinHeap(s), Self::MinHeap(t)) => Self::MinHeap(Box::new(s.lub(t))),
            (Self::MaxHeap(s), Self::MaxHeap(t)) => Self::MaxHeap(Box::new(s.lub(t))),
            (Self::Deque(s), Self::Deque(t)) => Self::Deque(Box::new(s.lub(t))),
            (Self::Sequence(s), Self::Sequence(t)) => Self::Sequence(Box::new(s.lub(t))),

            // Maps are covariant in both parameters
            (Self::Map { key: k1, value: v1 }, Self::Map { key: k2, value: v2 }) => Self::Map {
                key: Box::new(k1.lub(k2)),
                value: Box::new(v1.lub(v2)),
            },

            // Tuples of same arity: compute LUB pointwise
            (Self::Tuple(e1), Self::Tuple(e2)) if e1.len() == e2.len() => {
                Self::Tuple(e1.iter().zip(e2).map(|(a, b)| a.lub(b)).collect())
            }

            // Different sequence types: generalize to Sequence<lub of elements>
            _ if Self::is_sequence(self) && Self::is_sequence(other) => {
                let elem1 = self.sequence_element_type().expect("must be seq");
                let elem2 = other.sequence_element_type().expect("must be seq");
                Self::Sequence(Box::new(elem1.lub(&elem2)))
            }

            // Functions: covariant in return, contravariant in parameters
            (
                Self::Function {
                    parameters: p1,
                    return_type: r1,
                },
                Self::Function {
                    parameters: p2,
                    return_type: r2,
                },
            ) => {
                // Return type: covariant, so take LUB
                let return_type = Box::new(r1.lub(r2));

                // Parameters: contravariant, so we need GLB (greatest lower bound)
                let parameters = match (p1, p2) {
                    (Some(ps1), Some(ps2)) if ps1.len() == ps2.len() => {
                        Some(ps1.iter().zip(ps2).map(|(a, b)| a.glb(b)).collect())
                    }
                    // Incompatible or unknown parameter lists
                    _ => None,
                };

                Self::Function {
                    parameters,
                    return_type,
                }
            }

            // No common supertype found: default to Any
            _ => Self::Any,
        }
    }

    /// Computes the Greatest Lower Bound (meet) of two types.
    ///
    /// The GLB is the most general type that is a subtype of both inputs.
    /// This is required for contravariant positions (e.g., function parameters).
    ///
    /// Note: Not all type pairs have a representable GLB in our type system.
    /// In such cases, we return a conservative approximation.
    fn glb(&self, other: &Self) -> Self {
        // Any is the top type: glb(Any, T) = T
        match (self, other) {
            (unknown, t) | (t, unknown) if unknown.is_unknown() => t.clone(),
            _ if self == other => self.clone(),
            _ if self.is_subtype(other) => self.clone(),
            _ if other.is_subtype(self) => other.clone(),

            // Covariant types: compute GLB pointwise
            (Self::Option(s), Self::Option(t)) => Self::Option(Box::new(s.glb(t))),
            (Self::List(s), Self::List(t)) => Self::List(Box::new(s.glb(t))),
            (Self::Iterator(s), Self::Iterator(t)) => Self::Iterator(Box::new(s.glb(t))),
            (Self::MinHeap(s), Self::MinHeap(t)) => Self::MinHeap(Box::new(s.glb(t))),
            (Self::MaxHeap(s), Self::MaxHeap(t)) => Self::MaxHeap(Box::new(s.glb(t))),
            (Self::Deque(s), Self::Deque(t)) => Self::Deque(Box::new(s.glb(t))),
            (Self::Sequence(s), Self::Sequence(t)) => Self::Sequence(Box::new(s.glb(t))),

            (Self::Map { key: k1, value: v1 }, Self::Map { key: k2, value: v2 }) => Self::Map {
                key: Box::new(k1.glb(k2)),
                value: Box::new(v1.glb(v2)),
            },

            (Self::Tuple(e1), Self::Tuple(e2)) if e1.len() == e2.len() => {
                Self::Tuple(e1.iter().zip(e2).map(|(a, b)| a.glb(b)).collect())
            }

            // No representable GLB: conservatively return first type
            // A complete type system would have a Bottom type (⊥) here
            _ => self.clone(),
        }
    }

    /// Computes the element type of a tuple when viewed as a sequence.
    /// Returns the LUB of all tuple elements.
    fn compute_tuple_element_type(elems: &[Self]) -> Self {
        if elems.is_empty() {
            return Self::Any;
        }

        elems
            .iter()
            .skip(1)
            .fold(elems[0].clone(), |acc, elem| acc.lub(elem))
    }

    /// Checks if a type is a sequence-like type.
    fn is_sequence(ty: &Self) -> bool {
        matches!(
            ty,
            Self::Sequence(_)
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
    /// - `Tuple<T1, ..., Tn>` → `lub(T1, ..., Tn)`
    /// - `Map<K, V>` → `Tuple<K, V>`
    pub fn sequence_element_type(&self) -> Option<Self> {
        match self {
            Self::Sequence(t)
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
    /// `Sequence<T>` takes any kind of sequence of what fits `T`.
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
            (Self::Sequence(a), Self::Sequence(p)) => a.fits(p) || matches(a, p),
            (argument, Self::Sequence(p)) if Self::is_sequence(argument) => {
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
            Self::Sequence(t) => Self::Sequence(map(t)),
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
            | Self::Sequence(t)
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
    /// - `Sequence<String>` overlaps `List<Any>`: a `List<String>` inhabits
    ///   both, even though neither type is a subtype of the other
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
        // another Never. This has to come first: Never is a subtype of every
        // type, so the shortcut below would otherwise report overlap.
        if matches!(self, Self::Never) || matches!(other, Self::Never) {
            return false;
        }

        // A subtype relation in either direction implies a common inhabitant.
        // This also handles Any, the supertype of everything.
        if self.is_subtype(other) || other.is_subtype(self) {
            return true;
        }

        match (self, other) {
            // Sequence<T> overlaps every sequence-family type whose element
            // type overlaps T, even when the subtype check fails because the
            // relationship points in different directions per layer.
            (
                Self::Sequence(t),
                Self::List(u)
                | Self::Iterator(u)
                | Self::MinHeap(u)
                | Self::MaxHeap(u)
                | Self::Deque(u)
                | Self::Sequence(u),
            )
            | (
                Self::List(u)
                | Self::Iterator(u)
                | Self::MinHeap(u)
                | Self::MaxHeap(u)
                | Self::Deque(u),
                Self::Sequence(t),
            ) => t.overlaps(u),
            (Self::Sequence(t), Self::String) | (Self::String, Self::Sequence(t)) => {
                Self::String.overlaps(t)
            }
            (Self::Sequence(t), Self::Tuple(elems)) | (Self::Tuple(elems), Self::Sequence(t)) => {
                elems.iter().all(|elem| elem.overlaps(t))
            }
            (Self::Sequence(t), Self::Map { key, value })
            | (Self::Map { key, value }, Self::Sequence(t)) => {
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

    // BRUH
    pub fn is_incompatible_with(&self, other: &Self) -> bool {
        !self.is_subtype(other) && !other.is_subtype(self)
    }

    /// Returns a new type with the element type replaced. For container types
    /// like `List<T>`, this returns `List<new_elem>`. Returns `None` if the
    /// type does not have a replaceable element type.
    pub fn with_element_type(&self, new_elem: Self) -> Self {
        match self {
            Self::List(_) => Self::List(Box::new(new_elem)),
            Self::Sequence(_) => Self::Sequence(Box::new(new_elem)),
            Self::Iterator(_) => Self::Iterator(Box::new(new_elem)),
            Self::MinHeap(_) => Self::MinHeap(Box::new(new_elem)),
            Self::MaxHeap(_) => Self::MaxHeap(Box::new(new_elem)),
            Self::Deque(_) => Self::Deque(Box::new(new_elem)),
            Self::Map { key, .. } => Self::Map {
                key: key.clone(),
                value: Box::new(new_elem),
            },
            _ => self.clone(),
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
    }

    pub fn unpack(&self) -> Option<Box<dyn Iterator<Item = &Self> + '_>> {
        match self {
            // Any just unpacks to an infinite list of Any
            // The same types as `is_unknown`, spelled out for this exhaustive match.
            Self::Any | Self::Var(_) | Self::Generic(_) => {
                Some(Box::new(std::iter::repeat(&Self::Any)))
            }
            Self::List(elem)
            | Self::Sequence(elem)
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
            Self::Sequence(elem) => write!(f, "Sequence<{elem}>"),
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
    fn test_list_of_type_compatibility() {
        let list_of_two_tuple_int = StaticType::List(Box::new(StaticType::Tuple(vec![
            StaticType::Int,
            StaticType::Int,
        ])));
        let list_of_any = StaticType::List(Box::new(StaticType::Any));
        assert!(list_of_two_tuple_int.is_subtype(&list_of_any));
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
        let seq_of = |elem: StaticType| StaticType::Sequence(Box::new(elem));

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
        assert!(!StaticType::Int.is_subtype(&StaticType::Number));
        assert!(!StaticType::Float.is_subtype(&StaticType::Number));
        assert_eq!(StaticType::Int.lub(&StaticType::Float), StaticType::Any);
        assert_eq!(StaticType::Int.lub(&StaticType::Number), StaticType::Any);
        assert!(StaticType::from_name_and_args("Rational", vec![]).is_err());
        assert!(StaticType::from_name_and_args("Complex", vec![]).is_err());
    }
}
