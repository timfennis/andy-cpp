use crate::r#struct::StructId;
use itertools::Itertools;
use std::fmt;

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
    /// `push<T>([T], T)`. Each call stands for its own type in place of
    /// it, which the analyser works out from the arguments. Every method
    /// here treats one as `Any`.
    Generic(Box<str>),
    /// The type of an expression whose check failed, which was reported.
    /// Nothing built from it is reported again: it fits anything, anything
    /// fits it, and a call it's passed to fails silently. In `[p].map(fn(p)
    /// => p.size).sum()`, only the missing `size` is reported, not `sum` of
    /// a list of what it gave. Only the analyser makes it, and it shows as
    /// `Any`.
    Error,
    /// Bottom type for diverging expressions (`return`, `break`, `continue`).
    /// A value of it fits anywhere, and it joins with any type as that type.
    Never,
    /// The type of what nothing in the program says the type of, as what an
    /// empty `[]`, `%{}` or `None` holds, or the parameter of a function
    /// that's never called: a type no other type is, which has no values. A type parameter or an
    /// `Any` takes it, but a `[Int]` or a `[Any]` isn't a
    /// `[Unknown]`, so which overload `pick([])` means isn't decided by
    /// it. Only the analyser makes it.
    Unknown,
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

    // What a value of a kind of sequence iterates as, which only a parameter is: `[Int]` passes for an `Iterable<Int>`.
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
        "Any", "Never", "Bool", "Number", "Float", "Int", "String", "Option", "Iterable",
        "Iterator", "MinHeap", "MaxHeap", "Deque",
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

    /// The type a value of either type has: the type itself when both are
    /// the same, and otherwise `Any`. `Never` has no values, so it joins as
    /// the other type. `[Int]` and `[Float]` join as `Any`, not as
    /// `[Any]`, which neither list is.
    #[must_use]
    pub fn join(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Never, other) | (other, Self::Never) => other.clone(),
            (Self::Error, _) | (_, Self::Error) => Self::Error,
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
    /// - `[T]`, `Iterator<T>`, etc. → `T`
    /// - `String` → `String`
    /// - `(T1, ..., Tn)` → the join of `T1`, ..., `Tn`
    /// - `%{K: V}` → `(K, V)`
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
            Self::Error => Some(Self::Error),
            unknown if unknown.is_unknown() => Some(Self::Any),
            _ => None,
        }
    }

    /// Whether a value of this type can be passed where `parameter` is
    /// expected as it is, with nothing to check as the program runs. The
    /// two types are the same, except that `parameter` can be `Any`, which
    /// takes anything, and that its type parameters and the parts of it not
    /// known yet stand for any type. Inside a container, an `Option` or a
    /// tuple, `Any` is just another type: a `[Int]` doesn't fit a
    /// `[Any]`, as the list could then get a String. A function that
    /// takes an `Any` fits a function type with any parameter there, and
    /// any function fits one that returns `Any`. A `Iterable<T>` takes any
    /// kind of sequence of what fits `T`.
    pub fn fits(&self, parameter: &Self) -> bool {
        matches!(parameter, Self::Any) || self.matches(parameter, false, true)
    }

    /// How many parts this type has, itself included: `[(Int, String)]`
    /// has four.
    pub fn size(&self) -> usize {
        let parts = std::cell::Cell::new(0);
        self.contains(&|_| {
            parts.set(parts.get() + 1);
            false
        });
        parts.get()
    }

    /// Whether a value of this type might be passed where `parameter` is
    /// expected, once what isn't known about it is: as [`Self::fits`],
    /// except that the parts of this type not known yet stand for any type
    /// too. `Any` is not one of them: it goes only where `Any` is taken,
    /// and takes a cast to go anywhere else.
    pub fn may_fit(&self, parameter: &Self) -> bool {
        matches!(parameter, Self::Any) || self.matches(parameter, true, true)
    }

    /// Whether this type is `parameter`, where what [`Self::fits`] and
    /// [`Self::may_fit`] let stand for any type does, and the variables of
    /// this type too if `unknowns_match`. Where `loose`, a function that
    /// takes an `Any` fits a function type with any parameter there, and any
    /// function one that returns `Any`. That holds for a value and what a
    /// function returns, not inside a container, which could be given the
    /// other function, or a parameter, which flips which side takes what: a
    /// `fn(fn(Any) -> Int) -> Int` may pass a String to its callback, so it
    /// isn't a `fn(fn(Int) -> Int) -> Int`.
    fn matches(&self, parameter: &Self, unknowns_match: bool, loose: bool) -> bool {
        let wildcard = |typ: &Self| matches!(typ, Self::Var(_) | Self::Generic(_));
        if wildcard(parameter)
            || (unknowns_match && wildcard(self))
            || self.is_error()
            || parameter.is_error()
        {
            return true;
        }
        let matches = |a: &Self, p: &Self| a.matches(p, unknowns_match, false);
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
                let parameters = match (a, p) {
                    // A function value whose parameters aren't known fits
                    // a function type that says nothing about them either.
                    (_, None) => true,
                    // A function that takes anything, as `print` does,
                    // takes what any function type passes, as one that
                    // takes `Any` does.
                    (None, Some(_)) => loose || unknowns_match,
                    (Some(a), Some(p)) => {
                        a.len() == p.len()
                            && a.iter()
                                .zip(p)
                                .all(|(a, p)| (loose && matches!(a, Self::Any)) || matches(a, p))
                    }
                };
                parameters
                    && ((loose && matches!(pr.as_ref(), Self::Any))
                        || ar.matches(pr, unknowns_match, loose))
            }
            _ => self == parameter,
        }
    }

    /// Whether this type stands for every type: `Any`, a type variable, a
    /// type parameter or a failed check's type, which are all `Any` to the
    /// methods here.
    pub fn is_unknown(&self) -> bool {
        matches!(
            self,
            Self::Any | Self::Var(_) | Self::Generic(_) | Self::Error
        )
    }

    /// Whether this is the type of an expression whose check failed.
    pub fn is_error(&self) -> bool {
        matches!(self, Self::Error)
    }

    /// Whether a failed check's type is part of this type: `[Error]`
    /// is the type of a list holding a value whose check failed.
    pub fn has_errors(&self) -> bool {
        self.contains(&Self::is_error)
    }

    /// This type with each part that `replace` gives a type for replaced by
    /// that type, looking at the outer parts first: `[?0]` becomes
    /// `[Int]` when `replace` maps `?0` to `Int`.
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
            | Self::Error
            | Self::Never
            | Self::Unknown
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
                parameters
                    .iter()
                    .flatten()
                    .any(|parameter| parameter.contains(test))
                    || return_type.contains(test)
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
            | Self::Error
            | Self::Never
            | Self::Unknown
            | Self::Bool
            | Self::Struct { .. }
            | Self::Number
            | Self::Float
            | Self::Int
            | Self::String => false,
        }
    }

    /// The type parameters in this type, each once, outermost first: `T`
    /// and `U` in `fn(T) -> [U]`.
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

    /// Checks whether some runtime value could satisfy both `self` and `other`.
    ///
    /// # Examples
    /// - `Iterable<String>` overlaps `[Any]`: a list of Strings is
    ///   both
    /// - `Any` overlaps every type
    /// - `[Int]` does not overlap `[String]`
    /// - `Int` does not overlap `String`
    /// - `fn(String) -> String` does not overlap `fn(Int) -> Int`, but
    ///   `fn(*) -> Int`, a function that takes anything, does
    ///
    /// This is the question a cast asks: `cast` is impossible only when no value
    /// could satisfy both the operand's type and the target, and anything else
    /// is left to the runtime check.
    ///
    /// Element types are treated as inhabited, so `[Int]` and `[String]`
    /// count as disjoint even though the empty list inhabits both. Casting an
    /// empty container to an unrelated element type is therefore rejected —
    /// a deliberate trade, because the alternative gives up the compile-time
    /// error on every non-empty `cast::<[String]>(ints)` to allow a cast
    /// that could only ever be vacuous.
    ///
    /// `Never` overlaps nothing, since no value inhabits it. A container of
    /// `Never` is empty, though, and an empty container of one kind is a
    /// container of anything: `Option<Never>` overlaps `Option<Int>`, as
    /// `None` is both.
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
            // Iterable<T> overlaps every sequence-family type whose elements
            // could be Ts.
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
            ) => Self::held_overlap(t, u),
            (Self::Iterable(t), Self::String) | (Self::String, Self::Iterable(t)) => {
                Self::held_overlap(t, &Self::String)
            }
            (Self::Iterable(t), Self::Tuple(elems)) | (Self::Tuple(elems), Self::Iterable(t)) => {
                elems.iter().all(|elem| elem.overlaps(t))
            }
            (Self::Iterable(t), Self::Map { key, value })
            | (Self::Map { key, value }, Self::Iterable(t)) => {
                let pair = Self::Tuple(vec![key.as_ref().clone(), value.as_ref().clone()]);
                Self::held_overlap(t, &pair)
            }

            // Containers of the same kind overlap when what they hold could be
            // the same.
            (Self::Option(s), Self::Option(t))
            | (Self::List(s), Self::List(t))
            | (Self::Iterator(s), Self::Iterator(t))
            | (Self::MinHeap(s), Self::MinHeap(t))
            | (Self::MaxHeap(s), Self::MaxHeap(t))
            | (Self::Deque(s), Self::Deque(t)) => Self::held_overlap(s, t),
            (Self::Tuple(s_elems), Self::Tuple(t_elems)) => {
                s_elems.len() == t_elems.len()
                    && s_elems.iter().zip(t_elems).all(|(s, t)| s.overlaps(t))
            }
            (Self::Map { key: k1, value: v1 }, Self::Map { key: k2, value: v2 }) => {
                Self::held_overlap(k1, k2) && Self::held_overlap(v1, v2)
            }

            // A function that takes anything, `(*)`, could be one of any
            // parameter list.
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
                let parameters = match (p1, p2) {
                    (Some(p1), Some(p2)) => {
                        p1.len() == p2.len() && p1.iter().zip(p2).all(|(a, b)| a.overlaps(b))
                    }
                    _ => true,
                };
                parameters && Self::held_overlap(r1, r2)
            }

            _ => false,
        }
    }

    /// Whether what two containers hold, or two functions return, could be
    /// the same: the types overlap, or one is `Never`, so there's nothing
    /// there, which the other could have too. An `Option<Never>` is `None`,
    /// and a function returning `Never` doesn't return.
    fn held_overlap(a: &Self, b: &Self) -> bool {
        matches!(a, Self::Never) || matches!(b, Self::Never) || a.overlaps(b)
    }

    pub fn index_element_type(&self) -> Option<Self> {
        if let Self::Map { value, .. } = self {
            return Some(value.as_ref().clone());
        }

        self.sequence_element_type()
    }

    /// Whether a value of this type can be called: a function, or a value
    /// whose type isn't known yet or whose check failed. An `Any` takes a
    /// cast to a function type to be called, like any other use.
    pub fn could_be_callable(&self) -> bool {
        matches!(
            self,
            Self::Function { .. } | Self::Var(_) | Self::Generic(_) | Self::Error
        )
    }

    /// Whether a value of this type is, or holds, a container a program can
    /// write to: a list, map, deque or heap, as `Option<[Int]>` does. A
    /// function holds none: what it takes and returns is checked per call.
    pub fn holds_a_mutable_container(&self) -> bool {
        match self {
            Self::List(_)
            | Self::Map { .. }
            | Self::Deque(_)
            | Self::MinHeap(_)
            | Self::MaxHeap(_) => true,
            Self::Option(inner) => inner.holds_a_mutable_container(),
            Self::Tuple(elements) => elements.iter().any(Self::holds_a_mutable_container),
            _ => false,
        }
    }

    pub fn unpack(&self) -> Option<Box<dyn Iterator<Item = &Self> + '_>> {
        match self {

            Self::Error => Some(Box::new(std::iter::repeat(&Self::Error))),
            Self::List(elem)
            | Self::Iterable(elem)
            | Self::Iterator(elem)
            | Self::MinHeap(elem)
            | Self::MaxHeap(elem)
            | Self::Deque(elem) => Some(Box::new(std::iter::repeat(&**elem))),
            Self::Tuple(types) => Some(Box::new(types.iter())),
            Self::String => Some(Box::new(std::iter::repeat(&Self::String))),
            // An `Any` could hold anything, which a cast has to say first. What
            // isn't known yet is unpacked by the analyser once it is.
            Self::Any
            | Self::Var(_)
            | Self::Generic(_)
            | Self::Bool
            | Self::Function { .. }
            | Self::Option(_)
            | Self::Number
            | Self::Float
            | Self::Int
            | Self::Map { .. }
            | Self::Struct { .. } // for now, we won't have positional unpacking
            | Self::Never
            | Self::Unknown => None,
        }
    }
}

impl fmt::Display for StaticType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // A failed check's type shows as what it was before there was one.
            Self::Any | Self::Error => write!(f, "Any"),
            Self::Var(_) => write!(f, "_"),
            Self::Never => write!(f, "Never"),
            Self::Unknown => write!(f, "Unknown"),
            Self::Bool => write!(f, "Bool"),
            Self::Function {
                parameters,
                return_type,
            } => write!(
                f,
                "fn({}) -> {return_type}",
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
            Self::List(elem) => write!(f, "[{elem}]"),
            Self::String => write!(f, "String"),
            Self::Tuple(tup) if tup.is_empty() => write!(f, "()"),
            Self::Tuple(tup) if tup.len() == 1 => write!(f, "({},)", tup[0]),
            Self::Tuple(tup) => write!(f, "({})", tup.iter().join(", ")),
            Self::Map { key, value } if matches!(value.as_ref(), Self::Tuple(t) if t.is_empty()) => {
                write!(f, "%{{{key}}}")
            }
            Self::Map { key, value } => write!(f, "%{{{key}: {value}}}"),
            Self::Iterator(elem) => write!(f, "Iterator<{elem}>"),
            Self::MinHeap(elem) => write!(f, "MinHeap<{elem}>"),
            Self::MaxHeap(elem) => write!(f, "MaxHeap<{elem}>"),
            Self::Deque(elem) => write!(f, "Deque<{elem}>"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn lists_tuples_maps_and_functions_show_as_they_are_written() {
        let list = |elem| StaticType::List(Box::new(elem));
        assert_eq!(list(list(StaticType::Int)).to_string(), "[[Int]]");
        assert_eq!(
            StaticType::Tuple(vec![StaticType::Int, list(StaticType::String)]).to_string(),
            "(Int, [String])"
        );
        assert_eq!(
            StaticType::Tuple(vec![StaticType::Int]).to_string(),
            "(Int,)"
        );
        assert_eq!(StaticType::unit().to_string(), "()");
        let map = |key, value| StaticType::Map {
            key: Box::new(key),
            value: Box::new(value),
        };
        assert_eq!(
            map(StaticType::String, list(StaticType::Int)).to_string(),
            "%{String: [Int]}"
        );
        assert_eq!(
            map(StaticType::Int, StaticType::unit()).to_string(),
            "%{Int}"
        );
        assert_eq!(
            StaticType::Function {
                parameters: Some(vec![StaticType::Int]),
                return_type: Box::new(StaticType::Bool),
            }
            .to_string(),
            "fn(Int) -> Bool"
        );
    }

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
        // An `Any` goes only where `Any` is taken, whatever it holds.
        assert!(!StaticType::Any.may_fit(&list_of_two_tuple_int));
    }

    #[test]
    fn test_overlaps() {
        let list_of = |elem: StaticType| StaticType::List(Box::new(elem));
        let seq_of = |elem: StaticType| StaticType::Iterable(Box::new(elem));

        // Mixed-direction relationships overlap: a [String] inhabits both.
        assert!(seq_of(StaticType::String).overlaps(&list_of(StaticType::Any)));
        assert!(list_of(StaticType::Any).overlaps(&seq_of(StaticType::String)));

        // Any overlaps everything, including containers of concrete elements.
        assert!(StaticType::Any.overlaps(&list_of(StaticType::Int)));
        assert!(list_of(StaticType::Int).overlaps(&StaticType::Any));

        // Functions overlap when their parameters and returns could: one that
        // takes anything, `(*)`, could take any parameters, and one that
        // never returns could return anything.
        let function =
            |parameters: Option<Vec<StaticType>>, returns: StaticType| StaticType::Function {
                parameters,
                return_type: Box::new(returns),
            };
        let int_to_int = function(Some(vec![StaticType::Int]), StaticType::Int);
        let string_to_string = function(Some(vec![StaticType::String]), StaticType::String);
        assert!(!string_to_string.overlaps(&int_to_int));
        assert!(!function(Some(vec![StaticType::Int]), StaticType::String).overlaps(&int_to_int));
        assert!(function(None, StaticType::Int).overlaps(&int_to_int));
        assert!(function(Some(vec![StaticType::Int]), StaticType::Never).overlaps(&int_to_int));
        assert!(function(Some(vec![StaticType::Any]), StaticType::Any).overlaps(&int_to_int));

        // A container of `Never` is empty, which a container of anything
        // could be, but a tuple with a `Never` in it has no value at all.
        let option_of = |elem: StaticType| StaticType::Option(Box::new(elem));
        assert!(!StaticType::Never.overlaps(&StaticType::Int));
        assert!(option_of(StaticType::Never).overlaps(&option_of(StaticType::Int)));
        assert!(list_of(StaticType::Int).overlaps(&list_of(StaticType::Never)));
        assert!(seq_of(StaticType::Never).overlaps(&StaticType::String));
        assert!(
            !StaticType::Tuple(vec![StaticType::Never, StaticType::Int])
                .overlaps(&StaticType::Tuple(vec![StaticType::Int, StaticType::Int]))
        );
        assert!(
            function(Some(vec![]), option_of(StaticType::Never))
                .overlaps(&function(Some(vec![]), option_of(StaticType::Int)))
        );

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
