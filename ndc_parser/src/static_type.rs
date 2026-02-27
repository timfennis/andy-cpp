use itertools::Itertools;
use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TypeSignature {
    Variadic,
    Exact(Vec<Parameter>),
}

impl TypeSignature {
    /// Matches a list of `ValueTypes` to a type signature. It can return `None` if there is no match or
    /// `Some(num)` where num is the sum of the distances of the types. The type `Int`, is distance 1
    /// away from `Number`, and `Number` is 1 distance from `Any`, then `Int` is distance 2 from `Any`.
    pub fn calc_type_score(&self, types: &[StaticType]) -> Option<u32> {
        match self {
            Self::Variadic => Some(0),
            Self::Exact(signature) => {
                if types.len() == signature.len() {
                    let mut acc = 0;
                    for (a, b) in types.iter().zip(signature.iter()) {
                        let dist = if a == &b.type_name {
                            0
                        } else if a.is_subtype(&b.type_name) {
                            1
                        } else {
                            return None;
                        };
                        acc += dist;
                    }

                    return Some(acc);
                }

                None
            }
        }
    }

    pub fn arity(&self) -> Option<usize> {
        match self {
            Self::Variadic => None,
            Self::Exact(args) => Some(args.len()),
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum StaticType {
    Any,
    Bool,
    Function {
        parameters: Option<Vec<StaticType>>,
        return_type: Box<StaticType>,
    },
    Option(Box<StaticType>),

    // Numbers
    Number,
    Float,
    Int,
    Rational,
    Complex,

    // Sequences List<Int> -> List<Number>
    Sequence(Box<StaticType>),
    List(Box<StaticType>),
    String,
    Tuple(Vec<StaticType>),
    Map {
        key: Box<StaticType>,
        value: Box<StaticType>,
    },
    Iterator(Box<StaticType>),
    MinHeap(Box<StaticType>),
    MaxHeap(Box<StaticType>),
    Deque(Box<StaticType>),
}

impl StaticType {
    /// Checks if `self` is a subtype of `other`.
    ///
    /// A type S is a subtype of T (S <: T) if a value of type S can be safely
    /// used wherever a value of type T is expected.
    ///
    /// # Key Rules
    /// - `Any` is the top type (supertype of all types)
    /// - `Number` > `{Float, Int, Rational, Complex}`
    /// - `Sequence<T>` > sequence types with element type T
    /// - Generic types are covariant in their type parameters
    /// - Function parameters are **contravariant**, returns are **covariant**
    ///
    /// Note: this function probably doesn't handle variadic functions correctly
    pub fn is_subtype(&self, other: &Self) -> bool {
        // Any is the universal supertype
        if matches!(other, Self::Any) {
            return true;
        }

        // Only Any satisfies the above; all other types fail here
        if matches!(self, Self::Any) {
            return false;
        }

        #[allow(clippy::match_same_arms)]
        #[allow(clippy::unnested_or_patterns)]
        match (self, other) {
            // Reflexivity: every type is a subtype of itself
            _ if self == other => true,

            // Number hierarchy: all numeric types are subtypes of Number
            (Self::Float | Self::Int | Self::Rational | Self::Complex, Self::Number) => true,

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
                            ps1.iter()
                                .zip(ps2)
                                .all(|(p1, p2)| p2.is_subtype(p1))
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
    /// - `lub(Int, Float) = Number`
    /// - `lub(List<Int>, List<Float>) = List<Number>`
    /// - `lub(List<Int>, Iterator<Int>) = Sequence<Int>`
    /// - `lub(Int, String) = Any`
    pub fn lub(&self, other: &Self) -> Self {
        // Any is the top type
        if matches!(self, Self::Any) || matches!(other, Self::Any) {
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
            // Number type lattice: all numeric types join to Number
            (Self::Float, Self::Int | Self::Rational | Self::Complex)
            | (Self::Int, Self::Float | Self::Rational | Self::Complex)
            | (Self::Rational, Self::Float | Self::Int | Self::Complex)
            | (Self::Complex, Self::Float | Self::Int | Self::Rational) => Self::Number,

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
                    (None, None) => None,
                    (Some(ps1), Some(ps2)) if ps1.len() == ps2.len() => {
                        Some(ps1.iter().zip(ps2).map(|(a, b)| a.glb(b)).collect())
                    }
                    // Incompatible parameter lists
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
            (Self::Any, t) | (t, Self::Any) => t.clone(),
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
            Self::Any => Some(Self::Any),
            _ => None,
        }
    }

    #[must_use]
    pub fn unit() -> Self {
        Self::Tuple(vec![])
    }

    #[must_use]
    pub fn supports_vectorization(&self) -> bool {
        match self {
            Self::Tuple(values) => values.iter().all(|v| v.is_number()),
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        matches!(
            self,
            Self::Number | Self::Float | Self::Int | Self::Rational | Self::Complex
        )
    }

    #[must_use]
    pub fn supports_vectorization_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Tuple(l), Self::Tuple(r))
                if {
                    l.len() == r.len()
                        && self.supports_vectorization()
                        && other.supports_vectorization()
                } =>
            {
                true
            }
            (tup @ Self::Tuple(_), maybe_num) | (maybe_num, tup @ Self::Tuple(_)) => {
                tup.supports_vectorization() && maybe_num.is_number()
            }
            _ => false,
        }
    }

    // BRUH
    pub fn is_incompatible_with(&self, other: &Self) -> bool {
        !self.is_subtype(other) && !other.is_subtype(self)
    }

    pub fn index_element_type(&self) -> Option<Self> {
        if let Self::Map { value, .. } = self {
            return Some(value.as_ref().clone());
        }

        self.sequence_element_type()
    }

    pub fn is_fn_and_matches(&self, types: &[Self]) -> bool {
        // If the thing is not a function we're not interested
        let Self::Function { parameters, .. } = self else {
            return false;
        };

        let Some(_) = parameters else {
            // If this branch happens then the function we're matching against is variadic meaning it's always a match
            return true;
        };

        self.is_subtype(&Self::Function {
            parameters: Some(types.to_vec()),
            return_type: Box::new(Self::Any),
        })
    }

    pub fn unpack(&self) -> Option<Box<dyn Iterator<Item = &Self> + '_>> {
        match self {
            // Any just unpacks to an infinite list of Any
            Self::Any => Some(Box::new(std::iter::repeat(&Self::Any))),
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
            | Self::Rational
            | Self::Complex
            | Self::Map { .. } => None,
        }
    }
}

impl fmt::Display for StaticType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Any => write!(f, "Any"),
            Self::Bool => write!(f, "Bool"),
            Self::Function {
                parameters,
                return_type,
            } => write!(
                f,
                "Function({}) -> {return_type}",
                parameters
                    .as_deref()
                    .map(|p| p.iter().join(", "))
                    .unwrap_or(String::from("*"))
            ),
            Self::Option(elem) => write!(f, "Option<{elem}>"),
            Self::Number => write!(f, "Number"),
            Self::Float => write!(f, "Float"),
            Self::Int => write!(f, "Int"),
            Self::Rational => write!(f, "Rational"),
            Self::Complex => write!(f, "Complex"),
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

        assert!(fun.is_fn_and_matches(&[list_of_two_tuple_int, StaticType::Int]));
    }
}
