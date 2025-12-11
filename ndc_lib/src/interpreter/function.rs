use crate::ast::{ExpressionLocation, ResolvedVar};
use crate::hash_map::{DefaultHasher, HashMap};
use crate::interpreter::environment::Environment;
use crate::interpreter::evaluate::{
    ErrorConverter, EvaluationError, EvaluationResult, evaluate_expression,
};
use crate::interpreter::num::{BinaryOperatorError, Number};
use crate::interpreter::sequence::Sequence;
use crate::interpreter::value::Value;
use crate::lexer::Span;
use derive_builder::Builder;
use itertools::Itertools;
use std::cell::{BorrowError, BorrowMutError, RefCell};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

/// Callable is a wrapper around a `OverloadedFunction` pointer and the environment to make it
/// easy to have an executable function as a method signature in the standard library
pub struct Callable<'a> {
    pub function: Rc<RefCell<Function>>,
    pub environment: &'a Rc<RefCell<Environment>>,
}

impl Callable<'_> {
    pub fn call(&self, args: &mut [Value]) -> EvaluationResult {
        self.function.borrow().call(args, self.environment)
    }
}

#[derive(Clone, Builder)]
pub struct Function {
    #[builder(default, setter(strip_option))]
    name: Option<String>,
    #[builder(default, setter(strip_option))]
    documentation: Option<String>,
    body: FunctionBody,
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Function(name={}, sig={})",
            self.name(),
            self.type_signature()
        )
    }
}

impl Function {
    pub fn arity(&self) -> Option<usize> {
        self.body.arity()
    }
    pub fn name(&self) -> &str {
        self.name.as_deref().unwrap_or_default()
    }

    pub fn documentation(&self) -> &str {
        self.documentation.as_deref().unwrap_or_default()
    }

    pub fn short_documentation(&self) -> &str {
        self.documentation()
            .trim()
            .lines()
            .next()
            .unwrap_or_default()
    }

    pub fn from_body(body: FunctionBody) -> Self {
        Self {
            name: None,
            documentation: None,
            body,
        }
    }

    pub fn body(&self) -> &FunctionBody {
        &self.body
    }

    pub fn type_signature(&self) -> TypeSignature {
        self.body.type_signature()
    }
    pub fn return_type(&self) -> &StaticType {
        self.body.return_type()
    }

    pub fn static_type(&self) -> StaticType {
        StaticType::Function {
            parameters: match self.body.type_signature() {
                TypeSignature::Variadic => None,
                TypeSignature::Exact(types) => {
                    Some(types.iter().map(|x| x.type_name.clone()).collect())
                }
            },
            return_type: Box::new(self.body.return_type().clone()),
        }
    }

    pub(crate) fn call(
        &self,
        args: &mut [Value],
        env: &Rc<RefCell<Environment>>,
    ) -> EvaluationResult {
        self.body.call(args, env)
    }

    fn call_vectorized(
        &self,
        args: &mut [Value],
        env: &Rc<RefCell<Environment>>,
    ) -> EvaluationResult {
        let [left, right] = args else {
            // Vectorized application only works in cases where there are two tuple arguments
            panic!("incorrect argument count for vectorization should have been handled by caller");
        };

        // TODO: let caller handle checks?
        // if !left.supports_vectorization_with(right) {
        //     return Err(FunctionCarrier::FunctionNotFound);
        // }

        let (left, right) = match (left, right) {
            // Both are tuples
            (Value::Sequence(Sequence::Tuple(left)), Value::Sequence(Sequence::Tuple(right))) => {
                (left, right.as_slice())
            }
            // Left is a number and right is a tuple
            (left @ Value::Number(_), Value::Sequence(Sequence::Tuple(right))) => (
                &mut Rc::new(vec![left.clone(); right.len()]),
                right.as_slice(),
            ),
            // Left is a tuple and right is a number
            (Value::Sequence(Sequence::Tuple(left)), right @ Value::Number(_)) => {
                (left, std::slice::from_ref(right))
            }
            _ => {
                panic!("caller should handle all checks before vectorizing")
            }
        };

        let left_mut: &mut Vec<Value> = Rc::make_mut(left);

        // Zip the mutable vector with the immutable right side and perform the operations on all elements
        // TODO: maybe one day figure out how to get rid of all these clones
        for (l, r) in left_mut.iter_mut().zip(right.iter().cycle()) {
            *l = self.call(&mut [l.clone(), r.clone()], env)?;
        }

        Ok(Value::Sequence(Sequence::Tuple(left.clone())))
    }
}

#[derive(Clone)]
pub enum FunctionBody {
    Closure {
        parameter_names: Vec<String>,
        body: ExpressionLocation,
        return_type: StaticType,
        environment: Rc<RefCell<Environment>>,
    },
    NumericUnaryOp {
        body: fn(number: Number) -> Number,
    },
    NumericBinaryOp {
        body: fn(left: Number, right: Number) -> Result<Number, BinaryOperatorError>,
    },
    GenericFunction {
        type_signature: TypeSignature,
        return_type: StaticType,
        function: fn(&mut [Value], &Rc<RefCell<Environment>>) -> EvaluationResult,
    },
    Memoized {
        cache: RefCell<HashMap<u64, Value>>,
        function: Box<FunctionBody>,
    },
}

impl FunctionBody {
    pub fn arity(&self) -> Option<usize> {
        match self {
            Self::Closure {
                parameter_names, ..
            } => Some(parameter_names.len()),
            Self::NumericUnaryOp { .. } => Some(1),
            Self::NumericBinaryOp { .. } => Some(2),
            Self::GenericFunction { type_signature, .. } => type_signature.arity(),
            Self::Memoized { function, .. } => function.arity(),
        }
    }

    pub fn generic(
        type_signature: TypeSignature,
        return_type: StaticType,
        function: fn(&mut [Value], &Rc<RefCell<Environment>>) -> EvaluationResult,
    ) -> Self {
        Self::GenericFunction {
            type_signature,
            return_type,
            function,
        }
    }

    fn type_signature(&self) -> TypeSignature {
        match self {
            Self::Closure {
                parameter_names, ..
            } => TypeSignature::Exact(
                parameter_names
                    .iter()
                    .map(|name| Parameter::new(name, StaticType::Any))
                    .collect(),
            ),
            Self::Memoized { cache: _, function } => function.type_signature(),
            Self::NumericUnaryOp { .. } => {
                TypeSignature::Exact(vec![Parameter::new("num", StaticType::Number)])
            }
            Self::NumericBinaryOp { .. } => TypeSignature::Exact(vec![
                Parameter::new("left", StaticType::Number),
                Parameter::new("right", StaticType::Number),
            ]),
            Self::GenericFunction { type_signature, .. } => type_signature.clone(),
        }
    }

    pub fn return_type(&self) -> &StaticType {
        match self {
            Self::Closure { return_type, .. } | Self::GenericFunction { return_type, .. } => {
                return_type
            }
            Self::NumericUnaryOp { .. } | Self::NumericBinaryOp { .. } => &StaticType::Number,
            Self::Memoized { function, .. } => function.return_type(),
        }
    }
    pub fn call(&self, args: &mut [Value], env: &Rc<RefCell<Environment>>) -> EvaluationResult {
        match self {
            Self::Closure {
                body, environment, ..
            } => {
                let mut local_scope = Environment::new_scope(environment);

                {
                    for (position, value) in args.iter().enumerate() {
                        // NOTE: stores a copy of the value in the environment (which is fine?)
                        // NOTE: we just assume here that the arguments are slotted in order starting at 0
                        // because why not? Is this a call convention?
                        local_scope.set(
                            ResolvedVar::Captured {
                                depth: 0,
                                slot: position,
                            },
                            value.clone(),
                        )
                    }
                }

                let local_scope = Rc::new(RefCell::new(local_scope));
                match evaluate_expression(body, &local_scope) {
                    Err(FunctionCarrier::Return(v)) => Ok(v),
                    r => r,
                }
            }
            Self::NumericUnaryOp { body } => match args {
                [Value::Number(num)] => Ok(Value::Number(body(num.clone()))),
                [v] => Err(FunctionCallError::ArgumentTypeError {
                    expected: StaticType::Number,
                    actual: v.static_type(),
                }
                .into()),
                args => Err(FunctionCallError::ArgumentCountError {
                    expected: 1,
                    actual: args.len(),
                }
                .into()),
            },
            Self::NumericBinaryOp { body } => match args {
                [Value::Number(left), Value::Number(right)] => Ok(Value::Number(
                    body(left.clone(), right.clone())
                        .map_err(|err| FunctionCarrier::IntoEvaluationError(Box::new(err)))?,
                )),
                [Value::Number(_), right] => Err(FunctionCallError::ArgumentTypeError {
                    expected: StaticType::Number,
                    actual: right.static_type(),
                }
                .into()),
                [left, _] => Err(FunctionCallError::ArgumentTypeError {
                    expected: StaticType::Number,
                    actual: left.static_type(),
                }
                .into()),
                args => Err(FunctionCallError::ArgumentCountError {
                    expected: 2,
                    actual: args.len(),
                }
                .into()),
            },
            Self::GenericFunction { function, .. } => function(args, env),
            Self::Memoized { cache, function } => {
                let mut hasher = DefaultHasher::default();
                for arg in &*args {
                    arg.hash(&mut hasher);
                }

                let key = hasher.finish();

                if !cache.borrow().contains_key(&key) {
                    let result = function.call(args, env)?;
                    cache.borrow_mut().insert(key, result);
                }

                Ok(cache
                    .borrow()
                    .get(&key)
                    .expect("guaranteed to work")
                    .clone())
            }
        }
    }
}

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

    fn arity(&self) -> Option<usize> {
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

    // Sequences
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
    #[allow(clippy::unnested_or_patterns)]
    pub fn is_subtype(&self, other: &StaticType) -> bool {
        // Any is the universal supertype
        if matches!(other, Self::Any) {
            return true;
        }

        // Only Any satisfies the above; all other types fail here
        if matches!(self, Self::Any) {
            return false;
        }

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

        let Some(param_types) = parameters else {
            // If this branch happens then the function we're matching against is variadic meaning it's always a match
            return true;
        };

        param_types.len() == types.len()
            && types
                .iter()
                .zip(param_types.iter())
                .all(|(typ1, typ2)| typ1.is_subtype(typ2))
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
            Self::Bool
            | Self::Function { .. }
            | Self::Option(_)
            | Self::Number
            | Self::Float
            | Self::Int
            | Self::Rational
            | Self::Complex
            | Self::String
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

#[derive(thiserror::Error, Debug)]
pub enum FunctionCallError {
    #[error("invalid argument, expected {expected} got {actual}")]
    ArgumentTypeError {
        expected: StaticType,
        actual: StaticType,
    },

    #[error("invalid argument count, expected {expected} arguments got {actual}")]
    ArgumentCountError { expected: usize, actual: usize },

    #[error("cannot convert argument to native type: {0}")]
    ConvertToNativeTypeError(String),
}

impl From<FunctionCallError> for FunctionCarrier {
    fn from(value: FunctionCallError) -> Self {
        Self::IntoEvaluationError(Box::new(value))
    }
}

// Named after the Carrier trait
#[derive(thiserror::Error, Debug)]
pub enum FunctionCarrier {
    #[error("not an error")]
    Return(Value),
    #[error("not an error")]
    Break(Value),
    #[error("not an error")]
    Continue,
    #[error("evaluation error {0}")]
    EvaluationError(#[from] EvaluationError),
    #[error("function does not exist")]
    FunctionNotFound, // This error has specific handling behavior and needs its own variant
    #[error("unconverted evaluation error")]
    IntoEvaluationError(Box<dyn ErrorConverter>),
}

impl FunctionCarrier {
    #[must_use]
    pub fn lift_if(self, span: Span) -> Self {
        match self {
            Self::IntoEvaluationError(into) => {
                Self::EvaluationError(into.as_evaluation_error(span))
            }
            e => e,
        }
    }
}

impl From<anyhow::Error> for FunctionCarrier {
    fn from(value: anyhow::Error) -> Self {
        Self::IntoEvaluationError(Box::new(value))
    }
}

impl From<BorrowMutError> for FunctionCarrier {
    fn from(value: BorrowMutError) -> Self {
        // TODO: maybe this needs a better message
        Self::IntoEvaluationError(Box::new(value))
    }
}

impl From<BorrowError> for FunctionCarrier {
    fn from(value: BorrowError) -> Self {
        // TODO: maybe this needs a better message
        Self::IntoEvaluationError(Box::new(value))
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
