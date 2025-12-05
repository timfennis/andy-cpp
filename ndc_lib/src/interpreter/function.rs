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
    pub function: Rc<RefCell<OverloadedFunction>>,
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
}

#[derive(Clone)]
pub enum FunctionBody {
    Closure {
        parameter_names: Vec<String>,
        body: ExpressionLocation,
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
        function: fn(&mut [Value], &Rc<RefCell<Environment>>) -> EvaluationResult,
    ) -> Self {
        Self::GenericFunction {
            type_signature,
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
    fn calc_type_score(&self, types: &[StaticType]) -> Option<u32> {
        match self {
            Self::Variadic => Some(0),
            Self::Exact(signature) => {
                if types.len() == signature.len() {
                    let mut acc = 0;
                    for (a, b) in types.iter().zip(signature.iter()) {
                        let dist = if a == &b.type_name {
                            0
                        } else if a.is_subtype_of(&b.type_name) {
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

#[derive(Clone)]
pub struct OverloadedFunction {
    implementations: HashMap<TypeSignature, Function>,
}

impl OverloadedFunction {
    pub fn from_multiple(functions: Vec<Function>) -> Self {
        Self {
            implementations: functions
                .into_iter()
                .map(|f| (f.type_signature(), f))
                .collect(),
        }
    }

    pub fn arity(&self) -> Option<usize> {
        let arity = self
            .implementations
            .iter()
            .next()
            .map(|(sig, _)| sig.arity())
            .expect("OverloadedFunction cannot be empty");

        debug_assert!(
            self.implementations
                .iter()
                .all(|(sig, _)| sig.arity() == arity),
            "failed asserting that arity of all functions in OverloadedFunction are equal: {}",
            self.implementations
                .values()
                .map(|fun| fun.name())
                .join(", ")
        );
        arity
    }

    pub fn name(&self) -> Option<&str> {
        if let Some((_, ff)) = (&self.implementations).into_iter().next() {
            return ff.name.as_deref();
        }

        None
    }

    pub fn add(&mut self, function: Function) {
        self.implementations
            .insert(function.type_signature(), function);
    }

    pub fn implementations(&self) -> impl Iterator<Item = (TypeSignature, Function)> {
        self.implementations.clone().into_iter()
    }

    /// Ads values from the other overloaded function by draining it
    pub fn merge(&mut self, other: &mut Self) {
        for (key, value) in other.implementations.drain() {
            debug_assert!(
                !self.implementations.contains_key(&key),
                "conflict while merging OverloadedFunction implementations"
            );
            self.implementations.insert(key, value);
        }
    }

    pub fn call(&self, args: &mut [Value], env: &Rc<RefCell<Environment>>) -> EvaluationResult {
        let types: Vec<StaticType> = args.iter().map(Value::static_type).collect();

        let mut best_function_match = None;
        let mut best_distance = u32::MAX;

        for (signature, function) in &self.implementations {
            let Some(cur) = signature.calc_type_score(&types) else {
                continue;
            };
            if cur < best_distance {
                best_distance = cur;
                best_function_match = Some(function);
            }
        }

        best_function_match
            .ok_or(FunctionCarrier::FunctionNotFound)
            .and_then(|function| function.body().call(args, env))
            // For now if we can't find a specific function we just try to vectorize as a fallback
            .or_else(|err| {
                if let FunctionCarrier::FunctionNotFound = err {
                    self.call_vectorized(args, env)
                } else {
                    Err(err)
                }
            })
    }

    fn call_vectorized(
        &self,
        args: &mut [Value],
        env: &Rc<RefCell<Environment>>,
    ) -> EvaluationResult {
        let [left, right] = args else {
            // Vectorized application only works in cases where there are two tuple arguments
            return Err(FunctionCarrier::FunctionNotFound);
        };

        if !left.supports_vectorization_with(right) {
            return Err(FunctionCarrier::FunctionNotFound);
        }

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
                return Err(FunctionCarrier::FunctionNotFound);
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

impl From<FunctionBody> for OverloadedFunction {
    fn from(value: FunctionBody) -> Self {
        Function::from_body(value).into()
    }
}

impl From<Function> for OverloadedFunction {
    fn from(value: Function) -> Self {
        let type_signature = value.type_signature();
        Self {
            implementations: HashMap::from([(type_signature, value)]),
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
    Function,
    Option,

    // Numbers
    Number,
    Float,
    Int,
    Rational,
    Complex,

    // Sequences
    Sequence,
    List,
    String,
    Tuple(Vec<StaticType>),
    Map,
    Iterator,
    MinHeap,
    MaxHeap,
    Deque,
}

impl StaticType {
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

    #[allow(clippy::match_like_matches_macro)]
    pub fn is_subtype_of(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Self::Any) => true,
            (Self::Int | Self::Rational | Self::Complex | Self::Float, Self::Number) => true,
            (
                Self::String
                | Self::List
                | Self::Deque
                | Self::MaxHeap
                | Self::MinHeap
                | Self::Tuple(_)
                | Self::Iterator
                | Self::Map,
                Self::Sequence,
            ) => true,
            _ => false,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Any => "Any",
            Self::Bool => "Bool",
            Self::Function => "Function",
            Self::Option => "Option",
            Self::Number => "Number",
            Self::Float => "Float",
            Self::Int => "Int",
            Self::Rational => "Rational",
            Self::Complex => "Complex",
            Self::Sequence => "Sequence",
            Self::List => "List",
            Self::String => "String",
            Self::Tuple(_) => "Tuple<...>", // TODO: what do we need alooc
            Self::Map => "Map",
            Self::Iterator => "Iterator",
            Self::MinHeap => "MinHeap",
            Self::MaxHeap => "MaxHeap",
            Self::Deque => "Deque",
        }
    }
}

impl fmt::Display for StaticType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
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
                    .map(|p| format!("{}: {}", p.name, p.type_name.as_str()))
                    .join(", ")
            ),
        }
    }
}
