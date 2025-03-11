use std::cell::{BorrowError, BorrowMutError, RefCell};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::ast::ExpressionLocation;
use crate::hash_map::{DefaultHasher, HashMap};
use crate::interpreter::environment::{Environment, EnvironmentRef};
use crate::interpreter::evaluate::{
    ErrorConverter, EvaluationError, EvaluationResult, evaluate_expression,
};
use crate::interpreter::num::{Number, NumberType};
use crate::interpreter::sequence::Sequence;
use crate::interpreter::value::{Value, ValueType};
use crate::lexer::Span;

/// Callable is a wrapper aroudn a `OverloadedFunction` pointer and the environment to make it
/// easy to have an executable fucntion as a method signature in the standard library
pub struct Callable<'a> {
    pub function: Rc<RefCell<OverloadedFunction>>,
    pub environment: &'a EnvironmentRef,
}

impl Callable<'_> {
    pub fn call(&self, args: &mut [Value]) -> EvaluationResult {
        self.function.borrow().call(args, self.environment)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TypeSignature {
    Variadic,
    Exact(Vec<ParamType>),
}

#[derive(Clone)]
pub struct OverloadedFunction {
    implementations: HashMap<TypeSignature, Function>,
}

impl OverloadedFunction {
    pub fn add(&mut self, function: Function) {
        self.implementations
            .insert(function.type_signature(), function);
    }

    pub fn call(&self, args: &mut [Value], env: &EnvironmentRef) -> EvaluationResult {
        let types: Vec<ValueType> = args.iter().map(ValueType::from).collect();

        let mut best = None;
        let mut best_distance = u32::MAX;
        for (signature, function) in &self.implementations {
            let Some(cur) = match_types_to_signature(&types, signature) else {
                continue;
            };
            if cur < best_distance {
                best_distance = cur;
                best = Some(function);
            }
        }

        if let Some(function) = best {
            function.call(args, env)
        } else {
            Err(FunctionCarrier::FunctionNotFound)
        }
    }
}

/// Matches a list of `ValueTypes` to a type signature. It can return `None` if there is no match or
/// `Some(num)` where num is the sum of the distances of the types. The type `Int`, is distance 1
/// away from `Number`, and `Number` is 1 distance from `Any`, then `Int` is distance 2 from `Any`.
fn match_types_to_signature(types: &[ValueType], signature: &TypeSignature) -> Option<u32> {
    match signature {
        TypeSignature::Variadic => Some(0),
        TypeSignature::Exact(signature) => {
            if types.len() == signature.len() {
                let mut acc = 0;
                for (a, b) in types.iter().zip(signature.iter()) {
                    let dist = b.distance(a)?;
                    acc += dist;
                }

                return Some(acc);
            }

            None
        }
    }
}

impl From<Function> for OverloadedFunction {
    fn from(function: Function) -> Self {
        let type_signature = function.type_signature();
        Self {
            implementations: HashMap::from([(type_signature, function)]),
        }
    }
}

impl fmt::Debug for OverloadedFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for type_signature in self.implementations.keys() {
            write!(f, "fn({type_signature:?})")?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub enum Function {
    Closure {
        parameter_names: Vec<String>,
        body: Rc<ExpressionLocation>,
        environment: EnvironmentRef,
    },
    SingleNumberFunction {
        body: fn(number: Number) -> Number,
    },
    GenericFunction {
        type_signature: TypeSignature,
        function: fn(&mut [Value], &EnvironmentRef) -> EvaluationResult,
    },
    Memoized {
        cache: RefCell<HashMap<u64, Value>>,
        function: Box<Function>,
    },
}

impl Function {
    pub fn generic(
        type_signature: TypeSignature,
        function: fn(&mut [Value], &EnvironmentRef) -> EvaluationResult,
    ) -> Self {
        Self::GenericFunction {
            type_signature,
            function,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum ParamType {
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
    Tuple,
    Map,
    Iterator,
    MinHeap,
    MaxHeap,
    Deque,
}

impl ParamType {
    fn distance(&self, other: &ValueType) -> Option<u32> {
        #[allow(clippy::match_same_arms)]
        match (self, other) {
            (Self::Bool, ValueType::Bool) => Some(0),
            (Self::Option, ValueType::Option) => Some(0),
            (Self::Int, ValueType::Number(NumberType::Int)) => Some(0),
            (Self::Float, ValueType::Number(NumberType::Float)) => Some(0),
            (Self::Rational, ValueType::Number(NumberType::Rational)) => Some(0),
            (Self::Complex, ValueType::Number(NumberType::Complex)) => Some(0),
            (Self::String, ValueType::String) => Some(0),
            (Self::List, ValueType::List) => Some(0),
            // TODO: once ParamType supports parameters we can calculate the proper distance to Tuple
            (Self::Tuple, ValueType::Tuple(_)) => Some(0),
            (Self::Map, ValueType::Map) => Some(0),
            (Self::Iterator, ValueType::Iterator) => Some(0),
            (Self::Function, ValueType::Function) => Some(0),
            (Self::Deque, ValueType::Deque) => Some(0),
            (Self::MinHeap, ValueType::MinHeap) => Some(0),
            (Self::MaxHeap, ValueType::MaxHeap) => Some(0),
            (Self::Any, _) => Some(2),
            (Self::Number, ValueType::Number(_)) => Some(1),
            (
                Self::Sequence,
                ValueType::List
                | ValueType::String
                | ValueType::Map
                // Sequence is always 1 distance to tuple
                | ValueType::Tuple(_)
                | ValueType::Iterator
                | ValueType::MinHeap
                | ValueType::MaxHeap
                | ValueType::Deque,
            ) => Some(1),
            _ => None,
        }
    }
}

/// Converts the concrete type of a value to the specific `ParamType`
impl From<&Value> for ParamType {
    fn from(value: &Value) -> Self {
        match value {
            Value::Option(_) => Self::Option,
            Value::Number(Number::Rational(_)) => Self::Rational,
            Value::Number(Number::Complex(_)) => Self::Complex,
            Value::Number(Number::Int(_)) => Self::Int,
            Value::Number(Number::Float(_)) => Self::Float,
            Value::Bool(_) => Self::Bool,
            Value::Sequence(Sequence::String(_)) => Self::String,
            Value::Sequence(Sequence::List(_)) => Self::List,
            Value::Sequence(Sequence::Tuple(_)) => Self::Tuple,
            Value::Function(_) => Self::Function,
            Value::Sequence(Sequence::Map(_, _)) => Self::Map,
            Value::Sequence(Sequence::Iterator(_)) => Self::Iterator,
            Value::Sequence(Sequence::MaxHeap(_)) => Self::MaxHeap,
            Value::Sequence(Sequence::MinHeap(_)) => Self::MinHeap,
            Value::Sequence(Sequence::Deque(_)) => Self::Deque,
        }
    }
}

impl Function {
    fn type_signature(&self) -> TypeSignature {
        match self {
            Self::Closure {
                parameter_names, ..
            } => TypeSignature::Exact(parameter_names.iter().map(|_| ParamType::Any).collect()),
            Self::Memoized { cache: _, function } => function.type_signature(),
            Self::SingleNumberFunction { .. } => TypeSignature::Exact(vec![ParamType::Number]),
            Self::GenericFunction { type_signature, .. } => type_signature.clone(),
        }
    }

    pub fn call(&self, args: &mut [Value], env: &EnvironmentRef) -> EvaluationResult {
        match self {
            Self::Closure {
                body,
                environment,
                parameter_names: parameters,
            } => {
                let mut local_scope = Environment::new_scope(environment);

                let mut env = local_scope.borrow_mut();
                for (name, value) in parameters.iter().zip(args.iter()) {
                    //TODO: is this clone a good plan?
                    env.declare(name, value.clone());
                }
                // This drop is very important
                drop(env);

                match evaluate_expression(body, &mut local_scope) {
                    Err(FunctionCarrier::Return(v)) => Ok(v),
                    r => r,
                }
            }
            Self::SingleNumberFunction { body } => match args {
                [Value::Number(num)] => Ok(Value::Number((body)(num.clone()))),
                [v] => Err(FunctionCallError::ArgumentTypeError {
                    expected: ValueType::Number(NumberType::Float),
                    actual: v.value_type(),
                }
                .into()),
                _ => Err(FunctionCallError::ArgumentCountError {
                    expected: 1,
                    actual: 0,
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

#[derive(thiserror::Error, Debug)]
pub enum FunctionCallError {
    #[error("invalid argument, expected {expected} got {actual}")]
    ArgumentTypeError {
        expected: ValueType,
        actual: ValueType,
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
    #[error("evaluation error {0}")]
    EvaluationError(#[from] EvaluationError),
    #[error("function does not exist")]
    FunctionNotFound, // This error has specific handling behavior and needs its own variant
    #[error("unconverted evaluation error")]
    IntoEvaluationError(Box<dyn ErrorConverter>),
}

impl FunctionCarrier {
    #[must_use]
    pub fn lift(self, span: Span) -> Self {
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
