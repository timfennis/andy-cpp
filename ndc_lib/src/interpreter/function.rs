use ndc_parser::{ExpressionLocation, ResolvedVar};
use crate::hash_map::{DefaultHasher, HashMap};
use crate::interpreter::environment::Environment;
use crate::interpreter::evaluate::{
    ErrorConverter, EvaluationError, EvaluationResult, evaluate_expression,
};
use crate::interpreter::num::{BinaryOperatorError, Number};
use crate::interpreter::sequence::Sequence;
use crate::interpreter::value::Value;
use derive_builder::Builder;
use ndc_lexer::Span;
pub use ndc_parser::{Parameter, StaticType, TypeSignature};
use std::cell::{BorrowError, BorrowMutError, RefCell};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

/// Callable is a wrapper around a `OverloadedFunction` pointer and the environment to make it
/// easy to have an executable function as a method signature in the standard library
pub struct Callable<'a> {
    pub function: Rc<Function>,
    pub environment: &'a Rc<RefCell<Environment>>,
}

impl Callable<'_> {
    pub fn call(&self, args: &mut [Value]) -> EvaluationResult {
        self.function.call(args, self.environment)
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

    pub fn call(&self, args: &mut [Value], env: &Rc<RefCell<Environment>>) -> EvaluationResult {
        let result = self.body.call(args, env);

        match result {
            Err(FunctionCarrier::Return(value)) | Ok(value) => Ok(value),
            e => e,
        }
    }

    pub fn call_checked(
        &self,
        args: &mut [Value],
        env: &Rc<RefCell<Environment>>,
    ) -> EvaluationResult {
        let arg_types = args.iter().map(|arg| arg.static_type()).collect::<Vec<_>>();

        if self.static_type().is_fn_and_matches(&arg_types) {
            self.call(args, env)
        } else {
            Err(FunctionCarrier::FunctionTypeMismatch)
        }
    }

    pub fn call_vectorized(
        &self,
        args: &mut [Value],
        env: &Rc<RefCell<Environment>>,
    ) -> EvaluationResult {
        let [left, right] = args else {
            panic!("incorrect argument count for vectorization should have been handled by caller");
        };

        let result = match (left, right) {
            (
                Value::Sequence(Sequence::Tuple(left_rc)),
                Value::Sequence(Sequence::Tuple(right_rc)),
            ) => left_rc
                .iter()
                .zip(right_rc.iter())
                .map(|(l, r)| self.call(&mut [l.clone(), r.clone()], env))
                .collect::<Result<Vec<_>, _>>()?,
            (left @ Value::Number(_), Value::Sequence(Sequence::Tuple(right_rc))) => right_rc
                .iter()
                .map(|r| self.call(&mut [left.clone(), r.clone()], env))
                .collect::<Result<Vec<_>, _>>()?,
            (Value::Sequence(Sequence::Tuple(left_rc)), right @ Value::Number(_)) => left_rc
                .iter()
                .map(|l| self.call(&mut [l.clone(), right.clone()], env))
                .collect::<Result<Vec<_>, _>>()?,
            _ => panic!("caller should handle all checks before vectorizing"),
        };

        Ok(Value::Sequence(Sequence::Tuple(Rc::new(result))))
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
    FunctionTypeMismatch, // This error has specific handling behavior and needs its own variant
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
