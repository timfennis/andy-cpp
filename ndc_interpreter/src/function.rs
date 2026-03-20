use crate::hash_map::{DefaultHasher, HashMap};
use crate::num::{BinaryOperatorError, Number};
use crate::value::Value;
use derive_builder::Builder;
use ndc_core::{Parameter, StaticType, TypeSignature};
use ndc_lexer::Span;
use ndc_vm::value::NativeFunction as VmNativeFunction;
use std::cell::{BorrowError, BorrowMutError, RefCell};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

/// Wraps a VM function value for round-tripping through interpreter values.
/// `identity` is the raw pointer of the `Rc<CompiledFunction>` prototype,
/// used to implement stable equality for VM-compiled functions.
/// `call` is optionally populated by `vm_to_interp_callable` so that the
/// interpreter can invoke the VM function when used as a HOF callback.
pub(crate) struct VmFunctionWrapper {
    pub(crate) vm_value: ndc_vm::Value,
    pub(crate) identity: Option<usize>,
    pub(crate) call: Option<Rc<dyn Fn(&mut [Value]) -> EvaluationResult>>,
}

#[derive(Clone, Builder)]
pub struct Function {
    #[builder(default, setter(strip_option))]
    name: Option<String>,
    #[builder(default, setter(strip_option))]
    documentation: Option<String>,
    body: FunctionBody,
    /// VM-native implementation of this function. When set, `make_vm_globals` uses this
    /// directly instead of wrapping the interpreter body through the bridge.
    #[builder(default, setter(strip_option))]
    vm_native: Option<Rc<VmNativeFunction>>,
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

    /// Attaches or replaces the VmNative body on this function.
    pub fn set_vm_native(&mut self, native: Rc<VmNativeFunction>) {
        self.vm_native = Some(native);
    }

    pub fn vm_native(&self) -> Option<Rc<VmNativeFunction>> {
        self.vm_native.clone()
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

    pub fn call(&self, args: &mut [Value]) -> EvaluationResult {
        let result = self.body.call(args);

        match result {
            Err(FunctionCarrier::Return(value)) | Ok(value) => Ok(value),
            e => e,
        }
    }
}

#[derive(Clone)]
pub enum FunctionBody {
    NumericUnaryOp {
        body: fn(number: Number) -> Number,
    },
    NumericBinaryOp {
        body: fn(left: Number, right: Number) -> Result<Number, BinaryOperatorError>,
    },
    Memoized {
        cache: RefCell<HashMap<u64, Value>>,
        function: Box<FunctionBody>,
    },
    /// Opaque foreign function that cannot be called by the interpreter.
    /// Used to round-trip VM functions through interpreter values.
    Opaque {
        data: Rc<dyn std::any::Any>,
        static_type: StaticType,
    },
    /// A callable VM function wrapped for use in interpreter callbacks (e.g. HOFs).
    NativeClosure {
        static_type: StaticType,
        call: Rc<dyn Fn(&mut [Value]) -> EvaluationResult>,
        /// Prototype pointer identity, used for equality comparison (same as Opaque/VmFunctionWrapper).
        identity: Option<usize>,
    },
    /// A stdlib function whose canonical implementation is a VM-native closure.
    /// The interpreter calls it via a thin adapter in `vm_bridge::call_vm_native`.
    VmNative {
        native: Rc<VmNativeFunction>,
        type_signature: TypeSignature,
        return_type: StaticType,
    },
}

impl FunctionBody {
    pub fn arity(&self) -> Option<usize> {
        match self {
            Self::NumericUnaryOp { .. } => Some(1),
            Self::NumericBinaryOp { .. } => Some(2),
            Self::Memoized { function, .. } => function.arity(),
            Self::Opaque { .. } | Self::NativeClosure { .. } => None,
            Self::VmNative { type_signature, .. } => type_signature.arity(),
        }
    }

    fn type_signature(&self) -> TypeSignature {
        match self {
            Self::Memoized { cache: _, function } => function.type_signature(),
            Self::NumericUnaryOp { .. } => {
                TypeSignature::Exact(vec![Parameter::new("num", StaticType::Number)])
            }
            Self::NumericBinaryOp { .. } => TypeSignature::Exact(vec![
                Parameter::new("left", StaticType::Number),
                Parameter::new("right", StaticType::Number),
            ]),
            Self::Opaque { .. } | Self::NativeClosure { .. } => TypeSignature::Variadic,
            Self::VmNative { type_signature, .. } => type_signature.clone(),
        }
    }

    pub fn return_type(&self) -> &StaticType {
        match self {
            Self::NumericUnaryOp { .. } | Self::NumericBinaryOp { .. } => &StaticType::Number,
            Self::Memoized { function, .. } => function.return_type(),
            Self::Opaque { static_type, .. } | Self::NativeClosure { static_type, .. } => {
                static_type
            }
            Self::VmNative { return_type, .. } => return_type,
        }
    }

    pub fn call(&self, args: &mut [Value]) -> EvaluationResult {
        match self {
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
            Self::Opaque { data, .. } => {
                if let Some(wrapper) = data.downcast_ref::<VmFunctionWrapper>() {
                    if let Some(call) = &wrapper.call {
                        return call(args);
                    }
                    // Fallback: call a Native VM function directly (no globals needed).
                    // This handles closures that were interp→vm→interp round-tripped through
                    // a list mutation sync (e.g. push/pop), where call is None but the
                    // underlying NativeFunction wraps the original interpreter closure.
                    if let ndc_vm::Value::Object(obj) = &wrapper.vm_value {
                        if let ndc_vm::value::Object::Function(ndc_vm::value::Function::Native(
                            native,
                        )) = obj.as_ref()
                        {
                            let vm_args: Vec<ndc_vm::Value> = args
                                .iter()
                                .map(|a| crate::vm_bridge::interp_to_vm(a.clone()))
                                .collect();
                            let result = match &native.func {
                                ndc_vm::value::NativeFunc::Simple(f) => f(&vm_args),
                                ndc_vm::value::NativeFunc::WithVm(f) => {
                                    f(&vm_args, &mut ndc_vm::Vm::stub())
                                }
                            }
                            .map_err(|e: ndc_vm::error::VmError| {
                                FunctionCarrier::IntoEvaluationError(Box::new(anyhow::anyhow!(e)))
                            })?;
                            return Ok(crate::vm_bridge::vm_to_interp(&result));
                        }
                    }
                }
                Err(FunctionCallError::ArgumentCountError {
                    expected: 0,
                    actual: args.len(),
                }
                .into())
            }
            Self::NativeClosure { call, .. } => call(args),
            Self::VmNative { native, .. } => crate::vm_bridge::call_vm_native(native, args),
            Self::Memoized { cache, function } => {
                let mut hasher = DefaultHasher::default();
                for arg in &*args {
                    arg.hash(&mut hasher);
                }

                let key = hasher.finish();

                if !cache.borrow().contains_key(&key) {
                    let result = function.call(args)?;
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

pub type EvaluationResult = Result<Value, FunctionCarrier>;

#[derive(thiserror::Error, Debug)]
#[error("{text}")]
pub struct EvaluationError {
    text: String,
    span: Span,
    help_text: Option<String>,
}

impl EvaluationError {
    #[must_use]
    pub fn with_help(text: String, span: Span, help_text: String) -> Self {
        Self {
            text,
            span,
            help_text: Some(help_text),
        }
    }

    #[must_use]
    pub fn new(message: String, span: Span) -> Self {
        Self {
            text: message,
            span,
            help_text: None,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn help_text(&self) -> Option<&str> {
        self.help_text.as_deref()
    }
}

pub trait ErrorConverter: fmt::Debug + fmt::Display {
    fn as_evaluation_error(&self, span: Span) -> EvaluationError;
}

impl<E> ErrorConverter for E
where
    E: fmt::Debug + fmt::Display,
{
    fn as_evaluation_error(&self, span: Span) -> EvaluationError {
        EvaluationError {
            text: format!("{self}"),
            span,
            help_text: None,
        }
    }
}
