use super::{Object, Value};
use crate::chunk::{Chunk, OpCode};
use crate::error::VmError;
use crate::hash_map::HashMap;
use ndc_core::StaticType;
use ndc_core::r#struct::StructInfo;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;

#[derive(Clone)]
pub enum Function {
    /// A closure is a compiled function lifted to closure
    Closure(ClosureFunction),
    /// A compiled function is a function written in Andy C++ that has been compiled by the bytecode compiler
    Compiled(Rc<CompiledFunction>),
    /// A native function is one defined in rust (as part of the stdlib for instance)
    Native(Rc<NativeFunction>),

    Memoized {
        cache: Rc<RefCell<HashMap<u64, Value>>>,
        function: Rc<Self>,
    },

    /// A function passed as a function type it may not have: a value of
    /// type `Any` used as a `Fn(Int) -> Int` that holds a function whose
    /// return type isn't known. Each call checks what it returns.
    Checked {
        function: Rc<Self>,
        /// The function type it's used as, boxed so that it doesn't make
        /// every function value bigger.
        typ: Box<StaticType>,
    },
}

/// How a function fits a function type, from the function's own type,
/// which leaves out what isn't known as the program runs.
#[derive(Debug, PartialEq, Eq)]
pub enum FunctionFit {
    /// It takes what the type passes and returns what it returns.
    Proven,
    /// It takes what the type passes, but what it returns has to be checked
    /// at each call.
    IfChecked,
    /// It can't be a function of that type.
    No,
}

pub struct NativeFunction {
    pub name: String,
    pub documentation: Option<String>,
    pub func: NativeFunc,
    pub static_type: StaticType,
}

type SimpleFn = dyn Fn(&[Value]) -> Result<Value, VmError>;
type WithVmFn = dyn Fn(&[Value], &mut crate::Vm) -> Result<Value, VmError>;

pub enum NativeFunc {
    /// Zero-allocation path: args are a slice directly into the VM stack.
    /// Use for functions that do not invoke VM callbacks (no `VmCallable` params).
    Simple(Box<SimpleFn>),
    /// HOF path: args are drained off the stack before the call so `&mut Vm`
    /// can be passed safely. Use for functions with `&VmCallable` params.
    WithVm(Box<WithVmFn>),
}

pub struct CompiledFunction {
    pub name: Option<String>,
    pub(crate) static_type: StaticType,
    pub(crate) body: Chunk,
    pub(crate) num_locals: usize,
    /// For a program run at the top level, how many global slots it needs, so
    /// a declaration that never runs still has its slot; 0 for any other
    /// function.
    pub(crate) num_globals: usize,
}

#[derive(Clone)]
pub struct ClosureFunction {
    pub(crate) prototype: Rc<CompiledFunction>,
    /// The cells of the variables the closure captured, each shared with the
    /// frame that declared it, see [`Value::Cell`].
    pub(crate) upvalues: Vec<Rc<RefCell<Value>>>,
}

impl CompiledFunction {
    #[must_use]
    pub fn num_locals(&self) -> usize {
        self.num_locals
    }

    pub fn opcodes(&self) -> &[OpCode] {
        self.body.opcodes()
    }

    pub fn body(&self) -> &Chunk {
        &self.body
    }
}
impl Function {
    /// The constructor bound by a struct declaration: takes the field values
    /// positionally and produces a new instance.
    pub fn struct_constructor(info: Rc<StructInfo>) -> Self {
        let static_type = info.constructor_type();
        let name = info.name.to_string();
        Self::Native(Rc::new(NativeFunction {
            name,
            documentation: None,
            static_type,
            func: NativeFunc::Simple(Box::new(move |args| {
                if args.len() != info.fields.len() {
                    return Err(VmError::native(format!(
                        "constructor {} expects {} arguments, got {}",
                        info.name,
                        info.fields.len(),
                        args.len(),
                    )));
                }
                Ok(Value::Object(Rc::new(Object::Struct {
                    info: Rc::clone(&info),
                    fields: RefCell::new(args.to_vec()),
                })))
            })),
        }))
    }

    /// The getter bound by a struct declaration for field `index`: `x(p)`,
    /// which member access `p.x` lowers to.
    pub fn struct_getter(info: Rc<StructInfo>, index: usize) -> Self {
        let static_type = info.getter_type(index);
        let name = format!("{}.{}", info.name, info.field_name(index));
        Self::Native(Rc::new(NativeFunction {
            name,
            documentation: None,
            static_type,
            func: NativeFunc::Simple(Box::new(move |args| {
                let [receiver] = args else {
                    return Err(VmError::native(format!(
                        "field access {}.{} expects 1 argument, got {}",
                        info.name,
                        info.field_name(index),
                        args.len(),
                    )));
                };
                // The pointer comparison is the nominal type check: another
                // struct type may share the field name at a different index.
                if let Value::Object(obj) = receiver
                    && let Object::Struct {
                        info: actual,
                        fields,
                    } = obj.as_ref()
                    && Rc::ptr_eq(actual, &info)
                {
                    Ok(fields.borrow()[index].clone())
                } else {
                    Err(VmError::native(format!(
                        "cannot read field {} of {receiver:?}",
                        info.field_name(index),
                    )))
                }
            })),
        }))
    }

    /// The setter bound by a struct declaration for field `index`: `x=(p, v)`,
    /// which field assignment `p.x = v` lowers to.
    pub fn struct_setter(info: Rc<StructInfo>, index: usize) -> Self {
        let static_type = info.setter_type(index);
        let name = format!("{}.{}=", info.name, info.field_name(index));
        Self::Native(Rc::new(NativeFunction {
            name,
            documentation: None,
            static_type,
            func: NativeFunc::Simple(Box::new(move |args| {
                let [receiver, value] = args else {
                    return Err(VmError::native(format!(
                        "field assignment {}.{} expects 2 arguments, got {}",
                        info.name,
                        info.field_name(index),
                        args.len(),
                    )));
                };
                if let Value::Object(obj) = receiver
                    && let Object::Struct {
                        info: actual,
                        fields,
                    } = obj.as_ref()
                    && Rc::ptr_eq(actual, &info)
                {
                    fields.borrow_mut()[index] = value.clone();
                    Ok(Value::unit())
                } else {
                    Err(VmError::native(format!(
                        "cannot assign field {} of {receiver:?}",
                        info.field_name(index),
                    )))
                }
            })),
        }))
    }

    pub fn prototype(&self) -> Option<&Rc<CompiledFunction>> {
        match self {
            Self::Closure(c) => Some(&c.prototype),
            Self::Compiled(f) => Some(f),
            Self::Memoized { function, .. } | Self::Checked { function, .. } => {
                function.prototype()
            }
            Self::Native(_) => None,
        }
    }

    pub fn documentation(&self) -> Option<&str> {
        match self {
            Self::Native(f) => f.documentation.as_deref(),
            Self::Memoized { function, .. } | Self::Checked { function, .. } => {
                function.documentation()
            }
            _ => None,
        }
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            Self::Compiled(f) => f.name.as_deref(),
            Self::Native(f) => Some(&f.name),
            Self::Closure(c) => c.prototype.name.as_deref(),
            Self::Memoized { function, .. } | Self::Checked { function, .. } => function.name(),
        }
    }

    pub fn static_type(&self) -> StaticType {
        self.signature().clone()
    }

    /// The function's type: what it takes and returns, as far as that's
    /// known as the program runs.
    fn signature(&self) -> &StaticType {
        match self {
            Self::Compiled(f) => &f.static_type,
            Self::Native(f) => &f.static_type,
            Self::Closure(c) => &c.prototype.static_type,
            Self::Memoized { function, .. } => function.signature(),
            Self::Checked { typ, .. } => typ,
        }
    }

    /// The function a [`Self::Checked`] checks, or this one: checking its
    /// calls doesn't make it another function, as far as `==` goes.
    pub fn unchecked(&self) -> &Self {
        match self {
            Self::Checked { function, .. } => function.unchecked(),
            function => function,
        }
    }

    /// The types of the parameters, when it takes a known number of them.
    pub fn parameters(&self) -> Option<&[StaticType]> {
        match self.signature() {
            StaticType::Function {
                parameters: Some(parameters),
                ..
            } => Some(parameters),
            _ => None,
        }
    }

    /// How this function fits `expected`, a function type. A parameter or
    /// return type that its own type leaves unknown, as `Any`, fits
    /// anything, but an unknown return type has to be checked at each call:
    /// a lambda that returns a String doesn't fit `Fn(Int) -> Int`, and one
    /// whose return type isn't known fits it if checked.
    pub fn fit(&self, expected: &StaticType) -> FunctionFit {
        let StaticType::Function {
            parameters: expected_parameters,
            return_type: expected_return,
        } = expected
        else {
            return FunctionFit::No;
        };
        let StaticType::Function {
            parameters,
            return_type,
        } = self.signature()
        else {
            return FunctionFit::No;
        };
        let takes = match (expected_parameters, parameters) {
            (Some(expected), Some(parameters)) => {
                expected.len() == parameters.len()
                    && expected
                        .iter()
                        .zip(parameters)
                        .all(|(expected, parameter)| could_be(expected, parameter))
            }
            _ => true,
        };
        if !takes || !could_be(return_type, expected_return) {
            FunctionFit::No
        } else if **expected_return == StaticType::Any
            || **return_type == StaticType::Never
            || (return_type == expected_return && !has_unknowns(return_type))
        {
            FunctionFit::Proven
        } else {
            FunctionFit::IfChecked
        }
    }

    /// Whether this function accepts the given arguments, checking every
    /// element of a container argument against a parameter like
    /// `List<Int>`, which [`Self::matches_value_args`] doesn't.
    pub fn conforms_value_args(&self, args: &[Value]) -> bool {
        match self.signature() {
            StaticType::Function {
                parameters: Some(params),
                ..
            } => {
                params.len() == args.len()
                    && params
                        .iter()
                        .zip(args)
                        .all(|(param, arg)| arg.conforms_to(param))
            }
            StaticType::Function {
                parameters: None, ..
            } => true,
            _ => false,
        }
    }

    /// Returns true if this function accepts the given arguments, without
    /// allocating a `StaticType` for each of them.
    pub fn matches_value_args(&self, args: &[Value]) -> bool {
        let StaticType::Function { parameters, .. } = self.signature() else {
            return false;
        };
        match parameters {
            None => true,
            Some(params) => {
                params.len() == args.len()
                    && params
                        .iter()
                        .zip(args.iter())
                        .all(|(param, arg)| arg.matches_param(param))
            }
        }
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Compiled(func) => write!(f, "function {:?}", func.name),
            Self::Native(native) => write!(f, "{native:?}"),
            Self::Closure(closure) => write!(f, "<closure over {:?}>", closure.prototype.name),
            Self::Memoized { function, .. } => write!(f, "<memoized {:?}>", function),
            Self::Checked { function, typ } => write!(f, "<{function:?} checked as {typ}>"),
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Compiled(func) => {
                let name = func.name.as_deref().unwrap_or("?");
                write!(f, "<fn {name}>")
            }
            Self::Native(native) => write!(f, "<fn {}>", native.name),
            Self::Closure(closure) => write!(f, "<closure over {:?}>", closure.prototype.name),
            Self::Memoized { function, .. } => write!(f, "<memoized {function}>"),
            Self::Checked { function, .. } => write!(f, "{function}"),
        }
    }
}
impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn {} {:?}>", self.name, self.static_type)
    }
}

/// Whether a value of type `found` could be an `expected`, where what isn't
/// known in either, `Any` or a type parameter, could be anything.
fn could_be(found: &StaticType, expected: &StaticType) -> bool {
    let unknown = |typ: &StaticType| {
        matches!(
            typ,
            StaticType::Any | StaticType::Generic(_) | StaticType::Var(_)
        )
    };
    // A function that never returns returns whatever is expected.
    if unknown(found) || unknown(expected) || *found == StaticType::Never {
        return true;
    }
    match (found, expected) {
        (StaticType::List(a), StaticType::List(b))
        | (StaticType::Deque(a), StaticType::Deque(b))
        | (StaticType::Option(a), StaticType::Option(b))
        | (StaticType::Iterator(a), StaticType::Iterator(b))
        | (StaticType::Iterable(a), StaticType::Iterable(b))
        | (StaticType::MinHeap(a), StaticType::MinHeap(b))
        | (StaticType::MaxHeap(a), StaticType::MaxHeap(b)) => could_be(a, b),
        (found, StaticType::Iterable(element)) => found
            .sequence_element_type()
            .is_some_and(|found| could_be(&found, element)),
        (StaticType::Tuple(a), StaticType::Tuple(b)) => {
            a.len() == b.len() && a.iter().zip(b).all(|(a, b)| could_be(a, b))
        }
        (
            StaticType::Map { key, value },
            StaticType::Map {
                key: expected_key,
                value: expected_value,
            },
        ) => could_be(key, expected_key) && could_be(value, expected_value),
        (
            StaticType::Function {
                parameters,
                return_type,
            },
            StaticType::Function {
                parameters: expected_parameters,
                return_type: expected_return,
            },
        ) => {
            let takes = match (parameters, expected_parameters) {
                (Some(a), Some(b)) => {
                    a.len() == b.len() && a.iter().zip(b).all(|(a, b)| could_be(b, a))
                }
                _ => true,
            };
            takes && could_be(return_type, expected_return)
        }
        (found, expected) => found == expected,
    }
}

/// Whether `typ` leaves something unknown: `Any` or a type parameter.
fn has_unknowns(typ: &StaticType) -> bool {
    typ.contains(&|part| {
        matches!(
            part,
            StaticType::Any | StaticType::Generic(_) | StaticType::Var(_)
        )
    })
}
