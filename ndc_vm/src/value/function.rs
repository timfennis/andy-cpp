use super::{Object, Value};
use crate::chunk::{Chunk, OpCode};
use crate::error::VmError;
use ndc_core::StaticType;
use ndc_core::hash_map::HashMap;
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
}

#[derive(Clone)]
pub struct ClosureFunction {
    pub(crate) prototype: Rc<CompiledFunction>,
    pub(crate) upvalues: Vec<Rc<RefCell<UpvalueCell>>>,
}

pub enum UpvalueCell {
    Open(usize),
    Closed(Value),
}

impl CompiledFunction {
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
            Self::Memoized { function, .. } => function.prototype(),
            Self::Native(_) => None,
        }
    }

    pub fn documentation(&self) -> Option<&str> {
        match self {
            Self::Native(f) => f.documentation.as_deref(),
            Self::Memoized { function, .. } => function.documentation(),
            _ => None,
        }
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            Self::Compiled(f) => f.name.as_deref(),
            Self::Native(f) => Some(&f.name),
            Self::Closure(c) => c.prototype.name.as_deref(),
            Self::Memoized { function, .. } => function.name(),
        }
    }

    pub fn static_type(&self) -> StaticType {
        match self {
            Self::Compiled(f) => f.static_type.clone(),
            Self::Native(f) => f.static_type.clone(),
            Self::Closure(c) => c.prototype.static_type.clone(),
            Self::Memoized { function, .. } => function.static_type(),
        }
    }

    /// Returns true if this function accepts arguments of the given types,
    /// without allocating any intermediate `StaticType` values.
    pub fn matches_arg_types(&self, arg_types: &[StaticType]) -> bool {
        let st = match self {
            Self::Native(f) => &f.static_type,
            Self::Compiled(f) => &f.static_type,
            Self::Closure(c) => &c.prototype.static_type,
            Self::Memoized { function, .. } => return function.matches_arg_types(arg_types),
        };
        let StaticType::Function { parameters, .. } = st else {
            return false;
        };
        match parameters {
            None => true, // variadic: always matches
            Some(params) => {
                params.len() == arg_types.len()
                    && params
                        .iter()
                        .zip(arg_types.iter())
                        .all(|(param, actual)| actual.is_subtype(param))
            }
        }
    }

    /// Like [`matches_arg_types`] but takes runtime [`Value`]s directly,
    /// avoiding any `StaticType` allocation for parameters typed `Any`
    /// (the common case for stdlib functions).
    pub fn matches_value_args(&self, args: &[Value]) -> bool {
        let st = match self {
            Self::Native(f) => &f.static_type,
            Self::Compiled(f) => &f.static_type,
            Self::Closure(c) => &c.prototype.static_type,
            Self::Memoized { function, .. } => return function.matches_value_args(args),
        };
        let StaticType::Function { parameters, .. } = st else {
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
        }
    }
}
impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn {} {:?}>", self.name, self.static_type)
    }
}
