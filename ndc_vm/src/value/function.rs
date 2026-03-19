use super::Value;
use crate::chunk::{Chunk, OpCode};
use crate::error::VmError;
use ndc_core::hash_map::HashMap;
use ndc_core::{StaticType, TypeSignature};
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
        function: Box<Self>,
    },
}

pub struct NativeFunction {
    pub name: String,
    pub func: NativeFunc,
    pub static_type: StaticType,
}

pub enum NativeFunc {
    /// Zero-allocation path: args are a slice directly into the VM stack.
    /// Use for functions that do not invoke VM callbacks (no `VmCallable` params).
    Simple(Box<dyn Fn(&[Value]) -> Result<Value, VmError>>),
    /// HOF path: args are drained off the stack before the call so `&mut Vm`
    /// can be passed safely. Use for functions with `&VmCallable` params.
    WithVm(Box<dyn Fn(&[Value], &mut crate::Vm) -> Result<Value, VmError>>),
}

pub struct CompiledFunction {
    pub name: Option<String>,
    pub(crate) type_signature: TypeSignature,
    pub(crate) body: Chunk,
    pub(crate) return_type: StaticType,
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
}
impl Function {
    pub fn prototype(&self) -> Option<&Rc<CompiledFunction>> {
        match self {
            Self::Compiled(f) => Some(f),
            Self::Closure(c) => Some(&c.prototype),
            Self::Native(_) => None,
            Self::Memoized { function, .. } => function.prototype(),
        }
    }

    /// Returns true if this function (or the inner function of a memoized wrapper)
    /// is a native function. Native functions bridge to the tree-walk interpreter,
    /// which may call closures back via `Vm::call_function` in a fresh VM context,
    /// where open upvalues pointing to the current stack would be invalid.
    pub fn is_native(&self) -> bool {
        match self {
            Self::Native(_) => true,
            Self::Memoized { function, .. } => function.is_native(),
            _ => false,
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
            Self::Compiled(f) => StaticType::Function {
                parameters: match &f.type_signature {
                    TypeSignature::Variadic => None,
                    TypeSignature::Exact(types) => {
                        Some(types.iter().map(|x| x.type_name.clone()).collect())
                    }
                },
                return_type: Box::new(f.return_type.clone()),
            },
            Self::Native(f) => f.static_type.clone(),
            Self::Closure(c) => Function::Compiled(c.prototype.clone()).static_type(),
            Self::Memoized { function, .. } => function.static_type(),
        }
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Compiled(func) => write!(f, "function {:?}", func.name),
            Self::Native(native) => write!(f, "<native function {:?}>", native.static_type),
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
            Self::Native(native) => write!(f, "<native fn {:?}>", native.static_type),
            Self::Closure(closure) => write!(f, "<closure over {:?}>", closure.prototype.name),
            Self::Memoized { function, .. } => write!(f, "<memoized {function}>"),
        }
    }
}
impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn {:?}>", self.static_type)
    }
}
