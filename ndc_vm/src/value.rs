use crate::chunk::{Chunk, OpCode};
use ndc_parser::{StaticType, TypeSignature};
use std::fmt::Formatter;
use std::rc::Rc;

/// Enumerates all the different types of values that exist in the language
/// All values should be pretty cheap to clone because the bigger ones are wrapped using Rc's
#[derive(Clone, Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    None,
    Object(Box<Object>),
}

#[derive(Clone, Debug)]
pub enum Object {
    Some(Value),
    BigInt(num::BigInt),
    Complex(num::Complex<f64>),
    Rational(num::BigRational),
    String(String),
    List(Vec<Value>),
    Tuple(Vec<Value>),
    Function(Function),
    // tec....
}

#[derive(Clone)]
pub enum Function {
    Compiled(Rc<CompiledFunction>),
    Native(Rc<dyn Fn(&[Value]) -> Value>),
}

pub struct CompiledFunction {
    pub name: Option<String>,
    pub(crate) type_signature: TypeSignature,
    pub(crate) body: Chunk,
    pub(crate) return_type: StaticType,
}

impl CompiledFunction {
    pub fn opcodes(&self) -> &[OpCode] {
        self.body.opcodes()
    }
}

impl From<Object> for Value {
    fn from(value: Object) -> Self {
        Self::Object(Box::new(value))
    }
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Compiled(func) => write!(f, "function {:?}", func.name),
            Self::Native(_) => write!(f, "<native function>"),
        }
    }
}
