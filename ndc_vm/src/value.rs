use crate::chunk::Chunk;
use ndc_core::hash_map::HashMap;
use ndc_parser::{StaticType, TypeSignature};
use std::cell::RefCell;
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
    Function(Rc<Function>),
    // tec....
}

pub struct Function {
    name: Option<String>,
    documentation: Option<String>,
    body: FunctionBody,
}

impl Function {
    pub(crate) fn new_compiled(
        name: Option<String>,
        documentation: Option<String>,
        type_signature: TypeSignature,
        body: Chunk,
        return_type: StaticType,
    ) -> Self {
        Self {
            name,
            documentation,
            body: FunctionBody::Compiled {
                type_signature,
                body,
                return_type,
            },
        }
    }

    pub fn into_chunk(self) -> Chunk {
        match self.body {
            FunctionBody::Compiled { body, .. } => body,
            FunctionBody::Memoized { .. } => panic!("cannot get chunk from memoized function"),
        }
    }
}

pub enum FunctionBody {
    Compiled {
        type_signature: TypeSignature,
        body: Chunk,
        return_type: StaticType,
        // environment: Rc<RefCell<Environment>>,
    },
    // NativeFunction {
    //     type_signature: TypeSignature,
    //     return_type: StaticType,
    //     function: fn(&mut [Value], &Rc<RefCell<Environment>>) -> EvaluationResult,
    // },
    Memoized {
        cache: RefCell<HashMap<u64, Value>>,
        function: Box<Self>,
    },
}

impl From<Object> for Value {
    fn from(value: Object) -> Self {
        Self::Object(Box::new(value))
    }
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // Whatever
        write!(f, "function {:?}", self.name)
    }
}
