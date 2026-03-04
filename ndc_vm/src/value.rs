use crate::chunk::{Chunk, OpCode};
use ndc_parser::{StaticType, TypeSignature};
use std::fmt;
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

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Compiled(func) => write!(f, "function {:?}", func.name),
            Self::Native(_) => write!(f, "<native function>"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(n) => write!(f, "{n}"),
            Self::Float(n) => write!(f, "{n}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::None => write!(f, "None"),
            Self::Object(obj) => write!(f, "{obj}"),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Some(v) => write!(f, "Some({v})"),
            Self::BigInt(n) => write!(f, "{n}"),
            Self::Complex(c) => write!(f, "{c}"),
            Self::Rational(r) => write!(f, "{r}"),
            Self::String(s) => write!(f, "\"{s}\""),
            Self::List(vs) => {
                write!(f, "[")?;
                for (i, v) in vs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{v}")?;
                }
                write!(f, "]")
            }
            Self::Tuple(vs) => {
                write!(f, "(")?;
                for (i, v) in vs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{v}")?;
                }
                write!(f, ")")
            }
            Self::Function(func) => write!(f, "{func}"),
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
            Self::Native(_) => write!(f, "<native fn>"),
        }
    }
}

impl fmt::Display for CompiledFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = self.name.as_deref().unwrap_or("<script>");
        writeln!(f, "== {name} ==")?;

        let mut nested: Vec<Rc<Self>> = Vec::new();

        for (i, op, constant) in self.body.iter() {
            match (op, constant) {
                (OpCode::Constant(idx), Some(val)) => {
                    write!(f, "{i:04}  {:<20}", format!("Constant({idx})"))?;
                    if let Value::Object(obj) = val {
                        if let Object::Function(Function::Compiled(func)) = obj.as_ref() {
                            let name = func.name.as_deref().unwrap_or("?");
                            writeln!(f, " ; <fn {name}>")?;
                            nested.push(Rc::clone(func));
                            continue;
                        }
                    }
                    writeln!(f, " ; {val}")?;
                }
                (op, _) => writeln!(f, "{i:04}  {op:?}")?,
            }
        }

        for func in nested {
            writeln!(f)?;
            write!(f, "{func}")?;
        }

        Ok(())
    }
}
