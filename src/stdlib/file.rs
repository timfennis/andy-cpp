use std::cell::RefCell;
use std::fs::read_to_string;
use std::path::Path;
use std::rc::Rc;

use num::BigInt;

use crate::interpreter::environment::Environment;
use crate::interpreter::function::{
    Function, FunctionCallError, FunctionCarrier, ParamType, TypeSignature,
};
use crate::interpreter::int::Int;
use crate::interpreter::num::Number;
use crate::interpreter::value::{Sequence, Value, ValueType};

pub fn register(env: &mut Environment) {
    env.declare(
        "print",
        Value::from(Function::GenericFunction {
            function: |args, env| {
                env.borrow_mut()
                    .with_output(|output| {
                        let mut iter = args.iter().peekable();
                        while let Some(arg) = iter.next() {
                            if iter.peek().is_some() {
                                write!(output, "{arg} ")?;
                            } else {
                                writeln!(output, "{arg}")?;
                            }
                        }
                        Ok(())
                    })
                    .map_err(|err| FunctionCarrier::IntoEvaluationError(Box::new(err)))?;
                Ok(Value::Unit)
            },
            type_signature: TypeSignature::Variadic,
        }),
    );

    env.declare(
        "dbg",
        Value::from(Function::GenericFunction {
            function: |args, env| {
                env.borrow_mut()
                    .with_output(|output| {
                        let mut iter = args.iter().peekable();
                        while let Some(arg) = iter.next() {
                            if iter.peek().is_some() {
                                write!(output, "{arg:?} ")?;
                            } else {
                                writeln!(output, "{arg:?}")?;
                            }
                        }
                        Ok(())
                    })
                    .map_err(|err| FunctionCarrier::IntoEvaluationError(Box::new(err)))?;
                Ok(Value::Unit)
            },
            type_signature: TypeSignature::Variadic,
        }),
    );
    env.declare(
        "read_file",
        Value::from(Function::GenericFunction {
            function: |args, _env| match args {
                [Value::Sequence(Sequence::String(s))] => {
                    read_to_string(Path::new(s.borrow().as_str()))
                        .map(|contents| {
                            Value::Sequence(Sequence::String(Rc::new(RefCell::new(contents))))
                        })
                        .map_err(|err| FunctionCarrier::IntoEvaluationError(Box::new(err)))
                }
                [value] => Err(FunctionCallError::ArgumentTypeError {
                    expected: ValueType::String,
                    actual: ValueType::from(value),
                }
                .into()),
                args => Err(FunctionCallError::ArgumentCountError {
                    expected: 1,
                    actual: args.len(),
                }
                .into()),
            },
            type_signature: TypeSignature::Exact(vec![ParamType::String]),
        }),
    );
    env.declare(
        "int",
        Value::from(Function::GenericFunction {
            function: |args, _env| match args {
                [Value::Number(n)] => Ok(Value::Number(n.to_int_lossy()?)),
                [Value::Sequence(Sequence::String(s))] => {
                    let s = s.borrow();
                    let bi = s.parse::<BigInt>().map_err(|err| {
                        FunctionCallError::ConvertToNativeTypeError(format!(
                            "string \"{s}\" cannot be converted into an integer because \"{err}\""
                        ))
                    })?;
                    Ok(Value::Number(Number::Int(Int::BigInt(bi).simplify())))
                }
                [single_value] => Err(FunctionCallError::ConvertToNativeTypeError(format!(
                    "cannot convert {single_value} to string"
                ))
                .into()),
                vals => Err(FunctionCallError::ArgumentCountError {
                    expected: 1,
                    actual: vals.len(),
                }
                .into()),
            },
            type_signature: TypeSignature::Exact(vec![ParamType::Any]),
        }),
    );
}
