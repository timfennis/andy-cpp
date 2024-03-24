use std::cell::RefCell;
use std::fs::read_to_string;
use std::path::Path;
use std::rc::Rc;

use num::BigInt;

use crate::interpreter::environment::Environment;
use crate::interpreter::evaluate::EvaluationError;
use crate::interpreter::function::{Function, FunctionCarrier};
use crate::interpreter::int::Int;
use crate::interpreter::num::Number;
use crate::interpreter::value::{Sequence, Value, ValueType};
use crate::lexer::Location;

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
                    .map_err(FunctionCarrier::IOError)?;
                Ok(Value::Unit)
            },
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
                        .map_err(|err| {
                            // FIXME: fix location
                            FunctionCarrier::EvaluationError(EvaluationError::io_error(
                                &err,
                                Location { line: 0, column: 0 },
                                Location { line: 0, column: 0 },
                            ))
                        })
                }
                [value] => Err(FunctionCarrier::argument_type_error(
                    &ValueType::String,
                    &ValueType::from(value),
                )),
                args => Err(FunctionCarrier::argument_count_error(1, args.len())),
            },
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
                        FunctionCarrier::ArgumentError(format!(
                            "{s} cannot be converted into an integer because \"{err}\""
                        ))
                    })?;
                    Ok(Value::Number(Number::Int(Int::BigInt(bi).simplify())))
                }
                [single_value] => Err(FunctionCarrier::ArgumentError(format!(
                    "cannot convert {single_value} to string"
                ))),
                vals => Err(FunctionCarrier::argument_count_error(1, vals.len())),
            },
        }),
    );
}
