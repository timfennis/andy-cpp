#![allow(clippy::must_use_candidate)]

use std::cell::RefCell;
use std::fs::read_to_string;
use std::path::Path;
use std::rc::Rc;

use num::{BigInt, Integer, ToPrimitive};

use andy_cpp_macros::andycpp_function;

use crate::interpreter::environment::Environment;
use crate::interpreter::evaluate::EvaluationError;
use crate::interpreter::function::{Function, FunctionCarrier};
use crate::interpreter::int::Int;
use crate::interpreter::num::Number;
use crate::interpreter::value::{Sequence, Value, ValueType};
use crate::lexer::Location;

#[andycpp_function]
pub fn lcm(a: BigInt, b: BigInt) -> BigInt {
    a.lcm(&b)
}

#[andycpp_function]
pub fn ceil(number: &Number) -> Number {
    number.ceil()
}

#[andycpp_function]
pub fn round(number: &Number) -> Number {
    number.round()
}

#[andycpp_function]
pub fn floor(number: &Number) -> Number {
    number.floor()
}

#[allow(clippy::too_many_lines)]
pub fn bind_to_environment(env: &mut Environment) {
    env.declare("lcm", Value::from(Function::generic(lcm)));

    env.declare("ceil", Value::from(Function::generic(ceil)));
    env.declare("round", Value::from(Function::generic(round)));
    env.declare("floor", Value::from(Function::generic(floor)));

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
    macro_rules! delegate_to_f64 {
        ($method:ident) => {
            let function = $crate::interpreter::value::Value::from(
                $crate::interpreter::function::Function::SingleNumberFunction {
                    body: |num: Number| match num {
                        Number::Int(i) => Number::Float(f64::from(i).$method()),
                        Number::Float(f) => Number::Float(f.$method()),
                        Number::Rational(r) => {
                            Number::Float(r.to_f64().unwrap_or(f64::NAN).$method())
                        }
                        Number::Complex(c) => Number::Complex(c.$method()),
                    },
                },
            );
            env.declare(stringify!($method), function);
        };
    }

    delegate_to_f64!(acos);
    delegate_to_f64!(acosh);
    delegate_to_f64!(asin);
    delegate_to_f64!(asinh);
    delegate_to_f64!(atan);
    delegate_to_f64!(atanh);
    delegate_to_f64!(cbrt);
    delegate_to_f64!(cos);
    delegate_to_f64!(exp);
    delegate_to_f64!(ln);
    delegate_to_f64!(log2);
    delegate_to_f64!(log10);
    delegate_to_f64!(sin);
    delegate_to_f64!(sqrt);
    delegate_to_f64!(tan);
}
