use std::cell::RefCell;
use std::fmt::Debug;
use std::fs::read_to_string;
use std::path::Path;
use std::rc::Rc;

use num::{BigInt, ToPrimitive};

use crate::interpreter::environment::{Environment, EnvironmentRef};
use crate::interpreter::evaluate::{EvaluationError, EvaluationResult};
use crate::interpreter::function::{Function, FunctionCarrier};
use crate::interpreter::int::Int;
use crate::interpreter::num::{Number, SingleNumberFunction};
use crate::interpreter::value::{Sequence, Value, ValueType};
use crate::lexer::Location;

#[derive(Debug)]
struct GenericFunction {
    function: fn(&[Value], &EnvironmentRef) -> EvaluationResult,
}

impl Function for GenericFunction {
    fn call(&self, args: &[Value], env: &EnvironmentRef) -> EvaluationResult {
        (self.function)(args, env)
    }
}

#[allow(clippy::too_many_lines)]
pub fn bind_to_environment(env: &mut Environment) {
    env.declare(
        "print",
        Value::Function(Rc::new(GenericFunction {
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
        })),
    );
    env.declare(
        "read_file",
        Value::Function(Rc::new(GenericFunction {
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
        })),
    );
    env.declare(
        "int",
        Value::Function(Rc::new(GenericFunction {
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
        })),
    );
    env.declare(
        "ceil",
        Value::Function(Rc::new(SingleNumberFunction {
            function: |num: Number| num.ceil(),
        })),
    );
    env.declare(
        "round",
        Value::Function(Rc::new(SingleNumberFunction {
            function: |num: Number| num.round(),
        })),
    );
    env.declare(
        "floor",
        Value::Function(Rc::new(SingleNumberFunction {
            function: |num: Number| num.floor(),
        })),
    );
    macro_rules! delegate_to_f64 {
        ($method:ident) => {
            let function = $crate::interpreter::value::Value::Function(std::rc::Rc::new(
                $crate::interpreter::num::SingleNumberFunction {
                    function: |num: Number| match num {
                        Number::Int(i) => Number::Float(f64::from(i).$method()),
                        Number::Float(f) => Number::Float(f.$method()),
                        Number::Rational(r) => {
                            Number::Float(r.to_f64().unwrap_or(f64::NAN).$method())
                        }
                        Number::Complex(c) => Number::Complex(c.$method()),
                    },
                },
            ));
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
