use std::rc::Rc;

use num::ToPrimitive;

use crate::interpreter::environment::Environment;
use crate::interpreter::num::SingleNumberFunction;
use crate::interpreter::{Number, Value};

pub fn bind_to_environment(env: &mut Environment) {
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
            let function = $crate::interpreter::Value::Function(std::rc::Rc::new(
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
