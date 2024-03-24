use crate::interpreter::environment::Environment;
use crate::interpreter::function::Function;
use crate::interpreter::num::Number;
use crate::interpreter::value::Value;
use andy_cpp_macros::andycpp_function;
use num::{BigInt, Integer, ToPrimitive};

pub fn register(env: &mut Environment) {
    env.declare("lcm", Value::from(Function::generic(lcm)));

    env.declare("ceil", Value::from(Function::generic(ceil)));
    env.declare("round", Value::from(Function::generic(round)));
    env.declare("floor", Value::from(Function::generic(floor)));

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

#[andycpp_function]
fn lcm(a: BigInt, b: BigInt) -> BigInt {
    a.lcm(&b)
}

#[andycpp_function]
fn ceil(number: &Number) -> Number {
    number.ceil()
}

#[andycpp_function]
fn round(number: &Number) -> Number {
    number.round()
}

#[andycpp_function]
fn floor(number: &Number) -> Number {
    number.floor()
}
