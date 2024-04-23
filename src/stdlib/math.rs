use crate::interpreter::environment::Environment;
use crate::interpreter::num::Number;
use andy_cpp_macros::export_module;
use num::ToPrimitive;

#[export_module]
mod inner {
    use crate::interpreter::num::Number;
    use num::{BigInt, Integer};

    pub fn lcm(a: &BigInt, b: &BigInt) -> BigInt {
        a.lcm(b)
    }

    pub fn ceil(number: &Number) -> Number {
        number.ceil()
    }

    pub fn round(number: &Number) -> Number {
        number.round()
    }

    pub fn floor(number: &Number) -> Number {
        number.floor()
    }
}

pub mod f64 {
    use super::{f64, Environment, Number, ToPrimitive};

    pub fn register(env: &mut Environment) {
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
}
