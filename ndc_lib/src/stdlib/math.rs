use crate::interpreter::environment::Environment;
use crate::interpreter::num::Number;
use crate::interpreter::sequence::Sequence;
use crate::interpreter::value::Value;
use factorial::Factorial;
use ndc_macros::export_module;
use num::ToPrimitive;

trait FallibleSum<E> {
    fn try_sum(&mut self) -> Result<Number, E>;
}

impl<C, T> FallibleSum<anyhow::Error> for C
where
    C: Iterator<Item = T>,
    T: std::borrow::Borrow<Value>,
{
    fn try_sum(&mut self) -> anyhow::Result<Number> {
        self.try_fold(Number::from(0), |acc, cur| match cur.borrow() {
            Value::Number(n) => Ok(acc + n),
            value => Err(anyhow::anyhow!(
                "cannot sum {} and number",
                value.value_type()
            )),
        })
    }
}

trait FallibleProduct<E> {
    fn try_product(&mut self) -> Result<Number, E>;
}

impl<C, T> FallibleProduct<anyhow::Error> for C
where
    C: Iterator<Item = T>,
    T: std::borrow::Borrow<Value>,
{
    fn try_product(&mut self) -> anyhow::Result<Number> {
        self.try_fold(Number::from(1), |acc, cur| match cur.borrow() {
            Value::Number(n) => Ok(acc * n),
            value => Err(anyhow::anyhow!(
                "cannot multiply {} and number",
                value.value_type()
            )),
        })
    }
}
#[export_module]
mod inner {
    use std::ops::Sub;

    use super::FallibleSum;
    use crate::interpreter::int::Int;
    use crate::interpreter::num::Number;
    use anyhow::{Context, anyhow};
    use num::{BigInt, BigRational, BigUint, Integer, complex::Complex64};

    pub fn signum(n: &Number) -> Number {
        n.signum()
    }

    pub fn real(c: Complex64) -> f64 {
        c.re
    }

    pub fn imag(c: Complex64) -> f64 {
        c.im
    }

    pub fn numerator(r: &BigRational) -> BigInt {
        r.numer().clone()
    }

    pub fn denominator(r: &BigRational) -> BigInt {
        r.denom().clone()
    }

    pub fn sum(seq: &Sequence) -> anyhow::Result<Number> {
        match seq {
            Sequence::String(_s) => Err(anyhow!("string cannot be summed")),
            Sequence::List(list) => list.borrow().iter().try_sum(),
            Sequence::Tuple(tup) => tup.iter().try_sum(),
            Sequence::Map(map, _) => map.borrow().keys().try_sum(),
            Sequence::Iterator(iter) => iter.borrow_mut().try_sum(),
            Sequence::MaxHeap(h) => h.borrow().iter().map(|v| &v.0).try_sum(),
            Sequence::MinHeap(h) => h.borrow().iter().map(|v| &v.0.0).try_sum(),
            Sequence::Deque(d) => d.borrow().iter().try_sum(),
        }
    }

    pub fn product(seq: &Sequence) -> anyhow::Result<Number> {
        match seq {
            Sequence::String(_s) => Err(anyhow!("string cannot be summed")),
            Sequence::List(list) => list.borrow().iter().try_product(),
            Sequence::Tuple(tup) => tup.iter().try_product(),
            Sequence::Map(map, _) => map.borrow().keys().try_product(),
            Sequence::Iterator(iter) => iter.borrow_mut().try_product(),
            Sequence::MaxHeap(h) => h.borrow().iter().map(|v| &v.0).try_product(),
            Sequence::MinHeap(h) => h.borrow().iter().map(|v| &v.0.0).try_product(),
            Sequence::Deque(d) => d.borrow().iter().try_product(),
        }
    }

    pub fn factorial(a: &BigInt) -> anyhow::Result<BigInt> {
        let num =
            BigUint::try_from(a).context("cannot compute the factorial of a negative number")?;

        Ok(num.factorial().into())
    }

    pub fn gcd(a: &BigInt, b: &BigInt) -> BigInt {
        a.gcd(b)
    }

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

    pub fn abs(number: &Number) -> Number {
        number.abs()
    }

    pub fn abs_diff(left: &Number, right: &Number) -> Number {
        // TODO: why are we taking references if we just have to clone?!?!?
        (left.clone().sub(right.clone())).abs()
    }

    pub fn float(value: &Value) -> anyhow::Result<f64> {
        match value {
            Value::Number(Number::Int(Int::BigInt(i))) => i
                .to_f64()
                .ok_or_else(|| anyhow::anyhow!("failed to convert int to float (overflow?)")),
            Value::Number(Number::Int(Int::Int64(i))) => i
                .to_f64()
                .ok_or_else(|| anyhow::anyhow!("failed to convert int to float (overflow?)")),
            Value::Number(Number::Rational(r)) => r
                .to_f64()
                .ok_or_else(|| anyhow::anyhow!("failed to convert rational to float (overflow?)")),
            Value::Number(Number::Float(f)) => Ok(*f),
            Value::Bool(true) => Ok(1.0),
            Value::Bool(false) => Ok(0.0),
            Value::Sequence(Sequence::String(string)) => {
                let string = string.borrow();
                Ok(string.parse::<f64>()?)
            }
            value => Err(anyhow::anyhow!(
                "cannot convert {} to float",
                value.value_type()
            )),
        }
    }
    pub fn int(value: &Value) -> anyhow::Result<Number> {
        match value {
            Value::Number(number) => Ok(number.to_int_lossy()?),
            Value::Bool(true) => Ok(Number::from(1)),
            Value::Bool(false) => Ok(Number::from(0)),
            Value::Sequence(Sequence::String(string)) => {
                let string = string.borrow();
                let bi = string.parse::<BigInt>()?;
                Ok(Number::Int(Int::BigInt(bi).simplified()))
            }
            value => Err(anyhow::anyhow!(
                "cannot convert {} to int",
                value.value_type()
            )),
        }
    }
}

pub mod f64 {
    use super::{Environment, Number, ToPrimitive, f64};
    use crate::interpreter::function::{FunctionBody, FunctionCallError, ParamType, TypeSignature};
    use crate::interpreter::int::Int;
    use crate::interpreter::sequence::Sequence;
    use crate::interpreter::value::Value;
    use num::BigInt;

    pub fn register(env: &mut Environment) {
        macro_rules! delegate_to_f64 {
            ($method:ident,$docs:literal) => {
                let function = $crate::interpreter::value::Value::function(
                    $crate::interpreter::function::Function::new_with_docs(
                        $crate::interpreter::function::FunctionBody::SingleNumberFunction {
                            body: |num: Number| match num {
                                Number::Int(i) => Number::Float(f64::from(i).$method()),
                                Number::Float(f) => Number::Float(f.$method()),
                                Number::Rational(r) => {
                                    Number::Float(r.to_f64().unwrap_or(f64::NAN).$method())
                                }
                                Number::Complex(c) => Number::Complex(c.$method()),
                            },
                        },
                        String::from($docs),
                    ),
                );
                env.declare(stringify!($method), function);
            };
        }

        delegate_to_f64!(
            acos,
            "Computes the arccosine of a number. Return value is in radians in the range [0, pi] or NaN if the number is outside the range [-1, 1]."
        );
        delegate_to_f64!(acosh, "Inverse hyperbolic cosine function.");
        delegate_to_f64!(
            asin,
            "Computes the arcsine of a number. Return value is in radians in the range [-pi/2, pi/2] or NaN if the number is outside the range [-1, 1]."
        );
        delegate_to_f64!(asinh, "Inverse hyperbolic sine function.");
        delegate_to_f64!(
            atan,
            "Computes the arctangent of a number. Return value is in radians in the range [-pi/2, pi/2];"
        );
        delegate_to_f64!(atanh, "Inverse hyperbolic tangent function.");
        delegate_to_f64!(cbrt, "Returns the cube root of a number.");
        delegate_to_f64!(cos, "Computes the cosine of a number (in radians).");
        delegate_to_f64!(exp, "Returns `e^(arg)`, (the exponential function).");
        delegate_to_f64!(ln, "Returns the natural logarithm of the number.");
        delegate_to_f64!(log2, "Returns the base 2 logarithm of the number.");
        delegate_to_f64!(log10, "Returns the base 10 logarithm of the number.");
        delegate_to_f64!(sin, "Computes the sine of a number (in radians).");
        delegate_to_f64!(sqrt, "Returns the square root of a number.");
        delegate_to_f64!(tan, "Computes the tangent of a number (in radians).");
        delegate_to_f64!(tanh, "Hyperbolic tangent function.");

        env.declare(
            "int",
            Value::function(FunctionBody::GenericFunction {
                function: |args, _env| match args {
                    [Value::Number(n)] => Ok(Value::Number(n.to_int_lossy()?)),
                    [Value::Sequence(Sequence::String(s))] => {
                        let s = s.borrow();
                        let bi = s.parse::<BigInt>().map_err(|err| {
                            FunctionCallError::ConvertToNativeTypeError(format!(
                                "string \"{s}\" cannot be converted into an integer because \"{err}\""
                            ))
                        })?;
                        Ok(Value::Number(Number::Int(Int::BigInt(bi).simplified())))
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
}
