use crate::interpreter::environment::Environment;
use crate::interpreter::num::Number;
use crate::interpreter::sequence::Sequence;
use crate::interpreter::value::Value;
use andy_cpp_macros::export_module;
use factorial::Factorial;
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
            Value::Number(n) => Ok(acc + n.clone()),
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
            Value::Number(n) => Ok(acc * n.clone()),
            value => Err(anyhow::anyhow!(
                "cannot multiply {} and number",
                value.value_type()
            )),
        })
    }
}
#[export_module]
mod inner {
    use super::FallibleSum;
    use crate::interpreter::int::Int;
    use crate::interpreter::num::Number;
    use anyhow::{anyhow, Context};
    use num::{BigInt, BigUint, Integer};

    pub fn sum(seq: &Sequence) -> anyhow::Result<Number> {
        match seq {
            Sequence::String(_s) => Err(anyhow!("string cannot be summed")),
            Sequence::List(list) => list.borrow().iter().try_sum(),
            Sequence::Tuple(tup) => tup.iter().try_sum(),
            Sequence::Map(map, _) => map.borrow().keys().try_sum(),
            Sequence::Iterator(iter) => iter.borrow_mut().try_sum(),
        }
    }

    pub fn product(seq: &Sequence) -> anyhow::Result<Number> {
        match seq {
            Sequence::String(_s) => Err(anyhow!("string cannot be summed")),
            Sequence::List(list) => list.borrow().iter().try_product(),
            Sequence::Tuple(tup) => tup.iter().try_product(),
            Sequence::Map(map, _) => map.borrow().keys().try_product(),
            Sequence::Iterator(iter) => iter.borrow_mut().try_product(),
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
    use super::{f64, Environment, Number, ToPrimitive};
    use crate::interpreter::function::{Function, FunctionCallError, ParamType, TypeSignature};
    use crate::interpreter::int::Int;
    use crate::interpreter::sequence::Sequence;
    use crate::interpreter::value::Value;
    use num::BigInt;

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
