use crate::interpreter::environment::Environment;
use crate::interpreter::num::{BinaryOperatorError, Number};
use crate::interpreter::sequence::Sequence;
use crate::interpreter::value::Value;
use factorial::Factorial;
use ndc_macros::export_module;
use num::ToPrimitive;
use std::ops::{Add, Mul};

trait FallibleSum {
    fn try_sum(&mut self) -> Result<Number, BinaryOperatorError>;
}

impl<C, T> FallibleSum for C
where
    C: Iterator<Item = T>,
    T: std::borrow::Borrow<Value>,
{
    fn try_sum(&mut self) -> Result<Number, BinaryOperatorError> {
        self.try_fold(Number::from(0), |acc, cur| match cur.borrow() {
            Value::Number(n) => acc.add(n),
            value => Err(BinaryOperatorError::new(format!(
                "cannot sum {} and number",
                value.value_type()
            ))),
        })
    }
}

trait FallibleProduct {
    fn try_product(&mut self) -> Result<Number, BinaryOperatorError>;
}

impl<C, T> FallibleProduct for C
where
    C: Iterator<Item = T>,
    T: std::borrow::Borrow<Value>,
{
    fn try_product(&mut self) -> Result<Number, BinaryOperatorError> {
        self.try_fold(Number::from(1), |acc, cur| match cur.borrow() {
            Value::Number(n) => acc.mul(n),
            value => Err(BinaryOperatorError::new(format!(
                "cannot multiply {} and number",
                value.value_type()
            ))),
        })
    }
}

#[export_module]
mod inner {
    use std::ops::Sub;

    use super::FallibleSum;
    use crate::interpreter::int::Int;
    use crate::interpreter::num::Number;
    use anyhow::Context;
    use num::{BigInt, BigRational, BigUint, Integer, complex::Complex64};

    /// Returns the sign of a number.
    ///
    /// For `int`, `float`, and `rational` types, this function returns `-1` if the number is negative, `0` if zero, and `1` if positive.
    /// For complex numbers, it returns the number divided by its magnitude (`z / |z|`) if non-zero, or `0` if the number is `0`.
    pub fn signum(n: &Number) -> Number {
        n.signum()
    }

    /// Returns the real part of a complex number.
    pub fn real(c: Complex64) -> f64 {
        c.re
    }

    /// Returns the imaginary part of a complex number.
    pub fn imag(c: Complex64) -> f64 {
        c.im
    }

    /// Returns the numerator of a rational number.
    pub fn numerator(r: &BigRational) -> BigInt {
        r.numer().clone()
    }

    /// Returns the denominator of a rational number.
    pub fn denominator(r: &BigRational) -> BigInt {
        r.denom().clone()
    }

    pub fn sum(seq: &Sequence) -> anyhow::Result<Number> {
        match seq {
            Sequence::String(_s) => Err(BinaryOperatorError::new(
                "string cannot be summed".to_string(),
            )),
            Sequence::List(list) => list.borrow().iter().try_sum(),
            Sequence::Tuple(tup) => tup.iter().try_sum(),
            Sequence::Map(map, _) => map.borrow().keys().try_sum(),
            Sequence::Iterator(iter) => iter.borrow_mut().try_sum(),
            Sequence::MaxHeap(h) => h.borrow().iter().map(|v| &v.0).try_sum(),
            Sequence::MinHeap(h) => h.borrow().iter().map(|v| &v.0.0).try_sum(),
            Sequence::Deque(d) => d.borrow().iter().try_sum(),
        }
        .context("type error while multiplying sequence")
    }

    pub fn product(seq: &Sequence) -> anyhow::Result<Number> {
        match seq {
            Sequence::String(_s) => Err(BinaryOperatorError::new(
                "string cannot be multiplied".to_string(),
            )),
            Sequence::List(list) => list.borrow().iter().try_product(),
            Sequence::Tuple(tup) => tup.iter().try_product(),
            Sequence::Map(map, _) => map.borrow().keys().try_product(),
            Sequence::Iterator(iter) => iter.borrow_mut().try_product(),
            Sequence::MaxHeap(h) => h.borrow().iter().map(|v| &v.0).try_product(),
            Sequence::MinHeap(h) => h.borrow().iter().map(|v| &v.0.0).try_product(),
            Sequence::Deque(d) => d.borrow().iter().try_product(),
        }
        .context("type error while multiplying sequence")
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

    pub fn abs_diff(left: &Number, right: &Number) -> Result<Number, BinaryOperatorError> {
        Ok(left.sub(right)?.abs())
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

    pub fn atan2(y: f64, x: f64) -> f64 {
        y.atan2(x)
    }

    /// Converts a given `Value` to an `Int`.
    ///
    /// Conversion rules:
    /// - Integers are unchanged
    /// - Floating-point numbers have their decimal part truncated
    /// - Rational numbers are rounded down
    /// - `true` is converted to `1`, and `false` to `0`
    /// - Strings are parsed as decimal integers; other representations result in an error
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
    use crate::interpreter::function::{
        FunctionBody, FunctionBuilder, FunctionCarrier, Parameter, StaticType, TypeSignature,
    };
    use crate::interpreter::num::BinaryOperatorError;
    use crate::interpreter::value::Value;
    use std::cmp::Ordering;
    use std::ops::Not;

    pub fn register(env: &mut Environment) {
        macro_rules! implement_binary_operator_on_num {
            ($operator:literal,$method:expr) => {
                env.declare_global_fn(
                    FunctionBuilder::default()
                        .name($operator.to_string())
                        .body(FunctionBody::NumericBinaryOp { body: $method })
                        .build()
                        .expect("must be valid"),
                );
            };
        }

        implement_binary_operator_on_num!("-", std::ops::Sub::sub);
        implement_binary_operator_on_num!("+", std::ops::Add::add);
        implement_binary_operator_on_num!("*", std::ops::Mul::mul);
        implement_binary_operator_on_num!("/", std::ops::Div::div);
        implement_binary_operator_on_num!("\\", Number::floor_div);
        implement_binary_operator_on_num!("^", Number::pow);
        implement_binary_operator_on_num!("%", std::ops::Rem::rem);
        implement_binary_operator_on_num!("%%", Number::checked_rem_euclid);

        env.declare_global_fn(
            FunctionBuilder::default()
                .body(FunctionBody::NumericUnaryOp {
                    body: std::ops::Neg::neg,
                })
                .name("-".to_string())
                .build()
                .expect("must succeed"),
        );

        macro_rules! impl_cmp {
            ($operator:literal,$expected:pat) => {
                env.declare_global_fn(
                    FunctionBuilder::default()
                        .name($operator.to_string())
                        .body(FunctionBody::GenericFunction {
                            type_signature: TypeSignature::Exact(vec![
                                Parameter::new("left", StaticType::Any),
                                Parameter::new("right", StaticType::Any),
                            ]),
                            function: |values, _env| match values {
                                [left, right] => match left.partial_cmp(&right) {
                                    Some($expected) => Ok(Value::Bool(true)),
                                    Some(_) => Ok(Value::Bool(false)),
                                    None => Err(anyhow::anyhow!("cannot compare {} and {}",left.value_type(),right.value_type()).into()),
                                },
                                _ => unreachable!("the type checker should never invoke this function if the argument count does not match")
                            },
                        })
                        .build()
                        .expect("must succeed")
                );
            };
        }

        impl_cmp!(">", Ordering::Greater);
        impl_cmp!(">=", Ordering::Greater | Ordering::Equal);
        impl_cmp!("<", Ordering::Less);
        impl_cmp!("<=", Ordering::Less | Ordering::Equal);

        env.declare_global_fn(
            FunctionBuilder::default()
                .name("==".to_string())
                .body(FunctionBody::GenericFunction {
                    type_signature: TypeSignature::Exact(vec![
                        Parameter::new("left", StaticType::Any),
                        Parameter::new("right", StaticType::Any),
                    ]),
                    function: |values, _env| match values {
                        [left, right] => Ok(Value::Bool(left == right)),
                        _ => unreachable!("the type checker should never invoke this function if the argument count does not match")
                    },
                })
                .build()
                .expect("must succeed")
        );

        env.declare_global_fn(
            FunctionBuilder::default()
                .name("!=".to_string())
                .body(FunctionBody::GenericFunction {
                    type_signature: TypeSignature::Exact(vec![
                        Parameter::new("left", StaticType::Any),
                        Parameter::new("right", StaticType::Any),
                    ]),
                    function: |values, _env| match values {
                        [left, right] => Ok(Value::Bool(left != right)),
                        _ => unreachable!("the type checker should never invoke this function if the argument count does not match")
                    },
                })
                .build()
                .expect("must succeed")
        );

        env.declare_global_fn(
            FunctionBuilder::default()
                .name("<=>".to_string())
                .body(FunctionBody::GenericFunction {
                    type_signature: TypeSignature::Exact(vec![
                        Parameter::new("left", StaticType::Any),
                        Parameter::new("right", StaticType::Any),
                    ]),
                    function: |values, _env| match values {
                        [left, right] => match left.partial_cmp(&right) {
                            Some(Ordering::Equal) => Ok(Value::from(0)),
                            Some(Ordering::Less) => Ok(Value::from(-1)),
                            Some(Ordering::Greater) => Ok(Value::from(1)),
                            None => Err(anyhow::anyhow!("cannot compare {} and {}",left.value_type(),right.value_type()).into()),
                        },
                        _ => unreachable!("the type checker should never invoke this function if the argument count does not match")
                    },
                })
                .build()
                .expect("must succeed")
        );

        env.declare_global_fn(
            FunctionBuilder::default()
                .name(">=<".to_string())
                .body(FunctionBody::GenericFunction {
                    type_signature: TypeSignature::Exact(vec![
                        Parameter::new("left", StaticType::Any),
                        Parameter::new("right", StaticType::Any),
                    ]),
                    function: |values, _env| match values {
                        [left, right] => match left.partial_cmp(&right) {
                            Some(Ordering::Equal) => Ok(Value::from(0)),
                            Some(Ordering::Less) => Ok(Value::from(1)),
                            Some(Ordering::Greater) => Ok(Value::from(-1)),
                            None => Err(anyhow::anyhow!("cannot compare {} and {}",left.value_type(),right.value_type()).into()),
                        },
                        _ => unreachable!("the type checker should never invoke this function if the argument count does not match")
                    },
                })
                .build()
                .expect("must succeed")
        );

        macro_rules! impl_bitop {
            ($operator:literal,$operation:expr) => {
                env.declare_global_fn(
                    FunctionBuilder::default()
                        .name($operator.to_string())
                        .body(FunctionBody::GenericFunction {
                            type_signature: TypeSignature::Exact(vec![
                                Parameter::new("left", StaticType::Bool),
                                Parameter::new("right", StaticType::Bool),
                            ]),
                            function: |values, _env| match values {
                                [Value::Bool(left), Value::Bool(right)] => Ok(Value::Bool($operation(*left, *right))),
                                _ => unreachable!("the type checker should never invoke this function if the argument count does not match")
                            },
                        })
                        .build()
                        .expect("must succeed")
                );
                env.declare_global_fn(
                    FunctionBuilder::default()
                        .name($operator.to_string())
                        .body(FunctionBody::GenericFunction {
                            type_signature: TypeSignature::Exact(vec![
                                Parameter::new("left", StaticType::Int),
                                Parameter::new("right", StaticType::Int),
                            ]),
                            function: |values, _env| match values {
                                // TODO: remove this clone
                                [Value::Number(Number::Int(left)), Value::Number(Number::Int(right))] => Ok(Value::Number(Number::Int($operation(left.clone(), right.clone())))),
                                _ => unreachable!("the type checker should never invoke this function if the argument count does not match")
                            },
                        })
                        .build()
                        .expect("must succeed"),
                );
            };
        }

        impl_bitop!("&", std::ops::BitAnd::bitand);
        impl_bitop!("|", std::ops::BitOr::bitor);
        impl_bitop!("~", std::ops::BitXor::bitxor);

        env.declare_global_fn(
            FunctionBuilder::default()
                .body(FunctionBody::NumericUnaryOp { body: |x| x.not() })
                .name("~".to_string())
                .build()
                .expect("must succeed"),
        );

        for ident in ["!", "not"] {
            env.declare_global_fn(
                FunctionBuilder::default()
                    .body(FunctionBody::GenericFunction {
                        type_signature: TypeSignature::Exact(vec![Parameter::new("value", StaticType::Bool)]),
                        function: |values, _env| match values {
                            [Value::Bool(b)] => Ok(Value::Bool(b.not())),
                            _ => unreachable!("the type checker should never invoke this function if the argument count does not match"),
                        },
                    })
                    .name(ident.to_string())
                    .build()
                    .expect("must succeed")
            );
        }

        env.declare_global_fn(
            FunctionBuilder::default()
                .name(">>".to_string())
                .body(FunctionBody::GenericFunction {
                    type_signature: TypeSignature::Exact(vec![
                        Parameter::new("left", StaticType::Int),
                        Parameter::new("right", StaticType::Int),
                    ]),
                    function: |values, _env| match values {
                        [Value::Number(Number::Int(left)), Value::Number(Number::Int(right))] => left.clone().checked_shr(right.clone())
                            .ok_or_else(|| FunctionCarrier::IntoEvaluationError(Box::new(BinaryOperatorError::new("cannot apply >> operator to operands".to_string())))) // TODO: improve error message
                            .map(|x| Value::Number(Number::Int(x))),
                        _ => unreachable!("the type checker should never invoke this function if the argument count does not match")
                    },
                })
                .build()
                .expect("must succeed")
        );

        env.declare_global_fn(
            FunctionBuilder::default()
                .name("<<".to_string())
                .body(FunctionBody::GenericFunction {
                    type_signature: TypeSignature::Exact(vec![
                        Parameter::new("left", StaticType::Int),
                        Parameter::new("right", StaticType::Int),
                    ]),
                    function: |values, _env| match values {
                        [Value::Number(Number::Int(left)), Value::Number(Number::Int(right))] => left.clone().checked_shl(right.clone())
                            .ok_or_else(|| FunctionCarrier::IntoEvaluationError(Box::new(BinaryOperatorError::new("cannot apply << operator to operands".to_string())))) // TODO: improve error message
                            .map(|x| Value::Number(Number::Int(x))),
                        _ => unreachable!("the type checker should never invoke this function if the argument count does not match")
                    },
                })
                .build()
                .expect("must succeed")
        );

        macro_rules! delegate_to_f64 {
            ($method:ident,$docs:literal) => {
                let function = FunctionBuilder::default()
                    .body(
                        $crate::interpreter::function::FunctionBody::NumericUnaryOp {
                            body: |num: Number| match num {
                                Number::Int(i) => Number::Float(f64::from(i).$method()),
                                Number::Float(f) => Number::Float(f.$method()),
                                Number::Rational(r) => {
                                    Number::Float(r.to_f64().unwrap_or(f64::NAN).$method())
                                }
                                Number::Complex(c) => Number::Complex(c.$method()),
                            },
                        },
                    )
                    .name(stringify!($method).to_string())
                    .documentation(String::from($docs))
                    .build()
                    .expect("expected delegate_to_f64 to always create function object correctly");
                env.declare_global_fn(function);
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
    }
}
