use factorial::Factorial;
use ndc_core::num::{BinaryOperatorError, Number};
use ndc_macros::export_module;
use ndc_vm::value::{Object, SeqValue, Value};
use num::ToPrimitive;
use std::ops::{Add, Mul};

#[export_module]
mod inner {
    use std::ops::Sub;

    use anyhow::Context;
    use ndc_core::int::Int;
    use ndc_core::num::Number;
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

    /// Returns the sum of all elements in a sequence.
    pub fn sum(seq: SeqValue) -> anyhow::Result<Number> {
        if matches!(&seq, Value::Object(o) if matches!(o.as_ref(), Object::String(_))) {
            anyhow::bail!("string cannot be summed");
        }
        seq.try_into_iter()
            .ok_or_else(|| anyhow::anyhow!("cannot sum non-sequence"))?
            .try_fold(Number::from(0), |acc, val| {
                let n = val
                    .to_number()
                    .ok_or_else(|| anyhow::anyhow!("cannot sum {}", val.static_type()))?;
                acc.add(&n).map_err(|e| anyhow::anyhow!("{e}"))
            })
    }

    /// Returns the product of all elements in a sequence.
    pub fn product(seq: SeqValue) -> anyhow::Result<Number> {
        if matches!(&seq, Value::Object(o) if matches!(o.as_ref(), Object::String(_))) {
            anyhow::bail!("string cannot be multiplied");
        }
        seq.try_into_iter()
            .ok_or_else(|| anyhow::anyhow!("cannot multiply non-sequence"))?
            .try_fold(Number::from(1), |acc, val| {
                let n = val
                    .to_number()
                    .ok_or_else(|| anyhow::anyhow!("cannot multiply {}", val.static_type()))?;
                acc.mul(&n).map_err(|e| anyhow::anyhow!("{e}"))
            })
    }

    /// Returns the factorial of a non-negative integer.
    pub fn factorial(a: &BigInt) -> anyhow::Result<BigInt> {
        let num =
            BigUint::try_from(a).context("cannot compute the factorial of a negative number")?;

        Ok(num.factorial().into())
    }

    /// Returns the greatest common divisor of two integers.
    pub fn gcd(a: &BigInt, b: &BigInt) -> BigInt {
        a.gcd(b)
    }

    /// Returns the least common multiple of two integers.
    pub fn lcm(a: &BigInt, b: &BigInt) -> BigInt {
        a.lcm(b)
    }

    /// Returns the smallest integer greater than or equal to the number.
    pub fn ceil(number: &Number) -> Number {
        number.ceil()
    }

    /// Rounds the number to the nearest integer, with ties rounding away from zero.
    pub fn round(number: &Number) -> Number {
        number.round()
    }

    /// Returns the largest integer less than or equal to the number.
    pub fn floor(number: &Number) -> Number {
        number.floor()
    }

    /// Returns the absolute value of a number.
    pub fn abs(number: &Number) -> Number {
        number.abs()
    }

    /// Returns the absolute difference between two numbers.
    pub fn abs_diff(left: &Number, right: &Number) -> Result<Number, BinaryOperatorError> {
        Ok(left.sub(right)?.abs())
    }

    /// Converts a value to a floating-point number.
    pub fn float(value: Value) -> anyhow::Result<f64> {
        match &value {
            Value::Bool(b) => Ok(if *b { 1.0 } else { 0.0 }),
            Value::Object(obj) => match obj.as_ref() {
                Object::String(s) => Ok(s.borrow().parse::<f64>()?),
                _ => value.to_f64().ok_or_else(|| {
                    anyhow::anyhow!("cannot convert {} to float", value.static_type())
                }),
            },
            _ => value
                .to_f64()
                .ok_or_else(|| anyhow::anyhow!("cannot convert {} to float", value.static_type())),
        }
    }

    /// Computes the four-quadrant arctangent of `y` and `x` in radians.
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
    pub fn int(value: Value) -> anyhow::Result<Number> {
        match &value {
            Value::Bool(b) => Ok(Number::from(if *b { 1i32 } else { 0i32 })),
            Value::Object(obj) => match obj.as_ref() {
                Object::String(s) => {
                    let bi = s.borrow().parse::<BigInt>()?;
                    Ok(Number::Int(Int::BigInt(bi).simplified()))
                }
                _ => value
                    .to_number()
                    .ok_or_else(|| {
                        anyhow::anyhow!("cannot convert {} to int", value.static_type())
                    })?
                    .to_int_lossy()
                    .map_err(|e| anyhow::anyhow!("{e}")),
            },
            _ => value
                .to_number()
                .ok_or_else(|| anyhow::anyhow!("cannot convert {} to int", value.static_type()))?
                .to_int_lossy()
                .map_err(|e| anyhow::anyhow!("{e}")),
        }
    }
}

pub mod f64 {
    use super::{Number, ToPrimitive, f64};
    use ndc_core::StaticType;
    use ndc_core::int::Int;
    use ndc_core::num::BinaryOperatorError;
    use ndc_vm::error::VmError;
    use ndc_vm::value::{NativeFunc, NativeFunction, Value};
    use std::cmp::Ordering;
    use std::ops::Not;
    use std::rc::Rc;

    pub fn register(env: &mut ndc_core::FunctionRegistry<Rc<NativeFunction>>) {
        macro_rules! implement_binary_operator_on_num {
            ($operator:literal,$method:expr,$docs:literal) => {
                env.declare_global_fn(Rc::new(NativeFunction {
                    name: $operator.to_string(),
                    documentation: Some($docs.to_string()),
                    static_type: StaticType::Function {
                        parameters: Some(vec![StaticType::Number, StaticType::Number]),
                        return_type: Box::new(StaticType::Number),
                    },
                    func: NativeFunc::Simple(Box::new(|args| match args {
                        [left, right] => {
                            let l = left.to_number().ok_or_else(|| {
                                VmError::native(format!(
                                    "expected number, got {}",
                                    left.static_type()
                                ))
                            })?;
                            let r = right.to_number().ok_or_else(|| {
                                VmError::native(format!(
                                    "expected number, got {}",
                                    right.static_type()
                                ))
                            })?;
                            $method(l, r)
                                .map(Value::from_number)
                                .map_err(|e: BinaryOperatorError| VmError::native(e.to_string()))
                        }
                        _ => Err(VmError::native(format!(
                            "expected 2 arguments, got {}",
                            args.len()
                        ))),
                    })),
                }));
            };
        }

        implement_binary_operator_on_num!("-", std::ops::Sub::sub, "Subtracts two numbers.");
        implement_binary_operator_on_num!("+", std::ops::Add::add, "Adds two numbers.");
        implement_binary_operator_on_num!("*", std::ops::Mul::mul, "Multiplies two numbers.");
        implement_binary_operator_on_num!("/", std::ops::Div::div, "Divides two numbers.");
        implement_binary_operator_on_num!(
            "\\",
            Number::floor_div,
            "Integer (floor) division of two numbers."
        );
        implement_binary_operator_on_num!(
            "^",
            Number::pow,
            "Raises the first number to the power of the second."
        );
        implement_binary_operator_on_num!(
            "%",
            std::ops::Rem::rem,
            "Returns the remainder of dividing two numbers."
        );
        implement_binary_operator_on_num!(
            "%%",
            Number::checked_rem_euclid,
            "Returns the Euclidean remainder of dividing two numbers. The result is always non-negative."
        );

        // Int-specific overloads: fast path on i64, fall back to Number on overflow/BigInt.
        macro_rules! implement_binary_operator_on_int {
            ($operator:literal, $checked_method:ident, $fallback:expr, $docs:literal) => {
                env.declare_global_fn(Rc::new(NativeFunction {
                    name: $operator.to_string(),
                    documentation: Some($docs.to_string()),
                    static_type: StaticType::Function {
                        parameters: Some(vec![StaticType::Int, StaticType::Int]),
                        return_type: Box::new(StaticType::Int),
                    },
                    func: NativeFunc::Simple(Box::new(|args| match args {
                        [Value::Int(l), Value::Int(r)] => {
                            if let Some(result) = l.$checked_method(*r) {
                                Ok(Value::Int(result))
                            } else {
                                let l = Int::Int64(*l);
                                let r = Int::Int64(*r);
                                Ok(Value::from_int($fallback(l, r)))
                            }
                        }
                        [left, right] => {
                            let l = left.to_int().ok_or_else(|| {
                                VmError::native(format!("expected int, got {}", left.static_type()))
                            })?;
                            let r = right.to_int().ok_or_else(|| {
                                VmError::native(format!(
                                    "expected int, got {}",
                                    right.static_type()
                                ))
                            })?;
                            Ok(Value::from_int($fallback(l, r)))
                        }
                        _ => Err(VmError::native(format!(
                            "expected 2 arguments, got {}",
                            args.len()
                        ))),
                    })),
                }));
            };
        }

        implement_binary_operator_on_int!(
            "+",
            checked_add,
            std::ops::Add::add,
            "Adds two integers."
        );
        implement_binary_operator_on_int!(
            "-",
            checked_sub,
            std::ops::Sub::sub,
            "Subtracts two integers."
        );
        implement_binary_operator_on_int!(
            "*",
            checked_mul,
            std::ops::Mul::mul,
            "Multiplies two integers."
        );
        implement_binary_operator_on_int!(
            "%",
            checked_rem,
            std::ops::Rem::rem,
            "Returns the remainder of dividing two integers."
        );

        // Float-specific overloads: operate directly on f64.
        macro_rules! implement_binary_operator_on_float {
            ($operator:literal, $op:expr, $docs:literal) => {
                env.declare_global_fn(Rc::new(NativeFunction {
                    name: $operator.to_string(),
                    documentation: Some($docs.to_string()),
                    static_type: StaticType::Function {
                        parameters: Some(vec![StaticType::Float, StaticType::Float]),
                        return_type: Box::new(StaticType::Float),
                    },
                    func: NativeFunc::Simple(Box::new(|args| match args {
                        [Value::Float(l), Value::Float(r)] => Ok(Value::Float($op(*l, *r))),
                        _ => Err(VmError::native(format!(
                            "expected 2 float arguments, got {}",
                            args.len()
                        ))),
                    })),
                }));
            };
        }

        implement_binary_operator_on_float!("+", std::ops::Add::add, "Adds two floats.");
        implement_binary_operator_on_float!("-", std::ops::Sub::sub, "Subtracts two floats.");
        implement_binary_operator_on_float!("*", std::ops::Mul::mul, "Multiplies two floats.");
        implement_binary_operator_on_float!("/", std::ops::Div::div, "Divides two floats.");
        implement_binary_operator_on_float!(
            "%",
            std::ops::Rem::rem,
            "Returns the remainder of dividing two floats."
        );

        env.declare_global_fn(Rc::new(NativeFunction {
            name: "-".to_string(),
            documentation: Some("Negates a number.".to_string()),
            static_type: StaticType::Function {
                parameters: Some(vec![StaticType::Number]),
                return_type: Box::new(StaticType::Number),
            },
            func: NativeFunc::Simple(Box::new(|args| match args {
                [v] => v
                    .to_number()
                    .map(std::ops::Neg::neg)
                    .map(Value::from_number)
                    .ok_or_else(|| {
                        VmError::native(format!("expected number, got {}", v.static_type()))
                    }),
                _ => Err(VmError::native(format!(
                    "expected 1 argument, got {}",
                    args.len()
                ))),
            })),
        }));

        macro_rules! impl_cmp {
            ($operator:literal,$expected:pat,$docs:literal) => {
                env.declare_global_fn(Rc::new(NativeFunction {
                    name: $operator.to_string(),
                    documentation: Some($docs.to_string()),
                    static_type: StaticType::Function {
                        parameters: Some(vec![StaticType::Any, StaticType::Any]),
                        return_type: Box::new(StaticType::Bool),
                    },
                    func: NativeFunc::Simple(Box::new(|args| match args {
                        [left, right] => match left.partial_cmp(right) {
                            Some($expected) => Ok(Value::Bool(true)),
                            Some(_) => Ok(Value::Bool(false)),
                            None => Err(VmError::native(format!(
                                "cannot compare {} and {}",
                                left.static_type(),
                                right.static_type()
                            ))),
                        },
                        _ => Err(VmError::native(format!(
                            "expected 2 arguments, got {}",
                            args.len()
                        ))),
                    })),
                }));
            };
        }

        impl_cmp!(
            ">",
            Ordering::Greater,
            "Returns true if the left value is greater than the right."
        );
        impl_cmp!(
            ">=",
            Ordering::Greater | Ordering::Equal,
            "Returns true if the left value is greater than or equal to the right."
        );
        impl_cmp!(
            "<",
            Ordering::Less,
            "Returns true if the left value is less than the right."
        );
        impl_cmp!(
            "<=",
            Ordering::Less | Ordering::Equal,
            "Returns true if the left value is less than or equal to the right."
        );

        env.declare_global_fn(Rc::new(NativeFunction {
            name: "==".to_string(),
            documentation: Some("Returns true if two values are equal.".to_string()),
            static_type: StaticType::Function {
                parameters: Some(vec![StaticType::Any, StaticType::Any]),
                return_type: Box::new(StaticType::Bool),
            },
            func: NativeFunc::Simple(Box::new(|args| match args {
                [left, right] => Ok(Value::Bool(left == right)),
                _ => Err(VmError::native(format!(
                    "expected 2 arguments, got {}",
                    args.len()
                ))),
            })),
        }));

        env.declare_global_fn(Rc::new(NativeFunction {
            name: "!=".to_string(),
            documentation: Some("Returns true if two values are not equal.".to_string()),
            static_type: StaticType::Function {
                parameters: Some(vec![StaticType::Any, StaticType::Any]),
                return_type: Box::new(StaticType::Bool),
            },
            func: NativeFunc::Simple(Box::new(|args| match args {
                [left, right] => Ok(Value::Bool(left != right)),
                _ => Err(VmError::native(format!(
                    "expected 2 arguments, got {}",
                    args.len()
                ))),
            })),
        }));

        env.declare_global_fn(Rc::new(NativeFunction {
            name: "<=>".to_string(),
            documentation: Some("Three-way comparison (spaceship operator). Returns -1 if left < right, 0 if equal, 1 if left > right.".to_string()),
            static_type: StaticType::Function {
                parameters: Some(vec![StaticType::Any, StaticType::Any]),
                return_type: Box::new(StaticType::Int),
            },
            func: NativeFunc::Simple(Box::new(|args| match args {
                [left, right] => match left.partial_cmp(right) {
                    Some(Ordering::Equal) => Ok(Value::Int(0)),
                    Some(Ordering::Less) => Ok(Value::Int(-1)),
                    Some(Ordering::Greater) => Ok(Value::Int(1)),
                    None => Err(VmError::native(format!(
                        "cannot compare {} and {}",
                        left.static_type(),
                        right.static_type()
                    ))),
                },
                _ => Err(VmError::native(format!(
                    "expected 2 arguments, got {}",
                    args.len()
                ))),
            })),
        }));

        env.declare_global_fn(Rc::new(NativeFunction {
            name: ">=<".to_string(),
            documentation: Some("Reverse three-way comparison. Returns 1 if left < right, 0 if equal, -1 if left > right.".to_string()),
            static_type: StaticType::Function {
                parameters: Some(vec![StaticType::Any, StaticType::Any]),
                return_type: Box::new(StaticType::Int),
            },
            func: NativeFunc::Simple(Box::new(|args| match args {
                [left, right] => match left.partial_cmp(right) {
                    Some(Ordering::Equal) => Ok(Value::Int(0)),
                    Some(Ordering::Less) => Ok(Value::Int(1)),
                    Some(Ordering::Greater) => Ok(Value::Int(-1)),
                    None => Err(VmError::native(format!(
                        "cannot compare {} and {}",
                        left.static_type(),
                        right.static_type()
                    ))),
                },
                _ => Err(VmError::native(format!(
                    "expected 2 arguments, got {}",
                    args.len()
                ))),
            })),
        }));

        macro_rules! impl_bitop {
            ($operator:literal,$operation:expr,$docs_bool:literal,$docs_int:literal) => {
                env.declare_global_fn(Rc::new(NativeFunction {
                    name: $operator.to_string(),
                    documentation: Some($docs_bool.to_string()),
                    static_type: StaticType::Function {
                        parameters: Some(vec![StaticType::Bool, StaticType::Bool]),
                        return_type: Box::new(StaticType::Bool),
                    },
                    func: NativeFunc::Simple(Box::new(|args| match args {
                        [Value::Bool(l), Value::Bool(r)] => Ok(Value::Bool($operation(*l, *r))),
                        _ => Err(VmError::native(format!(
                            "expected 2 bool arguments, got {}",
                            args.len()
                        ))),
                    })),
                }));
                env.declare_global_fn(Rc::new(NativeFunction {
                    name: $operator.to_string(),
                    documentation: Some($docs_int.to_string()),
                    static_type: StaticType::Function {
                        parameters: Some(vec![StaticType::Int, StaticType::Int]),
                        return_type: Box::new(StaticType::Int),
                    },
                    func: NativeFunc::Simple(Box::new(|args| match args {
                        [left, right] => {
                            let l = left.to_int().ok_or_else(|| {
                                VmError::native(format!("expected int, got {}", left.static_type()))
                            })?;
                            let r = right.to_int().ok_or_else(|| {
                                VmError::native(format!(
                                    "expected int, got {}",
                                    right.static_type()
                                ))
                            })?;
                            Ok(Value::from_int($operation(l, r)))
                        }
                        _ => Err(VmError::native(format!(
                            "expected 2 arguments, got {}",
                            args.len()
                        ))),
                    })),
                }));
            };
        }

        impl_bitop!(
            "&",
            std::ops::BitAnd::bitand,
            "Logical AND of two booleans.",
            "Bitwise AND of two integers."
        );
        impl_bitop!(
            "|",
            std::ops::BitOr::bitor,
            "Logical OR of two booleans.",
            "Bitwise OR of two integers."
        );
        impl_bitop!(
            "~",
            std::ops::BitXor::bitxor,
            "Logical XOR of two booleans.",
            "Bitwise XOR of two integers."
        );

        env.declare_global_fn(Rc::new(NativeFunction {
            name: "~".to_string(),
            documentation: Some("Bitwise NOT of a number.".to_string()),
            static_type: StaticType::Function {
                parameters: Some(vec![StaticType::Number]),
                return_type: Box::new(StaticType::Number),
            },
            func: NativeFunc::Simple(Box::new(|args| match args {
                [v] => v
                    .to_number()
                    .map(Not::not)
                    .map(Value::from_number)
                    .ok_or_else(|| {
                        VmError::native(format!("expected number, got {}", v.static_type()))
                    }),
                _ => Err(VmError::native(format!(
                    "expected 1 argument, got {}",
                    args.len()
                ))),
            })),
        }));

        for ident in ["!", "not"] {
            env.declare_global_fn(Rc::new(NativeFunction {
                name: ident.to_string(),
                documentation: Some(
                    "Logical negation. Returns the opposite boolean value.".to_string(),
                ),
                static_type: StaticType::Function {
                    parameters: Some(vec![StaticType::Bool]),
                    return_type: Box::new(StaticType::Bool),
                },
                func: NativeFunc::Simple(Box::new(|args| match args {
                    [Value::Bool(b)] => Ok(Value::Bool(b.not())),
                    _ => Err(VmError::native(format!(
                        "expected 1 bool argument, got {}",
                        args.len()
                    ))),
                })),
            }));
        }

        env.declare_global_fn(Rc::new(NativeFunction {
            name: ">>".to_string(),
            documentation: Some("Right bit shift.".to_string()),
            static_type: StaticType::Function {
                parameters: Some(vec![StaticType::Int, StaticType::Int]),
                return_type: Box::new(StaticType::Int),
            },
            func: NativeFunc::Simple(Box::new(|args| match args {
                [left, right] => {
                    let l = left.to_int().ok_or_else(|| {
                        VmError::native(format!("expected int, got {}", left.static_type()))
                    })?;
                    let r = right.to_int().ok_or_else(|| {
                        VmError::native(format!("expected int, got {}", right.static_type()))
                    })?;
                    l.checked_shr(r).map(Value::from_int).ok_or_else(|| {
                        VmError::native("cannot apply >> operator to operands".to_string())
                    })
                }
                _ => Err(VmError::native(format!(
                    "expected 2 arguments, got {}",
                    args.len()
                ))),
            })),
        }));

        env.declare_global_fn(Rc::new(NativeFunction {
            name: "<<".to_string(),
            documentation: Some("Left bit shift.".to_string()),
            static_type: StaticType::Function {
                parameters: Some(vec![StaticType::Int, StaticType::Int]),
                return_type: Box::new(StaticType::Int),
            },
            func: NativeFunc::Simple(Box::new(|args| match args {
                [left, right] => {
                    let l = left.to_int().ok_or_else(|| {
                        VmError::native(format!("expected int, got {}", left.static_type()))
                    })?;
                    let r = right.to_int().ok_or_else(|| {
                        VmError::native(format!("expected int, got {}", right.static_type()))
                    })?;
                    l.checked_shl(r).map(Value::from_int).ok_or_else(|| {
                        VmError::native("cannot apply << operator to operands".to_string())
                    })
                }
                _ => Err(VmError::native(format!(
                    "expected 2 arguments, got {}",
                    args.len()
                ))),
            })),
        }));

        macro_rules! delegate_to_f64 {
            ($method:ident,$docs:literal) => {
                env.declare_global_fn(Rc::new(NativeFunction {
                    name: stringify!($method).to_string(),
                    documentation: Some($docs.to_string()),
                    static_type: StaticType::Function {
                        parameters: Some(vec![StaticType::Number]),
                        return_type: Box::new(StaticType::Number),
                    },
                    func: NativeFunc::Simple(Box::new(|args| match args {
                        [v] => v
                            .to_number()
                            .map(|num| match num {
                                Number::Int(i) => Number::Float(f64::from(i).$method()),
                                Number::Float(f) => Number::Float(f.$method()),
                                Number::Rational(r) => {
                                    Number::Float(r.to_f64().unwrap_or(f64::NAN).$method())
                                }
                                Number::Complex(c) => Number::Complex(c.$method()),
                            })
                            .map(Value::from_number)
                            .ok_or_else(|| {
                                VmError::native(format!("expected number, got {}", v.static_type()))
                            }),
                        _ => Err(VmError::native(format!(
                            "expected 1 argument, got {}",
                            args.len()
                        ))),
                    })),
                }));
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
            "Computes the arctangent of a number. Return value is in radians in the range [-pi/2, pi/2]."
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
