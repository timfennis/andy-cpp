use crate::interpreter::int::Int;
use crate::interpreter::{EvaluationError, Function, Value};

use crate::ast::Operator;
use num::complex::Complex64;
use num::{BigRational, Complex, ToPrimitive};
use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Int(Int),
    Float(f64),
    Rational(BigRational),
    Complex(Complex64),
}

impl Number {
    fn type_name(&self) -> String {
        match self {
            Number::Int(_) => "int".to_string(),
            Number::Float(_) => "float".to_string(),
            Number::Rational(_) => "rational".to_string(),
            Number::Complex(_) => "complex".to_string(),
        }
    }
}

impl From<i32> for Number {
    fn from(value: i32) -> Self {
        Number::Int(value.into())
    }
}

impl From<f64> for Number {
    fn from(value: f64) -> Self {
        Number::Float(value)
    }
}

impl From<BigRational> for Number {
    fn from(value: BigRational) -> Self {
        Number::Rational(value)
    }
}

impl From<Complex64> for Number {
    fn from(value: Complex64) -> Self {
        Number::Complex(value)
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Number::Int(n1), Number::Int(n2)) => n1.partial_cmp(n2),
            (Number::Float(n1), Number::Float(n2)) => n1.partial_cmp(n2),
            (Number::Rational(n1), Number::Rational(n2)) => n1.partial_cmp(n2),
            (Number::Complex(_), Number::Complex(_)) => None,
            _ => panic!("Dont compare different numbers."),
        }
    }
}

impl Neg for Number {
    type Output = Number;

    fn neg(self) -> Self::Output {
        match self {
            Number::Int(i) => Number::Int(i.neg()),
            Number::Float(f) => Number::Float(f.neg()),
            Number::Rational(r) => Number::Rational(r.neg()),
            Number::Complex(c) => Number::Complex(c.neg()),
        }
    }
}

impl Add for Number {
    type Output = Number;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // Operands are the same
            (Number::Int(i1), Number::Int(i2)) => Number::Int(i1 + i2),
            (Number::Float(f1), Number::Float(f2)) => Number::Float(f1.add(f2)),
            (Number::Rational(r1), Number::Rational(r2)) => Number::Rational(r1 + r2),
            (Number::Complex(c1), Number::Complex(c2)) => Number::Complex(c1 + c2),

            // Float vs other
            (Number::Float(p1), Number::Int(p2)) => Number::Float(p1.add(f64::from(p2))),
            (Number::Float(p1), Number::Rational(p2)) => {
                Number::Float(p1.add(rational_to_float(&p2)))
            }
            (Number::Float(p1), Number::Complex(p2)) => Number::Complex(Complex::from(p1).add(p2)),

            // Int vs other
            (Number::Int(p1), Number::Float(p2)) => Number::Float(f64::from(p1).add(p2)),
            (Number::Int(p1), Number::Rational(p2)) => {
                Number::Rational(BigRational::from(p1).add(p2))
            }
            (Number::Int(p1), Number::Complex(p2)) => Number::Complex(Complex::from(p1).add(p2)),

            // Rational vs other
            (Number::Rational(p1), Number::Int(p2)) => {
                Number::Rational(p1.add(BigRational::from(p2)))
            }
            (Number::Rational(p1), Number::Float(p2)) => {
                Number::Float(rational_to_float(&p1).add(p2))
            }
            (Number::Rational(p1), Number::Complex(p2)) => {
                Number::Complex(Complex::from(p1.to_f64().unwrap_or(f64::NAN)).add(p2))
                //TODO: Check if this is logical
            }

            // Complex vs other
            (Number::Complex(p1), Number::Int(p2)) => Number::Complex(Complex::from(p2).add(p1)),
            (Number::Complex(p1), Number::Rational(p2)) => {
                Number::Complex(Complex::from(p2.to_f64().unwrap_or(f64::NAN)).add(p1))
            }
            (Number::Complex(p1), Number::Float(p2)) => Number::Complex(Complex::from(p2).add(p1)),
        }
    }
}

impl Sub for Number {
    type Output = Number;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // Operands are the same
            (Number::Int(i1), Number::Int(i2)) => Number::Int(i1 - i2),
            (Number::Float(f1), Number::Float(f2)) => Number::Float(f1.add(f2)),
            (Number::Rational(r1), Number::Rational(r2)) => Number::Rational(r1 - r2),
            (Number::Complex(c1), Number::Complex(c2)) => Number::Complex(c1 - c2),

            // Float vs other
            (Number::Float(p1), Number::Int(p2)) => Number::Float(p1.sub(f64::from(p2))),
            (Number::Float(p1), Number::Rational(p2)) => {
                Number::Float(p1.sub(rational_to_float(&p2)))
            }
            (Number::Float(p1), Number::Complex(p2)) => Number::Complex(Complex::from(p1).sub(p2)),

            // Int vs other
            (Number::Int(p1), Number::Float(p2)) => Number::Float(f64::from(p1).sub(p2)),
            (Number::Int(p1), Number::Rational(p2)) => {
                Number::Rational(BigRational::from(p1).sub(p2))
            }
            (Number::Int(p1), Number::Complex(p2)) => Number::Complex(Complex::from(p1).sub(p2)),

            // Rational vs Other
            (Number::Rational(p1), Number::Int(p2)) => {
                Number::Rational(p1.sub(BigRational::from(p2)))
            }
            (Number::Rational(p1), Number::Float(p2)) => {
                Number::Float(rational_to_float(&p1).sub(p2))
            }
            (Number::Rational(p1), Number::Complex(p2)) => {
                Number::Complex(Complex::from(p1.to_f64().unwrap_or(f64::NAN)).sub(p2))
                //TODO: Check if this is logical
            }

            // Complex vs Other
            (Number::Complex(p1), Number::Int(p2)) => Number::Complex(Complex::from(p2).sub(p1)),
            (Number::Complex(p1), Number::Rational(p2)) => {
                Number::Complex(Complex::from(p2.to_f64().unwrap_or(f64::NAN)).sub(p1))
            }
            (Number::Complex(p1), Number::Float(p2)) => Number::Complex(Complex::from(p2).sub(p1)),
        }
    }
}

impl Div for Number {
    type Output = Number;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(ref p1), Number::Int(p2)) => {
                if p2.is_zero() {
                    return Number::Float(f64::INFINITY);
                }
                if p1.rem(&p2) == 0i32.into() {
                    return Number::Int(p1.div(&p2));
                }
                Number::Rational(BigRational::new(p1.into(), p2.into()))
            }
            (Number::Float(p1), Number::Float(p2)) => Number::Float(p1 / p2),
            (Number::Rational(p1), Number::Rational(p2)) => Number::Rational(p1 / p2),
            (Number::Complex(p1), Number::Complex(p2)) => Number::Complex(p1 / p2),

            // Float vs other
            (Number::Float(p1), Number::Int(p2)) => Number::Float(p1.div(f64::from(p2))),
            (Number::Float(p1), Number::Rational(p2)) => {
                Number::Float(p1.div(rational_to_float(&p2)))
            }
            (Number::Float(p1), Number::Complex(p2)) => Number::Complex(Complex::from(p1).div(p2)),

            // Int vs other
            (Number::Int(p1), Number::Rational(p2)) => {
                Number::Rational(BigRational::from(p1).div(p2))
            }
            (Number::Int(p1), Number::Float(p2)) => Number::Float(f64::from(p1).div(p2)),
            (Number::Int(p1), Number::Complex(p2)) => Number::Complex(Complex::from(p1).div(p2)),

            // Rational vs other
            (Number::Rational(p1), Number::Int(p2)) => {
                Number::Rational(p1.div(BigRational::from(p2)))
            }
            (Number::Rational(p1), Number::Float(p2)) => {
                Number::Float(rational_to_float(&p1).div(p2))
            }
            (Number::Rational(p1), Number::Complex(p2)) => {
                Number::Complex(Complex::from(p1.to_f64().unwrap_or(f64::NAN)).div(p2))
                //TODO: Check if this is logical
            }

            // Complex vs Other
            (Number::Complex(p1), Number::Int(p2)) => Number::Complex(Complex::from(p2).div(p1)),
            (Number::Complex(p1), Number::Rational(p2)) => {
                Number::Complex(Complex::from(p2.to_f64().unwrap_or(f64::NAN)).div(p1))
            }
            (Number::Complex(p1), Number::Float(p2)) => Number::Complex(Complex::from(p2).div(p1)),
        }
    }
}

impl Mul for Number {
    type Output = Number;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(p1), Number::Int(p2)) => Number::Int(p1 * p2),
            (Number::Float(p1), Number::Float(p2)) => Number::Float(p1 * p2),
            (Number::Rational(p1), Number::Rational(p2)) => Number::Rational(p1 * p2),
            (Number::Complex(p1), Number::Complex(p2)) => Number::Complex(p1 * p2),

            // Float vs other
            (Number::Float(p1), Number::Int(p2)) => Number::Float(p1.mul(f64::from(p2))),
            (Number::Float(p1), Number::Rational(p2)) => {
                Number::Float(p1.mul(rational_to_float(&p2)))
            }
            (Number::Float(p1), Number::Complex(p2)) => Number::Complex(Complex::from(p1).mul(p2)),

            // Int vs other
            (Number::Int(p1), Number::Rational(p2)) => {
                Number::Rational(BigRational::from(p1).mul(p2))
            }
            (Number::Int(p1), Number::Float(p2)) => Number::Float(f64::from(p1).mul(p2)),
            (Number::Int(p1), Number::Complex(p2)) => Number::Complex(Complex::from(p1).mul(p2)),

            // Rational vs other
            (Number::Rational(p1), Number::Float(p2)) => {
                Number::Float(rational_to_float(&p1).mul(p2))
            }
            (Number::Rational(p1), Number::Int(p2)) => {
                Number::Rational(p1.mul(BigRational::from(p2)))
            }
            (Number::Rational(p1), Number::Complex(p2)) => {
                Number::Complex(Complex::from(p1.to_f64().unwrap_or(f64::NAN)).div(p2))
                //TODO: Check if this is logical
            }

            // Complex vs Other
            (Number::Complex(p1), Number::Int(p2)) => Number::Complex(Complex::from(p2).mul(p1)),
            (Number::Complex(p1), Number::Rational(p2)) => {
                Number::Complex(Complex::from(p2.to_f64().unwrap_or(f64::NAN)).mul(p1))
            }
            (Number::Complex(p1), Number::Float(p2)) => Number::Complex(Complex::from(p2).mul(p1)),
        }
    }
}

impl Rem for Number {
    type Output = Number;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(p1), Number::Int(p2)) => Number::Int(p1.rem(p2)),
            (Number::Float(p1), Number::Float(p2)) => Number::Float(p1.rem(p2)),
            (Number::Rational(p1), Number::Rational(p2)) => Number::Rational(p1.rem(p2)),
            (Number::Complex(p1), Number::Complex(p2)) => Number::Complex(p1.rem(p2)),

            // Rational vs Int
            (Number::Rational(p1), Number::Int(p2)) => {
                Number::Rational(p1.rem(BigRational::from(p2)))
            }
            (Number::Int(p1), Number::Rational(p2)) => {
                Number::Rational(BigRational::from(p1).rem(p2))
            }
            // TODO: implement other cases
            (a, b) => panic!(
                "remainder between {} and {} is not implemented",
                a.type_name(),
                b.type_name()
            ),
        }
    }
}

impl Number {
    /// Calculates the remainder of euclidean division.
    /// # Errors
    /// Returns an `EvaluationError` if the remainder of division is 0
    /// # Panics
    /// Panics if the evaluation between operands is not supported, this is a temporary condition
    pub fn checked_rem_euclid(self, rhs: Self) -> Result<Self, EvaluationError> {
        match (self, rhs) {
            (Number::Int(p1), Number::Int(p2)) => {
                Ok(Number::Int(p1.checked_rem_euclid(p2).ok_or({
                    EvaluationError::DivisionByZero {
                        operator: Operator::EuclideanModulo,
                    }
                })?))
            }
            (Number::Float(p1), Number::Float(p2)) => Ok(Number::Float(p1.rem_euclid(p2))),
            // (Number::Rational(p1), Number::Rational(p2)) => Ok(Number::Rational(p1.__(p2))),
            // TODO: implement other cases
            (a, b) => panic!(
                "remainder between {} and {} is not implemented",
                a.type_name(),
                b.type_name()
            ),
        }
    }

    /// Performs exponentiation
    /// # Errors
    /// Returns an `EvaluationError::IntegerOverflow` error if the right operand cannot be converted into a 32 bit
    /// integer
    /// # Panics
    /// Panics if the operation between the two number types has not yet been implemented
    pub fn checked_pow(self, rhs: Self) -> Result<Self, EvaluationError> {
        Ok(match (self, rhs) {
            // Int vs others
            (Number::Int(p1), Number::Int(p2)) => {
                if p2.is_negative() {
                    let p2 = i32::try_from(p2)?;
                    Number::Rational(BigRational::from(p1).pow(p2))
                } else {
                    Number::Int(
                        p1.checked_pow(&p2)
                            .ok_or(EvaluationError::IntegerOverflow {
                                operator: Operator::Exponent,
                            })?,
                    )
                }
            }
            (Number::Int(p1), Number::Float(p2)) => Number::Float(f64::from(p1).powf(p2)),
            (Number::Int(p1), Number::Complex(p2)) => {
                Number::Complex(Complex::from(f64::from(p1)).powc(p2))
            }
            (Number::Int(p1), Number::Rational(p2)) => {
                if p2.is_integer() {
                    if let Some(n) = p1.checked_pow(&Int::BigInt(p2.to_integer())) {
                        return Ok(Number::Int(n));
                    }
                }

                Number::Float(f64::from(p1).powf(rational_to_float(&p2)))
            }

            // Rational vs Others
            (Number::Rational(p1), Number::Int(p2)) => Number::Rational(p1.pow(i32::try_from(p2)?)),
            (Number::Rational(p1), Number::Rational(p2)) => {
                if p2.is_integer() {
                    if let Some(p2) = p2.to_i32() {
                        return Ok(Number::Rational(p1.pow(p2)));
                    }
                }
                return Err(EvaluationError::TypeError {
                    message: "Cannot raise a rational to the power of another rational, try converting the operands to floats".to_string(),
                });
            }
            (Number::Rational(p1), Number::Float(p2)) => {
                Number::Float(rational_to_float(&p1).powf(p2))
            }
            (Number::Rational(p1), Number::Complex(p2)) => {
                Number::Complex(rational_to_complex(&p1).powc(p2))
            }

            // Float vs others
            (Number::Float(p1), Number::Float(p2)) => Number::Float(p1.powf(p2)),
            (Number::Float(p1), Number::Complex(p2)) => Number::Complex(Complex::from(p1).powc(p2)),
            (Number::Float(p1), Number::Int(p2)) => Number::Float(p1.powf(f64::from(p2))),
            (Number::Float(p1), Number::Rational(p2)) => {
                Number::Float(p1.powf(rational_to_float(&p2)))
            }

            (Number::Complex(p1), Number::Complex(p2)) => Number::Complex(p1.powc(p2)),
            (Number::Complex(p1), Number::Float(p2)) => Number::Complex(p1.powc(Complex::from(p2))),
            (Number::Complex(p1), Number::Int(p2)) => {
                Number::Complex(p1.powc(Complex::from(f64::from(p2))))
            }
            (Number::Complex(p1), Number::Rational(p2)) => {
                // TODO: add more???
                Number::Complex(p1.powc(rational_to_complex(&p2)))
            }
        })
    }
}

macro_rules! implement_rounding {
    ($method:ident) => {
        impl Number {
            #[must_use]
            pub fn $method(&self) -> Number {
                match self {
                    Number::Int(i) => Number::Int(i.clone()),
                    Number::Float(f) => {
                        let f = f.$method();
                        if let Some(i) = Int::from_f64(f) {
                            Number::Int(i)
                        } else {
                            Number::Float(f)
                        }
                    }
                    Number::Rational(r) => Number::Int(Int::BigInt(r.$method().to_integer())),
                    Number::Complex(c) => Complex::new(c.re.$method(), c.im.$method()).into(),
                }
            }
        }
    };
}

implement_rounding!(ceil);
implement_rounding!(floor);
implement_rounding!(round);

impl TryFrom<Number> for usize {
    type Error = ();

    fn try_from(value: Number) -> Result<Self, Self::Error> {
        match value {
            Number::Int(Int::Int64(i)) => i.try_into().map_err(|_| ()),
            Number::Int(Int::BigInt(b)) => b.try_into().map_err(|_| ()),
            _ => Err(()),
        }
    }
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Int(i) => write!(f, "{i}"),
            Number::Float(ff) if ff.fract() == 0.0 => write!(f, "{ff:.1}"),
            Number::Float(ff) => write!(f, "{ff}"),
            Number::Rational(r) => write!(f, "{r}"),
            Number::Complex(r) => write!(f, "{r}"),
        }
    }
}

fn rational_to_float(r: &BigRational) -> f64 {
    r.to_f64().unwrap_or(f64::NAN)
}
fn rational_to_complex(r: &BigRational) -> Complex<f64> {
    Complex::from(r.to_f64().unwrap_or(f64::NAN))
}

#[derive(Debug, Clone, Copy)]
pub enum NumberType {
    Int,
    Float,
    Rational,
    Complex,
}

impl From<Number> for NumberType {
    fn from(value: Number) -> Self {
        match value {
            Number::Int(_) => NumberType::Int,
            Number::Float(_) => NumberType::Float,
            Number::Rational(_) => NumberType::Rational,
            Number::Complex(_) => NumberType::Complex,
        }
    }
}

impl Display for NumberType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            NumberType::Int => write!(f, "int"),
            NumberType::Float => write!(f, "float"),
            NumberType::Rational => write!(f, "rational"),
            NumberType::Complex => write!(f, "complex"),
        }
    }
}

#[derive(Debug)]
pub struct SingleNumberFunction {
    // pub name: &'static str,
    pub function: fn(number: Number) -> Number,
}

impl Function for SingleNumberFunction {
    fn call(&self, args: &[Value]) -> Value {
        if args.len() == 1 {
            let arg = args.get(0).expect("guaranteed to be 1");

            if let Value::Number(num) = arg {
                // TODO: is this clone wanted and cheap? Probably not
                return Value::Number((self.function)(num.clone()));
            }
        }

        todo!("what to do if we can't apply functions");
    }
}
