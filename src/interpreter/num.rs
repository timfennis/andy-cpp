use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::num::TryFromIntError;
use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

use num::bigint::TryFromBigIntError;
use num::complex::{Complex64, ComplexFloat};
use num::{BigInt, BigRational, Complex, FromPrimitive, Signed, ToPrimitive};
use ordered_float::OrderedFloat;

use crate::interpreter::evaluate::EvaluationError;
use crate::interpreter::int::Int;
use crate::lexer::Location;

#[derive(Debug, Clone)]
pub enum Number {
    Int(Int),
    Float(f64),
    Rational(Box<BigRational>),
    Complex(Complex64),
}

pub enum RealNumber<'a> {
    Int(&'a Int),
    Float(OrderedFloat<f64>),
}

impl From<Int> for Number {
    fn from(value: Int) -> Self {
        Self::Int(value)
    }
}

impl From<i32> for Number {
    fn from(value: i32) -> Self {
        Self::Int(Int::from(value))
    }
}

impl From<f64> for Number {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}

impl From<BigRational> for Number {
    fn from(value: BigRational) -> Self {
        Self::Rational(Box::new(value))
    }
}

impl From<Complex64> for Number {
    fn from(value: Complex64) -> Self {
        Self::Complex(value)
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.to_reals().partial_cmp(&other.to_reals())
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        // NOTE: Noulith has a flexible implementation where 123 = 123.0 but implementing that for Number makes it impossible to store these values in a HashSet
        // self.to_reals().eq(&other.to_reals())
        match (self, other) {
            (Number::Int(left), Number::Int(right)) => left.eq(right),
            (Number::Float(left), Number::Float(right)) => {
                OrderedFloat(*left).eq(&OrderedFloat(*right))
            }
            (Number::Rational(left), Number::Rational(right)) => left.eq(right),
            (Number::Complex(left), Number::Complex(right)) => left.eq(right),
            _ => false,
        }
    }
}

impl Hash for Number {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Number::Int(i) => {
                state.write_u8(1);
                i.hash(state);
            }
            Number::Float(f) => {
                state.write_u8(2);
                OrderedFloat(*f).hash(state);
            }
            Number::Rational(r) => {
                state.write_u8(3);
                r.hash(state);
            }
            Number::Complex(c) => {
                state.write_u8(4);
                OrderedFloat(c.re).hash(state);
                OrderedFloat(c.im).hash(state);
            }
        }
    }
}

impl<'a> PartialEq for RealNumber<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => left.eq(right),
            (Self::Float(left), Self::Float(right)) => left.eq(right),
            (Self::Int(left), Self::Float(right)) => {
                compare_int_to_float(left, *right) == Some(Ordering::Equal)
            }
            (Self::Float(left), Self::Int(right)) => {
                compare_int_to_float(right, *left) == Some(Ordering::Equal)
            }
        }
    }
}

fn compare_int_to_float(a: &Int, b: OrderedFloat<f64>) -> Option<Ordering> {
    if b.is_infinite() {
        if b.is_sign_positive() {
            Some(Ordering::Less)
        } else {
            Some(Ordering::Greater)
        }
    } else if b.is_nan() {
        Some(OrderedFloat(f64::from(a)).cmp(&b))
    } else {
        let x = BigInt::from_f64(b.trunc()).expect("b can't be NaN");
        a.to_bigint().partial_cmp(&x)
    }
}

impl<'a> PartialOrd for RealNumber<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (RealNumber::Int(a), RealNumber::Int(b)) => Some(a.cmp(b)),
            (RealNumber::Int(a), RealNumber::Float(b)) => compare_int_to_float(a, *b),
            (RealNumber::Float(a), RealNumber::Int(b)) => {
                compare_int_to_float(b, *a).map(Ordering::reverse)
            }
            (RealNumber::Float(a), RealNumber::Float(b)) => {
                Some(OrderedFloat(*a).cmp(&OrderedFloat(*b)))
            }
        }
    }
}

impl Neg for Number {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Int(i) => i.neg().into(),
            Self::Float(f) => f.neg().into(),
            Self::Rational(r) => r.neg().into(),
            Self::Complex(c) => c.neg().into(),
        }
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // Operands are the same
            (Self::Int(i1), Self::Int(i2)) => Self::Int(i1 + i2),
            (Self::Float(f1), Self::Float(f2)) => Self::Float(f1.add(f2)),
            (Self::Rational(r1), Self::Rational(r2)) => {
                Self::Rational(Box::new(Add::add(*r1, *r2)))
            }
            (Self::Complex(c1), Self::Complex(c2)) => Self::Complex(c1 + c2),

            // Float vs other
            (Self::Float(p1), Self::Int(p2)) => Self::Float(p1.add(f64::from(p2))),
            (Self::Float(p1), Self::Rational(p2)) => Self::Float(p1.add(rational_to_float(&p2))),
            (Self::Float(p1), Self::Complex(p2)) => Self::Complex(Complex::from(p1).add(p2)),

            // Int vs other
            (Self::Int(p1), Self::Float(p2)) => Self::Float(f64::from(p1).add(p2)),
            (Self::Int(p1), Self::Rational(p2)) => {
                Self::Rational(Box::new(Add::add(BigRational::from(p1), *p2)))
            }
            (Self::Int(p1), Self::Complex(p2)) => Self::Complex(Complex::from(p1).add(p2)),

            // Rational vs other
            (Self::Rational(p1), Self::Int(p2)) => {
                Self::Rational(Box::new(Add::add(*p1, BigRational::from(p2))))
            }
            (Self::Rational(p1), Self::Float(p2)) => Self::Float(rational_to_float(&p1).add(p2)),
            (Self::Rational(p1), Self::Complex(p2)) => {
                Self::Complex(Complex::from(p1.to_f64().unwrap_or(f64::NAN)).add(p2))
                //TODO: Check if this is logical
            }

            // Complex vs other
            (Self::Complex(p1), Self::Int(p2)) => Self::Complex(Complex::from(p2).add(p1)),
            (Self::Complex(p1), Self::Rational(p2)) => {
                Self::Complex(Complex::from(p2.to_f64().unwrap_or(f64::NAN)).add(p1))
            }
            (Self::Complex(p1), Self::Float(p2)) => Self::Complex(Complex::from(p2).add(p1)),
        }
    }
}

impl Sub for Number {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // Operands are the same
            (Self::Int(i1), Self::Int(i2)) => Self::Int(i1 - i2),
            (Self::Float(f1), Self::Float(f2)) => Self::Float(f1.sub(f2)),
            (Self::Rational(r1), Self::Rational(r2)) => {
                Self::Rational(Box::new(Sub::sub(*r1, *r2)))
            }
            (Self::Complex(c1), Self::Complex(c2)) => Self::Complex(c1 - c2),

            // Float vs other
            (Self::Float(p1), Self::Int(p2)) => Self::Float(p1.sub(f64::from(p2))),
            (Self::Float(p1), Self::Rational(p2)) => Self::Float(p1.sub(rational_to_float(&p2))),
            (Self::Float(p1), Self::Complex(p2)) => Self::Complex(Complex::from(p1).sub(p2)),

            // Int vs other
            (Self::Int(p1), Self::Float(p2)) => Self::Float(f64::from(p1).sub(p2)),
            (Self::Int(p1), Self::Rational(p2)) => {
                Self::Rational(Box::new(Sub::sub(BigRational::from(p1), *p2)))
            }
            (Self::Int(p1), Self::Complex(p2)) => Self::Complex(Complex::from(p1).sub(p2)),

            // Rational vs Other
            (Self::Rational(p1), Self::Int(p2)) => {
                Self::Rational(Box::new(Sub::sub(*p1, BigRational::from(p2))))
            }
            (Self::Rational(p1), Self::Float(p2)) => Self::Float(rational_to_float(&p1).sub(p2)),
            (Self::Rational(p1), Self::Complex(p2)) => {
                Self::Complex(Complex::from(p1.to_f64().unwrap_or(f64::NAN)).sub(p2))
                //TODO: Check if this is logical
            }

            // Complex vs Other
            (Self::Complex(p1), Self::Int(p2)) => Self::Complex(Complex::from(p2).sub(p1)),
            (Self::Complex(p1), Self::Rational(p2)) => {
                Self::Complex(Complex::from(p2.to_f64().unwrap_or(f64::NAN)).sub(p1))
            }
            (Self::Complex(p1), Self::Float(p2)) => Self::Complex(Complex::from(p2).sub(p1)),
        }
    }
}

impl Div for Number {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Int(ref p1), Self::Int(p2)) => {
                if p1.is_zero() && p2.is_zero() {
                    return Self::Float(f64::NAN);
                } else if p2.is_zero() {
                    return Self::Float(f64::INFINITY);
                } else if p1.rem(&p2) == 0i32.into() {
                    return Self::Int(p1.div(&p2));
                }
                BigRational::new(p1.into(), p2.into()).into()
            }
            (Self::Float(p1), Self::Float(p2)) => Self::Float(p1 / p2),
            (Self::Rational(p1), Self::Rational(p2)) => {
                Self::Rational(Box::new(Div::div(*p1, *p2)))
            }
            (Self::Complex(p1), Self::Complex(p2)) => Self::Complex(p1 / p2),

            // Float vs other
            (Self::Float(p1), Self::Int(p2)) => Self::Float(p1.div(f64::from(p2))),
            (Self::Float(p1), Self::Rational(p2)) => Self::Float(p1.div(rational_to_float(&p2))),
            (Self::Float(p1), Self::Complex(p2)) => Self::Complex(Complex::from(p1).div(p2)),

            // Int vs other
            (Self::Int(p1), Self::Rational(p2)) => {
                Self::Rational(Box::new(Div::div(BigRational::from(p1), *p2)))
            }
            (Self::Int(p1), Self::Float(p2)) => Self::Float(f64::from(p1).div(p2)),
            (Self::Int(p1), Self::Complex(p2)) => Self::Complex(Complex::from(p1).div(p2)),

            // Rational vs other
            (Self::Rational(p1), Self::Int(p2)) => {
                Self::Rational(Box::new(Div::div(*p1, BigRational::from(p2))))
            }
            (Self::Rational(p1), Self::Float(p2)) => Self::Float(rational_to_float(&p1).div(p2)),
            (Self::Rational(p1), Self::Complex(p2)) => {
                Self::Complex(Complex::from(p1.to_f64().unwrap_or(f64::NAN)).div(p2))
                //TODO: Check if this is logical
            }

            // Complex vs Other
            (Self::Complex(p1), Self::Int(p2)) => Self::Complex(Complex::from(p2).div(p1)),
            (Self::Complex(p1), Self::Rational(p2)) => {
                Self::Complex(Complex::from(p2.to_f64().unwrap_or(f64::NAN)).div(p1))
            }
            (Self::Complex(p1), Self::Float(p2)) => Self::Complex(Complex::from(p2).div(p1)),
        }
    }
}

impl Mul for Number {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Int(p1), Self::Int(p2)) => Self::Int(p1 * p2),
            (Self::Float(p1), Self::Float(p2)) => Self::Float(p1 * p2),
            (Self::Rational(p1), Self::Rational(p2)) => {
                Self::Rational(Box::new(Mul::mul(*p1, *p2)))
            }
            (Self::Complex(p1), Self::Complex(p2)) => Self::Complex(p1 * p2),

            // Float vs other
            (Self::Float(p1), Self::Int(p2)) => Self::Float(p1.mul(f64::from(p2))),
            (Self::Float(p1), Self::Rational(p2)) => Self::Float(p1.mul(rational_to_float(&p2))),
            (Self::Float(p1), Self::Complex(p2)) => Self::Complex(Complex::from(p1).mul(p2)),

            // Int vs other
            (Self::Int(p1), Self::Rational(p2)) => {
                Self::Rational(Box::new(Mul::mul(BigRational::from(p1), *p2)))
            }
            (Self::Int(p1), Self::Float(p2)) => Self::Float(f64::from(p1).mul(p2)),
            (Self::Int(p1), Self::Complex(p2)) => Self::Complex(Complex::from(p1).mul(p2)),

            // Rational vs other
            (Self::Rational(p1), Self::Float(p2)) => Self::Float(rational_to_float(&p1).mul(p2)),
            (Self::Rational(p1), Self::Int(p2)) => {
                Self::Rational(Box::new(Mul::mul(*p1, BigRational::from(p2))))
            }
            (Self::Rational(p1), Self::Complex(p2)) => {
                Self::Complex(Complex::from(p1.to_f64().unwrap_or(f64::NAN)).div(p2))
            }

            // Complex vs Other
            (Self::Complex(p1), Self::Int(p2)) => Self::Complex(Complex::from(p2).mul(p1)),
            (Self::Complex(p1), Self::Rational(p2)) => {
                Self::Complex(Complex::from(p2.to_f64().unwrap_or(f64::NAN)).mul(p1))
            }
            (Self::Complex(p1), Self::Float(p2)) => Self::Complex(Complex::from(p2).mul(p1)),
        }
    }
}

impl Rem for Number {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Int(p1), Self::Int(p2)) => Self::Int(p1.rem(p2)),
            (Self::Float(p1), Self::Float(p2)) => Self::Float(p1.rem(p2)),
            (Self::Rational(p1), Self::Rational(p2)) => {
                Self::Rational(Box::new(Rem::rem(*p1, *p2)))
            }
            (Self::Complex(p1), Self::Complex(p2)) => Self::Complex(p1.rem(p2)),

            // Rational vs Int
            (Self::Rational(p1), Self::Int(p2)) => {
                Self::Rational(Box::new(Rem::rem(*p1, BigRational::from(p2))))
            }
            (Self::Int(p1), Self::Rational(p2)) => {
                Self::Rational(Box::new(Rem::rem(BigRational::from(p1), *p2)))
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
    fn type_name(&self) -> String {
        match self {
            Self::Int(_) => "int".to_string(),
            Self::Float(_) => "float".to_string(),
            Self::Rational(_) => "rational".to_string(),
            Self::Complex(_) => "complex".to_string(),
        }
    }

    pub fn checked_rem_euclid(self, rhs: Self) -> Result<Self, EvaluationError> {
        match (self, rhs) {
            (Self::Int(p1), Self::Int(p2)) => Ok(Self::Int(p1.checked_rem_euclid(&p2).ok_or({
                //TODO move EvaluationError to caller so we can add location
                EvaluationError::type_error(
                    "cannot divide by zero",
                    Location { line: 0, column: 0 },
                    Location { line: 0, column: 0 },
                )
            })?)),
            (Self::Float(p1), Self::Float(p2)) => Ok(Self::Float(p1.rem_euclid(p2))),
            // (Number::Rational(p1), Number::Rational(p2)) => Ok(Number::Rational(p1.__(p2))),
            // TODO: implement other cases
            (a, b) => panic!(
                "remainder between {} and {} is not implemented",
                a.type_name(),
                b.type_name()
            ),
        }
    }
    pub fn checked_pow(self, rhs: Self) -> Result<Self, EvaluationError> {
        Ok(match (self, rhs) {
            // Int vs others
            (Self::Int(p1), Self::Int(p2)) => {
                if p2.is_negative() {
                    let p2 = i32::try_from(p2)?;
                    Self::Rational(Box::new(BigRational::from(p1).pow(p2)))
                } else {
                    //TODO: this error is wrong, if checked_pow fails the number is too large and we should use BigInt instead
                    Self::Int(p1.checked_pow(&p2).ok_or(EvaluationError::type_error(
                        "integer overflow",
                        Location { line: 0, column: 0 },
                        Location { line: 0, column: 0 },
                    ))?)
                }
            }
            (Self::Int(p1), Self::Float(p2)) => Self::Float(f64::from(p1).powf(p2)),
            (Self::Int(p1), Self::Complex(p2)) => {
                Self::Complex(Complex::from(f64::from(p1)).powc(p2))
            }
            (Self::Int(p1), Self::Rational(p2)) => {
                if p2.is_integer() {
                    if let Some(n) = p1.checked_pow(&Int::BigInt(p2.to_integer())) {
                        return Ok(Self::Int(n));
                    }
                }

                Self::Float(f64::from(p1).powf(rational_to_float(&p2)))
            }

            // Rational vs Others
            (Self::Rational(p1), Self::Int(p2)) => {
                Self::Rational(Box::new(p1.pow(i32::try_from(p2)?)))
            }
            (Self::Rational(p1), Self::Rational(p2)) => {
                if p2.is_integer() {
                    if let Some(p2) = p2.to_i32() {
                        return Ok(Self::Rational(Box::new(p1.pow(p2))));
                    }
                }

                return Err(EvaluationError::type_error(
                    "Cannot raise a rational to the power of another rational, try converting the operands to floats",
                    Location { line: 0, column: 0 },
                    Location { line: 0, column: 0 },
                ));
            }
            (Self::Rational(p1), Self::Float(p2)) => Self::Float(rational_to_float(&p1).powf(p2)),
            (Self::Rational(p1), Self::Complex(p2)) => {
                Self::Complex(rational_to_complex(&p1).powc(p2))
            }

            // Float vs others
            (Self::Float(p1), Self::Float(p2)) => Self::Float(p1.powf(p2)),
            (Self::Float(p1), Self::Complex(p2)) => Self::Complex(Complex::from(p1).powc(p2)),
            (Self::Float(p1), Self::Int(p2)) => Self::Float(p1.powf(f64::from(p2))),
            (Self::Float(p1), Self::Rational(p2)) => Self::Float(p1.powf(rational_to_float(&p2))),

            // Complex vs others
            (Self::Complex(p1), Self::Complex(p2)) => Self::Complex(p1.powc(p2)),
            (Self::Complex(p1), Self::Float(p2)) => Self::Complex(p1.powc(Complex::from(p2))),
            (Self::Complex(p1), Self::Int(p2)) => {
                Self::Complex(p1.powc(Complex::from(f64::from(p2))))
            }
            (Self::Complex(p1), Self::Rational(p2)) => {
                // TODO: add more???
                Self::Complex(p1.powc(rational_to_complex(&p2)))
            }
        })
    }

    /// # Errors
    /// Returns an `EvaluationError` (for now) if you try to convert Inf or NaN to an int
    pub fn to_int_lossy(&self) -> Result<Self, EvaluationError> {
        let n = match self {
            Number::Int(i) => Self::Int(i.clone()),
            Number::Float(f) => {
                if let Some(bi) = BigInt::from_f64(*f) {
                    Number::Int(Int::BigInt(bi).simplify())
                } else {
                    // TODO FIX line 0 column 0
                    return Err(EvaluationError::type_error(
                        &format!("cannot convert {f} to int"),
                        Location { line: 0, column: 0 },
                        Location { line: 0, column: 0 },
                    ));
                }
            }
            Number::Rational(r) => Self::Int(Int::BigInt(r.to_integer()).simplify()),
            Number::Complex(c) => {
                return Err(EvaluationError::type_error(
                    &format!("cannot convert complex number {c} to int"),
                    Location { line: 0, column: 0 },
                    Location { line: 0, column: 0 },
                ));
            }
        };
        Ok(n)
    }

    /// Converts this number into a real (complex) number with the imaginary part set to 0.0
    /// which makes it esy to do comparison on all numbers (and possibly other things)
    ///
    /// TODO: in the future we might want to create a new Real type which is the sum of Int and Float
    ///       in order to better handle `BigInt`s which now lose tons of precision and will yield incorrect results
    #[must_use]
    pub fn to_reals(&self) -> (RealNumber, RealNumber) {
        match self {
            Number::Int(i) => (RealNumber::Int(i), RealNumber::Float(OrderedFloat(0.0))),
            Number::Float(f) => (
                RealNumber::Float(OrderedFloat(*f)),
                RealNumber::Float(OrderedFloat(0.0)),
            ),
            Number::Rational(r) => (
                RealNumber::Float(OrderedFloat(rational_to_float(r))),
                RealNumber::Float(OrderedFloat(0.0)),
            ),
            Number::Complex(c) => (
                RealNumber::Float(OrderedFloat(c.re)),
                RealNumber::Float(OrderedFloat(c.im)),
            ),
        }
    }

    #[must_use]
    pub fn abs(&self) -> Number {
        match self {
            Number::Int(i) => Number::Int(i.abs()),
            Number::Float(f) => Number::Float(f.abs()),
            Number::Rational(r) => Number::Rational(Box::new(r.abs())),
            Number::Complex(c) => Number::Float(c.abs()),
        }
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

#[derive(thiserror::Error, Debug)]
pub enum NumberToUsizeError {
    #[error("cannot convert from {0} to usize")]
    UnsupportedVariant(String),
    #[error("failed to convert from int to because of {0}")]
    FromIntError(#[from] TryFromIntError),
    #[error("failed to convert from bigint to number because of {0}")]
    FromBigIntError(#[from] TryFromBigIntError<BigInt>),
}

impl TryFrom<Number> for usize {
    type Error = NumberToUsizeError;

    fn try_from(value: Number) -> Result<Self, Self::Error> {
        match value {
            Number::Int(Int::Int64(i)) => Ok(usize::try_from(i)?),
            Number::Int(Int::BigInt(b)) => Ok(usize::try_from(b)?),
            n => Err(NumberToUsizeError::UnsupportedVariant(n.type_name())),
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{i}"),
            Self::Float(ff) => {
                let mut buffer = ryu::Buffer::new();
                write!(f, "{}", buffer.format(*ff))
            }
            Self::Rational(r) => write!(f, "{r}"),
            Self::Complex(r) => write!(f, "{r}"),
        }
    }
}

fn rational_to_float(r: &BigRational) -> f64 {
    r.to_f64().unwrap_or(f64::NAN)
}

fn rational_to_complex(r: &BigRational) -> Complex<f64> {
    Complex::from(r.to_f64().unwrap_or(f64::NAN))
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum NumberType {
    Int,
    Float,
    Rational,
    Complex,
}

impl From<&Number> for NumberType {
    fn from(value: &Number) -> Self {
        match value {
            Number::Int(_) => Self::Int,
            Number::Float(_) => Self::Float,
            Number::Rational(_) => Self::Rational,
            Number::Complex(_) => Self::Complex,
        }
    }
}

impl fmt::Display for NumberType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::Rational => write!(f, "rational"),
            Self::Complex => write!(f, "complex"),
        }
    }
}
