use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::num::TryFromIntError;
use std::ops::{Add, Div, Mul, Neg, Not, Rem, Sub};

use num::bigint::TryFromBigIntError;
use num::complex::{Complex64, ComplexFloat};
use num::{BigInt, BigRational, Complex, FromPrimitive, Signed, ToPrimitive, Zero};
use ordered_float::OrderedFloat;

use crate::interpreter::evaluate::EvaluationError;
use crate::interpreter::int::Int;
use crate::lexer::Span;

use super::value::ValueType;

#[derive(Debug, Clone)]
pub enum Number {
    Int(Int),
    Float(f64),
    Rational(Box<BigRational>),
    Complex(Complex64),
}

#[derive(Debug)]
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
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

impl Default for Number {
    fn default() -> Self {
        Number::Int(Int::Int64(0))
    }
}

impl PartialEq for Int {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Int::BigInt(left), Int::BigInt(right)) => left == right,
            (Int::BigInt(left), Int::Int64(right)) => left == &BigInt::from(*right),
            (Int::Int64(left), Int::BigInt(right)) => &BigInt::from(*left) == right,
            (Int::Int64(left), Int::Int64(right)) => left == right,
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
                // If this float happens to be an integer (such as 1.0) hash it as if it's an int
                match Int::from_f64_if_int(*f) {
                    None => {
                        state.write_u8(2);
                        OrderedFloat(*f).hash(state);
                    }
                    Some(int) => {
                        state.write_u8(1);
                        int.hash(state);
                    }
                }
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

impl PartialEq for RealNumber<'_> {
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
        a.to_bigint()
            .partial_cmp(&x)
            .map(|ord| ord.then(0.0f64.total_cmp(&b.fract())))
    }
}

impl PartialOrd for RealNumber<'_> {
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

impl Not for Number {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Number::Int(int) => int.not().into(),
            // TODO: bitwise negation of all non integer numbers in Noulith result in NAN, is that what we want for our language too?
            _ => f64::NAN.into(),
        }
    }
}

trait Unbox {
    type Output;
    fn unbox(self) -> Self::Output;
}

impl Unbox for Box<BigRational> {
    type Output = BigRational;
    fn unbox(self) -> Self::Output {
        *self
    }
}

impl<'a> Unbox for &'a Box<BigRational> {
    type Output = &'a BigRational;
    fn unbox(self) -> Self::Output {
        &**self
    }
}

macro_rules! impl_binary_operator {
    ($self:ty, $other:ty, $trait:ident, $method:ident,$intmethod:expr,$floatmethod:expr,$rationalmethod:expr,$complexmethod:expr) => {
        impl $trait<$other> for $self {
            type Output = Number;
            fn $method(self, other: $other) -> Number {
                match (self, other) {
                    // Complex
                    (Number::Complex(left), right) => {
                        Number::Complex($complexmethod(left, right.to_complex()))
                    }
                    (left, Number::Complex(right)) => {
                        Number::Complex($complexmethod(left.to_complex(), right))
                    }
                    // Float
                    // NOTE: these `expect` calls are safe because complex has already been handled
                    (Number::Float(left), right) => Number::Float($floatmethod(
                        left,
                        right.to_f64().expect("cannot convert complex to float"),
                    )),
                    (left, Number::Float(right)) => Number::Float($floatmethod(
                        left.to_f64().expect("cannot convert complex to float"),
                        right,
                    )),
                    // Rational
                    // NOTE: these `expect` calls are safe because complex and float are handled
                    (left, Number::Rational(right)) => Number::rational($rationalmethod(
                        left.to_rational().expect("cannot convert to rational"),
                        right.unbox(),
                    )),
                    (Number::Rational(left), right) => Number::rational($rationalmethod(
                        left.unbox(),
                        right.to_rational().expect("cannot convert to rational"),
                    )),
                    // Integer
                    (Number::Int(left), Number::Int(right)) => Number::Int($intmethod(left, right)),
                }
            }
        }
    };
}

macro_rules! impl_binary_operator_all {
    ($implement:ident,$method:ident,$intmethod:expr,$floatmethod:expr,$rationalmethod:expr,$complexmethod:expr) => {
        impl_binary_operator!(
            Number,
            Number,
            $implement,
            $method,
            $intmethod,
            $floatmethod,
            $rationalmethod,
            $complexmethod
        );
        impl_binary_operator!(
            Number,
            &Number,
            $implement,
            $method,
            $intmethod,
            $floatmethod,
            $rationalmethod,
            $complexmethod
        );
        impl_binary_operator!(
            &Number,
            Number,
            $implement,
            $method,
            $intmethod,
            $floatmethod,
            $rationalmethod,
            $complexmethod
        );
        impl_binary_operator!(
            &Number,
            &Number,
            $implement,
            $method,
            $intmethod,
            $floatmethod,
            $rationalmethod,
            $complexmethod
        );
    };
}

impl_binary_operator_all!(Add, add, Add::add, Add::add, Add::add, Add::add);
impl_binary_operator_all!(Sub, sub, Sub::sub, Sub::sub, Sub::sub, Sub::sub);
impl_binary_operator_all!(Mul, mul, Mul::mul, Mul::mul, Mul::mul, Mul::mul);
impl_binary_operator_all!(Rem, rem, Rem::rem, Rem::rem, Rem::rem, Rem::rem);

impl Div<&Number> for &Number {
    type Output = Number;

    fn div(self, rhs: &Number) -> Self::Output {
        match (self.to_rational(), rhs.to_rational()) {
            (Some(left), Some(right)) if !right.is_zero() => Number::rational(left / right),
            _ => match (self.to_f64(), rhs.to_f64()) {
                (Some(left), Some(right)) => Number::Float(left / right),
                _ => Number::Complex(self.to_complex() / rhs.to_complex()),
            },
        }
    }
}

impl Div<Number> for Number {
    type Output = Number;

    fn div(self, rhs: Number) -> Self::Output {
        &self / &rhs
    }
}
impl Div<&Number> for Number {
    type Output = Number;

    fn div(self, rhs: &Number) -> Self::Output {
        &self / rhs
    }
}
impl Div<Number> for &Number {
    type Output = Number;

    fn div(self, rhs: Number) -> Self::Output {
        self / &rhs
    }
}

#[derive(thiserror::Error, Debug)]
pub enum EuclideanDivisionError {
    #[error("euclidean division failed (because division by zero?)")]
    OperationFailed,
    #[error("euclidean division is not defined for {left} and {right}")]
    UndefinedOperation { left: NumberType, right: NumberType },
}

impl Number {
    #[must_use]
    pub fn complex(re: f64, im: f64) -> Self {
        Self::Complex(Complex64 { re, im })
    }

    #[must_use]
    pub fn float(f: f64) -> Self {
        Self::Float(f)
    }

    #[must_use]
    pub fn rational(rat: BigRational) -> Self {
        Self::Rational(Box::new(rat))
    }

    // TODO: change this to &'static str
    fn type_name(&self) -> String {
        match self {
            Self::Int(_) => "int".to_string(),
            Self::Float(_) => "float".to_string(),
            Self::Rational(_) => "rational".to_string(),
            Self::Complex(_) => "complex".to_string(),
        }
    }

    pub fn checked_rem_euclid(self, rhs: Self) -> Result<Self, EuclideanDivisionError> {
        match (self, rhs) {
            (Self::Int(p1), Self::Int(p2)) => p1
                .checked_rem_euclid(&p2)
                .ok_or(EuclideanDivisionError::OperationFailed)
                .map(Self::Int),

            (Self::Float(p1), Self::Float(p2)) => Ok(Self::Float(p1.rem_euclid(p2))),
            (left, right) => Err(EuclideanDivisionError::UndefinedOperation {
                left: NumberType::from(&left),
                right: NumberType::from(&right),
            }),
        }
    }

    #[must_use]
    pub fn floor_div(self, rhs: Self) -> Self {
        match (self, rhs) {
            // Handle this case separately because it's faster??
            (Number::Int(Int::Int64(l)), Number::Int(Int::Int64(r))) => {
                Number::Int(Int::Int64(l.div_euclid(r)))
            }
            (l, r) => (l / r).floor(),
        }
    }

    #[must_use]
    pub fn pow(self, rhs: Self) -> Self {
        match (self, rhs) {
            // Int vs others
            (Self::Int(p1), Self::Int(p2)) => {
                if p2.is_negative() {
                    let p2 = p2.to_bigint();
                    let p2 = p2.magnitude();
                    let ans = num::pow::Pow::pow(p1.to_bigint(), p2);
                    Self::Rational(Box::new(BigRational::new(BigInt::from(1), ans)))
                } else {
                    Self::Int(p1.pow(&p2))
                }
            }
            (Self::Int(p1), Self::Float(p2)) => Self::Float(f64::from(p1).powf(p2)),
            (Self::Int(p1), Self::Complex(p2)) => {
                Self::Complex(Complex::from(f64::from(p1)).powc(p2))
            }
            (Self::Int(p1), Self::Rational(p2)) => {
                if p2.is_integer() {
                    return Number::Int(p1.pow(&Int::BigInt(p2.to_integer())));
                }

                Self::Float(f64::from(p1).powf(rational_to_float(&p2)))
            }

            // Rational vs Others
            (Self::Rational(p1), Self::Int(p2)) => {
                Self::Rational(Box::new(num::pow::Pow::pow(&*p1, p2.to_bigint())))
            }
            (Self::Rational(p1), Self::Rational(p2)) => {
                if p2.is_integer() {
                    if let Some(p2) = p2.to_i32() {
                        return Self::Rational(Box::new(p1.pow(p2)));
                    }
                }

                Self::Float(rational_to_float(&p1).powf(rational_to_float(&p2)))
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
                Self::Complex(p1.powc(rational_to_complex(&p2)))
            }
        }
    }

    /// # Errors
    /// Returns an `EvaluationError` (for now) if you try to convert Inf or NaN to an int
    pub fn to_int_lossy(&self) -> Result<Self, EvaluationError> {
        let n = match self {
            Number::Int(i) => Self::Int(i.clone()),
            Number::Float(f) => {
                if let Some(bi) = BigInt::from_f64(*f) {
                    Number::Int(Int::BigInt(bi).simplified())
                } else {
                    // TODO FIX line 0 column 0
                    return Err(EvaluationError::type_error(
                        format!("cannot convert {f} to int"),
                        Span::new(0, 0), // TODO: fix span creation (move out of this impl)
                    ));
                }
            }
            Number::Rational(r) => Self::Int(Int::BigInt(r.to_integer()).simplified()),
            Number::Complex(c) => {
                return Err(EvaluationError::type_error(
                    format!("cannot convert complex number {c} to int"),
                    Span::new(0, 0), // TODO: fix span creation (move out of this impl)
                ));
            }
        };
        Ok(n)
    }

    pub fn to_complex(&self) -> Complex64 {
        match self {
            Number::Int(i) => Complex64::from(i),
            Number::Float(f) => Complex64::from(f),
            Number::Rational(r) => rational_to_complex(&r),
            Number::Complex(c) => *c,
        }
    }

    pub fn to_f64(&self) -> Option<f64> {
        match self {
            Number::Int(i) => Some(f64::from(i)),
            Number::Float(f) => Some(*f),
            Number::Rational(r) => Some(rational_to_float(r)),
            Number::Complex(_) => None,
        }
    }

    pub fn to_rational(&self) -> Option<BigRational> {
        match self {
            Number::Int(i) => Some(BigRational::from(i)),
            Number::Float(_) => None,
            Number::Rational(r) => Some(BigRational::clone(&**r)),
            Number::Complex(_) => None,
        }
    }

    /// Converts this number into a real (complex) number with the imaginary part set to 0.0
    /// which makes it easy to do comparison on all numbers (and possibly other things)
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

    #[must_use]
    pub fn signum(&self) -> Self {
        match self {
            Number::Int(i) => i.signum().into(),
            Number::Float(f) => Number::Float(f.signum()),
            Number::Rational(ratio) => Number::from(ratio.signum()),
            Number::Complex(complex) => {
                // I trust you Brian :crycat:
                if complex.re.is_zero() && complex.im.is_zero() {
                    self.clone()
                } else {
                    Number::Complex(complex / complex.norm())
                }
            }
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
                        if let Some(i) = Int::from_f64_trunc(f) {
                            Number::Int(i)
                        } else {
                            Number::Float(f)
                        }
                    }
                    // TODO: fix bigint -> int
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
    #[error("failed to convert from int because of: '{0}'")]
    FromIntError(#[from] TryFromIntError),
    #[error("failed to convert from bigint to number because of: '{0}'")]
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

pub fn into_fallible_operation<E>(
    op: impl Fn(Number, Number) -> Number,
) -> impl Fn(Number, Number) -> Result<Number, E> {
    move |left, right| Ok(op(left, right))
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum NumberType {
    Int,
    Float,
    Rational,
    Complex,
}

impl From<NumberType> for ValueType {
    fn from(value: NumberType) -> Self {
        Self::Number(value)
    }
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
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::Rational => write!(f, "Rational"),
            Self::Complex => write!(f, "Complex"),
        }
    }
}
