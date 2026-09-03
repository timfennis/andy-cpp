use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Div, Mul, Neg, Not, Rem, Sub};

use crate::StaticType;
use num::bigint::TryFromBigIntError;
use num::complex::{Complex64, ComplexFloat};
use num::{BigInt, BigRational, Complex, FromPrimitive, Signed, ToPrimitive, Zero};

#[derive(Debug, Clone)]
pub enum AdvancedNumber {
    Int(BigInt),
    Float(f64),
    Rational(Box<BigRational>),
    Complex(Complex64),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
enum CanonicalScalar {
    NegInfinity,
    Finite(BigRational),
    PosInfinity,
    NaN,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct CanonicalNumber {
    real: CanonicalScalar,
    imaginary: CanonicalScalar,
}

impl CanonicalScalar {
    fn from_float(value: f64) -> Self {
        if value.is_nan() {
            Self::NaN
        } else if value == f64::NEG_INFINITY {
            Self::NegInfinity
        } else if value == f64::INFINITY {
            Self::PosInfinity
        } else {
            Self::Finite(
                BigRational::from_float(value)
                    .expect("finite f64 values have an exact rational representation"),
            )
        }
    }

    fn zero() -> Self {
        Self::Finite(BigRational::from_integer(BigInt::from(0)))
    }
}

impl From<BigInt> for AdvancedNumber {
    fn from(value: BigInt) -> Self {
        Self::Int(value)
    }
}

impl From<i32> for AdvancedNumber {
    fn from(value: i32) -> Self {
        Self::Int(BigInt::from(value))
    }
}

impl From<f64> for AdvancedNumber {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}

impl From<BigRational> for AdvancedNumber {
    fn from(value: BigRational) -> Self {
        Self::Rational(Box::new(value))
    }
}

impl From<Complex64> for AdvancedNumber {
    fn from(value: Complex64) -> Self {
        Self::Complex(value)
    }
}

impl PartialOrd for AdvancedNumber {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.canonical().cmp(&other.canonical()))
    }
}

impl PartialEq for AdvancedNumber {
    fn eq(&self, other: &Self) -> bool {
        self.canonical() == other.canonical()
    }
}

impl Default for AdvancedNumber {
    fn default() -> Self {
        Self::Int(BigInt::zero())
    }
}

/// Tags keeping the two hashing schemes below from colliding with each other.
const HASH_TAG_EXACT_I64: u8 = 0;
const HASH_TAG_CANONICAL: u8 = 1;

/// Hash a number whose exact value is the integer `value`.
///
/// Every numeric representation routes integers here, so `5`, `5.0`, `5n` and
/// `10/2` produce one hash without allocating a [`BigInt`] to say so.
pub fn hash_exact_i64<H: Hasher>(value: i64, state: &mut H) {
    HASH_TAG_EXACT_I64.hash(state);
    value.hash(state);
}

/// `5.0` answers `Some(5)`; `2.5`, `1e300`, `inf` and `NaN` answer `None`.
#[must_use]
pub fn exact_f64_to_i64(value: f64) -> Option<i64> {
    // `i64::MIN` is exactly -2^63, so negating it gives the first f64 above
    // `i64::MAX`. The bound is exclusive because `i64::MAX` itself rounds up
    // to 2^63 when converted.
    const LOWER: f64 = i64::MIN as f64;
    const UPPER: f64 = -LOWER;

    // `fract` is NaN for infinities and NaN, so both fail this comparison.
    if value.fract() == 0.0 && (LOWER..UPPER).contains(&value) {
        Some(value as i64)
    } else {
        None
    }
}

impl Hash for AdvancedNumber {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if let Some(value) = self.as_exact_i64() {
            hash_exact_i64(value, state);
            return;
        }

        HASH_TAG_CANONICAL.hash(state);
        self.canonical().hash(state);
    }
}

impl Neg for AdvancedNumber {
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

impl Not for AdvancedNumber {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Int(int) => int.not().into(),
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

#[derive(Debug)]
pub struct BinaryOperatorError(String);

impl fmt::Display for BinaryOperatorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for BinaryOperatorError {}

impl BinaryOperatorError {
    pub fn new(message: String) -> Self {
        Self(message)
    }

    pub fn undefined_operation(operator: &str, left: &StaticType, right: &StaticType) -> Self {
        Self(format!(
            "operator {operator} is not defined for {left} and {right}"
        ))
    }
}

macro_rules! impl_binary_operator {
    ($self:ty, $other:ty, $trait:ident, $method:ident,$intmethod:expr,$floatmethod:expr,$rationalmethod:expr,$complexmethod:expr) => {
        impl $trait<$other> for $self {
            type Output = Result<AdvancedNumber, BinaryOperatorError>;
            fn $method(self, other: $other) -> Self::Output {
                Ok(match (self, other) {
                    // Integer
                    (AdvancedNumber::Int(left), AdvancedNumber::Int(right)) => {
                        AdvancedNumber::Int($intmethod(left, right))
                    }
                    // Complex
                    (AdvancedNumber::Complex(left), right) => {
                        AdvancedNumber::Complex($complexmethod(left, right.to_complex()))
                    }
                    (left, AdvancedNumber::Complex(right)) => {
                        AdvancedNumber::Complex($complexmethod(left.to_complex(), right))
                    }
                    // Float
                    // NOTE: these `expect` calls are safe because complex has already been handled
                    (AdvancedNumber::Float(left), right) => AdvancedNumber::Float($floatmethod(
                        left,
                        right.to_f64().expect("cannot convert complex to float"),
                    )),
                    (left, AdvancedNumber::Float(right)) => AdvancedNumber::Float($floatmethod(
                        left.to_f64().expect("cannot convert complex to float"),
                        right,
                    )),
                    // Rational
                    // NOTE: these `expect` calls are safe because complex and float are handled
                    (left, AdvancedNumber::Rational(right)) => {
                        AdvancedNumber::rational($rationalmethod(
                            left.to_rational().expect("cannot convert to rational"),
                            right.unbox(),
                        ))
                    }
                    (AdvancedNumber::Rational(left), right) => {
                        AdvancedNumber::rational($rationalmethod(
                            left.unbox(),
                            right.to_rational().expect("cannot convert to rational"),
                        ))
                    }
                })
            }
        }
    };
}

macro_rules! impl_binary_operator_all {
    ($implement:ident,$method:ident,$intmethod:expr,$floatmethod:expr,$rationalmethod:expr,$complexmethod:expr) => {
        impl_binary_operator!(
            AdvancedNumber,
            AdvancedNumber,
            $implement,
            $method,
            $intmethod,
            $floatmethod,
            $rationalmethod,
            $complexmethod
        );
        impl_binary_operator!(
            AdvancedNumber,
            &AdvancedNumber,
            $implement,
            $method,
            $intmethod,
            $floatmethod,
            $rationalmethod,
            $complexmethod
        );
        impl_binary_operator!(
            &AdvancedNumber,
            AdvancedNumber,
            $implement,
            $method,
            $intmethod,
            $floatmethod,
            $rationalmethod,
            $complexmethod
        );
        impl_binary_operator!(
            &AdvancedNumber,
            &AdvancedNumber,
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

/// Returns `true` for the number kinds that use exact (integer/rational)
/// arithmetic.
fn is_exact(n: &AdvancedNumber) -> bool {
    matches!(n, AdvancedNumber::Int(_) | AdvancedNumber::Rational(_))
}

impl Rem<Self> for AdvancedNumber {
    type Output = Result<Self, BinaryOperatorError>;

    fn rem(self, rhs: Self) -> Self::Output {
        // Exact arithmetic cannot represent a zero-divisor result. Number
        // operations deliberately fall back to IEEE floating-point values in
        // that case, just like exact division does.
        if is_exact(&self) && is_exact(&rhs) && rhs.is_zero() {
            return Ok(Self::Float(
                self.to_f64().unwrap_or(f64::NAN) % rhs.to_f64().unwrap_or(f64::NAN),
            ));
        }
        Ok(match (self, rhs) {
            // Integer
            (Self::Int(left), Self::Int(right)) => Self::Int(left % right),
            // Complex
            (Self::Complex(left), right) => Self::Complex(left % right.to_complex()),
            (left, Self::Complex(right)) => Self::Complex(left.to_complex() % right),
            // Float
            (Self::Float(left), right) => {
                Self::Float(left % right.to_f64().expect("cannot convert complex to float"))
            }
            (left, Self::Float(right)) => {
                Self::Float(left.to_f64().expect("cannot convert complex to float") % right)
            }
            // Rational
            (left, Self::Rational(right)) => Self::rational(
                left.to_rational().expect("cannot convert to rational") % right.unbox(),
            ),
            (Self::Rational(left), right) => Self::rational(
                left.unbox() % right.to_rational().expect("cannot convert to rational"),
            ),
        })
    }
}

impl Rem<&Self> for AdvancedNumber {
    type Output = Result<Self, BinaryOperatorError>;

    fn rem(self, rhs: &Self) -> Self::Output {
        self % rhs.clone()
    }
}

impl Rem<AdvancedNumber> for &AdvancedNumber {
    type Output = Result<AdvancedNumber, BinaryOperatorError>;

    fn rem(self, rhs: AdvancedNumber) -> Self::Output {
        self.clone() % rhs
    }
}

impl Rem<&AdvancedNumber> for &AdvancedNumber {
    type Output = Result<AdvancedNumber, BinaryOperatorError>;

    fn rem(self, rhs: &AdvancedNumber) -> Self::Output {
        self.clone() % rhs.clone()
    }
}

impl Div<&AdvancedNumber> for &AdvancedNumber {
    type Output = AdvancedNumber;

    fn div(self, rhs: &AdvancedNumber) -> Self::Output {
        match (self.to_rational(), rhs.to_rational()) {
            (Some(left), Some(right)) if !right.is_zero() => AdvancedNumber::rational(left / right),
            _ => match (self.to_f64(), rhs.to_f64()) {
                (Some(left), Some(right)) => AdvancedNumber::Float(left / right),
                _ => AdvancedNumber::Complex(self.to_complex() / rhs.to_complex()),
            },
        }
    }
}

impl Div<Self> for AdvancedNumber {
    type Output = Result<Self, BinaryOperatorError>;

    fn div(self, rhs: Self) -> Self::Output {
        Ok(&self / &rhs)
    }
}
impl Div<&Self> for AdvancedNumber {
    type Output = Result<Self, BinaryOperatorError>;

    fn div(self, rhs: &Self) -> Self::Output {
        Ok(&self / rhs)
    }
}
impl Div<AdvancedNumber> for &AdvancedNumber {
    type Output = Result<AdvancedNumber, BinaryOperatorError>;

    fn div(self, rhs: AdvancedNumber) -> Self::Output {
        Ok(self / &rhs)
    }
}

impl AdvancedNumber {
    /// The exact value as an `i64`, when this number is a real integer that
    /// fits one. `5n`, `5.0`, `10/2` and `5+0i` all answer `Some(5)`, while
    /// `2.5`, `1e300`, `inf` and `5+1i` answer `None`.
    ///
    /// Every representation of one integer answers the same `Some`, which is
    /// what lets [`Hash`] skip building a [`CanonicalNumber`] for it.
    #[must_use]
    pub fn as_exact_i64(&self) -> Option<i64> {
        match self {
            Self::Int(value) => value.to_i64(),
            Self::Float(value) => exact_f64_to_i64(*value),
            // A reduced rational is an integer exactly when its denominator is
            // one, which `is_integer` checks without dividing.
            Self::Rational(value) => value.is_integer().then(|| value.numer().to_i64())?,
            Self::Complex(value) => (value.im == 0.0).then(|| exact_f64_to_i64(value.re))?,
        }
    }

    fn canonical(&self) -> CanonicalNumber {
        match self {
            Self::Int(value) => CanonicalNumber {
                real: CanonicalScalar::Finite(BigRational::from_integer(value.clone())),
                imaginary: CanonicalScalar::zero(),
            },
            Self::Float(value) => CanonicalNumber {
                real: CanonicalScalar::from_float(*value),
                imaginary: CanonicalScalar::zero(),
            },
            Self::Rational(value) => CanonicalNumber {
                real: CanonicalScalar::Finite(value.as_ref().clone()),
                imaginary: CanonicalScalar::zero(),
            },
            Self::Complex(value) => CanonicalNumber {
                real: CanonicalScalar::from_float(value.re),
                imaginary: CanonicalScalar::from_float(value.im),
            },
        }
    }

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

    pub fn static_type(&self) -> StaticType {
        StaticType::Number
    }

    #[must_use]
    pub fn is_zero(&self) -> bool {
        match self {
            Self::Int(i) => i.is_zero(),
            Self::Float(f) => *f == 0.0,
            Self::Rational(r) => r.is_zero(),
            Self::Complex(c) => c.is_zero(),
        }
    }

    pub fn checked_rem_euclid(self, rhs: &Self) -> Result<Self, BinaryOperatorError> {
        if matches!(self, Self::Complex(_)) || matches!(rhs, Self::Complex(_)) {
            return Err(BinaryOperatorError::undefined_operation(
                "%%",
                &self.static_type(),
                &rhs.static_type(),
            ));
        }
        if rhs.is_zero() {
            return Ok(Self::Float(
                self.to_f64()
                    .unwrap_or(f64::NAN)
                    .rem_euclid(rhs.to_f64().unwrap_or(f64::NAN)),
            ));
        }
        if let (Some(left), Some(right)) = (self.to_rational(), rhs.to_rational()) {
            let mut remainder = left % &right;
            if remainder.is_negative() {
                remainder += right.abs();
            }
            return Ok(Self::rational(remainder));
        }
        Ok(Self::Float(
            self.to_f64()
                .expect("complex operands were rejected")
                .rem_euclid(rhs.to_f64().expect("complex operands were rejected")),
        ))
    }

    pub fn floor_div(self, rhs: Self) -> Result<Self, BinaryOperatorError> {
        Ok(self.div(rhs)?.floor())
    }

    /// Raise an integer base to a (possibly negative) integer exponent.
    /// A negative exponent yields the reciprocal as a rational, except
    /// `0 ^ negative`, which is division by zero and returns an error
    /// instead of panicking with a zero denominator.
    fn int_pow(base: &BigInt, exponent: &BigInt) -> Result<Self, BinaryOperatorError> {
        if exponent.is_negative() {
            if base.is_zero() {
                return Err(BinaryOperatorError::new("division by zero".to_string()));
            }
            let denominator = num::pow::Pow::pow(base.clone(), exponent.magnitude());
            Ok(Self::Rational(Box::new(BigRational::new(
                BigInt::from(1),
                denominator,
            ))))
        } else {
            Ok(Self::Int(num::pow::Pow::pow(
                base.clone(),
                exponent.magnitude(),
            )))
        }
    }

    pub fn pow(self, rhs: Self) -> Result<Self, BinaryOperatorError> {
        // Reject astronomically large integer exponents up front: an exponent
        // that doesn't fit in u32 would produce a result too large to compute
        // in finite time. Without this guard, `2 ^ i64::MAX` hangs the VM.
        const MAX_EXPONENT_BITS: u64 = 32;
        let too_large = match &rhs {
            Self::Int(b) => b.magnitude().bits() > MAX_EXPONENT_BITS,
            Self::Rational(p) if p.is_integer() => p.numer().magnitude().bits() > MAX_EXPONENT_BITS,
            _ => false,
        };
        if too_large {
            return Err(BinaryOperatorError::new(
                "exponent too large to compute".to_string(),
            ));
        }

        Ok(match (self, rhs) {
            // Int vs others
            (Self::Int(p1), Self::Int(p2)) => return Self::int_pow(&p1, &p2),
            (Self::Int(p1), Self::Float(p2)) => {
                let p1 = bigint_to_float(&p1);
                if p1 < 0.0 && p2.fract() != 0.0 {
                    Self::Complex(Complex64::from(p1).powf(p2))
                } else {
                    Self::Float(p1.powf(p2))
                }
            }
            (Self::Int(p1), Self::Complex(p2)) => {
                Self::Complex(Complex::from(bigint_to_float(&p1)).powc(p2))
            }
            (Self::Int(p1), Self::Rational(p2)) => {
                if p2.is_integer() {
                    return Self::int_pow(&p1, &p2.to_integer());
                }

                let p1 = bigint_to_float(&p1);
                let p2 = rational_to_float(&p2);
                if p1 < 0.0 {
                    Self::Complex(Complex64::from(p1).powf(p2))
                } else {
                    Self::Float(p1.powf(p2))
                }
            }

            // Rational vs Others
            (Self::Rational(p1), Self::Int(p2)) => {
                Self::Rational(Box::new(num::pow::Pow::pow(&*p1, p2)))
            }
            (Self::Rational(p1), Self::Rational(p2)) => {
                if p2.is_integer()
                    && let Some(p2) = p2.to_i32()
                {
                    return Ok(Self::Rational(Box::new(p1.pow(p2))));
                }

                let p1 = rational_to_float(&p1);
                let p2 = rational_to_float(&p2);
                if p1 < 0.0 {
                    Self::Complex(Complex64::from(p1).powf(p2))
                } else {
                    Self::Float(p1.powf(p2))
                }
            }
            (Self::Rational(p1), Self::Float(p2)) => {
                let p1 = rational_to_float(&p1);
                if p1 < 0.0 && p2.fract() != 0.0 {
                    Self::Complex(Complex64::from(p1).powf(p2))
                } else {
                    Self::Float(p1.powf(p2))
                }
            }
            (Self::Rational(p1), Self::Complex(p2)) => {
                Self::Complex(rational_to_complex(&p1).powc(p2))
            }

            // Float vs others
            (Self::Float(p1), Self::Float(p2)) => {
                if p1 < 0.0 && p2.fract() != 0.0 {
                    Self::Complex(Complex64::from(p1).powf(p2))
                } else {
                    Self::Float(p1.powf(p2))
                }
            }
            (Self::Float(p1), Self::Complex(p2)) => Self::Complex(Complex::from(p1).powc(p2)),
            (Self::Float(p1), Self::Int(p2)) => Self::Float(p1.powf(bigint_to_float(&p2))),
            (Self::Float(p1), Self::Rational(p2)) => {
                let p2 = rational_to_float(&p2);
                if p1 < 0.0 && p2.fract() != 0.0 {
                    Self::Complex(Complex64::from(p1).powf(p2))
                } else {
                    Self::Float(p1.powf(p2))
                }
            }

            // Complex vs others
            (Self::Complex(p1), Self::Complex(p2)) => Self::Complex(p1.powc(p2)),
            (Self::Complex(p1), Self::Float(p2)) => Self::Complex(p1.powc(Complex::from(p2))),
            (Self::Complex(p1), Self::Int(p2)) => {
                Self::Complex(p1.powc(Complex::from(bigint_to_float(&p2))))
            }
            (Self::Complex(p1), Self::Rational(p2)) => {
                Self::Complex(p1.powc(rational_to_complex(&p2)))
            }
        })
    }

    /// # Errors
    /// Returns a `NumberConversionError` if you try to convert Inf, NaN, or Complex to an int
    pub fn to_int_lossy(&self) -> Result<Self, NumberConversionError> {
        let n = match self {
            Self::Int(i) => Self::Int(i.clone()),
            Self::Float(f) => {
                if let Some(bi) = BigInt::from_f64(*f) {
                    Self::Int(bi)
                } else {
                    return Err(NumberConversionError(format!("cannot convert {f} to int")));
                }
            }
            Self::Rational(r) => Self::Int(r.to_integer()),
            Self::Complex(c) => {
                return Err(NumberConversionError(format!(
                    "cannot convert complex number {c} to int"
                )));
            }
        };
        Ok(n)
    }

    #[must_use]
    pub fn to_complex(&self) -> Complex64 {
        match self {
            Self::Int(i) => Complex64::from(bigint_to_float(i)),
            Self::Float(f) => Complex64::from(f),
            Self::Rational(r) => rational_to_complex(r),
            Self::Complex(c) => *c,
        }
    }

    #[must_use]
    pub fn to_f64(&self) -> Option<f64> {
        match self {
            Self::Int(i) => Some(bigint_to_float(i)),
            Self::Float(f) => Some(*f),
            Self::Rational(r) => Some(rational_to_float(r)),
            Self::Complex(_) => None,
        }
    }

    #[must_use]
    pub fn to_rational(&self) -> Option<BigRational> {
        match self {
            Self::Int(i) => Some(BigRational::from_integer(i.clone())),
            Self::Rational(r) => Some(BigRational::clone(&**r)),
            Self::Float(_) | Self::Complex(_) => None,
        }
    }

    #[must_use]
    pub fn abs(&self) -> Self {
        match self {
            Self::Int(i) => Self::Int(i.abs()),
            Self::Float(f) => Self::Float(f.abs()),
            Self::Rational(r) => Self::Rational(Box::new(r.abs())),
            Self::Complex(c) => Self::Float(c.abs()),
        }
    }

    #[must_use]
    pub fn signum(&self) -> Self {
        match self {
            Self::Int(i) => i.signum().into(),
            Self::Float(f) => Self::Float(f.signum()),
            Self::Rational(ratio) => Self::from(ratio.signum()),
            Self::Complex(complex) => {
                // I trust you Brian :crycat:
                if complex.re.is_zero() && complex.im.is_zero() {
                    self.clone()
                } else {
                    Self::Complex(complex / complex.norm())
                }
            }
        }
    }
}

macro_rules! implement_rounding {
    ($method:ident) => {
        impl AdvancedNumber {
            #[must_use]
            pub fn $method(&self) -> AdvancedNumber {
                match self {
                    AdvancedNumber::Int(i) => AdvancedNumber::Int(i.clone()),
                    AdvancedNumber::Float(f) => {
                        let f = f.$method();
                        if let Some(i) = BigInt::from_f64(f) {
                            AdvancedNumber::Int(i)
                        } else {
                            AdvancedNumber::Float(f)
                        }
                    }
                    AdvancedNumber::Rational(r) => AdvancedNumber::Int(r.$method().to_integer()),
                    AdvancedNumber::Complex(c) => {
                        Complex::new(c.re.$method(), c.im.$method()).into()
                    }
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
    #[error("expected a non-negative integer, got {0}")]
    UnsupportedVariant(StaticType),
    #[error("this integer is out of range (must be non-negative and small enough to be an index)")]
    FromBigIntError(#[from] TryFromBigIntError<BigInt>),
}

impl TryFrom<AdvancedNumber> for usize {
    type Error = NumberToUsizeError;

    fn try_from(value: AdvancedNumber) -> Result<Self, Self::Error> {
        match value {
            AdvancedNumber::Int(value) => Ok(Self::try_from(value)?),
            n => Err(NumberToUsizeError::UnsupportedVariant(n.static_type())),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum NumberToFloatError {
    #[error("cannot convert {0} to float")]
    UnsupportedType(StaticType),
    #[error("cannot convert {0} to float")]
    UnsupportedValue(AdvancedNumber),
}

impl TryFrom<&AdvancedNumber> for f64 {
    type Error = NumberToFloatError;

    fn try_from(value: &AdvancedNumber) -> Result<Self, Self::Error> {
        match value {
            AdvancedNumber::Int(value) => value.to_f64(),
            AdvancedNumber::Float(f) => Some(*f),
            AdvancedNumber::Rational(r) => r.to_f64(),
            AdvancedNumber::Complex(_) => {
                return Err(Self::Error::UnsupportedType(value.static_type()));
            }
        }
        .ok_or_else(|| Self::Error::UnsupportedValue(value.clone()))
    }
}

#[derive(thiserror::Error, Debug)]
pub enum NumberToIntError {
    #[error("cannot convert {0} to int")]
    UnsupportedType(StaticType),
    #[error("cannot convert {0} to int")]
    UnsupportedValue(AdvancedNumber),
}

impl TryFrom<&AdvancedNumber> for i64 {
    type Error = NumberToIntError;

    fn try_from(value: &AdvancedNumber) -> Result<Self, Self::Error> {
        match value {
            AdvancedNumber::Int(integer) => integer
                .try_into()
                .map_err(|_err| NumberToIntError::UnsupportedValue(value.clone())),
            _ => Err(Self::Error::UnsupportedType(value.static_type())),
        }
    }
}

impl fmt::Display for AdvancedNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{i}"),
            Self::Float(ff) => {
                let mut buffer = ryu::Buffer::new();
                f.write_str(buffer.format(*ff))
            }
            Self::Rational(r) => write!(f, "{r}"),
            Self::Complex(r) => write!(f, "{r}"),
        }
    }
}

fn rational_to_float(r: &BigRational) -> f64 {
    r.to_f64().unwrap_or(f64::NAN)
}

fn bigint_to_float(value: &BigInt) -> f64 {
    value.to_f64().unwrap_or_else(|| {
        if value.is_negative() {
            f64::NEG_INFINITY
        } else {
            f64::INFINITY
        }
    })
}

fn rational_to_complex(r: &BigRational) -> Complex<f64> {
    Complex::from(r.to_f64().unwrap_or(f64::NAN))
}

#[derive(thiserror::Error, Debug)]
#[error("{0}")]
pub struct NumberConversionError(String);

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    fn hash(value: &AdvancedNumber) -> u64 {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);
        hasher.finish()
    }

    #[test]
    fn equality_and_hash_use_exact_numeric_values() {
        let integer = AdvancedNumber::Int(BigInt::from(1));
        let float = AdvancedNumber::Float(1.0);
        let complex = AdvancedNumber::complex(1.0, -0.0);

        assert_eq!(integer, float);
        assert_eq!(float, complex);
        assert_eq!(hash(&integer), hash(&float));
        assert_eq!(hash(&float), hash(&complex));

        let tenth = AdvancedNumber::rational(BigRational::new(1.into(), 10.into()));
        assert_ne!(tenth, AdvancedNumber::Float(0.1));
    }

    #[test]
    fn every_representation_of_one_integer_hashes_alike() {
        let five = [
            AdvancedNumber::Int(BigInt::from(5)),
            AdvancedNumber::Float(5.0),
            AdvancedNumber::rational(BigRational::new(10.into(), 2.into())),
            AdvancedNumber::complex(5.0, -0.0),
        ];

        for value in &five {
            assert_eq!(value.as_exact_i64(), Some(5));
            assert_eq!(*value, five[0]);
            assert_eq!(hash(value), hash(&five[0]));
        }
    }

    #[test]
    fn values_off_the_integer_fast_path_still_agree() {
        let huge = AdvancedNumber::Int(BigInt::from(u64::MAX) * BigInt::from(u64::MAX));
        let half = AdvancedNumber::rational(BigRational::new(1.into(), 2.into()));

        assert_eq!(huge.as_exact_i64(), None);
        assert_eq!(half.as_exact_i64(), None);

        // 0.5 is exactly 1/2, so the two representations stay interchangeable
        // on the canonical path.
        assert_eq!(half, AdvancedNumber::Float(0.5));
        assert_eq!(hash(&half), hash(&AdvancedNumber::Float(0.5)));
        assert_ne!(hash(&huge), hash(&half));
    }

    #[test]
    fn integer_fast_path_stops_at_the_i64_boundaries() {
        assert_eq!(exact_f64_to_i64(i64::MIN as f64), Some(i64::MIN));
        // 2^63 is one past i64::MAX, so it falls back instead of wrapping.
        assert_eq!(exact_f64_to_i64(-(i64::MIN as f64)), None);
        assert_eq!(exact_f64_to_i64(f64::INFINITY), None);
        assert_eq!(exact_f64_to_i64(f64::NAN), None);
        assert_eq!(exact_f64_to_i64(2.5), None);

        // Negative zero is zero, so it must not take a bucket of its own.
        assert_eq!(exact_f64_to_i64(-0.0), Some(0));
        assert_eq!(
            hash(&AdvancedNumber::Float(-0.0)),
            hash(&AdvancedNumber::Int(BigInt::from(0)))
        );
    }

    #[test]
    fn nan_is_equal_and_sorts_after_infinity() {
        let nan = AdvancedNumber::Float(f64::NAN);
        let complex_nan = AdvancedNumber::complex(f64::NAN, 0.0);
        let infinity = AdvancedNumber::Float(f64::INFINITY);

        assert_eq!(nan, nan.clone());
        assert_eq!(nan, complex_nan);
        assert!(nan > infinity);
    }

    #[test]
    fn complex_order_is_lexicographic() {
        assert!(AdvancedNumber::complex(2.0, 0.0) > AdvancedNumber::complex(1.0, 100.0));
        assert!(AdvancedNumber::complex(1.0, 2.0) < AdvancedNumber::complex(1.0, 3.0));
    }
}
