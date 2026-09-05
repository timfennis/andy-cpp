use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

use ndc_core::StaticType;
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

/// Tags keeping the three hashing schemes below from colliding with each other.
const HASH_TAG_EXACT_I64: u8 = 0;
const HASH_TAG_CANONICAL: u8 = 1;
const HASH_TAG_EXACT_F64: u8 = 2;

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

        // A value an `f64` represents exactly hashes from its bits, which
        // keeps float keys off the allocating path entirely. Only the values
        // no float can express build the exact rational form.
        if let Some(value) = self.as_exact_f64() {
            HASH_TAG_EXACT_F64.hash(state);
            // Every NaN compares equal here, so every NaN must hash alike
            // regardless of the payload bits it carries.
            let bits = if value.is_nan() {
                f64::NAN.to_bits()
            } else {
                value.to_bits()
            };
            bits.hash(state);
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
    ($self:ty, $other:ty, $trait:ident, $method:ident) => {
        impl $trait<$other> for $self {
            type Output = Result<AdvancedNumber, BinaryOperatorError>;
            fn $method(self, other: $other) -> Self::Output {
                Ok(match (self, other) {
                    // Integer
                    (AdvancedNumber::Int(left), AdvancedNumber::Int(right)) => {
                        AdvancedNumber::Int($trait::$method(left, right))
                    }
                    // Complex
                    (AdvancedNumber::Complex(left), right) => {
                        AdvancedNumber::Complex($trait::$method(left, right.to_complex()))
                    }
                    (left, AdvancedNumber::Complex(right)) => {
                        AdvancedNumber::Complex($trait::$method(left.to_complex(), right))
                    }
                    // Float
                    // NOTE: these `expect` calls are safe because complex has already been handled
                    (AdvancedNumber::Float(left), right) => {
                        AdvancedNumber::Float($trait::$method(left, right.expect_f64()))
                    }
                    (left, AdvancedNumber::Float(right)) => {
                        AdvancedNumber::Float($trait::$method(left.expect_f64(), right))
                    }
                    // Rational
                    // NOTE: these `expect` calls are safe because complex and float are handled
                    (left, AdvancedNumber::Rational(right)) => AdvancedNumber::rational(
                        $trait::$method(left.expect_rational(), right.unbox()),
                    ),
                    (AdvancedNumber::Rational(left), right) => AdvancedNumber::rational(
                        $trait::$method(left.unbox(), right.expect_rational()),
                    ),
                })
            }
        }
    };
}

impl_binary_operator!(AdvancedNumber, AdvancedNumber, Add, add);
impl_binary_operator!(AdvancedNumber, AdvancedNumber, Sub, sub);
impl_binary_operator!(AdvancedNumber, AdvancedNumber, Mul, mul);

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
            (Self::Float(left), right) => Self::Float(left % right.expect_f64()),
            (left, Self::Float(right)) => Self::Float(left.expect_f64() % right),
            // Rational
            (left, Self::Rational(right)) => Self::rational(left.expect_rational() % right.unbox()),
            (Self::Rational(left), right) => Self::rational(left.unbox() % right.expect_rational()),
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

    /// The value as the `f64` that represents it *exactly*, or `None` when
    /// the conversion would lose anything. One step up from
    /// [`Self::as_exact_i64`]: a float always qualifies, a rational only when
    /// its denominator is a power of two that survives the mantissa.
    ///
    /// Equal values agree here, because exactness is a property of the number
    /// and not of the variant holding it: `0.5` and `1n/2n` both answer
    /// `Some(0.5)`, so both reach the same hash.
    fn as_exact_f64(&self) -> Option<f64> {
        match self {
            Self::Int(value) => {
                let candidate = value.to_f64()?;
                (BigInt::from_f64(candidate).as_ref() == Some(value)).then_some(candidate)
            }
            Self::Float(value) => Some(*value),
            Self::Rational(value) => {
                let candidate = value.to_f64()?;
                BigRational::from_float(candidate)?
                    .eq(value.as_ref())
                    .then_some(candidate)
            }
            Self::Complex(value) => (value.im == 0.0).then_some(value.re),
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
    /// Wrap an exact fraction, collapsing a denominator of 1 back to
    /// [`Self::Int`]. Division and the remainders all land here, so without
    /// the collapse `4n / 2n` stays a `Rational` that prints as `2` but no
    /// longer matches the `Int` arm any consumer switches on.
    pub fn rational(rat: BigRational) -> Self {
        if rat.is_integer() {
            return Self::Int(rat.to_integer());
        }
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
    /// A negative exponent yields the reciprocal as a rational; `pow` has
    /// already rejected the `0 ^ negative` pair that would divide by zero.
    fn int_pow(base: &BigInt, exponent: &BigInt) -> Result<Self, BinaryOperatorError> {
        if exponent.is_negative() {
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

        // `0 ^ negative` is division by zero rather than a value. Both exact
        // bases need the guard here: the rational arms below would otherwise
        // panic inside num-rational with a zero denominator, taking the whole
        // process down. Inexact zeroes keep their float infinity.
        let exact_zero_base = match &self {
            Self::Int(base) => base.is_zero(),
            Self::Rational(base) => base.is_zero(),
            Self::Float(_) | Self::Complex(_) => false,
        };
        let exact_negative_exponent = match &rhs {
            Self::Int(exponent) => exponent.is_negative(),
            Self::Rational(exponent) => exponent.is_negative(),
            Self::Float(_) | Self::Complex(_) => false,
        };
        if exact_zero_base && exact_negative_exponent {
            return Err(BinaryOperatorError::new("division by zero".to_string()));
        }

        Ok(match (self, rhs) {
            // Exact results first: an integer exponent keeps an exact base exact.
            (Self::Int(base), Self::Int(exponent)) => return Self::int_pow(&base, &exponent),
            (Self::Int(base), Self::Rational(exponent)) if exponent.is_integer() => {
                return Self::int_pow(&base, &exponent.to_integer());
            }
            (Self::Rational(base), Self::Int(exponent)) => {
                Self::Rational(Box::new(num::pow::Pow::pow(&*base, exponent)))
            }
            (Self::Rational(base), Self::Rational(exponent))
                if exponent.is_integer() && exponent.to_i32().is_some() =>
            {
                let exponent = exponent.to_i32().expect("checked by the match guard");
                Self::Rational(Box::new(base.pow(exponent)))
            }

            // A complex operand on either side keeps the result complex.
            (Self::Complex(base), exponent) => Self::Complex(base.powc(exponent.to_complex())),
            (base, Self::Complex(exponent)) => Self::Complex(base.to_complex().powc(exponent)),

            // Everything else is real, so evaluate in floating point.
            (base, exponent) => float_pow(base.expect_f64(), exponent.expect_f64()),
        })
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

    /// The `f64` value of a number that a caller has already established is not
    /// complex. Panics otherwise.
    fn expect_f64(&self) -> f64 {
        self.to_f64().expect("cannot convert complex to float")
    }

    /// The exact rational value of a number that a caller has already
    /// established is an integer or a rational. Panics otherwise.
    fn expect_rational(&self) -> BigRational {
        self.to_rational().expect("cannot convert to rational")
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

/// `base ^ exponent` in floating point, escaping to the complex plane when a
/// negative base is raised to a fractional power: `(-8.0) ^ 2.0` is `64.0`,
/// but `(-8.0) ^ 0.5` has no real value.
fn float_pow(base: f64, exponent: f64) -> AdvancedNumber {
    // `fract()` is NaN for an infinite or NaN exponent, which compares
    // unequal to zero and would send a finite negative base into the complex
    // plane. `powf` already answers those correctly.
    if base < 0.0 && exponent.is_finite() && exponent.fract() != 0.0 {
        AdvancedNumber::Complex(Complex64::from(base).powf(exponent))
    } else {
        AdvancedNumber::Float(base.powf(exponent))
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
            AdvancedNumber::Rational(Box::new(BigRational::new(10.into(), 2.into()))),
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
