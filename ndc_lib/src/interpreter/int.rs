use crate::interpreter::evaluate::EvaluationError;
use crate::lexer::Span;
use num::FromPrimitive;
use num::bigint::{Sign, ToBigInt};
use num::complex::Complex64;
use num::traits::CheckedEuclid;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero, pow::Pow};
use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{BitAnd, BitOr, BitXor, Neg, Not, Shl, Shr};

#[derive(Debug, Clone)]
pub enum Int {
    Int64(i64),
    BigInt(BigInt),
}

impl Int {
    /// Truncates the float and returns Some<Int> if the float was not NaN or Inf
    #[must_use]
    pub fn from_f64_trunc(value: f64) -> Option<Self> {
        if value.is_nan() || value.is_infinite() {
            return None;
        }

        BigInt::from_f64(value).map(Int::BigInt)
    }

    /// Converts to float to an int only if it's fractal part is zero, otherwise it returns None
    #[must_use]
    pub fn from_f64_if_int(value: f64) -> Option<Self> {
        if value.fract() == 0.0 {
            Self::from_f64_trunc(value)
        } else {
            None
        }
    }

    #[must_use]
    pub fn to_bigint(&self) -> BigInt {
        match self {
            Self::Int64(i) => BigInt::from(*i),
            Self::BigInt(b) => b.clone(),
        }
    }

    #[must_use]
    pub fn simplified(self) -> Self {
        match self {
            i @ Self::Int64(_) => i,
            Self::BigInt(bi) => {
                if let Some(i) = bi.to_i64() {
                    Self::Int64(i)
                } else {
                    Self::BigInt(bi)
                }
            }
        }
    }

    pub fn checked_rem_euclid(self, rhs: &Self) -> Option<Self> {
        if let (Self::Int64(p1), Self::Int64(p2)) = (&self, &rhs) {
            if let Some(a) = (*p1).checked_rem_euclid(*p2) {
                return Some(Self::Int64(a));
            }
        }

        self.to_bigint()
            .checked_rem_euclid(&rhs.to_bigint())
            .map(Int::BigInt)
    }

    #[must_use]
    pub fn checked_shl(self, rhs: Self) -> Option<Self> {
        let rhs: u32 = match rhs {
            Self::Int64(rhs) => rhs.try_into().ok()?,
            Self::BigInt(rhs) => rhs.try_into().ok()?,
        };

        match self {
            Self::Int64(lhs) => {
                if let Some(result) = lhs.checked_shl(rhs) {
                    Some(Self::Int64(result))
                } else {
                    Some(Self::BigInt(lhs.to_bigint().expect("cannot fail").shl(rhs)))
                }
            }
            Self::BigInt(big_int) => Some(Self::BigInt(big_int.shl(rhs))),
        }
    }

    #[must_use]
    pub fn checked_shr(self, rhs: Self) -> Option<Self> {
        let rhs: u32 = match rhs {
            Self::Int64(rhs) => rhs.try_into().ok()?,
            Self::BigInt(rhs) => rhs.try_into().ok()?,
        };

        match self {
            Self::Int64(lhs) => {
                if let Some(result) = lhs.checked_shr(rhs) {
                    Some(Self::Int64(result))
                } else {
                    Some(Self::BigInt(lhs.to_bigint().expect("cannot fail").shr(rhs)))
                }
            }
            Self::BigInt(big_int) => Some(Self::BigInt(big_int.shr(rhs))),
        }
    }

    /// # Panics
    /// This method panics if the `rhs` argument is negative
    #[must_use]
    pub fn pow(&self, rhs: &Self) -> Self {
        let lhs = self.to_bigint();
        let rhs = rhs.to_bigint();
        match rhs.sign() {
            Sign::Minus => panic!("right hand side must not be negative"),
            Sign::NoSign => Self::Int64(1),
            // This is kinda dumb because if the `rhs` doesn't fit in a u64 we sure as hell aren't
            // going to be able to compute it in finite time
            Sign::Plus => Self::BigInt(Pow::pow(lhs, rhs.magnitude())),
        }
    }

    #[must_use]
    pub fn is_negative(&self) -> bool {
        match self {
            Self::Int64(i) => i.is_negative(),
            Self::BigInt(i) => i.is_negative(),
        }
    }

    #[must_use]
    pub fn is_zero(&self) -> bool {
        match self {
            Self::Int64(i) => i.is_zero(),
            Self::BigInt(i) => i.is_zero(),
        }
    }

    #[must_use]
    pub fn is_positive(&self) -> bool {
        match self {
            Self::Int64(i) => i.is_positive(),
            Self::BigInt(i) => i.is_positive(),
        }
    }

    #[must_use]
    pub fn abs(&self) -> Self {
        match self {
            Self::Int64(i) => Self::from(i.abs()),
            Self::BigInt(b) => Self::from(b.abs()),
        }
    }

    #[must_use]
    pub fn signum(&self) -> Self {
        match self {
            Self::Int64(i) => Self::Int64(i.signum()),
            Self::BigInt(i) => match i.sign() {
                Sign::Minus => Self::Int64(-1),
                Sign::NoSign => Self::Int64(0),
                Sign::Plus => Self::Int64(1),
            },
        }
    }
}

impl Eq for Int {}
impl Ord for Int {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Int64(l), Self::BigInt(r)) => BigInt::from(*l).cmp(r),
            (Self::BigInt(l), Self::Int64(r)) => l.cmp(&BigInt::from(*r)),
            (Self::Int64(l), Self::Int64(r)) => l.cmp(r),
            (Self::BigInt(l), Self::BigInt(r)) => l.cmp(r),
        }
    }
}

impl Hash for Int {
    // This hash implementation ensures that a BigInt that fits in an i64 is hashed the same way an i64 would
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Int64(i) => state.write_i64(*i),
            Self::BigInt(b) => {
                if let Some(i) = b.to_i64() {
                    state.write_i64(i);
                } else {
                    b.hash(state);
                }
            }
        }
    }
}

impl PartialOrd for Int {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Neg for Int {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Int64(i) => Self::Int64(i.neg()),
            Self::BigInt(i) => Self::BigInt(i.neg()),
        }
    }
}

impl Not for Int {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Int64(i) => i.not().into(),
            Self::BigInt(big_int) => big_int.not().into(),
        }
    }
}

macro_rules! impl_binary_operator {
    ($trait:ident, $method:ident, $safe_method:ident) => {
        impl std::ops::$trait<Int> for Int {
            type Output = Int;

            fn $method(self, rhs: Self) -> Self::Output {
                match (&self, &rhs) {
                    (Int::Int64(p1), Int::Int64(p2)) => {
                        if let Some(s) = p1.$safe_method(*p2) {
                            return Int::Int64(s);
                        }
                    }
                    _ => (),
                }
                Int::BigInt(self.to_bigint().$method(rhs.to_bigint()))
            }
        }
        impl std::ops::$trait<&Int> for Int {
            type Output = Int;

            fn $method(self, rhs: &Self) -> Self::Output {
                match (&self, rhs) {
                    (Int::Int64(p1), Int::Int64(p2)) => {
                        if let Some(s) = p1.$safe_method(*p2) {
                            return Int::Int64(s);
                        }
                    }
                    _ => (),
                }
                Int::BigInt(self.to_bigint().$method(rhs.to_bigint()))
            }
        }
        impl std::ops::$trait<Int> for &Int {
            type Output = Int;

            fn $method(self, rhs: Int) -> Self::Output {
                match (&self, &rhs) {
                    (Int::Int64(p1), Int::Int64(p2)) => {
                        if let Some(s) = p1.$safe_method(*p2) {
                            Int::Int64(s);
                        }
                    }
                    _ => {}
                }

                Int::BigInt(self.to_bigint().$method(rhs.to_bigint()))
            }
        }
        impl std::ops::$trait<&Int> for &Int {
            type Output = Int;

            fn $method(self, rhs: &Int) -> Self::Output {
                match (self, rhs) {
                    (Int::Int64(p1), Int::Int64(p2)) => {
                        if let Some(s) = p1.$safe_method(*p2) {
                            return Int::Int64(s);
                        }
                    }
                    _ => (),
                }
                Int::BigInt(self.to_bigint().$method(rhs.to_bigint()))
            }
        }
    };
}

impl_binary_operator!(Add, add, checked_add);
impl_binary_operator!(Sub, sub, checked_sub);
impl_binary_operator!(Mul, mul, checked_mul);
impl_binary_operator!(Div, div, checked_div);
impl_binary_operator!(Rem, rem, checked_rem);

impl BitAnd for &Int {
    type Output = Int;

    fn bitand(self, rhs: Self) -> Self::Output {
        if let (Int::Int64(p1), Int::Int64(p2)) = (self, rhs) {
            return Int::Int64(p1 & p2);
        }

        Int::BigInt(self.to_bigint().bitand(rhs.to_bigint())).simplified()
    }
}

impl BitAnd for Int {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        &self & &rhs
    }
}

impl BitOr for &Int {
    type Output = Int;

    fn bitor(self, rhs: Self) -> Self::Output {
        if let (Int::Int64(p1), Int::Int64(p2)) = (self, rhs) {
            return Int::Int64(p1 | p2);
        }

        Int::BigInt(self.to_bigint().bitor(rhs.to_bigint()))
    }
}

impl BitOr for Int {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        &self | &rhs
    }
}

impl BitXor for &Int {
    type Output = Int;

    fn bitxor(self, rhs: Self) -> Self::Output {
        if let (Int::Int64(p1), Int::Int64(p2)) = (self, rhs) {
            return Int::Int64(p1 ^ p2);
        }

        Int::BigInt(self.to_bigint().bitxor(rhs.to_bigint())).simplified()
    }
}

impl BitXor for Int {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        &self ^ &rhs
    }
}

impl From<i32> for Int {
    fn from(value: i32) -> Self {
        Self::Int64(i64::from(value))
    }
}

impl From<i64> for Int {
    fn from(value: i64) -> Self {
        Self::Int64(value)
    }
}

impl From<BigInt> for Int {
    fn from(value: BigInt) -> Self {
        Self::BigInt(value)
    }
}

impl From<Int> for f64 {
    fn from(value: Int) -> Self {
        Self::from(&value)
    }
}

impl From<&Int> for f64 {
    fn from(value: &Int) -> Self {
        match value {
            Int::Int64(i) => i.to_f64().unwrap_or(Self::INFINITY),
            Int::BigInt(i) => i.to_f64().unwrap_or(Self::INFINITY),
        }
    }
}

impl TryFrom<f64> for Int {
    type Error = EvaluationError;
    fn try_from(value: f64) -> Result<Self, Self::Error> {
        let bit_int = BigInt::from_f64(value).ok_or_else(|| {
            EvaluationError::type_error(
                format!("cannot convert {value:?} to int"),
                Span::new(0, 0), // TODO: fix span creation (move out of this impl)
            )
        })?;
        Ok(Self::BigInt(bit_int))
    }
}

impl From<Int> for BigInt {
    fn from(value: Int) -> Self {
        match value {
            Int::Int64(i) => Self::from(i),
            Int::BigInt(b) => b,
        }
    }
}

impl From<&Int> for BigInt {
    fn from(value: &Int) -> Self {
        match value {
            Int::Int64(i) => Self::from(*i),
            Int::BigInt(b) => b.clone(),
        }
    }
}

impl From<Int> for BigRational {
    fn from(value: Int) -> Self {
        Self::from(value.to_bigint())
    }
}

impl From<&Int> for BigRational {
    fn from(value: &Int) -> Self {
        Self::from(value.to_bigint())
    }
}

impl From<Int> for Complex64 {
    fn from(value: Int) -> Self {
        Self::from(&value)
    }
}

impl From<&Int> for Complex64 {
    fn from(value: &Int) -> Self {
        match value {
            Int::Int64(i) => Self::from(i.to_f64().unwrap_or(f64::INFINITY)),
            Int::BigInt(i) => Self::from(i.to_f64().unwrap_or(f64::INFINITY)),
        }
    }
}

impl TryFrom<Int> for i32 {
    type Error = EvaluationError;
    fn try_from(value: Int) -> Result<Self, Self::Error> {
        Ok(match value {
            Int::Int64(p2) => {
                if let Some(p2) = p2.to_i32() {
                    p2
                } else {
                    return Err(EvaluationError::type_error(
                        format!("cannot convert {p2} to 32-bit signed integer"),
                        Span::new(0, 0), // TODO: fix span creation (move out of this impl)
                    ));
                }
            }
            Int::BigInt(p2) => {
                if let Some(p2) = p2.to_i32() {
                    p2
                } else {
                    return Err(EvaluationError::type_error(
                        format!("cannot convert {p2} to 32-bit signed integer"),
                        Span::new(0, 0), // TODO: fix span creation (move out of this impl)
                    ));
                }
            }
        })
    }
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int64(i) => write!(f, "{i}"),
            Self::BigInt(b) => write!(f, "{b}"),
        }
    }
}
