use crate::interpreter::evaluate::EvaluationError;
use num::complex::Complex64;
use num::traits::CheckedEuclid;
use num::FromPrimitive;
use num::{BigInt, BigRational, Signed, ToPrimitive, Zero};
use std::fmt::{Display, Formatter};
use std::ops;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Int {
    Int64(i64),
    BigInt(BigInt),
}

impl From<i32> for Int {
    fn from(value: i32) -> Self {
        Self::Int64(i64::from(value))
    }
}
impl ops::Neg for Int {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Int64(i) => Self::Int64(i.neg()),
            Self::BigInt(i) => Self::BigInt(i.neg()),
        }
    }
}

impl Int {
    pub fn from_f64(value: f64) -> Option<Self> {
        if value.is_nan() || value.is_infinite() {
            return None;
        }

        BigInt::from_f64(value).map(Int::BigInt)
    }
    fn to_bigint(&self) -> BigInt {
        match self {
            Self::Int64(i) => BigInt::from(*i),
            Self::BigInt(b) => b.clone(),
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

    #[must_use] pub fn checked_pow(&self, rhs: &Self) -> Option<Self> {
        if let (Self::Int64(p1), Self::Int64(p2)) = (&self, &rhs) {
            if let Some(p2) = p2.to_u32() {
                if let Some(a) = p1.checked_pow(p2) {
                    return Some(Self::Int64(a));
                }
            }
        }

        let lhs = self.to_bigint();
        let rhs = rhs.to_bigint();
        if let Some(rhs) = rhs.to_u32() {
            return Some(Self::BigInt(lhs.pow(rhs)));
        }

        None
    }

    #[must_use] pub fn is_negative(&self) -> bool {
        match self {
            Self::Int64(i) => i.is_negative(),
            Self::BigInt(i) => i.is_negative(),
        }
    }

    #[must_use] pub fn is_zero(&self) -> bool {
        match self {
            Self::Int64(i) => i.is_zero(),
            Self::BigInt(i) => i.is_zero(),
        }
    }

    #[must_use] pub fn is_positive(&self) -> bool {
        match self {
            Self::Int64(i) => i.is_positive(),
            Self::BigInt(i) => i.is_positive(),
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

impl From<BigInt> for Int {
    fn from(value: BigInt) -> Self {
        Self::BigInt(value)
    }
}

impl From<Int> for f64 {
    fn from(value: Int) -> Self {
        match value {
            Int::Int64(i) => i.to_f64().unwrap_or(Self::INFINITY),
            Int::BigInt(i) => i.to_f64().unwrap_or(Self::INFINITY),
        }
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

impl From<Int> for Complex64 {
    fn from(value: Int) -> Self {
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
                    return Err(EvaluationError::TypeError {
                        message: format!("cannot raise rational to the power of {p2}"),
                    });
                }
            }
            Int::BigInt(p2) => {
                if let Some(p2) = p2.to_i32() {
                    p2
                } else {
                    return Err(EvaluationError::TypeError {
                        message: format!("cannot raise rational to the power of {p2}"),
                    });
                }
            }
        })
    }
}

impl Display for Int {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int64(i) => write!(f, "{i}"),
            Self::BigInt(b) => write!(f, "{b}"),
        }
    }
}
