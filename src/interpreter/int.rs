use crate::interpreter::EvaluationError;
use num::bigint::ToBigInt;
use num::traits::CheckedEuclid;
use num::{BigInt, BigRational, Complex, ToPrimitive};
use std::fmt::{Display, Formatter};

use std::ops;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Int {
    Int64(i64),
    BigInt(BigInt),
}

impl From<i32> for Int {
    fn from(value: i32) -> Self {
        Int::Int64(i64::from(value))
    }
}

impl ops::Neg for Int {
    type Output = Int;

    fn neg(self) -> Self::Output {
        match self {
            Int::Int64(i) => Int::Int64(i.neg()),
            Int::BigInt(i) => Int::BigInt(i.neg()),
        }
    }
}

impl Int {
    fn to_bigint(&self) -> BigInt {
        match self {
            Int::Int64(i) => BigInt::from(*i),
            Int::BigInt(b) => b.clone(),
        }
    }

    pub fn checked_rem_euclid(self, rhs: Self) -> Option<Self> {
        if let (Int::Int64(p1), Int::Int64(p2)) = (&self, &rhs) {
            if let Some(a) = (*p1).checked_rem_euclid(*p2) {
                return Some(Int::Int64(a));
            }
        }

        self.to_bigint()
            .checked_rem_euclid(&rhs.to_bigint())
            .map(Int::BigInt)
    }

    pub fn checked_pow(self, rhs: Self) -> Option<Self> {
        if let (Int::Int64(p1), Int::Int64(p2)) = (&self, &rhs) {
            if let Some(p2) = p2.to_u32() {
                if let Some(a) = p1.checked_pow(p2) {
                    return Some(Int::Int64(a));
                }
            }
        }

        let lhs = self.to_bigint();
        let rhs = rhs.to_bigint();
        if let Some(rhs) = rhs.to_u32() {
            return Some(Int::BigInt(lhs.pow(rhs)));
        }

        None
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

impl From<Int> for f64 {
    fn from(value: Int) -> Self {
        match value {
            Int::Int64(i) => i.to_f64().unwrap_or(f64::INFINITY),
            Int::BigInt(i) => i.to_f64().unwrap_or(f64::INFINITY),
        }
    }
}

impl From<Int> for BigInt {
    fn from(value: Int) -> Self {
        match value {
            Int::Int64(i) => BigInt::from(i),
            Int::BigInt(b) => b,
        }
    }
}

impl From<&Int> for BigInt {
    fn from(value: &Int) -> Self {
        match value {
            Int::Int64(i) => BigInt::from(*i),
            Int::BigInt(b) => b.clone(),
        }
    }
}

impl From<Int> for BigRational {
    fn from(value: Int) -> Self {
        BigRational::from(value.to_bigint())
    }
}

impl From<Int> for Complex<f64> {
    fn from(value: Int) -> Self {
        match value{
            Int::Int64(i) => Complex::from(i.to_f64().unwrap_or(f64::INFINITY)),
            Int::BigInt(i) => Complex::from(i.to_f64().unwrap_or(f64::INFINITY))
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
            Int::Int64(i) => write!(f, "{i}"),
            Int::BigInt(b) => write!(f, "{b}"),
        }
    }
}
