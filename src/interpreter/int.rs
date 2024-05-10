use crate::interpreter::evaluate::EvaluationError;
use crate::lexer::Location;
use num::bigint::Sign;
use num::complex::Complex64;
use num::traits::CheckedEuclid;
use num::FromPrimitive;
use num::{pow::Pow, BigInt, BigRational, Signed, ToPrimitive, Zero};
use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops;

#[derive(Debug, Clone)]
pub enum Int {
    Int64(i64),
    BigInt(BigInt),
}

impl Int {
    pub fn from_f64(value: f64) -> Option<Self> {
        if value.is_nan() || value.is_infinite() {
            return None;
        }

        BigInt::from_f64(value).map(Int::BigInt)
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
            i @ Int::Int64(_) => i,
            Int::BigInt(bi) => {
                if let Some(i) = bi.to_i64() {
                    Int::Int64(i)
                } else {
                    Int::BigInt(bi)
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

    /// # Panics
    /// This method panics if the `rhs` argument is negative
    #[must_use]
    pub fn pow(&self, rhs: &Self) -> Self {
        let lhs = self.to_bigint();
        let rhs = rhs.to_bigint();
        match rhs.sign() {
            Sign::Minus => panic!("right hand side must not be negative"),
            Sign::NoSign => Int::Int64(1),
            // This is kinda dumb because if the `rhs` doesn't fit in a u64 we sure as hell aren't
            // going to be able to compute it in finite time
            Sign::Plus => Int::BigInt(Pow::pow(lhs, rhs.magnitude())),
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
    pub fn abs(&self) -> Int {
        match self {
            Int::Int64(i) => Self::from(i.abs()),
            Int::BigInt(b) => Self::from(b.abs()),
        }
    }
}

impl Eq for Int {}
impl Ord for Int {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Int::Int64(l), Int::BigInt(r)) => BigInt::from(*l).cmp(r),
            (Int::BigInt(l), Int::Int64(r)) => l.cmp(&BigInt::from(*r)),
            (Int::Int64(l), Int::Int64(r)) => l.cmp(r),
            (Int::BigInt(l), Int::BigInt(r)) => l.cmp(r),
        }
    }
}

impl Hash for Int {
    // This hash implementation ensures that a BigInt that fits in an i64 is hashed the same way an i64 would
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Int::Int64(i) => state.write_i64(*i),
            Int::BigInt(b) => {
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

impl ops::Neg for Int {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Int64(i) => Self::Int64(i.neg()),
            Self::BigInt(i) => Self::BigInt(i.neg()),
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
        f64::from(&value)
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
                &format!("cannot convert {value:?} to int"),
                Location { line: 0, column: 0 },
                Location { line: 0, column: 0 },
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
                    return Err(EvaluationError::type_error(
                        &format!("cannot convert {p2} to 32-bit signed integer"),
                        Location { line: 0, column: 0 },
                        Location { line: 0, column: 0 },
                    ));
                }
            }
            Int::BigInt(p2) => {
                if let Some(p2) = p2.to_i32() {
                    p2
                } else {
                    return Err(EvaluationError::type_error(
                        &format!("cannot convert {p2} to 32-bit signed integer"),
                        Location { line: 0, column: 0 },
                        Location { line: 0, column: 0 },
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
