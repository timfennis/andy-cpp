use crate::ast::Operator;
use crate::interpreter::int::Int;
use crate::interpreter::EvaluationError;
use num::{BigInt, BigRational};
use std::fmt::Formatter;
use std::ops;
use std::ops::{Add, Rem};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Number {
    Int(Int),
    Float(f64),
    Rational(BigRational),
}

impl Number {
    fn type_name(&self) -> String {
        match self {
            Number::Int(_) => "int".to_string(),
            Number::Float(_) => "float".to_string(),
            Number::Rational(_) => "rational".to_string(),
        }
    }
}

impl From<i32> for Number {
    fn from(value: i32) -> Self {
        Number::Int(value.into())
    }
}

impl ops::Neg for Number {
    type Output = Number;

    fn neg(self) -> Self::Output {
        match self {
            Number::Int(i) => Number::Int(i.neg()),
            Number::Float(f) => Number::Float(f.neg()),
            Number::Rational(r) => Number::Rational(r.neg()),
        }
    }
}

impl ops::Add for Number {
    type Output = Number;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // Operands are the same
            (Number::Int(i1), Number::Int(i2)) => Number::Int(i1 + i2),
            (Number::Float(f1), Number::Float(f2)) => Number::Float(f1.add(f2)),
            (Number::Rational(r1), Number::Rational(r2)) => Number::Rational(r1 + r2),

            // Float
            (Number::Float(p1), Number::Int(p2)) => Number::Float(p1.add(f64::from(p2))),
            (Number::Int(p1), Number::Float(p2)) => Number::Float(f64::from(p1).add(p2)),

            // TODO: obviously we should implement all other cases instead of throwing an error
            (a, b) => panic!(
                "addition between {} and {} is not implemented",
                a.type_name(),
                b.type_name()
            ),
        }
    }
}

impl ops::Sub for Number {
    type Output = Number;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // Operands are the same
            (Number::Int(i1), Number::Int(i2)) => Number::Int(i1 - i2),
            (Number::Float(f1), Number::Float(f2)) => Number::Float(f1.add(f2)),
            (Number::Rational(r1), Number::Rational(r2)) => Number::Rational(r1 - r2),
            // TODO: implement other cases
            (a, b) => panic!(
                "subtraction between {} and {} is not implemented",
                a.type_name(),
                b.type_name()
            ),
        }
    }
}

impl ops::Div for Number {
    type Output = Number;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(ref p1), Number::Int(p2)) => {
                if p1.rem(&p2) == 0i32.into() {
                    return Number::Int(p1.div(&p2));
                }
                Number::Rational(BigRational::new(p1.into(), p2.into()))
            }
            (Number::Float(p1), Number::Float(p2)) => Number::Float(p1 / p2),
            (Number::Rational(p1), Number::Rational(p2)) => Number::Rational(p1 / p2),
            // TODO: implement other cases
            (a, b) => panic!(
                "division between {} and {} is not implemented",
                a.type_name(),
                b.type_name()
            ),
        }
    }
}

impl ops::Mul for Number {
    type Output = Number;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(p1), Number::Int(p2)) => Number::Int(p1 * p2),
            (Number::Float(p1), Number::Float(p2)) => Number::Float(p1 * p2),
            (Number::Rational(p1), Number::Rational(p2)) => Number::Rational(p1 * p2),
            // TODO: implement other cases
            (a, b) => panic!(
                "multiplication between {} and {} is not implemented",
                a.type_name(),
                b.type_name()
            ),
        }
    }
}

impl ops::Rem for Number {
    type Output = Number;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(p1), Number::Int(p2)) => Number::Int(p1.rem(p2)),
            (Number::Float(p1), Number::Float(p2)) => Number::Float(p1.rem(p2)),
            (Number::Rational(p1), Number::Rational(p2)) => Number::Rational(p1.rem(p2)),
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

    pub fn checked_pow(self, rhs: Self) -> Result<Self, EvaluationError> {
        Ok(match (self, rhs) {
            (Number::Int(p1), Number::Int(p2)) => {
                let a = p1.checked_pow(p2).ok_or(EvaluationError::IntegerOverflow {
                    operator: Operator::Exponent,
                })?;
                Number::Int(a)
            }
            (a, b) => panic!(
                "remainder between {} and {} is not implemented",
                a.type_name(),
                b.type_name()
            ),
        })
    }
}

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
            Number::Float(ff) => write!(f, "{ff}"),
            Number::Rational(r) => write!(f, "{r}"),
        }
    }
}
