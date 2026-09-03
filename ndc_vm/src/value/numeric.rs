use ndc_core::StaticType;
use ndc_core::num::{AdvancedNumber, hash_exact_i64};
use num::{FromPrimitive, ToPrimitive};
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};

/// The runtime representation mode of a numeric [`super::Value`].
///
/// This is an operational distinction used for overload selection and result
/// promotion. It does not define a subtype relationship between Andy types.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NumericMode {
    Int,
    Float,
    Number,
}

impl NumericMode {
    pub const ALL: [Self; 3] = [Self::Int, Self::Float, Self::Number];

    pub fn static_type(self) -> StaticType {
        match self {
            Self::Int => StaticType::Int,
            Self::Float => StaticType::Float,
            Self::Number => StaticType::Number,
        }
    }

    /// Return the numeric mode used by an operation combining both modes.
    pub fn promote(self, other: Self) -> Self {
        if self == Self::Number || other == Self::Number {
            Self::Number
        } else if self == Self::Float || other == Self::Float {
            Self::Float
        } else {
            Self::Int
        }
    }
}

/// A borrowed, non-subtyping view over the three numeric [`super::Value`] variants.
#[derive(Clone, Copy, Debug)]
pub enum NumericRef<'a> {
    Int(i64),
    Float(f64),
    Number(&'a AdvancedNumber),
}

impl<'a> NumericRef<'a> {
    pub fn mode(self) -> NumericMode {
        match self {
            Self::Int(_) => NumericMode::Int,
            Self::Float(_) => NumericMode::Float,
            Self::Number(_) => NumericMode::Number,
        }
    }

    pub fn as_int(self) -> Option<i64> {
        match self {
            Self::Int(value) => Some(value),
            Self::Float(_) | Self::Number(_) => None,
        }
    }

    pub fn as_number(self) -> Option<&'a AdvancedNumber> {
        match self {
            Self::Number(value) => Some(value),
            Self::Int(_) | Self::Float(_) => None,
        }
    }

    /// Promote a primitive numeric value to `f64` without accepting `Number`.
    pub fn to_primitive_float(self) -> Option<f64> {
        match self {
            Self::Int(value) => Some(value as f64),
            Self::Float(value) => Some(value),
            Self::Number(_) => None,
        }
    }

    /// Convert a real numeric value to `f64`.
    ///
    /// Complex `Number` values cannot be represented and return `None`.
    pub fn to_f64(self) -> Option<f64> {
        match self {
            Self::Int(value) => Some(value as f64),
            Self::Float(value) => Some(value),
            Self::Number(value) => value.to_f64(),
        }
    }

    /// Convert to `i64` using the language's checked, truncating `int()` policy.
    pub fn to_i64_truncating(self) -> Option<i64> {
        match self {
            Self::Int(value) => Some(value),
            Self::Float(value) => float_to_i64(value),
            Self::Number(value) => match value {
                AdvancedNumber::Int(value) => value.to_i64(),
                AdvancedNumber::Float(value) => float_to_i64(*value),
                AdvancedNumber::Rational(value) => value.to_integer().to_i64(),
                AdvancedNumber::Complex(_) => None,
            },
        }
    }

    /// Promote this value to the owned representation used by `Number`.
    pub fn to_advanced_number(self) -> AdvancedNumber {
        match self {
            Self::Int(value) => AdvancedNumber::Int(num::BigInt::from(value)),
            Self::Float(value) => AdvancedNumber::Float(value),
            Self::Number(value) => value.clone(),
        }
    }

    pub fn compare(self, other: Self) -> Ordering {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => left.cmp(&right),
            (Self::Float(left), Self::Float(right)) => compare_floats(left, right),
            (Self::Int(left), Self::Float(right)) => compare_int_float(left, right),
            (Self::Float(left), Self::Int(right)) => compare_int_float(right, left).reverse(),
            (Self::Number(left), Self::Number(right)) => left
                .partial_cmp(right)
                .expect("AdvancedNumber values are totally ordered"),
            (Self::Int(left), Self::Number(right)) => AdvancedNumber::Int(left.into())
                .partial_cmp(right)
                .expect("AdvancedNumber values are totally ordered"),
            (Self::Float(left), Self::Number(right)) => AdvancedNumber::Float(left)
                .partial_cmp(right)
                .expect("AdvancedNumber values are totally ordered"),
            (Self::Number(left), Self::Int(right)) => left
                .partial_cmp(&AdvancedNumber::Int(right.into()))
                .expect("AdvancedNumber values are totally ordered"),
            (Self::Number(left), Self::Float(right)) => left
                .partial_cmp(&AdvancedNumber::Float(right))
                .expect("AdvancedNumber values are totally ordered"),
        }
    }
}

impl PartialEq for NumericRef<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.compare(*other) == Ordering::Equal
    }
}

impl Eq for NumericRef<'_> {}

impl PartialOrd for NumericRef<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NumericRef<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.compare(*other)
    }
}

impl Hash for NumericRef<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            // Both arms agree with `AdvancedNumber`'s hash, which routes
            // integers through `hash_exact_i64` too, so `5`, `5.0` and `5n`
            // land in the same bucket.
            Self::Int(value) => hash_exact_i64(*value, state),
            Self::Float(value) => AdvancedNumber::Float(*value).hash(state),
            Self::Number(value) => value.hash(state),
        }
    }
}

fn float_to_i64(value: f64) -> Option<i64> {
    if value.is_finite() {
        num::BigInt::from_f64(value.trunc())?.to_i64()
    } else {
        None
    }
}

/// Match `AdvancedNumber`'s total ordering without constructing exact rational
/// representations for two values that are already stored as floats.
fn compare_floats(left: f64, right: f64) -> Ordering {
    match (left.is_nan(), right.is_nan()) {
        (true, true) => Ordering::Equal,
        (true, false) => Ordering::Greater,
        (false, true) => Ordering::Less,
        (false, false) => left
            .partial_cmp(&right)
            .expect("non-NaN floats are totally ordered"),
    }
}

/// Compare an `i64` to an `f64` exactly, without first allocating a `BigInt`
/// and exact `BigRational` for their `AdvancedNumber` representations.
fn compare_int_float(integer: i64, float: f64) -> Ordering {
    const I64_MIN_AS_F64: f64 = i64::MIN as f64;
    const I64_UPPER_BOUND_AS_F64: f64 = i64::MAX as f64;

    if float.is_nan() || float >= I64_UPPER_BOUND_AS_F64 {
        return Ordering::Less;
    }
    if float < I64_MIN_AS_F64 {
        return Ordering::Greater;
    }

    let truncated = float.trunc() as i64;
    match integer.cmp(&truncated) {
        Ordering::Equal if float.fract() == 0.0 => Ordering::Equal,
        Ordering::Equal if float.is_sign_positive() => Ordering::Less,
        Ordering::Equal => Ordering::Greater,
        ordering => ordering,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::hash_map::DefaultHasher;

    fn hash(value: NumericRef<'_>) -> u64 {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);
        hasher.finish()
    }

    #[test]
    fn equal_numeric_refs_hash_alike_across_modes() {
        let five = AdvancedNumber::Int(5.into());
        let five_halves = AdvancedNumber::rational(num::BigRational::new(5.into(), 2.into()));

        for reference in [NumericRef::Float(5.0), NumericRef::Number(&five)] {
            assert_eq!(reference, NumericRef::Int(5));
            assert_eq!(hash(reference), hash(NumericRef::Int(5)));
        }

        // Values that miss the integer fast path still have to agree.
        assert_eq!(NumericRef::Float(2.5), NumericRef::Number(&five_halves));
        assert_eq!(
            hash(NumericRef::Float(2.5)),
            hash(NumericRef::Number(&five_halves))
        );
        assert_ne!(hash(NumericRef::Int(5)), hash(NumericRef::Float(2.5)));
    }

    #[test]
    fn numeric_modes_promote_by_runtime_representation() {
        assert_eq!(NumericMode::Int.promote(NumericMode::Int), NumericMode::Int);
        assert_eq!(
            NumericMode::Int.promote(NumericMode::Float),
            NumericMode::Float
        );
        assert_eq!(
            NumericMode::Float.promote(NumericMode::Int),
            NumericMode::Float
        );
        for mode in NumericMode::ALL {
            assert_eq!(mode.promote(NumericMode::Number), NumericMode::Number);
            assert_eq!(NumericMode::Number.promote(mode), NumericMode::Number);
        }
    }
}
