use std::fmt;
use std::hash::{Hash, Hasher};

/// A range of `Int`s with an end, as it was written: `start..end` or
/// `start..=end`. It's a value: iterating over it, slicing with it or
/// asking what's in it never changes it.
///
/// Two ranges are equal when they give the same `Int`s, so `1..=3` is
/// `1..4`, and every empty range is every other one.
#[derive(Clone, Copy, Debug)]
pub struct Range {
    pub start: i64,
    pub end: i64,
    pub inclusive: bool,
}

impl Range {
    pub fn new(start: i64, end: i64, inclusive: bool) -> Self {
        Self {
            start,
            end,
            inclusive,
        }
    }

    /// The first and the last `Int` of the range, or `None` if it has none:
    /// `Some((1, 3))` for `1..4`.
    pub fn bounds(&self) -> Option<(i64, i64)> {
        let last = if self.inclusive {
            self.end
        } else {
            self.end.checked_sub(1)?
        };
        (self.start <= last).then_some((self.start, last))
    }

    /// How many `Int`s the range gives. `i64::MIN..=i64::MAX` gives 2^64,
    /// one more than a `u64` holds, so this is a `u128`.
    pub fn len(&self) -> u128 {
        self.bounds().map_or(0, |(first, last)| {
            (i128::from(last) - i128::from(first) + 1) as u128
        })
    }

    pub fn is_empty(&self) -> bool {
        self.bounds().is_none()
    }

    pub fn contains(&self, n: i64) -> bool {
        self.bounds()
            .is_some_and(|(first, last)| first <= n && n <= last)
    }
}

impl PartialEq for Range {
    fn eq(&self, other: &Self) -> bool {
        self.bounds() == other.bounds()
    }
}

impl Eq for Range {}

impl Hash for Range {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.bounds().hash(state);
    }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let operator = if self.inclusive { "..=" } else { ".." };
        write!(f, "{}{operator}{}", self.start, self.end)
    }
}

#[cfg(test)]
mod tests {
    use super::Range;

    #[test]
    fn ranges_that_give_the_same_ints_are_equal() {
        assert_eq!(Range::new(1, 3, true), Range::new(1, 4, false));
        assert_eq!(Range::new(5, 3, false), Range::new(0, 0, false));
        assert_eq!(Range::new(2, 2, false), Range::new(9, 1, true));
        assert_ne!(Range::new(1, 3, false), Range::new(1, 3, true));
    }

    #[test]
    fn the_largest_ranges_have_a_length() {
        assert_eq!(Range::new(i64::MIN, i64::MAX, true).len(), 1 << 64);
        assert_eq!(Range::new(i64::MIN, i64::MIN, false).len(), 0);
        assert_eq!(Range::new(i64::MAX, i64::MAX, true).len(), 1);
        assert!(Range::new(i64::MAX, i64::MAX, true).contains(i64::MAX));
    }
}
