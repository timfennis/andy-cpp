//! The hash maps and sets the interpreter uses, which hash every key the
//! same way on every run, so iterating one gives the same order each time:
//! `ndc disassemble` prints the same program twice, and the same program
//! prints a map the same way twice. A hash with a random seed defends
//! against keys chosen to collide, which a script run on your own machine
//! doesn't need.
//!
//! They hash with `rustc-hash`'s `FxHasher`, which has no seed at all and is
//! the fastest of the hashers measured on Advent of Code solutions. It mixes
//! less than a hasher built to resist attacks: its hash is a sum of what it
//! was given, each times a power of one constant, so distinct keys that hash
//! alike are easy to find. A map compares the keys it finds under a hash, so
//! that only costs time, but nothing may treat two values as equal because
//! their `FxHasher` hashes are. Which hash a key gets differs
//! between 32- and 64-bit machines. A map's order is still the order of its
//! hashes: code that needs another order sorts, or uses an ordered
//! collection.
//!
//! Where a hash has to stand for the value it came from, with nothing to
//! compare against, as a `pure fn`'s cache keys its results, [`WideHasher`]
//! hashes to 128 bits with two `foldhash` hashers of the quality kind, whose
//! folded multiplies leave no pattern of values that hash alike.

// The one place that names the hashers the rest of the code may not.
#![allow(clippy::disallowed_types)]

use foldhash::quality::{FixedState as QualityState, FoldHasher};
use rustc_hash::FxHasher;
use std::hash::{BuildHasher, Hasher};

/// Builds hashers that all start the same way.
#[derive(Clone, Copy, Debug, Default)]
pub struct FixedState;

impl BuildHasher for FixedState {
    type Hasher = FxHasher;

    fn build_hasher(&self) -> FxHasher {
        FxHasher::default()
    }
}

/// A hash map whose order is the same on every run.
pub type HashMap<K, V> = std::collections::HashMap<K, V, FixedState>;

/// A hash set whose order is the same on every run.
pub type HashSet<T> = std::collections::HashSet<T, FixedState>;

/// A hasher that hashes a value the same way on every run.
pub fn hasher() -> FxHasher {
    FixedState.build_hasher()
}

/// A hasher with no pattern of distinct values that hash alike, unlike
/// [`hasher`]'s, for a hash that's summed with others or kept in place of
/// the value.
pub fn quality_hasher() -> FoldHasher<'static> {
    QualityState::with_seed(0x243f_6a88_85a3_08d3).build_hasher()
}

/// Hashes to 128 bits, with two quality hashers seeded apart, for a hash
/// that stands for its value: two distinct values share one about once in
/// 2^128 tries.
pub struct WideHasher {
    low: FoldHasher<'static>,
    high: FoldHasher<'static>,
}

impl Default for WideHasher {
    fn default() -> Self {
        Self {
            low: quality_hasher(),
            high: QualityState::with_seed(0x1319_8a2e_0370_7344).build_hasher(),
        }
    }
}

impl WideHasher {
    /// The 128-bit hash of what was written.
    pub fn finish128(&self) -> u128 {
        (u128::from(self.high.finish()) << 64) | u128::from(self.low.finish())
    }
}

impl Hasher for WideHasher {
    fn finish(&self) -> u64 {
        self.low.finish()
    }

    fn write(&mut self, bytes: &[u8]) {
        self.low.write(bytes);
        self.high.write(bytes);
    }

    fn write_u8(&mut self, i: u8) {
        self.low.write_u8(i);
        self.high.write_u8(i);
    }

    fn write_u16(&mut self, i: u16) {
        self.low.write_u16(i);
        self.high.write_u16(i);
    }

    fn write_u32(&mut self, i: u32) {
        self.low.write_u32(i);
        self.high.write_u32(i);
    }

    fn write_u64(&mut self, i: u64) {
        self.low.write_u64(i);
        self.high.write_u64(i);
    }

    fn write_u128(&mut self, i: u128) {
        self.low.write_u128(i);
        self.high.write_u128(i);
    }

    fn write_usize(&mut self, i: usize) {
        self.low.write_usize(i);
        self.high.write_usize(i);
    }
}

#[cfg(test)]
mod tests {
    use super::{HashMap, hasher};
    use std::hash::{Hash, Hasher};

    #[test]
    fn a_value_hashes_the_same_each_time() {
        let hash = |value: &str| {
            let mut hasher = hasher();
            value.hash(&mut hasher);
            hasher.finish()
        };
        assert_eq!(hash("andy"), hash("andy"));
        assert_ne!(hash("andy"), hash("ndc"));
    }

    #[test]
    fn maps_built_the_same_way_iterate_in_the_same_order() {
        // Two maps seeded at random iterate in different orders even in one
        // run, as each gets a seed of its own.
        let map = || {
            (0..1000)
                .map(|key| (key, key))
                .collect::<HashMap<u32, u32>>()
        };
        assert!(map().keys().eq(map().keys()));
    }
}
