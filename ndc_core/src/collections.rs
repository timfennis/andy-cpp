//! The hash maps and sets the interpreter uses, which hash every key the
//! same way on every run, so iterating one gives the same order each time:
//! `ndc disassemble` prints the same program twice, and the same program
//! prints a map the same way twice. A hash with a random seed defends
//! against keys chosen to collide, which a script run on your own machine
//! doesn't need.
//!
//! They hash as fast as `ahash`'s own maps, which they're built on. Which
//! hash a key gets can differ between kinds of machine, as `ahash` uses the
//! processor's AES instructions where it has them. A map's order is still
//! the order of its hashes: code that needs another order sorts, or uses an
//! ordered collection.

// The one place that names the hashers the rest of the code may not.
#![allow(clippy::disallowed_types)]

use ahash::{AHasher, RandomState};
use std::hash::BuildHasher;

/// The fixed seeds every map and hasher here starts from.
const SEEDED: RandomState = RandomState::with_seeds(
    0x243f_6a88_85a3_08d3,
    0x1319_8a2e_0370_7344,
    0xa409_3822_299f_31d0,
    0x082e_fa98_ec4e_6c89,
);

/// Builds hashers that all start from the same fixed seeds.
#[derive(Clone, Copy, Debug, Default)]
pub struct FixedState;

impl BuildHasher for FixedState {
    type Hasher = AHasher;

    fn build_hasher(&self) -> AHasher {
        SEEDED.build_hasher()
    }
}

/// A hash map whose order is the same on every run.
pub type HashMap<K, V> = std::collections::HashMap<K, V, FixedState>;

/// A hash set whose order is the same on every run.
pub type HashSet<T> = std::collections::HashSet<T, FixedState>;

/// A hasher that hashes a value the same way on every run.
pub fn hasher() -> AHasher {
    FixedState.build_hasher()
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
