//! The map behind map values and sets.

use ndc_core::collections::FixedState;
pub use ndc_core::collections::hasher;

/// What a map value holds: its entries in the order they were inserted, so a
/// program iterates and prints a map the way it built it. Removing an entry
/// moves the last one into its place, which takes the same time however big
/// the map is.
pub type HashMap<K, V> = indexmap::IndexMap<K, V, FixedState>;
