#[cfg(feature = "ahash")]
pub use ahash::AHashMap as HashMap;
#[cfg(not(feature = "ahash"))]
pub use std::collections::HashMap;

use std::hash::Hash;

pub trait HashMapExt<K, V> {
    fn union(&mut self, right: HashMap<K, V>)
    where
        K: Hash + Eq;
    fn difference(&mut self, right: HashMap<K, V>)
    where
        K: Hash + Eq;
    fn intersection(&mut self, right: HashMap<K, V>)
    where
        K: Hash + Eq;
}

impl<K, V> HashMapExt<K, V> for HashMap<K, V> {
    fn union(&mut self, other: HashMap<K, V>)
    where
        K: Hash + Eq,
    {
        for (key, value) in other {
            self.insert(key, value);
        }
    }

    fn difference(&mut self, other: HashMap<K, V>)
    where
        K: Hash + Eq,
    {
        self.retain(|key, _| !other.contains_key(key));
    }

    fn intersection(&mut self, other: HashMap<K, V>)
    where
        K: Hash + Eq,
    {
        self.retain(|key, _| other.contains_key(key));
    }
}
