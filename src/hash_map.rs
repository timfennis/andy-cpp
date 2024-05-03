#[cfg(feature = "ahash")]
pub use ahash::AHashMap as HashMap;
#[cfg(not(feature = "ahash"))]
pub use std::collections::HashMap;

#[cfg(feature = "ahash")]
pub use ahash::AHasher as DefaultHasher;
#[cfg(not(feature = "ahash"))]
pub use std::hash::DefaultHasher;

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
pub fn union<K, V>(left: &HashMap<K, V>, right: &HashMap<K, V>) -> HashMap<K, V>
where
    K: Clone + Hash + Eq,
    V: Clone,
{
    left.iter()
        .chain(right.iter())
        .map(|(key, value)| (key.to_owned(), value.to_owned()))
        .collect()
}

pub fn intersection<K, V>(left: &HashMap<K, V>, right: &HashMap<K, V>) -> HashMap<K, V>
where
    K: Clone + Hash + Eq,
    V: Clone,
{
    left.iter()
        .filter_map(|(key, value)| {
            if right.contains_key(key) {
                Some((key.clone(), value.clone()))
            } else {
                None
            }
        })
        .collect()
}

pub fn symmetric_difference<K, V>(left: &HashMap<K, V>, right: &HashMap<K, V>) -> HashMap<K, V>
where
    K: Clone + Hash + Eq,
    V: Clone,
{
    left.iter()
        .chain(right.iter())
        .filter_map(|(key, value)| {
            if !right.contains_key(key) || !left.contains_key(key) {
                Some((key.clone(), value.clone()))
            } else {
                None
            }
        })
        .collect()
}
