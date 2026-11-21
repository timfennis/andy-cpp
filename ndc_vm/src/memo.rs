//! The cache of a `pure fn`: the result of each call, under a 128-bit hash
//! of the arguments it was called with. `fib(30)` finds the result of an
//! earlier `fib(30)`.
//!
//! The cache keeps no arguments, only their hash, so a call costs no copy of
//! a list it's given and no comparison with one. Two calls share a result
//! only when their arguments hash alike in all 128 bits, which distinct
//! arguments do about once in 2^128 tries. The hash comes from
//! [`WideHasher`], not the maps' hasher: a map compares the keys it finds
//! under a hash, so its hasher may let distinct keys hash alike, and this
//! cache has nothing to compare. A list changed after a call hashes anew at
//! the next call, so it finds the result for what it holds then.

use crate::value::Value;
use hashbrown::HashTable;
use ndc_core::collections::WideHasher;
use std::hash::Hash;

/// The results a `pure fn` has worked out so far.
///
/// The table holds only each entry's index, and the entries sit in order
/// in a `Vec`. A table of 4-byte slots stays small enough for the
/// processor's cache when there are many results: a call with new arguments
/// looks only at the table, and a call with arguments seen before reads one
/// entry, as a table of whole entries would.
#[derive(Default)]
pub struct MemoCache {
    table: HashTable<u32>,
    entries: Vec<(MemoKey, Value)>,
}

/// The 128-bit hash of a call's arguments.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct MemoKey(u128);

impl MemoKey {
    /// The key of a call with `args`. Arguments `==` calls equal hash alike:
    /// a `pure fn` takes only values it compares by what they are.
    pub fn of(args: &[Value]) -> Self {
        let mut hasher = WideHasher::default();
        for arg in args {
            arg.hash(&mut hasher);
        }
        Self(hasher.finish128())
    }

    /// The bits the table files the key under: its low half, as both halves
    /// are whole hashes.
    #[allow(clippy::cast_possible_truncation)]
    fn bucket(self) -> u64 {
        self.0 as u64
    }
}

impl MemoCache {
    /// The result of an earlier call whose arguments had this key.
    pub fn get(&self, key: MemoKey) -> Option<&Value> {
        let entries = &self.entries;
        self.table
            .find(key.bucket(), |&index| entries[index as usize].0 == key)
            .map(|&index| &entries[index as usize].1)
    }

    /// Remembers the result of the call whose arguments had this key.
    pub fn insert(&mut self, key: MemoKey, result: Value) {
        let Self { table, entries } = self;
        if let Some(&index) = table.find(key.bucket(), |&index| entries[index as usize].0 == key) {
            entries[index as usize].1 = result;
            return;
        }
        // Four billion results would take more memory than a machine has.
        let index = u32::try_from(entries.len()).expect("a memo cache holds under 2^32 results");
        entries.push((key, result));
        table.insert_unique(key.bucket(), index, |&index| {
            entries[index as usize].0.bucket()
        });
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn each_key_gets_its_own_result() {
        let mut cache = MemoCache::default();
        let one = MemoKey::of(&[Value::Int(1)]);
        let two = MemoKey::of(&[Value::Int(2)]);
        cache.insert(one, Value::Int(10));
        assert_eq!(cache.get(two), None);
        cache.insert(two, Value::Int(20));
        assert_eq!(cache.get(one), Some(&Value::Int(10)));
        assert_eq!(cache.get(two), Some(&Value::Int(20)));
    }

    #[test]
    fn arguments_that_fxhash_makes_alike_get_their_own_keys() {
        // `FxHasher`, which has no seed and sums what it's given times powers
        // of one constant, hashes these two alike.
        let args = |a, b, c, d| [a, b, c, d].map(Value::Int);
        assert_ne!(
            MemoKey::of(&args(0, 0, 0, 3917)),
            MemoKey::of(&args(12232, 20834, 48495, 0))
        );
    }

    #[test]
    fn a_list_changed_after_a_call_gets_a_new_key() {
        let list = Value::list(vec![Value::Int(1)]);
        let before = MemoKey::of(std::slice::from_ref(&list));
        let Value::Object(object) = &list else {
            unreachable!()
        };
        let crate::Object::List(values) = object.as_ref() else {
            unreachable!()
        };
        values.borrow_mut().push(Value::Int(2));
        assert_ne!(before, MemoKey::of(std::slice::from_ref(&list)));
        let fresh = Value::list(vec![Value::Int(1)]);
        assert_eq!(before, MemoKey::of(&[fresh]));
    }
}
