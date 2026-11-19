//! The cache of a `pure fn`: the result of each call, under the arguments it
//! was called with. `fib(30)` finds the result of an earlier `fib(30)`, and
//! two calls whose arguments only hash alike each get their own.

use crate::Object;
use crate::hash_map::hasher;
use crate::value::Value;
use hashbrown::HashTable;
use hashbrown::hash_table::Entry;
use std::hash::{Hash, Hasher};

/// The results a `pure fn` has worked out so far.
#[derive(Default)]
pub struct MemoCache {
    entries: HashTable<MemoEntry>,
    /// The copy the cache last made of the list, map or string each
    /// argument held, by position. A function called again and again with
    /// the same grid shares one copy of it between all its keys.
    copies: Vec<Option<Value>>,
}

struct MemoEntry {
    key: MemoKey,
    result: Value,
}

/// The arguments of a call, as they were when it was made, with their hash.
pub struct MemoKey {
    hash: u64,
    args: Args,
}

/// Arguments, kept next to each other when there are one or two of them, as
/// there usually are, to spare an allocation for each call.
enum Args {
    One([Value; 1]),
    Two([Value; 2]),
    Many(Box<[Value]>),
}

impl MemoCache {
    /// The result of an earlier call with arguments `==` to `args`, whose
    /// hash is `hash`.
    pub fn get(&self, hash: u64, args: &[Value]) -> Option<&Value> {
        self.entries
            .find(hash, |entry| entry.key.args.as_slice() == args)
            .map(|entry| &entry.result)
    }

    /// Remembers the result of the call made with `key`.
    pub fn insert(&mut self, key: MemoKey, result: Value) {
        match self.entries.entry(
            key.hash,
            |entry| entry.key.args.as_slice() == key.args.as_slice(),
            |entry| entry.key.hash,
        ) {
            Entry::Occupied(mut entry) => entry.get_mut().result = result,
            Entry::Vacant(entry) => {
                entry.insert(MemoEntry { key, result });
            }
        }
    }

    /// The key of a call with `args`, whose hash is `hash`. It holds the
    /// arguments as they are now, copying each list, map or string in them,
    /// so that the program changing one later doesn't change the key:
    /// `xs.push(1)` after `f(xs)` mustn't make the cache think `f` was
    /// called with the longer list.
    pub fn key(&mut self, hash: u64, args: &[Value]) -> MemoKey {
        let args = match args {
            [a] => Args::One([self.snapshot(0, a)]),
            [a, b] => Args::Two([self.snapshot(0, a), self.snapshot(1, b)]),
            args => Args::Many(
                args.iter()
                    .enumerate()
                    .map(|(position, arg)| self.snapshot(position, arg))
                    .collect(),
            ),
        };
        MemoKey { hash, args }
    }

    /// `value`, the argument at `position`, as it is now: the value itself
    /// when nothing in it can change, as with numbers and tuples of them, or
    /// else a copy no one else holds. The copy is the one the cache made
    /// last time when that's still equal, as nothing changes it.
    fn snapshot(&mut self, position: usize, value: &Value) -> Value {
        if is_immutable(value) {
            return value.clone();
        }
        if self.copies.len() <= position {
            self.copies.resize(position + 1, None);
        }
        let last = &mut self.copies[position];
        if let Some(copy) = last
            && copy == value
        {
            return copy.clone();
        }
        let copy = value.deep_copy();
        *last = Some(copy.clone());
        copy
    }
}

impl MemoKey {
    /// The hash a cache files a call with these arguments under. Arguments
    /// `==` calls equal hash alike: a `pure fn` takes only values it compares
    /// by what they are.
    pub fn hash(args: &[Value]) -> u64 {
        let mut hasher = hasher();
        for arg in args {
            arg.hash(&mut hasher);
        }
        hasher.finish()
    }
}

impl Args {
    fn as_slice(&self) -> &[Value] {
        match self {
            Self::One(args) => args,
            Self::Two(args) => args,
            Self::Many(args) => args,
        }
    }
}

fn is_immutable(value: &Value) -> bool {
    match value {
        Value::Object(object) => match object.as_ref() {
            Object::Some(value) => is_immutable(value),
            Object::Tuple(values) => values.iter().all(is_immutable),
            _ => false,
        },
        _ => true,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn calls_whose_arguments_hash_alike_get_their_own_results() {
        let mut cache = MemoCache::default();
        let key = cache.key(7, &[Value::Int(1)]);
        cache.insert(key, Value::Int(10));
        assert_eq!(cache.get(7, &[Value::Int(2)]), None);

        let key = cache.key(7, &[Value::Int(2)]);
        cache.insert(key, Value::Int(20));
        assert_eq!(cache.get(7, &[Value::Int(1)]), Some(&Value::Int(10)));
        assert_eq!(cache.get(7, &[Value::Int(2)]), Some(&Value::Int(20)));
    }

    #[test]
    fn changing_an_argument_later_leaves_the_key_as_it_was() {
        let list = Value::list(vec![Value::Int(1)]);
        let mut cache = MemoCache::default();
        let hash = MemoKey::hash(std::slice::from_ref(&list));
        let key = cache.key(hash, std::slice::from_ref(&list));
        cache.insert(key, Value::Int(1));

        let Value::Object(object) = &list else {
            unreachable!()
        };
        let Object::List(values) = object.as_ref() else {
            unreachable!()
        };
        values.borrow_mut().push(Value::Int(2));

        // The longer list finds nothing, even under the shorter one's hash.
        assert_eq!(cache.get(hash, std::slice::from_ref(&list)), None);
        let fresh = Value::list(vec![Value::Int(1)]);
        assert_eq!(cache.get(hash, &[fresh]), Some(&Value::Int(1)));
    }
}
