use char_index::OwnedIndexedChars;
use std::cell::{Ref, RefCell, RefMut};

/// Mutable UTF-8 storage with a character index built on the first indexed read.
/// Both states own the same string allocation; mutable borrows discard the index.
pub struct VmString {
    storage: RefCell<Storage>,
}

enum Storage {
    Plain(String),
    Indexed(OwnedIndexedChars),
}

impl VmString {
    pub fn new(string: String) -> Self {
        Self {
            storage: RefCell::new(Storage::Plain(string)),
        }
    }

    pub fn borrow(&self) -> Ref<'_, String> {
        Ref::map(self.storage.borrow(), |storage| match storage {
            Storage::Plain(string) => string,
            Storage::Indexed(string) => string.as_string(),
        })
    }

    pub fn borrow_mut(&self) -> RefMut<'_, String> {
        let mut storage = self.storage.borrow_mut();
        if matches!(*storage, Storage::Indexed(_)) {
            let Storage::Indexed(string) =
                std::mem::replace(&mut *storage, Storage::Plain(String::new()))
            else {
                unreachable!()
            };
            *storage = Storage::Plain(string.into_string());
        }
        RefMut::map(storage, |storage| match storage {
            Storage::Plain(string) => string,
            Storage::Indexed(_) => unreachable!(),
        })
    }

    /// Uses the cached count when available without building an index for `.len`.
    pub fn char_count(&self) -> usize {
        match &*self.storage.borrow() {
            Storage::Plain(string) => string.chars().count(),
            Storage::Indexed(string) => string.char_count(),
        }
    }

    pub fn indexed(&self) -> Ref<'_, OwnedIndexedChars> {
        if matches!(*self.storage.borrow(), Storage::Plain(_)) {
            let mut storage = self.storage.borrow_mut();
            let Storage::Plain(string) =
                std::mem::replace(&mut *storage, Storage::Plain(String::new()))
            else {
                unreachable!()
            };
            *storage = Storage::Indexed(OwnedIndexedChars::new(string));
        }
        Ref::map(self.storage.borrow(), |storage| match storage {
            Storage::Indexed(string) => string,
            Storage::Plain(_) => unreachable!(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::VmString;

    #[test]
    fn indexed_reads_match_utf8_boundaries_across_rollovers() {
        for source in [String::new(), "abcd".repeat(1024), "aé中😀".repeat(1024)] {
            let string = VmString::new(source.clone());
            let indexed = string.indexed();
            assert_eq!(indexed.char_count(), source.chars().count());
            for (index, (offset, ch)) in source.char_indices().enumerate() {
                assert_eq!(indexed.get_char(index), Some(ch));
                assert_eq!(indexed.get_index(index), Some(offset));
            }
            assert_eq!(indexed.get_char(indexed.char_count()), None);
            assert_eq!(indexed.get_index(indexed.char_count()), None);
        }
    }

    #[test]
    fn mutation_invalidates_index_without_copying_string() {
        let string = VmString::new("aé中😀".into());
        let original_ptr = string.borrow().as_ptr();
        assert_eq!(string.indexed().get_char(3), Some('😀'));
        assert_eq!(string.borrow().as_ptr(), original_ptr);
        {
            let mut plain = string.borrow_mut();
            assert_eq!(plain.as_ptr(), original_ptr);
            plain.replace_range(1..3, "b");
        }
        assert_eq!(string.char_count(), 4);
        assert_eq!(string.indexed().get_char(1), Some('b'));
        assert_eq!(string.indexed().get_char(3), Some('😀'));
        string.borrow_mut().clear();
        assert_eq!(string.char_count(), 0);
        assert_eq!(string.indexed().get_char(0), None);
    }
}
