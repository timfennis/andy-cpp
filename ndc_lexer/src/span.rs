use std::ops::Range;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct Span {
    offset: usize,
    length: usize,
}

impl Span {
    #[must_use]
    pub fn new(offset: usize, length: usize) -> Self {
        Self { offset, length }
    }

    #[must_use]
    pub fn merge(self, other: Self) -> Self {
        let from = self.offset.min(other.offset);
        let to = (self.offset + self.length).max(other.offset + other.length);
        Self {
            offset: from,
            length: to - from,
        }
    }

    #[must_use]
    pub fn range(&self) -> Range<usize> {
        self.offset..self.end()
    }

    #[must_use]
    pub fn offset(&self) -> usize {
        self.offset
    }

    #[must_use]
    pub fn end(&self) -> usize {
        self.offset + self.length
    }
}
