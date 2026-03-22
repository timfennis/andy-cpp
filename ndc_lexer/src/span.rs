use std::ops::Range;

#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
pub struct SourceId(u32);

impl SourceId {
    pub const SYNTHETIC: Self = Self(u32::MAX);

    #[must_use]
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    #[must_use]
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct Span {
    source_id: SourceId,
    offset: usize,
    length: usize,
}

impl Span {
    #[must_use]
    pub fn new(source_id: SourceId, offset: usize, length: usize) -> Self {
        Self {
            source_id,
            offset,
            length,
        }
    }

    #[must_use]
    pub fn synthetic() -> Self {
        Self {
            source_id: SourceId::SYNTHETIC,
            offset: 0,
            length: 0,
        }
    }

    #[must_use]
    pub fn merge(self, other: Self) -> Self {
        debug_assert_eq!(
            self.source_id, other.source_id,
            "cannot merge spans from different sources"
        );
        let from = self.offset.min(other.offset);
        let to = (self.offset + self.length).max(other.offset + other.length);
        Self {
            source_id: self.source_id,
            offset: from,
            length: to - from,
        }
    }

    #[must_use]
    pub fn source_id(&self) -> SourceId {
        self.source_id
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
