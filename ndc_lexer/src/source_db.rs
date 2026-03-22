use crate::SourceId;

#[derive(Default)]
pub struct SourceDb {
    sources: Vec<(String, String)>,
}

impl SourceDb {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, name: impl Into<String>, source: impl Into<String>) -> SourceId {
        let id = u32::try_from(self.sources.len()).expect("too many sources");
        self.sources.push((name.into(), source.into()));
        SourceId::new(id)
    }

    #[must_use]
    pub fn name(&self, id: SourceId) -> &str {
        &self.sources[id.index()].0
    }

    #[must_use]
    pub fn source(&self, id: SourceId) -> &str {
        &self.sources[id.index()].1
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.sources.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.sources.is_empty()
    }
}
