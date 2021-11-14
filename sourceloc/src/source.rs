use std::{
    path::PathBuf,
    sync::atomic::{self, AtomicU32},
};

/// An 8-byte `Source` identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceId(u64);

/// The kind of the source.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Source {
    File(PathBuf),
    Mem,
}

/// An arena of `Source`s.
pub struct Sources {
    id: u32,
    list: Vec<Source>,
}

impl Sources {
    /// Creates a new `Sources`.
    pub fn new() -> Self {
        static ID_COUNTER: AtomicU32 = AtomicU32::new(0);
        Sources {
            id: ID_COUNTER.fetch_add(1, atomic::Ordering::SeqCst),
            list: Vec::new(),
        }
    }

    /// Creates a new `SourceId` for the given `source`.
    pub fn add(&mut self, source: Source) -> SourceId {
        let id = SourceId(((self.id as u64) << 32) + self.list.len() as u64);
        self.list.push(source);
        id
    }

    /// Returns the `Source` for the given `id`.
    pub fn get(&self, id: SourceId) -> Option<&Source> {
        if id.parent_id() != self.id {
            return None;
        }
        self.list.get(id.self_id() as usize)
    }
}

impl SourceId {
    fn parent_id(&self) -> u32 {
        (self.0 >> 32) as u32
    }

    fn self_id(&self) -> u32 {
        (self.0 & 0xffffffff) as u32
    }
}

#[test]
fn test() {
    let mut sources = Sources::new();
    let a = sources.add(Source::File(PathBuf::from("a")));
    let a2 = sources.add(Source::File(PathBuf::from("a")));
    let b = sources.add(Source::File(PathBuf::from("b")));
    assert!(a != a2);
    assert!(a != b);
    assert!(a2 != b);
    assert!(sources.get(a).unwrap() == sources.get(a2).unwrap());
}

#[test]
fn sources_equality() {
    let mut sources1 = Sources::new();
    let mut sources2 = Sources::new();
    let a = sources1.add(Source::File(PathBuf::from("")));
    let b = sources2.add(Source::File(PathBuf::from("")));
    assert!(sources1.get(b).is_none());
    assert!(sources2.get(a).is_none());
}
