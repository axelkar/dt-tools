//! Source file identities.

use camino::{Utf8Path, Utf8PathBuf};
use rustc_hash::FxHashMap;

/// An identifier for a source file within one preprocessing run.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceId(pub u32);

/// A source file participating in a preprocessing run.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    pub id: SourceId,
    pub path: Utf8PathBuf,
}

/// Source files for one preprocessing run.
#[derive(Debug, Default)]
pub struct SourceSet {
    files: Vec<SourceFile>,
    by_path: FxHashMap<Utf8PathBuf, SourceId>,
}

impl SourceSet {
    /// Registers a path, returning its existing or new id.
    ///
    /// # Panics
    ///
    /// Panics if more than [`u32::MAX`] files are registered.
    pub fn add(&mut self, path: &Utf8Path) -> SourceId {
        if let Some(id) = self.by_path.get(path) {
            return *id;
        }
        let id = SourceId(u32::try_from(self.files.len()).expect("too many source files"));
        self.files.push(SourceFile {
            id,
            path: path.to_owned(),
        });
        self.by_path.insert(path.to_owned(), id);
        id
    }

    /// Returns the file with `id`.
    ///
    /// # Panics
    ///
    /// Panics if `id` is out of range for the set.
    #[must_use]
    pub fn get(&self, id: SourceId) -> &SourceFile {
        &self.files[id.0 as usize]
    }

    /// Consumes the set, returning the files in first-encounter order.
    #[must_use]
    pub fn into_files(self) -> Vec<SourceFile> {
        self.files
    }
}
