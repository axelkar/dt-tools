use crate::salsa::file::Files;

#[salsa::db]
pub trait BaseDb: salsa::Database {
    fn get_files(&self) -> &Files;
}

#[salsa::db]
#[derive(Clone, Default)]
pub struct BaseDatabase {
    storage: salsa::Storage<Self>,
    files: Files,
}

#[salsa::db]
impl salsa::Database for BaseDatabase {}

#[salsa::db]
impl BaseDb for BaseDatabase {
    fn get_files(&self) -> &Files {
        &self.files
    }
}

pub struct DbSnapshot {
    pub(crate) db: BaseDatabase,
}

/// Perform an operation on the database that may be cancelled.
///
/// From: <https://github.com/rust-lang/rust-analyzer/blob/4e4aee41c969e86adefdb8c687e2e91bb101329a/crates/ide/src/lib.rs#L862>
impl DbSnapshot {
    /// # Errors
    ///
    /// Returns an error if a Salsa query gets cancelled in `f`.
    pub fn with_db<F, T>(&self, f: F) -> Result<T, salsa::Cancelled>
    where
        F: FnOnce(&BaseDatabase) -> T + std::panic::UnwindSafe,
    {
        salsa::Cancelled::catch(|| f(&self.db))
    }
}
