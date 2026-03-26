//! Holds the [`File`] database input.

use camino::{Utf8Path, Utf8PathBuf};
use salsa::Durability;

use crate::{salsa::db::BaseDb, FxDashMap};

/// The main database input.
#[salsa::input(debug)]
pub struct File {
    #[returns(ref)]
    pub path: Utf8PathBuf,

    // TODO: combine `contents` and `is_readable_file` into `contents: Option<String>`
    /// **Note:** The file may not exist. Check [`File::is_readable_file`] before reading contents.
    #[returns(ref)]
    pub contents: String,

    /// Salsa doesn't support deleting inputs: <https://github.com/astral-sh/ruff/blob/438ef334d3e71ef0ac70927c799fb159d8f25267/crates/ruff_db/src/files.rs#L322-L323>
    ///
    /// Missing files should also exist inside Salsa's database: <https://github.com/astral-sh/ruff/blob/438ef334d3e71ef0ac70927c799fb159d8f25267/crates/ruff_db/src/files.rs#L31-L34>
    pub is_readable_file: bool,
}

#[derive(Clone, Default)]
pub struct Files {
    by_path: FxDashMap<Utf8PathBuf, File>,
}
impl Files {
    /// Gets a file by path.
    ///
    /// # Missing files
    ///
    /// **Note:** The file may not exist. Check [`File::is_readable_file`] before reading contents.
    ///
    /// # Panics
    ///
    /// The path must be absolute.
    // DashMap::entry requires the path to be owned...
    #[must_use]
    pub fn get_file(&self, db: &dyn BaseDb, path: &Utf8Path) -> File {
        assert!(path.is_absolute());

        *self.by_path.entry_ref(path).or_insert_with(|| {
            tracing::trace!("Adding file '{path}'");

            let (contents, is_readable_file) = match std::fs::read_to_string(path) {
                Ok(contents) => (contents, true),
                Err(_) => (String::new(), false),
            };

            File::builder(path.to_owned(), contents, is_readable_file)
                .path_durability(Durability::HIGH) // the path is immutable
                .new(db)
        })
    }
}

// SAFETY: uhh?? DashMap probably maybe please shouldn't die?
// TODO: wait for response here https://github.com/astral-sh/ruff/pull/12711/files#r2550904450
impl std::panic::RefUnwindSafe for Files {}

#[cfg(test)]
mod tests {
    use camino::Utf8Path;

    use crate::salsa::db::BaseDb;

    #[test]
    fn read_file() {
        let db = crate::salsa::db::BaseDatabase::default();
        let file_path = Utf8Path::new(env!("CARGO_MANIFEST_DIR")).join("test_data/basic.dts");
        let file = db.get_files().get_file(&db, &file_path.clone());

        assert_eq!(
            (file.contents(&db), file.is_readable_file(&db)),
            (&std::fs::read_to_string(file_path).unwrap(), true)
        );
    }
}
