//! Holds the [`File`] database input.

use camino::{Utf8Path, Utf8PathBuf};
use salsa::Durability;
use tower_lsp_server::ls_types::Uri;

use crate::{
    FxDashMap,
    lsp_utils::path_to_uri,
    salsa::{db::BaseDb, includes::IncludeDirs},
};

/// The main database input.
#[salsa::input(debug)]
pub struct File {
    #[returns(ref)]
    pub path: Utf8PathBuf,

    // TODO(important): combine `contents` and `is_readable_file` into `contents: Option<String>`
    /// **Note:** The file may not exist. Check [`File::is_readable_file`] before reading contents.
    #[returns(ref)]
    pub contents: String,

    /// Salsa doesn't support deleting inputs: <https://github.com/astral-sh/ruff/blob/438ef334d3e71ef0ac70927c799fb159d8f25267/crates/ruff_db/src/files.rs#L322-L323>
    ///
    /// Missing files should also exist inside Salsa's database: <https://github.com/astral-sh/ruff/blob/438ef334d3e71ef0ac70927c799fb159d8f25267/crates/ruff_db/src/files.rs#L31-L34>
    pub is_readable_file: bool,
}

impl File {
    /// Returns the zero-indexed line and column of an offset in this file.
    pub fn line_column(self, db: &dyn BaseDb, offset: usize) -> Option<(usize, usize)> {
        let rope = crate::salsa::rope(db, self).as_ref()?;

        let line = rope.try_byte_to_line(offset).ok()?;
        let first_char_of_line = rope.try_line_to_byte(line).ok()?;
        let column = offset - first_char_of_line;
        Some((line, column))
    }

    /// Returns the URI of the path.
    #[expect(clippy::missing_panics_doc, reason = "Shouldn't panic")]
    pub fn uri(self, db: &dyn BaseDb) -> Uri {
        path_to_uri(self.path(db)).expect("path should be absolute (checked in File constructors)")
    }

    /// Returns the path with the first [include directory](IncludeDirs) prefix removed.
    pub fn shorter_path<'db>(&self, db: &'db dyn BaseDb) -> &'db Utf8Path {
        let path = self.path(db);
        let include_dirs = IncludeDirs::get(db).include_dirs(db);

        include_dirs
            .iter()
            .find_map(|dir| path.strip_prefix(dir).ok())
            .unwrap_or(path.as_path())
    }
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

    /// Insert an in-memory file into the VFS, for testing.
    ///
    /// # Panics
    ///
    /// The path must be absolute.
    pub fn add_virtual(&self, db: &dyn BaseDb, path: Utf8PathBuf, contents: String) -> File {
        assert!(path.is_absolute());

        let file = File::builder(path.clone(), contents, true)
            .path_durability(Durability::HIGH)
            .new(db);
        self.by_path.insert(path, file);
        file
    }
}

// SAFETY: uhh?? DashMap probably maybe please shouldn't die?
// TODO: wait for response here https://github.com/astral-sh/ruff/pull/12711/files#r2550904450
impl std::panic::RefUnwindSafe for Files {}

#[cfg(test)]
mod tests {
    use camino::Utf8Path;

    use crate::salsa::{db::BaseDb, includes::IncludeDirs};

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

    #[test]
    fn shorter_path() {
        let db = crate::salsa::db::BaseDatabase::default();
        IncludeDirs::new(&db, vec!["/foo/bar".into()]);

        let file = db
            .get_files()
            .add_virtual(&db, "/foo/bar/main.dts".into(), String::new());
        assert_eq!(file.shorter_path(&db), "main.dts");

        let file = db
            .get_files()
            .add_virtual(&db, "/foo/baz/other.dts".into(), String::new());
        assert_eq!(file.shorter_path(&db), "/foo/baz/other.dts");
    }
}
