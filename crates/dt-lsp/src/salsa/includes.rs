use std::borrow::Cow;

use dt_analyzer::new::outline::AnalyzedToplevel;
use dt_diagnostic::{Diagnostic, Severity};
use dt_parser::TextRange;

use crate::salsa::db::BaseDb;

/// Global setting singleton
#[salsa::input(singleton)]
pub struct IncludeDirs {
    #[returns(ref)]
    include_dirs: Vec<camino::Utf8PathBuf>,
}

/// Dependencies of a document.
#[salsa::tracked]
pub struct DocumentDeps<'db> {
    #[tracked]
    #[returns(ref)]
    pub included_files: Vec<(TextRange, super::file::File)>,

    #[tracked]
    #[returns(ref)]
    pub diagnostics: Vec<Diagnostic>,
}

// TODO: per-AnalyzedInclude??
/// Dependencies of a document.
///
/// # Panics
///
/// Will panic if [`IncludeDirs`] hasn't been defined.
#[salsa::tracked]
pub fn document_deps(
    db: &dyn BaseDb,
    file: super::file::File,
) -> Result<DocumentDeps<'_>, ()> {
    let mut included_files = Vec::new();
    let mut diagnostics = Vec::new();

    let parent_path = file.path(db).parent().ok_or(())?;
    let outline = super::outline(db, file).ok_or(())?;

    // FIXME: actual config singleton!
    let include_dirs = IncludeDirs::get(db).include_dirs(db);

    let files = db.get_files();
    for include in outline
        .toplevels(db)
        .iter()
        .flat_map(AnalyzedToplevel::flatten_conditionals)
        .filter_map(AnalyzedToplevel::as_include)
    {
        if let Some(file) = include
            .possible_paths_utf8(parent_path, include_dirs)
            .map(|path| files.get_file(db, &path))
            .find(|file| file.is_readable_file(db))
        {
            included_files.push((include.text_range, file));
        } else {
            diagnostics.push(Diagnostic::new(
                include.text_range,
                Cow::Borrowed("Couldn't find file to include"),
                Severity::Error,
            ));
        };
    }

    super::tag_diagnostics(&mut diagnostics, concat!(module_path!(), "::document_deps"));

    Ok(DocumentDeps::new(db, included_files, diagnostics))
}
#[cfg(test)]
mod tests {
    use camino::Utf8Path;

    use crate::salsa::db::BaseDb;

    use super::*;

    #[test]
    fn test_document_deps() {
        let db = crate::salsa::db::BaseDatabase::default();
        IncludeDirs::new(&db, vec![]);

        let file_path = Utf8Path::new(env!("CARGO_MANIFEST_DIR")).join("test_data/including.dts");
        let file = db.get_files().get_file(&db, &file_path);

        let document_deps =
            document_deps(&db, file).expect("File should be readable and have a parent");

        assert_eq!(document_deps.included_files(&db).len(), 1);

        let diagnostics = document_deps.diagnostics(&db);
        assert_eq!(
            diagnostics.len(),
            2,
            "diagnostics are not as expected: {diagnostics:#?}"
        );
    }
}
