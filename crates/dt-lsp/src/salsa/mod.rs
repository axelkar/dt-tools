use std::borrow::Cow;

use dt_analyzer::new::outline::AnalyzedToplevel;
use dt_diagnostic::{Diagnostic, Severity};

pub mod db;
pub mod file;
mod macros;

#[salsa::tracked]
pub struct Parse<'db> {
    #[returns(ref)]
    pub parse: dt_parser::parser::Parse<'static>,
}

// `no_eq`: Probably faster to compare the inputs than the whole AST
/// Returns `None` if the file doesn't exist.
#[salsa::tracked(no_eq)]
pub fn parse_file(db: &dyn db::BaseDb, file: file::File) -> Option<Parse<'_>> {
    if !file.is_readable_file(db) {
        return None;
    }

    let contents: &str = file.contents(db);
    let parse = dt_parser::parser::parse(contents).into_static();
    Some(Parse::new(db, parse))
}

#[salsa::tracked]
pub struct Outline<'db> {
    #[tracked]
    #[returns(ref)]
    pub toplevels: Vec<AnalyzedToplevel>,

    #[tracked]
    #[returns(ref)]
    pub diagnostics: Vec<Diagnostic>,
}

// `no_eq`: Probably faster to compare the inputs than the whole AST
/// Computes an outline composed of [`dt_analyzer::new::outline::AnalyzedToplevel`]s.
///
/// Returns `None` if the file doesn't exist.
#[salsa::tracked(no_eq)]
pub fn outline(db: &dyn db::BaseDb, file: file::File) -> Option<Outline<'_>> {
    // Parse always changes when file contents change, so I think (axka, 2026-03-23) it's okay to
    // call the parser in here.
    let parse = parse_file(db, file)?;
    let file_ast = parse.parse(db).source_file();

    let mut diagnostics = Vec::new();
    let diag = std::sync::Mutex::new(&mut diagnostics);

    let toplevels = dt_analyzer::new::outline::analyze_file(&file_ast, file.contents(db), &diag);

    tag_diagnostics(&mut diagnostics, concat!(module_path!(), "::outline"));

    Some(Outline::new(db, toplevels, diagnostics))
}

pub fn tag_diagnostics(diagnostics: &mut Vec<Diagnostic>, tag: &str) {
    use std::fmt::Write;

    for diagnostic in diagnostics {
        write!(diagnostic.msg.to_mut(), " ({tag})").ok();
        for span_label in &mut diagnostic.span.span_labels {
            write!(span_label.msg.to_mut(), " ({tag})").ok();
        }
    }
}

/// Dependencies of a document.
#[salsa::tracked]
pub struct DocumentDeps<'db> {
    #[tracked]
    #[returns(ref)]
    pub included_files: Vec<file::File>,

    #[tracked]
    #[returns(ref)]
    pub diagnostics: Vec<Diagnostic>,
}

// TODO: per-AnalyzedInclude??
/// Dependencies of a document.
#[salsa::tracked]
pub fn document_deps<'db>(
    db: &'db dyn db::BaseDb,
    file: file::File,
    outline: Outline<'db>,
) -> Result<DocumentDeps<'db>, ()> {
    let mut included_files = Vec::new();
    let mut diagnostics = Vec::new();

    let parent_path = file.path(db).parent().ok_or(())?;
    // FIXME: actual config singleton!
    let include_dirs: &[&camino::Utf8Path] = &[];

    let files = db.get_files();
    for include in outline
        .toplevels(db)
        .iter()
        .filter_map(AnalyzedToplevel::as_include)
    {
        for path in include.possible_paths_utf8(parent_path, include_dirs) {
            let file = files.get_file(db, path);
            included_files.push(file);

            if !file.is_readable_file(db) {
                diagnostics.push(Diagnostic::new(
                    include.text_range,
                    Cow::Borrowed("Couldn't find file to include"),
                    Severity::Error,
                ));
            }
        }
    }

    tag_diagnostics(&mut diagnostics, concat!(module_path!(), "::document_deps"));

    Ok(DocumentDeps::new(db, included_files, diagnostics))
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;

    use crate::salsa::db::BaseDb;

    use super::*;

    #[test]
    fn test_parse_file() {
        let db = crate::salsa::db::BaseDatabase::default();
        let file_path = Utf8PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/basic.dts");
        let file = db.get_files().get_file(&db, file_path);

        let parse = parse_file(&db, file).expect("Should be a readable file");
        let parse = parse.parse(&db);

        assert_eq!(&parse.lex_errors, &[]);
        assert_eq!(&parse.errors, &[]);
    }

    #[test]
    fn test_outline() {
        let db = crate::salsa::db::BaseDatabase::default();
        let file_path =
            Utf8PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/including.dts");
        let file = db.get_files().get_file(&db, file_path);

        let outline = outline(&db, file).expect("Should be a readable file");

        assert_eq!(outline.diagnostics(&db), &[]);
        let toplevels = outline.toplevels(&db);
        assert!(
            matches!(
                toplevels.as_slice(),
                &[AnalyzedToplevel::Include(_), AnalyzedToplevel::Include(_)]
            ),
            "toplevels are not as expected: {toplevels:#?}"
        );
    }

    #[test]
    fn test_document_deps() {
        let db = crate::salsa::db::BaseDatabase::default();
        let file_path =
            Utf8PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data/including.dts");
        let file = db.get_files().get_file(&db, file_path);

        let outline = outline(&db, file).expect("Should be a readable file");

        let document_deps = document_deps(&db, file, outline).expect("File should have a parent");
        assert_eq!(document_deps.included_files(&db).len(), 2);

        let diagnostics = document_deps.diagnostics(&db);
        assert_eq!(
            diagnostics.len(),
            1,
            "diagnostics are not as expected: {diagnostics:#?}"
        );
    }
}
