use std::borrow::Cow;

use dt_analyzer::new::outline::AnalyzedToplevel;
use dt_diagnostic::{Diagnostic, MultiSpan, Severity};
use dt_parser::TextRange;

pub mod db;
pub mod file;
mod macros;

#[salsa::input(singleton)]
pub struct IncludeDirs {
    #[returns(ref)]
    include_dirs: Vec<camino::Utf8PathBuf>,
}

#[salsa::tracked]
pub struct Parse<'db> {
    // TODO: shouldn't this be no_eq as well like parse_file()??
    #[returns(ref)]
    pub parse: dt_parser::parser::Parse<'static>,
}

// `no_eq`: Always changes when `file.contents` changes
/// Returns `None` if the file doesn't exist.
#[salsa::tracked(no_eq, returns(ref))]
pub fn rope(db: &dyn db::BaseDb, file: file::File) -> Option<ropey::Rope> {
    if !file.is_readable_file(db) {
        return None;
    }
    Some(ropey::Rope::from_str(file.contents(db)))
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
    let parse = parse_file(db, file)?;
    let file_ast = parse.parse(db).source_file();

    let mut diagnostics = Vec::new();
    let diag = std::sync::Mutex::new(&mut diagnostics);

    let toplevels = dt_analyzer::new::outline::analyze_file(&file_ast, file.contents(db), &diag);

    tag_diagnostics(&mut diagnostics, concat!(module_path!(), "::outline"));

    Some(Outline::new(db, toplevels, diagnostics))
}

fn tag_diagnostics(diagnostics: &mut Vec<Diagnostic>, tag: &str) {
    for diagnostic in diagnostics {
        tag_diagnostic(diagnostic, tag);
    }
}
fn tag_diagnostic(diagnostic: &mut Diagnostic, tag: &str) {
    use std::fmt::Write;

    write!(diagnostic.msg.to_mut(), " [{tag}]").ok();
    for span_label in &mut diagnostic.span.span_labels {
        write!(span_label.msg.to_mut(), " [{tag}]").ok();
    }
}

/// Dependencies of a document.
#[salsa::tracked]
pub struct DocumentDeps<'db> {
    #[tracked]
    #[returns(ref)]
    pub included_files: Vec<(TextRange, file::File)>,

    #[tracked]
    #[returns(ref)]
    pub diagnostics: Vec<Diagnostic>,
}

// TODO: per-AnalyzedInclude??
/// Dependencies of a document.
#[salsa::tracked]
pub fn document_deps(
    db: &dyn db::BaseDb,
    file: file::File,
) -> Result<DocumentDeps<'_>, ()> {
    let mut included_files = Vec::new();
    let mut diagnostics = Vec::new();

    let parent_path = file.path(db).parent().ok_or(())?;
    let outline = outline(db, file).ok_or(())?;

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

    tag_diagnostics(&mut diagnostics, concat!(module_path!(), "::document_deps"));

    Ok(DocumentDeps::new(db, included_files, diagnostics))
}

#[salsa::tracked(returns(ref))]
pub fn compute_file_diagnostics<'db>(db: &'db dyn db::BaseDb, file: file::File) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    if !file.is_readable_file(db) {
        return vec![Diagnostic::new(
            dt_parser::TextRange { start: 0, end: 0 },
            Cow::Borrowed("File does not exist"),
            Severity::Error,
        )];
    }

    if let Some(parse) = parse_file(db, file) {
        let parse = parse.parse(db);

        let earliest_lex_error_range = parse.lex_errors.first().map(|e| e.text_range);
        for lex_error in &parse.lex_errors {
            diagnostics.push(Diagnostic::new(
                lex_error.text_range,
                format!("{} [lex error]", lex_error.inner).into(),
                Severity::Error,
            ));
        }

        for error in &parse.errors {
            if let Some(earliest_lex_error_range) = earliest_lex_error_range {
                if error.primary_span.start >= earliest_lex_error_range.start {
                    break;
                }
            }

            let mut diagnostic = Diagnostic {
                span: MultiSpan {
                    primary_spans: vec![error.primary_span],
                    span_labels: error.span_labels.clone(),
                },
                msg: error.message.clone(),
                severity: Severity::Error,
            };
            tag_diagnostic(&mut diagnostic, "dt-tools(syntax-error)");
            diagnostics.push(diagnostic);
        }

        // FIXME: main file detection
        let is_main_file = true;
        for mut lint in dt_lint::default_lint(&parse.source_file(), file.contents(db), is_main_file)
        {
            tag_diagnostic(&mut lint.diagnostic, &format!("dt-tools(lint {})", lint.id));
            diagnostics.push(lint.diagnostic);
        }
    }

    if let Some(outline) = outline(db, file) {
        diagnostics.extend_from_slice(outline.diagnostics(db));
    }

    if let Ok(document_deps) = document_deps(db, file) {
        diagnostics.extend_from_slice(document_deps.diagnostics(db));
    }

    diagnostics.dedup();
    diagnostics
}

#[cfg(test)]
mod tests {
    use camino::Utf8Path;

    use crate::salsa::db::BaseDb;

    use super::*;

    #[test]
    fn test_parse_file() {
        let db = crate::salsa::db::BaseDatabase::default();
        let file_path = Utf8Path::new(env!("CARGO_MANIFEST_DIR")).join("test_data/basic.dts");
        let file = db.get_files().get_file(&db, &file_path);

        let parse = parse_file(&db, file).expect("Should be a readable file");
        let parse = parse.parse(&db);

        assert_eq!(&parse.lex_errors, &[]);
        assert_eq!(&parse.errors, &[]);
    }

    #[test]
    fn test_outline() {
        let db = crate::salsa::db::BaseDatabase::default();
        let file_path = Utf8Path::new(env!("CARGO_MANIFEST_DIR")).join("test_data/including.dts");
        let file = db.get_files().get_file(&db, &file_path);

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
        IncludeDirs::new(&db, vec![]);

        let file_path = Utf8Path::new(env!("CARGO_MANIFEST_DIR")).join("test_data/including.dts");
        let file = db.get_files().get_file(&db, &file_path);

        let document_deps =
            document_deps(&db, file).expect("File should be readable and have a parent");

        assert_eq!(document_deps.included_files(&db).len(), 2);

        let diagnostics = document_deps.diagnostics(&db);
        assert_eq!(
            diagnostics.len(),
            1,
            "diagnostics are not as expected: {diagnostics:#?}"
        );
    }
}
