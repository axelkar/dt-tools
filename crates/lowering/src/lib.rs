use std::borrow::Cow;

use dt_tools_analyzer::new::outline::AnalyzedToplevel;
use dt_tools_diagnostic::{Diagnostic, MultiSpan, Severity, SpanLabel};

use crate::diag::{Diag, SourceMap};

pub mod codespan_reporting;
pub mod db;
pub mod diag;
mod expr_eval;
pub mod file;
// TODO: remove includes / document_deps in favor of new lowering stuff
mod check_mir_post;
mod extra_num_traits;
pub mod includes;
pub mod lowering;
pub mod macros;
pub mod mir;

#[salsa::tracked]
pub struct Parse<'db> {
    #[returns(ref)]
    #[no_eq]
    pub parse: dt_tools_parser::parser::Parse<'static>,
}

// `no_eq`: Always changes when `file.contents` changes
/// Returns `None` if the file doesn't exist.
#[salsa::tracked(no_eq, returns(ref), lru = 64)]
pub fn rope(db: &dyn db::BaseDb, file: file::File) -> Option<ropey::Rope> {
    if !file.is_readable_file(db) {
        return None;
    }
    Some(ropey::Rope::from_str(file.contents(db)))
}

// `no_eq`: Probably faster to compare the inputs than the whole AST
/// Returns `None` if the file doesn't exist.
#[salsa::tracked(no_eq, lru = 64)]
pub fn parse_file(db: &dyn db::BaseDb, file: file::File) -> Option<Parse<'_>> {
    let span = profiling::tracy_client::span!("lsp::salsa::parse_file");
    span.emit_text(file.path(db).as_str());

    if !file.is_readable_file(db) {
        return None;
    }

    let contents: &str = file.contents(db);
    let parse = dt_tools_parser::parser::parse(contents).into_static();
    Some(Parse::new(db, parse))
}

#[salsa::tracked]
pub struct Outline<'db> {
    #[tracked]
    #[returns(ref)]
    pub toplevels: Vec<AnalyzedToplevel>,

    #[tracked]
    #[returns(ref)]
    pub diagnostics: Vec<Diagnostic<file::File>>,
}

// TODO: remove outline in favor of new lowering stuff
/// Computes an outline composed of [`dt_tools_analyzer::new::outline::AnalyzedToplevel`]s.
///
/// Returns `None` if the file doesn't exist.
#[salsa::tracked]
pub fn outline(db: &dyn db::BaseDb, file: file::File) -> Option<Outline<'_>> {
    let parse = parse_file(db, file)?;
    let file_ast = parse.parse(db).source_file();

    let mut diagnostics = Vec::new();
    let diag = parking_lot::Mutex::new(&mut diagnostics);

    let toplevels =
        dt_tools_analyzer::new::outline::analyze_file(&file_ast, file.contents(db), file, &diag);

    tag_diagnostics(&mut diagnostics, concat!(module_path!(), "::outline"));

    Some(Outline::new(db, toplevels, diagnostics))
}

fn tag_diagnostics<F>(diagnostics: &mut Vec<Diagnostic<F>>, tag: &str) {
    for diagnostic in diagnostics {
        tag_diagnostic(diagnostic, tag);
    }
}
fn tag_diagnostic<F>(diagnostic: &mut Diagnostic<F>, tag: &str) {
    use std::fmt::Write;

    write!(diagnostic.msg.to_mut(), " [{tag}]").ok();
    for span_label in &mut diagnostic.span.span_labels {
        write!(span_label.msg.to_mut(), " [{tag}]").ok();
    }
}

pub fn emit_parse_errors(parse: &dt_tools_parser::parser::Parse, diag: &mut Diag<'_, '_>) -> bool {
    if parse.lex_errors.is_empty() && parse.errors.is_empty() {
        return false;
    }

    let earliest_lex_error_range = parse.lex_errors.first().map(|e| e.text_range);
    for lex_error in &parse.lex_errors {
        diag.emit(
            lex_error.text_range,
            format!("{} [lex error]", lex_error.inner),
            Severity::Error,
        );
    }

    for error in &parse.errors {
        if let Some(earliest_lex_error_range) = earliest_lex_error_range
            && error.primary_text_range.start >= earliest_lex_error_range.start
        {
            break;
        }

        let mut diagnostic = Diagnostic {
            span: MultiSpan {
                primary_spans: vec![diag.resolve(error.primary_text_range)],
                span_labels: error
                    .span_labels
                    .iter()
                    .map(|(text_range, msg)| SpanLabel {
                        span: diag.resolve(*text_range),
                        msg: msg.clone(),
                    })
                    .collect(),
            },
            msg: error.message.clone(),
            severity: Severity::Error,
        };
        tag_diagnostic(&mut diagnostic, "dt-tools(syntax-error)");
        diag.push(diagnostic);
    }

    false
}

#[salsa::tracked(returns(ref), lru = 64)]
pub fn compute_diagnostics(
    db: &dyn db::BaseDb,
    root_file: file::File,
) -> (Vec<Diagnostic<file::File>>, Vec<file::File>) {
    let span = profiling::tracy_client::span!("lsp::salsa::compute_file_diagnostics");
    span.emit_text(root_file.path(db).as_str());

    let mut diagnostics = Vec::new();

    if !root_file.is_readable_file(db) {
        // TODO: test this with LSP. Getting rope will probably panic
        return (
            vec![Diagnostic::new(
                dt_tools_parser::TextRange { start: 0, end: 0 }.within_file(root_file),
                Cow::Borrowed("File does not exist"),
                Severity::Error,
            )],
            vec![root_file],
        );
    }

    let mut processed_files = if let Some(result) = lowering::lower_root_file(db, root_file) {
        diagnostics.extend_from_slice(result.diagnostics(db));

        diagnostics.extend_from_slice(check_mir_post::check_mir_post(db, result));

        result.processed_files(db).clone()
    } else {
        Vec::new()
    };

    processed_files.push(root_file);
    processed_files.dedup();

    for file in &processed_files {
        if let Some(parse) = parse_file(db, *file) {
            let parse = parse.parse(db);

            let map = SourceMap::File(*file);
            emit_parse_errors(parse, &mut Diag::new(&mut diagnostics, &map));

            for mut lint in
                dt_tools_lint::default_lint(&parse.source_file(), file.contents(db), *file)
            {
                tag_diagnostic(&mut lint.diagnostic, &format!("dt-tools(lint {})", lint.id));
                diagnostics.push(lint.diagnostic);
            }
        }
    }

    diagnostics.dedup();
    (diagnostics, processed_files)
}

#[cfg(test)]
mod tests {
    use camino::Utf8Path;

    use super::*;
    use crate::db::BaseDb;

    #[test]
    fn test_parse_file() {
        let db = crate::db::BaseDatabase::default();
        let file_path = Utf8Path::new(env!("CARGO_MANIFEST_DIR")).join("test_data/basic.dts");
        let file = db.get_files().get_file(&db, &file_path);

        let parse = parse_file(&db, file).expect("Should be a readable file");
        let parse = parse.parse(&db);

        assert_eq!(&parse.lex_errors, &[]);
        assert_eq!(&parse.errors, &[]);
    }

    #[test]
    fn test_outline() {
        let db = crate::db::BaseDatabase::default();
        let file_path = Utf8Path::new(env!("CARGO_MANIFEST_DIR")).join("test_data/including.dts");
        let file = db.get_files().get_file(&db, &file_path);

        let outline = outline(&db, file).expect("Should be a readable file");

        assert_eq!(outline.diagnostics(&db), &[]);
        let toplevels = outline.toplevels(&db);
        assert!(
            matches!(
                toplevels.as_slice(),
                &[
                    AnalyzedToplevel::Include(_),
                    AnalyzedToplevel::PreprocessorConditional { .. },
                    AnalyzedToplevel::Include(_)
                ]
            ),
            "toplevels are not as expected: {toplevels:#?}"
        );
    }
}
