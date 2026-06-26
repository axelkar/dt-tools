use std::borrow::Cow;

use dt_tools_analyzer::new::outline::AnalyzedToplevel;
use dt_tools_diagnostic::{Diagnostic, DiagnosticCollector, MultiSpan, Severity, Span, SpanLabel};
use dt_tools_parser::{TextRange, ast::AstNode};

pub mod db;
mod expr_eval;
pub mod file;
pub mod includes;
pub mod macros;
pub mod mir;
pub mod preprocessor;

#[salsa::tracked]
pub struct Parse<'db> {
    #[returns(ref)]
    #[no_eq]
    pub parse: dt_tools_parser::parser::Parse<'static>,
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

pub fn emit_parse_errors(
    parse: &dt_tools_parser::parser::Parse,
    diag: &impl DiagnosticCollector<file::File>,
    spanner: &mut impl FnMut(TextRange) -> Span<file::File>,
) -> bool {
    if parse.lex_errors.is_empty() && parse.errors.is_empty() {
        return false;
    }

    let earliest_lex_error_range = parse.lex_errors.first().map(|e| e.text_range);
    for lex_error in &parse.lex_errors {
        diag.emit(Diagnostic::new(
            spanner(lex_error.text_range),
            format!("{} [lex error]", lex_error.inner).into(),
            Severity::Error,
        ));
    }

    for error in &parse.errors {
        if let Some(earliest_lex_error_range) = earliest_lex_error_range
            && error.primary_text_range.start >= earliest_lex_error_range.start
        {
            break;
        }

        let mut diagnostic = Diagnostic {
            span: MultiSpan {
                primary_spans: vec![spanner(error.primary_text_range)],
                span_labels: error
                    .span_labels
                    .iter()
                    .map(|(text_range, msg)| SpanLabel {
                        span: spanner(*text_range),
                        msg: msg.clone(),
                    })
                    .collect(),
            },
            msg: error.message.clone(),
            severity: Severity::Error,
        };
        tag_diagnostic(&mut diagnostic, "dt-tools(syntax-error)");
        diag.emit(diagnostic);
    }

    false
}

#[salsa::tracked(returns(ref))]
pub fn compute_diagnostics<'db>(
    db: &'db dyn db::BaseDb,
    main_file: file::File,
) -> (Vec<Diagnostic<file::File>>, Vec<file::File>) {
    let span = profiling::tracy_client::span!("lsp::salsa::compute_file_diagnostics");
    span.emit_text(main_file.path(db).as_str());

    let mut diagnostics = Vec::new();

    if !main_file.is_readable_file(db) {
        // TODO: test this with LSP. Getting rope will probably panic
        return (
            vec![Diagnostic::new(
                dt_tools_parser::TextRange { start: 0, end: 0 }.within_file(main_file),
                Cow::Borrowed("File does not exist"),
                Severity::Error,
            )],
            vec![main_file],
        );
    }

    if let Some(parse) = parse_file(db, main_file) {
        let parse = parse.parse(db);

        let diag = parking_lot::Mutex::new(&mut diagnostics);
        emit_parse_errors(parse, &diag, &mut |text_range| {
            text_range.within_file(main_file)
        });

        // FIXME: main file detection
        let is_main_file = false;
        for mut lint in dt_tools_lint::default_lint(
            &parse.source_file(),
            main_file.contents(db),
            is_main_file,
            main_file,
        ) {
            tag_diagnostic(&mut lint.diagnostic, &format!("dt-tools(lint {})", lint.id));
            diagnostics.push(lint.diagnostic);
        }
    }

    // TODO: re-enable? (though preprocessor_eval_file (also TODO: rename it) will probably
    // supersede the functionality here)
    /*
    if let Some(outline) = outline(db, file) {
        diagnostics.extend_from_slice(outline.diagnostics(db));
    }

    if let Ok(document_deps) = includes::document_deps(db, file) {
        diagnostics.extend_from_slice(document_deps.diagnostics(db));
    }
    */

    // Detect /plugin/; in the root file.
    let is_overlay = parse_file(db, main_file).is_some_and(|p| {
        p.parse(db).source_file().directives().any(|dir| {
            dir.syntax()
                .child_tokens()
                .any(|tok| tok.green.kind == dt_tools_parser::lexer::TokenKind::PluginDirective)
        })
    });

    let mut included_files = if let Some(result) =
        preprocessor::preprocessor_eval_file(db, main_file, None, is_overlay)
    {
        diagnostics.extend_from_slice(result.diagnostics(db));
        result.included_files(db).clone()
    } else {
        Vec::new()
    };

    diagnostics.dedup();
    included_files.dedup();
    (diagnostics, included_files)
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
