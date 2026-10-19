use std::borrow::Cow;

use dt_tools_diagnostic::{Diagnostic, MultiSpan, Severity, SpanLabel};

use crate::expand::{
    expand_root, expanded_syntax_diagnostics, preprocessor_diagnostics, resolve_diagnostic,
};

mod check_mir_post;
pub mod codespan_reporting;
pub mod db;
mod diag;
pub mod expand;
mod expr_eval;
mod extra_num_traits;
pub mod file;
pub mod includes;
pub mod lowering;
pub mod mir;

#[salsa::tracked]
pub struct Parse<'db> {
    #[returns(ref)]
    #[no_eq]
    pub parse: dt_tools_parser::parser::Parse<'db>,
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
    profiling::scope!("lowering::parse_file", file.path(db).as_str());

    if !file.is_readable_file(db) {
        return None;
    }

    let contents: &str = file.contents(db);
    let parse = dt_tools_parser::parser::Entrypoint::SourceFile.parse(contents);
    Some(Parse::new(db, parse))
}

pub(crate) fn tag_diagnostics<F>(diagnostics: &mut [Diagnostic<F>], tag: &str) {
    for diagnostic in diagnostics {
        tag_diagnostic(diagnostic, tag);
    }
}

pub(crate) fn tag_diagnostic<F>(diagnostic: &mut Diagnostic<F>, tag: &str) {
    use std::fmt::Write;

    write!(diagnostic.msg.to_mut(), " [{tag}]").ok();
    for span_label in &mut diagnostic.span.span_labels {
        write!(span_label.msg.to_mut(), " [{tag}]").ok();
    }
}

/// Converts a raw parse's errors to diagnostics for `file`.
pub fn emit_parse_errors(
    parse: &dt_tools_parser::parser::Parse,
    sink: &mut Vec<Diagnostic<file::File>>,
    file: file::File,
) -> bool {
    if parse.lex_errors.is_empty() && parse.errors.is_empty() {
        return false;
    }

    let earliest_lex_error_range = parse.lex_errors.first().map(|e| e.text_range);
    for lex_error in &parse.lex_errors {
        sink.push(Diagnostic {
            span: MultiSpan::from(lex_error.text_range.within_file(file)),
            msg: format!("{} [lex error]", lex_error.inner).into(),
            severity: Severity::Error,
        });
    }

    for error in &parse.errors {
        if let Some(earliest_lex_error_range) = earliest_lex_error_range
            && error.primary_text_range.start >= earliest_lex_error_range.start
        {
            break;
        }

        let mut diagnostic = Diagnostic {
            span: MultiSpan {
                primary_spans: vec![error.primary_text_range.within_file(file)],
                span_labels: error
                    .span_labels
                    .iter()
                    .map(|(text_range, msg)| SpanLabel {
                        span: (*text_range).within_file(file),
                        msg: msg.clone(),
                    })
                    .collect(),
            },
            msg: error.message.clone(),
            severity: Severity::Error,
        };
        tag_diagnostic(&mut diagnostic, "dt-tools(syntax-error)");
        sink.push(diagnostic);
    }

    false
}

#[salsa::tracked(returns(ref), lru = 64)]
pub fn compute_diagnostics(
    db: &dyn db::BaseDb,
    root_file: file::File,
) -> (Vec<Diagnostic<file::File>>, Vec<file::File>) {
    profiling::scope!(
        "lowering::compute_file_diagnostics",
        root_file.path(db).as_str()
    );

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

    let mut processed_files = if let Some(expanded) = expand_root(db, root_file).as_ref() {
        diagnostics.extend(preprocessor_diagnostics(db, expanded));
        diagnostics.extend(expanded_syntax_diagnostics(db, root_file));

        if let Some(result) = lowering::lower_root_file(db, root_file) {
            diagnostics.extend(
                result
                    .diagnostics(db)
                    .iter()
                    .map(|diagnostic| resolve_diagnostic(db, expanded, diagnostic)),
            );

            diagnostics.extend(
                check_mir_post::check_mir_post(db, result)
                    .iter()
                    .map(|diagnostic| resolve_diagnostic(db, expanded, diagnostic)),
            );

            result.processed_files(db).clone()
        } else {
            Vec::new()
        }
    } else {
        Vec::new()
    };

    processed_files.push(root_file);
    processed_files.dedup();

    for file in &processed_files {
        if let Some(parse) = parse_file(db, *file) {
            let parse = parse.parse(db);

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
}
