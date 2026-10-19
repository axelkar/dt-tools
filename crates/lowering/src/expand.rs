//! Expanded-file Salsa queries.

use dt_tools_diagnostic::{
    Diagnostic, MultiSpan, Severity, Span, SpanLabel, text_range::TextRange,
};
pub use dt_tools_preprocessor::PreprocessedRoot;
use dt_tools_preprocessor::{PreprocessorConfig, SourceId, preprocess};

use crate::{Parse, db::BaseDb, file::File, includes::IncludeDirs};

/// Identifies ranges in a root file's expanded text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExpandedText;

/// Returns the preprocessed text and provenance of `root`.
///
/// Returns `None` if the file doesn't exist.
#[salsa::tracked(returns(ref), lru = 32)]
pub fn expand_root(db: &dyn BaseDb, root: File) -> Option<PreprocessedRoot> {
    profiling::scope!("lowering::expand_root", root.path(db).as_str());

    if !root.is_readable_file(db) {
        return None;
    }

    let files = db.get_files();
    let config = PreprocessorConfig {
        include_dirs: IncludeDirs::get(db).include_dirs(db).clone(),
        ..PreprocessorConfig::default()
    };

    Some(preprocess(root.path(db), &config, |path| {
        let file = files.get_file(db, path);
        file.is_readable_file(db)
            .then(|| file.contents(db).to_owned())
    }))
}

/// Parses the expanded text of `root`.
///
/// Returns `None` if the file doesn't exist.
#[salsa::tracked(no_eq, lru = 64)]
pub fn expanded_parse(db: &dyn BaseDb, root: File) -> Option<Parse<'_>> {
    let expanded = expand_root(db, root).as_ref()?;
    let parse = dt_tools_parser::parser::Entrypoint::SourceFile.parse(&expanded.text);
    Some(Parse::new(db, parse))
}

/// Returns the file for a preprocessor source id.
pub fn source_file(db: &dyn BaseDb, expanded: &PreprocessedRoot, source: SourceId) -> File {
    db.get_files().get_file(db, &expanded.source(source).path)
}

/// Converts a preprocessor span to a file span.
pub fn resolve_span(
    db: &dyn BaseDb,
    expanded: &PreprocessedRoot,
    span: Span<SourceId>,
) -> Span<File> {
    Span {
        file: source_file(db, expanded, span.file),
        text_range: span.text_range,
    }
}

/// Resolves a range in the expanded text to file spans.
pub fn resolve_range(
    db: &dyn BaseDb,
    expanded: &PreprocessedRoot,
    range: TextRange,
) -> MultiSpan<File> {
    let resolved = expanded.resolve(range);
    MultiSpan {
        primary_spans: resolved
            .primary_spans
            .iter()
            .map(|span| resolve_span(db, expanded, *span))
            .collect(),
        span_labels: resolved
            .span_labels
            .iter()
            .map(|label| SpanLabel {
                span: resolve_span(db, expanded, label.span),
                msg: label.msg.clone(),
            })
            .collect(),
    }
}

/// Converts a diagnostic in expanded-text coordinates to file spans.
pub fn resolve_diagnostic(
    db: &dyn BaseDb,
    expanded: &PreprocessedRoot,
    diagnostic: &Diagnostic<ExpandedText>,
) -> Diagnostic<File> {
    let mut span = MultiSpan::empty();

    for raw_span in &diagnostic.span.primary_spans {
        let resolved = resolve_range(db, expanded, raw_span.text_range);
        span.primary_spans.extend(resolved.primary_spans);
        span.span_labels.extend(resolved.span_labels);
    }

    for label in &diagnostic.span.span_labels {
        let resolved = resolve_range(db, expanded, label.span.text_range);
        if let Some(file_span) = resolved.primary_spans.into_iter().next() {
            span.span_labels.push(SpanLabel {
                span: file_span,
                msg: label.msg.clone(),
            });
        }
    }

    Diagnostic {
        span,
        msg: diagnostic.msg.clone(),
        severity: diagnostic.severity,
    }
}

/// Returns the preprocessor diagnostics with file spans.
pub fn preprocessor_diagnostics(
    db: &dyn BaseDb,
    expanded: &PreprocessedRoot,
) -> Vec<Diagnostic<File>> {
    expanded
        .diagnostics
        .iter()
        .map(|diagnostic| Diagnostic {
            span: MultiSpan {
                primary_spans: diagnostic
                    .span
                    .primary_spans
                    .iter()
                    .map(|span| resolve_span(db, expanded, *span))
                    .collect(),
                span_labels: diagnostic
                    .span
                    .span_labels
                    .iter()
                    .map(|label| SpanLabel {
                        span: resolve_span(db, expanded, label.span),
                        msg: label.msg.clone(),
                    })
                    .collect(),
            },
            msg: diagnostic.msg.clone(),
            severity: diagnostic.severity,
        })
        .collect()
}

/// Returns the syntax diagnostics of `root`'s expanded text with file spans.
pub fn expanded_syntax_diagnostics(db: &dyn BaseDb, root: File) -> Vec<Diagnostic<File>> {
    let Some(expanded) = expand_root(db, root).as_ref() else {
        return Vec::new();
    };
    let Some(parse) = expanded_parse(db, root) else {
        return Vec::new();
    };

    parse_diagnostics(db, expanded, parse.parse(db))
}

/// Converts an expanded parse's errors to file diagnostics.
fn parse_diagnostics(
    db: &dyn BaseDb,
    expanded: &PreprocessedRoot,
    parse: &dt_tools_parser::parser::Parse<'_>,
) -> Vec<Diagnostic<File>> {
    let mut diagnostics = Vec::new();
    let earliest_lex_error = parse.lex_errors.first().map(|error| error.text_range);

    for lex_error in &parse.lex_errors {
        diagnostics.push(Diagnostic {
            span: resolve_range(db, expanded, lex_error.text_range),
            msg: format!("{} [lex error]", lex_error.inner).into(),
            severity: Severity::Error,
        });
    }

    for error in &parse.errors {
        if let Some(earliest) = earliest_lex_error
            && error.primary_text_range.start >= earliest.start
        {
            break;
        }

        let mut span = resolve_range(db, expanded, error.primary_text_range);
        span.span_labels
            .extend(error.span_labels.iter().filter_map(|(range, msg)| {
                let span = resolve_range(db, expanded, *range)
                    .primary_spans
                    .first()
                    .copied()?;
                Some(SpanLabel {
                    span,
                    msg: msg.clone(),
                })
            }));

        let mut diagnostic = Diagnostic {
            span,
            msg: error.message.clone(),
            severity: Severity::Error,
        };
        crate::tag_diagnostic(&mut diagnostic, "dt-tools(syntax-error)");
        diagnostics.push(diagnostic);
    }

    diagnostics
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;
    use dt_tools_diagnostic::Severity;

    use super::{
        expand_root, expanded_parse, expanded_syntax_diagnostics, preprocessor_diagnostics,
        source_file,
    };
    use crate::{
        db::{BaseDatabase, BaseDb},
        file::File,
        includes::IncludeDirs,
        parse_file,
    };

    fn virtual_file(db: &BaseDatabase, path: &str, contents: &str) -> File {
        db.get_files()
            .add_virtual(db, Utf8PathBuf::from(path), contents.to_owned())
    }

    #[test]
    fn expanded_parse_skips_inactive_branches() {
        let db = BaseDatabase::default();
        IncludeDirs::new(&db, Vec::new());
        let file = virtual_file(
            &db,
            "/main.dts",
            "#if 0\nthis is { not a node;\n#endif\n/ { compatible = \"test\"; };\n",
        );

        let raw = parse_file(&db, file).expect("readable");
        assert!(!raw.parse(&db).errors.is_empty());

        let expanded = expand_root(&db, file).as_ref().expect("readable");
        assert!(!expanded.text.contains("not a node"));

        let parse = expanded_parse(&db, file).expect("readable");
        let parse = parse.parse(&db);
        assert!(parse.errors.is_empty(), "{:?}", parse.errors);
        assert!(parse.lex_errors.is_empty());
    }

    #[test]
    fn expands_includes_before_parsing() {
        let db = BaseDatabase::default();
        IncludeDirs::new(&db, Vec::new());
        let main = virtual_file(&db, "/main.dts", "#include \"a.dtsi\"\n/ { x = <1>; };\n");
        virtual_file(&db, "/a.dtsi", "/ { y = <2>; };\n");

        let expanded = expand_root(&db, main).as_ref().expect("readable");
        assert!(expanded.text.contains("y = <2>"), "{:?}", expanded.text);

        assert_eq!(expanded.includes.len(), 1);
        let edge = &expanded.includes[0];
        assert_eq!(source_file(&db, expanded, edge.span.file), main);
        assert_eq!(source_file(&db, expanded, edge.file).path(&db), "/a.dtsi");

        let parse = expanded_parse(&db, main).expect("readable");
        let parse = parse.parse(&db);
        assert!(parse.errors.is_empty(), "{:?}", parse.errors);
    }

    #[test]
    fn reports_expanded_syntax_errors_with_file_spans() {
        let db = BaseDatabase::default();
        IncludeDirs::new(&db, Vec::new());
        let main = virtual_file(&db, "/main.dts", "/ { x = <1>;\n");

        let diagnostics = expanded_syntax_diagnostics(&db, main);
        assert!(!diagnostics.is_empty());
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert_eq!(diagnostics[0].span.primary_spans[0].file, main);
    }

    #[test]
    fn labels_macro_invocations() {
        let db = BaseDatabase::default();
        IncludeDirs::new(&db, Vec::new());
        let main = virtual_file(&db, "/main.dts", "#define BAD 1 +\n/ { x = <BAD>; };\n");

        let diagnostics = expanded_syntax_diagnostics(&db, main);
        assert!(
            diagnostics.iter().any(|diagnostic| {
                diagnostic
                    .span
                    .span_labels
                    .iter()
                    .any(|label| label.msg.contains("in this expansion of `BAD`"))
            }),
            "{diagnostics:#?}"
        );
    }

    #[test]
    fn converts_preprocessor_diagnostics_to_files() {
        let db = BaseDatabase::default();
        IncludeDirs::new(&db, Vec::new());
        let main = virtual_file(&db, "/main.dts", "#include \"missing.dtsi\"\n/ { };\n");

        let expanded = expand_root(&db, main).as_ref().expect("readable");
        let diagnostics = preprocessor_diagnostics(&db, expanded);

        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert_eq!(diagnostics[0].span.primary_spans[0].file, main);
    }
}
