use std::borrow::Cow;

use camino::Utf8Path;
use dt_tools_diagnostic::Span;
use dt_tools_lowering::file::File;
use dt_tools_parser::TextRange;
use fluent_uri::component::Scheme;
use ropey::Rope;
use tower_lsp_server::ls_types::{self, Position, Range, Uri};

pub fn position_to_offset(position: Position, rope: &Rope) -> Option<usize> {
    Some(rope.try_line_to_char(position.line as usize).ok()? + position.character as usize)
}

pub fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_byte_to_line(offset).ok()?;
    let first_char_of_line = rope.try_line_to_byte(line).ok()?;
    let column = offset - first_char_of_line;
    Some(Position::new(
        u32::try_from(line).ok()?,
        u32::try_from(column).ok()?,
    ))
}

pub fn range_to_lsp(text_range: TextRange, rope: &Rope) -> Option<Range> {
    Some(Range {
        start: offset_to_position(text_range.start, rope)?,
        end: offset_to_position(text_range.end, rope)?,
    })
}

pub fn uri_to_path(uri: &Uri) -> Option<Cow<'_, Utf8Path>> {
    const SCHEME_FILE: &Scheme = Scheme::new_or_panic("file");

    if uri.scheme() != SCHEME_FILE {
        return None;
    }

    let path = uri.to_file_path()?;
    let path = match path {
        Cow::Owned(path) => Cow::Owned(
            camino::Utf8PathBuf::from_path_buf(path)
                .expect("to_file_path's implementation always guarantees valid UTF-8"),
        ),
        Cow::Borrowed(path) => Cow::Borrowed(
            camino::Utf8Path::from_path(path)
                .expect("to_file_path's implementation always guarantees valid UTF-8"),
        ),
    };
    Some(path)
}

/// Returns `None` if the path is relative and inaccessible.
pub fn path_to_uri(path: &Utf8Path) -> Option<Uri> {
    // We can create URIs outself because other LSPs like rust-analyzer do it as well.
    Uri::from_file_path(path)
}

/// Returns the URI for a [`File`].
pub fn file_uri(db: &dyn dt_tools_lowering::db::BaseDb, file: File) -> Uri {
    path_to_uri(file.path(db)).expect("path should be absolute (checked in File constructors)")
}

/// Converts a [`dt_tools_diagnostic::Span`] to a [`ls_types::Location`].
pub fn span_to_location(
    db: &dyn dt_tools_lowering::db::BaseDb,
    span: &Span<File>,
) -> ls_types::Location {
    ls_types::Location {
        uri: file_uri(db, span.file),
        range: range_to_lsp(
            span.text_range,
            dt_tools_lowering::rope(db, span.file)
                .as_ref()
                .expect("file should exist"),
        )
        .expect("range should be in the rope"),
    }
}

/// Converts a [`dt_tools_diagnostic::Diagnostic`] to one or more [`ls_types::Diagnostic`]s.
///
/// `file_filter` exists because [`ls_types::Diagnostic`] does not contain a field for file
/// information; this must be run separately for every file.
#[expect(clippy::ref_option, reason = "Fixes borrow checker issues")]
pub fn dt_tools_diagnostic_to_lsp<'a>(
    db: &'a dyn dt_tools_lowering::db::BaseDb,
    diagnostic: &'a dt_tools_diagnostic::Diagnostic<File>,
    source: &'a Option<String>,
    file_filter: &'a File,
) -> impl Iterator<Item = ls_types::Diagnostic> + 'a {
    let rope = dt_tools_lowering::rope(db, *file_filter)
        .as_ref()
        .expect("file should exist");

    diagnostic
        .span
        .primary_spans
        .iter()
        .filter(|span| span.file == *file_filter)
        .enumerate()
        .map(move |(i, span)| ls_types::Diagnostic {
            range: range_to_lsp(span.text_range, rope).expect("range should be in the rope"),
            severity: Some(match diagnostic.severity {
                dt_tools_diagnostic::Severity::Warn => ls_types::DiagnosticSeverity::WARNING,
                dt_tools_diagnostic::Severity::Error => ls_types::DiagnosticSeverity::ERROR,
            }),
            source: source.clone(),
            message: diagnostic.msg.clone().into_owned(),
            related_information: Some(
                diagnostic
                    .span
                    .primary_spans
                    .iter()
                    .enumerate()
                    .filter(|(j, _)| *j != i)
                    .map(|(_, span)| ls_types::DiagnosticRelatedInformation {
                        // link to other primary spans
                        location: span_to_location(db, span),
                        message: "see also".to_owned(),
                    })
                    .chain(diagnostic.span.span_labels.iter().map(|span_label| {
                        ls_types::DiagnosticRelatedInformation {
                            // link to span labels
                            location: span_to_location(db, &span_label.span),
                            message: span_label.msg.clone().into_owned(),
                        }
                    }))
                    .collect(),
            ),
            ..Default::default()
        })
        .chain(
            diagnostic
                .span
                .span_labels
                .iter()
                .filter(|span_label| span_label.span.file == *file_filter)
                .map(move |span_label| {
                    ls_types::Diagnostic {
                        range: range_to_lsp(span_label.span.text_range, rope)
                            .expect("range should be in the rope"),
                        severity: Some(ls_types::DiagnosticSeverity::HINT),
                        source: source.clone(),
                        message: span_label.msg.clone().into_owned(),
                        related_information: Some(
                            diagnostic
                                .span
                                .primary_spans
                                .iter()
                                .map(|span| ls_types::DiagnosticRelatedInformation {
                                    // link to primary spans
                                    location: span_to_location(db, span),
                                    message: "original diagnostic".to_owned(),
                                })
                                .collect(),
                        ),
                        ..Default::default()
                    }
                }),
        )
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_range_to_lsp_trailing_newline() {
        let rope = ropey::Rope::from_str("\n");
        assert_eq!(
            range_to_lsp(TextRange { start: 0, end: 1 }, &rope),
            Some(Range {
                start: Position {
                    line: 0,
                    character: 0
                },
                end: Position {
                    line: 1,
                    character: 0
                },
            })
        );
        assert_eq!(range_to_lsp(TextRange { start: 0, end: 2 }, &rope), None);

        // TODO: make length>0 diagnostics in an empty document work
        // Let's hope fully empty files (e.g. created through VS Code or Windows) don't get
        // diagnostics with nonzero length (invalid according to LSP spec? or?). Not sure how rust-analyzer handles this.
        let rope = ropey::Rope::from_str("");
        assert_eq!(range_to_lsp(TextRange { start: 0, end: 1 }, &rope), None);

        assert_eq!(
            range_to_lsp(TextRange { start: 0, end: 0 }, &rope),
            Some(Range {
                start: Position {
                    line: 0,
                    character: 0
                },
                end: Position {
                    line: 0,
                    character: 0
                },
            })
        );
    }
}
