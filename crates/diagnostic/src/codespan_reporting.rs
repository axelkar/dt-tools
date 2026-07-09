use codespan_reporting::{
    diagnostic::Label,
    term::termcolor::{ColorChoice, StandardStream},
};

use crate::Severity;

/// Prints diagnostics of a single file using [`codespan_reporting`].
///
/// Note: [`Span::file`](crate::Span::file) is ignored.
#[expect(
    clippy::missing_errors_doc,
    reason = "Propagated from codespan_reporting, which doesn't document it at all."
)]
pub fn print_diagnostics_single_file<Name: std::fmt::Display + Clone, Source: AsRef<str>>(
    name: Name,
    source: Source,
    diagnostics: Vec<crate::Diagnostic<()>>,
    stderr: bool,
) -> Result<(), codespan_reporting::files::Error> {
    let mut files = codespan_reporting::files::SimpleFiles::new();
    let file_id = files.add(name, source);

    let writer = if stderr {
        StandardStream::stderr(ColorChoice::Always)
    } else {
        StandardStream::stdout(ColorChoice::Always)
    };
    let config = codespan_reporting::term::Config::default();

    for diagnostic in diagnostics {
        codespan_reporting::term::emit_to_write_style(
            &mut writer.lock(),
            &config,
            &files,
            &diagnostic.into_codespan_reporting(|_f| file_id),
        )?;
    }

    Ok(())
}

impl<F> crate::Diagnostic<F> {
    /// Converts [`crate::Diagnostic`] into [`codespan_reporting::diagnostic::Diagnostic`].
    ///
    /// [`Span::file`](crate::Span::file) is converted to the `FileId` using `convert_file`.
    pub fn into_codespan_reporting<FileId>(
        self,
        convert_file: impl Fn(F) -> FileId,
    ) -> codespan_reporting::diagnostic::Diagnostic<FileId> {
        codespan_reporting::diagnostic::Diagnostic::new(match self.severity {
            Severity::Error => codespan_reporting::diagnostic::Severity::Error,
            Severity::Warn => codespan_reporting::diagnostic::Severity::Warning,
        })
        .with_message(self.msg)
        .with_labels(
            self.span
                .primary_spans
                .into_iter()
                .map(|span| Label::primary(convert_file(span.file), span.text_range))
                .chain(self.span.span_labels.into_iter().map(|span_label| {
                    Label::secondary(
                        convert_file(span_label.span.file),
                        span_label.span.text_range,
                    )
                    .with_message(span_label.msg.as_ref())
                }))
                .collect(),
        )
    }
}
