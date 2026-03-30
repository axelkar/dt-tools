use codespan_reporting::diagnostic::Label;

use crate::Severity;

/// Prints diagnostics of a single file using [`codespan_reporting`].
#[expect(
    clippy::missing_errors_doc,
    reason = "Propagated from codespan_reporting, which doesn't document it at all."
)]
pub fn print_diagnostics_single_file<Name: std::fmt::Display + Clone, Source: AsRef<str>>(
    name: Name,
    source: Source,
    diagnostics: Vec<crate::Diagnostic>,
) -> Result<(), codespan_reporting::files::Error> {
    let mut files = codespan_reporting::files::SimpleFiles::new();
    let file_id = files.add(name, source);

    let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
        codespan_reporting::term::termcolor::ColorChoice::Always,
    );
    let config = codespan_reporting::term::Config::default();

    for diagnostic in diagnostics {
        codespan_reporting::term::emit_to_write_style(
            &mut writer.lock(),
            &config,
            &files,
            &diagnostic.into_codespan_reporting(file_id),
        )?;
    }

    Ok(())
}

impl crate::Diagnostic {
    /// Converts [`crate::Diagnostic`] into [`codespan_reporting::diagnostic::Diagnostic`].
    pub fn into_codespan_reporting<FileId: Clone>(
        self,
        file_id: FileId,
    ) -> codespan_reporting::diagnostic::Diagnostic<FileId> {
        codespan_reporting::diagnostic::Diagnostic::new(match self.severity {
            Severity::Error => codespan_reporting::diagnostic::Severity::Error,
            Severity::Warn => codespan_reporting::diagnostic::Severity::Warning,
        })
        .with_message(self.msg)
        .with_labels(
            self.span
                .primary_spans
                .iter()
                .map(|span| Label::primary(file_id.clone(), *span))
                .chain(self.span.span_labels.iter().map(|span| {
                    Label::secondary(file_id.clone(), span.span).with_message(span.msg.as_ref())
                }))
                .collect(),
        )
    }
}
