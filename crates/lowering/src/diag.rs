//! Diagnostic emission for preprocessed text.

use dt_tools_diagnostic::{Diagnostic, DiagnosticMessage, MultiSpan, Severity};
use dt_tools_parser::TextRange;

use crate::preprocess::PreprocessedText;

/// Diagnostic sink for ranges in preprocessed text.
pub struct Diag<'s> {
    sink: &'s mut Vec<Diagnostic<PreprocessedText>>,
}

impl<'s> Diag<'s> {
    /// Creates a [`Diag`] from a sink.
    pub fn new(sink: &'s mut Vec<Diagnostic<PreprocessedText>>) -> Self {
        Self { sink }
    }

    /// Emits a diagnostic for `range` in the preprocessed text.
    pub fn emit<Msg: Into<DiagnosticMessage>>(
        &mut self,
        range: TextRange,
        msg: Msg,
        severity: Severity,
    ) {
        self.sink.push(Diagnostic {
            span: MultiSpan::from(range.within_file(PreprocessedText)),
            msg: msg.into(),
            severity,
        });
    }

    /// Pushes a pre-built diagnostic.
    pub fn push(&mut self, diag: Diagnostic<PreprocessedText>) {
        self.sink.push(diag);
    }
}
