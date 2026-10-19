//! Diagnostic emission for expanded text.

use dt_tools_diagnostic::{Diagnostic, DiagnosticMessage, MultiSpan, Severity};
use dt_tools_parser::TextRange;

use crate::expand::ExpandedText;

/// Diagnostic sink for ranges in expanded text.
pub struct Diag<'s> {
    sink: &'s mut Vec<Diagnostic<ExpandedText>>,
}

impl<'s> Diag<'s> {
    /// Creates a [`Diag`] from a sink.
    pub fn new(sink: &'s mut Vec<Diagnostic<ExpandedText>>) -> Self {
        Self { sink }
    }

    /// Emits a diagnostic for `range` in the expanded text.
    pub fn emit<Msg: Into<DiagnosticMessage>>(
        &mut self,
        range: TextRange,
        msg: Msg,
        severity: Severity,
    ) {
        self.sink.push(Diagnostic {
            span: MultiSpan::from(range.within_file(ExpandedText)),
            msg: msg.into(),
            severity,
        });
    }

    /// Pushes a pre-built diagnostic.
    pub fn push(&mut self, diag: Diagnostic<ExpandedText>) {
        self.sink.push(diag);
    }
}
