use std::borrow::Cow;

use text_range::TextRange;

#[cfg(feature = "codespan-reporting")]
pub mod codespan_reporting;
pub mod text_range;

// TODO: https://doc.rust-lang.org/nightly/nightly-rustc/rustc_error_messages/enum.DiagMessage.html
pub type DiagnosticMessage = Cow<'static, str>;

/// Just like the [MultiSpan from rustc & clippy][1]
///
/// [1]: https://doc.rust-lang.org/nightly/nightly-rustc/rustc_error_messages/struct.MultiSpan.html
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MultiSpan {
    pub primary_spans: Vec<TextRange>,
    pub span_labels: Vec<SpanLabel>,
}

impl MultiSpan {
    /// Offsets the spans by the specified amount.
    pub fn offset(&mut self, offset: usize) {
        for tr in &mut self.primary_spans {
            *tr = tr.offset(offset);
        }
        for span_label in &mut self.span_labels {
            span_label.span = span_label.span.offset(offset);
        }
    }
}

impl From<TextRange> for MultiSpan {
    fn from(value: TextRange) -> Self {
        Self {
            primary_spans: vec![value],
            span_labels: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SpanLabel {
    pub span: TextRange,
    pub msg: DiagnosticMessage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Diagnostic {
    pub span: MultiSpan,
    pub msg: DiagnosticMessage,
    pub severity: Severity,
}
impl Diagnostic {
    #[must_use]
    pub fn new(primary_span: TextRange, msg: DiagnosticMessage, severity: Severity) -> Self {
        Self {
            span: MultiSpan::from(primary_span),
            msg,
            severity,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Severity {
    Warn,
    Error,
}

pub trait DiagnosticCollector {
    fn emit(&self, diag: Diagnostic);
}

impl<T: DiagnosticCollector> DiagnosticCollector for &T {
    fn emit(&self, diag: Diagnostic) {
        <T as DiagnosticCollector>::emit(*self, diag);
    }
}

impl DiagnosticCollector for std::sync::Mutex<&mut Vec<Diagnostic>> {
    fn emit(&self, diag: Diagnostic) {
        self.lock().unwrap().push(diag);
    }
}
impl DiagnosticCollector for std::sync::Mutex<Vec<Diagnostic>> {
    fn emit(&self, diag: Diagnostic) {
        self.lock().unwrap().push(diag);
    }
}

#[cfg(feature = "parking_lot")]
impl DiagnosticCollector for parking_lot::Mutex<&mut Vec<Diagnostic>> {
    fn emit(&self, diag: Diagnostic) {
        self.lock().push(diag);
    }
}
#[cfg(feature = "parking_lot")]
impl DiagnosticCollector for parking_lot::Mutex<Vec<Diagnostic>> {
    fn emit(&self, diag: Diagnostic) {
        self.lock().push(diag);
    }
}

pub struct OffsetDiagnosticCollector<T: DiagnosticCollector> {
    pub inner: T,
    pub offset: usize,
}
impl<T: DiagnosticCollector> DiagnosticCollector for OffsetDiagnosticCollector<T> {
    fn emit(&self, mut diag: Diagnostic) {
        diag.span.offset(self.offset);
        self.inner.emit(diag);
    }
}
