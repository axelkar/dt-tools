use std::borrow::Cow;

use text_range::TextRange;

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

impl DiagnosticCollector for std::sync::Mutex<&mut Vec<Diagnostic>> {
    fn emit(&self, diag: Diagnostic) {
        self.lock().unwrap().push(diag)
    }
}
impl DiagnosticCollector for std::sync::Mutex<Vec<Diagnostic>> {
    fn emit(&self, diag: Diagnostic) {
        self.lock().unwrap().push(diag)
    }
}

#[cfg(feature = "parking_lot")]
impl DiagnosticCollector for parking_lot::Mutex<&mut Vec<Diagnostic>> {
    fn emit(&self, diag: Diagnostic) {
        self.lock().push(diag)
    }
}
#[cfg(feature = "parking_lot")]
impl DiagnosticCollector for parking_lot::Mutex<Vec<Diagnostic>> {
    fn emit(&self, diag: Diagnostic) {
        self.lock().push(diag)
    }
}
