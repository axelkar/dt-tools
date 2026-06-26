use std::borrow::Cow;

use text_range::TextRange;

#[cfg(feature = "codespan-reporting")]
pub mod codespan_reporting;
pub mod text_range;

// TODO: https://doc.rust-lang.org/nightly/nightly-rustc/rustc_error_messages/enum.DiagMessage.html
pub type DiagnosticMessage = Cow<'static, str>;

/// Location in source code
///
/// `F` identifies a file/source.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span<F> {
    pub file: F,
    pub text_range: TextRange,
}
impl<F> Span<F> {
    /// Returns a `Span` with the same file as here and the passed-in `text_range` offset to be inside this span.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_tools_diagnostic::{Span, text_range::TextRange};
    ///
    /// let span = Span {
    ///     file: "foo",
    ///     text_range: TextRange::new(5, 10),
    /// };
    ///
    /// // E.g. from macro parse
    /// let text_range_inside = TextRange::new(2, 5);
    ///
    /// let span_inside = span.subspan_inside(text_range_inside);
    /// assert_eq!(span_inside, Span {
    ///     file: "foo",
    ///     text_range: TextRange::new(7, 10)
    /// });
    /// ```
    #[must_use]
    pub fn subspan_inside(&self, text_range: TextRange) -> Self
    where
        F: Clone,
    {
        Self {
            file: self.file.clone(),
            text_range: text_range.offset(self.text_range.start),
        }
    }
}

/// Just like the [MultiSpan from rustc & clippy][1]
///
/// `F` identifies a file/source.
///
/// [1]: https://doc.rust-lang.org/nightly/nightly-rustc/rustc_error_messages/struct.MultiSpan.html
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MultiSpan<F> {
    /// The primary ranges of text this error points to
    pub primary_spans: Vec<Span<F>>,
    /// Additional hints in the error
    pub span_labels: Vec<SpanLabel<F>>,
}

impl<F> From<Span<F>> for MultiSpan<F> {
    fn from(value: Span<F>) -> Self {
        Self {
            primary_spans: vec![value],
            span_labels: Vec::new(),
        }
    }
}

/// Span label, supplementing the primary spans with additional information
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SpanLabel<F> {
    pub span: Span<F>,
    pub msg: DiagnosticMessage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Diagnostic<F> {
    pub span: MultiSpan<F>,
    pub msg: DiagnosticMessage,
    pub severity: Severity,
}
impl<F> Diagnostic<F> {
    #[must_use]
    pub fn new(primary_span: Span<F>, msg: DiagnosticMessage, severity: Severity) -> Self {
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

pub trait DiagnosticCollector<F> {
    fn emit(&self, diag: Diagnostic<F>);
}

impl<F, T: DiagnosticCollector<F>> DiagnosticCollector<F> for &T {
    fn emit(&self, diag: Diagnostic<F>) {
        <T as DiagnosticCollector<F>>::emit(*self, diag);
    }
}

impl<F> DiagnosticCollector<F> for std::sync::Mutex<&mut Vec<Diagnostic<F>>> {
    fn emit(&self, diag: Diagnostic<F>) {
        self.lock().unwrap().push(diag);
    }
}
impl<F> DiagnosticCollector<F> for std::sync::Mutex<Vec<Diagnostic<F>>> {
    fn emit(&self, diag: Diagnostic<F>) {
        self.lock().unwrap().push(diag);
    }
}

#[cfg(feature = "parking_lot")]
impl<F> DiagnosticCollector<F> for parking_lot::Mutex<&mut Vec<Diagnostic<F>>> {
    fn emit(&self, diag: Diagnostic<F>) {
        self.lock().push(diag);
    }
}
#[cfg(feature = "parking_lot")]
impl<F> DiagnosticCollector<F> for parking_lot::Mutex<Vec<Diagnostic<F>>> {
    fn emit(&self, diag: Diagnostic<F>) {
        self.lock().push(diag);
    }
}
