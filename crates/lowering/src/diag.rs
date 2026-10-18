//! Diagnostic emission and source mapping for lowering.

use dt_tools_diagnostic::{Diagnostic, DiagnosticMessage, MultiSpan, Severity, Span};
use dt_tools_parser::TextRange;
use dt_tools_preprocessor::PreprocessedRoot;

use crate::{db::BaseDb, expand::resolve_range, file::File};

/// Where ranges passed to [`Diag`] are located.
enum Source<'a> {
    /// Byte offsets into `file`'s source text.
    File(File),
    /// Byte offsets into the expanded text of a root file.
    Expanded {
        db: &'a dyn BaseDb,
        expanded: &'a PreprocessedRoot,
        /// Fallback for ranges that have no corresponding source text.
        root: File,
    },
}

/// Diagnostic sink that maps ranges to file spans.
pub struct Diag<'s, 'a> {
    sink: &'s mut Vec<Diagnostic<File>>,
    source: Source<'a>,
}

impl<'s, 'a> Diag<'s, 'a> {
    /// Creates a [`Diag`] for ranges in `file`.
    pub fn file(sink: &'s mut Vec<Diagnostic<File>>, file: File) -> Self {
        Self {
            sink,
            source: Source::File(file),
        }
    }

    /// Creates a [`Diag`] for ranges in a preprocessed root's expanded text.
    pub fn expanded(
        sink: &'s mut Vec<Diagnostic<File>>,
        db: &'a dyn BaseDb,
        expanded: &'a PreprocessedRoot,
        root: File,
    ) -> Self {
        Self {
            sink,
            source: Source::Expanded { db, expanded, root },
        }
    }

    /// Emits a diagnostic for `range` in the current text.
    ///
    /// The diagnostic will include span labels from the source map chain (e.g. macro
    /// expansion sites).
    pub fn emit<Msg: Into<DiagnosticMessage>>(
        &mut self,
        range: TextRange,
        msg: Msg,
        severity: Severity,
    ) {
        /// Helper function that isn't monomorphized
        fn emit_inner(
            this: &mut Diag<'_, '_>,
            range: TextRange,
            msg: DiagnosticMessage,
            severity: Severity,
        ) {
            let multi_span = this.resolve_full(range);
            this.sink.push(Diagnostic {
                span: multi_span,
                msg,
                severity,
            });
        }

        emit_inner(self, range, msg.into(), severity);
    }

    /// Maps `range` to a [`MultiSpan<File>`] with primary spans and span labels.
    #[must_use]
    pub fn resolve_full(&self, range: TextRange) -> MultiSpan<File> {
        match self.source {
            Source::File(file) => MultiSpan::from(range.within_file(file)),
            Source::Expanded { db, expanded, root } => {
                let mut span = resolve_range(db, expanded, range);
                if span.primary_spans.is_empty() {
                    span.primary_spans.push(range.within_file(root));
                }
                span
            }
        }
    }

    /// Maps `range` to the first primary [`Span<File>`].
    ///
    /// # Panics
    ///
    /// Panics if [`Self::resolve_full`] doesn't return any primary spans.
    #[must_use]
    pub fn resolve(&self, range: TextRange) -> Span<File> {
        self.resolve_full(range)
            .primary_spans
            .into_iter()
            .next()
            .expect("resolve_full must return at least one primary span")
    }

    /// Pushes a pre-built diagnostic.
    pub fn push(&mut self, diag: Diagnostic<File>) {
        self.sink.push(diag);
    }
}
