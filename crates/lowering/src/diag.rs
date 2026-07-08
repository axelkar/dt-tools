//! Diagnostic emission and source mapping for lowering.

use dt_tools_diagnostic::{Diagnostic, DiagnosticMessage, Severity, Span};
use dt_tools_parser::TextRange;

use crate::file::File;

/// Maps a [`TextRange`] in the text being lowered to a [`Span<File>`].
///
/// It is an enum rather than a closure so that it does not cause monomorphization. A range emitted
/// deep inside a macro expansion is mapped up through the parents.
pub enum SourceMap<'a> {
    /// Ranges are byte offsets into the file's source text.
    File(File),
    /// Ranges must be shifted by `offset` before mapping through `parent`.
    ///
    /// Used when a substring is reparsed on its own (e.g preprocessor conditionals) so its local
    /// ranges need translating back into the enclosing text.
    Offset {
        parent: &'a SourceMap<'a>,
        offset: usize,
    },
    /// Ranges are byte offsets into a macro expansion.
    Macro {
        parent: &'a SourceMap<'a>,
        /// Span of the whole macro invocation in the parent text.
        invocation: Span<File>,
        // TODO: &[TextRangeMap]
    },
}

impl SourceMap<'_> {
    /// Maps `range` to a [`Span<File>`].
    fn resolve(&self, range: TextRange) -> Span<File> {
        match self {
            SourceMap::File(file) => range.within_file(*file),
            SourceMap::Offset { parent, offset } => parent.resolve(range.offset(*offset)),
            // TODO: add span label "in this macro invocation"
            // TODO: &[TextRangeMap]
            SourceMap::Macro { parent, invocation } => parent.resolve(range),
        }
    }
}

/// Diagnostic sink and source-mapping context.
///
/// The sink cannot live inside [`SourceMap`] because every `SourceMap` frame like
/// [`SourceMap::Macro`] would have to alias the single `&mut` sink.
pub struct Diag<'s, 'm> {
    pub(crate) sink: &'s mut Vec<Diagnostic<File>>,
    pub(crate) map: &'m SourceMap<'m>,
}

impl<'s, 'm> Diag<'s, 'm> {
    /// Creates a [`Diag`] from a sink and a source map.
    pub fn new(sink: &'s mut Vec<Diagnostic<File>>, map: &'m SourceMap<'m>) -> Self {
        Self { sink, map }
    }

    /// Emits a single-span diagnostic for `range` in the current text.
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
            let span = this.map.resolve(range);
            this.sink.push(Diagnostic::new(span, msg, severity));
        }

        emit_inner(self, range, msg.into(), severity);
    }

    /// Maps `range` to a [`Span<File>`].
    #[must_use]
    pub fn resolve(&self, range: TextRange) -> Span<File> {
        self.map.resolve(range)
    }

    /// Pushes a pre-built diagnostic.
    pub fn push(&mut self, diag: Diagnostic<File>) {
        self.sink.push(diag);
    }
}
