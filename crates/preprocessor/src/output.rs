//! Preprocessor output.

use dt_tools_diagnostic::{Diagnostic, MultiSpan, Span, text_range::TextRange};

use crate::{
    files::{SourceFile, SourceId},
    origin::ProvenanceMap,
};

/// An include from one file to another.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IncludeEdge {
    /// The included file.
    pub file: SourceId,
    /// Span of the include directive in the includer.
    pub span: Span<SourceId>,
}

/// A macro definition site.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroDefSite {
    /// The macro name.
    pub name: String,
    /// Span of the `#define` directive.
    pub span: Span<SourceId>,
}

/// The result of preprocessing a root file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreprocessedRoot {
    /// The expanded text.
    pub text: String,
    /// Where the expanded text came from.
    pub map: ProvenanceMap,
    /// Source files in first-encounter order.
    pub sources: Vec<SourceFile>,
    /// Include edges.
    pub includes: Vec<IncludeEdge>,
    /// Ranges skipped by false conditional branches.
    pub skipped: Vec<Span<SourceId>>,
    /// Macro definitions.
    pub macro_defs: Vec<MacroDefSite>,
    /// Diagnostics from preprocessing.
    pub diagnostics: Vec<Diagnostic<SourceId>>,
}

impl PreprocessedRoot {
    /// Resolves a range in [`Self::text`] to source spans.
    #[must_use]
    pub fn resolve(&self, range: TextRange) -> MultiSpan<SourceId> {
        self.map.resolve(range)
    }

    /// Returns the file with `id`.
    ///
    /// # Panics
    ///
    /// Panics if `id` is out of range for the sources.
    #[must_use]
    pub fn source(&self, id: SourceId) -> &SourceFile {
        &self.sources[id.0 as usize]
    }
}
