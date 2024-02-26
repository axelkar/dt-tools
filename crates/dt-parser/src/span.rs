use std::fmt::{Debug, Display};
use std::ops::Range;

/// About identical files: don't deduplicate the root Arcs.
//
/// Check out rust-analyzer hir::semantics::SemanticsImpl::find_file.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    // TODO: unicode? bytes or chars?
    pub start: usize,
    /// NOTE: not inclusive
    pub end: usize,
}
impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl Span {
    /// Calculate the length of the span
    #[inline(always)]
    pub fn length(&self) -> usize {
        self.end - self.start
    }

    /// Get the text
    pub fn text<'a>(&self, source: &'a str) -> Option<&'a str> {
        source.get(self.start..self.end)
    }

    pub fn range(&self) -> Range<usize> {
        self.start..self.end
    }

    // TODO: lsp_range(&self, rope: ropey::Rope) -> lsp_types::Range?
}

impl From<Span> for Range<usize> {
    fn from(value: Span) -> Self {
        value.range()
    }
}
