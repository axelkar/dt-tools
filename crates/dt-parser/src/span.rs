use std::fmt::{Debug, Display};
use std::ops::Range;

// TODO: About identical files: don't deduplicate the root Arcs.
// Check out rust-analyzer hir::semantics::SemanticsImpl::find_file.

/// A location in source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TextRange {
    /// The beginning byte offset of the span (inclusive)
    pub start: usize,
    /// The ending byte offset of the span (exclusive)
    pub end: usize,
}
impl Display for TextRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl TextRange {
    /// Returns the length of the span.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::TextRange;
    ///
    /// let span = TextRange { start: 4, end: 7 };
    /// assert_eq!(span.length(), 3);
    ///
    /// let span = TextRange { start: 4, end: 4 };
    /// assert_eq!(span.length(), 0);
    /// ```
    #[inline(always)]
    pub fn length(&self) -> usize {
        self.end - self.start
    }

    /// Returns the text as referenced from `source`.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::TextRange;
    ///
    /// let source = "foo bar baz";
    ///
    /// let span = TextRange { start: 4, end: 7 };
    /// assert_eq!(span.text(source), Some("bar"));
    ///
    /// // Out of bounds
    /// let span = TextRange { start: 11, end: 12 };
    /// assert_eq!(span.text(source), None);
    /// ```
    #[inline(always)]
    pub fn text<'i>(&self, source: &'i str) -> Option<&'i str> {
        source.get(self.start..self.end)
    }

    /// Returns the byte offset range in a [`Range`].
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::TextRange;
    ///
    /// let span = TextRange { start: 4, end: 7 };
    /// assert_eq!(span.byte_range(), 4..7);
    /// ```
    #[inline(always)]
    pub fn byte_range(&self) -> Range<usize> {
        self.start..self.end
    }

    // TODO: lsp_range(&self, rope: ropey::Rope) -> lsp_types::Range?
}

impl From<Range<usize>> for TextRange {
    #[inline(always)]
    fn from(value: Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}
impl From<TextRange> for Range<usize> {
    #[inline(always)]
    fn from(value: TextRange) -> Self {
        value.start..value.end
    }
}
