use std::fmt::{Debug, Display};
use std::ops::Range;

// TODO: About identical files: don't deduplicate the root Arcs.
// Check out rust-analyzer hir::semantics::SemanticsImpl::find_file.

/// A location in source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    // TODO: unicode? bytes or chars?
    /// The beginning offset of the span (inclusive)
    pub start: usize,
    /// The ending offset of the span (exclusive)
    pub end: usize,
}
impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl Span {
    /// Returns the length of the span.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::Span;
    ///
    /// let span = Span { start: 4, end: 7 };
    /// assert_eq!(span.length(), 3);
    ///
    /// let span = Span { start: 4, end: 4 };
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
    /// use dt_parser::Span;
    ///
    /// let source = "foo bar baz";
    ///
    /// let span = Span { start: 4, end: 7 };
    /// assert_eq!(span.text(source), Some("bar"));
    ///
    /// // Out of bounds
    /// let span = Span { start: 11, end: 12 };
    /// assert_eq!(span.text(source), None);
    /// ```
    #[inline(always)]
    pub fn text<'a>(&self, source: &'a str) -> Option<&'a str> {
        source.get(self.start..self.end)
    }

    /// Returns the offset range in a [`Range`].
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::Span;
    ///
    /// let span = Span { start: 4, end: 7 };
    /// assert_eq!(span.range(), 4..7);
    /// ```
    #[inline(always)]
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
