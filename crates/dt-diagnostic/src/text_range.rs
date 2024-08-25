use std::fmt::{Debug, Display};
use std::ops::Range;

// TODO: About identical files: don't deduplicate the root Arcs.
// Check out rust-analyzer hir::semantics::SemanticsImpl::find_file.
// TODO: use `text_size::TextRange` instead
// TODO: u32

/// A location in source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    /// Creates a new `TextRange`.
    pub const fn new(start: usize, end: usize) -> TextRange {
        TextRange { start, end }
    }
    /// Returns the length of the span.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_diagnostic::text_range::TextRange;
    ///
    /// let span = TextRange { start: 4, end: 7 };
    /// assert_eq!(span.length(), 3);
    ///
    /// let span = TextRange { start: 4, end: 4 };
    /// assert_eq!(span.length(), 0);
    /// ```
    #[inline(always)]
    pub const fn length(&self) -> usize {
        self.end - self.start
    }
    /// Offsets the span by the specified amount
    ///
    /// # Example
    ///
    /// ```
    /// use dt_diagnostic::text_range::TextRange;
    ///
    /// let span = TextRange { start: 4, end: 7 };
    /// assert_eq!(span.offset(2), TextRange { start: 6, end: 9 });
    /// ```
    #[inline(always)]
    pub const fn offset(self, offset: usize) -> Self {
        TextRange {
            start: self.start + offset,
            end: self.end + offset,
        }
    }

    /// Returns the text as referenced from `source`.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_diagnostic::text_range::TextRange;
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
    /// use dt_diagnostic::text_range::TextRange;
    ///
    /// let span = TextRange { start: 4, end: 7 };
    /// assert_eq!(span.byte_range(), 4..7);
    /// ```
    #[inline(always)]
    pub const fn byte_range(&self) -> Range<usize> {
        self.start..self.end
    }

    // TODO: lsp_range(&self, rope: ropey::Rope) -> lsp_types::Range?

    /// Returns a `TextRange` that would enclose both `self` and `end`.
    ///
    /// Note that this can also be used to extend the span "backwards":
    /// `start.to(end)` and `end.to(start)` return the same `TextRange`.
    ///
    /// ```text
    ///     ____             ___
    ///     self lorem ipsum end
    ///     ^^^^^^^^^^^^^^^^^^^^
    /// ```
    pub fn to(self, end: TextRange) -> TextRange {
        TextRange::new(self.start.min(end.start), self.end.max(end.end))
    }

    /// Returns a `TextRange` between the end of `self` to the beginning of `end`.
    ///
    /// ```text
    ///     ____             ___
    ///     self lorem ipsum end
    ///         ^^^^^^^^^^^^^
    /// ```
    pub fn between(self, end: TextRange) -> TextRange {
        TextRange::new(self.end.min(end.end), self.start.max(end.start))
    }

    /// Returns a `TextRange` from the beginning of `self` until the beginning of `end`.
    ///
    /// ```text
    ///     ____             ___
    ///     self lorem ipsum end
    ///     ^^^^^^^^^^^^^^^^^
    /// ```
    pub fn until(self, end: TextRange) -> TextRange {
        TextRange::new(self.start.min(end.start), self.start.max(end.start))
    }
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

#[cfg(test)]
mod tests {
    use super::TextRange;

    fn find_tr(s: &str, substr: &str) -> TextRange {
        let start = s.find(substr).unwrap();
        TextRange::new(start, start + substr.len())
    }

    ///     ____             ___
    ///     self lorem ipsum end
    ///     ^^^^^^^^^^^^^^^^^^^^
    #[test]
    fn to() {
        let s = " self lorem ipsum end ";
        let start = find_tr(s, "self");
        let end = find_tr(s, "end");

        assert_eq!(start.to(end), find_tr(s, "self lorem ipsum end"));
        assert_eq!(end.to(start), find_tr(s, "self lorem ipsum end"));
    }

    ///     ____             ___
    ///     self lorem ipsum end
    ///         ^^^^^^^^^^^^^
    #[test]
    fn between() {
        let s = " self lorem ipsum end ";
        let start = find_tr(s, "self");
        let end = find_tr(s, "end");

        assert_eq!(start.between(end), find_tr(s, " lorem ipsum "));
        assert_eq!(end.between(start), find_tr(s, " lorem ipsum "));
    }

    ///     ____             ___
    ///     self lorem ipsum end
    ///     ^^^^^^^^^^^^^^^^^
    #[test]
    fn until() {
        let s = " self lorem ipsum end ";
        let start = find_tr(s, "self");
        let end = find_tr(s, "end");

        assert_eq!(start.until(end), find_tr(s, "self lorem ipsum "));
        assert_eq!(end.until(start), find_tr(s, "self lorem ipsum "));
    }
}
