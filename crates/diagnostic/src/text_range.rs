use std::{
    fmt::{Debug, Display},
    ops::Range,
};

use crate::Span;

// TODO: use `text_size::TextRange` instead
// TODO: u32

/// Location in source code of a specific file
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TextRange {
    /// The beginning byte offset (inclusive)
    pub start: usize,
    /// The ending byte offset (exclusive)
    pub end: usize,
}
impl Display for TextRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl TextRange {
    /// Creates a new `TextRange`.
    #[must_use]
    #[inline]
    pub const fn new(start: usize, end: usize) -> TextRange {
        TextRange { start, end }
    }

    /// Returns the length of the text range.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_tools_diagnostic::text_range::TextRange;
    ///
    /// let text_range = TextRange { start: 4, end: 7 };
    /// assert_eq!(text_range.length(), 3);
    ///
    /// let text_range = TextRange { start: 4, end: 4 };
    /// assert_eq!(text_range.length(), 0);
    /// ```
    #[inline]
    #[must_use]
    pub const fn length(self) -> usize {
        // TODO: rename to len for consistency with Rust std
        self.end - self.start
    }

    /// Offsets the text range by the specified amount.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_tools_diagnostic::text_range::TextRange;
    ///
    /// let text_range = TextRange { start: 4, end: 7 };
    /// assert_eq!(text_range.offset(2), TextRange { start: 6, end: 9 });
    /// ```
    #[inline]
    #[must_use]
    pub const fn offset(self, offset: usize) -> Self {
        TextRange {
            start: self.start + offset,
            end: self.end + offset,
        }
    }

    /// Shortens the text range from the start by the specified amount.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_tools_diagnostic::text_range::TextRange;
    ///
    /// let text_range = TextRange { start: 4, end: 7 };
    /// assert_eq!(text_range.trim_start(2), TextRange { start: 6, end: 7 });
    /// ```
    #[inline]
    #[must_use]
    pub const fn trim_start(self, offset: usize) -> Self {
        TextRange {
            start: self.start + offset,
            end: self.end,
        }
    }

    /// Shortens the text range from the end by the specified amount.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_tools_diagnostic::text_range::TextRange;
    ///
    /// let text_range = TextRange { start: 4, end: 7 };
    /// assert_eq!(text_range.trim_end(2), TextRange { start: 4, end: 5 });
    /// ```
    #[inline]
    #[must_use]
    pub const fn trim_end(self, offset: usize) -> Self {
        TextRange {
            start: self.start,
            end: self.end - offset,
        }
    }

    /// Returns the text as referenced from `source`.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_tools_diagnostic::text_range::TextRange;
    ///
    /// let source = "foo bar baz";
    ///
    /// let text_range = TextRange { start: 4, end: 7 };
    /// assert_eq!(text_range.text(source), Some("bar"));
    ///
    /// // Out of bounds
    /// let text_range = TextRange { start: 11, end: 12 };
    /// assert_eq!(text_range.text(source), None);
    /// ```
    #[inline]
    #[must_use]
    pub fn text(self, source: &str) -> Option<&str> {
        source.get(self.start..self.end)
    }

    /// Returns the byte offset range in a [`Range`].
    ///
    /// # Example
    ///
    /// ```
    /// use dt_tools_diagnostic::text_range::TextRange;
    ///
    /// let span = TextRange { start: 4, end: 7 };
    /// assert_eq!(span.byte_range(), 4..7);
    /// ```
    #[inline]
    #[must_use]
    pub const fn byte_range(self) -> Range<usize> {
        self.start..self.end
    }

    /// Returns a `TextRange` that would enclose both `self` and `end`.
    ///
    /// Note that this can also be used to extend the text range "backwards":
    /// `start.to(end)` and `end.to(start)` return the same `TextRange`.
    ///
    /// ```text
    ///     ____             ___
    ///     self lorem ipsum end
    ///     ^^^^^^^^^^^^^^^^^^^^
    /// ```
    #[must_use]
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
    #[must_use]
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
    #[must_use]
    pub fn until(self, end: TextRange) -> TextRange {
        TextRange::new(self.start.min(end.start), self.start.max(end.start))
    }

    /// Returns the overlapping intersection of two ranges, or `None` if they don't overlap.
    ///
    /// ```text
    ///     ____
    ///         ____
    ///     -> None
    /// ```
    ///
    /// ```text
    ///     ____
    ///       ______
    ///       ^^
    /// ```
    ///
    /// ```text
    ///     ________
    ///         ____
    ///         ^^^^
    /// ```
    ///
    /// ```text
    ///     ____
    ///     ________
    ///     ^^^^
    /// ```
    #[must_use]
    pub fn intersect(self, other: TextRange) -> Option<TextRange> {
        let start = self.start.max(other.start);
        let end = self.end.min(other.end);

        if start < end {
            Some(TextRange::new(start, end))
        } else {
            None
        }
    }

    /// Returns the subranges of `self` that fall outside `other`.
    ///
    /// If `other` completely covers `self`, the returned vector will be empty.
    ///
    /// ```text
    ///     ________  self
    ///     ____      other
    ///         ^^^^  returned
    /// ```
    ///
    /// ```text
    ///     ________  self
    ///       ____    other
    ///     ^^    ^^  returned
    /// ```
    ///
    /// ```text
    ///       ____    self
    ///     ________  other
    ///               returned
    /// ```
    ///
    /// ```text
    ///     ____      self
    ///         ____  other
    ///     ^^^^      returned
    /// ```
    #[must_use]
    pub fn difference(self, other: TextRange) -> Vec<TextRange> {
        // No overlap at all
        if self.intersect(other).is_none() {
            return vec![self];
        }

        let mut result = Vec::with_capacity(2);

        if self.start < other.start {
            result.push(TextRange::new(self.start, other.start));
        }

        if self.end > other.end {
            result.push(TextRange::new(other.end, self.end));
        }

        result
    }

    /// Maps a sub-range of this range onto a target range of the exact same size as this range.
    ///
    /// # Panics
    ///
    /// Panics if `subrange` isn't inside `self` or if `self.length() != target.length()`.
    #[must_use]
    pub fn project_subrange(self, subrange: TextRange, target: TextRange) -> TextRange {
        assert!(
            self.fully_contains_range(subrange),
            "self should fully contain subrange. self={self}, subrange={subrange}"
        );
        assert_eq!(
            self.length(),
            target.length(),
            "self's and target's lengths should be equal. self={self}, subrange={subrange}, target={target}",
        );

        // Guaranteed >= 0 because subrange is inside self
        let relative_start = subrange.start - self.start;
        let relative_end = subrange.end - self.start;

        TextRange::new(target.start + relative_start, target.start + relative_end)
    }

    /// Returns true if this text range fully contains another text range.
    #[must_use]
    pub fn fully_contains_range(self, other: TextRange) -> bool {
        self.start <= other.start && other.end <= self.end
    }

    /// Returns a `Span` from this text range and a file.
    #[inline]
    #[must_use]
    pub fn within_file<F>(self, file: F) -> Span<F> {
        Span {
            file,
            text_range: self,
        }
    }
}

impl From<Range<usize>> for TextRange {
    #[inline]
    fn from(value: Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}
impl From<TextRange> for Range<usize> {
    #[inline]
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

    fn tr_from_ascii(line: &str) -> TextRange {
        let trimmed = line.trim();

        let start = trimmed.as_ptr() as usize - line.as_ptr() as usize;
        let end = start + trimmed.len();

        TextRange::new(start, end)
    }

    fn fmt_tr(tr: TextRange, s: &str) -> String {
        format!("{}{}", " ".repeat(tr.start), s.repeat(tr.length()))
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

    #[expect(
        clippy::needless_raw_string_hashes,
        reason = "expect-test auto update adds r#"
    )]
    #[test]
    fn intersect() {
        use expect_test::expect;

        let cases = [
            expect![[r#"
                ____
                    ____
                -> None
            "#]],
            expect![[r#"
                ____
                  ______
                  ^^
            "#]],
            expect![[r#"
                ________
                    ____
                    ^^^^
            "#]],
            expect![[r#"
                ____
                ________
                ^^^^
            "#]],
        ];

        for case in cases {
            let lines = case.data().lines().collect::<Vec<_>>();
            let [_, this, other, _expected_output, _] = lines.try_into().unwrap();
            let mut this_tr = tr_from_ascii(this);
            let mut other_tr = tr_from_ascii(other);

            // Remove indent
            let min = this_tr.start.min(other_tr.start);
            this_tr.start -= min;
            this_tr.end -= min;
            other_tr.start -= min;
            other_tr.end -= min;

            let this = fmt_tr(this_tr, "_");
            let other = fmt_tr(other_tr, "_");

            let actual_output_tr = this_tr.intersect(other_tr);

            let actual_output =
                actual_output_tr.map_or_else(|| "-> None".to_owned(), |tr| fmt_tr(tr, "^"));

            let actual = format!("{this}\n{other}\n{actual_output}\n");
            case.assert_eq(&actual);
        }
    }
}
