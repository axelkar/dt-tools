//! Preprocessing tokens.

use dt_tools_diagnostic::text_range::TextRange;

use crate::lexer::PpTokenKind;

/// A token produced by the preprocessing lexer.
///
/// See [GCC's tokenization documentation](https://gcc.gnu.org/onlinedocs/cpp/Tokenization.html)
/// and [cppreference.com's translation phases](https://en.cppreference.com/c/language/translation_phases#Phase_3).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PpToken {
    /// The token kind.
    pub kind: PpTokenKind,
    /// Range in the cleaned text ([C translation phases](https://en.cppreference.com/c/language/translation_phases) 1–3 applied).
    pub cleaned: TextRange,
    /// Range in the original file. Also known as the "spelled" range.
    pub orig: TextRange,
    /// Whether whitespace or a comment separates this token from the previous one.
    ///
    /// Always `false` for the first token.
    pub has_space_before: bool,
    /// Whether this is the first token on its [logical line](https://gcc.gnu.org/onlinedocs/cpp/Initial-processing.html).
    pub line_start: bool,
}

impl PpToken {
    /// Returns the token's text in `cleaned`.
    ///
    /// Compared to the token's text in the original file, this has
    /// for example line continuations removed.
    #[must_use]
    pub fn text<'a>(&self, cleaned: &'a str) -> &'a str {
        &cleaned[self.cleaned.byte_range()]
    }
}
