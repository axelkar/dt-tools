use crate::{
    TextRange,
    lexer::{Token, TokenKind},
};

/// The cached result of [`Source::peek_kind`].
#[derive(Debug, Clone, Copy)]
enum CachedPeek {
    /// No peek has been performed since the cursor moved.
    Missing,
    /// The cursor is at the end of input.
    End,
    /// The cursor points at a token of this kind.
    Kind(TokenKind),
}

#[derive(Debug)]
pub(super) struct Source<'t, 'input> {
    pub(super) tokens: &'t [Token<'input>],
    /// The index of the (possibly trivia) token after the previous non-trivia token if it exists.
    ///
    /// ```text
    /// foo   bar
    /// ^^^ cursor
    /// prev_next_cursor = uninitialized (impl detail: 0)
    /// ```
    ///
    /// ```ignore
    /// assert_eq!(source.peek_kind(), Some(Foo));
    /// assert_eq!(source.next_token().unwrap().kind.unwrap(), Foo);
    /// ```
    ///
    /// ```text
    /// foo   bar
    ///    ^^^ cursor
    ///    ^^^ prev_next_cursor
    /// ```
    ///
    /// ## Branch A
    ///
    /// ```ignore
    /// assert_eq!(source.peek_kind(), Some(Bar));
    /// ```
    ///
    /// ```text
    /// foo   bar
    ///       ^^^ cursor
    ///    ^^^ prev_next_cursor
    /// ```
    ///
    /// ```ignore
    /// assert_eq!(source.next_token().unwrap().kind.unwrap(), Bar);
    /// ```
    ///
    /// ```text
    /// foo   bar
    ///          ^ cursor
    ///          ^ prev_next_cursor
    /// ```
    ///
    /// ## Branch B
    ///
    /// ```ignore
    /// assert_eq!(source.next_token().unwrap().kind.unwrap(), Bar);
    /// ```
    ///
    /// ```text
    /// foo   bar
    ///          ^ cursor
    ///          ^ prev_next_cursor
    /// ```
    pub(super) prev_next_cursor: usize,
    /// From which index the next immediate token will be read from.
    pub(super) cursor: usize,
    pub(super) steps: u32,
    /// Cached result of [`Source::peek_kind`], invalidated whenever the cursor moves.
    cached_peek: CachedPeek,
}

impl<'t, 'input> Source<'t, 'input> {
    pub(super) fn new(tokens: &'t [Token<'input>]) -> Self {
        Self {
            tokens,
            prev_next_cursor: 0,
            cursor: 0,
            steps: 0,
            cached_peek: CachedPeek::Missing,
        }
    }

    pub(super) fn peek_text(&mut self) -> Option<&'input str> {
        Some(self.tokens.get(self.cursor)?.text)
    }

    pub(super) fn next_token(&mut self) -> Option<&'t Token<'input>> {
        // A cached peek means the cursor is already on a non-trivia token.
        if matches!(self.cached_peek, CachedPeek::Missing) {
            self.skip_trivia();
        }

        let token = self.tokens.get(self.cursor)?;
        self.cursor += 1;
        self.cached_peek = CachedPeek::Missing;
        self.steps = 0;
        self.prev_next_cursor = self.cursor;

        Some(token)
    }

    /// Returns the last token's range if it exists. The token may be trivia.
    pub(crate) fn last_token_range(&self) -> Option<TextRange> {
        Some(self.tokens.last()?.text_range)
    }

    /// Returns the range of the current token if it exists.
    pub(crate) fn range(&self) -> Option<TextRange> {
        Some(self.peek_token_immediate()?.text_range)
    }

    /// Returns the range of the (possibly trivia) token after the previous non-trivia token if it exists.
    ///
    /// Unless `cursor` is 0 (meaning no bumps have occurred), this will always return `Some`.
    ///
    /// Usually this is like so:
    /// ```text
    /// foo   bar
    ///       ^^^ if cursor is here
    ///    ^^^ prev_next_cursor is here
    /// ```
    // FIXME: this will usually return Some since prev_next_cursor is initialized to 0, except at
    // end of input!!
    pub(crate) fn prev_next_range(&self) -> Option<TextRange> {
        Some(self.tokens.get(self.prev_next_cursor)?.text_range)
    }

    #[cfg(test)]
    /// Returns the text of the (possibly trivia) token after the previous non-trivia token if it exists.
    ///
    /// Unless `cursor` is 0 (meaning no bumps have occurred), this will always return `Some`.
    pub(crate) fn prev_next_text(&self) -> Option<&str> {
        Some(self.tokens.get(self.prev_next_cursor)?.text)
    }

    /// Peeks ahead at the current token's kind.
    ///
    /// Returns None at end of input
    pub(super) fn peek_kind(&mut self) -> Option<TokenKind> {
        match self.cached_peek {
            CachedPeek::Kind(kind) => {
                // Preserve the step accounting of a real peek.
                self.steps += 2;
                return Some(kind);
            }
            CachedPeek::End => {
                self.steps += 2;
                return None;
            }
            CachedPeek::Missing => {}
        }

        self.skip_trivia();
        let kind = self.peek_kind_immediate();
        self.cached_peek = match kind {
            Some(kind) => CachedPeek::Kind(kind),
            None => CachedPeek::End,
        };
        kind
    }

    pub(super) fn skip_trivia(&mut self) {
        profiling::scope!("parser::Source::skip_trivia");

        self.cached_peek = CachedPeek::Missing;

        while self
            .tokens
            .get(self.cursor)
            .is_some_and(|tok| tok.kind.is_ok_and(TokenKind::is_trivia))
        {
            self.cursor += 1;
            self.steps = 0;
        }
        self.steps += 1;
    }

    /// Peeks ahead at the current token's kind.
    ///
    /// Doesn't skip trivia.
    ///
    /// Returns None at end of input.
    #[inline]
    pub(super) fn peek_kind_immediate(&mut self) -> Option<TokenKind> {
        profiling::scope!("parser::Source::peek_kind_immediate");

        self.steps += 1;

        // We can ignore the error when peeking
        Some(
            // TODO: TokenKind::LexError
            self.peek_token_immediate()?
                .kind
                .unwrap_or(TokenKind::Unrecognized),
        )
    }

    /// Peeks ahead at the immediate next token's kind.
    ///
    /// Doesn't skip trivia.
    ///
    /// Returns None at end of input.
    pub(super) fn peek_immediate_next_kind(&self) -> Option<TokenKind> {
        Some(
            self.tokens
                .get(self.cursor + 1)?
                .kind
                .unwrap_or(TokenKind::Unrecognized),
        )
    }

    /// Peeks ahead at the current token.
    ///
    /// Doesn't skip trivia.
    ///
    /// Returns None at end of input.
    fn peek_token_immediate(&self) -> Option<&Token<'_>> {
        self.tokens.get(self.cursor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[track_caller]
    fn expect_next_token_text(source: &mut Source, expected: &str) {
        let tok = source.next_token().expect("should have a token");
        assert_eq!(tok.text, expected);
    }

    #[test]
    fn prev_cursor() {
        let tokens: Vec<_> = Lexer::new("a /*a*/b\t/*b*/c").collect();
        assert_eq!(tokens.len(), 7);
        let mut source = Source::new(&tokens);

        assert_eq!(source.cursor, 0);
        assert_eq!(source.prev_next_cursor, 0);
        //assert_eq!(source.prev_next_text(), None);
        assert_eq!(source.prev_next_text(), Some("a"));

        expect_next_token_text(&mut source, "a");

        assert_eq!(source.cursor, 1);
        assert_eq!(source.prev_next_cursor, 1);
        assert_eq!(source.prev_next_text(), Some(" "));

        expect_next_token_text(&mut source, "b");

        assert_eq!(source.cursor, 4);
        assert_eq!(source.prev_next_cursor, 4);
        assert_eq!(source.prev_next_text(), Some("\t"));

        expect_next_token_text(&mut source, "c");

        assert_eq!(source.cursor, 7);
        assert_eq!(source.prev_next_cursor, 7);
        assert_eq!(source.prev_next_text(), None);

        // parser will only peek at end of input
        assert_eq!(source.peek_kind(), None);

        assert_eq!(source.cursor, 7);
        assert_eq!(source.prev_next_cursor, 7);
        assert_eq!(source.prev_next_text(), None);
    }

    #[test]
    fn prev_cursor_trailing_trivia() {
        let tokens: Vec<_> = Lexer::new("a /*a*/b\t/*b*/c\n").collect();
        assert_eq!(tokens.len(), 8);
        let mut source = Source::new(&tokens);

        assert_eq!(source.cursor, 0);
        assert_eq!(source.prev_next_cursor, 0);
        //assert_eq!(source.prev_next_text(), None);
        assert_eq!(source.prev_next_text(), Some("a"));

        expect_next_token_text(&mut source, "a");

        assert_eq!(source.cursor, 1);
        assert_eq!(source.prev_next_cursor, 1);
        assert_eq!(source.prev_next_text(), Some(" "));

        expect_next_token_text(&mut source, "b");

        assert_eq!(source.cursor, 4);
        assert_eq!(source.prev_next_cursor, 4);
        assert_eq!(source.prev_next_text(), Some("\t"));

        expect_next_token_text(&mut source, "c");

        assert_eq!(source.cursor, 7);
        assert_eq!(source.prev_next_cursor, 7);
        assert_eq!(source.prev_next_text(), Some("\n"));

        // parser will only peek at end of input
        assert_eq!(source.peek_kind(), None);

        assert_eq!(source.cursor, 8);
        assert_eq!(source.prev_next_cursor, 7);
        assert_eq!(source.prev_next_text(), Some("\n"));
    }
}
