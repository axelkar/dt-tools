use crate::{
    lexer::{Token, TokenKind},
    TextRange,
};

const PARSER_STEP_LIMIT: u32 = 1_000;

#[derive(Debug)]
pub(super) struct Source<'t, 'input> {
    tokens: &'t [Token<'input>],
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
    prev_next_cursor: usize,
    /// From which index the next immediate token will be read from.
    cursor: usize,
    steps: u32,
    // for debugger:
    #[cfg(debug_assertions)]
    byte_offset: usize,
    #[cfg(debug_assertions)]
    following_src: String,
}

impl<'t, 'input> Source<'t, 'input> {
    pub(super) fn new(tokens: &'t [Token<'input>]) -> Self {
        Self {
            tokens,
            prev_next_cursor: 0,
            cursor: 0,
            steps: 0,
            #[cfg(debug_assertions)]
            byte_offset: 0,
            #[cfg(debug_assertions)]
            following_src: String::new(),
        }
    }

    #[cfg(debug_assertions)]
    fn update_debug(&mut self) {
        if let Some(token) = self.tokens.get(self.cursor) {
            self.byte_offset = token.text_range.start;
            self.following_src = String::new();
            for token in self.tokens.iter().skip(self.cursor).take(15) {
                self.following_src.push_str(token.text);
            }
            if self.following_src.len() > 25 {
                let boundary = (0..4)
                    .find(|num| self.following_src.is_char_boundary(25 - num))
                    .expect("should have a boundary max 4 bytes down");
                self.following_src.truncate(boundary);
            }
        }
    }

    pub(super) fn next_token(&mut self) -> Option<&'t Token<'input>> {
        self.skip_trivia();

        let token = self.tokens.get(self.cursor)?;
        self.cursor += 1;
        self.steps = 0;
        self.prev_next_cursor = self.cursor;

        #[cfg(debug_assertions)]
        self.update_debug();

        #[cfg(feature = "visualize")]
        super::visualizer::Event::NextToken {
            cursor: self.cursor,
            prev_next_cursor: self.prev_next_cursor,
        }
        .visualize();

        Some(token)
    }

    /// Returns the last token's range if it exists. The token may be trivia.
    pub(crate) fn last_token_range(&self) -> Option<TextRange> {
        tracing::debug!(last_token = ?self.tokens.last());
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
    // EOF!!
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
    /// Returns None on EOF
    pub(super) fn peek_kind(&mut self) -> Option<TokenKind> {
        self.skip_trivia();
        self.peek_kind_immediate()
    }

    pub(super) fn skip_trivia(&mut self) {
        while self.peek_kind_immediate().is_some_and(TokenKind::is_trivia) {
            self.cursor += 1;
            self.steps = 0;
        }

        #[cfg(feature = "visualize")]
        super::visualizer::Event::SkippedTrivia {
            cursor: self.cursor,
        }
        .visualize();

        #[cfg(debug_assertions)]
        self.update_debug();
    }

    /// Peeks ahead at the current token's kind.
    ///
    /// Doesn't skip trivia.
    ///
    /// Returns None on EOF.
    pub(super) fn peek_kind_immediate(&mut self) -> Option<TokenKind> {
        assert!(self.steps < PARSER_STEP_LIMIT, "the parser seems stuck");
        self.steps += 1;

        #[cfg(feature = "visualize")]
        super::visualizer::Event::PeekKindImmediate.visualize();

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
    /// Returns None on EOF.
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
    /// Returns None on EOF.
    fn peek_token_immediate(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;

    use super::*;

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

        // parser will only peek at EOF
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

        // parser will only peek at EOF
        assert_eq!(source.peek_kind(), None);

        assert_eq!(source.cursor, 8);
        assert_eq!(source.prev_next_cursor, 7);
        assert_eq!(source.prev_next_text(), Some("\n"));
    }
}
