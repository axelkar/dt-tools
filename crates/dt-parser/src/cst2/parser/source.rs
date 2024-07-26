use crate::{
    cst2::lexer::{Token, TokenKind},
    TextRange,
};

#[derive(Debug)]
pub(super) struct Source<'t, 'input> {
    tokens: &'t [Token<'input>],
    prev_cursor: usize,
    cursor: usize,
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
            prev_cursor: 0,
            cursor: 0,
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
                let boundary = (1..4)
                    .find(|num| self.following_src.is_char_boundary(25 - num))
                    .expect("should have a boundary max 4 bytes down");
                self.following_src.truncate(boundary);
            }
        }
    }

    pub(super) fn next_token(&mut self) -> Option<&'t Token<'input>> {
        self.prev_cursor = self.cursor;

        self.skip_trivia();

        let token = self.tokens.get(self.cursor)?;
        self.cursor += 1;

        #[cfg(debug_assertions)]
        self.update_debug();

        Some(token)
    }

    /// Returns the last token's range if it exists.
    pub(crate) fn last_token_range(&self) -> Option<TextRange> {
        Some(self.tokens.last()?.text_range)
    }

    /// Returns the range of the current token if it exists.
    pub(crate) fn range(&self) -> Option<TextRange> {
        Some(self.peek_token_immediate()?.text_range)
    }

    /// Returns the range of the first (possibly trivia) token after the previous non-trivia token if it exists.
    pub(crate) fn prev_next_range(&self) -> Option<TextRange> {
        Some(self.tokens.get(self.prev_cursor + 1)?.text_range)
    }

    /// Peeks ahead at the current token's kind.
    ///
    /// Returns None on EOF
    pub(super) fn peek_kind(&mut self) -> Option<TokenKind> {
        self.skip_trivia();
        self.peek_kind_immediate()
    }

    pub(super) fn skip_trivia(&mut self) {
        while self
            .peek_kind_immediate()
            .map_or(false, TokenKind::is_trivia)
        {
            self.cursor += 1;
        }

        #[cfg(debug_assertions)]
        self.update_debug();
    }

    /// Peeks ahead at the current token's kind.
    ///
    /// Doesn't skip trivia.
    ///
    /// Returns None on EOF.
    pub(super) fn peek_kind_immediate(&self) -> Option<TokenKind> {
        // We can ignore the error when peeking
        Some(
            self.peek_token_immediate()?
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
