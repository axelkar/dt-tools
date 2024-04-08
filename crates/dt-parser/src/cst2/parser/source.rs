use crate::{
    cst2::lexer::{Token, TokenKind},
    TextRange,
};

#[derive(Debug)]
pub(super) struct Source<'t, 'input> {
    tokens: &'t [Token<'input>],
    cursor: usize,
}

impl<'t, 'input> Source<'t, 'input> {
    pub(super) fn new(tokens: &'t [Token<'input>]) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub(super) fn next_token(&mut self) -> Option<&'t Token<'input>> {
        self.skip_trivia();

        let token = self.tokens.get(self.cursor)?;
        self.cursor += 1;

        Some(token)
    }

    /// Returns the last token's range if it exists.
    pub(crate) fn last_token_range(&self) -> Option<TextRange> {
        self.tokens.last().map(|token| token.text_range)
    }

    /// Peeks ahead at the next token's kind.
    ///
    /// Returns None on EOF
    pub(super) fn peek_kind(&mut self) -> Option<TokenKind> {
        self.skip_trivia();
        self.peek_kind_raw()
    }

    /// Peeks ahead at the next token.
    ///
    /// Returns None on EOF
    pub(super) fn peek_token(&mut self) -> Option<&Token> {
        self.skip_trivia();
        self.peek_token_raw()
    }

    fn skip_trivia(&mut self) {
        while self.peek_kind_raw().map_or(false, TokenKind::is_trivia) {
            self.cursor += 1;
        }
    }

    /// Peeks ahead at the next token's kind.
    ///
    /// Returns None on EOF
    fn peek_kind_raw(&self) -> Option<TokenKind> {
        // We can ignore the error when peeking
        Some(self.peek_token_raw()?.kind.unwrap_or(TokenKind::LexError))
    }

    fn peek_token_raw(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }
}
