use crate::cst2::lexer::{Token, TokenKind};

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

    /// Peeks ahead at the next token's kind.
    ///
    /// Returns None on EOF
    pub(super) fn peek_kind(&mut self) -> Option<TokenKind> {
        self.skip_trivia();
        self.peek_kind_raw()
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
        match self.tokens.get(self.cursor)?.kind {
            Ok(kind) => Some(kind),
            // We can ignore the error when peeking
            Err(_err) => Some(TokenKind::LexerError),
        }
    }
}
