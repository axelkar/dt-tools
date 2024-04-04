//! Should we parse some stuff like numbers and strings here and store them in [`Token`]?
//!
//! How about ditching the source after lexing and storing all non-trivial details in tuple enums?

use logos::Logos;

use crate::TextRange;

#[derive(Debug, Clone)]
/// [`logos::Lexer`] wrapper for also returning the slice in the iterator
pub(crate) struct Lexer<'input> {
    inner: logos::Lexer<'input, TokenKind>,
}

impl<'input> Lexer<'input> {
    pub(crate) fn new(input: &'input str) -> Self {
        Self {
            inner: TokenKind::lexer(input),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        let text = self.inner.slice();
        let text_range = self.inner.span().into();

        Some(Token {
            kind,
            text,
            text_range,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Token<'input> {
    pub(crate) kind: Result<TokenKind, LexError>,
    pub(crate) text: &'input str,
    pub(crate) text_range: TextRange,
}

#[derive(thiserror::Error, Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LexError {
    #[error("Unexpected EOF (hint: unterminated string literal)")]
    UnexpectedEofString,

    #[error("Unexpected EOF (hint: unterminated block comment)")]
    UnexpectedEofBlockComment,

    #[default]
    #[error("Unexpected input or EOF")]
    Default,
}

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[logos(error = LexError)]
// TODO: #[logos(source = &[u8])] ?
pub enum TokenKind {
    #[regex("[ \t\r\n]+")]
    Whitespace,

    // Supports `/* /* */` but not `/* /* */ */` or `/* */ */`
    #[token("/*", callback = lex_block_comment)]
    BlockComment,

    #[regex(r"//[^\n\r]+")]
    LineComment,

    /// An error!!
    ///
    /// Not outputted by Logos but by the parser
    LexerError,

    // TODO: include more chars for this?
    //#[regex("[A-Za-z0-9-,_#]+", priority = 1)]
    #[regex(r#"[^ \t\r\n"/{}<>\[\]();:&]+"#, priority = 1)]
    Ident,

    #[token("=")]
    Equals,

    #[token(";")]
    Semicolon,

    #[regex("&")]
    Ampersand,

    #[token(":")]
    Colon,

    #[token("@")]
    AtSign,

    #[token(",")]
    Comma,

    // -- Basic prop types --
    //#[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#)]
    #[token("\"", callback = lex_string)]
    String,

    #[regex(r"-?\d*")]
    #[regex(r"-?0[xX][0-9a-fA-F]*")]
    Number,

    // TODO: bytestring

    // -- Curly braces, angle brackets, brackets, parenthesis --
    #[token("{")]
    LCurly,
    #[token("}")]
    RCurly,
    #[token("<")]
    LAngle,
    #[token(">")]
    RAngle,
    #[token("[")]
    LBrack,
    #[token("]")]
    RBrack,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    // -- Math symbols --
    #[token("/")]
    Slash,
    #[token("*")]
    Asterisk,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
}

impl TokenKind {
    pub fn static_text(self) -> Option<&'static str> {
        use TokenKind::*;
        Some(match self {
            Equals => "=",
            Semicolon => ";",
            Ampersand => "&",
            Colon => ":",
            AtSign => "@",
            Comma => ",",
            LCurly => "{",
            RCurly => "}",
            LAngle => "<",
            RAngle => ">",
            LBrack => "[",
            RBrack => "]",
            LParen => "(",
            RParen => ")",
            Slash => "/",
            Asterisk => "*",
            Plus => "+",
            Minus => "-",
            _ => return None,
        })
    }
    /// Returns true for comment and whitespace token kinds.
    pub fn is_trivia(self) -> bool {
        matches!(
            self,
            TokenKind::Whitespace | TokenKind::LineComment | TokenKind::BlockComment
        )
    }
}

fn lex_block_comment(lex: &mut logos::Lexer<TokenKind>) -> Result<(), LexError> {
    let remainder: &str = lex.remainder();
    let mut asterisk_found = false;
    let mut total_len = 0;

    for c in remainder.chars() {
        total_len += c.len_utf8();

        if c == '*' {
            asterisk_found = true;
            continue;
        }

        if c == '/' && asterisk_found {
            lex.bump(remainder[0..total_len].as_bytes().len());
            return Ok(());
        }

        asterisk_found = false;
    }
    lex.bump(remainder[0..total_len].as_bytes().len());
    Err(LexError::UnexpectedEofBlockComment)
}

fn lex_string(lex: &mut logos::Lexer<TokenKind>) -> Result<(), LexError> {
    let remainder: &str = lex.remainder();
    let mut escaped = false;
    let mut total_len = 0;

    for c in remainder.chars() {
        total_len += c.len_utf8();

        if c == '\\' {
            escaped = !escaped;
            continue;
        }

        if c == '"' && !escaped {
            lex.bump(remainder[0..total_len].as_bytes().len());
            return Ok(());
        }

        escaped = false;
    }
    lex.bump(remainder[0..total_len].as_bytes().len());
    Err(LexError::UnexpectedEofString)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(input: &str, kind: TokenKind) {
        let lexer = Lexer::new(input);
        assert_eq!(
            lexer.collect::<Vec<_>>(),
            vec![Token {
                kind: Ok(kind),
                text: input,
                text_range: TextRange {
                    start: 0,
                    end: input.len()
                }
            }]
        );
    }

    #[test]
    fn lex_spaces() {
        check("     ", TokenKind::Whitespace);
    }

    #[test]
    fn lex_comments() {
        check("/* content */", TokenKind::BlockComment);
        check("/*\n * content\n **/", TokenKind::BlockComment);
        check("/******/", TokenKind::BlockComment);
        check("// hello", TokenKind::LineComment);
    }

    #[test]
    fn lex_numbers() {
        // FIXME: Right now it parses `123` as Ident
        // Do I need to use extras?
        //check("123", TokenKind::Number);
    }

    #[test]
    fn lex_strings() {
        check(r#""abc""#, TokenKind::String);
        check(r#""abc\n""#, TokenKind::String);
        check(r#""abc\"""#, TokenKind::String);
        check("\"åäö\n\"", TokenKind::String);

        let mut lexer = TokenKind::lexer(r#""abc\";"#);
        assert_eq!(lexer.next(), Some(Err(LexError::UnexpectedEofString)));
        assert_eq!(lexer.span(), 0..7);
        assert_eq!(lexer.slice(), r#""abc\";"#);
    }

    #[test]
    fn lex_from_test_data() {
        use std::fmt::Write as _;

        let src = include_str!("../../test_data/1.dts");
        let expected = include_str!("../../test_data/1.lex.expected");
        let mut output = String::new();

        let mut lexer = TokenKind::lexer(src);
        while let Some(token) = lexer.next() {
            let span = lexer.span();
            let slice = lexer.slice();
            match token {
                Ok(token) => writeln!(&mut output, "{token:?}@{span:?} {slice:?}").unwrap(),
                Err(err) => writeln!(&mut output, "ERROR: {err:?}@{span:?} {slice:?}").unwrap(),
            }
        }
        pretty_assertions::assert_eq!(expected, output);
    }

    #[test]
    fn lex_ident() {
        let mut lexer = TokenKind::lexer("/");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Slash)));
        assert_eq!(lexer.span(), 0..1);
        assert_eq!(lexer.slice(), "/");

        let mut lexer = TokenKind::lexer("#a-cells");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Ident)));
        assert_eq!(lexer.span(), 0..8);
        assert_eq!(lexer.slice(), "#a-cells");

        let mut lexer = TokenKind::lexer("PREPROCESSOR");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Ident)));
        assert_eq!(lexer.span(), 0..12);
        assert_eq!(lexer.slice(), "PREPROCESSOR");
    }

    #[test]
    fn lex_ident_and_whitespace() {
        let mut lexer = TokenKind::lexer("hello world");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Ident)));
        assert_eq!(lexer.span(), 0..5);
        assert_eq!(lexer.slice(), "hello");

        assert_eq!(lexer.next(), Some(Ok(TokenKind::Whitespace)));
        assert_eq!(lexer.span(), 5..6);
        assert_eq!(lexer.slice(), " ");

        assert_eq!(lexer.next(), Some(Ok(TokenKind::Ident)));
        assert_eq!(lexer.span(), 6..11);
        assert_eq!(lexer.slice(), "world");
    }
}
