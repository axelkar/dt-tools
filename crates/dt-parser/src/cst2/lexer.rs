//! Should we parse some stuff like numbers and strings here and store them in [`Token`]?
//!
//! How about ditching the source after lexing and storing all dynamic details in tuple enums?

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
pub struct Token<'input> {
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
    /// Not produced by Logos but by the parser
    LexError,

    // TODO: include more chars for this?
    //#[regex("[A-Za-z0-9-,_#]+", priority = 1)]
    #[regex(r#"[^ \t\r\n"/{}<>\[\]();:&=@]+"#)]
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

    #[token(",", priority = 3)]
    Comma,

    // -- Basic prop types --
    //#[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#)]
    #[token("\"", callback = lex_string)]
    String,

    #[regex(r"-?\d+", priority = 5)]
    #[regex(r"-?0[xX][0-9a-fA-F]*", priority = 5)]
    Number,

    // bytestring can be implemented with LBrack + Ident* + RBrack

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
    #[token("*", priority = 3)]
    Asterisk,
    #[token("+", priority = 3)]
    Plus,
    #[token("-", priority = 3)]
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
    #[inline(always)]
    pub fn is_trivia(self) -> bool {
        matches!(
            self,
            TokenKind::Whitespace | TokenKind::LineComment | TokenKind::BlockComment
        )
    }
}

impl core::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        use TokenKind::*;
        f.write_str(match self {
            Whitespace => "whitespace",
            BlockComment => "block comment",
            LineComment => "line comment",
            LexError => "an unrecognized token",
            Ident => "identifier",
            Equals => "‘=’",
            Semicolon => "‘;’",
            Ampersand => "‘&’",
            Colon => "‘:’",
            AtSign => "‘@’",
            Comma => "‘,’",
            String => "string literal",
            Number => "number literal",
            LCurly => "‘{’",
            RCurly => "‘}’",
            LAngle => "‘<’",
            RAngle => "‘>’",
            LBrack => "‘[’",
            RBrack => "‘]’",
            LParen => "‘(’",
            RParen => "‘)’",
            Slash => "‘/’",
            Asterisk => "‘*’",
            Plus => "‘+’",
            Minus => "‘-’",
        })
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
    fn lex_number_literals() {
        check("123", TokenKind::Number);
        check("0xdeadbeef", TokenKind::Number);
    }

    #[test]
    fn lex_number_and_comma() {
        // This syntax is rejected in cells in dtc, but not as prop names
        check("123,", TokenKind::Ident);
    }

    #[test]
    fn lex_string_literals() {
        check(r#""abc""#, TokenKind::String);
        check(r#""abc\a\b\e\f\n\r\t\v\\\'\x01""#, TokenKind::String); // escapes
        check(r#""abc\"""#, TokenKind::String); // \" escape
        check(r#""åäö""#, TokenKind::String); // 2-byte letters
        check("\"\n\"", TokenKind::String); // newline
        check(r#""📦""#, TokenKind::String); // 4-byte emoji
        check(r#""你好""#, TokenKind::String); // 3-byte characters

        // Unterminated string
        let mut lexer = TokenKind::lexer(r#""abc\";"#);
        assert_eq!(lexer.next(), Some(Err(LexError::UnexpectedEofString)));
        assert_eq!(lexer.slice(), r#""abc\";"#);
    }

    fn test_expected(src: &str, expected: &str) {
        use std::fmt::Write as _;

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
    fn lex_from_test_data_1() {
        let src = include_str!("../../test_data/1.dts");
        let expected = include_str!("../../test_data/1.lex.expected");

        test_expected(src, expected);
    }

    #[test]
    fn lex_identifiers() {
        check("/", TokenKind::Slash);
        check("#a-cells", TokenKind::Ident); // hashtags
        check("PREPROCESSOR", TokenKind::Ident);
        check("åäö", TokenKind::Ident); // 2-byte letters
        check("test1234", TokenKind::Ident); // digits in an identifier
        check("a-b", TokenKind::Ident); // has minus
        check("-a", TokenKind::Ident); // has minus
        check("a-", TokenKind::Ident); // has minus
        check("123test", TokenKind::Ident); // begins with digits
    }

    #[test]
    fn lex_minus() {
        check("-", TokenKind::Minus);
    }

    #[test]
    fn lex_ident_and_whitespace() {
        let mut lexer = TokenKind::lexer("hello world");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Ident)));
        assert_eq!(lexer.slice(), "hello");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Whitespace)));
        assert_eq!(lexer.slice(), " ");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Ident)));
        assert_eq!(lexer.slice(), "world");
    }
}
