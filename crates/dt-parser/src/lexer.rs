//! The module containing the lexer.

// TODO: Should we parse some stuff like strings here and store them in [`Token`]?
// TODO: How about ditching the source after lexing and storing all dynamic details in tuple enums?

use logos::Logos;

use crate::TextRange;

/// Runs the lexer and returns a list of tokens.
///
/// # Example
///
/// ```
/// use dt_parser::lexer::{lex, TokenKind, LexError};
/// let tokens = lex("example 1\"");
///
/// assert_eq!(tokens[0].kind, Ok(TokenKind::Ident));
/// assert_eq!(tokens[1].kind, Ok(TokenKind::Whitespace));
/// assert_eq!(tokens[2].kind, Ok(TokenKind::Number));
/// assert_eq!(tokens[3].kind, Err(LexError::UnexpectedEofString));
/// assert!(tokens.get(4).is_none());
/// ```
#[must_use]
pub fn lex(input: &str) -> Vec<Token> {
    Lexer::new(input).collect()
}

/// [`logos::Lexer`] wrapper for also returning the slice and text range in the iterator
#[derive(Debug, Clone)]
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

/// A structure representing a token from the lexer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'input> {
    pub kind: Result<TokenKind, LexError>,
    pub text: &'input str,
    pub text_range: TextRange,
}

/// An error returned from the lexer.
#[derive(thiserror::Error, Debug, Clone, Copy, PartialEq, Eq, Default, displaydoc::Display)]
pub enum LexError {
    /// Unexpected EOF (hint: unterminated incbin directive)
    UnexpectedEofIncbin,

    /// Missing ( in incbin directive
    IncbinMissingLParen,

    /// Missing ) in incbin directive
    IncbinMissingRParen,

    /// Missing number in incbin directive
    IncbinMissingNumber,

    /// Missing comma in incbin directive
    IncbinMissingComma,

    /// Missing string in incbin directive
    IncbinMissingString,

    /// Unexpected EOF (hint: unterminated string literal)
    UnexpectedEofString,

    /// Unexpected EOF (hint: unterminated bytestring literal)
    UnexpectedEofBytestring,

    /// Unexpected EOF (hint: unterminated char literal)
    UnexpectedEofChar,

    /// Unexpected EOF (hint: unterminated block comment)
    UnexpectedEofBlockComment,

    /// Unexpected input or EOF
    #[default]
    Default,
}

/// The kind of a token
#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[logos(error = LexError)]
// TODO: #[logos(source = [u8])] ?
pub enum TokenKind {
    #[regex("[ \t\r\n]+")]
    Whitespace,

    // Supports `/* /* */` but not `/* /* */ */` or `/* */ */`
    #[token("/*", callback = lex_block_comment)]
    BlockComment,

    #[token(",")]
    Comma,

    #[regex(r"//[^\n\r]+")]
    LineComment,

    #[regex(r"#( |\t)*undef[^\n]*")]
    UndefDirective,
    #[regex(r"#( |\t)*pragma[^\n]*")]
    PragmaDirective,
    #[regex(r"#( |\t)*else[^\n]*")]
    ElseDirective,
    #[regex(r"#( |\t)*endif[^\n]*")]
    EndifDirective,
    #[regex(r"#( |\t)*ifndef[^\n]*")]
    IfndefDirective,
    #[regex(r"#( |\t)*ifdef[^\n]*")]
    IfdefDirective,
    #[regex(r"#( |\t)*if[^\n]*")]
    IfDirective,
    #[regex(r#"#( |\t)*define[^\n\\"'/]*"#, callback = lex_preprocessor_directive)]
    DefineDirective,
    #[regex(r"#( |\t)*include[^\n]*")]
    IncludeDirective,

    // TODO: bits directive
    // TODO: plugin directive
    #[token("/bits/")]
    BitsDirective,

    #[token("/include/")]
    DtIncludeDirective,

    #[token("/memreserve/")]
    MemreserveDirective,

    #[token("/delete-node/")]
    DeleteNodeDirective,

    #[token("/delete-property/")]
    DeletePropertyDirective,

    #[token("/dts-v1/")]
    V1Directive,

    #[token("/plugin/")]
    PluginDirective,

    /// A lex error!
    Unrecognized,

    /// **Combined token**, which is only generated in the parser.
    Name,

    #[regex(r#"[^ \t\r\n"'/*+%|{}<>\[()?;:&=@,0-9-][^ \t\r\n"'/*+%|{}<>\[()?;:&=@,-]*"#)]
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

    // -- Basic prop types --
    #[token("\"", callback = lex_string)]
    String,

    /// A single character literal like `'\x41'`, `'a'` or `'\0'`
    #[token("'", callback = lex_char)]
    Char,

    // I would have wanted to do [0-1_]*[0-1][0-1_]* but it isn't possible without backtracking.
    // Maybe I should write a callback
    #[regex("[0-9][0-9_]*((u|i)(8|16|32|64|128))?")]
    #[regex("0b[0-1_]+((u|i)(8|16|32|64|128))?")]
    #[regex("0o[0-7_]+((u|i)(8|16|32|64|128))?")]
    #[regex("0x[0-9a-fA-F_]+((u|i)(8|16|32|64|128))?")]
    Number,

    #[token("[", callback = lex_bytestring)]
    DtBytestring,

    // -- Curly braces, angle brackets, brackets, parenthesis --
    #[token("{")]
    LCurly,
    #[token("}")]
    RCurly,
    #[token("<")]
    LAngle,
    #[token(">")]
    RAngle,
    //#[token("[")]
    //LBrack,
    //#[token("]")]
    //RBrack,
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
    #[token("%")]
    Modulo,
    #[token("|")]
    BitwiseOr,
}

impl TokenKind {
    /// Returns the static text of the token kind, if applicable.
    /// ```
    /// use dt_parser::lexer::TokenKind;
    /// assert_eq!(TokenKind::Comma.static_text(), Some(","));
    /// assert_eq!(TokenKind::Ident.static_text(), None);
    /// ```
    #[must_use]
    pub fn static_text(self) -> Option<&'static str> {
        Some(match self {
            TokenKind::Comma => ",",
            TokenKind::BitsDirective => "/bits/",
            TokenKind::DtIncludeDirective => "/include/",
            TokenKind::MemreserveDirective => "/memreserve/",
            TokenKind::DeleteNodeDirective => "/delete-node/",
            TokenKind::DeletePropertyDirective => "/delete-property/",
            TokenKind::V1Directive => "/dts-v1/",
            TokenKind::PluginDirective => "/plugin/",
            TokenKind::Equals => "=",
            TokenKind::Semicolon => ";",
            TokenKind::Ampersand => "&",
            TokenKind::Colon => ":",
            TokenKind::AtSign => "@",
            TokenKind::LCurly => "{",
            TokenKind::RCurly => "}",
            TokenKind::LAngle => "<",
            TokenKind::RAngle => ">",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::Slash => "/",
            TokenKind::Asterisk => "*",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Modulo => "%",
            TokenKind::BitwiseOr => "|",
            _ => return None,
        })
    }

    /// Returns true for comment and whitespace token kinds.
    #[inline]
    #[must_use]
    pub fn is_trivia(self) -> bool {
        matches!(
            self,
            TokenKind::Whitespace | TokenKind::LineComment | TokenKind::BlockComment
        )
    }

    /// Returns true for preprocessor directive token kinds.
    #[inline]
    #[must_use]
    pub fn is_preprocessor_directive(self) -> bool {
        matches!(
            self,
            TokenKind::UndefDirective
                | TokenKind::PragmaDirective
                | TokenKind::ElseDirective
                | TokenKind::EndifDirective
                | TokenKind::IfndefDirective
                | TokenKind::IfdefDirective
                | TokenKind::IfDirective
                | TokenKind::DefineDirective
                | TokenKind::IncludeDirective
        )
    }
}

impl core::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str(match self {
            TokenKind::Whitespace => "whitespace",
            TokenKind::BlockComment => "block comment",
            TokenKind::LineComment => "line comment",
            TokenKind::Comma => "‘,’",
            TokenKind::UndefDirective => "‘#undef‘ preprocessor directive",
            TokenKind::PragmaDirective => "‘#pragma‘ preprocessor directive",
            TokenKind::ElseDirective => "‘#else‘ preprocessor directive",
            TokenKind::EndifDirective => "‘#endif‘ preprocessor directive",
            TokenKind::IfndefDirective => "‘#ifndef‘ preprocessor directive",
            TokenKind::IfdefDirective => "‘#ifdef‘ preprocessor directive",
            TokenKind::IfDirective => "‘#if‘ preprocessor directive",
            TokenKind::DefineDirective => "‘#define‘ preprocessor directive",
            TokenKind::IncludeDirective => "‘#include‘ preprocessor directive",
            TokenKind::BitsDirective => "‘/bits/‘",
            TokenKind::DtIncludeDirective => "‘/include/‘",
            TokenKind::MemreserveDirective => "‘/memreserve/‘",
            TokenKind::DeleteNodeDirective => "‘/delete-node/‘",
            TokenKind::DeletePropertyDirective => "‘/delete-property/‘",
            TokenKind::V1Directive => "‘/dts-v1/‘",
            TokenKind::PluginDirective => "‘/plugin/‘",
            TokenKind::Unrecognized => "unrecognized token",
            TokenKind::Name => "name",
            TokenKind::Ident => "identifier",
            TokenKind::Equals => "‘=’",
            TokenKind::Semicolon => "‘;’",
            TokenKind::Ampersand => "‘&’",
            TokenKind::Colon => "‘:’",
            TokenKind::AtSign => "‘@’",
            TokenKind::String => "string literal",
            TokenKind::Char => "character literal",
            TokenKind::Number => "number literal",
            TokenKind::DtBytestring => "bytestring literal",
            TokenKind::LCurly => "‘{’",
            TokenKind::RCurly => "‘}’",
            TokenKind::LAngle => "‘<’",
            TokenKind::RAngle => "‘>’",
            TokenKind::LParen => "‘(’",
            TokenKind::RParen => "‘)’",
            TokenKind::Slash => "‘/’",
            TokenKind::Asterisk => "‘*’",
            TokenKind::Plus => "‘+’",
            TokenKind::Minus => "‘-’",
            TokenKind::Modulo => "‘%’",
            TokenKind::BitwiseOr => "‘|’",
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
            lex.bump(total_len);
            return Ok(());
        }

        asterisk_found = false;
    }
    lex.bump(total_len);
    Err(LexError::UnexpectedEofBlockComment)
}

fn lex_preprocessor_directive(lex: &mut logos::Lexer<TokenKind>) -> Result<(), LexError> {
    /// take [^\n\\"'/]*
    fn take(lex: &mut logos::Lexer<TokenKind>) {
        let remainder: &str = lex.remainder();
        let mut total_len = 0;

        for c in remainder.chars() {
            if let '\n' | '\\' | '"' | '\'' | '/' = c {
                lex.bump(total_len);
                break;
            }

            total_len += c.len_utf8();
        }
    }
    /// take [^\n\r]+
    fn take_line_comment(lex: &mut logos::Lexer<TokenKind>) {
        let remainder: &str = lex.remainder();
        let mut total_len = 0;

        for c in remainder.chars() {
            if let '\n' | '\r' = c {
                lex.bump(total_len);
                break;
            }

            total_len += c.len_utf8();
        }
    }

    let remainder: &str = lex.remainder();

    match remainder.as_bytes().first() {
        Some(b'\\') => {
            let mut escaped = false;
            let mut total_len = 0;

            for c in remainder.chars() {
                total_len += c.len_utf8();

                if c == '\\' {
                    escaped = !escaped;
                    continue;
                }

                if c == '\n' && !escaped {
                    lex.bump(total_len);
                    return Ok(());
                }

                escaped = false;
            }
            lex.bump(total_len);
        }
        Some(b'\'') => {
            lex.bump(1);
            // This makes sure /* inside character literals don't get parsed as block comments
            lex_char(lex)?;

            take(lex);
            return lex_preprocessor_directive(lex);
        }
        Some(b'"') => {
            lex.bump(1);
            // This makes sure /* inside strings don't get parsed as block comments
            lex_preprocessor_string(lex)?;

            take(lex);
            return lex_preprocessor_directive(lex);
        }
        Some(b'/') => match remainder.as_bytes().get(1) {
            Some(&b'*') => {
                lex.bump(2);
                lex_block_comment(lex)?;

                take(lex);
                return lex_preprocessor_directive(lex);
            }
            Some(&b'/') => {
                lex.bump(2);
                take_line_comment(lex);
            }
            _ => {
                lex.bump(1);

                take(lex);
                return lex_preprocessor_directive(lex);
            }
        },
        _ => {}
    }

    Ok(())
}

// preprocessor to dtc:
// in quotes = "a\\\nb" -> ab
// in quotes = "a\nb" -> only "a gets expanded and b" gets left at macro definition
// in quotes = a\nb -> a\nb
// in quotes = a\\"b -> a\\"b
// in quotes = a\"b -> a\"b
// in quotes = "a\\\\\nb" -> a\b
// FIXME: this lexes "a\nb" succesfully, but it should be a deny error
fn lex_preprocessor_string(lex: &mut logos::Lexer<TokenKind>) -> Result<(), LexError> {
    let remainder: &str = lex.remainder();
    let mut escaped = false;
    let mut total_len = 0;

    for c in remainder.chars() {
        total_len += c.len_utf8();

        if c == '\\' {
            escaped = true;
            continue;
        }

        if c == '"' && !escaped {
            lex.bump(total_len);
            return Ok(());
        }

        escaped = false;
    }
    lex.bump(total_len);
    Err(LexError::UnexpectedEofString)
}

// dtc:
// in quotes = "a\\\nb" -> syntax error
// in quotes = "a\nb" -> "a\nb"
// in quotes = "a\\nb" -> "a\nb"
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
            lex.bump(total_len);
            return Ok(());
        }

        escaped = false;
    }
    lex.bump(total_len);
    Err(LexError::UnexpectedEofString)
}

fn lex_bytestring(lex: &mut logos::Lexer<TokenKind>) -> Result<(), LexError> {
    let remainder: &str = lex.remainder();
    let mut total_len = 0;

    // TODO: benchmark as_bytes vs chars
    for c in remainder.chars() {
        total_len += c.len_utf8();

        // TODO: warn about invalid bytestring digits
        if c == ']' {
            lex.bump(total_len);
            return Ok(());
        }
    }
    lex.bump(total_len);
    Err(LexError::UnexpectedEofBytestring)
}

fn lex_char(lex: &mut logos::Lexer<TokenKind>) -> Result<(), LexError> {
    let remainder: &str = lex.remainder();
    let mut escaped = false;
    let mut total_len = 0;

    for c in remainder.chars() {
        total_len += c.len_utf8();

        if c == '\\' {
            escaped = !escaped;
            continue;
        }

        if c == '\'' && !escaped {
            lex.bump(total_len);
            return Ok(());
        }

        escaped = false;
    }
    lex.bump(total_len);
    Err(LexError::UnexpectedEofChar)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[track_caller]
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
    fn lex_semicolon() {
        check(";", TokenKind::Semicolon);
    }

    #[test]
    fn lex_preprocessor_directive() {
        check("#define FOO BAR", TokenKind::DefineDirective);
        check("#define FOO 123", TokenKind::DefineDirective);
        check("#define FOO \"bar\"", TokenKind::DefineDirective);
        check("#define FOO <123>", TokenKind::DefineDirective);
        check("#include \"a.dtsi\"", TokenKind::IncludeDirective);
        check("#        define CONFIG_FOO", TokenKind::DefineDirective);
        check("#ifdef FOO", TokenKind::IfdefDirective);
        check("#else", TokenKind::ElseDirective);
    }

    #[test]
    fn lex_number_literals() {
        check("123", TokenKind::Number);
        check("1_000_000", TokenKind::Number);
        check("0xdeadbeef", TokenKind::Number);
        check("0x07", TokenKind::Number);
        check("0o777", TokenKind::Number);
        check("0b00101010", TokenKind::Number);

        // TODO: is this syntax in DTC? used out in the wild? in the spec?
        //check("123U", TokenKind::Number);
        //check("123L", TokenKind::Number);
        //check("123UL", TokenKind::Number);
        //check("123LL", TokenKind::Number);
        //check("123ULL", TokenKind::Number);
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

    #[test]
    fn lex_bytestring_literals() {
        check("[0123456789abcdef]", TokenKind::DtBytestring);
        check("[  01 23 45 6789 a b ]", TokenKind::DtBytestring);

        // Unterminated string
        let mut lexer = TokenKind::lexer("[1234");
        assert_eq!(lexer.next(), Some(Err(LexError::UnexpectedEofBytestring)));
        assert_eq!(lexer.slice(), "[1234");
    }

    #[track_caller]
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
        let src = include_str!("../test_data/1.dts");
        let expected = include_str!("../test_data/1.lex.expected");

        test_expected(src, expected);
    }

    #[test]
    fn lex_from_test_data_2_macros() {
        let src = include_str!("../test_data/2-macro-def.dts");
        let expected = include_str!("../test_data/2-macro-def.lex.expected");

        test_expected(src, expected);
    }

    #[test]
    fn lex_identifiers() {
        check("/", TokenKind::Slash);
        check("#a", TokenKind::Ident); // hashtags
        check("DEFINE_MACRO", TokenKind::Ident);
        check("åäö", TokenKind::Ident); // 2-byte letters
    }

    #[test]
    fn lex_minus_identifiers() {
        let mut lexer = TokenKind::lexer("#a-cells");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Ident)));
        assert_eq!(lexer.slice(), "#a");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Minus)));
        assert_eq!(lexer.slice(), "-");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Ident)));
        assert_eq!(lexer.slice(), "cells");

        let mut lexer = TokenKind::lexer("-a");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Minus)));
        assert_eq!(lexer.slice(), "-");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Ident)));
        assert_eq!(lexer.slice(), "a");

        let mut lexer = TokenKind::lexer("a-");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Ident)));
        assert_eq!(lexer.slice(), "a");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Minus)));
        assert_eq!(lexer.slice(), "-");
    }

    #[test]
    fn lex_number_identifiers() {
        let mut lexer = TokenKind::lexer("123abc");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Number)));
        assert_eq!(lexer.slice(), "123");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Ident)));
        assert_eq!(lexer.slice(), "abc");

        // Macro names (which should be only one Ident) are allowed to have numbers
        let mut lexer = TokenKind::lexer("abc123");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::Ident)));
        assert_eq!(lexer.slice(), "abc123");
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
