use std::{iter::Peekable, str::Chars};

#[derive(thiserror::Error, Debug, displaydoc::Display, PartialEq, Eq)]
pub enum StringParseError {
    /// escape at end of string
    EscapeAtEndOfString,

    /// hex escape with no valid digits
    HexNoDigits,

    /// When parsing a char literal, there were too many chars.
    CharTooManyChars,

    /// When parsing a char literal, there was no char.
    CharMissingChar,

    /// missing literal delimiter
    MissingDelimiter,
}

struct InterpretEscapedString<'a> {
    s: Peekable<Chars<'a>>,
}

impl Iterator for InterpretEscapedString<'_> {
    type Item = Result<char, StringParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.s.next().map(|c| match c {
            '\\' => match self.s.next() {
                None => Err(StringParseError::EscapeAtEndOfString),
                Some('a') => Ok('\x07'),
                Some('b') => Ok('\x08'),
                Some('v') => Ok('\x0b'),
                Some('f') => Ok('\x0c'),
                Some('n') => Ok('\n'),
                Some('r') => Ok('\r'),
                Some('t') => Ok('\t'),
                Some('\\') => Ok('\\'),
                Some('x') => {
                    let Some(mut num) = self.s.next().and_then(|c| c.to_digit(16)) else {
                        return Err(StringParseError::HexNoDigits);
                    };
                    if let Some(second) = self.s.peek().and_then(|c| c.to_digit(16)) {
                        self.s.next();
                        num += second << 4;
                    }

                    #[expect(
                        clippy::cast_possible_truncation,
                        reason = "guaranteed to be in range"
                    )]
                    Ok(num as u8 as char)
                }
                Some(c) => Ok(c),
            },
            c => Ok(c),
        })
    }
}

/// Interprets an escaped string literal in a C-like language.
///
/// # Example
///
/// ```
/// use dt_tools_analyzer::string::{interpret_escaped_string, StringParseError};
///
/// assert_eq!(interpret_escaped_string(r#""\\a\a""#), Ok("\\a\x07".into()));
/// assert_eq!(interpret_escaped_string(r#""\""#), Err(StringParseError::EscapeAtEndOfString));
/// ```
///
/// # Errors
///
/// May return a [`StringParseError`] if the input is invalid.
pub fn interpret_escaped_string(s: &str) -> Result<String, StringParseError> {
    let s = s
        .strip_prefix('"')
        .ok_or(StringParseError::MissingDelimiter)?
        .strip_suffix('"')
        .ok_or(StringParseError::MissingDelimiter)?;
    (InterpretEscapedString {
        s: s.chars().peekable(),
    })
    .collect()
}

/// Interprets an escaped character literal in a C-like language.
///
/// # Example
///
/// ```
/// use dt_tools_analyzer::string::{interpret_escaped_char, StringParseError};
///
/// assert_eq!(interpret_escaped_char(r"'\a'"), Ok('\x07'));
/// assert_eq!(interpret_escaped_char("'a'"), Ok('a'));
/// assert_eq!(interpret_escaped_char("'aa'"), Err(StringParseError::CharTooManyChars));
/// assert_eq!(interpret_escaped_char(r"'\'"), Err(StringParseError::EscapeAtEndOfString));
/// ```
///
/// # Errors
///
/// May return a [`StringParseError`] if the input is invalid.
pub fn interpret_escaped_char(s: &str) -> Result<char, StringParseError> {
    let s = s
        .strip_prefix('\'')
        .ok_or(StringParseError::MissingDelimiter)?
        .strip_suffix('\'')
        .ok_or(StringParseError::MissingDelimiter)?;
    let mut iter = InterpretEscapedString {
        s: s.chars().peekable(),
    };
    let ch = iter.next().ok_or(StringParseError::CharMissingChar)??;

    if iter.next().is_some() {
        Err(StringParseError::CharTooManyChars)
    } else {
        Ok(ch)
    }
}
