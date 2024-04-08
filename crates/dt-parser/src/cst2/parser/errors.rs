use super::super::TokenKind;
use smallvec::SmallVec;

use crate::{cst2::lexer::LexError, TextRange};

/// CST parser error
// TODO: From rust-analyzer: If possible, errors are not reported during parsing and are postponed
// for a separate validation step. For example, parser accepts visibility modifiers on trait
// methods, but then a separate tree traversal flags all such visibilities as erroneous.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub expected: SmallVec<[TokenKind; 2]>,
    pub found: Option<TokenKind>,
    pub text_range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WrappedLexError<'input> {
    pub inner: LexError,
    pub text_range: TextRange,
    pub text: &'input str,
}

impl core::fmt::Display for ParseError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str("Expected ")?;

        let num_expected = self.expected.len();
        let is_first = |idx| idx == 0;
        let is_last = |idx| idx == num_expected - 1;

        for (idx, expected_kind) in self.expected.iter().enumerate() {
            if is_first(idx) {
                write!(f, "{}", expected_kind)?;
            } else if is_last(idx) {
                write!(f, " or {}", expected_kind)?;
            } else {
                write!(f, ", {}", expected_kind)?;
            }
        }

        if self.expected.is_empty() {
            write!(f, "nothing")?;
        }

        if let Some(found) = self.found {
            write!(f, ", but found {}", found)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::Range as StdRange;

    use pretty_assertions::assert_eq;

    fn check(
        expected: Vec<TokenKind>,
        found: Option<TokenKind>,
        range: StdRange<usize>,
        output: &str,
    ) {
        let error = ParseError {
            expected: expected.into(),
            found,
            text_range: range.into(),
        };

        assert_eq!(format!("{}", error), output);
    }

    #[test]
    fn one_expected_did_find() {
        check(
            vec![TokenKind::Equals],
            Some(TokenKind::Ident),
            10..20,
            "Expected ‘=’, but found identifier",
        );
    }

    #[test]
    fn one_expected_did_not_find() {
        check(
            vec![TokenKind::RParen],
            None,
            5..6,
            "Expected ‘)’",
        );
    }

    #[test]
    fn multiple_expected_did_find() {
        check(
            vec![
                TokenKind::Number,
                TokenKind::Ident,
                TokenKind::Minus,
                TokenKind::LParen,
            ],
            Some(TokenKind::RBrack),
            100..105,
            "Expected number literal, identifier, ‘-’ or ‘(’, but found ‘]’",
        );
    }
}
