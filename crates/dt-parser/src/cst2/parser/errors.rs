use dt_diagnostic::SpanLabel;
use std::borrow::Cow;

use super::super::TokenKind;

use crate::{cst2::lexer::LexError, TextRange};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Expected {
    Kind(TokenKind),
    PreprocessorDirective,
    Value,
    Cell,
}
impl core::fmt::Display for Expected {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Expected::Kind(kind) => kind.fmt(f),
            Expected::PreprocessorDirective => f.write_str("preprocessor directive"),
            Expected::Value => f.write_str("value"),
            Expected::Cell => f.write_str("cell"),
        }
    }
}
impl From<TokenKind> for Expected {
    fn from(value: TokenKind) -> Self {
        Self::Kind(value)
    }
}

/// CST parser error
// TODO: From rust-analyzer: If possible, errors are not reported during parsing and are postponed
// for a separate validation step. For example, parser accepts visibility modifiers on trait
// methods, but then a separate tree traversal flags all such visibilities as erroneous.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub message: Cow<'static, str>,
    pub primary_span: TextRange,
    pub span_labels: Vec<SpanLabel>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WrappedLexError<'input> {
    pub inner: LexError,
    pub text_range: TextRange,
    pub text: &'input str,
}
