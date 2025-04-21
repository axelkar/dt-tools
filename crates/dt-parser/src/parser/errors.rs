use dt_diagnostic::SpanLabel;
use itertools::Itertools;
use std::borrow::Cow;
use tracing::debug;

use super::{Marker, Parser};

use crate::{
    cst::NodeKind,
    lexer::{LexError, TokenKind},
    TextRange,
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Expected {
    Kind(TokenKind),
    PreprocessorDirective,
    Value,
    Cell,
    LabelName,
    Eof,
}
impl core::fmt::Display for Expected {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Expected::Kind(kind) => kind.fmt(f),
            Expected::PreprocessorDirective => f.write_str("preprocessor directive"),
            Expected::Value => f.write_str("value"),
            Expected::Cell => f.write_str("cell"),
            Expected::LabelName => f.write_str("label name"),
            Expected::Eof => f.write_str("end-of-file"),
        }
    }
}

/// Generates an error message using [`Parser::expected`] and [`Parser::peek`].
fn fmt_expected_message(p: &mut Parser) -> String {
    #[cfg(feature = "grammar-tracing")]
    error!("emit_expect_error");

    use std::fmt::Write;
    let mut message = "Expected ".to_owned();

    p.expected = std::mem::take(&mut p.expected)
        .into_iter()
        .unique()
        .collect::<Vec<_>>()
        .into();

    let num_expected = p.expected.len();

    for (idx, expected_kind) in p.expected.iter().enumerate() {
        let is_first = idx == 0;
        let is_last = idx == num_expected - 1;

        if is_first {
            write!(message, "{expected_kind}").ok();
        } else if is_last {
            write!(message, " or {expected_kind}").ok();
        } else {
            write!(message, ", {expected_kind}").ok();
        }
    }

    if let Some(kind) = p.peek() {
        write!(message, ", but found {kind}").ok();
    } else {
        write!(message, ", but found end-of-file").ok();
    }

    message
}

/// An error from the parser.
// TODO: From rust-analyzer: If possible, errors are not reported during parsing and are postponed
// for a separate validation step. For example, parser accepts visibility modifiers on trait
// methods, but then a separate tree traversal flags all such visibilities as erroneous.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    /// The error message
    pub message: Cow<'static, str>,
    /// The primary range of text this error points to
    pub primary_span: TextRange,
    /// Additional hints in the error
    pub span_labels: Vec<SpanLabel>,
}

/// [`LexError`] wrapped with a text range and the source text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WrappedLexError<'input> {
    pub inner: LexError,
    pub text_range: TextRange,
    pub text: &'input str,
}

#[doc(hidden)]
pub struct MessageMissing;
#[doc(hidden)]
pub struct MessageFilled(Cow<'static, str>);

#[derive(PartialEq, Eq)]
enum Bump {
    No,
    Yes,
    YesWrapErr,
}

pub struct ErrorBuilder<'p, 't, 'input, MsgState> {
    p: &'p mut Parser<'t, 'input>,
    m: Option<Marker>,
    bump: Bump,
    msg: MsgState,
    span_labels: Vec<SpanLabel>,
    hints: Vec<Cow<'static, str>>,
}
impl<'p, 't, 'input> ErrorBuilder<'p, 't, 'input, MessageMissing> {
    #[inline]
    #[must_use]
    pub(super) fn new(p: &'p mut Parser<'t, 'input>) -> Self {
        ErrorBuilder {
            p,
            m: None,
            bump: Bump::No,
            msg: MessageMissing,
            span_labels: Vec::new(),
            hints: Vec::new(),
        }
    }
}
impl<'p, 't, 'input, MsgState> ErrorBuilder<'p, 't, 'input, MsgState> {
    /// Completes a marker as [`NodeKind::ParseError`]. If used with `bump`, includes the token
    /// into the node.
    #[inline]
    #[must_use]
    pub fn complete(mut self, m: Marker) -> Self {
        self.m = Some(m);
        self
    }

    /// Bumps a token if not at the end.
    #[inline]
    #[must_use]
    pub fn bump(mut self) -> Self {
        self.bump = Bump::Yes;
        self
    }
    /// Bumps a token and wraps it with [`NodeKind::ParseError`] if not at the end.
    #[inline]
    #[must_use]
    pub fn bump_wrap_err(mut self) -> Self {
        self.bump = Bump::YesWrapErr;
        self
    }

    /// Gets the error from `Parser::expected`.
    #[inline]
    #[must_use]
    pub fn msg_expected(self) -> ErrorBuilder<'p, 't, 'input, MessageFilled> {
        ErrorBuilder {
            msg: MessageFilled(Cow::Owned(fmt_expected_message(self.p))),
            p: self.p,
            m: self.m,
            bump: self.bump,
            span_labels: self.span_labels,
            hints: self.hints,
        }
    }

    /// Sets a custom message.
    #[inline]
    #[must_use]
    pub fn msg_custom(self, msg: Cow<'static, str>) -> ErrorBuilder<'p, 't, 'input, MessageFilled> {
        ErrorBuilder {
            p: self.p,
            m: self.m,
            bump: self.bump,
            msg: MessageFilled(msg),
            span_labels: self.span_labels,
            hints: self.hints,
        }
    }

    /// Adds a span label.
    #[inline]
    #[must_use]
    pub fn add_span_label(mut self, span: TextRange, msg: Cow<'static, str>) -> Self {
        self.span_labels.push(SpanLabel { span, msg });
        self
    }

    /// Adds a hint to the same position as the error.
    ///
    /// The position is resolved in the emit function.
    #[inline]
    #[must_use]
    pub fn add_hint(mut self, msg: Cow<'static, str>) -> Self {
        self.hints.push(msg);
        self
    }
}

impl ErrorBuilder<'_, '_, '_, MessageFilled> {
    /// Does the things specified in the builder.
    #[inline]
    pub fn emit(mut self) {
        let primary_span = self.p.current_token_error_range();

        for hint in self.hints {
            self.span_labels.push(SpanLabel {
                span: primary_span,
                msg: hint,
            });
        }

        debug!(?primary_span, message = %self.msg.0, expected = ?self.p.expected);

        self.p.emit_parse_error(ParseError {
            message: self.msg.0,
            primary_span,
            span_labels: self.span_labels,
        });

        match self.bump {
            Bump::Yes if !self.p.at_end() => self.p.bump(),
            Bump::YesWrapErr if !self.p.at_end() => {
                let e = self.p.start();
                self.p.bump();
                e.complete(self.p, NodeKind::ParseError);
            }
            _ => {}
        }
        if let Some(m) = self.m {
            m.complete(self.p, NodeKind::ParseError);
        }
    }
}
