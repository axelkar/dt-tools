//! Condition expression evaluation.

use std::sync::Arc;

use dt_tools_diagnostic::{Severity, text_range::TextRange};

use super::{Expander, MacroToken};
use crate::{lexer::PpTokenKind, token::PpToken};

impl Expander<'_> {
    /// Expands a condition's tokens.
    pub(super) fn expand_condition(&mut self, tokens: &[PpToken]) -> Vec<MacroToken> {
        let mut replaced = Vec::with_capacity(tokens.len());
        let mut index = 0;

        while index < tokens.len() {
            let token = tokens[index];
            if token.kind == PpTokenKind::Ident && token.text(self.cleaned_text) == "defined" {
                let (value, consumed) = self.eval_defined(tokens, index);
                replaced.push(MacroToken {
                    token: PpToken {
                        kind: PpTokenKind::Number,
                        cleaned: token.cleaned,
                        orig: token.orig,
                        has_space_before: token.has_space_before,
                        line_start: token.line_start,
                    },
                    text: Some(Arc::from(if value { "1" } else { "0" })),
                    hide: None,
                    depth: 0,
                });
                index += consumed;
                continue;
            }

            replaced.push(MacroToken::source(token));
            index += 1;
        }

        self.prescan(replaced)
    }

    /// Evaluates `defined` at `index`, returning its value and consumed token count.
    fn eval_defined(&mut self, tokens: &[PpToken], index: usize) -> (bool, usize) {
        debug_assert_eq!(tokens[index].kind, PpTokenKind::Ident);
        debug_assert_eq!(tokens[index].text(self.cleaned_text), "defined");

        let (name, consumed) = match tokens.get(index + 1) {
            Some(lparen)
                if self.is_punct(Some(lparen), "(")
                    && let (Some(name), Some(rparen)) =
                        (tokens.get(index + 2), tokens.get(index + 3))
                    && name.kind == PpTokenKind::Ident
                    && self.is_punct(Some(rparen), ")") =>
            {
                (*name, 4)
            }
            Some(name) if name.kind == PpTokenKind::Ident => (*name, 2),
            _ => {
                self.report(
                    Severity::Error,
                    "`defined` requires a macro name".to_owned(),
                    tokens[index].orig,
                );
                return (false, 1);
            }
        };

        let value = self.env.get(name.text(self.cleaned_text)).is_some();
        (value, consumed)
    }
}

/// Evaluates a conditional expression, returning its value and diagnostics.
pub(super) fn evaluate(
    tokens: &[MacroToken],
    cleaned: &str,
    origin: TextRange,
) -> (i64, Vec<(String, TextRange)>) {
    let mut parser = Parser {
        tokens,
        index: 0,
        cleaned,
        origin,
        diagnostics: Vec::new(),
    };
    let value = parser.prefix();

    if let Some(token) = parser.tokens.get(parser.index) {
        parser.error("unexpected token in condition".to_owned(), token.token.orig);
    }

    (value, parser.diagnostics)
}

/// A minimal condition parser.
struct Parser<'a> {
    tokens: &'a [MacroToken],
    index: usize,
    cleaned: &'a str,
    /// Range of the whole directive.
    origin: TextRange,
    diagnostics: Vec<(String, TextRange)>,
}

impl Parser<'_> {
    /// Parses and evaluates one value.
    fn prefix(&mut self) -> i64 {
        let Some(token) = self.tokens.get(self.index) else {
            self.error("expected a value in condition".to_owned(), self.origin);
            return 0;
        };
        self.index += 1;
        let text = token.text(self.cleaned);

        match token.token.kind {
            PpTokenKind::Number => text.parse::<i64>().unwrap_or_else(|_| {
                self.error(
                    format!("invalid integer literal `{text}`"),
                    token.token.orig,
                );
                0
            }),
            // Unexpanded identifiers are undefined and evaluate to 0
            PpTokenKind::Ident => 0,
            PpTokenKind::Punct => match text {
                "(" => {
                    let value = self.prefix();
                    self.expect_punct(")");
                    value
                }
                "!" => i64::from(self.prefix() == 0),
                _ => {
                    self.error(
                        format!("unsupported operator `{text}` in condition"),
                        token.token.orig,
                    );
                    0
                }
            },
            _ => {
                self.error(
                    "unsupported token in condition".to_owned(),
                    token.token.orig,
                );
                0
            }
        }
    }

    /// Consumes the punctuator `text`.
    fn expect_punct(&mut self, text: &str) {
        let Some(token) = self.tokens.get(self.index) else {
            self.error(format!("expected `{text}`"), self.origin);
            return;
        };
        if token.token.kind == PpTokenKind::Punct && token.text(self.cleaned) == text {
            self.index += 1;
        } else {
            self.error(format!("expected `{text}`"), token.token.orig);
        }
    }

    /// Records a diagnostic.
    fn error(&mut self, message: String, range: TextRange) {
        self.diagnostics.push((message, range));
    }
}
