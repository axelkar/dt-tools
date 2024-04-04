use std::{borrow::Cow, sync::Arc};

use dt_parser::{
    ast::{self, AstNode},
    cst::{
        kinds::{NodeKind, TokenKind},
        RedNode,
    },
};

use crate::{EarlyLintPass, LintId, LintSeverity, MultiSpan};

/// Collect syntax errors via the lint system
///
/// This lint should always be mandatory when deriving custom lint sets
pub struct SyntaxError;

impl EarlyLintPass for SyntaxError {
    fn check_document(&mut self, cx: &mut crate::EarlyContext<'_>, doc: &ast::Document) {
        find_syntax_errors(cx, &doc.syntax(), cx.src);
    }
}

pub fn find_syntax_errors(cx: &mut crate::EarlyContext<'_>, red: &Arc<RedNode>, src: &str) {
    fn err(cx: &mut crate::EarlyContext<'_>, msg: Cow<'static, str>, span: impl Into<MultiSpan>) {
        cx.add_lint_from_cst(LintId::SyntaxError, msg, LintSeverity::Error, span.into());
    }
    match red.green.kind {
        NodeKind::Error => err(cx, "Syntax error".into(), *red.text_range()),
        NodeKind::InvalidPunct => err(
            cx,
            format!(
                "Invalid punctuation: `{}`",
                red.green.text_range.text(src).unwrap_or_default()
            )
            .into(),
            *red.text_range(),
        ),
        _ => {}
    }
    for token in red.child_tokens() {
        match token.green.kind {
            TokenKind::Error if token.parent.green.kind == NodeKind::Ident => {
                err(cx, "Missing identifier".into(), *token.text_range())
            }
            TokenKind::Error => err(cx, "Syntax error".into(), *token.text_range()),
            TokenKind::SeparatedMissingFirst => {
                err(cx, "Missing first item".into(), *token.text_range())
            }
            TokenKind::MissingPunct('}') => {
                let unclosed = token
                    .parent
                    .child_tokens()
                    .find(|tok| tok.green.kind == TokenKind::LCurly);
                err(
                    cx,
                    "Missing punctuation: `}`".into(),
                    MultiSpan {
                        primary_spans: vec![*token.text_range()],
                        span_labels: if let Some(unclosed) = unclosed {
                            vec![(*unclosed.text_range(), "Unclosed `{`".into())]
                        } else {
                            Vec::new()
                        },
                    },
                )
            }
            TokenKind::MissingPunct(c) => err(
                cx,
                format!("Missing punctuation: `{c}`").into(),
                *token.text_range(),
            ),
            TokenKind::UnexpectedWhitespace => {
                err(cx, "Unexpected whitespace".into(), *token.text_range())
            }
            TokenKind::UnexpectedItem => err(
                cx,
                format!(
                    "Unexpected item: `{}`",
                    token.green.text_range.text(src).unwrap_or_default()
                )
                .into(),
                *token.text_range(),
            ),
            _ => {}
        }
    }
    for child_node in red.child_nodes() {
        find_syntax_errors(cx, &child_node, src);
    }
}
