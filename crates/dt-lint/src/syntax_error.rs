use std::{borrow::Cow, sync::Arc};

use dt_parser::{
    ast::{self, AstNode},
    cst::{
        kinds::{NodeKind, TokenKind},
        RedNode,
    },
};

use crate::{EarlyLintPass, LintId, LintSeverity, MultiSpan};

pub struct SyntaxError;

impl EarlyLintPass for SyntaxError {
    fn check_document(&mut self, cx: &mut crate::EarlyContext<'_>, doc: &ast::Document) {
        find_syntax_errors(cx, &doc.syntax(), cx.src);
    }
}

pub fn find_syntax_errors(cx: &mut crate::EarlyContext<'_>, red: &Arc<RedNode>, src: &str) {
    fn err(
        cx: &mut crate::EarlyContext<'_>,
        msg: Cow<'static, str>,
        span: impl Into<MultiSpan>,
    ) {
        cx.add_lint_from_cst(
            LintId::SyntaxError,
            msg,
            LintSeverity::Error,
            span.into(),
        );
    }
    match red.green.kind {
        NodeKind::Error => err(cx, "Syntax error".into(), *red.span()),
        NodeKind::InvalidPunct => err(
            cx,
            format!(
                "Invalid punctuation: `{}`",
                red.green.span.text(src).unwrap_or_default()
            )
            .into(),
            *red.span(),
        ),
        _ => {}
    }
    for token in red.child_tokens() {
        match token.green.kind {
            TokenKind::Error if token.parent.green.kind == NodeKind::Ident => {
                err(cx, "Missing identifier".into(), *token.span())
            }
            TokenKind::Error => err(cx, "Syntax error".into(), *token.span()),
            TokenKind::SeparatedMissingFirst => err(cx, "Missing first item".into(), *token.span()),
            TokenKind::MissingPunct('}') => {
                let unclosed = token.parent.child_tokens().find(|tok| tok.green.kind == TokenKind::LCurly);
                err(
                    cx,
                    "Missing punctuation: `}`".into(),
                    MultiSpan {
                        primary_spans: vec![*token.span()],
                        span_labels: if let Some(unclosed) = unclosed {
                            vec![(*unclosed.span(), "Unclosed `{`".into())]
                        } else {
                            Vec::new()
                        }
                    }
                )
            },
            TokenKind::MissingPunct(c) => {
                err(
                    cx,
                    format!("Missing punctuation: `{c}`").into(),
                    *token.span()
                )
            },
            TokenKind::UnexpectedWhitespace => {
                err(cx, "Unexpected whitespace".into(), *token.span())
            }
            TokenKind::UnexpectedItem => err(
                cx,
                format!(
                    "Unexpected item: `{}`",
                    token.green.span.text(src).unwrap_or_default()
                )
                .into(),
                *token.span(),
            ),
            _ => {}
        }
    }
    for child_node in red.child_nodes() {
        find_syntax_errors(cx, &child_node, src);
    }
}
