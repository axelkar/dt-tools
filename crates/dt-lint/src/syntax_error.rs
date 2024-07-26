use std::{borrow::Cow, sync::Arc};

use dt_parser::{
    ast::{self, AstNode},
    cst2::{
        NodeKind,
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
        find_syntax_errors(cx, &doc.syntax());
    }
}

pub fn find_syntax_errors(cx: &mut crate::EarlyContext<'_>, red: &Arc<RedNode>) {
    fn err(cx: &mut crate::EarlyContext<'_>, msg: Cow<'static, str>, span: impl Into<MultiSpan>) {
        cx.add_lint_from_cst(LintId::SyntaxError, msg, LintSeverity::Error, span.into());
    }
    if red.green.kind == NodeKind::ParseError {
        err(cx, "Syntax error".into(), red.text_range())
    }
    for child_node in red.child_nodes() {
        find_syntax_errors(cx, &child_node);
    }
}
