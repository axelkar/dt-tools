use dt_parser::ast::{self, AstNode, HasIdent, HasLabel as _};

use crate::{EarlyLintPass, LintId, LintSeverity};

/// Lint items by [`dtc`](https://github.com/dgibson/dtc)'s rules
///
/// (CURRENTLY) Even if documents don't pass this lint they can still be consumed for DTB conversion,
/// validation and more.
///
/// This includes lints for making sure that
/// - `/dts-v1/;` is the first item (other than whitespace and comments)
/// - Properties are defined before nodes
// - TODO: No duplicate items
pub struct DtcStyle;

// TODO: validate idents

impl EarlyLintPass for DtcStyle {
    fn check_document(&mut self, cx: &mut crate::EarlyContext<'_>, doc: &ast::Document) {
        // TODO: use ast::Item instead?
        let first = doc
            .syntax()
            .children()
            .find_map(|item| item.filter_token(|token| !token.green.kind.is_trivia()));
        if let Some(first) = first {
            let directive = first
                .clone()
                .into_node()
                .and_then(ast::Directive::cast)
                .and_then(|dir| dir.ident()?.text(cx.src));
            if directive != Some("dts-v1") {
                cx.add_lint_from_cst(
                    LintId::DtcStyle,
                    "First item must be `/dts-v1/;` directive",
                    LintSeverity::Error,
                    first.span(),
                );
            }
        }

        for node in doc.nodes() {
            self.check_node(cx, &node)
        }
    }
    fn check_node(&mut self, cx: &mut crate::EarlyContext<'_>, node: &ast::DtNode) {
        if let Some(label) = node.label() {
            self.check_label(cx, &label);
        }
        let mut last_prop = None;
        for property in node.properties() {
            self.check_property(cx, &property);
            last_prop = Some(property);
        }
        for node in node.subnodes() {
            self.check_node(cx, &node)
        }
        if let Some(last_prop) = last_prop {
            if let Some(first_node) = node.subnodes().next() {
                if last_prop.syntax().span().end > first_node.syntax().span().start {
                    cx.add_lint_from_cst(
                        LintId::DtcStyle,
                        "Properties must not be defined after nodes",
                        LintSeverity::Error,
                        *last_prop.syntax().span(),
                    );
                }
            }
        }
    }
}
