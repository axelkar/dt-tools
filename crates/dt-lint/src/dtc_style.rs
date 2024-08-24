use dt_parser::{
    ast::{self, AstNode, AstNodeOrToken, HasLabel as _},
    cst2::{lexer::TokenKind, NodeKind},
};

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
    fn check_document(&mut self, cx: &mut crate::EarlyContext<'_>, file: &ast::SourceFile) {
        if cx.is_main_file {
            if let Some(first) = file.items().next() {
                let is_v1_directive = first.clone().into_directive().ok().map_or(false, |dir| {
                    dir.syntax()
                        .child_tokens()
                        .any(|tok| tok.green.kind == TokenKind::V1Directive)
                });

                if !is_v1_directive {
                    cx.add_lint_from_cst(
                        LintId::DtcStyle,
                        "First item must be `/dts-v1/;` directive",
                        LintSeverity::Error,
                        first.syntax().text_range(),
                    );
                }
            }
        }

        for prop in file.properties() {
            cx.add_lint_from_cst(
                LintId::DtcStyle,
                "Properties must not be defined outside nodes",
                LintSeverity::Error,
                prop.syntax().text_range(),
            );
        }

        for node in file.nodes() {
            if !node.is_root() && !node.is_extension() {
                cx.add_lint_from_cst(
                    LintId::DtcStyle,
                    "Subnodes must be defined inside other nodes",
                    LintSeverity::Error,
                    node.syntax().text_range(),
                );
            }
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
                if last_prop.syntax().text_range().end > first_node.syntax().text_range().start {
                    cx.add_lint_from_cst(
                        LintId::DtcStyle,
                        "Properties must not be defined after nodes",
                        LintSeverity::Error,
                        last_prop.syntax().text_range(),
                    );
                }
            }
        }
    }
    fn check_property(&mut self, cx: &mut crate::EarlyContext<'_>, property: &ast::DtProperty) {
        // TODO: This should really be in validations or in the grammar..
        if let Some(unit_address) = property
            .syntax()
            .child_nodes()
            .find(|node| node.green.kind == NodeKind::UnitAddress)
        {
            cx.add_lint_from_cst(
                LintId::DtcStyle,
                "Properties must not have unit addresses",
                LintSeverity::Error,
                unit_address.text_range(),
            );
        }

        if let Some(label) = property.label() {
            self.check_label(cx, &label);
        }
    }
}
