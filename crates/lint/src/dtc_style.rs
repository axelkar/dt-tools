use dt_tools_diagnostic::Severity;
use dt_tools_parser::{
    ast::{self, AstNode, HasLabel as _},
    cst::NodeKind,
};

use crate::{EarlyLintPass, LintId};

/// Lint items by [`dtc`](https://github.com/dgibson/dtc)'s rules
///
/// (CURRENTLY) Even if documents don't pass this lint they can still be consumed for DTB conversion,
/// validation and more.
///
/// This includes lints for making sure that
/// - Properties are defined before nodes
// - TODO: No duplicate items
pub struct DtcStyle;

// TODO: validate idents

impl<F: std::fmt::Debug + Clone> EarlyLintPass<F> for DtcStyle {
    fn check_document(&mut self, cx: &mut crate::EarlyContext<'_, F>, file: &ast::SourceFile) {
        for prop in file.properties() {
            cx.add_lint_from_cst(
                LintId::DtcStyle,
                "Properties must not be defined outside nodes",
                Severity::Error,
                prop.syntax().text_range(),
            );
        }

        for node in file.nodes() {
            if !node.is_root() && !node.is_extension() {
                cx.add_lint_from_cst(
                    LintId::DtcStyle,
                    "Subnodes must be defined inside other nodes",
                    Severity::Error,
                    node.syntax().text_range(),
                );
            }
            self.check_node(cx, &node);
        }
    }
    fn check_node(&mut self, cx: &mut crate::EarlyContext<'_, F>, node: &ast::DtNode) {
        if let Some(label) = node.label() {
            self.check_label(cx, &label);
        }

        let mut last_prop = None;
        for property in node.properties() {
            self.check_property(cx, &property);
            last_prop = Some(property);
        }

        for node in node.subnodes() {
            self.check_node(cx, &node);
        }

        if let Some(last_prop) = last_prop
            && let Some(first_node) = node.subnodes().next()
            && last_prop.syntax().text_range().end > first_node.syntax().text_range().start
        {
            cx.add_lint_from_cst(
                LintId::DtcStyle,
                "Properties must not be defined after nodes",
                Severity::Error,
                last_prop.syntax().text_range(),
            );
        }
    }
    fn check_property(&mut self, cx: &mut crate::EarlyContext<'_, F>, property: &ast::DtProperty) {
        // TODO: This should really be in validations or in the grammar..
        if let Some(unit_address) = property
            .syntax()
            .child_nodes()
            .find(|node| node.green.kind == NodeKind::UnitAddress)
        {
            cx.add_lint_from_cst(
                LintId::DtcStyle,
                "Properties must not have unit addresses",
                Severity::Error,
                unit_address.text_range(),
            );
        }

        if let Some(label) = property.label() {
            self.check_label(cx, &label);
        }
    }
}
