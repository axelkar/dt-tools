use dt_parser::{
    ast::{self, AstNode, HasIdent, HasLabel as _},
    cst::TreeItem,
};

use crate::{EarlyLintPass, LintId, LintSeverity};

/// https://docs.kernel.org/devicetree/bindings/dts-coding-style.html
pub struct KernelCodingStyle;

fn valid_node_name(s: &str) -> bool {
    s.chars()
        .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-')
        || s == "/"
}
fn valid_node_unit_name(s: &str) -> bool {
    (s.chars().all(|c| matches!(c, '0'..='9' | 'a'..='f')) && !s.starts_with('0')) || s == "0"
}
fn valid_prop_name(s: &str) -> bool {
    s.chars().enumerate().all(|(i, c)| {
        c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-' || (i == 0 && c == '#')
    })
}
fn valid_label_name(s: &str) -> bool {
    s.chars()
        .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_')
}

impl EarlyLintPass for KernelCodingStyle {
    fn check_node(&mut self, cx: &mut crate::EarlyContext<'_>, node: &ast::DtNode) {
        if let Some(ident) = node.ident() {
            if let Some(text) = ident.text(cx.src) {
                if node.is_extension() {
                    if !valid_label_name(text) {
                        cx.add_lint_from_cst(
                            LintId::KernelCodingStyle,
                            format!("Label name `{text}` should match `[a-z0-9_]+`"),
                            LintSeverity::Warn,
                            *ident.syntax().span(),
                        );
                    }
                } else if !valid_node_name(text) {
                    cx.add_lint_from_cst(
                        LintId::KernelCodingStyle,
                        format!("Node name `{text}` should match `[a-z0-9-]+`"),
                        LintSeverity::Warn,
                        *ident.syntax().span(),
                    );
                }
            }
        }
        if let Some(ident) = node.unit_address() {
            if let Some(text) = ident.text(cx.src) {
                // TODO: "Unless a bus defines differently,"
                if !valid_node_unit_name(text) {
                    cx.add_lint_from_cst(
                        LintId::KernelCodingStyle,
                        format!(
                            "Node unit name `{text}` should be a hex number without leading zeros"
                        ),
                        LintSeverity::Warn,
                        *ident.syntax().span(),
                    );
                }
            }
        }

        if let Some(label) = node.label() {
            self.check_label(cx, &label);
        }
        for property in node.properties() {
            // TODO: property order, maybe easier after analyzation
            self.check_property(cx, &property)
        }
        for node in node.subnodes() {
            self.check_node(cx, &node)
        }
    }
    fn check_property(&mut self, cx: &mut crate::EarlyContext<'_>, property: &ast::DtProperty) {
        if let Some(ident) = property.ident() {
            if let Some(text) = ident.text(cx.src) {
                if text != "device_type" && !valid_prop_name(text) {
                    cx.add_lint_from_cst(
                        LintId::KernelCodingStyle,
                        format!("Property name `{text}` should match `#?[a-z0-9-]+`"),
                        LintSeverity::Warn,
                        *ident.syntax().span(),
                    );
                }
            }
        }
        for value in property.values() {
            if let ast::PropValue::Cell(cell) = value {
                for value in cell.values() {
                    if let TreeItem::Token(dt_number) = value {
                        if let Some(text) = dt_number.text(cx.src) {
                            // Hex values in properties, e.g. "reg", shall use lowercase hex.
                            if text.contains(|c: char| c.is_ascii_uppercase()) {
                                cx.add_lint_from_cst(
                                    LintId::KernelCodingStyle,
                                    "Hex values in properties must use lowercase hex",
                                    LintSeverity::Warn,
                                    *dt_number.span(),
                                );
                            }
                        }
                    }
                }
            }
        }
        if let Some(label) = property.label() {
            self.check_label(cx, &label);
        }
    }
    fn check_label(&mut self, cx: &mut crate::EarlyContext, label: &ast::DtLabel) {
        if let Some(ident) = label.ident() {
            if let Some(text) = ident.text(cx.src) {
                if !valid_label_name(text) {
                    cx.add_lint_from_cst(
                        LintId::KernelCodingStyle,
                        format!("Label name `{text}` should match `[a-z0-9_]+`"),
                        LintSeverity::Warn,
                        *ident.syntax().span(),
                    );
                }
            }
        }
    }
}
