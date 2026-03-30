use dt_diagnostic::Severity;
use dt_parser::ast::{self, AstToken, HasLabel as _, HasName};

use crate::{EarlyLintPass, LintId};

/// Lint items to match the Linux kernel's devicetree [coding style](https://docs.kernel.org/devicetree/bindings/dts-coding-style.html).
///
/// Currently excludes naming conventions.
pub struct KernelCodingStyle;

impl EarlyLintPass for KernelCodingStyle {
    fn check_node(&mut self, cx: &mut crate::EarlyContext<'_>, node: &ast::DtNode) {
        if let Some(label) = node.label() {
            self.check_label(cx, &label);
        }
        for property in node.properties() {
            // TODO: property order, maybe easier after analysis
            self.check_property(cx, &property);
        }
        for node in node.subnodes() {
            self.check_node(cx, &node);
        }
    }
    fn check_property(&mut self, cx: &mut crate::EarlyContext<'_>, property: &ast::DtProperty) {
        for value in property.values() {
            if let ast::PropValue::CellList(cell_list) = value {
                for cell in cell_list.cells() {
                    if let ast::Cell::Number(dt_number) = cell {
                        let text = dt_number.text();
                        // Hex values in properties, e.g. "reg", shall use lowercase hex.
                        if text.contains(|c: char| c.is_ascii_uppercase()) {
                            cx.add_lint_from_cst(
                                LintId::KernelCodingStyle,
                                "Hex values in properties must use lowercase hex",
                                Severity::Warn,
                                dt_number.text_range(),
                            );
                        }
                    }
                }
            }
        }
        if let Some(label) = property.label() {
            self.check_label(cx, &label);
        }
    }
}
