//! # Devicetree linter
//!
//! A crate for linting [Devicetree][1] documents one-by-one and as collections.
//!
//! [1]: https://www.devicetree.org/

use dt_tools_diagnostic::{Diagnostic, DiagnosticMessage, Severity};
use dt_tools_parser::{
    TextRange,
    ast::{self, HasLabel},
};

mod dtc_style;
mod kernel_coding_style;

pub mod lints {
    //! The full collection of lints upstream.
    //!
    //! Currently all lints in here are applied by the [`default_lint`](crate::default_lint) function
    //! used by the LSP.
    pub use crate::dtc_style::DtcStyle;
    pub use crate::kernel_coding_style::KernelCodingStyle;
}

// TODO: something like this:
// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_ast/visit/index.html

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LintId {
    DtcStyle,
    KernelCodingStyle,
}
impl std::fmt::Display for LintId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::DtcStyle => "dtc_style",
            Self::KernelCodingStyle => "kernel_coding_style",
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EarlyLint<F> {
    pub id: LintId,
    pub diagnostic: Diagnostic<F>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EarlyContext<'i, F> {
    pub lints: Vec<EarlyLint<F>>,
    pub src: &'i str,
    pub is_main_file: bool,
    pub file: F,
}
impl<F: std::fmt::Debug + Clone> EarlyContext<'_, F> {
    pub fn add_lint_from_cst(
        &mut self,
        id: LintId,
        msg: impl Into<DiagnosticMessage> + Clone,
        severity: Severity,
        text_range: TextRange,
        //span: impl Into<MultiSpan<F>>,
    ) {
        tracing::debug!("adding lint {}, {:?}", msg.clone().into(), severity);
        self.lints.push(EarlyLint {
            id,
            diagnostic: Diagnostic {
                span: text_range.within_file(self.file.clone()).into(),
                msg: msg.into(),
                severity,
            },
        });
    }
    pub fn add_lint(&mut self, lint: EarlyLint<F>) {
        tracing::debug!("adding lint {lint:?}");
        self.lints.push(lint);
    }
}

/// The early lint pass before type information is acquired. This runs on [AST](ast)s.
pub trait EarlyLintPass<F> {
    /// Lint a source file's [AST](ast::SourceFile)
    fn check_document(&mut self, cx: &mut EarlyContext<'_, F>, file: &ast::SourceFile) {
        for node in file.nodes() {
            self.check_node(cx, &node);
        }
    }
    /// Lint a node's [AST](ast::DtNode)
    fn check_node(&mut self, cx: &mut EarlyContext<'_, F>, node: &ast::DtNode) {
        if let Some(label) = node.label() {
            self.check_label(cx, &label);
        }
        for property in node.properties() {
            self.check_property(cx, &property);
        }
        for node in node.subnodes() {
            self.check_node(cx, &node);
        }
    }
    /// Lint a property's [AST](ast::DtProperty)
    fn check_property(&mut self, cx: &mut EarlyContext<F>, property: &ast::DtProperty) {
        if let Some(label) = property.label() {
            self.check_label(cx, &label);
        }
    }
    /// Lint a node label's [AST](ast::DtLabel)
    fn check_label(&mut self, _cx: &mut EarlyContext<F>, _label: &ast::DtLabel) {}
}

#[must_use]
pub fn default_lint<F: std::fmt::Debug + Clone>(
    file_ast: &ast::SourceFile,
    src: &str,
    is_main_file: bool,
    file_id: F,
) -> Vec<EarlyLint<F>> {
    let mut cx = EarlyContext {
        lints: Vec::new(),
        src,
        is_main_file,
        file: file_id,
    };
    // TODO: go over the tree only once
    crate::lints::KernelCodingStyle.check_document(&mut cx, file_ast);
    crate::lints::DtcStyle.check_document(&mut cx, file_ast);
    // TODO: warn for `&LABEL,` (ident eats the comma) in a devicetree cell
    cx.lints
}
