//! # Devicetree linter
//!
//! A crate for linting [Devicetree][1] documents one-by-one and as collections.
//!
//! [1]: https://www.devicetree.org/

use dt_parser::{
    ast::{self, HasLabel},
    TextRange,
};
use std::borrow::Cow;

//mod dtc_style;
//mod kernel_coding_style;

// TODO: delete
//mod syntax_error;

pub mod lints {
    //! The full collection of lints upstream.
    //!
    //! Currently all lints in here are applied by the [default_lint](crate::default_lint) function
    //! used by the LSP.
    //pub use crate::dtc_style::DtcStyle;
    //pub use crate::kernel_coding_style::KernelCodingStyle;
}

// TODO: something like this:
// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_ast/visit/index.html

pub type DiagnosticMessage = Cow<'static, str>;

/// Just like the [MultiSpan from rustc & clippy][1]
///
/// [1]: https://doc.rust-lang.org/nightly/nightly-rustc/rustc_error_messages/struct.MultiSpan.html
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MultiSpan {
    pub primary_spans: Vec<TextRange>,
    pub span_labels: Vec<(TextRange, DiagnosticMessage)>,
}
impl From<TextRange> for MultiSpan {
    fn from(value: TextRange) -> Self {
        Self {
            primary_spans: vec![value],
            span_labels: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LintId {
    DtcStyle,
    KernelCodingStyle,
    SyntaxError,
}
impl std::fmt::Display for LintId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::DtcStyle => "dtc_style",
            Self::KernelCodingStyle => "kernel_coding_style",
            Self::SyntaxError => "syntax_error",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LintSeverity {
    Warn,
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EarlyLint {
    pub id: LintId,
    pub span: MultiSpan,
    pub msg: DiagnosticMessage,
    pub severity: LintSeverity,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EarlyContext<'i> {
    pub lints: Vec<EarlyLint>,
    pub src: &'i str,
}
impl EarlyContext<'_> {
    pub fn add_lint_from_cst(
        &mut self,
        id: LintId,
        msg: impl Into<DiagnosticMessage> + Clone,
        severity: LintSeverity,
        span: impl Into<MultiSpan>,
    ) {
        eprintln!("adding lint {}, {:?}", msg.clone().into(), severity);
        self.lints.push(EarlyLint {
            id,
            span: span.into(),
            msg: msg.into(),
            severity,
        });
    }
    pub fn add_lint(&mut self, lint: EarlyLint) {
        eprintln!("adding lint {lint:?}");
        self.lints.push(lint);
    }
}

/// The early lint pass before type information is acquired. This runs on [AST](ast)s.
pub trait EarlyLintPass {
    /// Lint a document's [AST](ast::Document)
    fn check_document(&mut self, cx: &mut EarlyContext<'_>, doc: &ast::Document) {
        for node in doc.nodes() {
            self.check_node(cx, &node)
        }
    }
    /// Lint a node's [AST](ast::DtNode)
    fn check_node(&mut self, cx: &mut EarlyContext<'_>, node: &ast::DtNode) {
        if let Some(label) = node.label() {
            self.check_label(cx, &label);
        }
        for property in node.properties() {
            self.check_property(cx, &property)
        }
        for node in node.subnodes() {
            self.check_node(cx, &node)
        }
    }
    /// Lint a property's [AST](ast::DtProperty)
    fn check_property(&mut self, cx: &mut EarlyContext, property: &ast::DtProperty) {
        if let Some(label) = property.label() {
            self.check_label(cx, &label);
        }
    }
    /// Lint a node label's [AST](ast::DtLabel)
    fn check_label(&mut self, _cx: &mut EarlyContext, _label: &ast::DtLabel) {}
}

pub fn default_lint(_doc: &ast::Document, src: &str) -> Vec<EarlyLint> {
    let cx = EarlyContext {
        lints: Vec::new(),
        src,
    };
    // TODO: go over the tree just once
    //crate::lints::KernelCodingStyle.check_document(&mut cx, doc);
    //crate::lints::DtcStyle.check_document(&mut cx, doc);
    // TODO: warn for `&LABEL,` (ident eats the comma), although... it shouldn't be possible when
    // not in a dt cell e.g. `a = &LABEL, "foo";`
    cx.lints
}
