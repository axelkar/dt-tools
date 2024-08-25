//! Per-file computed value stage
//!
//! Each reference and macro is resolved
//!
//! There is only one virtual root node

use std::borrow::Cow;

use dt_diagnostic::{Diagnostic, DiagnosticCollector, MultiSpan, Severity, SpanLabel};
use dt_parser::{
    ast::{self, AstNode, AstToken, HasName},
    match_ast, TextRange,
};
use enum_as_inner::EnumAsInner;
use rustc_hash::FxHashMap;

use crate::resolved_prop::Value;

use super::stage1::{AnalyzedToplevelNode, LabelDef};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedInclude<'a> {
    pub text_range: TextRange,
    /// Reference to the analyzed file of the include.
    ///
    /// This is used to detect duplicates.
    pub analyzed: &'a Stage2File,
    /// Map of labels defined in the included file.
    pub labels: FxHashMap<String, LabelDef>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stage2File {
    pub root_node: Stage2Node,
}

#[derive(derive_more::Debug, Default, Clone, PartialEq, Eq)]
#[debug("Stage2Node {children:#?}")]
pub struct Stage2Node {
    /// List of ASTs this node was merged from
    pub asts: Vec<ast::DtNode>,
    pub children: FxHashMap<String, Stage2Tree>,
}

impl Stage2Node {
    /// Returns the text range for the name.
    ///
    /// This returns `None` when
    ///
    /// * Node doesn't have any ASTs ([`Stage2File::root_node`] when there are no root nodes)
    /// * There is no name ([`Stage2Node`'s children](`Stage2Node::children`) always have a name)
    fn name_text_range(&self) -> Option<TextRange> {
        Some(self.asts.last()?.name()?.syntax().text_range())
    }

    // TODO: remove once sum stage is done
    #[cfg(test)]
    pub(crate) fn into_json(self) -> serde_json::Value {
        use serde_json::Value as JValue;
        let mut map = serde_json::Map::new();
        for (name, tree) in self.children {
            map.insert(
                name,
                match tree {
                    Stage2Tree::Prop(prop) => {
                        if prop.values.is_empty() {
                            JValue::Bool(true)
                        } else {
                            JValue::Array(prop.values.into_iter().map(Value::into_json).collect())
                        }
                    }
                    Stage2Tree::Node(node) => node.into_json(),
                },
            );
        }
        JValue::Object(map)
    }
}

/// A computed definition from a single file
#[derive(derive_more::Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum Stage2Tree {
    #[debug("{_0:?}")]
    Prop(Stage2Property),
    #[debug("{_0:?}")]
    Node(Stage2Node),
}

#[derive(derive_more::Debug, Clone, PartialEq, Eq)]
#[debug("{values:?}")]
pub struct Stage2Property {
    #[debug(skip)]
    pub ast: ast::DtProperty,
    pub values: Vec<Value>,
}
impl Stage2Property {
    /// Returns the text range for the name.
    ///
    /// This returns `None` when there is no name
    /// ([`Stage2Node`'s children](`Stage2Node::children`) always have a name)
    fn name_text_range(&self) -> Option<TextRange> {
        Some(self.ast.name()?.syntax().text_range())
    }
}

// DTC impl:
// Phandle values can be before the label definition
// Extensions must be defined after the label definition

/// # Parameters
///
/// * `stage1`: Iterator of stage 1 toplevel nodes
/// * `includes`: List of (potentially transitive) includes
/// * `diag`: Single-file diagnostic collector
pub fn compute<'a, I: IntoIterator<Item = &'a AnalyzedToplevelNode>>(
    stage1: I,
    _includes: &[ResolvedInclude],
    diag: &impl DiagnosticCollector,
) -> Stage2File {
    let mut root_node = Stage2Node::default();
    for stage1_node in stage1 {
        if stage1_node.is_extension {
            // TODO: cache path in LabelDef
        } else {
            merge_root_node(&stage1_node.ast, diag, &mut root_node);
        }
    }
    Stage2File { root_node }
}

fn merge_root_node(ast: &ast::DtNode, diag: &impl DiagnosticCollector, stage2: &mut Stage2Node) {
    stage2.asts.push(ast.clone());

    //for (name, child) in ast.syntax().child_nodes().filter_map(|node| {
    for syntax in ast.syntax().child_nodes() {
        match_ast! {
            match syntax {
                ast::DtNode(child_ast) => if child_ast.is_extension() {
                    diag.emit(Diagnostic::new(child_ast.syntax().text_range(), Cow::Borrowed("Extension nodes may not be defined in other nodes"), Severity::Error));
                    continue
                } else {
                    let name = match child_ast.text_name("") {
                        Some(name) => name,
                        None => continue
                    };
                    match stage2.children.get_mut(name.as_ref()) {
                        Some(Stage2Tree::Prop(other)) => {
                            // can't mix
                            diag.emit(Diagnostic {
                                span: MultiSpan {
                                    primary_spans: vec![child_ast.syntax().text_range()],
                                    span_labels: vec![SpanLabel {
                                        span: other.name_text_range().expect("Must have a name"),
                                        msg: Cow::Owned(format!("previous definition of `{name}` here")),
                                    }],
                                },
                                msg: Cow::Owned(format!("`{name}` is defined multiple times")),
                                severity: Severity::Error,
                            });
                            continue
                        }
                        Some(Stage2Tree::Node(other)) => {
                            // merge
                            merge_root_node(&child_ast, diag, other);
                        }
                        None => {
                            let mut child_node = Stage2Node::default();
                            merge_root_node(&child_ast, diag, &mut child_node);
                            stage2.children.insert(name.as_ref().to_owned(), Stage2Tree::Node(child_node));
                        }
                    }
                    // TODO: what to do with unit address?, $nodename (jsonschema property)?
                    // Kernel 6.10:
                    // Documentation/devicetree/bindings/thermal/thermal-zones.yaml#L41
                    // Documentation/devicetree/bindings/riscv/sifive.yaml#L17
                    // Documentation/devicetree/bindings/i2c/i2c-virtio.yaml#L20
                    // Documentation/devicetree/bindings/serial/serial.yaml#L23
                },
                ast::DtProperty(prop_ast) => {
                    let name_ast = match prop_ast.name() {
                        Some(name_ast) => name_ast,
                        None => continue
                    };
                    let name = name_ast.syntax().text().as_str();

                    if let Some(Stage2Tree::Node(other)) = stage2.children.get(name) {
                        // can't mix
                        // TODO: DTC supports node_name_vs_property_name as a warning
                        diag.emit(Diagnostic {
                            span: MultiSpan {
                                primary_spans: vec![name_ast.syntax().text_range()],
                                span_labels: vec![SpanLabel {
                                    span: other.name_text_range().expect("Must have a name"),
                                    msg: Cow::Owned(format!("previous definition of `{name}` here")),
                                }],
                            },
                            msg: Cow::Owned(format!("`{name}` is defined multiple times")),
                            severity: Severity::Error,
                        });
                        continue
                    } else {
                        // TODO: pass diag to Value::from_ast
                        if let Ok(values) = prop_ast.values().map(|value_ast|
                            match Value::from_ast(&value_ast, |_| None, |_| None) {
                                Ok(value) => Ok(value),
                                Err(err) => {
                                    diag.emit(Diagnostic::new(
                                        prop_ast.syntax().text_range(),
                                        Cow::Owned(err.to_string()),
                                        Severity::Error,
                                    ));
                                    Err(())
                                }
                            }
                        ).collect::<Result<Vec<_>, ()>>() {
                            let prop = Stage2Property {
                                ast: prop_ast,
                                values
                            };
                            stage2.children.insert(name.to_owned(), Stage2Tree::Prop(prop));
                        }
                    }
                },
                _ => continue
            }
        }
    }
}
