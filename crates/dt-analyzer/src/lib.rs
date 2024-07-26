//! # Devicetree analyzer
//!
//! A crate for analyzing [CST nodes](RedNode) produced by [`dt_parser`] to property values.
//!
//! # Example
//!
//! ```
//! use std::collections::HashMap;
//! use dt_analyzer::{analyze_cst, PropDefinition, Value};
//! use dt_parser::{ast::{self, AstNode}, cst2::RedNode};
//! use std::sync::Arc;
//!
//! let text = "
//! /dts-v1/;
//!
//! / {
//!   a = <1>;
//!   label: foo {
//!     b = <2>;
//!   };
//! };
//!
//! &label {
//!   c = <3>;
//! };
//! ";
//! let parse = dt_parser::cst2::parser::parse(text);
//! let doc = ast::Document::cast(RedNode::new(Arc::new(parse.green_node))).unwrap();
//! let hm: HashMap<_, _> = analyze_cst(&doc, text).unwrap().tree
//!     .dfs_iter()
//!     .map(|(path, value)| (path.join("/"), value))
//!     .collect();
//! eprintln!("{hm:#?}");
//! assert_eq!(hm["a"], Value::U32(1));
//! assert_eq!(hm["foo/b"], Value::U32(2));
//! //assert_eq!(hm["foo/c"], Value::U32(3));
//! assert_eq!(hm.len(), 3);
//! ```
// TODO: fix extensions!

use std::{borrow::Cow, collections::HashMap};

use dt_parser::ast::{self, AstNode, HasLabel as _, HasName as _};
pub use prop::{
    analyze_node, CustomValue, CustomValueCellItem, DefinitionTree, DefinitionTreeNode,
    PhandleTarget, PropDefinition, Value, ValueFromAstError,
};
pub use string::StringParseError;

mod prop;
mod string;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileDefinition {
    pub tree: DefinitionTreeNode,
    pub exported_labels: HashMap<String, Vec<Label>>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    // TODO: references
    pub node_ast: ast::DtNode,
    pub label_ast: ast::DtLabel,
}

/// Analyzes an [`ast::Document`]
///
/// Returns none if the root node cannot be casted to an [`ast::Document`] or if the root (`/`)
/// dt-node cannot be found.
///
/// # Examples
///
/// See [crate root](crate).
pub fn analyze_cst(doc: &ast::Document, src: &str) -> Option<FileDefinition> {
    let root_node = doc.nodes().find(|node| node.is_root())?;
    let extensions = doc.nodes().filter(ast::DtNode::is_extension);
    // TODO: check includes for extension labels?
    let labels = {
        let labels = find_labels(&root_node, src); // TODO: do find_labels for extensions too
        let mut hm: HashMap<String, Vec<Label>> = HashMap::new();
        for (name, label) in labels {
            let name = name.to_owned();
            // TODO: a type like: at least one element vec
            let vec = hm.entry(name).or_default();
            vec.push(label);
        }
        hm
    };

    // DTC rules:
    // duplicate labels aren't allowed
    // labels can be used before and after their definition, without scope

    let mut tree = analyze_node(root_node, src)?;
    for extension in extensions {
        // TODO: path-based phandles
        let label = extension.extension_name()?.name()?.text(src)?;
        let label = match labels.get(label) {
            Some(labels) => labels,
            None => {
                tracing::warn!(
                    "Couldn't find label {label} for extension at {:?}!",
                    extension.syntax_ref().text_range()
                );
                continue;
            }
        }
        .last()?;

        let Some(ex_tree) = analyze_node(extension, src) else {
            continue;
        };
        let ex_tree = ex_tree.prefix(label.node_ast.path(src).map(Cow::into_owned));
        tree.merge(ex_tree);
    }

    Some(FileDefinition {
        tree,
        exported_labels: labels,
    })
}

pub fn find_labels<'i>(node: &ast::DtNode, src: &'i str) -> Vec<(&'i str, Label)> {
    node.label()
        .and_then(|label| {
            Some((
                label.name()?.text(src)?,
                Label {
                    node_ast: node.clone(),
                    label_ast: label,
                },
            ))
        })
        .into_iter()
        .chain(node.subnodes().flat_map(|node| find_labels(&node, src)))
        .collect()
}
