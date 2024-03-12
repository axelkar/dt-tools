//! # Devicetree analyzer
//!
//! A crate for analyzing [CST nodes](RedNode) to property values.
//!
//! # Examples
//!
//! ```
//! use std::collections::HashMap;
//! use dt_analyzer::{analyze_cst, PropDefinition, Value};
//! use dt_parser::Span;
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
//! let cst = dt_parser::parse(text).unwrap();
//! let hm: HashMap<_, _> = analyze_cst(cst, text).unwrap().tree
//!     .dfs_iter()
//!     .map(|(path, value)| (path.join("/"), value))
//!     .collect();
//! eprintln!("{hm:#?}");
//! assert_eq!(hm["a"], Value::U32(1));
//! assert_eq!(hm["foo/b"], Value::U32(2));
//! assert_eq!(hm["foo/c"], Value::U32(3));
//! assert_eq!(hm.len(), 3);
//! ```

use std::{borrow::Cow, collections::HashMap, sync::Arc};

use dt_parser::{
    ast::{self, AstNode as _, HasIdent, HasLabel},
    cst::RedNode,
};
pub use prop::{
    analyze_node, CustomValue, CustomValueCellItem, PhandleTarget, PropDefinition, Value,
    ValueFromAstError,
    DefinitionTree, DefinitionTreeNode
};
pub use string::StringParseError;

mod prop;
mod string;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileDefinition {
    pub tree: DefinitionTreeNode,
    pub labels: HashMap<String, Vec<Label>>,
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
pub fn analyze_cst(cst: Arc<RedNode>, src: &str) -> Option<FileDefinition> {
    let doc = ast::Document::cast(cst)?;
    let root_node = doc.nodes().find(|node| node.is_root(src))?;
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

    let mut tree = analyze_node(root_node, src)?;
    for extension in extensions {
        let label = extension.ident()?.text(src)?;
        let label = labels.get(label)?.last()?; // TODO: error or warn on unknown label

        let Some(ex_tree) = analyze_node(extension, src) else { continue };
        let ex_tree = ex_tree.prefix(label.node_ast.path(src).into_iter().map(Cow::into_owned));
        tree.merge(ex_tree);
    }

    Some(FileDefinition {
        tree,
        labels,
    })
}

pub fn find_labels<'i>(node: &ast::DtNode, src: &'i str) -> Vec<(&'i str, Label)> {
    node.label()
        .and_then(|label| {
            Some((
                label.ident()?.text(src)?,
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
