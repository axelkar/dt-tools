//! # Devicetree analyzer
//!
//! A crate for analyzing [CST nodes](RedNode) produced by [`dt_parser`] to property values.
//!
//! # Example
//!
//! ```
//! use std::collections::HashMap;
//! use dt_analyzer::{analyze_cst, PropDefinition, Value};
//! use dt_parser::{ast::{self, AstNode}, cst::RedNode};
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
//! let parse = ast::SourceFile::parse(text);
//! let file = parse.source_file();
//! let hm: HashMap<_, _> = analyze_cst(&file, text).unwrap().tree
//!     .dfs_iter()
//!     .map(|(path, value)| (path.join("/"), value))
//!     .collect();
//! eprintln!("{hm:#?}");
//! assert_eq!(hm["a"], Value::U32(1));
//! assert_eq!(hm["foo/b"], Value::U32(2));
//! //assert_eq!(hm["foo/c"], Value::U32(3));
//! assert_eq!(hm.len(), 3);
//! ```

use std::{borrow::Cow, collections::HashMap};

use dt_parser::ast::{self, AstNode, AstToken as _, HasLabel as _, HasName as _};
pub use prop::{
    analyze_node, CustomValue, CustomValueCellItem, DefinitionTree, DefinitionTreeNode,
    PhandleTarget, PropDefinition, Value, ValueFromAstError,
};
pub use string::StringParseError;

mod macros;
pub mod new;
mod prop;
pub mod resolved_prop;
mod string;
#[cfg(test)]
mod tests;

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

/// Analyzes an [`ast::SourceFile`]
///
/// Returns none if the root node cannot be casted to an [`ast::SourceFile`] or if the root (`/`)
/// dt-node cannot be found.
///
/// # Examples
///
/// See [crate root](crate).
pub fn analyze_cst(file: &ast::SourceFile, src: &str) -> Option<FileDefinition> {
    let root_node = file.nodes().find(dt_parser::ast::DtNode::is_root)?;
    let extensions = file.nodes().filter(ast::DtNode::is_extension);
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

    let mut tree = analyze_node(&root_node, src)?;
    for extension in extensions {
        // TODO: path-based phandles
        let label = extension.extension_name()?.name()?;
        let label = label.syntax().text().as_str();

        let label = if let Some(labels) = labels.get(label) {
            labels
        } else {
            tracing::warn!(
                "Couldn't find label {label} for extension at {:?}!",
                extension.syntax().text_range()
            );
            continue;
        }
        .last()?;

        let Some(ex_tree) = analyze_node(&extension, src) else {
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

#[must_use]
pub fn find_labels<'i>(node: &ast::DtNode, src: &'i str) -> Vec<(&'i str, Label)> {
    node.label()
        .and_then(|label| {
            Some((
                label.name()?.syntax().text_from_source(src),
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
