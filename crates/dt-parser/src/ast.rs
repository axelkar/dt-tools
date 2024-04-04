//! Abstract syntax trees.
//!
//! All AST items reference [`RedNode`]s through [`AstNode::syntax()`].
//!
//! To get an AST item, use [`AstNode::cast()`] with a [`Arc<RedNode>`] as follows:
//!
//! ```
//! use dt_parser::cst::RedNode;
//! use dt_parser::ast::{self, AstNode as _};
//!
//! let red_node: Arc<RedNode>;
//! # use std::sync::Arc;
//! # use dt_parser::{TextRange, cst::{GreenNode, GreenToken, GreenItem, kinds::*}};
//! # let green_node = Arc::new(GreenNode {
//! #     kind: NodeKind::Directive,
//! #     text_range: TextRange { start: 0, end: 0 },
//! #     children: vec! [
//! #         GreenItem::Node(Arc::new(GreenNode {
//! #             kind: NodeKind::Ident,
//! #             text_range: TextRange { start: 0, end: 0 },
//! #             children: vec! [
//! #                 GreenItem::Token(Arc::new(GreenToken {
//! #                     kind: TokenKind::Ident,
//! #                     text_range: TextRange { start: 0, end: 0 }
//! #                 }))
//! #             ]
//! #         }))
//! #     ]
//! # });
//! # red_node = RedNode::new(green_node);
//! let ast = ast::Directive::cast(red_node).unwrap();
//!
//! // Use Has- traits
//! use dt_parser::ast::HasIdent;
//! assert!(matches!(ast.ident(), Some(ast::Ident { .. })))
//! ```
use std::{borrow::Cow, sync::Arc};

use crate::cst::{
    kinds::{NodeKind, TokenKind},
    GreenToken, RedItem, RedNode, RedToken, TreeItem,
};

/// Trait used for downcasting from [`RedNode`]s to AST nodes.
pub trait AstNode {
    /// Try to cast a [`RedNode`] to an AST node.
    ///
    /// The implementor must check [`GreenNode::kind`](crate::cst::GreenNode::kind)!
    ///
    /// All functions which reference children (e.g. [`Document::nodes`]) should return [`Option`]s
    /// or similar!
    fn cast(syntax: Arc<RedNode>) -> Option<Self>
    where
        Self: Sized;

    /// Returns the syntax node.
    fn syntax(&self) -> Arc<RedNode>;

    /// Returns a reference to the syntax node.
    fn syntax_ref(&self) -> &RedNode;
}

/// Matches a `RedNode` against an `ast` type.
///
/// # Example:
///
/// ```ignore
/// match_ast! {
///     match node {
///         ast::DtNode(it) => { ... },
///         ast::DtProperty(it) => { ... },
///         ast::Ident(it) => { ... },
///         _ => None,
///     }
/// }
/// ```
#[macro_export]
macro_rules! match_ast {
    (match $node:ident { $($tt:tt)* }) => { $crate::match_ast!(match ($node) { $($tt)* }) };
    (match ($node:expr) {
        $( $( $path:ident )::+ ($it:pat) => $res:expr, )*
        _ => $catch_all:expr $(,)?
    }) => {{
        $( if let Some($it) = $($path::)+cast($node.clone()) { $res } else )*
        { $catch_all }
    }};
}
//pub use match_ast;

/// Trait for [`AstNode`]s with [`Ident`]s
pub trait HasIdent: AstNode {
    /// Returns the [`Ident`] if it exists.
    fn ident(&self) -> Option<Ident> {
        self.syntax().child_nodes().find_map(Ident::cast)
    }
}

/// Trait for [`AstNode`]s with [`DtLabel`]s
pub trait HasLabel: AstNode {
    /// Returns the [`DtLabel`] if it exists.
    fn label(&self) -> Option<DtLabel> {
        self.syntax().child_nodes().find_map(DtLabel::cast)
    }
}

/// An identifier.
///
/// Kind: [`NodeKind::Ident`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    syntax: Arc<RedNode>,
}
impl AstNode for Ident {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::Ident => Some(Self { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> Arc<RedNode> {
        self.syntax.clone()
    }
    fn syntax_ref(&self) -> &RedNode {
        &self.syntax
    }
}
impl Ident {
    // TODO: HasText::text, SyntaxToken::text and/or GreenToken::text?
    /// Returns the inner [`Arc<GreenToken>`] with the kind [`TokenKind::Ident`] if it exists.
    pub fn ident_tok(&self) -> Option<Arc<GreenToken>> {
        Some(
            self.syntax
                .child_tokens()
                .find(|tok| tok.green.kind == TokenKind::Ident)?
                .green
                .clone(),
        )
    }

    /// Returns the text of the inner token if it exists.
    pub fn text<'i>(&self, src: &'i str) -> Option<&'i str> {
        self.ident_tok()?.text_range.text(src)
    }
}

/// A [Devicetree][1] document, which contains [`ToplevelItem`]s.
///
/// Kind: [`NodeKind::Document`]
///
/// [1]: https://devicetree.org
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Document {
    syntax: Arc<RedNode>,
}
impl AstNode for Document {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::Document => Some(Self { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> Arc<RedNode> {
        self.syntax.clone()
    }
    fn syntax_ref(&self) -> &RedNode {
        &self.syntax
    }
}
impl Document {
    /// Returns an iterator over direct [`ToplevelItem`] children.
    pub fn items(&self) -> impl Iterator<Item = ToplevelItem> + '_ {
        self.syntax.child_nodes().filter_map(ToplevelItem::cast)
    }
    /// Returns an iterator over direct [`Directive`] children.
    pub fn directives(&self) -> impl Iterator<Item = Directive> + '_ {
        self.syntax.child_nodes().filter_map(Directive::cast)
    }
    /// Returns an iterator over direct [`DtNode`] children.
    pub fn nodes(&self) -> impl Iterator<Item = DtNode> + '_ {
        self.syntax.child_nodes().filter_map(DtNode::cast)
    }
}

/// A [Devicetree directive][1].
///
/// Kind: [`NodeKind::Directive`]
///
/// [1]: https://devicetree-specification.readthedocs.io/en/latest/chapter6-source-language.html#compiler-directives
// TODO: directive parameters
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Directive {
    syntax: Arc<RedNode>,
}
impl AstNode for Directive {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::Directive => Some(Self { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> Arc<RedNode> {
        self.syntax.clone()
    }
    fn syntax_ref(&self) -> &RedNode {
        &self.syntax
    }
}
impl HasIdent for Directive {}

/// A [Devicetree property][1].
///
/// Kind: [`NodeKind::DtProperty`]
///
/// [1]: https://devicetree-specification.readthedocs.io/en/latest/chapter6-source-language.html#node-and-property-definitions
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DtProperty {
    syntax: Arc<RedNode>,
}
impl AstNode for DtProperty {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtProperty => Some(Self { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> Arc<RedNode> {
        self.syntax.clone()
    }
    fn syntax_ref(&self) -> &RedNode {
        &self.syntax
    }
}
impl DtProperty {
    /// Returns an iterator over direct [`PropValue`] children.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, Document};
    ///
    /// let src = b"/ { a = <1 2 3> <4>; };";
    /// let doc = Document::cast(dt_parser::parse(src).unwrap()).unwrap();
    /// let property = doc.nodes().next().unwrap().properties().next().unwrap();
    /// let mut values = property.values();
    ///
    /// assert!(values.next().is_some());
    /// assert!(values.next().is_some());
    /// assert_eq!(values.next(), None);
    ///
    /// ```
    pub fn values(&self) -> impl Iterator<Item = PropValue> + '_ {
        self.syntax.child_nodes().filter_map(PropValue::cast)
    }
}
impl HasIdent for DtProperty {}
impl HasLabel for DtProperty {}

/// A value of a [`DtProperty`].
///
/// Kind: [`NodeKind::DtCell`] or [`NodeKind::DtString`]
///
/// More value types are to come later as the library gets developmed
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PropValue {
    Cell(DtCell),
    String(DtString),
}
impl AstNode for PropValue {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtCell => Some(Self::Cell(DtCell { syntax })),
            NodeKind::DtString => Some(Self::String(DtString { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> Arc<RedNode> {
        match self {
            Self::Cell(it) => it.syntax.clone(),
            Self::String(it) => it.syntax.clone(),
        }
    }
    fn syntax_ref(&self) -> &RedNode {
        match self {
            Self::Cell(it) => &it.syntax,
            Self::String(it) => &it.syntax,
        }
    }
}

/// A phandle item in a [`DtCell`].
///
/// Kind: [`NodeKind::DtPhandle`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DtPhandle {
    syntax: Arc<RedNode>,
}
impl AstNode for DtPhandle {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtPhandle => Some(Self { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> Arc<RedNode> {
        self.syntax.clone()
    }
    fn syntax_ref(&self) -> &RedNode {
        &self.syntax
    }
}
impl DtPhandle {
    /// Returns true if this is a path phandle and not a label phandle.
    pub fn is_path(&self) -> bool {
        self.syntax
            .child_tokens()
            .any(|tok| tok.green.kind == TokenKind::DtPathPhandle)
    }
    /// Returns true if this is a label phandle and not a path phandle.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, Document, PropValue};
    /// use dt_parser::cst::TreeItem;
    ///
    /// let src = b"/ { a = <1 2 &LABEL &{/path/based/phandle}>; };";
    /// let doc = Document::cast(dt_parser::parse(src).unwrap()).unwrap();
    /// let node = doc.nodes().next().unwrap();
    /// let property = node.properties().next().unwrap();
    /// let property_value = property.values().next().unwrap();
    /// let cell = match property_value {
    ///     PropValue::Cell(cell) => cell,
    ///     _ => panic!("unexpected"),
    /// };
    /// let mut phandles = cell.values().filter_map(TreeItem::into_node);
    ///
    /// assert!(phandles.next().unwrap().is_label());
    /// assert!(!phandles.next().unwrap().is_label());
    /// assert_eq!(phandles.next(), None);
    ///
    /// ```
    pub fn is_label(&self) -> bool {
        !self.is_path()
    }
}
impl HasIdent for DtPhandle {}

/// A Devicetree cell. A type of [`PropValue`]
///
/// Kind: [`NodeKind::DtCell`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DtCell {
    syntax: Arc<RedNode>,
}
impl AstNode for DtCell {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtCell => Some(Self { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> Arc<RedNode> {
        self.syntax.clone()
    }
    fn syntax_ref(&self) -> &RedNode {
        &self.syntax
    }
}
impl DtCell {
    /// Returns an iterator over phandle and number children.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, Document, PropValue};
    ///
    /// let src = b"/ { a = <1 2 &LABEL>; };";
    /// let doc = Document::cast(dt_parser::parse(src).unwrap()).unwrap();
    /// let node = doc.nodes().next().unwrap();
    /// let property = node.properties().next().unwrap();
    /// let property_value = property.values().next().unwrap();
    /// let cell = match property_value {
    ///     PropValue::Cell(cell) => cell,
    ///     _ => panic!("unexpected"),
    /// };
    /// let mut values = cell.values();
    ///
    /// assert!(values.next().is_some());
    /// assert!(values.next().is_some());
    /// assert!(values.next().is_some());
    /// assert_eq!(values.next(), None);
    ///
    /// ```
    // TODO: make DtNumbers nodes so this can return better types
    pub fn values(&self) -> impl Iterator<Item = TreeItem<DtPhandle, Arc<RedToken>>> + '_ {
        self.syntax.children().filter_map(|item| {
            Some(match item {
                RedItem::Node(node) => TreeItem::Node(DtPhandle::cast(node)?),
                RedItem::Token(token) if token.green.kind == TokenKind::DtNumber => {
                    TreeItem::Token(token)
                }
                _ => return None,
            })
        })
    }
}

/// A Devicetree string. A type of [`PropValue`].
///
/// Kind: [`NodeKind::DtString`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DtString {
    syntax: Arc<RedNode>,
}
impl AstNode for DtString {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtString => Some(Self { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> Arc<RedNode> {
        self.syntax.clone()
    }
    fn syntax_ref(&self) -> &RedNode {
        &self.syntax
    }
}
impl DtString {
    /// Returns the content token if it exists.
    pub fn contents(&self) -> Option<Arc<RedToken>> {
        self.syntax
            .child_tokens()
            .find(|tok| tok.green.kind == TokenKind::DtStringContents)
    }
}

/// A [Devicetree node][1].
///
/// Kind: [`NodeKind::DtNode`]
///
/// [1]: https://devicetree-specification.readthedocs.io/en/latest/chapter6-source-language.html#node-and-property-definitions
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DtNode {
    syntax: Arc<RedNode>,
}
impl AstNode for DtNode {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtNode => Some(Self { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> Arc<RedNode> {
        self.syntax.clone()
    }
    fn syntax_ref(&self) -> &RedNode {
        &self.syntax
    }
}
impl DtNode {
    /// Returns an iterator over direct [`DtProperty`] children.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, Document};
    ///
    /// let src = b"/ { a = <0>; b; };";
    /// let doc = Document::cast(dt_parser::parse(src).unwrap()).unwrap();
    /// let node = doc.nodes().next().unwrap();
    /// let mut properties = node.properties();
    ///
    /// assert!(properties.next().is_some());
    /// assert!(properties.next().is_some());
    /// assert_eq!(properties.next(), None);
    ///
    /// ```
    pub fn properties(&self) -> impl Iterator<Item = DtProperty> + '_ {
        self.syntax.child_nodes().filter_map(DtProperty::cast)
    }

    /// Returns an iterator over direct [`DtNode`] children.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, Document};
    ///
    /// let src = b"/ { a = <0>; b { c = <1>; }; d {}; };";
    /// let doc = Document::cast(dt_parser::parse(src).unwrap()).unwrap();
    /// let node = doc.nodes().next().unwrap();
    /// let mut subnodes = node.subnodes();
    ///
    /// assert!(subnodes.next().is_some());
    /// assert!(subnodes.next().is_some());
    /// assert_eq!(subnodes.next(), None);
    ///
    /// ```
    pub fn subnodes(&self) -> impl Iterator<Item = DtNode> + '_ {
        self.syntax.child_nodes().filter_map(DtNode::cast)
    }

    /// Returns the unit addresses ident.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, Document, HasIdent};
    ///
    /// let src = "/ { my_node@unit_address { foo = <1>; }; };";
    /// let doc = Document::cast(dt_parser::parse(src.as_bytes()).unwrap()).unwrap();
    /// let node = doc
    ///     .nodes().next().unwrap()
    ///     .subnodes().next().unwrap();
    ///
    /// assert_eq!(node.ident().unwrap().text(src), Some("my_node"));
    /// assert_eq!(node.unit_address().unwrap().text(src), Some("unit_address"));
    ///
    /// ```
    pub fn unit_address(&self) -> Option<Ident> {
        self.syntax
            .child_nodes()
            .find(|node| node.green.kind == NodeKind::DtNodeUnitAddress)?
            .child_nodes()
            .find_map(Ident::cast)
    }

    /// Returns the name with the unit address.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, Document, HasIdent};
    ///
    /// let src = "/ { my_node@unit_address { foo = <1>; }; };";
    /// let doc = Document::cast(dt_parser::parse(src.as_bytes()).unwrap()).unwrap();
    /// let node = doc
    ///     .nodes().next().unwrap()
    ///     .subnodes().next().unwrap();
    ///
    /// assert_eq!(node.name(src).as_deref(), Some("my_node@unit_address"));
    ///
    /// ```
    pub fn name<'i>(&self, src: &'i str) -> Option<Cow<'i, str>> {
        Some(match self.unit_address() {
            None => Cow::Borrowed(self.ident()?.text(src)?),
            Some(unit) => Cow::Owned({
                let ident = self.ident()?.text(src)?;
                let unit = unit.text(src)?;
                let mut out = String::with_capacity(ident.len() + 1 + unit.len());
                out.push_str(ident);
                out.push('@');
                out.push_str(unit);
                out
            }),
        })
    }

    /// Returns true if this is a node extension (e.g. `&UART_1 { .. };`).
    ///
    /// NOTE: for the extension to be valid the immediate parent must be a [`Document`]
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, Document};
    ///
    /// let src = b"&a { b { .. }; };";
    /// let doc = Document::cast(dt_parser::parse(src).unwrap()).unwrap();
    /// let node_a = doc.nodes().next().unwrap();
    /// let node_b = node_a.subnodes().next().unwrap();
    ///
    /// assert!(node_a.is_extension());
    /// assert!(!node_b.is_extension());
    ///
    /// ```
    pub fn is_extension(&self) -> bool {
        self.syntax
            .child_nodes()
            .any(|node| node.green.kind == NodeKind::DtNodeExtension)
    }

    /// Returns true if this is not a node extension.
    ///
    /// <div class="warning">
    /// This may be part of an extension.
    ///
    /// You can try the following code to get the value
    /// for all parent ancestors.
    /// </div>
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, Document};
    ///
    /// let src = b"&a { b { c { .. }; }; };";
    /// let doc = Document::cast(dt_parser::parse(src).unwrap()).unwrap();
    /// let node = doc
    ///     .nodes().next().unwrap()
    ///     .subnodes().next().unwrap()
    ///     .subnodes().next().unwrap();
    ///
    /// assert!(node.parent_nodes().any(|node| node.is_extension()));
    /// ```
    pub fn is_concrete(&self) -> bool {
        !self.is_extension()
    }

    /// Returns an iterator over [`DtNode`] ancestors.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, Document};
    ///
    /// let src = b"&a { b { c { d { .. }; }; }; };";
    /// let doc = Document::cast(dt_parser::parse(src).unwrap()).unwrap();
    /// let node = doc
    ///     .nodes().next().unwrap()
    ///     .subnodes().next().unwrap()
    ///     .subnodes().next().unwrap()
    ///     .subnodes().next().unwrap();
    ///
    /// assert_eq!(node.parent_nodes().count(), 3);
    ///
    /// ```
    pub fn parent_nodes(&self) -> impl Iterator<Item = DtNode> + '_ {
        self.syntax.parent_ancestors().filter_map(DtNode::cast)
    }

    /// Returns true if this is a concrete node with the name "/".
    pub fn is_root(&self, src: &str) -> bool {
        self.is_concrete() && self.name(src).map(|name| name == "/").unwrap_or_default()
    }

    /// Returns an iterator over this node's path.
    ///
    /// This skips any nodes with the name "/".
    pub fn path<'i>(&self, src: &'i str) -> impl Iterator<Item = Cow<'i, str>> {
        self.syntax
            .parent_ancestors()
            .filter_map(DtNode::cast)
            .chain(std::iter::once(self.clone()))
            .filter_map(|node| node.name(src))
            .filter(|part| part != "/")
    }
}
impl HasIdent for DtNode {}
impl HasLabel for DtNode {}

/// A [`DtNode`]'s optional label
///
/// Kind: [`NodeKind::DtLabel`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DtLabel {
    syntax: Arc<RedNode>,
}
impl AstNode for DtLabel {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtLabel => Some(Self { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> Arc<RedNode> {
        self.syntax.clone()
    }
    fn syntax_ref(&self) -> &RedNode {
        &self.syntax
    }
}
impl HasIdent for DtLabel {}

/// Top-level item in a [`Document`]
///
/// Kind: [`NodeKind::DtNode`] or [`NodeKind::Directive`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ToplevelItem {
    /// A Devicetree node
    Node(DtNode),
    /// A compiler or DTS directive
    ///
    /// e.g. `/include/ file.dts;`, `#include file.dts`
    Directive(Directive),
}
impl AstNode for ToplevelItem {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtNode => Some(Self::Node(DtNode { syntax })),
            NodeKind::Directive => Some(Self::Directive(Directive { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> Arc<RedNode> {
        match self {
            Self::Node(it) => it.syntax.clone(),
            Self::Directive(it) => it.syntax.clone(),
        }
    }
    fn syntax_ref(&self) -> &RedNode {
        match self {
            Self::Node(it) => &it.syntax,
            Self::Directive(it) => &it.syntax,
        }
    }
}
impl HasIdent for ToplevelItem {}
