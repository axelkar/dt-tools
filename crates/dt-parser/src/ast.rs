//! Abstract syntax trees.
//!
//! All AST items reference [`RedNode`]s through [`AstNode::syntax()`].
//!
//! To get an AST item, use [`AstNode::cast()`] with a [`Arc<RedNode>`] as follows:
//!
//! ```
//! use dt_parser::cst2::RedNode;
//! use dt_parser::ast::{self, AstNode as _};
//!
//! let red_node: Arc<RedNode>;
//! # use std::sync::Arc;
//! # use dt_parser::{TextRange, cst2::{GreenNode, GreenToken, GreenItem, NodeKind,
//! lexer::TokenKind}};
//! # let green_node = Arc::new(GreenNode {
//! #     kind: NodeKind::Directive,
//! #     width: 3,
//! #     children: vec! [
//! #         GreenItem::Token(Arc::new(GreenToken {
//! #             kind: TokenKind::Ident,
//! #             text: dt_parser::cst2::TokenText::Static("foo"),
//! #         }))
//! #     ]
//! # });
//! # red_node = RedNode::new(green_node);
//! let ast = ast::Directive::cast(red_node).unwrap();
//!
//! // Use Has- traits
//! use dt_parser::ast::HasIdent;
//! assert!(ast.ident().is_some())
//! ```
use std::borrow::Cow;
use std::sync::Arc;

use crate::cst2::{lexer::TokenKind, NodeKind, RedItem, RedNode, RedToken};

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

/// Trait for [`AstNode`]s with [`TokenKind::Comma`]s
pub trait HasComma: AstNode {
    /// Returns the first token with kind [`TokenKind::Comma`] if it exists.
    fn comma(&self) -> Option<Arc<RedToken>> {
        self.syntax()
            .child_tokens()
            .find(|tok| tok.green.kind == TokenKind::Comma)
    }
}

/// Trait for [`AstNode`]s with [`TokenKind::Ident`]s
// TODO: don't use this, instead use HasName
pub trait HasIdent: AstNode {
    /// Returns the first token with kind [`TokenKind::Ident`] if it exists.
    fn ident(&self) -> Option<Arc<RedToken>> {
        self.syntax()
            .child_tokens()
            .find(|tok| tok.green.kind == TokenKind::Ident)
    }
}

/// Trait for [`AstNode`]s with [`Name`]s
pub trait HasName: AstNode {
    /// Returns the [`Name`] if it exists.
    fn name(&self) -> Option<Name> {
        self.syntax().child_nodes().find_map(Name::cast)
    }
}

/// Trait for [`AstNode`]s with [`DtLabel`]s
pub trait HasLabel: AstNode {
    /// Returns the [`DtLabel`] if it exists.
    fn label(&self) -> Option<DtLabel> {
        self.syntax().child_nodes().find_map(DtLabel::cast)
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

// FIXME: not true
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
    /// Returns an iterator over direct [`DtPropValue`] children.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, Document};
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "/ { a = <1 2 3> <4>; };";
    /// let doc = Document::cast(RedNode::new(Arc::new(dt_parser::cst2::parser::parse(src).green_node))).unwrap();
    /// let property = doc.nodes().next().unwrap().properties().next().unwrap();
    /// let list = property.values().unwrap();
    /// let mut values = list.values();
    ///
    /// assert!(values.next().is_some());
    /// assert!(values.next().is_some());
    /// assert_eq!(values.next(), None);
    ///
    /// ```
    // TODO: better ergonomics
    pub fn values(&self) -> Option<PropValueList> {
        self.syntax.child_nodes().find_map(PropValueList::cast)
    }
}
impl HasName for DtProperty {}
impl HasLabel for DtProperty {}

/// A dt prop value list.
///
/// Kind: [`NodeKind::PropValueList`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PropValueList {
    syntax: Arc<RedNode>,
}
impl AstNode for PropValueList {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::PropValueList => Some(Self { syntax }),
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
impl PropValueList {
    pub fn values(&self) -> impl Iterator<Item = PropValue> + '_ {
        self.syntax.children().filter_map(PropValue::cast_item)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PropValue {
    String(Arc<RedToken>),
    CellList(DtCellList),
}
impl PropValue {
    fn cast_item(syntax: RedItem) -> Option<Self> {
        match syntax {
            RedItem::Node(syntax) => Self::cast_node(syntax),
            RedItem::Token(syntax) => Self::cast_token(syntax),
        }
    }
    fn cast_node(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtCellList => Some(Self::CellList(DtCellList { syntax })),
            _ => None,
        }
    }
    fn cast_token(syntax: Arc<RedToken>) -> Option<Self> {
        match syntax.green.kind {
            TokenKind::String => Some(Self::String(syntax)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Cell {
    Phandle(DtPhandle),
    Number(Arc<RedToken>),
}
impl Cell {
    fn cast_item(syntax: RedItem) -> Option<Self> {
        match syntax {
            RedItem::Node(syntax) => Self::cast_node(syntax),
            RedItem::Token(syntax) => Self::cast_token(syntax),
        }
    }
    fn cast_node(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtPhandle => Some(Self::Phandle(DtPhandle { syntax })),
            _ => None,
        }
    }
    fn cast_token(syntax: Arc<RedToken>) -> Option<Self> {
        match syntax.green.kind {
            TokenKind::Number => Some(Self::Number(syntax)),
            _ => None,
        }
    }
}

/// A phandle item in a [`DtCellList`].
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
            .any(|tok| tok.green.kind == TokenKind::LCurly)
    }
    /// Returns true if this is a label phandle and not a path phandle.
    ///
    /// # Example
    ///
    /// TODO: Example for is_label
    pub fn is_label(&self) -> bool {
        !self.is_path()
    }
}
impl HasName for DtPhandle {}

/// A Devicetree cell list.
///
/// Kind: [`NodeKind::DtCellList`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DtCellList {
    syntax: Arc<RedNode>,
}
impl AstNode for DtCellList {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtCellList => Some(Self { syntax }),
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
impl DtCellList {
    // TODO: add example
    pub fn cells(&self) -> impl Iterator<Item = Cell> + '_ {
        self.syntax.children().filter_map(Cell::cast_item)
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
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "/ { a = <0>; b; };";
    /// let doc = Document::cast(RedNode::new(Arc::new(dt_parser::cst2::parser::parse(src).green_node))).unwrap();
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
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "/ { a = <0>; b { c = <1>; }; d {}; };";
    /// let doc = Document::cast(RedNode::new(Arc::new(dt_parser::cst2::parser::parse(src).green_node))).unwrap();
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
    /// use dt_parser::ast::{DtNode, AstNode, Document, HasName};
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "/ { my_node@unit_address { foo = <1>; }; };";
    /// let doc = Document::cast(RedNode::new(Arc::new(dt_parser::cst2::parser::parse(src).green_node))).unwrap();
    /// let node = doc
    ///     .nodes().next().unwrap()
    ///     .subnodes().next().unwrap();
    ///
    /// assert_eq!(node.name().unwrap().text(src), Some("my_node"));
    /// assert_eq!(node.unit_address().unwrap().text(src), Some("unit_address"));
    ///
    /// ```
    pub fn unit_address(&self) -> Option<Name> {
        self.syntax
            .child_nodes()
            .find(|node| node.green.kind == NodeKind::UnitAddress)?
            .child_nodes()
            .find_map(Name::cast)
    }

    /// Returns the name with the unit address.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, Document, HasName};
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "/ { my_node@unit_address { foo = <1>; }; };";
    /// let doc = Document::cast(RedNode::new(Arc::new(dt_parser::cst2::parser::parse(src).green_node))).unwrap();
    /// let node = doc
    ///     .nodes().next().unwrap()
    ///     .subnodes().next().unwrap();
    ///
    /// assert_eq!(node.text_name(src).unwrap().to_string(), "my_node@unit_address");
    ///
    /// ```
    pub fn text_name<'i>(&self, src: &'i str) -> Option<Cow<'i, str>> {
        Some(match self.unit_address() {
            None => Cow::Borrowed(self.name()?.text(src)?),

            // Since whitespace before and after @ should be deny-linted I should be able to
            // substring this too:
            Some(unit) => Cow::Owned({
                let name = self.name()?;
                let name = name.text(src)?;
                let unit = unit.text(src)?;
                let mut out = String::with_capacity(name.len() + 1 + unit.len());
                out.push_str(name);
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
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "&a { b { .. }; };";
    /// let doc = Document::cast(RedNode::new(Arc::new(dt_parser::cst2::parser::parse(src).green_node))).unwrap();
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
            .any(|node| node.green.kind == NodeKind::DtPhandle)
    }

    pub fn extension_name(&self) -> Option<DtPhandle> {
        self.syntax.child_nodes().find_map(DtPhandle::cast)
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
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "&a { b { c { .. }; }; };";
    /// let doc = Document::cast(RedNode::new(Arc::new(dt_parser::cst2::parser::parse(src).green_node))).unwrap();
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
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "&a { b { c { d { .. }; }; }; };";
    /// let doc = Document::cast(RedNode::new(Arc::new(dt_parser::cst2::parser::parse(src).green_node))).unwrap();
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
    pub fn is_root(&self) -> bool {
        self.syntax
            .child_tokens()
            .any(|tok| tok.green.kind == TokenKind::Slash)
    }

    /// Returns an iterator over this node's path.
    ///
    /// This skips any nodes with the name "/".
    pub fn path<'i>(&self, src: &'i str) -> impl Iterator<Item = Cow<'i, str>> {
        self.syntax
            .parent_ancestors()
            .filter_map(DtNode::cast)
            .chain(std::iter::once(self.clone()))
            .filter(|node| !node.is_root())
            .filter_map(|node| node.text_name(src))
    }
}
impl HasName for DtNode {}
impl HasLabel for DtNode {}

/// A name composed of different kinds of tokens.
///
/// Kind: [`NodeKind::Name`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    syntax: Arc<RedNode>,
}
impl AstNode for Name {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::Name => Some(Self { syntax }),
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
impl Name {
    pub fn text<'i>(&self, src: &'i str) -> Option<&'i str> {
        src.get(self.syntax.text_offset..(self.syntax.text_offset + self.syntax.green.width))
    }
}

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
impl HasName for DtLabel {}

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
