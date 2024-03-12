use std::{borrow::Cow, sync::Arc};

use crate::cst::{
    kinds::{NodeKind, TokenKind}, GreenToken, RedItem, RedNode, RedToken, TreeItem
};

pub trait AstNode {
    fn cast(syntax: Arc<RedNode>) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> Arc<RedNode>;
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

pub trait HasIdent: AstNode {
    fn ident(&self) -> Option<Ident> {
        self.syntax().child_nodes().find_map(Ident::cast)
    }
}
pub trait HasLabel: AstNode {
    fn label(&self) -> Option<DtLabel> {
        self.syntax().child_nodes().find_map(DtLabel::cast)
    }
}

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
    pub fn ident_tok(&self) -> Option<Arc<GreenToken>> {
        Some(
            self.syntax
                .child_tokens()
                .find(|tok| tok.green.kind == TokenKind::Ident)?
                .green
                .clone(),
        )
    }
    pub fn text<'i>(&self, src: &'i str) -> Option<&'i str> {
        self.ident_tok()?.span.text(src)
    }
}

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
    /// Get direct [`Directive`] children
    pub fn directives(&self) -> impl Iterator<Item = Directive> + '_ {
        self.syntax.child_nodes().filter_map(Directive::cast)
    }
    /// Get direct [`DtNode`] children
    pub fn nodes(&self) -> impl Iterator<Item = DtNode> + '_ {
        self.syntax.child_nodes().filter_map(DtNode::cast)
    }
}

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
    /// Get direct [`PropValue`] children
    pub fn values(&self) -> impl Iterator<Item = PropValue> + '_ {
        self.syntax.child_nodes().filter_map(PropValue::cast)
    }
}
impl HasIdent for DtProperty {}
impl HasLabel for DtProperty {}

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
    /// Whether this is a path phandle and not a label phandle
    pub fn is_path(&self) -> bool {
        self.syntax
            .child_tokens()
            .any(|tok| tok.green.kind == TokenKind::DtPathPhandle)
    }
    /// Whether this is a label phandle and not a path phandle
    pub fn is_label(&self) -> bool {
        !self.is_path()
    }
}
impl HasIdent for DtPhandle {}

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
    /// Get either phandle or token children
    pub fn values(&self) -> impl Iterator<Item = TreeItem<DtPhandle, Arc<RedToken>>> + '_ {
        self.syntax.children().filter_map(|item| Some(match item {
            RedItem::Node(node) => TreeItem::Node(DtPhandle::cast(node)?),
            RedItem::Token(token) if token.green.kind == TokenKind::DtNumber => {
                TreeItem::Token(token)
            },
            _ => return None
        }))
    }
}

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
    pub fn contents(&self) -> Option<Arc<RedToken>> {
        self.syntax
            .child_tokens()
            .find(|tok| tok.green.kind == TokenKind::DtStringContents)
    }
}

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
    /// Get direct [`DtProperty`] children
    pub fn properties(&self) -> impl Iterator<Item = DtProperty> + '_ {
        self.syntax.child_nodes().filter_map(DtProperty::cast)
    }
    /// Get direct [`DtNode`] children
    pub fn subnodes(&self) -> impl Iterator<Item = DtNode> + '_ {
        self.syntax.child_nodes().filter_map(DtNode::cast)
    }

    /// Get [`NodeKind::DtNodeUnitAddress`] -> [`Ident`]
    pub fn unit_address(&self) -> Option<Ident> {
        self.syntax
            .child_nodes()
            .find(|node| node.green.kind == NodeKind::DtNodeUnitAddress)?
            .child_nodes()
            .find_map(Ident::cast)
    }
    /// Get the name with the unit address
    pub fn name<'i>(&self, src: &'i str) -> Option<Cow<'i, str>> {
        Some(match self.unit_address() {
            None => Cow::Borrowed(self.ident()?.text(src)?),
            Some(unit) => Cow::Owned(format!("{}@{}", self.ident()?.text(src)?, unit.text(src)?))
        })
    }

    /// Whether this is a node extension (e.g. `&UART_1 { a = <1>; };`)
    ///
    /// NOTE: make sure that the parent is ast::Document
    pub fn is_extension(&self) -> bool {
        self.syntax
            .child_nodes()
            .any(|node| node.green.kind == NodeKind::DtNodeExtension)
    }
    /// Whether this is not a node extension (may be part of an extension, though)
    pub fn is_concrete(&self) -> bool {
        !self.is_extension()
    }
    /// Whether this is a root node
    pub fn is_root(&self, src: &str) -> bool {
        self.is_concrete()
            && self
                .ident()
                .and_then(|ident| Some(ident.text(src)? == "/"))
                .unwrap_or_default()
    }

    /// Get this node's path
    ///
    /// This skips any nodes with the name "/"
    pub fn path<'i>(&self, src: &'i str) -> Vec<Cow<'i, str>> {
        self.syntax
            .parent_ancestors()
            .filter_map(DtNode::cast)
            .chain(std::iter::once(self.clone()))
            .filter_map(|node| node.name(src))
            .filter(|part| part != &"/")
            .collect()
    }
}
impl HasIdent for DtNode {}
impl HasLabel for DtNode {}

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Item {
    Node(DtNode),
    Property(DtProperty),
    Directive(Directive)
}
impl AstNode for Item {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtNode => Some(Self::Node(DtNode { syntax })),
            NodeKind::DtProperty => Some(Self::Property(DtProperty { syntax })),
            NodeKind::Directive => Some(Self::Directive(Directive { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> Arc<RedNode> {
        match self {
            Self::Node(it) => it.syntax.clone(),
            Self::Property(it) => it.syntax.clone(),
            Self::Directive(it) => it.syntax.clone(),
        }
    }
    fn syntax_ref(&self) -> &RedNode {
        match self {
            Self::Node(it) => &it.syntax,
            Self::Property(it) => &it.syntax,
            Self::Directive(it) => &it.syntax,
        }
    }
}
impl HasIdent for Item {}
