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
//! #     kind: NodeKind::DtNode,
//! #     width: 6,
//! #     children: vec! [
//! #         GreenItem::Token(Arc::new(GreenToken {
//! #             kind: TokenKind::Name,
//! #             text: dt_parser::cst2::TokenText::Static("foo"),
//! #         })),
//! #         GreenItem::Token(Arc::new(GreenToken {
//! #             kind: TokenKind::LCurly,
//! #             text: dt_parser::cst2::TokenText::Static("{"),
//! #         })),
//! #         GreenItem::Token(Arc::new(GreenToken {
//! #             kind: TokenKind::LCurly,
//! #             text: dt_parser::cst2::TokenText::Static("}"),
//! #         })),
//! #         GreenItem::Token(Arc::new(GreenToken {
//! #             kind: TokenKind::LCurly,
//! #             text: dt_parser::cst2::TokenText::Static(";"),
//! #         })),
//! #     ]
//! # });
//! # red_node = RedNode::new(green_node);
//! let ast = ast::DtNode::cast(red_node).unwrap();
//!
//! // Use Has- traits
//! use dt_parser::ast::HasName;
//! assert!(ast.name().is_some())
//! ```
use std::borrow::Cow;
use std::sync::Arc;

use either::Either;
use enum_as_inner::EnumAsInner;

use crate::cst2::{
    lexer::TokenKind, parser::Parse, NodeKind, RedItem, RedNode, RedToken, TreeItem,
};

/// Trait used for downcasting from [`RedNode`]s to AST nodes.
pub trait AstNode: Sized {
    /// Try to cast a [`RedNode`] to an AST node.
    ///
    /// The implementor must check [`GreenNode::kind`](crate::cst2::GreenNode::kind)!
    ///
    /// All functions which reference children (e.g. [`SourceFile::nodes`]) should return [`Option`]s
    /// or similar!
    fn cast(syntax: Arc<RedNode>) -> Option<Self>;

    /// Returns the syntax node.
    fn syntax(&self) -> Arc<RedNode>;

    /// Returns a reference to the syntax node.
    fn syntax_ref(&self) -> &RedNode;
}

/// Trait used for downcasting from [`RedToken`]s to AST tokens.
pub trait AstToken: Sized {
    /// Try to cast a [`RedToken`] to an AST token.
    ///
    /// The implementor must check [`GreenToken::kind`](crate::cst2::GreenToken::kind)!
    fn cast(syntax: Arc<RedToken>) -> Option<Self>;

    /// Returns the syntax token.
    fn syntax(&self) -> Arc<RedToken>;

    /// Returns a reference to the syntax token.
    fn syntax_ref(&self) -> &RedToken;

    /// Returns the green token's kind.
    fn kind(&self) -> TokenKind {
        self.syntax_ref().green.kind
    }
}

/// Trait used for downcasting from [`RedToken`]s and [`RedNode`]s to AST nodes and tokens.
///
/// This must only be implemented for enums.
pub trait AstNodeOrToken: Sized {
    /// Try to cast a [`RedToken`] or a [`RedNode`]s to an AST node or token.
    fn cast(syntax: RedItem) -> Option<Self> {
        match syntax {
            RedItem::Node(syntax) => Self::cast_node(syntax),
            RedItem::Token(syntax) => Self::cast_token(syntax),
        }
    }

    /// Try to cast a [`RedNode`] to an AST node.
    ///
    /// The implementor must check [`GreenNode::kind`](crate::cst2::GreenNode::kind)!
    ///
    /// All functions which reference children (e.g. [`SourceFile::nodes`]) should return [`Option`]s
    /// or similar!
    fn cast_node(syntax: Arc<RedNode>) -> Option<Self>;

    /// Try to cast a [`RedToken`] to an AST token.
    ///
    /// The implementor must check [`GreenToken::kind`](crate::cst2::GreenToken::kind)!
    fn cast_token(syntax: Arc<RedToken>) -> Option<Self>;

    /// Returns the syntax node or token.
    fn syntax(&self) -> RedItem;

    /// Returns a reference to the syntax node or token.
    fn syntax_ref(&self) -> TreeItem<&RedNode, &RedToken>;
}

// TODO: Better to just make if-let chains? I think this breaks rust-analyzer
/// Matches an [`Arc<RedNode>`][RedNode] against an `ast` type.
///
/// # Example:
///
/// ```
/// # use dt_parser::{match_ast, ast, cst2::RedNode};
/// # use std::sync::Arc;
/// let node: Arc<RedNode>;
/// # use dt_parser::cst2::{GreenNode, NodeKind};
/// # node = RedNode::new(Arc::new(GreenNode { kind: NodeKind::SourceFile, width: 0, children: Vec::new() }));
/// # let _ =
/// match_ast! {
///     match node {
///         ast::DtNode(it) => { .. },
///         ast::DtProperty(it) => { .. },
///         _ => { .. },
///     }
/// }
/// # ;
/// ```
#[macro_export]
macro_rules! match_ast {
    (match $node:ident { $($tt:tt)* }) => { $crate::match_ast!(match ($node) { $($tt)* }) };
    (match ($node:expr) {
        $( $( $path:ident )::+ ($it:pat) => $res:expr, )*
        _ => $catch_all:expr $(,)?
    }) => {{
        use $crate::ast::AstNode;
        $( if let Some($it) = $($path::)+cast($node.clone()) { $res } else )*
        { $catch_all }
    }};
}
//pub use match_ast;

/// Trait for [`AstNode`]s with [`Name`]s
pub trait HasName: AstNode {
    /// Returns the [`Name`] if it exists.
    fn name(&self) -> Option<Name> {
        self.syntax().child_tokens().find_map(Name::cast)
    }
}

/// Trait for [`AstNode`]s with [`DtLabel`]s
pub trait HasLabel: AstNode {
    /// Returns the [`DtLabel`] if it exists.
    fn label(&self) -> Option<DtLabel> {
        self.syntax().child_nodes().find_map(DtLabel::cast)
    }
}

/// A [Devicetree][1] source file, which contains [`ToplevelItem`]s.
///
/// Kind: [`NodeKind::SourceFile`]
///
/// [1]: https://devicetree.org
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceFile {
    syntax: Arc<RedNode>,
}
impl AstNode for SourceFile {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::SourceFile => Some(Self { syntax }),
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
impl SourceFile {
    /// Returns an iterator over direct [`ToplevelItem`] children.
    pub fn items(&self) -> impl Iterator<Item = ToplevelItem> + '_ {
        self.syntax.children().filter_map(ToplevelItem::cast)
    }
    /// Returns an iterator over direct [`Directive`] children.
    pub fn directives(&self) -> impl Iterator<Item = Directive> + '_ {
        self.syntax.child_nodes().filter_map(Directive::cast)
    }
    /// Returns an iterator over direct [`DtNode`] children.
    pub fn nodes(&self) -> impl Iterator<Item = DtNode> + '_ {
        self.syntax.child_nodes().filter_map(DtNode::cast)
    }

    // TODO: don't make parser public
    // TODO: return a RedNode or SourceFile
    pub fn parse(text: &str) -> Parse {
        let parse = crate::cst2::parser::parse(text);
        // TODO:
        //errors.extend(validation::validate(&root));

        assert_eq!(parse.green_node.kind, NodeKind::SourceFile);
        parse
    }
}

/// A [DTS directive][1].
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
    /// use dt_parser::ast::{DtNode, AstNode, SourceFile};
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "/ { a = <1 2 3> <4>; };";
    /// let file = SourceFile::parse(src).source_file();
    /// let property = file.nodes().next().unwrap().properties().next().unwrap();
    /// let mut values = property.values();
    ///
    /// assert!(values.next().is_some());
    /// assert!(values.next().is_some());
    /// assert_eq!(values.next(), None);
    ///
    /// ```
    pub fn values(&self) -> impl Iterator<Item = PropValue> + '_ {
        match self
            .syntax
            .child_nodes()
            .find(|node| node.green.kind == NodeKind::PropValueList)
        {
            Some(value_list) => {
                Either::Left(value_list.owned_children().filter_map(PropValue::cast))
            }
            None => Either::Right(std::iter::empty()),
        }
    }

    /// Returns the unit addresses.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, SourceFile, HasName};
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "/ { my_prop@unit_address = <1>; };";
    /// let file = SourceFile::parse(src).source_file();
    /// let property = file
    ///     .nodes().next().unwrap()
    ///     .properties().next().unwrap();
    ///
    /// assert_eq!(property.name().unwrap().text(src), Some("my_prop"));
    /// assert_eq!(property.unit_address().unwrap().text(src), Some("unit_address"));
    ///
    /// ```
    pub fn unit_address(&self) -> Option<Name> {
        self.syntax
            .child_nodes()
            .find(|node| node.green.kind == NodeKind::UnitAddress)?
            .child_tokens()
            .find_map(Name::cast)
    }
}
impl HasName for DtProperty {}
impl HasLabel for DtProperty {}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum PropValue {
    String(Arc<RedToken>),
    CellList(DtCellList),
}
impl AstNodeOrToken for PropValue {
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
    fn syntax(&self) -> RedItem {
        match self {
            Self::CellList(it) => RedItem::Node(it.syntax.clone()),
            Self::String(it) => RedItem::Token(it.clone()),
        }
    }
    fn syntax_ref(&self) -> TreeItem<&RedNode, &RedToken> {
        match self {
            Self::CellList(it) => TreeItem::Node(&it.syntax),
            Self::String(it) => TreeItem::Token(it),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum Cell {
    Phandle(DtPhandle),
    Number(Arc<RedToken>),
}
impl AstNodeOrToken for Cell {
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
    fn syntax(&self) -> RedItem {
        match self {
            Self::Phandle(it) => RedItem::Node(it.syntax.clone()),
            Self::Number(it) => RedItem::Token(it.clone()),
        }
    }
    fn syntax_ref(&self) -> TreeItem<&RedNode, &RedToken> {
        match self {
            Self::Phandle(it) => TreeItem::Node(&it.syntax),
            Self::Number(it) => TreeItem::Token(it),
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
        self.syntax.children().filter_map(Cell::cast)
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
    /// use dt_parser::ast::{DtNode, AstNode, SourceFile};
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "/ { a = <0>; b; };";
    /// let file = SourceFile::parse(src).source_file();
    /// let node = file.nodes().next().unwrap();
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
    /// use dt_parser::ast::{DtNode, AstNode, SourceFile};
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "/ { a = <0>; b { c = <1>; }; d {}; };";
    /// let file = SourceFile::parse(src).source_file();
    /// let node = file.nodes().next().unwrap();
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

    /// Returns the unit addresses.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, SourceFile, HasName};
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "/ { my_node@unit_address { foo = <1>; }; };";
    /// let file = SourceFile::parse(src).source_file();
    /// let node = file
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
            .child_tokens()
            .find_map(Name::cast)
    }

    /// Returns the name with the unit address.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, SourceFile, HasName};
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "/ { my_node@unit_address { foo = <1>; }; };";
    /// let file = SourceFile::parse(src).source_file();
    /// let node = file
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
    /// NOTE: for the extension to be valid the immediate parent must be a [`SourceFile`]
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, SourceFile};
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "&a { b { .. }; };";
    /// let file = SourceFile::parse(src).source_file();
    /// let node_a = file.nodes().next().unwrap();
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
    /// use dt_parser::ast::{DtNode, AstNode, SourceFile};
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "&a { b { c { .. }; }; };";
    /// let file = SourceFile::parse(src).source_file();
    /// let node = file
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
    /// use dt_parser::ast::{DtNode, AstNode, SourceFile};
    /// use dt_parser::cst2::RedNode;
    /// use std::sync::Arc;
    ///
    /// let src = "&a { b { c { d { .. }; }; }; };";
    /// let file = SourceFile::parse(src).source_file();
    /// let node = file
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

/// A generic name.
///
/// This can only be found in property and node names currently. TODO: update
///
/// This cannot be found in expressions, they use `TokenKind::Ident`.
///
/// Kind: [`TokenKind::Name`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    syntax: Arc<RedToken>,
}
impl AstToken for Name {
    fn cast(syntax: Arc<RedToken>) -> Option<Self> {
        match syntax.green.kind {
            TokenKind::Name => Some(Self { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> Arc<RedToken> {
        self.syntax.clone()
    }
    fn syntax_ref(&self) -> &RedToken {
        &self.syntax
    }
}
impl Name {
    pub fn text<'i>(&self, src: &'i str) -> Option<&'i str> {
        src.get(self.syntax.text_offset..(self.syntax.text_offset + self.syntax.green.text.len()))
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

/// A preprocessor directive
///
/// e.g. `#include file.dts`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PreprocessorDirective {
    syntax: Arc<RedToken>,
}
impl AstToken for PreprocessorDirective {
    fn cast(syntax: Arc<RedToken>) -> Option<Self> {
        match syntax.green.kind {
            kind if kind.is_preprocessor_directive() => Some(Self { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> Arc<RedToken> {
        self.syntax.clone()
    }
    fn syntax_ref(&self) -> &RedToken {
        &self.syntax
    }
}

/// Top-level item in a [`SourceFile`]
///
/// Kind: [`NodeKind::DtNode`] or [`NodeKind::Directive`]
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum ToplevelItem {
    /// A Devicetree node
    Node(DtNode),
    /// A DTS directive
    ///
    /// e.g. `/include/ file.dts;`
    Directive(Directive),
    /// A preprocessor directive
    ///
    /// e.g. `#include file.dts`
    PreprocessorDirective(PreprocessorDirective),
}
impl AstNodeOrToken for ToplevelItem {
    fn cast_node(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtNode => Some(Self::Node(DtNode { syntax })),
            NodeKind::Directive => Some(Self::Directive(Directive { syntax })),
            _ => None,
        }
    }
    fn cast_token(syntax: Arc<RedToken>) -> Option<Self> {
        Some(Self::PreprocessorDirective(PreprocessorDirective::cast(
            syntax,
        )?))
    }
    fn syntax(&self) -> RedItem {
        match self {
            Self::Node(it) => RedItem::Node(it.syntax.clone()),
            Self::Directive(it) => RedItem::Node(it.syntax.clone()),
            Self::PreprocessorDirective(it) => RedItem::Token(it.syntax.clone()),
        }
    }
    fn syntax_ref(&self) -> TreeItem<&RedNode, &RedToken> {
        match self {
            Self::Node(it) => TreeItem::Node(&it.syntax),
            Self::Directive(it) => TreeItem::Node(&it.syntax),
            Self::PreprocessorDirective(it) => TreeItem::Token(&it.syntax),
        }
    }
}
