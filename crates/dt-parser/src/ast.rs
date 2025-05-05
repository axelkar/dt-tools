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
//! # use dt_parser::{TextRange, cst::{GreenNode, GreenToken, GreenItem, NodeKind},
//! lexer::TokenKind};
//! # let green_node = Arc::new(GreenNode {
//! #     kind: NodeKind::DtNode,
//! #     width: 6,
//! #     children: vec! [
//! #         GreenItem::Token(Arc::new(GreenToken {
//! #             kind: TokenKind::Name,
//! #             text: dt_parser::cst::TokenText::Static("foo"),
//! #         })),
//! #         GreenItem::Token(Arc::new(GreenToken {
//! #             kind: TokenKind::LCurly,
//! #             text: dt_parser::cst::TokenText::Static("{"),
//! #         })),
//! #         GreenItem::Token(Arc::new(GreenToken {
//! #             kind: TokenKind::LCurly,
//! #             text: dt_parser::cst::TokenText::Static("}"),
//! #         })),
//! #         GreenItem::Token(Arc::new(GreenToken {
//! #             kind: TokenKind::LCurly,
//! #             text: dt_parser::cst::TokenText::Static(";"),
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

use crate::{
    cst::{GreenToken, NodeKind, RedItem, RedItemRef, RedNode, RedToken, TreeItem},
    lexer::TokenKind,
    parser::Parse,
};

/// Trait used for downcasting from [`RedNode`]s to AST nodes.
pub trait AstNode: Sized {
    /// Try to cast a [`RedNode`] to an AST node.
    ///
    /// The implementor must check [`GreenNode::kind`](crate::cst::GreenNode::kind)!
    ///
    /// All functions which reference children (e.g. [`SourceFile::nodes`]) should return [`Option`]s
    /// or similar!
    fn cast(syntax: Arc<RedNode>) -> Option<Self>;

    /// Returns a reference to the syntax node.
    fn syntax(&self) -> &Arc<RedNode>;
}

/// Trait used for downcasting from [`RedToken`]s to AST tokens.
pub trait AstToken: Sized {
    /// Try to cast a [`RedToken`] to an AST token.
    ///
    /// The implementor must check [`GreenToken::kind`](crate::cst::GreenToken::kind)!
    fn cast(syntax: Arc<RedToken>) -> Option<Self>;

    /// Returns a reference to the syntax token.
    fn syntax(&self) -> &Arc<RedToken>;

    /// Returns the green token's kind.
    fn kind(&self) -> TokenKind {
        self.syntax().green.kind
    }
}

/// Trait used for downcasting from [`RedToken`]s and [`RedNode`]s to AST nodes and tokens.
///
/// This must only be implemented for enums.
pub trait AstNodeOrToken: Sized {
    /// Try to cast a [`RedToken`] or a [`RedNode`]s to an AST node or token.
    // TODO: return RedItem back if it doesn't succeed, so reditem doesn't have to be cloned in
    // match_ast!
    #[must_use]
    fn cast(syntax: RedItem) -> Option<Self> {
        match syntax {
            RedItem::Node(syntax) => Self::cast_node(syntax),
            RedItem::Token(syntax) => Self::cast_token(syntax),
        }
    }

    /// Try to cast a [`RedNode`] to an AST node.
    ///
    /// The implementor must check [`GreenNode::kind`](crate::cst::GreenNode::kind)!
    ///
    /// All functions which reference children (e.g. [`SourceFile::nodes`]) should return [`Option`]s
    /// or similar!
    fn cast_node(syntax: Arc<RedNode>) -> Option<Self>;

    /// Try to cast a [`RedToken`] to an AST token.
    ///
    /// The implementor must check [`GreenToken::kind`](crate::cst::GreenToken::kind)!
    fn cast_token(syntax: Arc<RedToken>) -> Option<Self>;

    /// Returns a reference to the syntax node or token.
    fn syntax(&self) -> RedItemRef;
}

// TODO: Better to just make if-let chains? I think this breaks rust-analyzer
/// Matches an [`Arc<RedNode>`][RedNode] against an `ast` type.
///
/// # Example:
///
/// ```
/// # use dt_parser::{match_ast, ast, cst::RedNode};
/// # use std::sync::Arc;
/// let node: Arc<RedNode>;
/// # use dt_parser::cst::{GreenNode, NodeKind};
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

/// Trait for [`AstNode`]s with [`Name`]s
pub trait HasName: AstNode {
    // TODO: &'self str text instead of something bound to Name.syntax.green
    /// Returns the first [`Name`] if it exists.
    fn name(&self) -> Option<Name> {
        self.syntax().child_tokens().find_map(Name::cast)
    }
    // TODO: make compatible users use this because there are no atomic operations or allocationa
    fn green_name(&self) -> Option<&Arc<GreenToken>> {
        self.syntax()
            .green
            .child_tokens()
            .find(|tok| tok.kind == TokenKind::Name)
    }
}

// FIXME: This should be able to return multiple labels
/// Trait for [`AstNode`]s with [`DtLabel`]s
pub trait HasLabel: AstNode {
    /// Returns the first [`DtLabel`] if it exists.
    fn label(&self) -> Option<DtLabel> {
        self.syntax().child_nodes().find_map(DtLabel::cast)
    }
}

/// Trait for [`AstNode`]s with [`MacroInvocation`]s
pub trait HasMacroInvocation: AstNode {
    /// Returns the first [`MacroInvocation`] if it exists.
    fn macro_invocation(&self) -> Option<MacroInvocation> {
        self.syntax().child_nodes().find_map(MacroInvocation::cast)
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
    fn syntax(&self) -> &Arc<RedNode> {
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
    /// Returns an iterator over direct [`DtProperty`] children.
    ///
    /// Note that properties must not be at the top level.
    pub fn properties(&self) -> impl Iterator<Item = DtProperty> + '_ {
        self.syntax.child_nodes().filter_map(DtProperty::cast)
    }

    // TODO: don't make parser public
    // TODO: return a RedNode or SourceFile
    #[must_use]
    pub fn parse(text: &str) -> Parse {
        let parse = crate::parser::parse(text);
        // TODO:
        //errors.extend(validation::validate(&root));

        debug_assert_eq!(parse.green_node.kind, NodeKind::SourceFile);
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
    fn syntax(&self) -> &Arc<RedNode> {
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
    fn syntax(&self) -> &Arc<RedNode> {
        &self.syntax
    }
}
impl DtProperty {
    /// Returns an iterator over direct [`PropValue`] children.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, SourceFile};
    /// use dt_parser::cst::RedNode;
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
    /// use dt_parser::cst::RedNode;
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
impl HasMacroInvocation for DtProperty {}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum PropValue {
    String(Arc<RedToken>),
    CellList(DtCellList),
    /// String-based reference
    Phandle(DtPhandle),
    Bytestring(Arc<RedToken>),
    Macro(MacroInvocation),
}
impl AstNodeOrToken for PropValue {
    fn cast_node(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtCellList => Some(Self::CellList(DtCellList { syntax })),
            NodeKind::DtPhandle => Some(Self::Phandle(DtPhandle { syntax })),
            NodeKind::MacroInvocation => Some(Self::Macro(MacroInvocation { syntax })),
            _ => None,
        }
    }
    fn cast_token(syntax: Arc<RedToken>) -> Option<Self> {
        match syntax.green.kind {
            TokenKind::String => Some(Self::String(syntax)),
            TokenKind::DtBytestring => Some(Self::Bytestring(syntax)),
            _ => None,
        }
    }
    fn syntax(&self) -> RedItemRef {
        match self {
            Self::CellList(it) => TreeItem::Node(&it.syntax),
            Self::String(it) | Self::Bytestring(it) => TreeItem::Token(it),
            Self::Phandle(it) => TreeItem::Node(&it.syntax),
            Self::Macro(it) => TreeItem::Node(&it.syntax),
        }
    }
}

/// A Devicetree cell, compiled to a 32-bit integer
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum Cell {
    /// 32-bit integer
    Number(Arc<RedToken>),
    /// Numeric reference
    Phandle(DtPhandle),
    Macro(MacroInvocation),
}
impl AstNodeOrToken for Cell {
    fn cast_node(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtPhandle => Some(Self::Phandle(DtPhandle { syntax })),
            NodeKind::MacroInvocation => Some(Self::Macro(MacroInvocation { syntax })),
            _ => None,
        }
    }
    fn cast_token(syntax: Arc<RedToken>) -> Option<Self> {
        match syntax.green.kind {
            TokenKind::Number => Some(Self::Number(syntax)),
            _ => None,
        }
    }
    fn syntax(&self) -> RedItemRef {
        match self {
            Self::Number(it) => TreeItem::Token(it),
            Self::Phandle(it) => TreeItem::Node(&it.syntax),
            Self::Macro(it) => TreeItem::Node(&it.syntax),
        }
    }
}

/// A phandle item as a [`Cell`] or a [`PropValue`].
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
    fn syntax(&self) -> &Arc<RedNode> {
        &self.syntax
    }
}
impl DtPhandle {
    /// Returns true if this is a path phandle and not a label phandle.
    #[must_use]
    pub fn is_path(&self) -> bool {
        self.syntax
            .child_tokens()
            .any(|tok| tok.green.kind == TokenKind::LCurly)
    }
    /// Returns true if this is a label phandle and not a path phandle.
    ///
    /// # Example
    ///
    /// TODO: Example for `is_label`
    #[must_use]
    pub fn is_label(&self) -> bool {
        !self.is_path()
    }
}
impl HasName for DtPhandle {}
impl HasMacroInvocation for DtPhandle {}

/// A macro invocation in an expression in a [`Cell`], as a [`PropValue`] or in a name position.
///
/// Kind: [`NodeKind::MacroInvocation`]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroInvocation {
    syntax: Arc<RedNode>,
}
impl AstNode for MacroInvocation {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::MacroInvocation => Some(Self { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &Arc<RedNode> {
        &self.syntax
    }
}
impl MacroInvocation {
    /// Returns the macro's identifier
    #[must_use]
    pub fn ident(&self) -> Option<Arc<RedToken>> {
        self.syntax
            .child_tokens()
            .find(|tok| tok.green.kind == TokenKind::Ident)
    }
    /// Returns the macro's identifier
    #[must_use]
    pub fn green_ident(&self) -> Option<&Arc<GreenToken>> {
        self.syntax
            .green
            .child_tokens()
            .find(|tok| tok.kind == TokenKind::Ident)
    }
    /// Returns the macro invocation's arguments.
    pub fn arguments(&self) -> impl Iterator<Item = Arc<RedNode>> + '_ {
        self.syntax
            .child_nodes()
            .filter(|node| node.green.kind == NodeKind::MacroArgument)
    }
}

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
    fn syntax(&self) -> &Arc<RedNode> {
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
    fn syntax(&self) -> &Arc<RedNode> {
        &self.syntax
    }
}
// TODO: enum for DtProperty and DtNode + they could also share some functions via a trait
impl DtNode {
    /// Returns an iterator over direct [`DtProperty`] children.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, SourceFile};
    /// use dt_parser::cst::RedNode;
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
    /// use dt_parser::cst::RedNode;
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
    /// use dt_parser::cst::RedNode;
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
    /// use dt_parser::cst::RedNode;
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
    // TODO: Cow -> String, remove src
    #[must_use]
    pub fn text_name(&self, _src: &str) -> Option<Cow<'static, str>> {
        Some(match self.unit_address() {
            None => Cow::Owned(self.name()?.syntax.text().clone().to_owned()),

            // Since whitespace before and after @ should be deny-linted I should be able to
            // substring this too:
            Some(unit) => Cow::Owned({
                let name = self.name()?;
                let name = name.syntax.text();
                let unit = unit.syntax.text();
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
    /// use dt_parser::cst::RedNode;
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
    #[must_use]
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
    /// use dt_parser::cst::RedNode;
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
    #[must_use]
    pub fn is_concrete(&self) -> bool {
        !self.is_extension()
    }

    /// Returns an iterator over [`DtNode`] ancestors.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_parser::ast::{DtNode, AstNode, SourceFile};
    /// use dt_parser::cst::RedNode;
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
    #[must_use]
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
impl HasMacroInvocation for DtNode {}

/// A generic name.
///
/// This can only be found in label, property and node names currently. TODO: update
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
    fn syntax(&self) -> &Arc<RedToken> {
        &self.syntax
    }
}
impl Name {
    // TODO: make callers use syntax.text directly
    #[inline]
    #[deprecated = "use syntax.text() directly"]
    #[must_use]
    pub fn text(&self, _src: &str) -> Option<&str> {
        Some(self.syntax.text())
    }
}

// FIXME: dirty unimplemented hack for dt_lsp::hover
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NameRef {
    syntax: Arc<RedNode>,
}
impl AstNode for NameRef {
    fn cast(_syntax: Arc<RedNode>) -> Option<Self> {
        None
    }
    fn syntax(&self) -> &Arc<RedNode> {
        &self.syntax
    }
}
impl NameRef {
    /// Returns the [`Name`].
    ///
    /// # Panics
    ///
    /// This may panic if the CST was not parsed as expected. [`Name`] expected to be a direct child.
    pub fn name(&self) -> Name {
        self.syntax()
            .child_tokens()
            .find_map(Name::cast)
            .expect("Guaranteed to exist by parser promise")
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
    fn syntax(&self) -> &Arc<RedNode> {
        &self.syntax
    }
}
impl HasName for DtLabel {}
impl HasMacroInvocation for DtLabel {}

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
    fn syntax(&self) -> &Arc<RedToken> {
        &self.syntax
    }
}

/// Top-level item in a [`SourceFile`]
///
/// Kind: [`NodeKind::DtNode`], [`NodeKind::Directive`] or a preprocessor directive token
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
    fn syntax(&self) -> RedItemRef {
        match self {
            Self::Node(it) => TreeItem::Node(&it.syntax),
            Self::Directive(it) => TreeItem::Node(&it.syntax),
            Self::PreprocessorDirective(it) => TreeItem::Token(&it.syntax),
        }
    }
}
