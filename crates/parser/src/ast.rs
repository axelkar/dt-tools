//! Abstract syntax trees.
//!
//! All AST items reference [`RedNode`]s through [`AstNode::syntax()`].
//!
//! To get an AST item, use [`AstNode::cast()`] with a [`Arc<RedNode>`] as follows:
//!
//! ```
//! use dt_tools_parser::cst::RedNode;
//! use dt_tools_parser::ast::{self, AstNode as _};
//!
//! let red_node: Arc<RedNode>;
//! # use std::sync::Arc;
//! # use dt_tools_parser::{TextRange, cst::{GreenNode, GreenToken, GreenItem, NodeKind},
//! lexer::TokenKind};
//! # let green_node = Arc::new(GreenNode {
//! #     kind: NodeKind::DtNode,
//! #     width: 6,
//! #     children: vec! [
//! #         GreenItem::Token(Arc::new(GreenToken {
//! #             kind: TokenKind::Name,
//! #             text: dt_tools_parser::cst::TokenText::Static("foo"),
//! #         })),
//! #         GreenItem::Token(Arc::new(GreenToken {
//! #             kind: TokenKind::LCurly,
//! #             text: dt_tools_parser::cst::TokenText::Static("{"),
//! #         })),
//! #         GreenItem::Token(Arc::new(GreenToken {
//! #             kind: TokenKind::LCurly,
//! #             text: dt_tools_parser::cst::TokenText::Static("}"),
//! #         })),
//! #         GreenItem::Token(Arc::new(GreenToken {
//! #             kind: TokenKind::LCurly,
//! #             text: dt_tools_parser::cst::TokenText::Static(";"),
//! #         })),
//! #     ]
//! # });
//! # red_node = RedNode::new(green_node);
//! let ast = ast::DtNode::cast(red_node).unwrap();
//!
//! // Use Has- traits
//! use dt_tools_parser::ast::HasName;
//! assert!(ast.name().is_some())
//! ```
use std::{borrow::Cow, sync::Arc};

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
pub trait AstNodeOrToken: Sized {
    /// Try to cast a [`RedToken`] or a [`RedNode`]s to an AST node or token.
    // TODO: return RedItem back if it doesn't succeed, so reditem doesn't have to be cloned in
    // match_ast!
    #[must_use]
    fn cast_either(syntax: RedItem) -> Option<Self> {
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
    fn syntax_item(&self) -> RedItemRef<'_>;
}

impl<T> AstNodeOrToken for T
where
    T: AstNode,
{
    fn cast_node(syntax: Arc<RedNode>) -> Option<Self> {
        Self::cast(syntax)
    }

    fn cast_token(_syntax: Arc<RedToken>) -> Option<Self> {
        None
    }

    fn syntax_item(&self) -> RedItemRef<'_> {
        TreeItem::Node(self.syntax())
    }
}

// TODO: own trait for cast_either?

macro_rules! define_ast_node {
    ($($(#[$attr:meta])* $name:ident : $kind:ident);+ $(;)?) => {
        $(
            $(#[$attr])*
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub struct $name {
                syntax: Arc<RedNode>
            }

            impl AstNode for $name {
                fn cast(syntax: Arc<RedNode>) -> Option<Self> {
                    match syntax.green.kind {
                        NodeKind::$kind => Some(Self { syntax }),
                        _ => None,
                    }
                }
                fn syntax(&self) -> &Arc<RedNode> {
                    &self.syntax
                }
            }
        )+
    }
}
macro_rules! define_ast_token {
    ($($(#[$attr:meta])* $name:ident : $kind:ident);+ $(;)?) => {
        $(
            $(#[$attr])*
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub struct $name {
                syntax: Arc<RedToken>
            }

            impl AstToken for $name {
                fn cast(syntax: Arc<RedToken>) -> Option<Self> {
                    match syntax.green.kind {
                        TokenKind::$kind => Some(Self { syntax }),
                        _ => None,
                    }
                }
                fn syntax(&self) -> &Arc<RedToken> {
                    &self.syntax
                }
            }
        )+
    }
}

// TODO: Better to just make if-let chains? I think this breaks rust-analyzer
/// Matches an [`Arc<RedNode>`][RedNode] against an `ast` type.
///
/// # Example:
///
/// ```
/// # use dt_tools_parser::{match_ast, ast, cst::RedNode};
/// # use std::sync::Arc;
/// let node: Arc<RedNode>;
/// # use dt_tools_parser::cst::{GreenNode, NodeKind};
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
    (match ($node:expr_2021) {
        $( $( $path:ident )::+ ($it:pat) => $res:expr_2021, )*
        _ => $catch_all:expr_2021 $(,)?
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

// FIXME: Remove uses of this old trait returning 0 or 1 labels
/// Trait for [`AstNode`]s with [`DtLabel`]s
pub trait HasLabel: AstNode {
    /// Returns the first [`DtLabel`] if it exists.
    fn label(&self) -> Option<DtLabel> {
        self.syntax().child_nodes().find_map(DtLabel::cast)
    }
}

/// Trait for [`AstNode`]s with [`DtLabel`]s
pub trait HasLabels: AstNode {
    /// Returns an iterator over direct [`DtLabel`] children.
    fn labels(&self) -> impl Iterator<Item = DtLabel> + '_ {
        self.syntax().child_nodes().filter_map(DtLabel::cast)
    }
}

/// Trait for [`AstNode`]s with [`MacroInvocation`]s
pub trait HasMacroInvocation: AstNode {
    /// Returns the first [`MacroInvocation`] if it exists.
    fn macro_invocation(&self) -> Option<MacroInvocation> {
        self.syntax().child_nodes().find_map(MacroInvocation::cast)
    }
}

/// Trait for [`AstNode`]s with [`UnitAddress`]s
pub trait HasUnitAddress: AstNode {
    /// Returns the first [`UnitAddress`] if it exists.
    fn unit_address(&self) -> Option<UnitAddress> {
        self.syntax().child_nodes().find_map(UnitAddress::cast)
    }
}

/// Trait for [`AstNode`]s with [`DtPhandle`]s
pub trait HasDtPhandle: AstNode {
    /// Returns the first [`DtPhandle`] if it exists.
    fn dt_phandle(&self) -> Option<DtPhandle> {
        self.syntax().child_nodes().find_map(DtPhandle::cast)
    }
}

define_ast_node! {
    /// A [Devicetree][1] source file, which contains [`ToplevelItem`]s.
    ///
    /// Kind: [`NodeKind::SourceFile`]
    ///
    /// [1]: https://devicetree.org
    SourceFile: SourceFile;
}

impl SourceFile {
    /// Returns an iterator over direct [`ToplevelItem`] children.
    pub fn items(&self) -> impl Iterator<Item = ToplevelItem> + '_ {
        self.syntax.children().filter_map(ToplevelItem::cast_either)
    }
    /// Returns an iterator over direct [`Directive`] children.
    pub fn directives(&self) -> impl Iterator<Item = DtsDirective> + '_ {
        self.syntax.child_nodes().filter_map(DtsDirective::cast)
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
    pub fn parse(text: &str) -> Parse<'_> {
        let parse = crate::parser::parse(text);
        // TODO:
        //errors.extend(validation::validate(&root));

        debug_assert_eq!(parse.green_node.kind, NodeKind::SourceFile);
        parse
    }
}

define_ast_node! {
    /// A [DTS directive][1].
    ///
    /// Kind: [`NodeKind::Directive`]
    ///
    /// [1]: https://devicetree-specification.readthedocs.io/en/latest/chapter6-source-language.html#compiler-directives
    // TODO: directive parameters
    DtsDirective: DtsDirective;

    /// Node wrapping a DTS directive's arguments.
    ///
    /// Kind: [`NodeKind::DirectiveArguments`]
    DirectiveArguments: DirectiveArguments;
}

impl DtsDirective {
    /// Returns the kind of the first token with [`TokenKind::is_dts_directive`] if one exists.
    #[must_use]
    pub fn kind(&self) -> Option<TokenKind> {
        self.syntax()
            .green
            .child_tokens()
            .map(|tok| tok.kind)
            .find(|kind| kind.is_dts_directive())
    }

    /// Returns the first [`DirectiveArguments`] if it exists.
    pub fn arguments(&self) -> Option<DirectiveArguments> {
        self.syntax()
            .child_nodes()
            .find_map(DirectiveArguments::cast)
    }
}
impl HasName for DirectiveArguments {}
impl HasUnitAddress for DirectiveArguments {}
impl HasMacroInvocation for DirectiveArguments {}
impl HasDtPhandle for DirectiveArguments {}

define_ast_node! {
    /// A [Devicetree property][1].
    ///
    /// Kind: [`NodeKind::DtProperty`]
    ///
    /// [1]: https://devicetree-specification.readthedocs.io/en/latest/chapter6-source-language.html#node-and-property-definitions
    DtProperty: DtProperty;

    /// Unit address part of a node's or a property's name.
    ///
    /// Kind: [`NodeKind::UnitAddress`]
    UnitAddress: UnitAddress;
}

impl DtProperty {
    /// Returns an iterator over direct [`PropValue`] children.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_tools_parser::ast::{DtNode, AstNode, SourceFile};
    /// use dt_tools_parser::cst::RedNode;
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
            Some(value_list) => Either::Left(
                value_list
                    .owned_children()
                    .filter_map(PropValue::cast_either),
            ),
            None => Either::Right(std::iter::empty()),
        }
    }
}
impl HasName for DtProperty {}
impl HasLabel for DtProperty {}
impl HasLabels for DtProperty {}
impl HasMacroInvocation for DtProperty {}
impl HasUnitAddress for DtProperty {}

impl HasName for UnitAddress {}
impl HasMacroInvocation for UnitAddress {}

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
    fn syntax_item(&self) -> RedItemRef<'_> {
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
    Number(Arc<RedToken>),
    Char(Arc<RedToken>),
    Phandle(DtPhandle),
    Macro(MacroInvocation),
    DtExpr(DtExpr),
}
impl AstNodeOrToken for Cell {
    fn cast_node(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtPhandle => Some(Self::Phandle(DtPhandle { syntax })),
            NodeKind::MacroInvocation => Some(Self::Macro(MacroInvocation { syntax })),
            NodeKind::DtExpr => Some(Self::DtExpr(DtExpr { syntax })),
            _ => None,
        }
    }
    fn cast_token(syntax: Arc<RedToken>) -> Option<Self> {
        match syntax.green.kind {
            TokenKind::Number => Some(Self::Number(syntax)),
            TokenKind::Char => Some(Self::Char(syntax)),
            _ => None,
        }
    }
    fn syntax_item(&self) -> RedItemRef<'_> {
        match self {
            Self::Number(it) | Self::Char(it) => TreeItem::Token(it),
            Self::Phandle(it) => TreeItem::Node(&it.syntax),
            Self::Macro(it) => TreeItem::Node(&it.syntax),
            Self::DtExpr(it) => TreeItem::Node(&it.syntax),
        }
    }
}

define_ast_node! {
    /// A phandle item as a [`Cell`] or a [`PropValue`].
    ///
    /// Kind: [`NodeKind::DtPhandle`]
    DtPhandle: DtPhandle;
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

define_ast_node! {
    /// A macro invocation in an expression in a [`Cell`], as a [`PropValue`] or in a name position.
    ///
    /// Kind: [`NodeKind::MacroInvocation`]
    MacroInvocation: MacroInvocation;
}
impl MacroInvocation {
    /// Returns the macro's identifier
    #[must_use]
    pub fn ident(&self) -> Option<Arc<RedToken>> {
        self.syntax
            .child_tokens()
            .find(|tok| tok.green.kind == TokenKind::Ident)
    }
    /// Returns the macro's identifier.
    ///
    /// The difference between this and [`Self::ident`] is that this returns a reference.
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

define_ast_node! {
    /// A Devicetree cell list.
    ///
    /// Kind: [`NodeKind::DtCellList`]
    DtCellList: DtCellList;
}
impl DtCellList {
    // TODO: add example
    pub fn cells(&self) -> impl Iterator<Item = Cell> + '_ {
        self.syntax.children().filter_map(Cell::cast_either)
    }
}

define_ast_node! {
    /// A [Devicetree node][1].
    ///
    /// Kind: [`NodeKind::DtNode`]
    ///
    /// [1]: https://devicetree-specification.readthedocs.io/en/latest/chapter6-source-language.html#node-and-property-definitions
    DtNode: DtNode;
}
impl DtNode {
    /// Returns an iterator over direct [`DtProperty`] children.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_tools_parser::ast::{DtNode, AstNode, SourceFile};
    /// use dt_tools_parser::cst::RedNode;
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

    /// Returns an iterator over direct [`Directive`] children.
    pub fn directives(&self) -> impl Iterator<Item = DtsDirective> + '_ {
        self.syntax.child_nodes().filter_map(DtsDirective::cast)
    }

    /// Returns an iterator over direct [`NodeItem`] children.
    pub fn node_items(&self) -> impl Iterator<Item = NodeItem> + '_ {
        self.syntax.child_nodes().filter_map(NodeItem::cast)
    }

    /// Returns an iterator over direct [`DtNode`] children.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_tools_parser::ast::{DtNode, AstNode, SourceFile};
    /// use dt_tools_parser::cst::RedNode;
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

    /// Returns the name with the unit address.
    ///
    /// # Example
    ///
    /// ```
    /// use dt_tools_parser::ast::{DtNode, AstNode, SourceFile, HasName};
    /// use dt_tools_parser::cst::RedNode;
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
        Some(match self.unit_address().and_then(|ast| ast.name()) {
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
    /// use dt_tools_parser::ast::{DtNode, AstNode, SourceFile};
    /// use dt_tools_parser::cst::RedNode;
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
    /// use dt_tools_parser::ast::{DtNode, AstNode, SourceFile};
    /// use dt_tools_parser::cst::RedNode;
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
    /// use dt_tools_parser::ast::{DtNode, AstNode, SourceFile};
    /// use dt_tools_parser::cst::RedNode;
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
    pub fn path<'i>(&self, src: &'i str) -> impl Iterator<Item = Cow<'i, str>> + use<'i> {
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
impl HasLabels for DtNode {}
impl HasMacroInvocation for DtNode {}
impl HasUnitAddress for DtNode {}

define_ast_token! {
    /// A generic name.
    ///
    /// This can only be found in label, property and node names currently. TODO: update
    ///
    /// Kind: [`TokenKind::Name`]
    Name: Name;
}

// FIXME: dirty unimplemented hack for dt_tools_lsp::hover
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

define_ast_node! {
    /// A [`DtNode`]'s optional label
    ///
    /// Kind: [`NodeKind::DtLabel`]
    DtLabel: DtLabel;
}
impl HasName for DtLabel {}
impl HasMacroInvocation for DtLabel {}

define_ast_node! {
    /// A parenthesized expression in a [`Cell`] that should evaluate to an integer.
    ///
    /// Wraps [`Expr`].
    ///
    /// Kind: [`NodeKind::DtExpr`]
    DtExpr: DtExpr;
}
impl DtExpr {
    /// Returns the first [`Expr`] if it exists.
    pub fn expr(&self) -> Option<Expr> {
        self.syntax().child_nodes().find_map(Expr::cast)
    }
}

define_ast_node! {
    /// A preprocessor conditional.
    ///
    /// Kind: [`NodeKind::PreprocessorConditional`]
    ///
    /// # Example
    ///
    /// ```dts
    /// #if 1
    /// #else
    /// #endif
    /// ```
    PreprocessorConditional: PreprocessorConditional;
}
impl PreprocessorConditional {
    /// Returns an iterator over the branches in the preprocessor conditional.
    pub fn branches(
        &self,
    ) -> impl Iterator<Item = (PreprocessorDirective, PreprocessorBranch)> + '_ {
        let mut iter = self.syntax.children();
        std::iter::from_fn(move || {
            let directive = (&mut iter)
                .filter_map(RedItem::into_token)
                .find_map(PreprocessorDirective::cast)?;
            let branch = (&mut iter)
                .filter_map(RedItem::into_node)
                .find_map(PreprocessorBranch::cast)?;

            Some((directive, branch))
        })
    }
}

define_ast_node! {
    /// Branch of a preprocessor conditional.
    ///
    /// Kind: [`NodeKind::PreprocessorBranch`]
    PreprocessorBranch: PreprocessorBranch;
}
impl PreprocessorBranch {
    /// Returns an iterator over direct [`ToplevelItem`] children.
    pub fn items(&self) -> impl Iterator<Item = ToplevelItem> + '_ {
        self.syntax.children().filter_map(ToplevelItem::cast_either)
    }
}

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
/// Kind: [`NodeKind::DtNode`], [`NodeKind::Directive`], a preprocessor conditional or a preprocessor directive token
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum ToplevelItem {
    /// A Devicetree node
    Node(DtNode),
    /// A DTS directive
    ///
    /// e.g. `/include/ file.dts;`
    Directive(DtsDirective),
    /// A preprocessor conditional.
    PreprocessorConditional(PreprocessorConditional),
    /// A preprocessor directive
    ///
    /// e.g. `#include file.dts`
    PreprocessorDirective(PreprocessorDirective),
}
impl AstNodeOrToken for ToplevelItem {
    fn cast_node(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtNode => Some(Self::Node(DtNode { syntax })),
            NodeKind::DtsDirective => Some(Self::Directive(DtsDirective { syntax })),
            NodeKind::PreprocessorConditional => {
                Some(Self::PreprocessorConditional(PreprocessorConditional {
                    syntax,
                }))
            }
            _ => None,
        }
    }
    fn cast_token(syntax: Arc<RedToken>) -> Option<Self> {
        Some(Self::PreprocessorDirective(PreprocessorDirective::cast(
            syntax,
        )?))
    }
    fn syntax_item(&self) -> RedItemRef<'_> {
        match self {
            Self::Node(it) => TreeItem::Node(&it.syntax),
            Self::Directive(it) => TreeItem::Node(&it.syntax),
            Self::PreprocessorConditional(it) => TreeItem::Node(&it.syntax),
            Self::PreprocessorDirective(it) => TreeItem::Token(&it.syntax),
        }
    }
}

define_ast_node! {
    PrefixExpr: PrefixExpr;
    ParenExpr: ParenExpr;
    LiteralExpr: LiteralExpr;
    /// Note: watch out for the ternary expression. It should have three [`Expr`] children.
    InfixExpr: InfixExpr;
}

impl ParenExpr {
    /// Returns the contained [`Expr`].
    pub fn expr(&self) -> Option<Expr> {
        self.syntax.child_nodes().find_map(Expr::cast)
    }
}
impl PrefixExpr {
    /// Returns the token kind of the operation.
    #[must_use]
    pub fn op(&self) -> Option<TokenKind> {
        self.syntax
            .green
            .child_tokens()
            .map(|tok| tok.kind)
            .find(|kind| !kind.is_trivia())
    }
    /// Returns the contained [`Expr`].
    pub fn expr(&self) -> Option<Expr> {
        self.syntax.child_nodes().find_map(Expr::cast)
    }
}

/// Expression that should evaluate to an integer.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum Expr {
    PrefixExpr(PrefixExpr),
    ParenExpr(ParenExpr),
    MacroInvocation(MacroInvocation),
    LiteralExpr(LiteralExpr),
    InfixExpr(InfixExpr),
}
impl AstNode for Expr {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::PrefixExpr => Some(Self::PrefixExpr(PrefixExpr { syntax })),
            NodeKind::ParenExpr => Some(Self::ParenExpr(ParenExpr { syntax })),
            NodeKind::MacroInvocation => Some(Self::MacroInvocation(MacroInvocation { syntax })),
            NodeKind::LiteralExpr => Some(Self::LiteralExpr(LiteralExpr { syntax })),
            NodeKind::InfixExpr => Some(Self::InfixExpr(InfixExpr { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &Arc<RedNode> {
        match self {
            Self::PrefixExpr(it) => it.syntax(),
            Self::ParenExpr(it) => it.syntax(),
            Self::MacroInvocation(it) => it.syntax(),
            Self::LiteralExpr(it) => it.syntax(),
            Self::InfixExpr(it) => it.syntax(),
        }
    }
}

/// Item inside a [`DtNode`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum NodeItem {
    DtProperty(DtProperty),
    DtNode(DtNode),
    Directive(DtsDirective),
}
impl AstNode for NodeItem {
    fn cast(syntax: Arc<RedNode>) -> Option<Self> {
        match syntax.green.kind {
            NodeKind::DtProperty => Some(Self::DtProperty(DtProperty { syntax })),
            NodeKind::DtNode => Some(Self::DtNode(DtNode { syntax })),
            NodeKind::DtsDirective => Some(Self::Directive(DtsDirective { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &Arc<RedNode> {
        match self {
            Self::DtProperty(it) => it.syntax(),
            Self::DtNode(it) => it.syntax(),
            Self::Directive(it) => it.syntax(),
        }
    }
}
