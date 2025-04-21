//! The module implementing the Concrete Syntax Tree for dt-tools.
//!
//! <span style="color:lime;">Green items</span> are returned from the parser. They cannot traverse
//! their parents.
//!
//! <span style="color:red;">Red items</span> are built on the fly on top of green nodes and can
//! traverse their parents. You basically get a tree you can go up and down in without cyclic
//! references!
//!
//! _"Items"_ are used to mean both nodes and tokens.
// TODO: insert picture of a visual explanation to CSTs

use std::{fmt, sync::Arc};

use crate::lexer::TokenKind;
use crate::TextRange;

/// The kind of a CST node
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeKind {
    // Entrypoints
    /// See [`Entrypoint`](super::parser::Entrypoint)
    SourceFile,
    /// See [`Entrypoint`](super::parser::Entrypoint)
    EntryName,
    /// See [`Entrypoint`](super::parser::Entrypoint)
    EntryPropValues,
    /// See [`Entrypoint`](super::parser::Entrypoint)
    EntryCells,

    /// A node wrapping a parse error
    ParseError,
    /// A node wrapping a DTS directive
    Directive,
    /// A node wrapping a DTS directive's arguments
    DirectiveArguments,
    PreprocessorDirective,
    DtNode,
    DtProperty,
    DtCellList,
    DtExpr,
    DtLabel,
    // TODO: How to model enum ast::DtPropValue in ungrammar?
    // PropValueList = (('String' | 'DtBytestring' | DtCellList | DtPhandle) ','?)*
    PropValueList,
    DtPhandle,
    UnitAddress,
    MacroInvocation,
    MacroArgument,
}

/// A node as returned by the parser. It cannot traverse its parents.
///
/// See the [crate documentation](crate) for more info.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenNode {
    /// The kind of node
    pub kind: NodeKind,
    /// Cached bytewise length of all of the textual contents
    pub width: usize,
    /// The child nodes and tokens
    pub children: Vec<GreenItem>,
}
impl GreenNode {
    /// Print a tree like rust-analyzer's debug AST
    #[must_use]
    #[expect(clippy::missing_panics_doc, reason = "Writing to a String can't panic")]
    pub fn print_tree(&self) -> String {
        let mut out = String::new();
        self.print_tree_file(&mut out).unwrap();
        out
    }

    /// Print a tree like rust-analyzer's debug AST
    ///
    /// # Errors
    ///
    /// Will return `Err` if writing to `out` fails
    #[inline]
    pub fn print_tree_file(&self, out: &mut impl std::fmt::Write) -> std::fmt::Result {
        self.print_tree_rec(0, 0, out)
    }

    fn print_tree_rec(
        &self,
        level: usize,
        mut offset: usize,
        out: &mut impl std::fmt::Write,
    ) -> std::fmt::Result {
        const INDENT: &str = "  ";
        writeln!(
            out,
            "{}{:?}@{}..{}",
            INDENT.repeat(level),
            self.kind,
            offset,
            offset + self.width
        )?;

        for child in &self.children {
            match child {
                GreenItem::Node(ref node) => node.print_tree_rec(level + 1, offset, out)?,
                GreenItem::Token(ref token) => writeln!(
                    out,
                    "{}{:?}@{}..{} {:?}",
                    INDENT.repeat(level + 1),
                    token.kind,
                    offset,
                    offset + token.length(),
                    token.text,
                )?,
            }
            offset += child.length();
        }
        Ok(())
    }

    /// Returns an iterator over immediate child nodes.
    pub fn child_nodes(&self) -> impl Iterator<Item = &Arc<GreenNode>> + '_ {
        self.children.iter().filter_map(GreenItem::as_node)
    }

    /// Returns an iterator over immediate child tokens.
    pub fn child_tokens(&self) -> impl Iterator<Item = &Arc<GreenToken>> + '_ {
        self.children.iter().filter_map(GreenItem::as_token)
    }
}

// TODO: benchmark without Static tokens
/// Static or dynamic string
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TokenText {
    Static(&'static str),
    Dynamic(String),
}
impl TokenText {
    /// Creates a `String` by cloning the current value's reference.
    #[must_use]
    pub fn to_owned(&self) -> String {
        match self {
            Self::Static(str) => (*str).to_owned(),
            Self::Dynamic(str) => str.clone(),
        }
    }
    /// Extracts the owned `String`.
    #[must_use]
    pub fn into_owned(self) -> String {
        match self {
            Self::Static(str) => str.to_owned(),
            Self::Dynamic(str) => str,
        }
    }
    #[must_use]
    pub fn as_str(&self) -> &str {
        match self {
            Self::Static(str) => str,
            Self::Dynamic(str) => str,
        }
    }
}
impl std::fmt::Display for TokenText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Static(str) => std::fmt::Display::fmt(str, f),
            Self::Dynamic(str) => std::fmt::Display::fmt(str, f),
        }
    }
}
impl std::fmt::Debug for TokenText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Static(str) => std::fmt::Debug::fmt(str, f),
            Self::Dynamic(str) => std::fmt::Debug::fmt(str, f),
        }
    }
}

impl std::ops::Deref for TokenText {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Static(str) => str,
            Self::Dynamic(str) => str.as_str(),
        }
    }
}

/// A token as returned by the parser. It cannot traverse its parents.
///
/// See the [crate documentation](crate) for more info.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenToken {
    pub kind: TokenKind,
    /// The text the token represents
    pub text: TokenText,
}
impl GreenToken {
    /// Returns the length of the textual contents.
    #[inline]
    #[must_use]
    pub fn length(&self) -> usize {
        match &self.text {
            TokenText::Static(s) => s.len(),
            TokenText::Dynamic(s) => s.len(),
        }
    }
}

/// A red token on top of a green token. It can traverse its parents.
///
/// See the [crate documentation](crate) for more info.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct RedToken {
    pub parent: Arc<RedNode>,
    pub green: Arc<GreenToken>,
    /// Offset in the source file
    pub text_offset: usize,
}
#[expect(clippy::missing_fields_in_debug, reason = "we can't print `parent`")]
impl std::fmt::Debug for RedToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RedToken")
            .field("green", &self.green)
            .finish()
    }
}
impl RedToken {
    /// Returns the text range.
    #[inline]
    #[must_use]
    pub fn text_range(&self) -> TextRange {
        TextRange {
            start: self.text_offset,
            end: self.text_offset + self.green.length(),
        }
    }

    /// Returns the text the token represents.
    #[inline]
    #[must_use]
    pub fn text(&self) -> &TokenText {
        &self.green.text
    }

    /// Returns the length of the textual contents.
    #[inline]
    #[must_use]
    pub fn length(&self) -> usize {
        self.green.length()
    }

    /// Returns the text the token represents from `src`.
    #[must_use]
    pub fn text_from_source<'i>(&self, src: &'i str) -> &'i str {
        let opt = src.get(self.text_offset..(self.text_offset + self.green.length()));

        debug_assert!(opt.is_some(), "Invalid text offset, wrong source?");

        opt.unwrap_or("")
    }

    /// Iterator over all the ancestors of this token excluding itself.
    pub fn parent_ancestors(&self) -> impl Iterator<Item = Arc<RedNode>> {
        std::iter::successors(Some(self.parent.clone()), |node| node.parent.clone())
    }
}

/// A red node on top of a green node. It can traverse its parents.
///
/// See the [crate documentation](crate) for more info.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct RedNode {
    /// The parent node if it exists.
    ///
    /// All of the `child_` functions set this to `self` for their children.
    pub parent: Option<Arc<RedNode>>,
    /// The respective [`GreenNode`].
    pub green: Arc<GreenNode>,
    /// Offset in the source file
    pub text_offset: usize,
}

impl RedNode {
    /// Creates a new red node with no parent.
    #[must_use]
    pub fn new(root: Arc<GreenNode>) -> Arc<Self> {
        Arc::new(RedNode {
            parent: None,
            green: root,
            text_offset: 0,
        })
    }

    /// Returns the text range.
    #[inline]
    #[must_use]
    pub fn text_range(&self) -> TextRange {
        TextRange {
            start: self.text_offset,
            end: self.text_offset + self.green.width,
        }
    }

    /// Returns the node at the specified byte offset if found.
    #[must_use]
    pub fn node_at_offset(self: &Arc<RedNode>, offset: usize) -> Option<Arc<RedNode>> {
        // TODO: non recursing algorithm? binary search?
        self.child_nodes()
            .find(|node| node.text_range().byte_range().contains(&offset))
            .map(|node| node.node_at_offset(offset).unwrap_or(node))
    }

    /// Returns the token at the specified byte offset if found.
    #[must_use]
    pub fn token_at_offset(self: &Arc<RedNode>, offset: usize) -> Option<Arc<RedToken>> {
        // TODO: non recursing algorithm? binary search?
        self.child_nodes()
            .find(|node| node.text_range().byte_range().contains(&offset))
            .and_then(|node| node.token_at_offset(offset))
            .or_else(|| {
                self.child_tokens()
                    .find(|token| token.text_range().byte_range().contains(&offset))
            })
    }

    /// Returns an iterator over all immediate children.
    ///
    /// The iterator returned owns `self`, and thus the iterator does not have a specific lifetime.
    pub fn owned_children(self: Arc<RedNode>) -> impl Iterator<Item = RedItem> {
        let mut current_text_offset = self.text_offset;

        let range = 0..self.green.children.len();
        range.map(move |idx| match &self.green.children[idx] {
            GreenItem::Node(node) => {
                let text_offset = current_text_offset;
                current_text_offset += node.width;

                RedItem::Node(Arc::new(RedNode {
                    parent: Some(Arc::clone(&self)),
                    green: Arc::clone(node),
                    text_offset,
                }))
            }
            GreenItem::Token(token) => {
                let text_offset = current_text_offset;
                current_text_offset += token.text.len();

                RedItem::Token(Arc::new(RedToken {
                    parent: Arc::clone(&self),
                    green: Arc::clone(token),
                    text_offset,
                }))
            }
        })
    }

    /// Returns an iterator over all immediate children.
    pub fn children<'a>(self: &'a Arc<RedNode>) -> impl Iterator<Item = RedItem> + 'a {
        let mut current_text_offset = self.text_offset;

        self.green
            .children
            .iter()
            .map(move |green_child| match green_child {
                GreenItem::Node(node) => {
                    let text_offset = current_text_offset;
                    current_text_offset += node.width;

                    RedItem::Node(Arc::new(RedNode {
                        parent: Some(Arc::clone(self)),
                        green: Arc::clone(node),
                        text_offset,
                    }))
                }
                GreenItem::Token(token) => {
                    let text_offset = current_text_offset;
                    current_text_offset += token.text.len();

                    RedItem::Token(Arc::new(RedToken {
                        parent: Arc::clone(self),
                        green: Arc::clone(token),
                        text_offset,
                    }))
                }
            })
    }

    /// Returns an iterator over immediate child nodes.
    pub fn child_nodes<'a>(self: &'a Arc<RedNode>) -> impl Iterator<Item = Arc<RedNode>> + 'a {
        self.children().filter_map(RedItem::into_node)
    }

    /// Returns an iterator over immediate child tokens.
    pub fn child_tokens<'a>(self: &'a Arc<RedNode>) -> impl Iterator<Item = Arc<RedToken>> + 'a {
        self.children().filter_map(RedItem::into_token)
    }

    /// Iterator over all the ancestors of this node excluding itself.
    pub fn parent_ancestors(&self) -> impl Iterator<Item = Arc<RedNode>> {
        std::iter::successors(self.parent.clone(), |node| node.parent.clone())
    }
}

#[expect(clippy::missing_fields_in_debug, reason = "we can't print `parent`")]
impl fmt::Debug for RedNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RedNode")
            .field("green", &self.green)
            .field("text_offset", &self.text_offset)
            .finish()
    }
}

/// Something that has distinct types for representing nodes and tokens. See [`GreenItem`] and
/// [`RedItem`] for more.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TreeItem<Node, Token> {
    Node(Node),
    Token(Token),
}
impl<Node, Token> TreeItem<Node, Token> {
    /// Returns the node wrapped in a `Some` if this is a node.
    pub fn as_node(&self) -> Option<&Node> {
        match self {
            Self::Node(node) => Some(node),
            _ => None,
        }
    }
    /// Returns the token wrapped in a `Some` if this is a token.
    pub fn as_token(&self) -> Option<&Token> {
        match self {
            Self::Token(token) => Some(token),
            _ => None,
        }
    }
    /// Returns the node wrapped in a `Some` if this is a node.
    pub fn into_node(self) -> Option<Node> {
        match self {
            Self::Node(node) => Some(node),
            _ => None,
        }
    }
    /// Returns the token wrapped in a `Some` if this is a token.
    pub fn into_token(self) -> Option<Token> {
        match self {
            Self::Token(token) => Some(token),
            _ => None,
        }
    }
}

/// An item as returned by the parser. It cannot traverse its parents.
///
/// See the [crate documentation](crate) for more info.
pub type GreenItem = TreeItem<Arc<GreenNode>, Arc<GreenToken>>;

/// A red item on top of a green item. It can traverse its parents.
///
/// See the [crate documentation](crate) for more info.
pub type RedItem = TreeItem<Arc<RedNode>, Arc<RedToken>>;

/// Similar to [`RedItem`], but with only references to the red items.
pub type RedItemRef<'a> = TreeItem<&'a Arc<RedNode>, &'a Arc<RedToken>>;

impl GreenItem {
    /// Returns the length of the textual contents.
    #[inline]
    #[must_use]
    pub fn length(&self) -> usize {
        match self {
            TreeItem::Node(node) => node.width,
            TreeItem::Token(token) => token.length(),
        }
    }
}

impl RedItem {
    /// Returns the text range.
    #[inline]
    #[must_use]
    pub fn text_range(&self) -> TextRange {
        match self {
            TreeItem::Node(node) => node.text_range(),
            TreeItem::Token(token) => token.text_range(),
        }
    }
}

impl RedItemRef<'_> {
    /// Returns the text range.
    #[inline]
    #[must_use]
    pub fn text_range(&self) -> TextRange {
        match self {
            TreeItem::Node(node) => node.text_range(),
            TreeItem::Token(token) => token.text_range(),
        }
    }
}
