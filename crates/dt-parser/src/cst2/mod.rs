use std::{fmt, sync::Arc};

use self::lexer::TokenKind;
use crate::TextRange;

pub mod grammar;
pub mod lexer;
pub mod parser;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeKind {
    SourceFile,
    /// A parse error
    ParseError,
    Directive,
    /// A directive's parameters, if any.
    DirectiveParams,
    PreprocessorDirective,
    DtNode,
    DtProperty,
    DtCellList,
    DtExpr,
    DtLabel,
    /// TODO: How to model enum ast::DtPropValue in ungrammar?
    /// PropValueList = (('String' | 'DtBytestring' | DtCellList | DtPhandle) ','?)*
    PropValueList,
    DtPhandle,
    UnitAddress,
    MacroInvocation,
    MacroParameter,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A "green node".
pub struct GreenNode {
    /// The kind of node
    pub kind: NodeKind,
    /// Cached length of all of the textual contents
    pub width: usize,
    /// The child nodes and tokens
    pub children: Vec<GreenItem>,
}
impl GreenNode {
    /// Print a tree like rust-analyzer's debug AST
    pub fn print_tree(&self) -> String {
        let mut out = String::new();
        self.print_tree_file(&mut out).unwrap();
        out
    }

    /// Print a tree like rust-analyzer's debug AST
    #[inline(always)]
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

        for child in self.children.iter() {
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
        self.children.iter().flat_map(GreenItem::as_node)
    }

    /// Returns an iterator over immediate child tokens.
    pub fn child_tokens(&self) -> impl Iterator<Item = &Arc<GreenToken>> + '_ {
        self.children.iter().flat_map(GreenItem::as_token)
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
    pub fn to_owned(&self) -> String {
        match self {
            Self::Static(str) => (*str).to_owned(),
            Self::Dynamic(str) => str.clone(),
        }
    }
    /// Extracts the owned `String`.
    pub fn into_owned(self) -> String {
        match self {
            Self::Static(str) => str.to_owned(),
            Self::Dynamic(str) => str,
        }
    }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenToken {
    pub kind: TokenKind,
    /// The text the token represents
    pub text: TokenText,
}
impl GreenToken {
    /// Returns the length of the textual contents.
    #[inline]
    pub fn length(&self) -> usize {
        match &self.text {
            TokenText::Static(s) => s.len(),
            TokenText::Dynamic(s) => s.len(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct RedToken {
    pub parent: Arc<RedNode>,
    pub green: Arc<GreenToken>,
    /// Offset in the source file
    pub text_offset: usize,
}
impl std::fmt::Debug for RedToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RedToken")
            .field("green", &self.green)
            .finish()
    }
}
impl RedToken {
    /// Returns the text range.
    #[inline(always)]
    pub fn text_range(&self) -> TextRange {
        TextRange {
            start: self.text_offset,
            end: self.text_offset + self.green.length(),
        }
    }

    /// Returns the text the token represents.
    #[inline(always)]
    pub fn text(&self) -> &TokenText {
        &self.green.text
    }

    /// Returns the length of the textual contents.
    #[inline(always)]
    pub fn length(&self) -> usize {
        self.green.length()
    }

    /// Returns the text the token represents from `src`.
    pub fn text_from_source<'i>(&self, src: &'i str) -> &'i str {
        let opt = src.get(self.text_offset..(self.text_offset + self.green.length()));

        if cfg!(debug_assertions) {
            opt.expect("Invalid text offset, wrong source?")
        } else {
            opt.unwrap_or("")
        }
    }

    /// Iterator over all the ancestors of this token excluding itself.
    pub fn parent_ancestors(&self) -> impl Iterator<Item = Arc<RedNode>> {
        std::iter::successors(Some(self.parent.clone()), |node| node.parent.clone())
    }
}

/// A "red node".
///
/// This references the green nodes and its own parent nodes.
///
/// You basically get a tree you can go up and down in without cyclic references!
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
    pub fn new(root: Arc<GreenNode>) -> Arc<Self> {
        Arc::new(RedNode {
            parent: None,
            green: root,
            text_offset: 0,
        })
    }

    /// Returns the text range.
    #[inline(always)]
    pub fn text_range(&self) -> TextRange {
        TextRange {
            start: self.text_offset,
            end: self.text_offset + self.green.width,
        }
    }

    /// Returns the node at the specified byte offset if found.
    pub fn node_at_offset(self: &Arc<RedNode>, offset: usize) -> Option<Arc<RedNode>> {
        // TODO: non recursing algorithm? binary search?
        self.child_nodes()
            .find(|node| node.text_range().byte_range().contains(&offset))
            .map(|node| node.node_at_offset(offset).unwrap_or(node))
    }

    /// Returns the token at the specified byte offset if found.
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

    /// Returns an iterator over all children.
    ///
    /// The iterator returned owns `Arc<RedNode>`, and thus the iterator does not have a specific lifetime.
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

    /// Returns an iterator over all children.
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

    /// Returns an iterator over the child nodes.
    pub fn child_nodes<'a>(self: &'a Arc<RedNode>) -> impl Iterator<Item = Arc<RedNode>> + 'a {
        self.children().flat_map(RedItem::into_node)
    }

    /// Returns an iterator over the child tokens.
    pub fn child_tokens<'a>(self: &'a Arc<RedNode>) -> impl Iterator<Item = Arc<RedToken>> + 'a {
        self.children().flat_map(RedItem::into_token)
    }

    /// Iterator over all the ancestors of this node excluding itself.
    pub fn parent_ancestors(&self) -> impl Iterator<Item = Arc<RedNode>> {
        std::iter::successors(self.parent.clone(), |node| node.parent.clone())
    }
}

impl fmt::Debug for RedNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RedNode")
            .field("green", &self.green)
            .field("text_offset", &self.text_offset)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TreeItem<Node, Token> {
    Node(Node),
    Token(Token),
}
impl<Node, Token> TreeItem<Node, Token> {
    pub fn as_node(&self) -> Option<&Node> {
        match self {
            Self::Node(node) => Some(node),
            _ => None,
        }
    }
    pub fn as_token(&self) -> Option<&Token> {
        match self {
            Self::Token(token) => Some(token),
            _ => None,
        }
    }
    pub fn into_node(self) -> Option<Node> {
        match self {
            Self::Node(node) => Some(node),
            _ => None,
        }
    }
    pub fn into_token(self) -> Option<Token> {
        match self {
            Self::Token(token) => Some(token),
            _ => None,
        }
    }
    pub fn map_node<NewNode>(self, f: impl FnOnce(Node) -> NewNode) -> TreeItem<NewNode, Token> {
        match self {
            Self::Node(node) => TreeItem::Node(f(node)),
            Self::Token(token) => TreeItem::Token(token),
        }
    }
    pub fn map_token<NewToken>(
        self,
        f: impl FnOnce(Token) -> NewToken,
    ) -> TreeItem<Node, NewToken> {
        match self {
            Self::Node(node) => TreeItem::Node(node),
            Self::Token(token) => TreeItem::Token(f(token)),
        }
    }
    pub fn filter_node(self, f: impl FnOnce(&Node) -> bool) -> Option<Self> {
        match self {
            Self::Node(node) if !f(&node) => None,
            other => Some(other),
        }
    }
    pub fn filter_token(self, f: impl FnOnce(&Token) -> bool) -> Option<Self> {
        match self {
            Self::Token(token) if !f(&token) => None,
            other => Some(other),
        }
    }
}

pub type GreenItem = TreeItem<Arc<GreenNode>, Arc<GreenToken>>;
pub type RedItem = TreeItem<Arc<RedNode>, Arc<RedToken>>;
pub type RedItemRef<'a> = TreeItem<&'a Arc<RedNode>, &'a Arc<RedToken>>;

impl GreenItem {
    /// Returns the length of the textual contents.
    #[inline]
    pub fn length(&self) -> usize {
        match self {
            TreeItem::Node(node) => node.width,
            TreeItem::Token(token) => token.length(),
        }
    }
}

impl RedItem {
    /// Returns the text range.
    #[inline(always)]
    pub fn text_range(&self) -> TextRange {
        match self {
            TreeItem::Node(node) => node.text_range(),
            TreeItem::Token(token) => token.text_range(),
        }
    }
}

impl RedItemRef<'_> {
    /// Returns the text range.
    #[inline(always)]
    pub fn text_range(&self) -> TextRange {
        match self {
            TreeItem::Node(node) => node.text_range(),
            TreeItem::Token(token) => token.text_range(),
        }
    }
}
